(* Zen special house — port of priv/sp/house/Zen.scala. *)

open Sp
open Engine
open Common

let matter_ref : card option ref = ref None
let inertia_ref : card option ref = ref None
let eguard_ref : card option ref = ref None
let mimic_ref : card option ref = ref None
let owner_has env (card : card) = Option.is_some (Slots.find_card (player_of_env env).slots_update card)
let cocoons p = match p.pstate.ps_data with Zen_cocoons xs -> xs | _ -> []
let set_cocoons p xs = Player.set_data p (Zen_cocoons xs)

(* triggerAbility(ZenSpark(...)): a bolt from the selected slot to [s] *)
let spark env (s : slot_update) =
  emit env.env_updater
    (Ev_ability
       {
         kind = "zen.spark";
         player = env.env_player;
         num = env.env_selected;
         target = Some (s.slots.player.pid, s.num);
       })

let elem_attack target d player =
  match target with
  | [] -> ()
  | num :: _ -> (
      let op = Player.other player in
      match op.slots_update.slot_updates.(num).value with
      | None -> Player.inflict op d
      | Some st ->
          let h = st.card.c_card.house_index in
          Slots.foreach op.slots_update (fun s ->
              if (Slot.get s).card.c_card.house_index = h then Slot.inflict s d))

(* Redlight: when summoned it is charged, shown as a red light on the card.
   On its owner's first turn after that it throws the light at the opponent's
   strongest creature (highest cost, leftmost on a tie) and the charge is
   spent.  The board shows the throw as red motes riding an arc. *)
let redlight_mark = "zen-redlight"

let redlight_charged (st : slot_state) =
  List.exists (fun (m : blacksmith_mark) -> m.bm_key = redlight_mark) st.marks

let set_redlight slot =
  Slot.update slot (fun st ->
      if redlight_charged st then st
      else { st with marks = { bm_key = redlight_mark; bm_used = false; bm_target = 0 } :: st.marks })

let clear_redlight slot =
  Slot.update slot (fun st ->
      { st with marks = List.filter (fun m -> m.bm_key <> redlight_mark) st.marks })

let redlight_arm : card_effect = fun env -> set_redlight (own_slot_of_env env)

let redlight_strike : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with
  | Some st when redlight_charged st -> (
      (* no creature to strike: keep the charge for a later turn *)
      match reduce_filled (other_of_env env).slots_update highest_cost with
      | None -> ()
      | Some s ->
          clear_redlight slot;
          emit env.env_updater
            (Ev_ability
               {
                 kind = "zen.redlight";
                 player = env.env_player;
                 num = env.env_selected;
                 target = Some (s.slots.player.pid, s.num);
               });
          Slot.inflict s (ability_damage env 6))
  | _ -> ()

(* Focus: 1 mana of the house of the opponent's two strongest creatures and of
   the caster's two weakest.  The creatures pay the mana themselves, so the
   board draws the arc from each of them to the house it pays, and a box
   closes over the creature Focus reads. *)
let focus_spell : card_effect =
  fun env ->
    let p = player_of_env env in
    (* the creatures of a board, weakest attack first *)
    let by_attack (su : slots_update) =
      Slots.foldl su [] (fun acc s ->
          match s.value with
          | None -> acc
          | Some st -> s :: acc)
      |> List.sort (fun a b ->
             let attack s = Slots.current_attack s (Slot.get s) in
             compare (attack a) (attack b))
    in
    let first n l = List.filteri (fun i _ -> i < n) l in
    let mana_for l =
      List.iter
        (fun s ->
          (* a house reaction (High Priest's Babi) can empty a slot that is
             still queued here, so re-read it before reading its card *)
          match s.value with
          | None -> ()
          | Some st ->
              (* the box reads the creature first, then its mana flies home *)
              emit env.env_updater
                (Ev_ability { kind = "zen.focus"; player = s.slots.player.pid; num = s.num; target = None });
              incr_mana_from s p 1 [ st.card.c_card.house_index ])
        l
    in
    (* the two weakest friends and the two strongest foes each pay a mana *)
    mana_for (first 2 (by_attack p.slots_update));
    mana_for (first 2 (List.rev (by_attack (other_of_env env).slots_update)))

let spawn_matter : card_effect =
 fun env ->
  match !matter_ref with
  | None -> ()
  | Some matter -> (
      let p = player_of_env env in
      let opens =
        Slots.get_open p.slots_update |> List.filter (fun n -> n > env.env_selected) |> List.sort compare
      in
      match opens with
      | [] -> ()
      | n :: _ ->
          ignore (Slots.summon p.slots_update n (make_creature matter));
          Option.iter
            (fun st ->
              Option.iter
                (fun f ->
                  let e = mk_env env.env_player env.env_updater in
                  e.env_selected <- n;
                  e.env_card <- Some st.card.c_card;
                  f e)
                st.card.c_card.effects.direct)
            p.slots_update.slot_updates.(n).value)

let inertia_heal : card_effect =
  fun env ->
    match !matter_ref with
    | Some matter when owner_has env matter ->
        let heal = List.length (Slots.get_open (player_of_env env).slots_update) in
        if heal > 0 then (
          Option.iter
            (fun s ->
              Updater.focus env.env_updater s.num env.env_player false;
              Slot.heal s heal;
              (* the life itself travels: a green line from Inertia to the
                 creature it heals, a small pulse on the stone, and one
                 mote per life on the creature *)
              emit env.env_updater
                (Ev_ability
                   {
                     kind = Printf.sprintf "zen.heal.%d" heal;
                     player = env.env_player;
                     num = env.env_selected;
                     target = Some (env.env_player, s.num);
                   }))
            (reduce_filled (player_of_env env).slots_update lowest_life);
          (* the owner is healed second, so its line waits until the creature's
             tether has faded *)
          Player.heal (player_of_env env) heal;
          emit env.env_updater
            (Ev_ability
               {
                 kind = Printf.sprintf "zen.inertia.life.%d" heal;
                 player = env.env_player;
                 num = env.env_selected;
                 target = None;
               }))
    | _ -> ()

let matter_strike : card_effect =
 fun env ->
  match !inertia_ref with
  | Some inertia when owner_has env inertia ->
      let damage = List.length (Slots.get_open (player_of_env env).slots_update) in
      if damage > 0 then
        Option.iter
          (fun s ->
            spark env s;
            Slot.inflict s (ability_damage env damage))
          (reduce_filled (other_of_env env).slots_update strongest)
  | _ -> ()

let zen_effect : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match (slot.value, Slot.opposite slot) with
  | Some mine, opp ->
      Option.iter
        (fun opp_st ->
          if opp_st.attack > 5 || opp_st.life > mine.life then (
            incr_mana_from slot (player_of_env env) 1 [ opp_st.card.c_card.house_index ];
            Slot.heal slot 3))
        opp.value
  | _ -> ()

(* Dreamer: 4 damage to the weakest enemy creature, paid with 2 of its own life. *)
let dreamer_shock : card_effect =
 fun env ->
  Option.iter
    (fun s ->
      spark env s;
      Slot.inflict s (ability_damage env 4);
      drain (own_slot_of_env env) (ability_damage env 2))
    (reduce_filled (other_of_env env).slots_update lowest_life)

let place_cocoon : card_effect =
 fun env ->
  let p = player_of_env env in
  match Slots.get_open p.slots_update with
  | [] -> set_cocoons p []
  | opens ->
      let num = List.nth opens (Dice.int env.env_updater.rng (List.length opens)) in
      set_cocoons p [ num ]

let eguard_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun e ->
        (match e.de_target with
        | None ->
            let d = e.de_damage in
            let ctx = d.context in
            let me = Reaction.bound_slot r in
            let attacker = me.slots.player.updater.player_updates.(ctx.ctx_player) in
            (* A creature's blow, not a spell: a spell carries the slot it
               targeted in its context (Air's Call, Fear's Strike), and that
               slot belongs to the owner, so the guard must not read it as the
               attacker.  Only a filled enemy slot answers the arc. *)
            if
              (not d.is_spell)
              && ctx.ctx_player <> me.slots.player.pid
              && ctx.ctx_selected <> no_selection
              && in_slot_range ctx.ctx_selected
              && Option.is_some attacker.slots_update.slot_updates.(ctx.ctx_selected).value
            then (
              Updater.focus me.slots.player.updater me.num me.slots.player.pid false;
              (* the guard answers with a short arc of electricity, so the board
                 shows the creature it is hitting back *)
              emit me.slots.player.updater
                (Ev_ability
                   {
                     kind = "zen.guard.zap";
                     player = me.slots.player.pid;
                     num = me.num;
                     target = Some (ctx.ctx_player, ctx.ctx_selected);
                   });
              Slot.inflict
                attacker.slots_update.slot_updates.(ctx.ctx_selected)
                (mk_damage ~is_ability:true 3
                   { ctx_player = me.slots.player.pid; ctx_card = !eguard_ref; ctx_selected = me.num }))
        | Some _ -> ());
        e.de_damage);
  }

let mimic_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num <> me.num && List.mem slot.num (cocoons me.slots.player) then (
          set_cocoons me.slots.player (List.filter (( <> ) slot.num) (cocoons me.slots.player));
          incr_mana_from me me.slots.player 2 [ (Slot.get slot).card.c_card.house_index ];
          Player.heal me.slots.player 3;
          Updater.focus me.slots.player.updater me.num me.slots.player.pid false));
    on_my_death =
      (fun dead ->
        match !mimic_ref with
        | Some mimic ->
            if Option.is_none (Slots.find_card dead.d_player.slots_update mimic) then
              set_cocoons dead.d_player []
        | None -> ());
  }

(* Inertia is immune to spells only while its Matter is still on the board;
   once Matter is gone a spell can reach it like any other creature. *)
let inertia_reaction () =
  let r = Reaction.default () in
  {
    r with
    inflict =
      (fun s d ->
        let matter_in_play =
          match !matter_ref with
          | Some matter -> Option.is_some (Slots.find_card s.slots.player.slots_update matter)
          | None -> false
        in
        if not d.is_spell || not matter_in_play then Slot.damage_slot s d);
  }

let fighter_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect = (fun d -> if is_effect d then { d with amount = max 0 (d.amount - 7) } else d);
    on_protect =
      (fun e ->
        if Option.is_none e.de_target && e.de_damage.amount > 0 then (
          let me = Reaction.bound_slot r in
          Updater.focus me.slots.player.updater me.num me.slots.player.pid false;
          (* +1 attack until the end of the owner's turn *)
          let bonus = Attack_func (fun a -> a + 1) in
          add_attack me bonus;
          Player.add_effect me.slots.player (On_end_turn, one_time_player_effect (remove_attack bonus)));
        e.de_damage);
  }

let eguard =
  creature ~key:"zen.guard" ~attack:(attack 3) ~life:22 ~description:"zen.guard.description"
    ~new_reaction:eguard_reaction ()

let () = eguard_ref := Some eguard

let mimic =
  creature ~key:"zen.mimic" ~attack:(attack 5) ~life:26 ~description:"zen.mimic.description"
    ~effects:(effects [ (On_turn, place_cocoon) ])
    ~new_reaction:mimic_reaction ()

let () = mimic_ref := Some mimic

let fighter =
  creature ~key:"zen.fighter" ~attack:(attack 5) ~life:26 ~description:"zen.fighter.description"
    ~new_reaction:fighter_reaction
    ~effects:(effects [ (On_turn, zen_effect) ])
    ()

let matter =
  creature ~key:"zen.matter" ~attack:(attack 5) ~life:30 ~description:"zen.matter.description"
    ~effects:(effects [ (Direct, matter_strike); (On_turn, matter_strike) ])
    ()

let () =
  matter.cost <- 8;
  matter_ref := Some matter

let inertia =
  creature ~key:"zen.inertia" ~attack:(attack 3) ~life:18 ~description:"zen.inertia.description"
    ~effects:(effects [ (Direct, spawn_matter); (Direct, inertia_heal); (On_turn, inertia_heal) ])
    ~new_reaction:inertia_reaction ()

let () = inertia_ref := Some inertia

let cards =
  [
    creature ~key:"zen.elementesist" ~attack:(attack 3) ~life:14 ~description:"zen.elementesist.description"
      ~run_attack:(custom_run_attack elem_attack) ();
    creature ~key:"zen.redlight" ~attack:(attack 3) ~life:18 ~description:"zen.redlight.description"
      ~effects:(effects [ (Direct, redlight_arm); (On_turn, redlight_strike) ])
      ();
    spell ~key:"zen.focus" ~description:"zen.focus.description"
      ~effects:(effects [ (Direct, focus_spell) ])
      ();
    eguard;
    creature ~key:"zen.dreamer" ~attack:(attack 4) ~life:27 ~description:"zen.dreamer.description"
      ~effects:(effects [ (On_turn, dreamer_shock) ])
      ();
    mimic;
    fighter;
    inertia;
  ]

let house = special_house ~key:"zen" ~id:6 ~additional_cards:[ matter ] ~data:(Zen_cocoons []) cards
