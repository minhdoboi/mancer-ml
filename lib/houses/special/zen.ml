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

let redlight_attack target d player =
  match target with
  | [] -> ()
  | num :: _ ->
      let op = Player.other player in
      List.iter
        (fun n ->
          match op.slots_update.slot_updates.(n).value with
          | None -> if n = num then Player.inflict op d
          | Some _ -> Slot.inflict op.slots_update.slot_updates.(n) d)
        (slot_interval (num - 1) (num + 1))

let focus_spell : card_effect =
 fun env ->
  let p = player_of_env env in
  List.iter
    (fun num ->
      match p.slots_update.slot_updates.(num).value with
      | None -> ()
      | Some st ->
          let amount = if num = env.env_selected then 2 else 1 in
          Houses.incr_mana p.houses_update amount [ st.card.c_card.house_index ])
    (slot_interval (env.env_selected - 1) (env.env_selected + 1))

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
      Option.iter
        (fun s ->
          Updater.focus env.env_updater s.num env.env_player false;
          Slot.heal s 3)
        (reduce_filled (player_of_env env).slots_update lowest_life)
  | _ -> ()

let matter_strike : card_effect =
 fun env ->
  match !inertia_ref with
  | Some inertia when owner_has env inertia ->
      Option.iter
        (fun s ->
          spark env s;
          Slot.inflict s (mk_damage ~is_ability:true 3 (ctx_of_env env)))
        (reduce_filled (other_of_env env).slots_update strongest)
  | _ -> ()

let zen_effect : card_effect =
 fun env ->
  let slot = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
  match (slot.value, Slot.opposite slot) with
  | Some mine, opp ->
      Option.iter
        (fun opp_st ->
          if
            opp_st.attack > mine.attack || opp_st.life > mine.life
            || opp_st.card.c_card.cost > mine.card.c_card.cost
          then (
            Houses.incr_mana (player_of_env env).houses_update 1 [ opp_st.card.c_card.house_index ];
            Slot.heal slot 3))
        opp.value
  | _ -> ()

let dreamer_shock : card_effect =
 fun env ->
  Option.iter
    (fun s ->
      spark env s;
      Slot.inflict s (mk_damage ~is_ability:true 4 (ctx_of_env env)))
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
            let ctx = e.de_damage.context in
            if ctx.ctx_selected <> no_selection then (
              let me = Reaction.bound_slot r in
              Updater.focus me.slots.player.updater me.num me.slots.player.pid false;
              let attacker = me.slots.player.updater.player_updates.(ctx.ctx_player) in
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
          Houses.incr_mana me.slots.player.houses_update 2 [ (Slot.get slot).card.c_card.house_index ];
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

let inertia_reaction () =
  let r = Reaction.default () in
  { r with inflict = (fun s d -> if not d.is_spell then Slot.damage_slot s d) }

let fighter_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect = (fun d -> if is_effect d then { d with amount = max 0 (d.amount - 5) } else d);
    on_protect =
      (fun e ->
        if Option.is_none e.de_target && e.de_damage.amount > 0 then (
          let me = Reaction.bound_slot r in
          Updater.focus me.slots.player.updater me.num me.slots.player.pid false;
          r.heal me 1);
        e.de_damage);
  }

let eguard =
  creature ~key:"zen.guard" ~attack:(attack 3) ~life:19 ~description:"zen.guard.description"
    ~new_reaction:eguard_reaction ()

let () = eguard_ref := Some eguard

let mimic =
  creature ~key:"zen.mimic" ~attack:(attack 5) ~life:26 ~description:"zen.mimic.description"
    ~effects:(effects [ (On_turn, place_cocoon) ])
    ~new_reaction:mimic_reaction ()

let () = mimic_ref := Some mimic

let fighter =
  creature ~key:"zen.fighter" ~attack:(attack 5) ~life:22 ~description:"zen.fighter.description"
    ~new_reaction:fighter_reaction
    ~effects:(effects [ (On_turn, zen_effect) ])
    ()

let matter =
  creature ~key:"zen.matter" ~attack:(attack 6) ~life:33 ~description:"zen.matter.description"
    ~effects:(effects [ (Direct, matter_strike); (On_turn, matter_strike) ])
    ()

let () =
  matter.cost <- 8;
  matter_ref := Some matter

let inertia =
  creature ~key:"zen.inertia" ~attack:(attack 4) ~life:22 ~description:"zen.inertia.description"
    ~effects:(effects [ (Direct, spawn_matter); (Direct, inertia_heal); (On_turn, inertia_heal) ])
    ~new_reaction:inertia_reaction ()

let () = inertia_ref := Some inertia

let cards =
  [
    creature ~key:"zen.elementesist" ~attack:(attack 3) ~life:14 ~description:"zen.elementesist.description"
      ~run_attack:(custom_run_attack elem_attack) ();
    creature ~key:"zen.redlight" ~attack:(attack 2) ~life:14 ~description:"zen.redlight.description"
      ~run_attack:(custom_run_attack redlight_attack) ();
    spell ~key:"zen.focus" ~description:"zen.focus.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, focus_spell) ])
      ();
    eguard;
    creature ~key:"zen.dreamer" ~attack:(attack 5) ~life:24 ~description:"zen.dreamer.description"
      ~effects:(effects [ (On_turn, dreamer_shock) ])
      ();
    mimic;
    fighter;
    inertia;
  ]

let house =
  {
    h_key = "zen";
    h_name = "zen";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 6;
    h_additional_cards = [ matter ];
    h_make_listener = (fun () -> default_listener);
    h_data = Zen_cocoons [];
  }
