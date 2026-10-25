(* Special house Limbo — port of priv/sp/house/Limbo.scala.

   Combat (and Baron, even from spells) that would kill a creature instead leaves it at 0 life in
   [Limbo_state] until the owner's end of turn. [delayed_destroy_hook] is the interception point; the
   engine now routes lethal damage through it. [No_limbo] is the adjudicator's "cannot enter limbo"
   flag, and also the player-data value that makes a second lethal hit after a Baron save actually
   end the game. *)

open Sp
open Engine
open Common

let heaven_ref : card option ref = ref None
let hell_ref : card option ref = ref None
let baron_ref : card option ref = ref None
let wraith_ref : card option ref = ref None
let double_attack = Attack_slot_state_func (fun a _ -> a * 2)
let is_limbo_state = function Limbo_state -> true | _ -> false
let is_no_limbo = function No_limbo -> true | _ -> false

let wraith_attack =
  Attack_state_func
    (fun a p ->
      a
      + Array.fold_left
          (fun n s -> match s.value with Some st when is_limbo_state st.data -> n + 1 | _ -> n)
          0 p.slots_update.slot_updates
      + match p.pstate.ps_data with Limbo_player `Limbo -> 1 | _ -> 0)

let first_special_card (p : player_update) =
  match
    Array.find_opt (fun h -> h.phd_house.h_index = special_house_index) p.pstate.ps_desc.init_desc.pd_houses
  with
  | None -> None
  | Some h ->
      let cards = List.fold_left (fun cs m -> m h cs) h.phd_cards p.pstate.ps_desc.desc_mods in
      if Array.length cards = 0 then None else Some cards.(0).cd_card

let nether_grasp : card_effect =
 fun env ->
  let amount = ref 0 in
  Slots.foldl (player_of_env env).slots_update () (fun () s ->
      Slot.inflict s (mk_damage 100 (ctx_of_env env));
      amount := !amount + 3);
  Slots.inflict_creatures (other_of_env env).slots_update (mk_damage ~is_spell:true !amount (ctx_of_env env))

let redeem : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with
  | Some st when is_limbo_state st.data ->
      let card = st.card in
      Slot.destroy slot;
      Slots.add_creature slot card
  | _ -> ()

let clean_limbo : card_effect =
 fun env ->
  let p = player_of_env env in
  Slots.foreach p.slots_update (fun s ->
      match s.value with
      | Some st when is_limbo_state st.data ->
          Slot.set_data s No_limbo;
          Slot.inflict s (mk_damage 100 (ctx_of_env env))
      | _ -> ());
  match p.pstate.ps_data with
  | Limbo_player `Limbo ->
      Player.set_data p (Limbo_player `No_limbo);
      Player.inflict p (mk_damage 0 (ctx_of_env env))
  | _ -> ()

let run_limbo_reactions (owner : player_update) (entering : slot_update) =
  Slots.foreach owner.slots_update (fun slot ->
      match slot.value with Some st -> st.reaction.on_limbo entering | None -> ())

(* LimboReaction.onLimbo: keepers/tower/adjudicator. Owner-death broadcast is not installed. *)
let heaven_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_limbo =
      (fun _ ->
        let me = Reaction.bound_slot r in
        let p = me.slots.player in
        Player.heal p 2;
        Slots.heal_creatures p.slots_update 2;
        focus_slot me);
  }

let hell_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_limbo =
      (fun _ ->
        let me = Reaction.bound_slot r in
        let op = other_player me.slots.player in
        (* Scala heals the opponent; the card text says damage. Port the code. *)
        Player.heal op 2;
        Slots.inflict_creatures op.slots_update
          (mk_damage ~is_ability:true 2
             { ctx_player = me.slots.player.pid; ctx_card = !hell_ref; ctx_selected = me.num });
        focus_slot me);
  }

let tower_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_limbo =
      (fun entering ->
        let me = Reaction.bound_slot r in
        let idx = (Slot.get entering).card.c_card.house_index in
        incr_mana_from me me.slots.player 1 [ idx ];
        incr_mana_from me (other_player me.slots.player) (-1) [ idx ];
        focus_slot me);
  }

let soul_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_limbo =
      (fun _ ->
        let me = Reaction.bound_slot r in
        match first_special_card me.slots.player with
        | Some c when is_card !heaven_ref c || is_card !hell_ref c -> (
            let empty = List.filter (fun s -> Option.is_none s.value) (Slot.adjacent me) in
            match Dice.shuffle me.slots.player.updater.rng empty with
            | dest :: _ ->
                Slots.add_creature dest (make_creature c);
                Slot.set_data dest No_limbo;
                focus_slot me
            | [] -> ())
        | _ -> ());
  }

let has_baron (p : player_update) =
  Array.exists
    (fun s -> match s.value with Some st -> is_card !baron_ref st.card.c_card | None -> false)
    p.slots_update.slot_updates

let set_limbo (s : slot_update) =
  Slot.update s (fun st -> { st with life = 0 });
  Slot.set_data s Limbo_state;
  run_limbo_reactions s.slots.player s

let limbo_listener () =
  let player = ref None in
  let get () = match !player with Some p -> p | None -> failwith "limbo listener not inited" in
  {
    default_listener with
    hl_on_death =
      (fun () ->
        let p = get () in
        match p.pstate.ps_data with
        | Limbo_player `No_limbo -> true
        | _ -> (
            if has_baron p then (
              Player.set_data p (Limbo_player `Limbo);
              p.pstate.ps_life <- 0;
              false)
            else match p.pstate.ps_data with Limbo_player `Limbo -> false | _ -> true));
    hl_init =
      (fun p ->
        player := Some p;
        Array.iter
          (fun (slot : slot_update) ->
            let old_delayed = slot.delayed_destroy_hook in
            slot.delayed_destroy_hook <-
              (fun d ->
                match slot.value with
                | None -> ()
                | Some st ->
                    if (is_card !baron_ref st.card.c_card || not (is_effect d)) && not (is_no_limbo st.data)
                    then (
                      let was_limbo = is_limbo_state st.data in
                      set_limbo slot;
                      if is_card !baron_ref st.card.c_card && not was_limbo then add_attack slot double_attack)
                    else old_delayed d);
            let old_over = slot.overridable_destroy in
            slot.overridable_destroy <-
              (fun () ->
                match slot.value with
                | None -> old_over ()
                | Some st ->
                    if is_card !baron_ref st.card.c_card && not (is_limbo_state st.data) then (
                      set_limbo slot;
                      add_attack slot double_attack)
                    else old_over ()))
          p.slots_update.slot_updates);
  }

let heaven =
  creature ~key:"limbo.heaven" ~attack:(attack 3) ~life:7 ~description:"limbo.heaven.description"
    ~new_reaction:heaven_reaction ()

let hell =
  creature ~key:"limbo.hell" ~attack:(attack 3) ~life:10 ~description:"limbo.hell.description"
    ~new_reaction:hell_reaction ()

let baron = creature ~key:"limbo.baron" ~attack:(attack 6) ~life:36 ~description:"limbo.baron.description" ()

let wraith =
  creature ~key:"limbo.wraith"
    ~attack:{ base = Some 1; sources = [ wraith_attack ] }
    ~life:14 ~description:"limbo.wraith.description" ~run_attack:multi_run_attack ()

let () =
  heaven_ref := Some heaven;
  hell_ref := Some hell;
  baron_ref := Some baron;
  wraith_ref := Some wraith

let cards =
  [
    heaven;
    hell;
    wraith;
    creature ~key:"limbo.tower" ~attack:(attack 0) ~life:21 ~description:"limbo.tower.description"
      ~new_reaction:tower_reaction ();
    spell ~key:"limbo.nether" ~description:"limbo.nether.description"
      ~effects:(effects [ (Direct, nether_grasp) ])
      ();
    creature ~key:"limbo.adjudicator" ~attack:(attack 3) ~life:21 ~description:"limbo.adjudicator.description"
      ~new_reaction:soul_reaction ();
    spell ~key:"limbo.redemption" ~description:"limbo.redemption.description"
      ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, redeem) ])
      ();
    baron;
  ]

let house =
  special_house ~key:"limbo" ~id:24 ~effects:[ (On_end_turn, clean_limbo) ] ~listener:limbo_listener cards
