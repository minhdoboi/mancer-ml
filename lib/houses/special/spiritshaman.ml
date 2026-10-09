(* Special house Spirit Shaman — port of priv/sp/house/SpiritShaman.scala. Totems never attack; when a
   release condition holds they are replaced in-slot by their spirit (no summon, no Direct). *)

open Sp
open Engine
open Common

let spirit_index = 4
let bear_rally = Attack_func (fun a -> a + 2)
let panther_count = function Count n -> n | _ -> 0

let panther_growth =
  Attack_slot_state_func
    (fun a slot -> a + match slot.value with Some st -> panther_count st.data | None -> 0)

let panther_attack target d player =
  let rec loop n killed = if killed && n < 8 then loop (n + 1) (single_target_attack target d player) in
  loop 0 (single_target_attack target d player)

let tortoise_spirit =
  creature ~key:"spiritshaman.tortoise.spirit" ~attack:(attack 4) ~life:8
    ~description:"spiritshaman.tortoise.spirit.description" ()

let fox_spirit =
  creature ~key:"spiritshaman.fox.spirit" ~attack:(attack 4) ~life:17
    ~description:"spiritshaman.fox.spirit.description" ()

let deer_spirit =
  creature ~key:"spiritshaman.deer.spirit" ~attack:(attack 4) ~life:28
    ~description:"spiritshaman.deer.spirit.description" ()

let owl_spirit =
  creature ~key:"spiritshaman.owl.spirit" ~attack:(attack 5) ~life:30
    ~description:"spiritshaman.owl.spirit.description" ()

let panther_spirit =
  creature ~key:"spiritshaman.panther.spirit" ~attack:(attack 2) ~life:34
    ~description:"spiritshaman.panther.spirit.description" ~run_attack:(custom_run_attack panther_attack) ()

let bear_spirit =
  creature ~key:"spiritshaman.bear.spirit" ~attack:(attack 9) ~life:37
    ~description:"spiritshaman.bear.spirit.description" ()

let eagle_spirit =
  creature ~key:"spiritshaman.eagle.spirit" ~attack:(attack 10) ~life:32
    ~description:"spiritshaman.eagle.spirit.description" ()

let spirits =
  [ tortoise_spirit; fox_spirit; deer_spirit; owl_spirit; panther_spirit; bear_spirit; eagle_spirit ]

let () = List.iter2 (fun (c : card) cost -> c.cost <- cost) spirits [ 1; 2; 3; 4; 5; 7; 8 ]

let lowest_element (p : player_update) =
  List.fold_left
    (fun best i ->
      if p.houses_update.hu_value.(i).mana < p.houses_update.hu_value.(best).mana then i else best)
    0 [ 1; 2; 3 ]

(* Forward: totems are filled in after the cards exist. *)
let tortoise_ref = ref None
let fox_ref = ref None
let deer_ref = ref None
let owl_ref = ref None
let panther_ref = ref None
let bear_ref = ref None
let eagle_ref = ref None
let is_card opt (c : card) = match opt with Some x -> x.id = c.id | None -> false

let spirit_of (c : card) =
  if is_card !tortoise_ref c then Some tortoise_spirit
  else if is_card !fox_ref c then Some fox_spirit
  else if is_card !deer_ref c then Some deer_spirit
  else if is_card !owl_ref c then Some owl_spirit
  else if is_card !panther_ref c then Some panther_spirit
  else if is_card !bear_ref c then Some bear_spirit
  else if is_card !eagle_ref c then Some eagle_spirit
  else None

let is_totem c = Option.is_some (spirit_of c)
let is_spirit_card (c : card) = (not c.is_spell) && c.house_id = 30

let occupied (p : player_update) =
  Array.fold_left (fun n s -> if Option.is_some s.value then n + 1 else n) 0 p.slots_update.slot_updates

let ready_to_release owner (slot : slot_update) =
  match slot.value with
  | None -> false
  | Some st ->
      if is_card !owl_ref st.card.c_card then
        List.exists (fun i -> owner.houses_update.hu_value.(i).mana <= 1) [ 0; 1; 2; 3 ]
      else if is_card !panther_ref st.card.c_card then (
        Player.refresh_slots owner;
        let nums = slot.num :: List.map (fun s -> s.num) (Slot.filled_adjacent slot) in
        let total =
          List.fold_left
            (fun acc n -> acc + match owner.pstate.ps_slots.(n) with Some s -> s.attack | None -> 0)
            0 nums
        in
        total >= 12)
      else if is_card !bear_ref st.card.c_card then occupied owner + occupied (other_player owner) >= 8
      else if is_card !eagle_ref st.card.c_card then owner.houses_update.hu_value.(spirit_index).mana >= 8
      else false

let rec release (slot : slot_update) =
  match slot.value with
  | None -> ()
  | Some st -> (
      match spirit_of st.card.c_card with
      | None -> ()
      | Some spirit ->
          let owner = slot.slots.player in
          Player.refresh_slots owner;
          let totem_attack =
            match owner.pstate.ps_slots.(slot.num) with Some s -> s.attack | None -> st.attack
          in
          let totem_card = st.card.c_card in
          let carried = st.status land (run_flag lor stun_flag) in
          ignore (Slot.remove slot None);
          Slots.add_creature slot (make_creature spirit);
          Slot.update slot (fun s -> { s with status = s.status lor carried });
          emit owner.updater
            (Ev_replaced { num = slot.num; player = owner.pid; creature = (Slot.get slot).card });
          focus_slot slot;
          on_release owner slot totem_card totem_attack)

and on_release owner slot totem_card totem_attack =
  let opp = other_player owner in
  let ctx =
    { ctx_player = owner.pid; ctx_card = Some (Slot.get slot).card.c_card; ctx_selected = slot.num }
  in
  if is_card !tortoise_ref totem_card then Houses.incr_mana owner.houses_update 1 [ spirit_index ]
  else if is_card !fox_ref totem_card then
    let nums =
      Array.to_list owner.slots_update.slot_updates
      |> List.filter_map (fun s ->
          match s.value with
          | Some st when s.num <> slot.num && is_totem st.card.c_card -> Some s.num
          | _ -> None)
    in
    List.iter (fun n -> release owner.slots_update.slot_updates.(n)) nums
  else if is_card !deer_ref totem_card then
    Slots.foreach owner.slots_update (fun s -> if s.num <> slot.num then Slot.heal s 6)
  else if is_card !owl_ref totem_card then Houses.incr_mana owner.houses_update 2 [ lowest_element owner ]
  else if is_card !panther_ref totem_card then add_attack slot (Attack_func (fun a -> a + totem_attack))
  else if is_card !bear_ref totem_card then
    Slots.inflict_creatures opp.slots_update (mk_damage ~is_ability:true 8 ctx)
  else if is_card !eagle_ref totem_card then (
    Houses.incr_mana owner.houses_update (-4) [ spirit_index ];
    Houses.incr_mana owner.houses_update 2 [ 0; 1; 2; 3 ])

let check_releases (owner : player_update) =
  let nums =
    Array.to_list owner.slots_update.slot_updates
    |> List.filter_map (fun s ->
        match s.value with Some st when is_totem st.card.c_card -> Some s.num | _ -> None)
  in
  List.iter
    (fun n ->
      let s = owner.slots_update.slot_updates.(n) in
      match s.value with
      | Some st when is_totem st.card.c_card && ready_to_release owner s -> release s
      | _ -> ())
    nums

let totem_turn : card_effect =
 fun env ->
  let slot = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
  Option.iter
    (fun (st : slot_state) ->
      let ready =
        if is_card !fox_ref st.card.c_card then st.life <= 5 else ready_to_release (player_of_env env) slot
      in
      if ready then release slot)
    slot.value

let panther_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num = me.num then Slot.update me (fun st -> { st with attack = Slots.current_attack me st }));
  }

let tortoise_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if e.s_player.pid = me.slots.player.pid && is_spirit_card e.s_card.c_card then release me);
  }

let shaman_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let wrap (target : player_update) =
          let old = target.submit_command_hook in
          target.submit_command_hook <-
            (fun c ->
              let r = old c in
              check_releases p;
              r)
        in
        wrap p;
        wrap (other_player p));
  }

let tortoise =
  creature ~key:"spiritshaman.tortoise" ~attack:(attack 0) ~life:8
    ~description:"spiritshaman.tortoise.description"
    ~effects:
      (effects
         [
           ( Direct,
             fun env ->
               List.iter
                 (fun s -> match s.value with Some st when is_totem st.card.c_card -> release s | _ -> ())
                 (Slot.filled_adjacent (player_of_env env).slots_update.slot_updates.(env.env_selected)) );
         ])
    ~new_reaction:tortoise_reaction ~run_attack:never_run_attack ()

let fox =
  creature ~key:"spiritshaman.fox" ~attack:(attack 0) ~life:11 ~description:"spiritshaman.fox.description"
    ~effects:
      (effects
         [
           ( Direct,
             fun env ->
               let opposite = (other_of_env env).slots_update.slot_updates.(env.env_selected) in
               Option.iter
                 (fun (st : slot_state) ->
                   focus_slot (player_of_env env).slots_update.slot_updates.(env.env_selected);
                   Slot.inflict opposite (mk_damage ~is_ability:true st.attack (ctx_of_env env)))
                 opposite.value );
           (On_turn, totem_turn);
         ])
    ~run_attack:never_run_attack ()

let deer =
  creature ~key:"spiritshaman.deer" ~attack:(attack 0) ~life:20 ~description:"spiritshaman.deer.description"
    ~effects:
      (effects
         [
           ( On_turn,
             fun env ->
               let slot = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
               Option.iter
                 (fun _ ->
                   focus_slot slot;
                   Player.heal (player_of_env env) 1;
                   if get_mana env 3 >= 7 then release slot)
                 slot.value );
         ])
    ~run_attack:never_run_attack ()

let owl =
  creature ~key:"spiritshaman.owl" ~attack:(attack 0) ~life:15 ~description:"spiritshaman.owl.description"
    ~effects:
      (effects
         [
           ( Direct,
             fun env ->
               Houses.incr_mana (player_of_env env).houses_update 1 [ lowest_element (player_of_env env) ] );
           (On_turn, totem_turn);
         ])
    ~run_attack:never_run_attack ()

let panther =
  creature ~key:"spiritshaman.panther"
    ~attack:{ base = Some 0; sources = [ panther_growth ] }
    ~life:32 ~description:"spiritshaman.panther.description"
    ~effects:
      (effects
         [
           ( On_turn,
             fun env ->
               let slot = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
               Option.iter
                 (fun (st : slot_state) ->
                   Slot.set_data slot (Count (panther_count st.data + 1));
                   Slot.update slot (fun s -> { s with attack = Slots.current_attack slot s });
                   if ready_to_release (player_of_env env) slot then release slot)
                 slot.value );
         ])
    ~new_reaction:panther_reaction ~run_attack:never_run_attack ~data:(Count 0) ()

let bear =
  creature ~key:"spiritshaman.bear" ~attack:(attack 0) ~life:34 ~description:"spiritshaman.bear.description"
    ~effects:
      (effects
         [
           ( Direct,
             fun env ->
               let p = player_of_env env in
               focus_slot p.slots_update.slot_updates.(env.env_selected);
               Slots.foreach p.slots_update (fun s ->
                   if s.num <> env.env_selected then add_attack s bear_rally);
               Player.add_effect p (On_end_turn, one_time_player_effect (remove_attack bear_rally)) );
           (On_turn, totem_turn);
         ])
    ~run_attack:never_run_attack ()

let eagle =
  creature ~key:"spiritshaman.eagle" ~attack:(attack 0) ~life:40 ~description:"spiritshaman.eagle.description"
    ~effects:
      (effects
         [
           ( Direct,
             fun env ->
               focus_slot (player_of_env env).slots_update.slot_updates.(env.env_selected);
               Houses.incr_mana (player_of_env env).houses_update 4 [ spirit_index ] );
           (On_turn, totem_turn);
         ])
    ~run_attack:never_run_attack ()

let () =
  tortoise_ref := Some tortoise;
  fox_ref := Some fox;
  deer_ref := Some deer;
  owl_ref := Some owl;
  panther_ref := Some panther;
  bear_ref := Some bear;
  eagle_ref := Some eagle

let cards =
  [
    tortoise;
    fox;
    deer;
    owl;
    panther;
    spell ~key:"spiritshaman.awakening" ~description:"spiritshaman.awakening.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 let p = player_of_env env in
                 let amount = get_mana env spirit_index + env.env_paid_cost in
                 Player.heal p amount;
                 Slots.heal_creatures p.slots_update amount;
                 let nums =
                   Array.to_list p.slots_update.slot_updates
                   |> List.filter_map (fun s ->
                       match s.value with Some st when is_totem st.card.c_card -> Some s.num | _ -> None)
                 in
                 List.iter (fun n -> release p.slots_update.slot_updates.(n)) nums );
           ])
      ();
    bear;
    eagle;
  ]

let house =
  {
    h_key = "spiritshaman";
    h_name = "spiritshaman";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 30;
    h_additional_cards = spirits;
    h_make_listener = shaman_listener;
    h_data = No_player_data;
  }
