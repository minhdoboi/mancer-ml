(* Faerie Druid special house — port of priv/sp/house/FaerieDruid.scala. *)

open Sp
open Engine
open Common

let rabble_ref : card option ref = ref None
let nightwing_ref : card option ref = ref None
let ring_ref : card option ref = ref None
let lower1 pos = Attack_bonus { tag = "lower1"; pos; bonus = (fun a -> max 0 (a - 1)) }
let ring_bonus pos = Attack_bonus { tag = "faerie_ring"; pos; bonus = (fun a -> a + 2) }
let dragon_bonus pid = Attack_bonus { tag = "faerie_dragon"; pos = pid; bonus = (fun a -> a + 2) }

let remove_bonus bonus : card_effect =
 fun env -> Slots.foreach (player_of_env env).slots_update (fun s -> remove_first_attack s bonus)

let lower_creature_cost_mod : desc_mod =
 fun _house cards ->
  Array.map
    (fun cd -> if not cd.cd_card.is_spell then { cd with cd_cost = max 0 (cd.cd_cost - 1) } else cd)
    cards

let twin_heal : card_effect =
 fun env ->
  let p = player_of_env env in
  List.iter (fun s -> Slot.heal s 4) (Slot.adjacent p.slots_update.slot_updates.(env.env_selected))

let fire : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  let d = spell_damage env 5 in
  for n = 0 to nb_slots - 1 do
    if Option.is_some p.slots_update.slot_updates.(n).value then
      Slot.inflict op.slots_update.slot_updates.(n) d
    else Slot.stun op.slots_update.slot_updates.(n)
  done

let cloud : card_effect =
 fun env ->
  match !rabble_ref with
  | None -> ()
  | Some rabble ->
      List.iter
        (fun num ->
          if Option.is_none (player_of_env env).slots_update.slot_updates.(num).value then
            Slots.add_creature (player_of_env env).slots_update.slot_updates.(num) (make_creature rabble))
        (player_of_env env).pstate.ps_slot_list

let piper : card_effect =
 fun env ->
  let p = player_of_env env in
  List.iter (fun s -> Slot.heal s 3) (Slot.adjacent p.slots_update.slot_updates.(env.env_selected));
  Updater.focus env.env_updater env.env_selected env.env_player true

let dragon : card_effect =
 fun env ->
  let p = player_of_env env in
  let bonus = dragon_bonus p.pid in
  Slots.foreach p.slots_update (fun s ->
      Slot.heal s 20;
      if s.num <> env.env_selected then add_attack s bonus);
  Player.add_effect p (On_end_turn, one_time_player_effect (remove_bonus bonus))

let rabble_reaction () =
  let r = Reaction.default () in
  { r with on_my_death = (fun dead -> Player.heal dead.d_player 4) }

let twin_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        ignore dead;
        List.iter (fun s -> Slot.heal s 4) (Slot.adjacent me));
  }

let nightwing_temper me_num opp_slot = add_attack opp_slot (lower1 me_num)
let nightwing_untemper me_num opp_slot = remove_first_attack opp_slot (lower1 me_num)

let nightwing_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num = me.num && slot.slots.player.pid = me.slots.player.pid then
          Slots.foreach (Player.other me.slots.player).slots_update (nightwing_temper me.num));
    on_remove =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num <> me.num && slot.slots.player.pid <> me.slots.player.pid then
          nightwing_untemper me.num slot);
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        Slots.foreach (Player.other me.slots.player).slots_update (nightwing_untemper me.num));
  }

let ring_reaction () = Reaction.default ()

let unicorn_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num && me.slots.player.pid = slot.slots.player.pid then
          Player.add_desc_mod me.slots.player lower_creature_cost_mod);
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        Player.remove_desc_mod me.slots.player lower_creature_cost_mod);
  }

let faerie_listener () =
  {
    hl_on_death = (fun () -> true);
    hl_mod = Fun.id;
    hl_intercept_submit = (fun c -> (false, c));
    hl_init =
      (fun p ->
        let old_sub = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old_sub c in
            if c.cmd_card.house_index = special_house_index then
              Array.iter
                (fun s ->
                  match s.value with
                  | Some st when match !ring_ref with Some r -> st.card.c_card.id = r.id | None -> false ->
                      let bonus = ring_bonus s.num in
                      add_attack s bonus;
                      Player.add_effect p (On_end_turn, one_time_player_effect (remove_bonus bonus))
                  | _ -> ())
                p.slots_update.slot_updates;
            r);
        let opp = p.updater.player_updates.(other p.pid) in
        Array.iter
          (fun slot ->
            let old = slot.add_hook in
            slot.add_hook <-
              (fun st ->
                old st;
                Array.iter
                  (fun s ->
                    match s.value with
                    | Some v
                      when match !nightwing_ref with Some n -> v.card.c_card.id = n.id | None -> false ->
                        nightwing_temper s.num slot
                    | _ -> ())
                  p.slots_update.slot_updates))
          opp.slots_update.slot_updates);
  }

let rabble =
  creature ~key:"faerie.rabble" ~attack:(attack 2) ~life:10 ~description:"faerie.rabble.description"
    ~new_reaction:rabble_reaction ()

let twins =
  creature ~key:"faerie.twins" ~attack:(attack 4) ~life:12 ~description:"faerie.twins.description"
    ~effects:(effects [ (Direct, twin_heal) ])
    ~new_reaction:twin_reaction ()

let faerie_fire =
  spell ~key:"faerie.fire" ~description:"faerie.fire.description" ~effects:(effects [ (Direct, fire) ]) ()

let nightwing =
  creature ~key:"faerie.nightwing" ~attack:(attack 4) ~life:18 ~description:"faerie.nightwing.description"
    ~new_reaction:nightwing_reaction ()

let ring =
  creature ~key:"faerie.ring" ~attack:(attack 2) ~life:20 ~description:"faerie.ring.description"
    ~effects:(effects [ (On_turn, add_mana 1 [ 4 ]) ])
    ~new_reaction:ring_reaction ()

let piper_c =
  creature ~key:"faerie.piper" ~attack:(attack 4) ~life:28 ~description:"faerie.piper.description"
    ~effects:(effects [ (On_turn, piper) ])
    ()

let cloud_c =
  spell ~key:"faerie.cloud" ~description:"faerie.cloud.description" ~effects:(effects [ (Direct, cloud) ]) ()

let unicorn =
  creature ~key:"faerie.unicorn" ~attack:(attack 7) ~life:34 ~description:"faerie.unicorn.description"
    ~status:run_flag ~new_reaction:unicorn_reaction ()

let dragon_c =
  creature ~key:"faerie.dragon" ~attack:(attack 9) ~life:39 ~description:"faerie.dragon.description"
    ~effects:(effects [ (Direct, dragon) ])
    ()

let () =
  rabble_ref := Some rabble;
  nightwing_ref := Some nightwing;
  ring_ref := Some ring;
  rabble.cost <- 1

let cards = [ twins; faerie_fire; nightwing; ring; piper_c; cloud_c; unicorn; dragon_c ]
let house = special_house ~key:"faerie" ~id:11 ~additional_cards:[ rabble ] ~listener:faerie_listener cards
