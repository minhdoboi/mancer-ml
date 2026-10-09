(* Special house Shadowmage — port of priv/sp/house/Shadowmage.scala.

   Stealth is slot data [Stealthed], not a UI flag: Shade Stalker stays hidden
   until it takes damage or damages an enemy creature (revealing on the attack
   is a deliberate deviation from Scala, which only reveals on damage taken).
   Eclipse's 30-point player shield lives in [Shadowmage_data] and is applied through [guard_hook]. *)

open Sp
open Engine
open Common

let assassin_ref : card option ref = ref None
let initial_data = { eclipse_shield = 0 }
let get_data p = match p.pstate.ps_data with Shadowmage_data d -> d | _ -> initial_data
let set_data p d = Player.set_data p (Shadowmage_data d)
let is_hidden (st : slot_state) = st.data = Stealthed

let visible_creatures (p : player_state) =
  List.filter_map
    (fun n -> match p.ps_slots.(n) with Some st when not (is_hidden st) -> Some n | _ -> None)
    p.ps_slot_list

let is_hidden_from (st : slot_state) ~owner ~viewer = owner <> viewer && is_hidden st

let assassin_slots pid (state : game_state) =
  let p = state.players.(pid) in
  List.filter
    (fun n -> match p.ps_slots.(n) with Some st -> st.card.c_card.cost <= 9 | None -> true)
    p.ps_slot_list

let hide_house_mod house_index : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> house_index then cards
  else Array.map (fun cd -> { cd with cd_enabled = false }) cards

let archer_shot : card_effect =
 fun env ->
  focus_slot (own_slot_of_env env);
  Player.inflict (other_of_env env) (ability_damage env 6)

let weave : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  let ids =
    List.filter_map
      (fun s ->
        match s.value with
        | Some st when not (has_flag st invincible_flag) ->
            Slot.toggle s invincible_flag;
            Some st.slot_id
        | _ -> None)
      (Slot.filled_adjacent slot)
  in
  if ids <> [] then
    let fade : card_effect =
     fun e ->
      List.iter
        (fun id ->
          Option.iter
            (fun s -> Slot.toggle_off s invincible_flag)
            (Slots.find_slot_id (other_of_env e).slots_update id))
        ids
    in
    Player.add_effect (other_of_env env) (On_end_turn, one_time_player_effect fade)

let silence : card_effect =
 fun env ->
  let opp = Slot.opposite (own_slot_of_env env) in
  Option.iter
    (fun (st : slot_state) ->
      focus_slot (own_slot_of_env env);
      Slot.stun opp;
      let mod_ = hide_house_mod st.card.c_card.house_index in
      let op = other_of_env env in
      Player.add_desc_mod op mod_;
      Player.add_effect op (On_end_turn, one_time_player_effect (remove_desc_mod mod_)))
    opp.value

let eclipse_cast : card_effect =
 fun env ->
  let p = player_of_env env in
  Houses.incr_mana p.houses_update 2 [ 0; 1; 2; 3 ];
  set_data p { eclipse_shield = 30 };
  let fade : card_effect = fun e -> set_data (other_of_env e) { eclipse_shield = 0 } in
  Player.add_effect (other_of_env env) (On_end_turn, one_time_player_effect fade)

let spike_of_pain : card_effect =
 fun env ->
  Slots.inflict_creatures (other_of_env env).slots_update (spell_damage env 16);
  let fade : card_effect = fun e -> Slots.heal_creatures (other_of_env e).slots_update 16 in
  Player.add_effect (player_of_env env) (On_end_turn, one_time_player_effect fade)

let assassinate : card_effect =
 fun env ->
  focus_slot (own_slot_of_env env);
  Slot.inflict (Slot.opposite (own_slot_of_env env)) (ability_damage env 12)

let reset_warden : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with Some st when st.data = Absorbed -> Slot.set_data slot No_data | _ -> ()

let archer_reaction () =
  let r = Reaction.default () in
  { r with on_my_death = (fun dead -> Player.heal (other_player dead.d_player) 6) }

let stalker_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun damage ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some st when damage.amount > 0 && st.data = Stealthed -> Slot.set_data me No_data
        | _ -> ());
  }

(* Striking from the shadows breaks cover: a normal single-target attack, but
   the stalker is revealed when it actually damages an enemy creature (a kill
   counts; a fully absorbed hit or a direct hit on the player does not). *)
let stalker_attack target d player =
  let other = other_player player in
  let before =
    List.filter_map
      (fun n ->
        match other.slots_update.slot_updates.(n).value with Some st -> Some (n, st.life) | None -> None)
      target
  in
  ignore (single_target_attack target d player);
  let damaged =
    List.exists
      (fun (num, life) ->
        match other.slots_update.slot_updates.(num).value with
        | Some st -> st.life < life
        | None -> true)
      before
  in
  if damaged then
    let me = player.slots_update.slot_updates.(d.context.ctx_selected) in
    match me.value with
    | Some st when st.data = Stealthed && st.card.c_card.key = "shadowmage.stalker" ->
        Slot.set_data me No_data
    | _ -> ()

let warden_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some st when d.amount > 0 && st.data <> Absorbed ->
            Slot.set_data me Absorbed;
            { d with amount = 0 }
        | _ -> d);
  }

let absorb_eclipse p d =
  let data = get_data p in
  if data.eclipse_shield > 0 && d.amount > 0 then (
    let absorbed = min data.eclipse_shield d.amount in
    set_data p { eclipse_shield = data.eclipse_shield - absorbed };
    { d with amount = d.amount - absorbed })
  else d

let shadowmage_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let old_guard = p.guard_hook in
        p.guard_hook <- (fun d -> absorb_eclipse p (old_guard d));
        let old_submit = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            if is_card !assassin_ref c.cmd_card then
              Option.iter
                (fun n ->
                  match p.slots_update.slot_updates.(n).value with
                  | Some st ->
                      let gain = (st.card.c_card.cost + 1) / 2 in
                      if gain > 0 then Houses.incr_mana p.houses_update gain [ st.card.c_card.house_index ]
                  | None -> ())
                c.cmd_input;
            old_submit c));
  }

let assassin =
  creature ~key:"shadowmage.assassin" ~attack:(attack 9) ~life:30
    ~description:"shadowmage.assassin.description" ~input_spec:(Select_owner assassin_slots)
    ~effects:(effects [ (Direct, assassinate) ])
    ()

let () = assassin_ref := Some assassin

let cards =
  [
    creature ~key:"shadowmage.archer" ~attack:(attack 4) ~life:11 ~description:"shadowmage.archer.description"
      ~effects:(effects [ (Direct, archer_shot) ])
      ~new_reaction:archer_reaction ();
    creature ~key:"shadowmage.stalker" ~attack:(attack 5) ~life:17
      ~description:"shadowmage.stalker.description" ~data:Stealthed ~new_reaction:stalker_reaction
      ~run_attack:(custom_run_attack stalker_attack) ();
    creature ~key:"shadowmage.weaver" ~attack:(attack 4) ~life:18 ~description:"shadowmage.weaver.description"
      ~effects:(effects [ (Direct, weave) ])
      ();
    creature ~key:"shadowmage.silencer" ~attack:(attack 5) ~life:27
      ~description:"shadowmage.silencer.description"
      ~effects:(effects [ (Direct, silence) ])
      ();
    spell ~key:"shadowmage.eclipse" ~description:"shadowmage.eclipse.description"
      ~effects:(effects [ (Direct, eclipse_cast) ])
      ();
    spell ~key:"shadowmage.spike" ~description:"shadowmage.spike.description"
      ~effects:(effects [ (Direct, spike_of_pain) ])
      ();
    creature ~key:"shadowmage.warden" ~attack:(attack 7) ~life:45 ~description:"shadowmage.warden.description"
      ~effects:(effects [ (On_turn, reset_warden); (On_end_turn, reset_warden) ])
      ~new_reaction:warden_reaction ();
    assassin;
  ]

let house =
  special_house ~key:"shadowmage" ~id:22 ~listener:shadowmage_listener ~data:(Shadowmage_data initial_data)
    cards
