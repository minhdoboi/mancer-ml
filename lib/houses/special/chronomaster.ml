(* Chronomaster special house — port of priv/sp/house/Chronomaster.scala. *)

open Sp
open Engine
open Common

let king_phase = "chronomaster.king.phase"
let unblockable_attack _target d player = Player.inflict (Player.other player) d
let get_chrono p = match p.pstate.ps_data with Chrono_data d -> d | _ -> { previous = None; current = None }
let set_chrono p d = Player.set_data p (Chrono_data d)

let slot_snaps (p : player_update) =
  Array.to_list p.slots_update.slot_updates
  |> List.filter_map (fun s ->
      Option.map
        (fun st ->
          {
            ss_num = s.num;
            ss_card = st.card;
            ss_life = st.life;
            ss_max_life = st.max_life;
            ss_status = st.status;
            ss_attack_sources = st.attack_sources;
            ss_target = st.target;
            ss_data = st.data;
          })
        s.value)

let take_snap (chrono : player_update) =
  { bs_owner = slot_snaps chrono; bs_opp = slot_snaps (Player.other chrono) }

let snapshot_for chrono_id : card_effect =
 fun env ->
  let chrono = env.env_updater.player_updates.(chrono_id) in
  match chrono.pstate.ps_data with
  | Chrono_data _ ->
      let now = take_snap chrono in
      let d = get_chrono chrono in
      set_chrono chrono { previous = d.current; current = Some now }
  | _ -> ()

let grant_extra slot n = if n > 0 && Option.is_some slot.value then add_attack slot (Extra_attacks n)

let clear_extra_attacks : card_effect =
 fun env ->
  Slots.foreach (player_of_env env).slots_update (fun s ->
      match s.value with
      | None -> ()
      | Some st ->
          List.iter
            (function Extra_attacks _ as e -> remove_first_attack s e | _ -> ())
            st.attack_sources.sources)

let freeze_effect : card_effect =
 fun env -> Slot.stun (other_of_env env).slots_update.slot_updates.(env.env_selected)

let accelerate_effect : card_effect =
 fun env ->
  let slot = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
  let times = Dice.int env.env_updater.rng 2 + 2 in
  grant_extra slot (times - 1);
  Player.add_effect (player_of_env env) (On_end_turn, one_time_player_effect clear_extra_attacks)

let aging_effect : card_effect =
 fun env -> add_attack (other_of_env env).slots_update.slot_updates.(env.env_selected) Aging_malus

let queen_effect : card_effect =
 fun env ->
  Slots.foreach (player_of_env env).slots_update (fun s -> grant_extra s 1);
  Player.add_effect (player_of_env env) (On_end_turn, one_time_player_effect clear_extra_attacks)

let king_effect : card_effect =
 fun env ->
  Player.add_effect (player_of_env env)
    ( On_turn,
      one_time_player_effect (fun e ->
          Player.add_transition (player_of_env e) (Wait_player (e.env_player, Some king_phase))) )

let filled_nums p =
  Array.to_list p.slots_update.slot_updates
  |> List.filter_map (fun s -> if Option.is_some s.value then Some s.num else None)

let clear_side p =
  List.iter (fun n -> Slot.remove p.slots_update.slot_updates.(n) None |> ignore) (filled_nums p)

let fill_side p snaps =
  List.sort (fun a b -> compare a.ss_num b.ss_num) snaps
  |> List.iter (fun snap -> Slots.add_creature p.slots_update.slot_updates.(snap.ss_num) snap.ss_card)

let write_side p snaps =
  List.iter
    (fun snap ->
      let slot = p.slots_update.slot_updates.(snap.ss_num) in
      let sources =
        {
          snap.ss_attack_sources with
          sources =
            List.filter (function Extra_attacks _ -> false | _ -> true) snap.ss_attack_sources.sources;
        }
      in
      Slot.update slot (fun st ->
          {
            st with
            life = snap.ss_life;
            max_life = snap.ss_max_life;
            status = snap.ss_status;
            attack_sources = sources;
            target = snap.ss_target;
            data = snap.ss_data;
          }))
    snaps

let restore_board chrono snap =
  let op = Player.other chrono in
  clear_side chrono;
  clear_side op;
  fill_side chrono snap.bs_owner;
  fill_side op snap.bs_opp;
  write_side chrono snap.bs_owner;
  write_side op snap.bs_opp

let groundhog_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  let snap = match get_chrono p with { previous = Some s; _ } -> s | _ -> empty_board_snap in
  restore_board p snap;
  Houses.incr_mana p.houses_update (-1) [ 0; 1; 2; 3; 4 ];
  Houses.incr_mana op.houses_update (-1) [ 0; 1; 2; 3; 4 ]

let init_snapshots : card_effect =
 fun env ->
  let snap = snapshot_for env.env_player in
  snap env;
  Player.add_effect (player_of_env env) (On_turn, snap);
  Player.add_effect (other_of_env env) (On_turn, snap)

let distortion_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun e -> match e.de_target with None -> { e.de_damage with amount = 0 } | Some _ -> e.de_damage);
  }

let cards =
  [
    spell ~key:"chronomaster.freeze" ~description:"chronomaster.freeze.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, freeze_effect) ])
      ();
    creature ~key:"chronomaster.sprites" ~attack:(attack 5) ~life:5
      ~description:"chronomaster.sprites.description"
      ~run_attack:(custom_run_attack unblockable_attack)
      ();
    spell ~key:"chronomaster.accelerate" ~description:"chronomaster.accelerate.description"
      ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, accelerate_effect) ])
      ();
    creature ~key:"chronomaster.wall" ~attack:(attack 0) ~life:17 ~description:"chronomaster.wall.description"
      ~new_reaction:distortion_reaction ();
    spell ~key:"chronomaster.aging" ~description:"chronomaster.aging.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, aging_effect) ])
      ();
    spell ~key:"chronomaster.groundhog" ~description:"chronomaster.groundhog.description"
      ~effects:(effects [ (Direct, groundhog_effect) ])
      ();
    creature ~key:"chronomaster.queen" ~attack:(attack 6) ~life:30
      ~description:"chronomaster.queen.description"
      ~effects:(effects [ (Direct, queen_effect) ])
      ();
    creature ~key:"chronomaster.king" ~attack:(attack 6) ~life:40 ~description:"chronomaster.king.description"
      ~effects:(effects [ (Direct, king_effect) ])
      ();
  ]

let house =
  {
    h_key = "chronomaster";
    h_name = "chronomaster";
    h_cards = cards;
    h_index = 4;
    h_effects = [ (On_start, init_snapshots) ];
    h_house_id = 8;
    h_additional_cards = [];
    h_make_listener = (fun () -> default_listener);
    h_data = Chrono_data { previous = None; current = None };
  }
