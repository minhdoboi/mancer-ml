(* Mechanics for Conjurer, Quantumancer, Meteomancer and Seasons. *)

open Mancer
open Sp
open Engine
open Common
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit ?(is_ability = false) ?(is_spell = false) ?card u pid num amount =
  let c = match card with Some k -> Some (card_by_key k) | None -> None in
  step u (fun () ->
      Slot.inflict (slot u pid num)
        (mk_damage ~is_ability ~is_spell amount { (ctx (1 - pid)) with ctx_card = c }))

let kill u pid num = hit u pid num 1000
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let settle u = step u (fun () -> ())
let ten = [ 10; 10; 10; 10; 10 ]
let flagged u pid num flag = has_flag (Option.get (slot u pid num).value) flag

let run_at u pid num =
  step u (fun () -> Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let cost_of u pid key =
  let pd = (player u pid).pstate.ps_desc in
  Array.fold_left
    (fun acc h ->
      let cards = List.fold_left (fun cs m -> m h cs) h.phd_cards pd.desc_mods in
      match Array.find_opt (fun cd -> cd.cd_card.key = key) cards with Some cd -> cd.cd_cost | None -> acc)
    (-1) pd.init_desc.pd_houses

let enabled_of u pid key =
  let pd = (player u pid).pstate.ps_desc in
  Array.exists
    (fun h ->
      let cards = List.fold_left (fun cs m -> m h cs) h.phd_cards pd.desc_mods in
      Array.exists (fun cd -> cd.cd_card.key = key && cd.cd_enabled) cards)
    pd.init_desc.pd_houses

let called u pid = match (player u pid).pstate.ps_data with Meteo_data d -> d.mt_called | _ -> None
let hidden u pid = match (player u pid).pstate.ps_data with Meteo_data d -> d.mt_hidden | _ -> false
let fate u pid = match (player u pid).pstate.ps_data with Meteo_data d -> d.mt_fate | _ -> -1
let midnight u pid = match (player u pid).pstate.ps_data with Season_data d -> d.se_midnight | _ -> false
let played_keys u pid =
  match (player u pid).pstate.ps_data with
  | Quantum_data d -> List.map (fun ps -> ps.ps_card.key) d.q_played
  | _ -> []

let wait_name u pid =
  List.find_map (function Wait_player (p, n) when p = pid -> n | _ -> None) (player u pid).pstate.ps_transitions

(* ---------------------------------------------------------------- conjurer *)

let nectar_heals_any () =
  let u = new_game ~mana:ten ~special:Conjurer.house () in
  place u 0 "fire.goblin" 2;
  place u 1 "earth.sprite" 3;
  hit ~is_ability:true u 0 2 6;
  play u 0 "conjurer.nectar" ~input:(Some 2) ~cost:1;
  check_int "own life" 15 (life_at u 0 2);
  check_int "fire" 11 (mana u 0 0);
  let v = new_game ~mana:ten ~special:Conjurer.house () in
  place v 0 "fire.goblin" 2;
  place v 1 "earth.sprite" 3;
  play v 0 "conjurer.nectar" ~input:(Some 3) ~cost:1 ~flag:(Some Target_other);
  check_int "opp life" 22 (life_at v 1 3);
  check_int "earth" 11 (mana v 0 3)

let bravery_skips_walls () =
  let u = new_game ~mana:ten ~special:Conjurer.house () in
  place u 0 "fire.goblin" 1;
  place u 0 "fire.wall" 2;
  play u 0 "conjurer.bravery" ~input:None ~cost:2;
  check_int "goblin" 5 (attack_at u 0 1);
  check_int "wall" 0 (attack_at u 0 2);
  check_int "special" 9 (mana u 0 4);
  on_end u 0;
  check_int "faded" 4 (attack_at u 0 1)

let war_mage_buffs_twice () =
  let u = new_game ~mana:ten ~special:Conjurer.house () in
  place u 0 "fire.goblin" 1;
  play u 0 "conjurer.warmage" ~input:(Some 2) ~cost:3;
  check_int "goblin" 6 (attack_at u 0 1);
  check_int "mage" 3 (attack_at u 0 2);
  play u 0 "conjurer.bravery" ~input:None ~cost:2;
  check_int "goblin conjure" 9 (attack_at u 0 1);
  check_int "mage conjure" 4 (attack_at u 0 2)

let bone_golem_ignores_effects () =
  let u = new_game ~mana:ten ~special:Conjurer.house () in
  place u 0 "conjurer.golem" 2;
  place u 0 "fire.goblin" 1;
  hit ~is_ability:true u 0 1 6;
  hit ~is_spell:true u 0 2 10;
  hit ~is_ability:true u 0 2 10;
  hit u 0 2 4;
  check_int "golem" 14 (life_at u 0 2);
  play u 0 "conjurer.bravery" ~input:None ~cost:2;
  check_int "grown" 6 (attack_at u 0 2);
  check_int "healed goblin" 13 (life_at u 0 1)

let nurture_stuns_then_heals () =
  let u = new_game ~mana:ten ~special:Conjurer.house () in
  place u 0 "fire.goblin" 1;
  place u 0 "conjurer.warmage" 2;
  hit ~is_ability:true u 0 1 10;
  play u 0 "conjurer.nurture" ~input:None ~cost:5;
  check_bool "goblin stun" true (flagged u 0 1 stun_flag);
  check_bool "mage stun" true (flagged u 0 2 stun_flag);
  check_int "goblin" 16 (life_at u 0 1);
  check_int "player" 71 (player_life u 0)

let pain_scales () =
  let u = new_game ~mana:ten ~special:Conjurer.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "conjurer.pain" ~input:(Some 2) ~cost:6;
  check_bool "dead" true (is_empty u 1 2);
  check_int "player" 37 (player_life u 1)

let unholy_guard () =
  let u = new_game ~mana:ten ~special:Conjurer.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "conjurer.guard" ~input:(Some 1) ~cost:7;
  check_int "eot" 1 (attack_at u 1 2);
  on_end u 1;
  check_int "restored" 4 (attack_at u 1 2);
  play u 0 "conjurer.bravery" ~input:None ~cost:2;
  check_int "permanent" 3 (attack_at u 1 2)

let dark_mystic () =
  let u = new_game ~mana:ten ~special:Conjurer.house () in
  play u 0 "conjurer.mystic" ~input:(Some 2) ~cost:8;
  check_int "fire" 12 (mana u 0 0);
  check_int "water" 12 (mana u 0 1);
  check_int "air" 12 (mana u 0 2);
  check_int "earth" 12 (mana u 0 3);
  check_int "special" 2 (mana u 0 4);
  play u 0 "conjurer.bravery" ~input:None ~cost:2;
  check_int "conjuration" 3 (mana u 0 4)

(* ------------------------------------------------------------ quantumancer *)

let surge_floors_at_three () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  place u 1 "fire.goblin" 0;
  place u 1 "water.sprite" 1;
  play u 0 "quantumancer.surge" ~input:None ~cost:1;
  check_int "fire 10" 6 (life_at u 1 0);
  let v = new_game ~mana:ten ~special:Quantumancer.house () in
  place v 1 "fire.goblin" 0;
  place v 1 "water.sprite" 1;
  step v (fun () -> Houses.incr_mana (player v 1).houses_update (-10) [ 0 ]);
  play v 0 "quantumancer.surge" ~input:None ~cost:1;
  check_int "floor" 13 (life_at v 1 0);
  check_int "water" 12 (life_at v 1 1)

let kineticist_eats_effects () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.kineticist" ~input:(Some 0) ~cost:2;
  hit ~is_ability:true u 0 0 10;
  check_int "healed" 17 (life_at u 0 0);
  check_int "grown" 3 (attack_at u 0 0);
  hit u 0 0 5;
  check_int "hurt" 12 (life_at u 0 0);
  check_int "still 3" 3 (attack_at u 0 0)

let dimension_mirrors_hits () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.warrior" ~input:(Some 0) ~cost:3;
  place u 1 "fire.goblin" 0;
  step u (fun () -> Slot.toggle (slot u 0 0) run_flag);
  run_at u 0 0;
  check_int "goblin" 12 (life_at u 1 0);
  check_int "player" 56 (player_life u 1)

let dimension_empty_is_once () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.warrior" ~input:(Some 0) ~cost:3;
  step u (fun () -> Slot.toggle (slot u 0 0) run_flag);
  run_at u 0 0;
  check_int "once" 56 (player_life u 1)

let martyr_absorbs_wave () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.martyr" ~input:(Some 0) ~cost:4;
  place u 0 "fire.goblin" 1;
  play u 1 "fire.wave" ~input:None ~cost:6;
  check_int "martyr" 2 (life_at u 0 0);
  check_int "goblin" 16 (life_at u 0 1)

let martyr_skips_owner_mods () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.martyr" ~input:(Some 0) ~cost:4;
  place u 0 "fire.goblin" 1;
  place u 0 "air.fairy" 2;
  hit ~is_spell:true u 0 1 5;
  check_int "martyr" 15 (life_at u 0 0);
  check_int "goblin" 16 (life_at u 0 1)

let phosphor_retaliates_combat () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.wall" ~input:(Some 0) ~cost:5;
  place u 1 "fire.goblin" 0;
  hit ~card:"fire.dragon" u 0 0 5;
  check_int "wall" 16 (life_at u 0 0);
  check_int "goblin" 4 (life_at u 1 0);
  let v = new_game ~mana:ten ~special:Quantumancer.house () in
  play v 0 "quantumancer.wall" ~input:(Some 0) ~cost:5;
  place v 1 "fire.goblin" 0;
  hit ~is_spell:true v 0 0 5;
  check_int "no retaliate" 16 (life_at v 1 0)

let sealing_bans_until_death () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.sealing" ~input:(Some 0) ~cost:6;
  check_bool "banned" true (has_desc_mod (player u 0) Quantumancer.hide_sealing);
  play u 1 "fire.goblin" ~input:(Some 0) ~cost:1;
  check_bool "sealed" false (enabled_of u 1 "fire.goblin");
  kill u 0 0;
  check_bool "goblin free" true (enabled_of u 1 "fire.goblin");
  check_bool "sealing free" false (has_desc_mod (player u 0) Quantumancer.hide_sealing)

let sealing_survives_move () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.sealing" ~input:(Some 0) ~cost:6;
  play u 1 "fire.goblin" ~input:(Some 0) ~cost:1;
  step u (fun () -> Slots.move (player u 0).slots_update 0 2);
  check_string "moved" "quantumancer.sealing" (Option.get (occupant u 0 2));
  check_bool "still sealed" false (enabled_of u 1 "fire.goblin");
  kill u 0 2;
  check_bool "lifted" true (enabled_of u 1 "fire.goblin")

let sealing_stolen_drops_list () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.sealing" ~input:(Some 0) ~cost:6;
  play u 1 "fire.goblin" ~input:(Some 0) ~cost:1;
  step u (fun () -> Slots.move ~dest_player:(player u 1) (player u 0).slots_update 0 3);
  check_string "stolen" "quantumancer.sealing" (Option.get (occupant u 1 3));
  check_bool "old seals gone" true (enabled_of u 1 "fire.goblin");
  check_bool "owner unbanned" false (has_desc_mod (player u 0) Quantumancer.hide_sealing)

let fusion_tracks_mana () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 0 "quantumancer.fusion" ~input:(Some 0) ~cost:7;
  check_int "start" 10 (attack_at u 0 0);
  step u (fun () -> Houses.incr_mana (player u 1).houses_update 5 [ 2 ]);
  check_int "air 15" 15 (attack_at u 0 0);
  on_turn u 0;
  check_int "fire drained" 9 (mana u 1 0);
  check_int "water" 10 (mana u 1 1)

let cosmic_offers_cheap_spells () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  place u 1 "fire.goblin" 0;
  play u 1 "fire.wave" ~input:None ~cost:6;
  check_bool "recorded" true (List.mem "fire.wave" (played_keys u 0));
  play u 0 "quantumancer.cosmic" ~input:(Some 0) ~cost:8;
  check_int "untouched goblin" 16 (life_at u 1 0);
  check_int "fire mana" 10 (mana u 0 0);
  check_bool "phase" true (wait_name u 0 = Some Quantumancer.cosmic_phase);
  check_int "free wave" 0 (cost_of u 0 "fire.wave")

let cosmic_replays () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  place u 1 "fire.goblin" 0;
  play u 1 "fire.wave" ~input:None ~cost:6;
  play u 0 "quantumancer.cosmic" ~input:(Some 0) ~cost:8;
  play u 0 "fire.wave" ~input:None ~cost:0;
  check_int "hit" 7 (life_at u 1 0);
  check_int "unpaid" 10 (mana u 0 0)

let cosmic_skips_expensive () =
  let u = new_game ~mana:ten ~special:Quantumancer.house () in
  play u 1 "fire.apocalypse" ~input:None ~cost:5;
  play u 0 "quantumancer.cosmic" ~input:(Some 0) ~cost:8;
  check_bool "no phase" true (wait_name u 0 = None);
  check_int "alive" 60 (player_life u 1)

(* ------------------------------------------------------------- meteomancer *)

let meteo_attunes () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.thief" ~input:(Some 0) ~cost:1 ~flag:(Some (Called_element 2));
  check_bool "air" true (called u 0 = Some 2);
  check_bool "shown" true (hidden u 0 = false);
  play u 0 "meteomancer.extremophile" ~input:(Some 1) ~cost:2;
  check_bool "kept" true (called u 0 = Some 2);
  play u 0 "meteomancer.clairvoyant" ~input:(Some 2) ~cost:3 ~flag:(Some (Called_element 0));
  check_bool "switched" true (called u 0 = Some 0)

let mistwalker_hides () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.mistwalker" ~input:(Some 0) ~cost:4 ~flag:(Some (Called_element 1));
  check_bool "called" true (called u 0 = Some 1);
  check_bool "hidden" true (hidden u 0);
  play u 0 "meteomancer.thief" ~input:(Some 1) ~cost:1 ~flag:(Some (Called_element 1));
  check_bool "revealed" true (hidden u 0 = false)

let clairvoyant_swaps () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.clairvoyant" ~input:(Some 0) ~cost:3 ~flag:(Some (Called_element 0));
  check_int "fire" 13 (mana u 1 0);
  check_int "water" 9 (mana u 1 1);
  check_int "air" 9 (mana u 1 2);
  check_int "earth" 9 (mana u 1 3);
  check_int "special" 9 (mana u 1 4)

let extremophile_ignores_then_spawns () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.extremophile" ~input:(Some 2) ~cost:2 ~flag:(Some (Called_element 0));
  hit ~is_ability:true u 0 2 10;
  hit u 0 2 4;
  check_int "combat only" 7 (life_at u 0 2);
  play u 1 "fire.goblin" ~input:(Some 0) ~cost:1;
  check_string "rightmost" "meteomancer.extremophile" (Option.get (occupant u 0 5))

let thief_thunder () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.thief" ~input:(Some 0) ~cost:1 ~flag:(Some (Called_element 0));
  place u 1 "fire.goblin" 0;
  place u 1 "fire.goblin" 1;
  hit u 1 0 10;
  play u 1 "fire.wave" ~input:None ~cost:6;
  check_int "highest" 10 (life_at u 1 1);
  check_int "player" 54 (player_life u 1)

let grenadier_halves_called () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.grenadier" ~input:(Some 0) ~cost:5 ~flag:(Some (Called_element 0));
  play u 1 "fire.goblin" ~input:(Some 0) ~cost:1;
  check_int "halved" 8 (life_at u 1 0);
  play u 1 "water.sprite" ~input:(Some 1) ~cost:2;
  check_int "ping" 18 (life_at u 1 1)

let fateweaver_charges () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.fateweaver" ~input:(Some 0) ~cost:6 ~flag:(Some (Called_element 0));
  play u 1 "fire.goblin" ~input:(Some 0) ~cost:1;
  check_int "mana" 14 (mana u 0 0);
  check_int "charge" 1 (fate u 0);
  play u 0 "fire.goblin" ~input:(Some 1) ~cost:1;
  check_bool "extra" true (Option.is_some (wait_name u 0))

let prism_grows_and_pings () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.prism" ~input:(Some 0) ~cost:7 ~flag:(Some (Called_element 0));
  place u 1 "fire.goblin" 0;
  let before = attack_at u 0 0 in
  play u 1 "fire.goblin" ~input:(Some 1) ~cost:1;
  check_int "grown" (before + 1) (attack_at u 0 0);
  on_turn u 0;
  check_int "half of 9" 12 (life_at u 1 0)

let storm_strikes () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.storm" ~input:(Some 0) ~cost:8 ~flag:(Some (Called_element 0));
  place u 1 "fire.goblin" 0;
  play u 1 "fire.goblin" ~input:(Some 1) ~cost:1;
  check_int "player" 41 (player_life u 1);
  check_bool "slot 0" true (is_empty u 1 0);
  check_bool "slot 1" true (is_empty u 1 1)

let mistwalker_alone () =
  let u = new_game ~mana:ten ~special:Meteomancer.house () in
  play u 0 "meteomancer.mistwalker" ~input:(Some 0) ~cost:4 ~flag:(Some (Called_element 0));
  check_int "solo" 7 (attack_at u 0 0);
  play u 0 "meteomancer.thief" ~input:(Some 1) ~cost:1 ~flag:(Some (Called_element 0));
  check_int "pair" 2 (attack_at u 0 0)

let called_choices_expand () =
  init_all_houses ();
  let cmd =
    {
      cmd_player = 0;
      cmd_card = card_by_key "meteomancer.thief";
      cmd_input = Some 0;
      cmd_cost = 1;
      cmd_flag = None;
    }
  in
  check_int "four" 4 (List.length (Meteomancer.with_called_choices cmd));
  let flagged = { cmd with cmd_flag = Some (Called_element 1) } in
  check_int "kept" 1 (List.length (Meteomancer.with_called_choices flagged));
  let other =
    {
      cmd_player = 0;
      cmd_card = card_by_key "fire.goblin";
      cmd_input = Some 0;
      cmd_cost = 1;
      cmd_flag = None;
    }
  in
  check_int "plain" 1 (List.length (Meteomancer.with_called_choices other))

(* ----------------------------------------------------------------- seasons *)

let nectarbloom_heals () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  place u 0 "seasons.Nectarbloom.name" 2;
  hit u 0 2 4;
  on_turn u 0;
  check_int "healed" 8 (life_at u 0 2)

let ailuv_spawns_two () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  play u 0 "seasons.AiluvBees.name" ~input:(Some 0) ~cost:1;
  let blooms =
    Array.to_list (player u 0).slots_update.slot_updates
    |> List.filter (fun s -> occupant u 0 s.num = Some "seasons.Nectarbloom.name")
    |> List.length
  in
  check_int "two blooms" 2 blooms

let equinox_both_ways () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  place u 0 "fire.goblin" 1;
  place u 1 "fire.goblin" 1;
  hit u 0 1 4;
  play u 0 "seasons.SpiritofEquinox.name" ~input:(Some 2) ~cost:2;
  check_int "opp player" 56 (player_life u 1);
  check_int "opp goblin" 12 (life_at u 1 1);
  check_int "own goblin" 16 (life_at u 0 1);
  check_int "own player" 64 (player_life u 0)

let solar_wave_triples () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  place u 1 "fire.goblin" 0;
  play u 0 "seasons.SolarWave.name" ~input:None ~cost:3;
  (* 5 empty slots, then Midnight Sun on the opponent (both seats are Seasons) shaves 1 *)
  check_int "empty slots" 56 (player_life u 1);
  hit u 1 0 2;
  check_int "tripled" 10 (life_at u 1 0)

let midnight_reduces () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  check_bool "on" true (midnight u 0);
  step u (fun () -> Player.inflict (player u 0) (mk_damage 5 (ctx 1)));
  check_int "shaved" 56 (player_life u 0);
  play u 0 "seasons.SolarWave.name" ~input:None ~cost:3;
  check_bool "off" true (midnight u 0 = false)

let lower_basic_until_plentitude () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  check_int "wave" 5 (cost_of u 0 "fire.wave");
  play u 0 "seasons.GaeasAvatar.name" ~input:(Some 0) ~cost:6;
  check_int "restored" 6 (cost_of u 0 "fire.wave")

let gaea_counts_occupied () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  play u 0 "seasons.GaeasAvatar.name" ~input:(Some 0) ~cost:6;
  check_int "self" 1 (attack_at u 0 0);
  place u 1 "fire.goblin" 2;
  settle u;
  check_int "both" 2 (attack_at u 0 0)

let gaea_ignores_spells () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  play u 0 "seasons.GaeasAvatar.name" ~input:(Some 0) ~cost:6;
  hit ~is_spell:true u 0 0 20;
  check_int "full" 60 (life_at u 0 0);
  hit u 0 0 5;
  check_int "combat" 55 (life_at u 0 0)

let behemoth_caps_mana () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  play u 0 "seasons.BehemothofFrozenTime.name" ~input:(Some 0) ~cost:7;
  step u (fun () -> Houses.incr_mana (player u 1).houses_update (-2) [ 0 ]);
  check_int "drained" 8 (mana u 1 0);
  step u (fun () -> Houses.incr_mana (player u 1).houses_update 5 [ 0 ]);
  check_int "capped" 9 (mana u 1 0)

let wintry_pokes_open () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  place u 1 "fire.goblin" 2;
  on_turn u 0;
  check_int "open opposite" 15 (life_at u 1 2)

let aurora_heals_and_feeds () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  place u 0 "fire.goblin" 1;
  hit u 0 1 8;
  play u 0 "seasons.TheGreatAurora.name" ~input:None ~cost:8;
  check_int "healed" 16 (life_at u 0 1);
  check_int "fire" 13 (mana u 0 0)

let eternal_extra_phases () =
  let u = new_game ~mana:ten ~special:Seasons.house () in
  play u 0 "seasons.EternalSummer.name" ~input:None ~cost:4;
  check_int "two waits"
    2
    (List.length
       (List.filter (function Wait_player (0, _) -> true | _ -> false) (player u 0).pstate.ps_transitions))

let () =
  Alcotest.run "houses_e"
    [
      ( "conjurer",
        [
          Alcotest.test_case "nectar" `Quick nectar_heals_any;
          Alcotest.test_case "bravery" `Quick bravery_skips_walls;
          Alcotest.test_case "war mage" `Quick war_mage_buffs_twice;
          Alcotest.test_case "golem" `Quick bone_golem_ignores_effects;
          Alcotest.test_case "nurture" `Quick nurture_stuns_then_heals;
          Alcotest.test_case "pain" `Quick pain_scales;
          Alcotest.test_case "guard" `Quick unholy_guard;
          Alcotest.test_case "mystic" `Quick dark_mystic;
        ] );
      ( "quantumancer",
        [
          Alcotest.test_case "surge" `Quick surge_floors_at_three;
          Alcotest.test_case "kineticist" `Quick kineticist_eats_effects;
          Alcotest.test_case "dimension hit" `Quick dimension_mirrors_hits;
          Alcotest.test_case "dimension empty" `Quick dimension_empty_is_once;
          Alcotest.test_case "martyr" `Quick martyr_absorbs_wave;
          Alcotest.test_case "martyr mods" `Quick martyr_skips_owner_mods;
          Alcotest.test_case "phosphor" `Quick phosphor_retaliates_combat;
          Alcotest.test_case "sealing" `Quick sealing_bans_until_death;
          Alcotest.test_case "sealing move" `Quick sealing_survives_move;
          Alcotest.test_case "sealing steal" `Quick sealing_stolen_drops_list;
          Alcotest.test_case "fusion" `Quick fusion_tracks_mana;
          Alcotest.test_case "cosmic offer" `Quick cosmic_offers_cheap_spells;
          Alcotest.test_case "cosmic replay" `Quick cosmic_replays;
          Alcotest.test_case "cosmic skip" `Quick cosmic_skips_expensive;
        ] );
      ( "meteomancer",
        [
          Alcotest.test_case "attune" `Quick meteo_attunes;
          Alcotest.test_case "mistwalker hide" `Quick mistwalker_hides;
          Alcotest.test_case "clairvoyant" `Quick clairvoyant_swaps;
          Alcotest.test_case "extremophile" `Quick extremophile_ignores_then_spawns;
          Alcotest.test_case "thief" `Quick thief_thunder;
          Alcotest.test_case "grenadier" `Quick grenadier_halves_called;
          Alcotest.test_case "fateweaver" `Quick fateweaver_charges;
          Alcotest.test_case "prism" `Quick prism_grows_and_pings;
          Alcotest.test_case "storm" `Quick storm_strikes;
          Alcotest.test_case "mistwalker solo" `Quick mistwalker_alone;
          Alcotest.test_case "choices" `Quick called_choices_expand;
        ] );
      ( "seasons",
        [
          Alcotest.test_case "nectarbloom" `Quick nectarbloom_heals;
          Alcotest.test_case "ailuv" `Quick ailuv_spawns_two;
          Alcotest.test_case "equinox" `Quick equinox_both_ways;
          Alcotest.test_case "solar wave" `Quick solar_wave_triples;
          Alcotest.test_case "midnight" `Quick midnight_reduces;
          Alcotest.test_case "lower basic" `Quick lower_basic_until_plentitude;
          Alcotest.test_case "gaea attack" `Quick gaea_counts_occupied;
          Alcotest.test_case "gaea spells" `Quick gaea_ignores_spells;
          Alcotest.test_case "behemoth" `Quick behemoth_caps_mana;
          Alcotest.test_case "wintry" `Quick wintry_pokes_open;
          Alcotest.test_case "aurora" `Quick aurora_heals_and_feeds;
          Alcotest.test_case "eternal" `Quick eternal_extra_phases;
        ] );
    ]
