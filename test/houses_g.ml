(* Mechanics for Dark Priest, Elementalist, High Priest and Kinetician. *)

open Mancer
open Sp
open Engine
open Common
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit ?(is_ability = false) ?(is_spell = false) u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage ~is_ability ~is_spell amount (ctx (1 - pid))))

let kill u pid num = hit u pid num 1000
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let on_start u pid = step u (fun () -> Player.apply_effects (player u pid) On_start)
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

let special_keys u pid =
  let pd = (player u pid).pstate.ps_desc in
  match Array.find_opt (fun h -> h.phd_house.h_index = 4) pd.init_desc.pd_houses with
  | None -> []
  | Some h ->
      let cards = List.fold_left (fun cs m -> m h cs) h.phd_cards pd.desc_mods in
      Array.to_list cards |> List.map (fun cd -> cd.cd_card.key)

(* ----------------------------------------------------------- dark priest *)

let restless_at u =
  List.init 6 Fun.id
  |> List.find_map (fun n ->
      match occupant u 1 n with Some "darkpriest.RestlessSoul.name" -> Some n | _ -> None)

let restless_spawns () =
  let u = new_game ~mana:ten ~special:Darkpriest.house () in
  on_start u 0;
  (match restless_at u with
  | Some n ->
      kill u 1 n;
      check_int "special" 12 (mana u 0 4)
  | None -> Alcotest.fail "restless did not spawn");
  on_end u 1;
  check_bool "respawn" true (Option.is_some (restless_at u))

let ghost_heals () =
  let u = new_game ~mana:ten ~special:Darkpriest.house () in
  place u 0 "fire.goblin" 1;
  play u 0 "darkpriest.Ghost.name" ~input:(Some 2) ~cost:1;
  check_int "heal" 62 (player_life u 0)

let black_mass_hits () =
  let u = new_game ~mana:ten ~special:Darkpriest.house () in
  place u 0 "fire.goblin" 1;
  place u 1 "fire.goblin" 2;
  place u 1 "earth.sprite" 3;
  play u 0 "darkpriest.BlackMass.name" ~input:(Some 1) ~cost:3;
  check_bool "eaten" true (is_empty u 0 1);
  check_int "fire" 8 (life_at u 1 2);
  check_int "earth" 14 (life_at u 1 3)

let occult_plants () =
  let u = new_game ~mana:ten ~special:Darkpriest.house () in
  play u 0 "darkpriest.Occultist.name" ~input:(Some 2) ~cost:2;
  check_string "shadow" "darkpriest.ShadowOfPriest.name" (Option.get (occupant u 1 2))

let monk_heals () =
  let u = new_game ~mana:ten ~special:Darkpriest.house () in
  play u 0 "darkpriest.BlackMonk.name" ~input:(Some 2) ~cost:5;
  hit u 0 2 6;
  check_int "player" 66 (player_life u 0)

(* ---------------------------------------------------------- elementalist *)

let avalanche_drains () =
  let u = new_game ~mana:ten ~special:Elementalist.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "elementalist.avalanche" ~input:None ~cost:4;
  check_int "hurt" (-1) (life_at u 1 2);
  check_int "heal" 80 (player_life u 0);
  check_int "earth" 0 (mana u 0 3)

let incinerate_bans () =
  let u = new_game ~mana:ten ~special:Elementalist.house () in
  place u 0 "fire.goblin" 1;
  place u 1 "earth.sprite" 2;
  play u 0 "elementalist.incineration" ~input:None ~cost:5;
  check_bool "own" true (is_empty u 0 1);
  check_bool "opp" true (is_empty u 1 2);
  check_bool "banned" false (enabled_of u 0 "fire.goblin")

let frost_blocks () =
  let u = new_game ~mana:ten ~special:Elementalist.house () in
  play u 0 "elementalist.frost" ~input:(Some 2) ~cost:8;
  check_int "player" 60 (player_life u 1);
  check_bool "blocked" false (List.mem 2 (player u 1).pstate.ps_slot_list)

let golem_grows () =
  let u = new_game ~mana:ten ~special:Elementalist.house () in
  play u 0 "elementalist.golem" ~input:(Some 2) ~cost:7;
  check_int "open" 7 (attack_at u 0 2);
  place u 1 "fire.goblin" 2;
  settle u;
  check_int "blocked" 11 (attack_at u 0 2)

let freeze_skips () =
  let u = new_game ~mana:ten ~special:Elementalist.house () in
  play u 0 "elementalist.freeze" ~input:None ~cost:2;
  check_bool "skipped" false (enabled_of u 1 "fire.goblin")

let freeze_chain () =
  let u = new_game ~mana:ten ~special:Elementalist.house () in
  play u 0 "elementalist.freeze" ~input:None ~cost:2;
  on_end u 0;
  check_bool "foe skipped" false (enabled_of u 1 "fire.goblin");
  check_bool "foe has nothing to play" true (is_disabled (player u 1).pstate);
  check_bool "caster can play" false (is_disabled (player u 0).pstate);
  check_bool "foe turn skipped" true (turn_skipped (player u 1).pstate);
  check_bool "caster turn not skipped" false (turn_skipped (player u 0).pstate);
  check_bool "caster free during foe turn" true (enabled_of u 0 "fire.goblin");
  on_end u 1;
  check_bool "foe back" true (enabled_of u 1 "fire.goblin");
  check_bool "caster skipped" false (enabled_of u 0 "fire.goblin");
  on_end u 0;
  check_bool "caster back" true (enabled_of u 0 "fire.goblin")

(* ----------------------------------------------------------- high priest *)

let scarab_covers () =
  let u = new_game ~mana:ten ~special:Highpriest.house () in
  play u 0 "highpriest.Sacredscarab" ~input:(Some 2) ~cost:1;
  place u 0 "fire.goblin" 1;
  hit u 0 2 5;
  check_int "reduced" 8 (life_at u 0 2)

let apis_feeds () =
  let u = new_game ~mana:ten ~special:Highpriest.house () in
  place u 0 "highpriest.Apis" 1;
  on_turn u 0;
  check_int "special" 11 (mana u 0 4)

let curse_sums () =
  let u = new_game ~mana:ten ~special:Highpriest.house () in
  place u 1 "fire.goblin" 1;
  place u 1 "fire.goblin" 2;
  play u 0 "highpriest.Curseofchaos" ~input:(Some 2) ~cost:5;
  check_int "mid" 8 (life_at u 1 2);
  check_int "adj" 8 (life_at u 1 1)

let path_swaps () =
  let u = new_game ~mana:ten ~special:Highpriest.house () in
  place u 0 "air.fairy" 1;
  place u 0 "air.fairy" 2;
  settle u;
  check_bool "path" true (List.mem "highpriest.Ancientcrocodile" (special_keys u 0))

let wajet_scales () =
  let u = new_game ~mana:ten ~special:Highpriest.house () in
  play u 1 "fire.goblin" ~input:(Some 0) ~cost:1;
  play u 0 "highpriest.EyeofWajet" ~input:None ~cost:5;
  check_int "player" 61 (player_life u 0)

let bennu_strikes_before_death () =
  let u = new_game ~mana:ten ~special:Highpriest.house () in
  place u 0 "highpriest.Bennu" 0;
  place u 1 "fire.goblin" 0;
  kill u 0 0;
  check_bool "bennu gone" true (is_empty u 0 0);
  check_int "15 to the opposite" 1 (life_at u 1 0)

(* ----------------------------------------------------------- kinetician *)

let warp_caps () =
  let u = new_game ~mana:ten ~special:Kinetician.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "kinetician.warp" ~input:(Some 2) ~cost:4;
  check_int "cost" 1 (life_at u 1 2)

let barrier_doubles () =
  let u = new_game ~mana:ten ~special:Kinetician.house () in
  place u 0 "fire.goblin" 2;
  play u 0 "kinetician.barrier" ~input:(Some 2) ~cost:7;
  check_int "life" 32 (life_at u 0 2);
  check_bool "no run" false (flagged u 0 2 run_flag)

let lone_mage () =
  let u = new_game ~mana:ten ~special:Kinetician.house () in
  play u 0 "kinetician.mage" ~input:(Some 2) ~cost:5;
  on_turn u 0;
  check_int "empty" 54 (player_life u 1)

let call_halves () =
  let u = new_game ~mana:ten ~special:Kinetician.house () in
  place u 0 "fire.goblin" 1;
  play u 0 "kinetician.call" ~input:(Some 1) ~cost:6;
  check_int "wave" 3 (cost_of u 0 "fire.wave")

let () =
  Alcotest.run "houses_g"
    [
      ( "darkpriest",
        [
          Alcotest.test_case "restless" `Quick restless_spawns;
          Alcotest.test_case "ghost" `Quick ghost_heals;
          Alcotest.test_case "black mass" `Quick black_mass_hits;
          Alcotest.test_case "occult" `Quick occult_plants;
          Alcotest.test_case "monk" `Quick monk_heals;
        ] );
      ( "elementalist",
        [
          Alcotest.test_case "avalanche" `Quick avalanche_drains;
          Alcotest.test_case "incinerate" `Quick incinerate_bans;
          Alcotest.test_case "frost" `Quick frost_blocks;
          Alcotest.test_case "golem" `Quick golem_grows;
          Alcotest.test_case "freeze" `Quick freeze_skips;
          Alcotest.test_case "freeze chain" `Quick freeze_chain;
        ] );
      ( "highpriest",
        [
          Alcotest.test_case "scarab" `Quick scarab_covers;
          Alcotest.test_case "apis" `Quick apis_feeds;
          Alcotest.test_case "curse" `Quick curse_sums;
          Alcotest.test_case "path" `Quick path_swaps;
          Alcotest.test_case "wajet" `Quick wajet_scales;
          Alcotest.test_case "bennu" `Quick bennu_strikes_before_death;
        ] );
      ( "kinetician",
        [
          Alcotest.test_case "warp" `Quick warp_caps;
          Alcotest.test_case "barrier" `Quick barrier_doubles;
          Alcotest.test_case "lone" `Quick lone_mage;
          Alcotest.test_case "call" `Quick call_halves;
        ] );
    ]
