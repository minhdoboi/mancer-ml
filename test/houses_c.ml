(* Mechanics for Shadowmage, Colors, Limbo and Rogue. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit ?(is_ability = false) ?(is_spell = false) u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage ~is_ability ~is_spell amount (ctx (1 - pid))))

let kill u pid num = hit u pid num 1000
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let settle u = step u (fun () -> ())
let all_mana = [ 6; 4; 4; 4; 10 ]
let flagged u pid num flag = has_flag (Option.get (slot u pid num).value) flag

let desc_enabled u pid house_index =
  let pd = (player u pid).pstate.ps_desc in
  match Array.find_opt (fun h -> h.phd_house.h_index = house_index) pd.init_desc.pd_houses with
  | None -> false
  | Some h ->
      List.fold_left (fun cs m -> m h cs) h.phd_cards pd.desc_mods |> Array.exists (fun cd -> cd.cd_enabled)

let eclipse u pid = match (player u pid).pstate.ps_data with Shadowmage_data d -> d.eclipse_shield | _ -> -1

(* ------------------------------------------------------------- shadowmage *)

let shadowmage_costs () =
  init_all_houses ();
  check_int "archer" 1 (card_by_key "shadowmage.archer").cost;
  check_int "assassin" 8 (card_by_key "shadowmage.assassin").cost

let archer_shot_and_heal () =
  let u = new_game ~special:Shadowmage.house () in
  play u 0 "shadowmage.archer" ~input:(Some 2) ~cost:1;
  check_int "6 on summon" 54 (player_life u 1);
  kill u 0 2;
  check_bool "dead" true (is_empty u 0 2);
  check_int "opponent heals 6" 60 (player_life u 1)

let stalker_hides_until_hit () =
  let u = new_game ~special:Shadowmage.house () in
  place u 0 "shadowmage.stalker" 3;
  check_bool "hidden" true (Shadowmage.is_hidden (Option.get (slot u 0 3).value));
  check_bool "not visible" true (Shadowmage.visible_creatures (player u 0).pstate = []);
  hit ~is_ability:true u 0 3 4;
  check_bool "revealed" false (Shadowmage.is_hidden (Option.get (slot u 0 3).value));
  check_int "13 life" 13 (life_at u 0 3)

let stalker_stays_hidden_if_absorbed () =
  let u = new_game ~mana:all_mana ~special:Shadowmage.house () in
  place u 0 "shadowmage.stalker" 3;
  play u 0 "shadowmage.weaver" ~input:(Some 2) ~cost:3;
  check_bool "invincible" true (flagged u 0 3 invincible_flag);
  hit ~is_ability:true u 0 3 8;
  check_int "untouched" 17 (life_at u 0 3);
  check_bool "still hidden" true (Shadowmage.is_hidden (Option.get (slot u 0 3).value))

let weaver_shields_neighbours () =
  let u = new_game ~mana:all_mana ~special:Shadowmage.house () in
  place u 0 "fire.goblin" 1;
  place u 0 "earth.sprite" 3;
  play u 0 "shadowmage.weaver" ~input:(Some 2) ~cost:3;
  check_bool "left" true (flagged u 0 1 invincible_flag);
  check_bool "right" true (flagged u 0 3 invincible_flag);
  check_bool "self" false (flagged u 0 2 invincible_flag);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_bool "still up on own end" true (flagged u 0 1 invincible_flag);
  step u (fun () -> Player.apply_effects (player u 1) On_end_turn);
  check_bool "faded left" false (flagged u 0 1 invincible_flag);
  check_bool "faded right" false (flagged u 0 3 invincible_flag)

let silencer_hides_element () =
  let u = new_game ~mana:all_mana ~special:Shadowmage.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "shadowmage.silencer" ~input:(Some 2) ~cost:4;
  check_bool "stunned" true (flagged u 1 2 stun_flag);
  check_bool "fire hidden" false (desc_enabled u 1 0);
  check_bool "special still there" true (desc_enabled u 1 4);
  step u (fun () -> Player.apply_effects (player u 1) On_end_turn);
  check_bool "fire back" true (desc_enabled u 1 0)

let eclipse_absorbs_thirty () =
  let u = new_game ~mana:all_mana ~special:Shadowmage.house () in
  play u 0 "shadowmage.eclipse" ~input:None ~cost:5;
  check_int "fire +2" 8 (mana u 0 0);
  check_int "shield" 30 (eclipse u 0);
  step u (fun () -> Player.inflict (player u 0) (mk_damage ~is_spell:true 12 (ctx 1)));
  check_int "life untouched" 60 (player_life u 0);
  check_int "18 left" 18 (eclipse u 0);
  step u (fun () -> Player.inflict (player u 0) (mk_damage ~is_spell:true 25 (ctx 1)));
  check_int "overflow" 53 (player_life u 0);
  check_int "empty" 0 (eclipse u 0)

let eclipse_fades_on_opponent_end () =
  let u = new_game ~mana:all_mana ~special:Shadowmage.house () in
  play u 0 "shadowmage.eclipse" ~input:None ~cost:5;
  step u (fun () -> Player.apply_effects (player u 1) On_end_turn);
  step u (fun () -> Player.inflict (player u 0) (mk_damage ~is_spell:true 8 (ctx 1)));
  check_int "took 8" 52 (player_life u 0);
  check_int "shield gone" 0 (eclipse u 0)

let spike_then_heal () =
  let u = new_game ~mana:all_mana ~special:Shadowmage.house () in
  place u 1 "fire.goblin" 1;
  place u 1 "earth.sprite" 3;
  play u 0 "shadowmage.spike" ~input:None ~cost:6;
  check_bool "goblin died" true (is_empty u 1 1);
  check_int "sprite 22 - 16" 6 (life_at u 1 3);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_bool "goblin stays dead" true (is_empty u 1 1);
  check_int "sprite healed 16" 22 (life_at u 1 3)

let warden_ignores_first () =
  let u = new_game ~special:Shadowmage.house () in
  place u 0 "shadowmage.warden" 4;
  hit ~is_ability:true u 0 4 9;
  check_int "first ignored" 45 (life_at u 0 4);
  hit ~is_ability:true u 0 4 9;
  check_int "second lands" 36 (life_at u 0 4);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  hit ~is_ability:true u 0 4 5;
  check_int "reset" 36 (life_at u 0 4)

let assassin_replaces_and_snipe () =
  let u = new_game ~mana:all_mana ~special:Shadowmage.house () in
  place u 0 "fire.goblin" 2;
  place u 1 "earth.sprite" 2;
  play u 0 "shadowmage.assassin" ~input:(Some 2) ~cost:8;
  check_string "in" "shadowmage.assassin" (Option.get (occupant u 0 2));
  check_int "sprite 22 - 12" 10 (life_at u 1 2);
  check_int "half goblin cost" 7 (mana u 0 0)

let assassin_empty_slot () =
  let u = new_game ~mana:all_mana ~special:Shadowmage.house () in
  let before = mana u 0 0 in
  play u 0 "shadowmage.assassin" ~input:(Some 4) ~cost:8;
  check_string "in" "shadowmage.assassin" (Option.get (occupant u 0 4));
  check_int "no elemental gain" before (mana u 0 0)

(* ----------------------------------------------------------------- colors *)

let colors_costs () =
  init_all_houses ();
  check_int "discoloration" 0 (card_by_key "colors.Discoloration.name").cost;
  check_int "amber" 1 (card_by_key "colors.Amber.name").cost;
  check_int "violet" 2 (card_by_key "colors.Violet.name").cost;
  check_int "azure" 2 (card_by_key "colors.Azure.name").cost;
  check_int "silver" 3 (card_by_key "colors.Silver.name").cost;
  check_int "gold" 4 (card_by_key "colors.Gold.name").cost;
  check_int "emerald" 4 (card_by_key "colors.Emerald.name").cost;
  check_int "crimson" 4 (card_by_key "colors.Crimson.name").cost

let color_spend_and_attack () =
  let u = new_game ~mana:all_mana ~special:Colors.house () in
  play u 0 "colors.Amber.name" ~input:(Some 0) ~cost:1;
  settle u;
  check_int "spent the pool" 0 (mana u 0 4);
  check_int "attack is X" 10 (attack_at u 0 0)

let amber_feeds_top_powers () =
  let u = new_game ~mana:[ 3; 5; 4; 7; 8 ] ~special:Colors.house () in
  play u 0 "colors.Amber.name" ~input:(Some 0) ~cost:1;
  kill u 0 0;
  check_int "earth +8" 15 (mana u 0 3);
  check_int "water +8" 13 (mana u 0 1)

let violet_cuts_cheap_cards () =
  let u = new_game ~mana:all_mana ~special:Colors.house () in
  play u 0 "colors.Violet.name" ~input:(Some 0) ~cost:2;
  let pd = (player u 0).pstate.ps_desc in
  let fire = pd.init_desc.pd_houses.(0) in
  let cards = List.fold_left (fun cs m -> m fire cs) fire.phd_cards pd.desc_mods in
  let goblin = Array.to_list cards |> List.find (fun cd -> cd.cd_card.key = "fire.goblin") in
  check_int "goblin 1 - 1" 0 goblin.cd_cost

let azure_shields_nearby_costs () =
  let u = new_game ~mana:[ 6; 4; 4; 4; 5 ] ~special:Colors.house () in
  place u 0 "fire.drake" 1;
  play u 0 "colors.Azure.name" ~input:(Some 0) ~cost:2;
  let before = life_at u 0 1 in
  hit ~is_spell:true u 0 1 9;
  check_int "drake cost 4 is in X±1" before (life_at u 0 1);
  hit ~is_spell:true u 0 0 9;
  check_int "azure itself cost 2 is not" (24 - 9) (life_at u 0 0)

let crimson_boosts_player_hits () =
  let u = new_game ~mana:all_mana ~special:Colors.house () in
  play u 0 "colors.Crimson.name" ~input:(Some 0) ~cost:4;
  place u 0 "fire.goblin" 1;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "4 + X" (60 - 14) (player_life u 1)

let discolor_gives_an_extra_play () =
  let u = new_game ~mana:all_mana ~special:Colors.house () in
  play u 0 "colors.Amber.name" ~input:(Some 0) ~cost:1;
  play u 0 "colors.Discoloration.name" ~input:(Some 0) ~cost:0;
  check_bool "gone" true (is_empty u 0 0);
  check_bool "extra action" true
    (match Player.pop_transition (player u 0) with
    | Some (Wait_player (0, Some name)) -> name = Colors.discolor_phase
    | _ -> false)

(* ------------------------------------------------------------------ limbo *)

let combat_goes_to_limbo () =
  let u = new_game ~special:Limbo.house () in
  place u 0 "fire.goblin" 0;
  hit u 0 0 100;
  check_string "still there" "fire.goblin" (Option.get (occupant u 0 0));
  check_int "0 life" 0 (life_at u 0 0);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_bool "dies at end of turn" true (is_empty u 0 0)

let spell_kills_normally () =
  let u = new_game ~special:Limbo.house () in
  place u 0 "fire.goblin" 0;
  hit ~is_spell:true u 0 0 100;
  check_bool "spells kill" true (is_empty u 0 0)

let baron_survives_spells_and_doubles () =
  let u = new_game ~special:Limbo.house () in
  place u 0 "limbo.baron" 0;
  settle u;
  check_int "printed 6" 6 (attack_at u 0 0);
  hit ~is_spell:true u 0 0 100;
  check_string "in limbo" "limbo.baron" (Option.get (occupant u 0 0));
  settle u;
  check_int "doubled" 12 (attack_at u 0 0)

let heaven_heals_on_limbo () =
  let u = new_game ~special:Limbo.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "limbo.heaven" 0;
  place u 0 "fire.goblin" 2;
  hit ~is_spell:true u 0 0 4;
  check_int "nicked" 3 (life_at u 0 0);
  hit u 0 2 100;
  check_int "owner +2" 42 (player_life u 0);
  check_int "heaven +2" 5 (life_at u 0 0)

let wraith_counts_limbo () =
  let u = new_game ~special:Limbo.house () in
  place u 0 "limbo.wraith" 0;
  place u 0 "fire.goblin" 1;
  settle u;
  check_int "base 1" 1 (attack_at u 0 0);
  hit u 0 1 100;
  settle u;
  check_int "plus the goblin in limbo" 2 (attack_at u 0 0)

let nether_pays_per_creature () =
  let u = new_game ~mana:all_mana ~special:Limbo.house () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.goblin" 1;
  place u 1 "earth.sprite" 3;
  play u 0 "limbo.nether" ~input:None ~cost:5;
  check_int "own goblin in limbo" 0 (life_at u 0 0);
  check_int "3 per own creature" (22 - 6) (life_at u 1 3)

let redemption_rebuilds () =
  let u = new_game ~mana:all_mana ~special:Limbo.house () in
  place u 0 "fire.goblin" 0;
  hit u 0 0 100;
  play u 0 "limbo.redemption" ~input:(Some 0) ~cost:7;
  check_int "full life" 16 (life_at u 0 0)

let baron_saves_the_player () =
  let u = new_game ~special:Limbo.house () in
  place u 0 "limbo.baron" 0;
  step u (fun () -> Player.inflict (player u 0) (mk_damage 100 (ctx 1)));
  check_bool "game continues" true (Option.is_none u.ended);
  check_bool "in limbo" true
    (match (player u 0).pstate.ps_data with Limbo_player `Limbo -> true | _ -> false);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_bool "dies after the turn" true (u.ended = Some 1)

(* ------------------------------------------------------------------ rogue *)

let sneak_open_and_blocked () =
  let u = new_game ~mana:all_mana ~special:Rogue.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "rogue.sneak" ~input:(Some 2) ~cost:1;
  check_int "4 on an open lane" 12 (life_at u 1 2);
  check_bool "stunned" true (flagged u 1 2 stun_flag);
  let v = new_game ~mana:all_mana ~special:Rogue.house () in
  place v 0 "fire.goblin" 2;
  place v 1 "fire.goblin" 2;
  play v 0 "rogue.sneak" ~input:(Some 2) ~cost:1;
  check_int "8 when blocked" 8 (life_at v 1 2)

let trickster_only_when_opposed () =
  let u = new_game ~special:Rogue.house () in
  place u 0 "rogue.trickster" 2;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "miss" 60 (player_life u 1);
  place u 1 "fire.goblin" 2;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "2" 58 (player_life u 1)

let thief_drains_opposite_house () =
  let u = new_game ~special:Rogue.house () in
  place u 0 "rogue.thief" 2;
  place u 1 "earth.sprite" 2;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "earth -1" 3 (mana u 1 3);
  check_int "fire untouched" 6 (mana u 1 0)

let dancer_strikes_twice () =
  let u = new_game ~special:Rogue.house () in
  place u 0 "rogue.dancer" 2;
  place u 1 "fire.goblin" 2;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "two hits" 10 (life_at u 1 2)

let fighter_grows_from_opposite () =
  let goblin = card_by_key "fire.goblin" in
  let poke u amount =
    step u (fun () ->
        Slot.inflict (slot u 0 2)
          (mk_damage amount { ctx_player = 1; ctx_card = Some goblin; ctx_selected = 2 }))
  in
  let u = new_game ~special:Rogue.house () in
  place u 0 "rogue.fighter" 2;
  place u 1 "fire.goblin" 2;
  poke u 4;
  check_int "attack +2" 5 (attack_at u 0 2);
  check_int "life" 27 (life_at u 0 2);
  let v = new_game ~special:Rogue.house () in
  place v 0 "rogue.fighter" 2;
  place v 1 "fire.goblin" 2;
  poke v 31;
  check_bool "lethal does not buff" true (is_empty v 0 2)

let assassin_finishes_the_lane () =
  let u = new_game ~mana:all_mana ~special:Rogue.house () in
  place u 1 "fire.wall" 2;
  play u 0 "rogue.assassin" ~input:(Some 2) ~cost:6;
  check_bool "wall gone" true (is_empty u 1 2);
  check_int "then she attacks" 51 (player_life u 1);
  let v = new_game ~mana:all_mana ~special:Rogue.house () in
  place v 1 "earth.sprite" 2;
  play v 0 "rogue.assassin" ~input:(Some 2) ~cost:6;
  check_int "sprite lives" 14 (life_at v 1 2);
  check_int "no follow-up" 60 (player_life v 1)

let trap_skips_a_turn () =
  let u = new_game ~mana:all_mana ~special:Rogue.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "rogue.trap" ~input:None ~cost:7;
  check_int "all pools -2" 4 (mana u 1 0);
  check_bool "stunned" true (flagged u 1 2 stun_flag);
  check_bool "cannot play" false (desc_enabled u 1 0);
  step u (fun () -> Player.apply_effects (player u 1) On_end_turn);
  check_bool "back" true (desc_enabled u 1 0)

let archer_clears_weak () =
  let u = new_game ~mana:all_mana ~special:Rogue.house () in
  place u 1 "fire.wall" 2;
  place u 1 "earth.sprite" 4;
  play u 0 "rogue.archer" ~input:(Some 2) ~cost:8;
  check_bool "wall gone" true (is_empty u 1 2);
  check_string "sprite lives" "earth.sprite" (Option.get (occupant u 1 4));
  check_int "punish the opposite kill" 57 (player_life u 1);
  step u (fun () ->
      match (slot u 0 2).value with
      | Some st -> (
          match st.card.c_body.b_run_attack.apply_ with
          | Custom f ->
              f [ 4 ]
                (mk_damage 6 { ctx_player = 0; ctx_card = Some st.card.c_card; ctx_selected = 2 })
                (player u 0)
          | _ -> ())
      | None -> ());
  check_int "combat, no execute" 16 (life_at u 1 4);
  check_int "no extra 3" 57 (player_life u 1)

let () =
  let open Alcotest in
  run "houses_c"
    [
      ( "shadowmage",
        [
          test_case "costs" `Quick shadowmage_costs;
          test_case "archer" `Quick archer_shot_and_heal;
          test_case "stalker" `Quick stalker_hides_until_hit;
          test_case "stalker absorb" `Quick stalker_stays_hidden_if_absorbed;
          test_case "weaver" `Quick weaver_shields_neighbours;
          test_case "silencer" `Quick silencer_hides_element;
          test_case "eclipse" `Quick eclipse_absorbs_thirty;
          test_case "eclipse fade" `Quick eclipse_fades_on_opponent_end;
          test_case "spike" `Quick spike_then_heal;
          test_case "warden" `Quick warden_ignores_first;
          test_case "assassin" `Quick assassin_replaces_and_snipe;
          test_case "assassin empty" `Quick assassin_empty_slot;
        ] );
      ( "colors",
        [
          test_case "costs" `Quick colors_costs;
          test_case "spend" `Quick color_spend_and_attack;
          test_case "amber" `Quick amber_feeds_top_powers;
          test_case "violet" `Quick violet_cuts_cheap_cards;
          test_case "azure" `Quick azure_shields_nearby_costs;
          test_case "crimson" `Quick crimson_boosts_player_hits;
          test_case "discolor" `Quick discolor_gives_an_extra_play;
        ] );
      ( "limbo",
        [
          test_case "combat limbo" `Quick combat_goes_to_limbo;
          test_case "spell kill" `Quick spell_kills_normally;
          test_case "baron" `Quick baron_survives_spells_and_doubles;
          test_case "heaven" `Quick heaven_heals_on_limbo;
          test_case "wraith" `Quick wraith_counts_limbo;
          test_case "nether" `Quick nether_pays_per_creature;
          test_case "redemption" `Quick redemption_rebuilds;
          test_case "baron save" `Quick baron_saves_the_player;
        ] );
      ( "rogue",
        [
          test_case "sneak" `Quick sneak_open_and_blocked;
          test_case "trickster" `Quick trickster_only_when_opposed;
          test_case "thief" `Quick thief_drains_opposite_house;
          test_case "dancer" `Quick dancer_strikes_twice;
          test_case "fighter" `Quick fighter_grows_from_opposite;
          test_case "assassin" `Quick assassin_finishes_the_lane;
          test_case "trap" `Quick trap_skips_a_turn;
          test_case "archer" `Quick archer_clears_weak;
        ] );
    ]
