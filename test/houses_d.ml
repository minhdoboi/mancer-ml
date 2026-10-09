(* Mechanics for Tinker, Entomologist, Soulbinder, Shadowcaster and Spirit Shaman. *)

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
let growth u pid i = (player u pid).houses_update.hu_value.(i).growth
let settle u = step u (fun () -> ())
let all_mana = [ 6; 4; 4; 4; 10 ]
let flagged u pid num flag = has_flag (Option.get (slot u pid num).value) flag

let special_cost u pid key =
  let pd = (player u pid).pstate.ps_desc in
  match Array.find_opt (fun h -> h.phd_house.h_index = 4) pd.init_desc.pd_houses with
  | None -> -1
  | Some h -> (
      let cards = List.fold_left (fun cs m -> m h cs) h.phd_cards pd.desc_mods in
      match Array.find_opt (fun cd -> cd.cd_card.key = key) cards with Some cd -> cd.cd_cost | None -> -1)

let card_enabled u pid key =
  let pd = (player u pid).pstate.ps_desc in
  match Array.find_opt (fun h -> h.phd_house.h_index = 4) pd.init_desc.pd_houses with
  | None -> false
  | Some h -> (
      let cards = List.fold_left (fun cs m -> m h cs) h.phd_cards pd.desc_mods in
      match Array.find_opt (fun cd -> cd.cd_card.key = key) cards with
      | Some cd -> cd.cd_enabled
      | None -> false)

let souls u pid = match (player u pid).pstate.ps_data with Soulbinder_data xs -> xs | _ -> []
let power u pid = match (player u pid).pstate.ps_data with Shadowcaster_data x -> x | _ -> -1
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let on_start u pid = step u (fun () -> Player.apply_effects (player u pid) On_start)

let run_at u pid num =
  step u (fun () -> Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let destroy_at u pid num = step u (fun () -> Slot.destroy (slot u pid num))

(* ----------------------------------------------------------------- tinker *)

let tinker_costs () =
  init_all_houses ();
  check_int "plant" 1 (card_by_key "tinker.plant").cost;
  check_int "dragon" 8 (card_by_key "tinker.dragon").cost;
  check_int "bomb" 0 (card_by_key "tinker.bomb").cost

let plant_puts_a_bomb () =
  let u = new_game ~special:Tinker.house () in
  play u 0 "tinker.plant" ~input:(Some 3) ~cost:1;
  check_string "bomb" "tinker.bomb" (Option.get (occupant u 1 3));
  check_int "life" 5 (life_at u 1 3);
  check_int "attack" 0 (attack_at u 1 3)

let bomb_ticks_then_explodes () =
  let u = new_game ~special:Tinker.house () in
  place u 1 "tinker.bomb" 2;
  on_turn u 1;
  check_int "ticked" 4 (life_at u 1 2);
  check_int "owner unhurt" 60 (player_life u 1);
  for _ = 1 to 4 do
    on_turn u 1
  done;
  check_bool "gone" true (is_empty u 1 2);
  check_int "blast 3" 57 (player_life u 1)

let spider_plants_and_swings_again () =
  let u = new_game ~mana:all_mana ~special:Tinker.house () in
  play u 0 "tinker.spider" ~input:(Some 2) ~cost:2;
  check_string "bomb opposite" "tinker.bomb" (Option.get (occupant u 1 2));
  let v = new_game ~special:Tinker.house () in
  place v 0 "tinker.spider" 1;
  place v 1 "fire.goblin" 1;
  hit ~is_ability:true v 1 1 13;
  run_at v 0 1;
  check_bool "killed" true (is_empty v 1 1);
  check_int "follow-up on player" 56 (player_life v 1)

let falcon_right_then_left () =
  let u = new_game ~mana:all_mana ~special:Tinker.house () in
  play u 0 "tinker.falcon" ~input:(Some 2) ~cost:3;
  check_string "rightmost" "tinker.bomb" (Option.get (occupant u 1 5));
  destroy_at u 0 2;
  check_string "leftmost" "tinker.bomb" (Option.get (occupant u 1 0));
  check_string "still right" "tinker.bomb" (Option.get (occupant u 1 5))

let scatter_plants_in_emptied_slots () =
  let u = new_game ~mana:all_mana ~special:Tinker.house () in
  place u 1 "fire.wall" 1;
  place u 1 "earth.sprite" 3;
  play u 0 "tinker.scatter" ~input:None ~cost:4;
  check_string "wall gone to bomb" "tinker.bomb" (Option.get (occupant u 1 1));
  check_string "sprite lives" "earth.sprite" (Option.get (occupant u 1 3));
  check_int "sprite 14" 14 (life_at u 1 3)

let soldier_grows_on_opp_bombs () =
  let u = new_game ~special:Tinker.house () in
  place u 0 "tinker.soldier" 2;
  place u 1 "tinker.bomb" 0;
  place u 1 "tinker.bomb" 1;
  check_int "base" 3 (attack_at u 0 2);
  destroy_at u 1 0;
  check_int "plus one" 4 (attack_at u 0 2);
  check_int "blast" 57 (player_life u 1);
  destroy_at u 1 1;
  check_int "plus two" 5 (attack_at u 0 2)

let automaton_plants_the_only_open () =
  let u = new_game ~special:Tinker.house () in
  place u 0 "tinker.automaton" 2;
  List.iter (fun n -> place u 1 "fire.goblin" n) [ 0; 1; 2; 3; 5 ];
  on_end u 0;
  check_string "slot 4" "tinker.bomb" (Option.get (occupant u 1 4))

let queen_detonates_for_six () =
  let u = new_game ~mana:all_mana ~special:Tinker.house () in
  place u 1 "tinker.bomb" 1;
  place u 1 "tinker.bomb" 4;
  play u 0 "tinker.queen" ~input:(Some 2) ~cost:7;
  check_bool "bombs gone" true (is_empty u 1 1 && is_empty u 1 4);
  check_int "12 from two blasts" 48 (player_life u 1);
  check_string "queen" "tinker.queen" (Option.get (occupant u 0 2))

let dragon_swings_and_plants_three () =
  let u = new_game ~mana:all_mana ~special:Tinker.house () in
  place u 1 "fire.goblin" 0;
  place u 1 "fire.goblin" 1;
  place u 1 "fire.wall" 2;
  play u 0 "tinker.dragon" ~input:(Some 2) ~cost:8;
  check_bool "wall to bomb or empty" true
    (match occupant u 1 2 with Some "tinker.bomb" | None -> true | _ -> false);
  let bombs =
    List.init 6 (fun n -> match occupant u 1 n with Some "tinker.bomb" -> 1 | _ -> 0)
    |> List.fold_left ( + ) 0
  in
  check_int "three bombs" 3 bombs;
  check_int "kill refund" 3 (mana u 0 4)

(* ----------------------------------------------------------- entomologist *)

let beetle_hits_and_feeds_fire () =
  let u = new_game ~special:Entomologist.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "entomologist.beetle" ~input:(Some 2) ~cost:1;
  check_int "4 on opposite" 12 (life_at u 1 2);
  kill u 0 2;
  check_int "fire +1" 7 (mana u 0 0)

let hivemind_aims_everyone () =
  let u = new_game ~mana:all_mana ~special:Entomologist.house () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.goblin" 2;
  place u 1 "fire.goblin" 4;
  play u 0 "entomologist.hivemind" ~input:(Some 4) ~cost:3;
  check_int "left aims at 4" 4 (List.hd (Option.get (slot u 0 0).value).target);
  check_int "mid aims at 4" 4 (List.hd (Option.get (slot u 0 2).value).target);
  on_end u 0;
  check_int "recovered" 0 (List.hd (Option.get (slot u 0 0).value).target)

let locust_ticks_once () =
  let u = new_game ~mana:all_mana ~special:Entomologist.house () in
  place u 1 "fire.goblin" 3;
  play u 0 "entomologist.locust" ~input:(Some 3) ~cost:4;
  check_bool "cursed" true (flagged u 1 3 cursed_flag);
  on_end u 0;
  check_int "8 on creature" 8 (life_at u 1 3);
  check_int "4 on player" 56 (player_life u 1)

let giant_ant_soaks_and_discounts () =
  let u = new_game ~mana:all_mana ~special:Entomologist.house () in
  play u 0 "entomologist.ant" ~input:(Some 2) ~cost:5;
  hit u 0 2 5;
  check_int "3 after -2" 20 (life_at u 0 2);
  check_int "next ant 3" 3 (special_cost u 0 "entomologist.ant")

let assassin_spawns_drones () =
  let u = new_game ~mana:all_mana ~special:Entomologist.house () in
  play u 0 "entomologist.assassin" ~input:(Some 2) ~cost:6;
  check_string "left drone" "entomologist.drone" (Option.get (occupant u 0 1));
  check_string "right drone" "entomologist.drone" (Option.get (occupant u 0 3));
  settle u;
  check_int "+4 from two drones" 9 (attack_at u 0 2)

let hive_fills_the_open_slot () =
  let u = new_game ~special:Entomologist.house () in
  place u 0 "entomologist.hive" 1;
  List.iter (fun n -> place u 0 "fire.goblin" n) [ 2; 3; 4; 5 ];
  on_end u 0;
  check_string "insect in 0" "entomologist.warrior" (Option.get (occupant u 0 0))

let mantis_bridles_growth () =
  let u = new_game ~mana:all_mana ~special:Entomologist.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "entomologist.mantis" ~input:(Some 2) ~cost:8;
  check_int "opp fire mana -1" 5 (mana u 1 0);
  check_int "fire growth stolen" 0 (growth u 1 0);
  kill u 0 2;
  check_int "5 on opposite" 11 (life_at u 1 2);
  check_int "growth restored" 1 (growth u 1 0)

(* ------------------------------------------------------------- soulbinder *)

let start_plants_one_soul () =
  let u = new_game ~special:Soulbinder.house () in
  on_start u 0;
  check_int "one soul" 1 (List.length (souls u 0))

let summon_soul_and_mana () =
  let u = new_game ~mana:all_mana ~special:Soulbinder.house () in
  play u 0 "soulbinder.summon" ~input:(Some 3) ~cost:2;
  check_bool "soul at 3" true (List.mem 3 (souls u 0));
  check_int "fire +1" 7 (mana u 0 0)

let tethered_returns_a_soul () =
  let u = new_game ~mana:all_mana ~special:Soulbinder.house () in
  play u 0 "soulbinder.summon" ~input:(Some 2) ~cost:2;
  play u 0 "soulbinder.tethered" ~input:(Some 2) ~cost:1;
  check_bool "consumed" false (List.mem 2 (souls u 0));
  kill u 0 2;
  check_bool "restored" true (List.mem 2 (souls u 0))

let empty_souls_hide_creatures () =
  let u = new_game ~special:Soulbinder.house () in
  on_start u 0;
  check_bool "tethered playable" true (card_enabled u 0 "soulbinder.tethered");
  step u (fun () -> Player.set_data (player u 0) (Soulbinder_data []));
  check_bool "creatures hidden" false (card_enabled u 0 "soulbinder.tethered");
  check_bool "spells stay" true (card_enabled u 0 "soulbinder.release")

let sheppard_gains_from_souls () =
  let u = new_game ~mana:all_mana ~special:Soulbinder.house () in
  play u 0 "soulbinder.summon" ~input:(Some 1) ~cost:2;
  play u 0 "soulbinder.summon" ~input:(Some 3) ~cost:2;
  play u 0 "soulbinder.sheppard" ~input:(Some 1) ~cost:6;
  settle u;
  check_int "4+1 remaining soul" 5 (attack_at u 0 1)

let priestess_passage () =
  let u = new_game ~mana:all_mana ~special:Soulbinder.house () in
  play u 0 "soulbinder.summon" ~input:(Some 2) ~cost:2;
  play u 0 "soulbinder.priestess" ~input:(Some 2) ~cost:7;
  (* own souls: consumed the one we summoned onto; opponent also soulbinder with 0 *)
  check_int "no souls left after summon-over" 0 (List.length (souls u 0))

let release_clears_and_replants () =
  let u = new_game ~mana:all_mana ~special:Soulbinder.house () in
  play u 0 "soulbinder.summon" ~input:(Some 0) ~cost:2;
  play u 0 "soulbinder.summon" ~input:(Some 1) ~cost:2;
  let life = player_life u 0 in
  play u 0 "soulbinder.release" ~input:None ~cost:5;
  check_int "heal 3 per soul (2 own)" (life + 6) (player_life u 0);
  check_int "two new souls" 2 (List.length (souls u 0))

(* ----------------------------------------------------------- shadowcaster *)

let shadowcaster_costs () =
  init_all_houses ();
  check_int "bat" 1 (card_by_key "shadowcaster.bat").cost;
  check_int "priestess" 8 (card_by_key "shadowcaster.priestess").cost

let wave_is_power_plus_two () =
  let u = new_game ~mana:all_mana ~special:Shadowcaster.house () in
  place u 1 "fire.goblin" 1;
  step u (fun () -> Player.set_data (player u 0) (Shadowcaster_data 5));
  play u 0 "shadowcaster.wave" ~input:None ~cost:2;
  check_int "7" 9 (life_at u 1 1)

let scythe_kills_and_pays () =
  let u = new_game ~mana:all_mana ~special:Shadowcaster.house () in
  List.iter (fun n -> place u 1 "fire.goblin" n) [ 0; 1; 2 ];
  step u (fun () -> Player.set_data (player u 0) (Shadowcaster_data 12));
  play u 0 "shadowcaster.scythe" ~input:None ~cost:5;
  check_bool "board clear" true (is_empty u 1 0 && is_empty u 1 1 && is_empty u 1 2);
  check_int "2 per kill" 54 (player_life u 1);
  check_int "power +3" 15 (power u 0)

let assassin_kills_on_summon_turn () =
  let u = new_game ~mana:all_mana ~special:Shadowcaster.house () in
  place u 1 "fire.goblin" 2;
  hit ~is_ability:true u 1 2 15;
  play u 0 "shadowcaster.assassin" ~input:(Some 2) ~cost:6;
  run_at u 0 2;
  check_bool "dead" true (is_empty u 1 2);
  check_int "power" 1 (power u 0)

let mage_taxes_and_feeds_power () =
  let u = new_game ~mana:all_mana ~special:Shadowcaster.house () in
  play u 0 "shadowcaster.mage" ~input:(Some 0) ~cost:4;
  check_int "scythe costs 6" 6 (special_cost u 0 "shadowcaster.scythe");
  place u 1 "fire.goblin" 1;
  let before = power u 0 in
  let special = mana u 0 4 in
  play u 0 "shadowcaster.scythe" ~input:None ~cost:6;
  check_int "power +1" (before + 1) (power u 0);
  check_int "paid tax" (special - 6) (mana u 0 4)

let priestess_stuns_and_dies_for_two () =
  let u = new_game ~mana:all_mana ~special:Shadowcaster.house () in
  place u 1 "fire.goblin" 0;
  place u 1 "fire.goblin" 1;
  play u 0 "shadowcaster.priestess" ~input:(Some 2) ~cost:8;
  check_bool "stunned 0" true (flagged u 1 0 stun_flag);
  check_bool "stunned 1" true (flagged u 1 1 stun_flag);
  let before = power u 0 in
  kill u 0 2;
  check_int "+2" (before + 2) (power u 0)

let noble_only_from_opp_spell () =
  let u = new_game ~mana:all_mana ~special:Shadowcaster.house () in
  play u 0 "shadowcaster.noble" ~input:(Some 1) ~cost:3;
  let v = new_game ~mana:all_mana ~special:Shadowcaster.house () in
  play v 0 "shadowcaster.noble" ~input:(Some 1) ~cost:3;
  hit ~is_spell:true u 0 1 100;
  check_int "spell" 2 (power u 0);
  hit ~is_ability:true v 0 1 100;
  check_int "ability" 0 (power v 0)

let bat_heals_when_it_hits () =
  let u = new_game ~special:Shadowcaster.house () in
  play u 0 "shadowcaster.bat" ~input:(Some 1) ~cost:1;
  place u 1 "fire.goblin" 1;
  hit ~is_ability:true u 0 1 5;
  check_int "hurt" 6 (life_at u 0 1);
  step u (fun () -> Slot.toggle_run (slot u 0 1));
  run_at u 0 1;
  check_int "healed 3" 9 (life_at u 0 1)

(* ---------------------------------------------------------- spirit shaman *)

let shaman_costs () =
  init_all_houses ();
  check_int "tortoise" 1 (card_by_key "spiritshaman.tortoise").cost;
  check_int "eagle" 8 (card_by_key "spiritshaman.eagle").cost

let tortoise_releases_neighbour () =
  let u = new_game ~mana:all_mana ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.deer" ~input:(Some 1) ~cost:3;
  play u 0 "spiritshaman.tortoise" ~input:(Some 2) ~cost:1;
  check_string "deer spirit" "spiritshaman.deer.spirit" (Option.get (occupant u 0 1));
  check_string "tortoise stays" "spiritshaman.tortoise" (Option.get (occupant u 0 2))

let tortoise_on_another_totem () =
  let u = new_game ~mana:all_mana ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.tortoise" ~input:(Some 0) ~cost:1;
  play u 0 "spiritshaman.deer" ~input:(Some 3) ~cost:3;
  check_string "released" "spiritshaman.tortoise.spirit" (Option.get (occupant u 0 0));
  check_int "spirit +1" 7 (mana u 0 4)

let fox_hurts_then_releases_at_five () =
  let u = new_game ~mana:all_mana ~special:Spiritshaman.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "spiritshaman.fox" ~input:(Some 2) ~cost:2;
  check_int "goblin -4" 12 (life_at u 1 2);
  hit ~is_ability:true u 0 2 6;
  on_turn u 0;
  check_string "fox spirit" "spiritshaman.fox.spirit" (Option.get (occupant u 0 2))

let fox_releases_the_others () =
  let u = new_game ~mana:all_mana ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.fox" ~input:(Some 0) ~cost:2;
  play u 0 "spiritshaman.deer" ~input:(Some 1) ~cost:3;
  play u 0 "spiritshaman.bear" ~input:(Some 3) ~cost:7;
  hit ~is_ability:true u 0 0 7;
  on_turn u 0;
  check_string "fox" "spiritshaman.fox.spirit" (Option.get (occupant u 0 0));
  check_string "deer" "spiritshaman.deer.spirit" (Option.get (occupant u 0 1));
  check_string "bear" "spiritshaman.bear.spirit" (Option.get (occupant u 0 3))

let deer_at_seven_earth () =
  let u = new_game ~mana:[ 10; 10; 10; 7; 10 ] ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.deer" ~input:(Some 1) ~cost:3;
  play u 0 "spiritshaman.eagle" ~input:(Some 2) ~cost:8;
  hit ~is_ability:true u 0 2 3;
  on_turn u 0;
  check_string "deer spirit" "spiritshaman.deer.spirit" (Option.get (occupant u 0 1))

let eagle_trades_spirit () =
  let u = new_game ~mana:[ 10; 10; 10; 10; 14 ] ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.eagle" ~input:(Some 1) ~cost:8;
  check_string "spirit" "spiritshaman.eagle.spirit" (Option.get (occupant u 0 1));
  check_int "14-8+4-4" 6 (mana u 0 4);
  check_int "basics +2" 12 (mana u 0 0)

let bear_at_eight_creatures () =
  let u = new_game ~mana:all_mana ~special:Spiritshaman.house () in
  List.iter (fun n -> place u 1 "fire.goblin" n) [ 0; 1; 2; 3 ];
  List.iter (fun n -> place u 0 "fire.goblin" n) [ 0; 1; 2 ];
  play u 0 "spiritshaman.bear" ~input:(Some 3) ~cost:7;
  check_string "bear spirit" "spiritshaman.bear.spirit" (Option.get (occupant u 0 3));
  check_int "8 on goblins" 8 (life_at u 1 0)

let panther_grows_then_releases () =
  let u = new_game ~mana:all_mana ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.panther" ~input:(Some 1) ~cost:5;
  place u 0 "fire.goblin" 0;
  on_turn u 0;
  check_string "still totem" "spiritshaman.panther" (Option.get (occupant u 0 1));
  check_int "growth 1" 1 (attack_at u 0 1);
  for _ = 1 to 7 do
    on_turn u 0
  done;
  check_string "spirit" "spiritshaman.panther.spirit" (Option.get (occupant u 0 1));
  check_int "2+8" 10 (attack_at u 0 1)

let awakening_heals_and_releases () =
  let u = new_game ~mana:[ 10; 10; 10; 10; 20 ] ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.deer" ~input:(Some 0) ~cost:3;
  play u 0 "spiritshaman.fox" ~input:(Some 1) ~cost:2;
  step u (fun () -> Player.inflict (player u 0) (mk_damage 10 (ctx 1)));
  let before = mana u 0 4 in
  let life = player_life u 0 in
  play u 0 "spiritshaman.awakening" ~input:None ~cost:6;
  check_int "heal spirit-before" (life + before) (player_life u 0);
  check_string "deer" "spiritshaman.deer.spirit" (Option.get (occupant u 0 0));
  check_string "fox" "spiritshaman.fox.spirit" (Option.get (occupant u 0 1))

let replaced_event () =
  let u = new_game ~mana:[ 10; 10; 10; 7; 10 ] ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.deer" ~input:(Some 1) ~cost:3;
  let seen = ref [] in
  Updater.listen u (function
    | Ev_replaced { num; creature; _ } -> seen := (num, creature.c_card.key) :: !seen
    | _ -> ());
  on_turn u 0;
  check_string "deer spirit" "spiritshaman.deer.spirit" (Option.get (occupant u 0 1));
  check_int "one replace" 1 (List.length !seen);
  check_int "slot 1" 1 (fst (List.hd !seen))

let ready_spirit_keeps_run () =
  let u = new_game ~mana:[ 10; 10; 10; 7; 10 ] ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.deer" ~input:(Some 1) ~cost:3;
  step u (fun () -> Slots.toggle_run (player u 0).slots_update);
  on_turn u 0;
  check_bool "runnable" true (Slot.is_runnable (Option.get (slot u 0 1).value));
  let v = new_game ~mana:[ 10; 10; 10; 7; 10 ] ~special:Spiritshaman.house () in
  play v 0 "spiritshaman.deer" ~input:(Some 1) ~cost:3;
  on_turn v 0;
  check_bool "fresh waits" false (Slot.is_runnable (Option.get (slot v 0 1).value))

let panther_keeps_growth_after_move () =
  let u = new_game ~mana:all_mana ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.panther" ~input:(Some 1) ~cost:5;
  for _ = 1 to 3 do
    on_turn u 0
  done;
  check_int "grown" 3 (attack_at u 0 1);
  step u (fun () -> Slots.move (player u 0).slots_update 1 4);
  check_string "moved" "spiritshaman.panther" (Option.get (occupant u 0 4));
  check_int "growth kept" 3 (attack_at u 0 4);
  place u 0 "fire.goblin" 2;
  step u (fun () -> Slots.move (player u 0).slots_update 4 2);
  check_int "after swap" 3 (attack_at u 0 2);
  step u (fun () -> Spiritshaman.release (slot u 0 2));
  check_string "spirit" "spiritshaman.panther.spirit" (Option.get (occupant u 0 2));
  check_int "2+3" 5 (attack_at u 0 2)

let grown_totem_does_not_attack () =
  let u = new_game ~mana:all_mana ~special:Spiritshaman.house () in
  play u 0 "spiritshaman.panther" ~input:(Some 1) ~cost:5;
  on_turn u 0;
  step u (fun () -> Slots.toggle_run (player u 0).slots_update);
  check_int "growth" 1 (attack_at u 0 1);
  let ran = ref 0 in
  Updater.listen u (function Ev_run_slot _ -> incr ran | _ -> ());
  step u (fun () -> Player.run_slots (player u 0));
  check_int "no swing" 0 !ran;
  check_int "opp untouched" 60 (player_life u 1)

let () =
  let open Alcotest in
  run "houses_d"
    [
      ( "tinker",
        [
          test_case "costs" `Quick tinker_costs;
          test_case "plant" `Quick plant_puts_a_bomb;
          test_case "tick" `Quick bomb_ticks_then_explodes;
          test_case "spider" `Quick spider_plants_and_swings_again;
          test_case "falcon" `Quick falcon_right_then_left;
          test_case "scatter" `Quick scatter_plants_in_emptied_slots;
          test_case "soldier" `Quick soldier_grows_on_opp_bombs;
          test_case "automaton" `Quick automaton_plants_the_only_open;
          test_case "queen" `Quick queen_detonates_for_six;
          test_case "dragon" `Quick dragon_swings_and_plants_three;
        ] );
      ( "entomologist",
        [
          test_case "beetle" `Quick beetle_hits_and_feeds_fire;
          test_case "hivemind" `Quick hivemind_aims_everyone;
          test_case "locust" `Quick locust_ticks_once;
          test_case "ant" `Quick giant_ant_soaks_and_discounts;
          test_case "assassin" `Quick assassin_spawns_drones;
          test_case "hive" `Quick hive_fills_the_open_slot;
          test_case "mantis" `Quick mantis_bridles_growth;
        ] );
      ( "soulbinder",
        [
          test_case "start" `Quick start_plants_one_soul;
          test_case "summon" `Quick summon_soul_and_mana;
          test_case "tethered" `Quick tethered_returns_a_soul;
          test_case "hide" `Quick empty_souls_hide_creatures;
          test_case "sheppard" `Quick sheppard_gains_from_souls;
          test_case "priestess" `Quick priestess_passage;
          test_case "release" `Quick release_clears_and_replants;
        ] );
      ( "shadowcaster",
        [
          test_case "costs" `Quick shadowcaster_costs;
          test_case "wave" `Quick wave_is_power_plus_two;
          test_case "scythe" `Quick scythe_kills_and_pays;
          test_case "assassin" `Quick assassin_kills_on_summon_turn;
          test_case "mage" `Quick mage_taxes_and_feeds_power;
          test_case "priestess" `Quick priestess_stuns_and_dies_for_two;
          test_case "noble" `Quick noble_only_from_opp_spell;
          test_case "bat" `Quick bat_heals_when_it_hits;
        ] );
      ( "spirit shaman",
        [
          test_case "costs" `Quick shaman_costs;
          test_case "tortoise neighbour" `Quick tortoise_releases_neighbour;
          test_case "tortoise on summon" `Quick tortoise_on_another_totem;
          test_case "fox" `Quick fox_hurts_then_releases_at_five;
          test_case "fox chain" `Quick fox_releases_the_others;
          test_case "deer" `Quick deer_at_seven_earth;
          test_case "eagle" `Quick eagle_trades_spirit;
          test_case "bear" `Quick bear_at_eight_creatures;
          test_case "panther" `Quick panther_grows_then_releases;
          test_case "awakening" `Quick awakening_heals_and_releases;
          test_case "replaced" `Quick replaced_event;
          test_case "run flag" `Quick ready_spirit_keeps_run;
          test_case "panther move" `Quick panther_keeps_growth_after_move;
          test_case "totem silent" `Quick grown_totem_does_not_attack;
        ] );
    ]
