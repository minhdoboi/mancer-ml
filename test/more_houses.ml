(* Mechanics for Dudes, Vampires, Warp, Snowblood and Dreamweaver, same style as test/houses.ml. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit ?(is_ability = false) u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage ~is_ability amount (ctx (1 - pid))))

let kill u pid num = hit u pid num 1000
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana

(* [place] writes states without a refresh; attack values catch up on the next engine step *)
let settle u = step u (fun () -> ())
let all_mana = [ 6; 4; 4; 4; 10 ]

(* ------------------------------------------------------------------ dudes *)

let dudes_costs () =
  init_all_houses ();
  check_int "eight dudes" 8 (List.length Dudes.house.h_cards);
  check_int "Axl is third" 3 (card_by_key "Axl").cost;
  check_int "Edi E is last" 8 (card_by_key "Edi E").cost

let dude_fights () =
  let u = new_game ~special:Dudes.house () in
  place u 0 "Bill Bull" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "7 attack" (60 - 7) (player_life u 1)

(* ---------------------------------------------------------------- vampires *)

let noctule_heals_on_player_hit () =
  let u = new_game ~special:Vampires.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "vampires.noctule" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "enemy takes 5" 55 (player_life u 1);
  check_int "owner heals 5" 45 (player_life u 0)

let noctule_does_not_heal_on_creature () =
  let u = new_game ~special:Vampires.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "vampires.noctule" 0;
  place u 1 "earth.troll" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "no heal" 40 (player_life u 0)

let ghoul_executes_weak () =
  let u = new_game ~special:Vampires.house () in
  place u 0 "vampires.ghoul" 0;
  place u 1 "fire.dragon" 0;
  step u (fun () -> Slot.update (slot u 1 0) (fun st -> { st with life = 10 }));
  step u (fun () -> Player.run_slots (player u 0));
  check_bool "dragon with 10 life dies" true (is_empty u 1 0)

let ghoul_hits_strong_normally () =
  let u = new_game ~special:Vampires.house () in
  place u 0 "vampires.ghoul" 0;
  place u 1 "fire.dragon" 0;
  let before = life_at u 1 0 in
  step u (fun () -> Player.run_slots (player u 0));
  check_int "4 damage" (before - 4) (life_at u 1 0)

let aristocrat_follows_weakest () =
  let u = new_game ~special:Vampires.house () in
  place u 1 "fire.dragon" 1;
  place u 1 "fire.goblin" 4;
  step u (fun () -> Slot.update (slot u 1 4) (fun st -> { st with life = 3 }));
  place u 0 "vampires.aristocrat" 0;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "left slot 0" true (is_empty u 0 0);
  check_string "in front of the weakest" "vampires.aristocrat" (Option.get (occupant u 0 4))

let aristocrat_punishes_kill () =
  let u = new_game ~special:Vampires.house () in
  place u 0 "vampires.aristocrat" 0;
  place u 1 "fire.goblin" 0;
  step u (fun () -> Slot.update (slot u 1 0) (fun st -> { st with life = 3 }));
  step u (fun () -> Player.run_slots (player u 0));
  (* Scala reads the attack of the creature it killed (a goblin: 4) *)
  check_int "kill hits the player for the victim's attack" (60 - 4) (player_life u 1)

let mansion_makes_ghouls () =
  let u = new_game ~mana:all_mana ~special:Vampires.house () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.goblin" 2;
  play u 0 "vampires.mansion" ~input:(Some 1) ~cost:8;
  check_string "left" "vampires.ghoul" (Option.get (occupant u 0 0));
  check_string "right" "vampires.ghoul" (Option.get (occupant u 0 2))

let mansion_replaces_dead_basic () =
  let u = new_game ~special:Vampires.house () in
  place u 0 "vampires.mansion" 1;
  place u 0 "fire.goblin" 0;
  kill u 0 0;
  check_string "neophyte" "vampires.neophyte" (Option.get (occupant u 0 0))

let mansion_ignores_special_dead () =
  let u = new_game ~special:Vampires.house () in
  place u 0 "vampires.mansion" 1;
  place u 0 "vampires.ghoul" 0;
  kill u 0 0;
  check_bool "stays empty" true (is_empty u 0 0)

let nosferatu_feeds_on_death () =
  let u = new_game ~special:Vampires.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "vampires.nosferatu" 0;
  step u (fun () -> Slot.update (slot u 0 0) (fun st -> { st with life = 20 }));
  place u 1 "fire.goblin" 3;
  kill u 1 3;
  check_int "heals itself 2" 22 (life_at u 0 0);
  check_int "heals owner 3" 43 (player_life u 0)

let acolyte_adds_four () =
  let u = new_game ~special:Vampires.house () in
  place u 0 "vampires.acolyte" 0;
  place u 1 "fire.dragon" 2;
  let before = life_at u 1 2 in
  hit u 1 2 8;
  (* hit [u 1 2] is damage from player 0 *)
  check_int "8 + 4" (before - 12) (life_at u 1 2)

let acolyte_ignores_small_hits () =
  let u = new_game ~special:Vampires.house () in
  place u 0 "vampires.acolyte" 0;
  place u 1 "fire.dragon" 2;
  let before = life_at u 1 2 in
  hit u 1 2 5;
  check_int "5 only" (before - 5) (life_at u 1 2)

let flock_moves_weakest () =
  let u = new_game ~special:Vampires.house () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.dragon" 1;
  step u (fun () -> Slot.update (slot u 0 0) (fun st -> { st with life = 2 }));
  play u 0 "vampires.flock" ~input:(Some 4) ~cost:1;
  check_bool "left" true (is_empty u 0 0);
  check_string "arrived" "fire.goblin" (Option.get (occupant u 0 4));
  check_bool "invulnerable" true (has_flag (Option.get (slot u 0 4).value) invincible_flag);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "for one turn" false (has_flag (Option.get (slot u 0 4).value) invincible_flag)

let ties_feeds_neighbours () =
  let u = new_game ~special:Vampires.house () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.dragon" 1;
  place u 0 "fire.goblin" 2;
  let a = attack_at u 0 1 in
  let n = attack_at u 0 0 in
  play u 0 "vampires.ties" ~input:(Some 1) ~cost:5;
  check_bool "dragon gone" true (is_empty u 0 1);
  check_int "left +attack" (n + a) (attack_at u 0 0);
  check_int "right +attack" (n + a) (attack_at u 0 2)

(* --------------------------------------------------------------- snowblood *)

let hunter_pays_for_kill () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  place u 1 "fire.goblin" 2;
  step u (fun () -> Slot.update (slot u 1 2) (fun st -> { st with life = 2 }));
  let cost = (card_by_key "fire.goblin").cost in
  play u 0 "snowblood.hunter" ~input:(Some 2) ~cost:2;
  check_bool "goblin dead" true (is_empty u 1 2);
  check_int "ceil(cost / 3) fire mana" (6 + ((cost + 2) / 3)) (mana u 0 0)

let hunter_no_bounty_on_survivor () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  place u 1 "fire.dragon" 2;
  let before = life_at u 1 2 in
  play u 0 "snowblood.hunter" ~input:(Some 2) ~cost:2;
  check_int "2 damage" (before - 2) (life_at u 1 2);
  check_int "no mana" 6 (mana u 0 0)

let tracker_shields_neighbour () =
  let u = new_game ~special:Snowblood.house () in
  place u 0 "snowblood.tracker" 1;
  play u 0 "fire.goblin" ~input:(Some 2) ~cost:1;
  check_bool "invulnerable" true (has_flag (Option.get (slot u 0 2).value) invincible_flag);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "one turn" false (has_flag (Option.get (slot u 0 2).value) invincible_flag)

let maiko_weakens_three () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  List.iter (fun n -> place u 1 "fire.dragon" n) [ 0; 1; 2 ];
  let a = attack_at u 1 0 in
  play u 0 "snowblood.maiko" ~input:(Some 1) ~cost:3;
  check_int "slot 0" (a - 1) (attack_at u 1 0);
  check_int "slot 1" (a - 1) (attack_at u 1 1);
  check_int "slot 2" (a - 1) (attack_at u 1 2);
  place u 1 "fire.dragon" 4;
  check_int "out of reach" a (attack_at u 1 4);
  place u 1 "fire.dragon" 3;
  check_int "outside the three" a (attack_at u 1 3);
  kill u 0 1;
  check_int "restored on death" a (attack_at u 1 0)

let maiko_weakens_late_arrivals () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  play u 0 "snowblood.maiko" ~input:(Some 1) ~cost:3;
  place u 1 "fire.dragon" 2;
  settle u;
  check_int "enemy that lands next to it" (9 - 1) (attack_at u 1 2)

let maiko_heals_neighbours () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  place u 0 "snowblood.maiko" 1;
  place u 0 "fire.dragon" 0;
  step u (fun () -> Slot.update (slot u 0 0) (fun st -> { st with life = 10 }));
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "+1" 11 (life_at u 0 0)

let maiko_becomes_a_letter () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  play u 0 "snowblood.maiko" ~input:(Some 1) ~cost:3;
  let special = (player u 0).pstate.ps_desc.init_desc.pd_houses.(1) in
  let cards =
    List.fold_left (fun cs m -> m special cs) special.phd_cards (player u 0).pstate.ps_desc.desc_mods
  in
  check_bool "letter replaces maiko" true (Array.exists (fun cd -> cd.cd_card.key = "snowblood.letter") cards);
  check_int "letter costs 3" 3 (card_by_key "snowblood.letter").cost

let letter_takes_one () =
  let u = new_game ~special:Snowblood.house () in
  place u 0 "snowblood.letter" 0;
  hit u 0 0 10;
  check_int "10 becomes 1" 2 (life_at u 0 0);
  step u (fun () -> Slot.heal (slot u 0 0) 5);
  check_int "no heal" 2 (life_at u 0 0)

let letter_hits_back_on_death () =
  let u = new_game ~special:Snowblood.house () in
  place u 0 "snowblood.letter" 0;
  place u 1 "fire.dragon" 0;
  let before = life_at u 1 0 in
  List.iter (fun _ -> hit u 0 0 10) [ 1; 2; 3 ];
  check_bool "letter dead" true (is_empty u 0 0);
  check_int "15 to the opposite" (before - 15) (life_at u 1 0)

let attacks_reach_a_letter () =
  let u = new_game ~special:Snowblood.house () in
  place u 1 "snowblood.letter" 0;
  place u 0 "fire.dragon" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "a 9 attack still deals 1" 2 (life_at u 1 0)

let kojiro_burns_the_run () =
  let u = new_game ~special:Snowblood.house () in
  List.iter (fun n -> place u 1 "fire.dragon" n) [ 1; 2; 3; 5 ];
  place u 0 "snowblood.kojiro" 2;
  let before n = life_at u 1 n in
  let b1 = before 1 and b2 = before 2 and b3 = before 3 and b5 = before 5 in
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "slot 1" (b1 - 2) (life_at u 1 1);
  check_int "slot 2" (b2 - 2) (life_at u 1 2);
  check_int "slot 3" (b3 - 2) (life_at u 1 3);
  check_int "slot 5 is cut off" b5 (life_at u 1 5)

let kojiro_rushes () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  place u 1 "fire.goblin" 0;
  play u 0 "snowblood.kojiro" ~input:(Some 0) ~cost:5;
  check_int "+4 on the turn it lands" 9 (attack_at u 0 0);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_int "gone after the turn" 5 (attack_at u 0 0)

let kojiro_rushes_only_when_opposed () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  play u 0 "snowblood.kojiro" ~input:(Some 0) ~cost:5;
  check_int "no bonus without an opposite creature" 5 (attack_at u 0 0)

let kojiro_lands_on_occupied () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  place u 0 "fire.goblin" 3;
  play u 0 "snowblood.kojiro" ~input:(Some 3) ~cost:5;
  check_string "replaced" "snowblood.kojiro" (Option.get (occupant u 0 3))

let janus_strikes_on_summon () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  place u 0 "fire.goblin" 1;
  place u 0 "fire.dragon" 4;
  hit u 0 1 4;
  let neighbour = life_at u 0 1 and other = life_at u 0 4 in
  let life = player_life u 0 and fire = mana u 0 0 in
  play u 0 "snowblood.janus" ~input:(Some 0) ~cost:7;
  check_int "neighbour healed" (neighbour + 2) (life_at u 0 1);
  check_int "other slot hit" (other - 1) (life_at u 0 4);
  check_int "owner heals" (life + 1) (player_life u 0);
  check_int "owner gains fire mana" (fire + 1) (mana u 0 0)

let janus_feeds_on_its_own_board () =
  let u = new_game ~special:Snowblood.house () in
  place u 0 "snowblood.janus" 0;
  place u 0 "fire.goblin" 1;
  place u 0 "fire.dragon" 4;
  hit u 0 1 4;
  let neighbour = life_at u 0 1 and other = life_at u 0 4 in
  let life = player_life u 0 and fire = mana u 0 0 in
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "neighbour healed" (neighbour + 2) (life_at u 0 1);
  check_int "other slot hit" (other - 1) (life_at u 0 4);
  check_int "owner heals" (life + 1) (player_life u 0);
  check_int "owner gains fire mana" (fire + 1) (mana u 0 0)

let echo_replays_and_attacks () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  place u 0 "snowblood.tracker" 0;
  place u 1 "fire.dragon" 0;
  let before = life_at u 1 0 in
  play u 0 "snowblood.echo" ~input:None ~cost:4;
  check_int "tracker attacks right away" (before - 3) (life_at u 1 0)

let amaterasu_fire () =
  let u = new_game ~special:Snowblood.house () in
  place u 0 "snowblood.amaterasu" 0;
  place u 1 "earth.troll" 3;
  let before = life_at u 1 3 in
  play u 0 "fire.goblin" ~input:(Some 3) ~cost:1;
  check_int "5 to the opposite" (before - 5) (life_at u 1 3)

let amaterasu_air_and_earth () =
  let u = new_game ~mana:all_mana ~special:Snowblood.house () in
  place u 0 "snowblood.amaterasu" 0;
  play u 0 "air.fairy" ~input:(Some 3) ~cost:1;
  check_int "air: 3 to the opponent" 57 (player_life u 1);
  play u 0 "earth.sprite" ~input:(Some 4) ~cost:1;
  check_int "earth: heal 3" 63 (player_life u 0)

let amaterasu_water () =
  let u = new_game ~mana:[ 6; 4; 5; 5; 10 ] ~special:Snowblood.house () in
  place u 0 "snowblood.amaterasu" 0;
  play u 0 "water.golem" ~input:(Some 3) ~cost:1;
  check_int "water is the lowest power" 4 (mana u 0 1)

let amaterasu_arrival () =
  let u = new_game ~special:Snowblood.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "earth.sprite" 0;
  place u 0 "earth.troll" 1;
  step u (fun () -> Slot.update (slot u 0 0) (fun st -> { st with life = 1 }));
  play u 0 "snowblood.amaterasu" ~input:(Some 2) ~cost:7;
  check_int "heals 3 per earth creature" 46 (player_life u 0)

let guide_hits_with_the_next_summon () =
  let u = new_game ~special:Snowblood.house () in
  place u 0 "snowblood.guide" 0;
  place u 1 "fire.dragon" 2;
  place u 1 "fire.dragon" 3;
  let a = life_at u 1 2 in
  play u 0 "fire.dragon" ~input:(Some 1) ~cost:6;
  check_int "9 to each" (a - 9) (life_at u 1 2);
  check_int "to each" (a - 9) (life_at u 1 3);
  play u 0 "fire.dragon" ~input:(Some 4) ~cost:6;
  check_int "only the next one" (a - 9) (life_at u 1 2)

let guide_heals_neighbours () =
  let u = new_game ~special:Snowblood.house () in
  place u 0 "snowblood.guide" 0;
  step u (fun () -> Slot.update (slot u 0 0) (fun st -> { st with life = 10 }));
  play u 0 "fire.goblin" ~input:(Some 1) ~cost:1;
  check_int "heals the run" 11 (life_at u 0 0)

(* -------------------------------------------------------------------- warp *)

let is_bridled u pid num =
  match (slot u pid num).value with
  | Some { card = { c_body = { b_data = Mortal_of _; _ }; _ }; _ } -> true
  | _ -> false

let quake_uses_power_gap () =
  let u = new_game ~mana:[ 6; 4; 4; 4; 10 ] ~special:Warp.house () in
  step u (fun () -> (player u 1).houses_update.hu_value.(0) <- { mana = 2; growth = 0 });
  place u 1 "fire.dragon" 0;
  let before = life_at u 1 0 in
  play u 0 "warp.quake" ~input:None ~cost:2;
  check_int "6 - 2" (before - 4) (life_at u 1 0)

let ram_destroys_and_pays () =
  let u = new_game ~mana:[ 4; 4; 4; 4; 10 ] ~special:Warp.house () in
  place u 1 "fire.dragon" 2;
  let cost = (card_by_key "fire.dragon").cost in
  play u 0 "warp.ram" ~input:(Some 2) ~cost:6;
  check_bool "gone" true (is_empty u 1 2);
  check_int "its owner gets cost - 3" (4 + cost - 3) (mana u 1 0)

let queen_silences_for_two_turns () =
  let u = new_game ~special:Warp.house () in
  place u 1 "fire.wall" 1;
  let before = life_at u 1 1 in
  play u 0 "warp.queen" ~input:(Some 0) ~cost:8;
  check_int "4 damage" (before - 4) (life_at u 1 1);
  check_bool "bridled" true (is_bridled u 1 1);
  check_bool "no ability" true (Option.is_none (Option.get (slot u 1 1).value).card.c_card.effects.direct);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_bool "still bridled after this turn" true (is_bridled u 1 1);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_bool "free after the next" false (is_bridled u 1 1);
  check_string "same card" "fire.wall" (Option.get (occupant u 1 1));
  check_int "life kept" (before - 4) (life_at u 1 1)

let cloak_adds_the_creature () =
  let u = new_game ~mana:all_mana ~special:Warp.house () in
  place u 0 "fire.goblin" 0;
  let life = life_at u 0 0 and a = attack_at u 0 0 in
  play u 0 "warp.cloak" ~input:(Some 0) ~cost:3;
  check_string "cloak on top" "warp.cloak" (Option.get (occupant u 0 0));
  check_int "life" (15 + life) (life_at u 0 0);
  check_int "attack" (2 + a) (attack_at u 0 0);
  check_int "2 fire mana" 8 (mana u 0 0);
  kill u 0 0;
  check_string "restored" "fire.goblin" (Option.get (occupant u 0 0));
  check_int "with its life" life (life_at u 0 0)

let schizo_silences_opposite () =
  let u = new_game ~special:Warp.house () in
  place u 1 "fire.wall" 0;
  place u 0 "warp.schizo" 0;
  check_bool "silenced" true (is_bridled u 1 0);
  kill u 0 0;
  check_bool "released" false (is_bridled u 1 0);
  check_string "still there" "fire.wall" (Option.get (occupant u 1 0))

let schizo_does_not_trigger_bennu () =
  let u = new_duel ~special0:Warp.house ~special1:Highpriest.house () in
  place u 1 "highpriest.Bennu" 0;
  place u 0 "warp.schizo" 0;
  check_string "still bennu" "highpriest.Bennu" (Option.get (occupant u 1 0));
  check_bool "silenced" true (is_bridled u 1 0);
  check_int "schizo not hit by the dying attack" 22 (life_at u 0 0)

let stranger_copies_power () =
  let u = new_game ~mana:all_mana ~special:Warp.house () in
  place u 0 "warp.stranger" 0;
  check_int "highest enemy power" 10 (attack_at u 0 0)

let stranger_merges () =
  let u = new_game ~mana:all_mana ~special:Warp.house () in
  place u 1 "fire.wall" 0;
  let opp = (Option.get (slot u 1 0).value).card.c_card in
  play u 0 "warp.stranger" ~input:(Some 0) ~cost:7;
  let st = Option.get (slot u 0 0).value in
  check_string "still a stranger" "warp.stranger" st.card.c_card.key;
  check_bool "with the opposite effects" true (st.card.c_card.effects == opp.effects);
  check_int "attack is still the stranger's" 10 st.attack;
  check_int "life is the stranger's" 30 st.life

(* The merge destroys the Stranger and drops the merged creature into its slot: a
   new occupant needs a new slot id, or the Stranger's death fade keeps hiding it *)
let stranger_merge_makes_a_new_occupant () =
  let u = new_game ~mana:all_mana ~special:Warp.house () in
  place u 1 "fire.monk" 0;
  place u 0 "warp.stranger" 0;
  let before = (Option.get (slot u 0 0).value).slot_id in
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  let st = Option.get (slot u 0 0).value in
  check_string "merged" "warp.stranger" st.card.c_card.key;
  check_bool "new occupant" true (st.slot_id <> before)

let photographer_replaces_neighbour () =
  let u = new_game ~special:Warp.house () in
  place u 0 "warp.photographer" 1;
  place u 0 "fire.dragon" 0;
  step u (fun () -> Slot.update (slot u 0 1) (fun st -> { st with life = 7 }));
  kill u 0 0;
  check_string "replaced" "warp.photographer" (Option.get (occupant u 0 0));
  check_int "same life as the original" 7 (life_at u 0 0)

let photographer_ignores_far_deaths () =
  let u = new_game ~special:Warp.house () in
  place u 0 "warp.photographer" 1;
  place u 0 "fire.dragon" 3;
  kill u 0 3;
  check_bool "stays empty" true (is_empty u 0 3)

(* The Photographer a death leaves behind has no reaction of its own: one mass damage
   over a neighbour and its own neighbour gives one Photographer, not a chain *)
let photographer_does_not_chain () =
  let u = new_game ~special:Warp.house () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.dragon" 1;
  place u 0 "warp.photographer" 2;
  step u (fun () ->
      Slot.update (slot u 0 0) (fun st -> { st with life = 3 });
      Slot.update (slot u 0 1) (fun st -> { st with life = 3 }));
  step u (fun () -> Slots.inflict_creatures (player u 0).slots_update (mk_damage 5 (ctx 1)));
  check_string "neighbour replaced" "warp.photographer" (Option.get (occupant u 0 1));
  check_bool "no chain" true (is_empty u 0 0)

let errant_hides_after_a_kill () =
  let u = new_game ~special:Warp.house () in
  place u 0 "warp.errant" 0;
  place u 1 "fire.goblin" 0;
  step u (fun () -> Slot.update (slot u 1 0) (fun st -> { st with life = 1 }));
  step u (fun () -> Player.run_slots (player u 0));
  check_bool "killed" true (is_empty u 1 0);
  check_bool "paused" true (has_flag (Option.get (slot u 0 0).value) paused_flag);
  hit u 0 0 1;
  check_bool "wakes when hit" false (has_flag (Option.get (slot u 0 0).value) paused_flag)

(* ------------------------------------------------------------- dreamweaver *)

let wisp_counts_company () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 0 "dreamweaver.wisp" 0;
  settle u;
  check_int "alone" 2 (attack_at u 0 0);
  place u 0 "fire.goblin" 1;
  place u 0 "fire.goblin" 2;
  settle u;
  check_int "two friends" 4 (attack_at u 0 0)

let wisp_resists_enemy_effects () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 0 "dreamweaver.wisp" 0;
  hit ~is_ability:true u 0 0 5;
  check_int "ability ignored" 8 (life_at u 0 0);
  hit u 0 0 5;
  check_int "attack hurts" 3 (life_at u 0 0)

let roc_halves_unopposed () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 0 "dreamweaver.roc" 0;
  settle u;
  check_int "unopposed" 3 (attack_at u 0 0);
  place u 1 "fire.goblin" 0;
  settle u;
  check_int "opposed" 6 (attack_at u 0 0)

let sword_counts_neighbours () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 0 "dreamweaver.sword" 1;
  settle u;
  check_int "alone" 4 (attack_at u 0 1);
  place u 0 "fire.goblin" 0;
  place u 0 "fire.goblin" 2;
  settle u;
  check_int "two neighbours" 6 (attack_at u 0 1);
  kill u 0 0;
  check_int "one left" 5 (attack_at u 0 1)

let sword_cuts_three () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 0 "dreamweaver.sword" 2;
  List.iter (fun n -> place u 1 "fire.dragon" n) [ 1; 2; 3; 4 ];
  let a = life_at u 1 1 in
  step u (fun () -> Player.run_slots (player u 0));
  let dmg = a - life_at u 1 1 in
  check_bool "hits something" true (dmg > 0);
  check_int "opposite" (a - dmg) (life_at u 1 2);
  check_int "right" (a - dmg) (life_at u 1 3);
  check_int "not the fourth" a (life_at u 1 4)

let castle_shields_friends () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 0 "dreamweaver.castle" 0;
  place u 0 "fire.dragon" 1;
  let a = life_at u 0 1 and c = life_at u 0 0 in
  hit u 0 1 5;
  check_int "2 less" (a - 3) (life_at u 0 1);
  hit u 0 0 5;
  check_int "not itself" (c - 5) (life_at u 0 0)

let castle_hits_everything () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 0 "dreamweaver.castle" 0;
  place u 1 "fire.dragon" 3;
  let a = life_at u 1 3 in
  step u (fun () -> Player.run_slots (player u 0));
  check_int "player" 58 (player_life u 1);
  check_int "creatures" (a - 2) (life_at u 1 3)

let aurora_heals_all () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 0 "fire.dragon" 0;
  place u 1 "fire.dragon" 0;
  step u (fun () ->
      Slot.update (slot u 0 0) (fun st -> { st with life = 1 });
      Slot.update (slot u 1 0) (fun st -> { st with life = 1 }));
  play u 0 "dreamweaver.aurora" ~input:None ~cost:3;
  check_int "mine" 13 (life_at u 0 0);
  check_int "theirs" 13 (life_at u 1 0);
  check_int "my fire +1" 7 (mana u 0 0);
  check_int "their fire +1" 7 (mana u 1 0)

let guide_heals_by_dream_power () =
  let u = new_game ~mana:all_mana ~special:Dreamweaver.house () in
  place u 0 "fire.dragon" 0;
  step u (fun () -> Slot.update (slot u 0 0) (fun st -> { st with life = 1 }));
  play u 0 "dreamweaver.guide" ~input:(Some 1) ~cost:4;
  (* the guide's cost is paid first: 10 - 4 *)
  check_int "heals 6" 7 (life_at u 0 0);
  check_int "3 life per creature" (60 + 6) (player_life u 0)

let mare_counts_empty_slots () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 1 "fire.dragon" 0;
  let a = life_at u 1 0 in
  play u 0 "dreamweaver.mare" ~input:(Some 0) ~cost:8;
  check_int "5 empty slots" (a - 30) (life_at u 1 0)

let butterfly_collects_mana () =
  let u = new_game ~special:Dreamweaver.house () in
  place u 0 "dreamweaver.butterfly" 0;
  play u 1 "fire.goblin" ~input:(Some 2) ~cost:1;
  check_int "owner gets 3 fire" 9 (mana u 0 0);
  play u 0 "fire.goblin" ~input:(Some 3) ~cost:1;
  check_int "own summons do not count" 8 (mana u 0 0)

let () =
  let open Alcotest in
  run "more_houses"
    [
      ("dudes", [ test_case "costs" `Quick dudes_costs; test_case "fight" `Quick dude_fights ]);
      ( "vampires",
        [
          test_case "noctule heals" `Quick noctule_heals_on_player_hit;
          test_case "noctule creature" `Quick noctule_does_not_heal_on_creature;
          test_case "ghoul executes" `Quick ghoul_executes_weak;
          test_case "ghoul normal" `Quick ghoul_hits_strong_normally;
          test_case "aristocrat moves" `Quick aristocrat_follows_weakest;
          test_case "aristocrat kill" `Quick aristocrat_punishes_kill;
          test_case "mansion ghouls" `Quick mansion_makes_ghouls;
          test_case "mansion neophyte" `Quick mansion_replaces_dead_basic;
          test_case "mansion special" `Quick mansion_ignores_special_dead;
          test_case "nosferatu" `Quick nosferatu_feeds_on_death;
          test_case "acolyte" `Quick acolyte_adds_four;
          test_case "acolyte small" `Quick acolyte_ignores_small_hits;
          test_case "flock" `Quick flock_moves_weakest;
          test_case "ties" `Quick ties_feeds_neighbours;
        ] );
      ( "snowblood",
        [
          test_case "hunter bounty" `Quick hunter_pays_for_kill;
          test_case "hunter no bounty" `Quick hunter_no_bounty_on_survivor;
          test_case "tracker" `Quick tracker_shields_neighbour;
          test_case "maiko weakens" `Quick maiko_weakens_three;
          test_case "maiko late" `Quick maiko_weakens_late_arrivals;
          test_case "maiko heals" `Quick maiko_heals_neighbours;
          test_case "maiko letter" `Quick maiko_becomes_a_letter;
          test_case "letter" `Quick letter_takes_one;
          test_case "letter death" `Quick letter_hits_back_on_death;
          test_case "letter attacked" `Quick attacks_reach_a_letter;
          test_case "kojiro run" `Quick kojiro_burns_the_run;
          test_case "kojiro rush" `Quick kojiro_rushes;
          test_case "kojiro needs an opponent" `Quick kojiro_rushes_only_when_opposed;
          test_case "kojiro occupied" `Quick kojiro_lands_on_occupied;
          test_case "janus turn" `Quick janus_feeds_on_its_own_board;
          test_case "janus on summon" `Quick janus_strikes_on_summon;
          test_case "echo" `Quick echo_replays_and_attacks;
          test_case "amaterasu fire" `Quick amaterasu_fire;
          test_case "amaterasu air earth" `Quick amaterasu_air_and_earth;
          test_case "amaterasu water" `Quick amaterasu_water;
          test_case "amaterasu arrival" `Quick amaterasu_arrival;
          test_case "guide hit" `Quick guide_hits_with_the_next_summon;
          test_case "guide heal" `Quick guide_heals_neighbours;
        ] );
      ( "warp",
        [
          test_case "quake" `Quick quake_uses_power_gap;
          test_case "ram" `Quick ram_destroys_and_pays;
          test_case "queen" `Quick queen_silences_for_two_turns;
          test_case "cloak" `Quick cloak_adds_the_creature;
          test_case "schizo" `Quick schizo_silences_opposite;
          test_case "schizo bennu" `Quick schizo_does_not_trigger_bennu;
          test_case "stranger power" `Quick stranger_copies_power;
          test_case "stranger merge" `Quick stranger_merges;
          test_case "stranger merge occupant" `Quick stranger_merge_makes_a_new_occupant;
          test_case "photographer" `Quick photographer_replaces_neighbour;
          test_case "photographer far" `Quick photographer_ignores_far_deaths;
          test_case "photographer chain" `Quick photographer_does_not_chain;
          test_case "errant" `Quick errant_hides_after_a_kill;
        ] );
      ( "dreamweaver",
        [
          test_case "wisp attack" `Quick wisp_counts_company;
          test_case "wisp ethereal" `Quick wisp_resists_enemy_effects;
          test_case "roc" `Quick roc_halves_unopposed;
          test_case "sword attack" `Quick sword_counts_neighbours;
          test_case "sword cleave" `Quick sword_cuts_three;
          test_case "castle shield" `Quick castle_shields_friends;
          test_case "castle attack" `Quick castle_hits_everything;
          test_case "aurora" `Quick aurora_heals_all;
          test_case "guide" `Quick guide_heals_by_dream_power;
          test_case "mare" `Quick mare_counts_empty_slots;
          test_case "butterfly" `Quick butterfly_collects_mana;
        ] );
    ]
