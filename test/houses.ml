(* Mechanics for Water, Air, Earth and Troopers, same style as test/mechanics.ml. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

(* ------------------------------------------------------------------ water *)

let golem_ignores_ability () =
  let u = new_game () in
  place u 1 "water.golem" 0;
  place u 0 "fire.wall" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "golem ignores ability damage" 12 (life_at u 1 0)

let golem_takes_combat () =
  let u = new_game () in
  place u 1 "water.golem" 0;
  place u 0 "fire.dragon" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "golem 12 - 9 combat" 3 (life_at u 1 0)

let turtle_absorbs_five () =
  let u = new_game () in
  place u 1 "water.turtle" 0;
  place u 0 "fire.goblin" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "4 attack - 5 absorb" 17 (life_at u 1 0)

let iceguard_halves_player_damage () =
  let u = new_game () in
  place u 1 "water.guard" 1;
  let d = mk_damage ~is_spell:true 9 { ctx_player = 0; ctx_card = None; ctx_selected = 0 } in
  step u (fun () -> Player.inflict (player u 1) d);
  check_int "ceil(9/2) = 5" (60 - 5) (player_life u 1)

let elder_air_growth () =
  let u = new_game () in
  let growth () = (player u 0).houses_update.hu_value.(2).growth in
  let before = growth () in
  place u 0 "water.elder" 0;
  check_int "air growth +1" (before + 1) (growth ())

let apostate_adds_fire () =
  let u = new_game ~mana0:3 () in
  play u 0 "water.apostate" ~input:(Some 0) ~cost:3;
  check_int "fire mana 3 + 2" 5 (player u 0).houses_update.hu_value.(0).mana

let overlord_gives_run () =
  let u = new_game () in
  place ~run:false u 0 "water.lord" 1;
  place ~run:false u 0 "fire.goblin" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "adjacent goblin attacks the turn it lands" (60 - 4) (player_life u 1)

(* -------------------------------------------------------------------- air *)

let griffin_fires_when_air_ge_5 () =
  let u = new_game ~mana:[ 4; 4; 6; 4 ] () in
  play u 0 "air.griffin" ~input:(Some 0) ~cost:2;
  check_int "pre-spend air 6 >= 5" (60 - 5) (player_life u 1)

let griffin_skips_when_air_lt_5 () =
  let u = new_game ~mana:[ 4; 4; 4; 4 ] () in
  play u 0 "air.griffin" ~input:(Some 0) ~cost:2;
  check_int "pre-spend air 4 < 5" 60 (player_life u 1)

let phoenix_reborn () =
  let u = new_game ~mana0:10 () in
  place u 0 "air.phoenix" 0;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 18 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  Alcotest.(check (option string)) "phoenix returns" (Some "air.phoenix") (occupant u 0 0)

let phoenix_stays_dead_without_fire () =
  let u = new_game ~mana0:9 () in
  place u 0 "air.phoenix" 0;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 18 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_bool "mana 9 is not enough" true (is_empty u 0 0)

let dead_hook_override_suppresses_on_my_death () =
  let u = new_game ~mana0:10 () in
  place u 0 "air.phoenix" 0;
  (* SlotsUpdate.onDead is a var: a house that replaces it decides whether
     onMyDeath runs, so the phoenix must not come back here *)
  (player u 0).slots_update.on_dead_hook <- ignore;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 18 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_bool "no rebirth" true (is_empty u 0 0)

let twister_destroys () =
  let u = new_game ~mana:[ 4; 4; 10; 4 ] () in
  place u 1 "fire.dragon" 2;
  play u 0 "air.twister" ~input:(Some 2) ~cost:10;
  check_bool "dragon is destroyed" true (is_empty u 1 2)

let cloud_is_multi () =
  let u = new_game () in
  place u 0 "air.cloud" 0;
  place u 1 "fire.goblin" 2;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "player takes 4" (60 - 4) (player_life u 1);
  check_int "off-lane goblin also takes 4" 12 (life_at u 1 2)

let titan_hits_opposite () =
  let u = new_game ~mana:[ 4; 4; 12; 4 ] () in
  place u 1 "fire.dragon" 1;
  play u 0 "air.titan" ~input:(Some 1) ~cost:12;
  check_int "dragon 41 - 15" 26 (life_at u 1 1)

let lightning_desc_fills_air_damage () =
  let u = new_game ~mana:[ 4; 4; 7; 4 ] () in
  let c = card_by_key "air.lightnin" in
  match c.desc_args with
  | None -> Alcotest.fail "lightning should compute [{0}] damage"
  | Some f ->
      Alcotest.(check (list string)) "5 + 7 air" [ "12" ] (f u.state 0);
      let text = Assets.describe ~state:u.state ~player:0 c in
      check_bool "placeholder filled" true (not (String.contains text '{'))

(* ------------------------------------------------------------------ earth *)

let hermit_earth_growth () =
  let u = new_game () in
  let growth () = (player u 0).houses_update.hu_value.(3).growth in
  let before = growth () in
  place u 0 "earth.hermit" 0;
  check_int "earth growth +2" (before + 2) (growth ())

let fury_sums_top_two () =
  let u = new_game ~mana:[ 4; 4; 4; 6 ] () in
  (* not adjacent, so no orc/bull bonus; no SpellMod on the board *)
  place u 0 "fire.monk" 0;
  place u 0 "fire.drake" 3;
  (* Fury reads the attacks published by the previous step *)
  step u ignore;
  play u 0 "earth.fury" ~input:None ~cost:6;
  check_int "3 + 4" (60 - 7) (player_life u 1)

let fury_counts_elemental_before_payment () =
  let u = new_game ~mana:[ 4; 4; 4; 10 ] () in
  place u 0 "earth.elemental" 0;
  place u 0 "fire.monk" 3;
  step u ignore;
  play u 0 "earth.fury" ~input:None ~cost:6;
  (* elemental attack = earth mana before paying (10), not after (4) *)
  check_int "10 + 3" (60 - 13) (player_life u 1)

let spider_fills_adjacent () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] () in
  play u 0 "earth.hugespider" ~input:(Some 2) ~cost:7;
  Alcotest.(check (option string)) "left spider" (Some "earth.forestspider") (occupant u 0 1);
  Alcotest.(check (option string)) "right spider" (Some "earth.forestspider") (occupant u 0 3)

let spider_skips_blocked_slot () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] () in
  Player.block_slot (player u 0) 1;
  play u 0 "earth.hugespider" ~input:(Some 2) ~cost:7;
  check_bool "blocked slot stays empty" true (is_empty u 0 1);
  Alcotest.(check (option string)) "right spider" (Some "earth.forestspider") (occupant u 0 3)

let care_heals_player_and_creature () =
  let u = new_game ~mana:[ 4; 4; 4; 2 ] () in
  place u 0 "fire.goblin" 0;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 8 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  step u (fun () ->
      Player.inflict (player u 0)
        (mk_damage ~is_spell:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  play u 0 "earth.care" ~input:(Some 0) ~cost:2;
  check_int "player +8" (50 + 8) (player_life u 0);
  check_int "goblin 8 + 8" 16 (life_at u 0 0)

(* -------------------------------------------------------------- troopers *)

let marine_pokes_summons () =
  let u = new_game () in
  place u 0 "Marine" 0;
  place u 1 "fire.goblin" 2;
  check_int "goblin 16 - 4" 12 (life_at u 1 2)

let goliath_ignores_spell () =
  let u = new_game () in
  place u 1 "Goliath" 0;
  play u 0 "fire.wave" ~input:None ~cost:6;
  check_int "immune to spell" 20 (life_at u 1 0)

let goliath_absorbs_combat () =
  let u = new_game () in
  place u 1 "Goliath" 0;
  place u 0 "fire.goblin" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "4 - 1 absorb" 17 (life_at u 1 0)

let siege_hits_fattest () =
  let u = new_game () in
  place u 0 "SiegeTank" 0;
  place u 1 "fire.goblin" 1;
  place u 1 "fire.dragon" 3;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "dragon 41 - 8" 33 (life_at u 1 3);
  check_int "goblin untouched" 16 (life_at u 1 1)

let conscription_special_mana () =
  let u = new_game () in
  play u 0 "Conscription" ~input:None ~cost:0;
  check_int "special mana 2 -> 3" 3 (player u 0).houses_update.hu_value.(4).mana

let special_deck_has_one_card_per_quarter () =
  init_all_houses ();
  let d = Game.make_player_desc ~special:Troopers.house Fire.house in
  Alcotest.(check (list string))
    "Troopers deck"
    [ "Conscription"; "Barracks"; "Goliath"; "NuclearMissile" ]
    (Array.to_list d.pd_houses.(1).phd_cards |> List.map (fun cd -> cd.cd_card.key))

let extra_sum u pid num =
  match (slot u pid num).value with
  | Some st ->
      List.fold_left (fun a -> function Extra_attacks n -> a + n | _ -> a) 0 st.attack_sources.sources
  | None -> 0

let has_aging u pid num =
  match (slot u pid num).value with
  | Some st -> List.exists (function Aging_malus -> true | _ -> false) st.attack_sources.sources
  | None -> false

let stunned u pid num = match (slot u pid num).value with Some st -> has_flag st stun_flag | None -> false

(* -------------------------------------------------------------------- zen *)

let zen_costs () =
  init_all_houses ();
  check_int "inertia costs 8" 8 (card_by_key "zen.inertia").cost;
  check_int "matter costs 8" 8 (card_by_key "zen.matter").cost

let inertia_spawns_matter () =
  let u = new_game ~mana0:10 ~special:Zen.house () in
  place u 0 "fire.goblin" 3;
  play u 0 "zen.inertia" ~input:(Some 2) ~cost:8;
  Alcotest.(check (option string)) "inertia" (Some "zen.inertia") (occupant u 0 2);
  Alcotest.(check (option string)) "matter to the right" (Some "zen.matter") (occupant u 0 4);
  check_int "matter life" 33 (life_at u 0 4);
  check_int "goblin stays" 16 (life_at u 0 3)

let inertia_no_matter_when_right_full () =
  let u = new_game ~mana0:10 ~special:Zen.house () in
  List.iter (fun n -> place u 0 "fire.goblin" n) [ 1; 2; 3; 4; 5 ];
  play u 0 "zen.inertia" ~input:(Some 0) ~cost:8;
  Alcotest.(check (option string)) "inertia" (Some "zen.inertia") (occupant u 0 0);
  check_bool "no matter" false
    (List.exists (fun n -> occupant u 0 n = Some "zen.matter") [ 0; 1; 2; 3; 4; 5 ])

let inertia_matter_partner () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 1;
  place u 0 "zen.matter" 2;
  place u 0 "fire.goblin" 4;
  place u 1 "earth.sprite" 1;
  place u 1 "fire.goblin" 3;
  step u (fun () ->
      Slot.inflict (slot u 0 4)
        (mk_damage ~is_ability:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "wounded goblin" 6 (life_at u 0 4);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "strongest enemy hit" 13 (life_at u 1 3);
  check_int "sprite untouched" 22 (life_at u 1 1);
  check_int "weakest ally healed" 9 (life_at u 0 4)

let inertia_heal_shows_the_life_travelling () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 1;
  place u 0 "zen.matter" 2;
  place u 0 "fire.goblin" 4;
  step u (fun () ->
      Slot.inflict (slot u 0 4)
        (mk_damage ~is_ability:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "heal announced" true
    (List.mem
       (Ev_ability { kind = "zen.heal"; player = 0; num = 1; target = Some (0, 4) })
       !events)

let fighter_grows_when_the_owner_is_hit () =
  let u = new_game ~special:Zen.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "zen.fighter" 2;
  step u (fun () ->
      Slot.inflict (slot u 0 2)
        (mk_damage ~is_ability:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "absorb 7 of 10" 23 (life_at u 0 2);
  step u (fun () ->
      Player.inflict (player u 0) (mk_damage 4 { ctx_player = 1; ctx_card = None; ctx_selected = 0 });
      Player.inflict (player u 0)
        (mk_damage ~is_spell:true 3 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "fighter attack up" 7 (attack_at u 0 2);
  check_int "player life" 33 (player_life u 0);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_int "bonus over" 5 (attack_at u 0 2)

let inertia_ignores_spells () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 2;
  step u (fun () ->
      Slot.inflict (slot u 0 2)
        (mk_damage ~is_spell:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "spell ignored" 22 (life_at u 0 2);
  step u (fun () ->
      Slot.inflict (slot u 0 2)
        (mk_damage ~is_ability:true 6 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "ability hits" 16 (life_at u 0 2)

let redlight_boosts_the_weaker_neighbour () =
  let u = new_game ~special:Zen.house () in
  place u 1 "fire.blargl" 1;
  place u 1 "fire.bull" 2;
  place u 0 "zen.redlight" 2;
  place u 0 "fire.goblin" 1;
  step u (fun () -> ());
  check_int "goblin boosted" 5 (attack_at u 0 1);
  check_int "redlight boosted" 3 (attack_at u 0 2)

let redlight_leaves_the_stronger_neighbour () =
  let u = new_game ~special:Zen.house () in
  place u 1 "fire.goblin" 1;
  place u 0 "zen.redlight" 2;
  place u 0 "fire.blargl" 1;
  step u (fun () -> ());
  check_int "blargl untouched" 8 (attack_at u 0 1)

let redlight_bonus_follows_the_board () =
  let u = new_game ~special:Zen.house () in
  place u 1 "fire.blargl" 1;
  place u 0 "zen.redlight" 2;
  place u 0 "fire.goblin" 1;
  step u (fun () -> ());
  check_int "goblin boosted" 5 (attack_at u 0 1);
  step u (fun () ->
      Slot.inflict (slot u 1 1) (mk_damage 30 { ctx_player = 0; ctx_card = None; ctx_selected = 0 }));
  step u (fun () -> ());
  check_int "goblin back to base" 4 (attack_at u 0 1)

let redlight_bonus_leaves_a_moved_neighbour () =
  let u = new_game ~special:Zen.house () in
  place u 1 "fire.blargl" 1;
  place u 0 "zen.redlight" 2;
  place u 0 "fire.goblin" 1;
  step u (fun () -> ());
  check_int "goblin boosted" 5 (attack_at u 0 1);
  step u (fun () -> Slots.move (player u 0).slots_update 1 4);
  step u (fun () -> ());
  check_int "goblin moved away" 4 (attack_at u 0 4)

let focus_reads_the_whole_board () =
  let u = new_game ~special:Zen.house () in
  (* the caster's board: two empty slots at the end *)
  place u 0 "fire.dragon" 0;
  place u 0 "earth.troll" 1;
  place u 0 "air.griffin" 2;
  place u 0 "earth.forestspider" 3;
  (* the opponent's board: five creatures, the two strongest pay the mana *)
  place u 1 "fire.dragon" 0;
  place u 1 "earth.troll" 1;
  place u 1 "air.griffin" 2;
  place u 1 "earth.forestspider" 3;
  place u 1 "fire.goblin" 4;
  let mana i = (player u 0).houses_update.hu_value.(i).mana in
  let fire = mana 0 and air = mana 2 and earth = mana 3 in
  let life = player_life u 0 in
  play u 0 "zen.focus" ~input:None ~cost:3;
  check_int "fire mana" (fire + 1) (mana 0);
  check_int "air mana" (air + 1) (mana 2);
  check_int "earth mana" (earth + 2) (mana 3);
  check_int "no life" life (player_life u 0)

let dreamer_pays_for_the_shock () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.dreamer" 0;
  place u 1 "fire.goblin" 2;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "goblin hit" 12 (life_at u 1 2);
  check_int "dreamer pays" 25 (life_at u 0 0);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "dreamer pays twice" 23 (life_at u 0 0)

(* ------------------------------------------------------------------- wind *)

let winged_grants_run () =
  let u = new_game ~special:Wind.house () in
  play u 0 "wind.WingedWarrior.name" ~input:(Some 1) ~cost:1;
  place ~run:false u 0 "fire.goblin" 3;
  check_bool "goblin ran" true
    (match (slot u 0 3).value with Some st -> has_flag st run_flag | None -> false)

let spirit_punishes_pass () =
  let u = new_game ~special:Wind.house () in
  place u 0 "wind.TheSpiritOfThunderstorm.name" 2;
  step u (fun () -> Player.submit (player u 0) None);
  check_int "player 3" 57 (player_life u 1)

let vortex_boosts_spells () =
  let u = new_game ~mana:[ 4; 4; 6; 4; 6 ] ~special:Wind.house () in
  (player u 1).houses_update.hu_value.(2) <- { mana = 5; growth = 1 };
  play u 0 "wind.EnergeticVortex.name" ~input:None ~cost:5;
  step u (fun () ->
      Player.inflict (player u 1)
        (mk_damage ~is_spell:true 10 { ctx_player = 0; ctx_card = None; ctx_selected = 0 }));
  check_int "ceil(10 * 1.5) = 15" 45 (player_life u 1)

(* ------------------------------------------------------------ chronomaster *)

let freeze_stuns () =
  let u = new_game ~special:Chronomaster.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "chronomaster.freeze" ~input:(Some 2) ~cost:1;
  check_bool "stunned" true (stunned u 1 2)

let sprites_unblockable () =
  let u = new_game ~special:Chronomaster.house () in
  (player u 1).pstate.ps_life <- 40;
  place u 0 "chronomaster.sprites" 2;
  place u 1 "fire.goblin" 2;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "hits the player" 35 (player_life u 1);
  check_int "goblin untouched" 16 (life_at u 1 2)

let accelerate_extra_swings () =
  let u = new_game ~special:Chronomaster.house () in
  (player u 1).pstate.ps_life <- 40;
  place u 0 "fire.goblin" 2;
  play u 0 "chronomaster.accelerate" ~input:(Some 2) ~cost:3;
  let extras = extra_sum u 0 2 in
  check_bool "1 or 2 extras" true (extras >= 1 && extras <= 2);
  step u (fun () -> Player.run_slots (player u 0));
  check_int "swings" (40 - (4 * (1 + extras))) (player_life u 1)

let wall_shields_player () =
  let u = new_game ~special:Chronomaster.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "chronomaster.wall" 2;
  place u 0 "fire.goblin" 3;
  step u (fun () ->
      Player.inflict (player u 0)
        (mk_damage ~is_spell:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "player shielded" 40 (player_life u 0);
  step u (fun () ->
      Slot.inflict (slot u 0 3) (mk_damage 4 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "goblin still hit" 12 (life_at u 0 3)

let aging_minus_three () =
  let u = new_game ~special:Chronomaster.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "chronomaster.aging" ~input:(Some 2) ~cost:5;
  check_int "attack 1" 1 (attack_at u 1 2);
  check_bool "malus" true (has_aging u 1 2)

let groundhog_rewinds () =
  let u = new_game ~mana:[ 10; 10; 10; 10; 10 ] ~special:Chronomaster.house () in
  (player u 0).pstate.ps_life <- 40;
  (player u 1).pstate.ps_life <- 40;
  step u (fun () -> Player.apply_effects (player u 0) On_start);
  place u 0 "fire.goblin" 2;
  place u 1 "earth.sprite" 2;
  step u (fun () -> Player.apply_effects (player u 1) On_turn);
  step u (fun () -> Slot.destroy (slot u 0 2));
  place u 1 "fire.goblin" 4;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "goblin gone" true (is_empty u 0 2);
  check_bool "opp goblin" true (not (is_empty u 1 4));
  (player u 0).pstate.ps_life <- 22;
  (player u 1).pstate.ps_life <- 18;
  play u 0 "chronomaster.groundhog" ~input:None ~cost:6;
  Alcotest.(check (option string)) "goblin restored" (Some "fire.goblin") (occupant u 0 2);
  Alcotest.(check (option string)) "sprite kept" (Some "earth.sprite") (occupant u 1 2);
  check_bool "late goblin undone" true (is_empty u 1 4);
  check_int "life kept" 22 (player_life u 0);
  check_int "opp life kept" 18 (player_life u 1);
  check_int "basics taxed" 9 (player u 0).houses_update.hu_value.(0).mana;
  check_int "special after cost and tax" 3 (player u 0).houses_update.hu_value.(4).mana;
  check_int "opp taxed" 9 (player u 1).houses_update.hu_value.(0).mana

let queen_extra () =
  let u = new_game ~mana0:10 ~special:Chronomaster.house () in
  place u 0 "fire.goblin" 1;
  play u 0 "chronomaster.queen" ~input:(Some 3) ~cost:7;
  check_int "goblin extra" 1 (extra_sum u 0 1)

let king_extra_turn () =
  let u = new_game ~mana0:10 ~special:Chronomaster.house () in
  play u 0 "chronomaster.king" ~input:(Some 2) ~cost:8;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "wait player" true
    (List.exists
       (function Wait_player (0, Some name) -> name = Chronomaster.king_phase | _ -> false)
       (player u 0).pstate.ps_transitions)

(* -------------------------------------------------------------- disciples *)

let prisoner_spawns () =
  let u = new_game ~special:Disciples.house () in
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_bool "a prisoner arrived" true
    (List.exists (fun n -> occupant u 0 n = Some "disciples.prisoner") [ 0; 1; 2; 3; 4; 5 ])

let prisoner_death_drains_highest_mana () =
  let u = new_game ~special:Disciples.house ~mana:[ 5; 4; 4; 3; 6 ] () in
  place u 0 "disciples.prisoner" 1;
  let mana () = Array.to_list (Array.map (fun h -> h.mana) (player u 0).houses_update.hu_value) in
  let before = mana () in
  step u (fun () -> Slot.destroy (slot u 0 1));
  let after = mana () in
  let dropped =
    List.fold_left2
      (fun n (i, a) (_, b) -> n + if i < 4 && a > b then 1 else 0)
      0
      (List.mapi (fun i x -> (i, x)) before)
      (List.mapi (fun i x -> (i, x)) after)
  in
  check_int "two basics lose 1" 2 dropped;
  check_int "special +1" (List.nth before 4 + 1) (List.nth after 4)

let liberator_enrages () =
  let u = new_game ~mana0:10 ~special:Disciples.house () in
  place u 0 "disciples.prisoner" 1;
  play u 0 "disciples.liberator" ~input:(Some 3) ~cost:6;
  Alcotest.(check (option string)) "enraged" (Some "disciples.enraged") (occupant u 0 1)

(* Preacher and False Prophet both add incr_basic_cost_mod; losing one must
   leave the other's copy in place (State.scala:112 removes one occurrence) *)
let shared_cost_mod_survives () =
  let u = new_game ~mana:[ 6; 4; 4; 4; 5 ] ~special:Disciples.house () in
  let copies () =
    List.length (List.filter (( == ) Common.incr_basic_cost_mod) (player u 0).pstate.ps_desc.desc_mods)
  in
  place u 0 "disciples.preacher" 1;
  play u 0 "disciples.prophet" ~input:(Some 3) ~cost:3;
  check_int "both sources" 2 (copies ());
  step u (fun () -> Slot.destroy (slot u 0 1));
  check_int "prophet's copy kept" 1 (copies ())

(* ------------------------------------------------------------------ rng *)

(* Card randomness draws from the updater's rng, so the global Random (which
   bot search also uses) cannot shift a seeded game *)
let cocoons_follow_updater_rng () =
  let draws ~noise =
    let u = new_game ~special:Zen.house () in
    place u 0 "zen.mimic" 0;
    List.init 8 (fun _ ->
        for _ = 1 to noise do
          ignore (Random.bits ())
        done;
        step u (fun () -> Player.apply_effects (player u 0) On_turn);
        match (player u 0).pstate.ps_data with Zen_cocoons xs -> xs | _ -> [])
  in
  Alcotest.(check (list (list int))) "same cocoons" (draws ~noise:0) (draws ~noise:7)

let () =
  let open Alcotest in
  run "houses"
    [
      ( "water",
        [
          test_case "golem ignores abilities" `Quick golem_ignores_ability;
          test_case "golem takes combat" `Quick golem_takes_combat;
          test_case "turtle absorb" `Quick turtle_absorbs_five;
          test_case "iceguard" `Quick iceguard_halves_player_damage;
          test_case "elder growth" `Quick elder_air_growth;
          test_case "apostate" `Quick apostate_adds_fire;
          test_case "overlord run" `Quick overlord_gives_run;
        ] );
      ( "air",
        [
          test_case "griffin fires" `Quick griffin_fires_when_air_ge_5;
          test_case "griffin skips" `Quick griffin_skips_when_air_lt_5;
          test_case "phoenix reborn" `Quick phoenix_reborn;
          test_case "phoenix stays dead" `Quick phoenix_stays_dead_without_fire;
          test_case "on_dead_hook override" `Quick dead_hook_override_suppresses_on_my_death;
          test_case "twister" `Quick twister_destroys;
          test_case "cloud multi" `Quick cloud_is_multi;
          test_case "titan" `Quick titan_hits_opposite;
          test_case "lightning desc" `Quick lightning_desc_fills_air_damage;
        ] );
      ( "earth",
        [
          test_case "hermit growth" `Quick hermit_earth_growth;
          test_case "fury" `Quick fury_sums_top_two;
          test_case "fury before payment" `Quick fury_counts_elemental_before_payment;
          test_case "spider" `Quick spider_fills_adjacent;
          test_case "spider blocked slot" `Quick spider_skips_blocked_slot;
          test_case "care" `Quick care_heals_player_and_creature;
        ] );
      ( "troopers",
        [
          test_case "marine" `Quick marine_pokes_summons;
          test_case "goliath spell" `Quick goliath_ignores_spell;
          test_case "goliath absorb" `Quick goliath_absorbs_combat;
          test_case "siege" `Quick siege_hits_fattest;
          test_case "conscription" `Quick conscription_special_mana;
          test_case "special deck" `Quick special_deck_has_one_card_per_quarter;
        ] );
      ( "zen",
        [
          test_case "costs" `Quick zen_costs;
          test_case "inertia spawns matter" `Quick inertia_spawns_matter;
          test_case "inertia no matter" `Quick inertia_no_matter_when_right_full;
          test_case "partner heal strike" `Quick inertia_matter_partner;
          test_case "inertia heal flow" `Quick inertia_heal_shows_the_life_travelling;
          test_case "fighter owner hits" `Quick fighter_grows_when_the_owner_is_hit;
          test_case "inertia ignores spells" `Quick inertia_ignores_spells;
          test_case "redlight boosts neighbour" `Quick redlight_boosts_the_weaker_neighbour;
          test_case "redlight spares stronger" `Quick redlight_leaves_the_stronger_neighbour;
          test_case "redlight follows the board" `Quick redlight_bonus_follows_the_board;
          test_case "redlight loses a moved neighbour" `Quick redlight_bonus_leaves_a_moved_neighbour;
          test_case "focus reads the whole board" `Quick focus_reads_the_whole_board;
          test_case "dreamer pays for the shock" `Quick dreamer_pays_for_the_shock;
        ] );
      ( "wind",
        [
          test_case "winged run" `Quick winged_grants_run;
          test_case "spirit pass" `Quick spirit_punishes_pass;
          test_case "vortex spells" `Quick vortex_boosts_spells;
        ] );
      ( "chronomaster",
        [
          test_case "freeze" `Quick freeze_stuns;
          test_case "sprites" `Quick sprites_unblockable;
          test_case "accelerate" `Quick accelerate_extra_swings;
          test_case "wall" `Quick wall_shields_player;
          test_case "aging" `Quick aging_minus_three;
          test_case "groundhog" `Quick groundhog_rewinds;
          test_case "queen" `Quick queen_extra;
          test_case "king" `Quick king_extra_turn;
        ] );
      ( "disciples",
        [
          test_case "prisoner spawn" `Quick prisoner_spawns;
          test_case "prisoner death mana" `Quick prisoner_death_drains_highest_mana;
          test_case "liberator" `Quick liberator_enrages;
          test_case "shared cost mod" `Quick shared_cost_mod_survives;
        ] );
      ("rng", [ test_case "cocoons use updater rng" `Quick cocoons_follow_updater_rng ]);
    ]
