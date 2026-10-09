(* Mechanics for Water, Air, Earth and Troopers, same style as test/mechanics.ml. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

(* ------------------------------------------------------------------ water *)

let blizzard_ignores_ability () =
  let u = new_game () in
  place u 1 "sadness.blizzard" 0;
  place u 0 "anger.hotspot" 0;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "blizzard ignores ability damage" 12 (life_at u 1 0)

let blizzard_takes_combat () =
  let u = new_game () in
  place u 1 "sadness.blizzard" 0;
  place u 0 "anger.dragon" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "blizzard 12 - 9 combat" 3 (life_at u 1 0)

let shellycoat_absorbs_five () =
  let u = new_game () in
  place u 1 "sadness.shellycoat" 0;
  place u 0 "anger.werewolf" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "4 attack - 5 absorb" 16 (life_at u 1 0)

let stag_halves_player_damage () =
  let u = new_game () in
  place u 1 "sadness.stag" 1;
  let d = mk_damage ~is_spell:true 9 { ctx_player = 0; ctx_card = None; ctx_selected = 0 } in
  step u (fun () -> Player.inflict (player u 1) d);
  check_int "ceil(9/2) = 5" (60 - 5) (player_life u 1)

let greywolf_air_growth () =
  let u = new_game () in
  let growth () = (player u 0).houses_update.hu_value.(2).growth in
  let before = growth () in
  place u 0 "sadness.greywolf" 0;
  check_int "air growth +1" (before + 1) (growth ())

let victim_adds_fire () =
  let u = new_game ~mana0:3 () in
  play u 0 "sadness.victim" ~input:(Some 0) ~cost:3;
  check_int "fire mana 3 + 2" 5 (player u 0).houses_update.hu_value.(0).mana

let banshee_gives_run () =
  let u = new_game () in
  place ~run:false u 0 "sadness.banshee" 1;
  place ~run:false u 0 "anger.werewolf" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "adjacent werewolf attacks the turn it lands" (60 - 4) (player_life u 1)

(* -------------------------------------------------------------------- air *)

let harpy_fires_when_air_ge_5 () =
  let u = new_game ~mana:[ 4; 4; 6; 4 ] () in
  play u 0 "fear.harpy" ~input:(Some 0) ~cost:2;
  check_int "pre-spend air 6 >= 5" (60 - 5) (player_life u 1)

let harpy_skips_when_air_lt_5 () =
  let u = new_game ~mana:[ 4; 4; 4; 4 ] () in
  play u 0 "fear.harpy" ~input:(Some 0) ~cost:2;
  check_int "pre-spend air 4 < 5" 60 (player_life u 1)

let mimic_reborn () =
  let u = new_game ~mana0:10 () in
  place u 0 "fear.mimic" 0;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 18 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  Alcotest.(check (option string)) "mimic returns" (Some "fear.mimic") (occupant u 0 0)

let mimic_stays_dead_without_fire () =
  let u = new_game ~mana0:9 () in
  place u 0 "fear.mimic" 0;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 18 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_bool "mana 9 is not enough" true (is_empty u 0 0)

let dead_hook_override_suppresses_on_my_death () =
  let u = new_game ~mana0:10 () in
  place u 0 "fear.mimic" 0;
  (* SlotsUpdate.onDead is a var: a house that replaces it decides whether
     onMyDeath runs, so the mimic must not come back here *)
  (player u 0).slots_update.on_dead_hook <- ignore;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 18 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_bool "no rebirth" true (is_empty u 0 0)

let fear_destroys () =
  let u = new_game ~mana:[ 4; 4; 10; 4 ] () in
  place u 1 "anger.dragon" 2;
  play u 0 "fear.fear" ~input:(Some 2) ~cost:10;
  check_bool "dragon is destroyed" true (is_empty u 1 2)

let cloud_is_multi () =
  let u = new_game () in
  place u 0 "fear.clouds" 0;
  place u 1 "anger.werewolf" 2;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "player takes 4" (60 - 4) (player_life u 1);
  check_int "off-lane werewolf also takes 4" 12 (life_at u 1 2)

let malediction_hits_opposite () =
  let u = new_game ~mana:[ 4; 4; 12; 4 ] () in
  place u 1 "anger.dragon" 1;
  play u 0 "fear.malediction" ~input:(Some 1) ~cost:12;
  check_int "dragon 41 - 15" 26 (life_at u 1 1)

let spirit_desc_fills_air_damage () =
  let u = new_game ~mana:[ 4; 4; 7; 4 ] () in
  let c = card_by_key "fear.spirit" in
  match c.desc_args with
  | None -> Alcotest.fail "spirit should compute [{0}] damage"
  | Some f ->
      Alcotest.(check (list string)) "5 + 7 air" [ "12" ] (f u.state 0);
      let text = Assets.describe ~state:u.state ~player:0 c in
      check_bool "placeholder filled" true (not (String.contains text '{'))

(* ------------------------------------------------------------------ earth *)

let kitsune_earth_growth () =
  let u = new_game () in
  let growth () = (player u 0).houses_update.hu_value.(3).growth in
  let before = growth () in
  place u 0 "joy.kitsune" 0;
  check_int "earth growth +2" (before + 2) (growth ())

let fusion_sums_top_two () =
  let u = new_game ~mana:[ 4; 4; 4; 6 ] () in
  (* not adjacent, so no orc/bull bonus; no SpellMod on the board *)
  place u 0 "anger.sentinel" 0;
  place u 0 "anger.oni" 3;
  (* Fury reads the attacks published by the previous step *)
  step u ignore;
  play u 0 "joy.fusion" ~input:None ~cost:6;
  check_int "3 + 4" (60 - 7) (player_life u 1)

let fusion_counts_elemental_before_payment () =
  let u = new_game ~mana:[ 4; 4; 4; 10 ] () in
  place u 0 "joy.sunstonegolem" 0;
  place u 0 "anger.sentinel" 3;
  step u ignore;
  play u 0 "joy.fusion" ~input:None ~cost:6;
  (* elemental attack = earth mana before paying (10), not after (4) *)
  check_int "10 + 3" (60 - 13) (player_life u 1)

let nymphea_fills_adjacent () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] () in
  play u 0 "joy.nymphea" ~input:(Some 2) ~cost:7;
  Alcotest.(check (option string)) "left spider" (Some "joy.nymph") (occupant u 0 1);
  Alcotest.(check (option string)) "right spider" (Some "joy.nymph") (occupant u 0 3)

let nymphea_skips_blocked_slot () =
  let u = new_game ~mana:[ 4; 4; 4; 7 ] () in
  Player.block_slot (player u 0) 1;
  play u 0 "joy.nymphea" ~input:(Some 2) ~cost:7;
  check_bool "blocked slot stays empty" true (is_empty u 0 1);
  Alcotest.(check (option string)) "right spider" (Some "joy.nymph") (occupant u 0 3)

let care_heals_player_and_creature () =
  let u = new_game ~mana:[ 4; 4; 4; 2 ] () in
  place u 0 "anger.werewolf" 0;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 8 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  step u (fun () ->
      Player.inflict (player u 0)
        (mk_damage ~is_spell:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  play u 0 "joy.care" ~input:(Some 0) ~cost:2;
  check_int "player +8" (50 + 8) (player_life u 0);
  check_int "werewolf 8 + 8" 16 (life_at u 0 0)

(* -------------------------------------------------------------- troopers *)

let marine_pokes_summons () =
  let u = new_game () in
  place u 0 "Marine" 0;
  place u 1 "anger.werewolf" 2;
  check_int "werewolf 16 - 4" 12 (life_at u 1 2)

let goliath_ignores_spell () =
  let u = new_game () in
  place u 1 "Goliath" 0;
  play u 0 "anger.wave" ~input:None ~cost:6;
  check_int "immune to spell" 20 (life_at u 1 0)

let goliath_absorbs_combat () =
  let u = new_game () in
  place u 1 "Goliath" 0;
  place u 0 "anger.werewolf" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "4 - 1 absorb" 17 (life_at u 1 0)

let siege_hits_fattest () =
  let u = new_game () in
  place u 0 "SiegeTank" 0;
  place u 1 "anger.werewolf" 1;
  place u 1 "anger.dragon" 3;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "dragon 41 - 8" 33 (life_at u 1 3);
  check_int "werewolf untouched" 16 (life_at u 1 1)

let conscription_special_mana () =
  let u = new_game () in
  play u 0 "Conscription" ~input:None ~cost:0;
  check_int "special mana 2 -> 3" 3 (player u 0).houses_update.hu_value.(4).mana

let special_deck_has_one_card_per_quarter () =
  init_all_houses ();
  let d = Game.make_player_desc ~special:Troopers.house Anger.house in
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
  place u 0 "anger.werewolf" 3;
  play u 0 "zen.inertia" ~input:(Some 2) ~cost:8;
  Alcotest.(check (option string)) "inertia" (Some "zen.inertia") (occupant u 0 2);
  Alcotest.(check (option string)) "matter to the right" (Some "zen.matter") (occupant u 0 4);
  check_int "matter life" 30 (life_at u 0 4);
  check_int "werewolf stays" 16 (life_at u 0 3)

let inertia_no_matter_when_right_full () =
  let u = new_game ~mana0:10 ~special:Zen.house () in
  List.iter (fun n -> place u 0 "anger.werewolf" n) [ 1; 2; 3; 4; 5 ];
  play u 0 "zen.inertia" ~input:(Some 0) ~cost:8;
  Alcotest.(check (option string)) "inertia" (Some "zen.inertia") (occupant u 0 0);
  check_bool "no matter" false
    (List.exists (fun n -> occupant u 0 n = Some "zen.matter") [ 0; 1; 2; 3; 4; 5 ])

let inertia_matter_partner () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 1;
  place u 0 "zen.matter" 2;
  place u 0 "anger.werewolf" 4;
  place u 1 "joy.cat" 1;
  place u 1 "anger.werewolf" 3;
  step u (fun () ->
      Slot.inflict (slot u 0 4)
        (mk_damage ~is_ability:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "wounded werewolf" 6 (life_at u 0 4);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "strongest enemy hit" 13 (life_at u 1 3);
  check_int "cat untouched" 22 (life_at u 1 1);
  check_int "weakest ally healed" 9 (life_at u 0 4)

let matter_strike_scales_with_empty_slots () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 1;
  place u 0 "zen.matter" 2;
  place u 0 "anger.werewolf" 4;
  place u 0 "anger.werewolf" 5;
  place u 1 "joy.cat" 1;
  place u 1 "anger.werewolf" 3;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  (* two empty slots left, so the strike deals 2, not 3 *)
  check_int "strongest enemy hit by the empty slots" 14 (life_at u 1 3);
  check_int "cat untouched" 22 (life_at u 1 1)

let matter_strike_skips_on_a_full_board () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 1;
  place u 0 "zen.matter" 2;
  place u 0 "anger.werewolf" 0;
  place u 0 "anger.werewolf" 3;
  place u 0 "anger.werewolf" 4;
  place u 0 "anger.werewolf" 5;
  place u 1 "anger.werewolf" 3;
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "no strike on a full board" 16 (life_at u 1 3);
  check_bool "no strike announced" false
    (List.exists (function Ev_ability { kind = "zen.spark"; _ } -> true | _ -> false) !events)

let inertia_heal_shows_the_life_travelling () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 1;
  place u 0 "zen.matter" 2;
  place u 0 "anger.werewolf" 4;
  step u (fun () ->
      Slot.inflict (slot u 0 4)
        (mk_damage ~is_ability:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "heal announced" true
    (List.mem
       (Ev_ability { kind = "zen.heal.3"; player = 0; num = 1; target = Some (0, 4) })
       !events)

let inertia_heals_its_owner () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 1;
  place u 0 "zen.matter" 2;
  step u (fun () ->
      Player.inflict (player u 0)
        (mk_damage 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "owner wounded" 50 (player_life u 0);
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  (* four empty slots left, so the owner is healed by 4 *)
  check_int "owner healed" 54 (player_life u 0);
  check_bool "owner heal announced" true
    (List.exists
       (function
         | Ev_ability { kind; player = 0; num = 1; _ }
           when String.starts_with ~prefix:"zen.inertia.life" kind ->
             true
         | _ -> false)
       !events)

let inertia_does_not_heal_on_a_full_board () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 1;
  place u 0 "zen.matter" 2;
  place u 0 "anger.werewolf" 0;
  place u 0 "anger.werewolf" 3;
  place u 0 "anger.werewolf" 4;
  place u 0 "anger.werewolf" 5;
  step u (fun () ->
      Slot.inflict (slot u 0 0)
        (mk_damage ~is_ability:true 3 { ctx_player = 1; ctx_card = None; ctx_selected = 0 });
      Player.inflict (player u 0)
        (mk_damage 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  let before = life_at u 0 0 in
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "no heal on a full board" before (life_at u 0 0);
  check_int "owner not healed on a full board" 50 (player_life u 0);
  check_bool "no heal announced" false
    (List.exists
       (function
         | Ev_ability { kind; _ } when String.starts_with ~prefix:"zen.heal" kind -> true
         | _ -> false)
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

let inertia_ignores_spells_with_matter () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 2;
  place u 0 "zen.matter" 4;
  step u (fun () ->
      Slot.inflict (slot u 0 2)
        (mk_damage ~is_spell:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "spell ignored while matter lives" 18 (life_at u 0 2);
  step u (fun () ->
      Slot.inflict (slot u 0 2)
        (mk_damage ~is_ability:true 6 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "ability hits" 12 (life_at u 0 2)

let inertia_spell_hits_without_matter () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.inertia" 2;
  step u (fun () ->
      Slot.inflict (slot u 0 2)
        (mk_damage ~is_spell:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "spell lands without matter" 8 (life_at u 0 2)

let redlight_charged u pid n =
  match (slot u pid n).value with
  | Some st -> List.exists (fun (m : blacksmith_mark) -> m.bm_key = "zen-redlight") st.marks
  | None -> false

let redlight_strikes_opponent_strongest () =
  let u = new_game ~mana0:10 ~special:Zen.house () in
  (* werewolf costs 1 and has more life (16), monk costs 3 and has less (13):
     "strongest" here means highest cost, so the monk is the one struck *)
  place u 1 "anger.werewolf" 1;
  place u 1 "anger.sentinel" 3;
  play u 0 "zen.redlight" ~input:(Some 0) ~cost:8;
  check_bool "charge is on the card" true (redlight_charged u 0 0);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "highest-cost monk struck by 6" 7 (life_at u 1 3);
  check_int "cheaper werewolf untouched" 16 (life_at u 1 1);
  check_bool "charge spent" false (redlight_charged u 0 0)

let redlight_strikes_leftmost_on_a_tie () =
  let u = new_game ~mana0:10 ~special:Zen.house () in
  place u 1 "anger.werewolf" 2;
  place u 1 "anger.werewolf" 4;
  play u 0 "zen.redlight" ~input:(Some 0) ~cost:8;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "leftmost struck" 10 (life_at u 1 2);
  check_int "right one untouched" 16 (life_at u 1 4)

let redlight_shows_the_arc_and_fires_once () =
  let u = new_game ~mana0:10 ~special:Zen.house () in
  place u 1 "anger.werewolf" 1;
  play u 0 "zen.redlight" ~input:(Some 0) ~cost:8;
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "werewolf struck" 10 (life_at u 1 1);
  check_bool "red arc announced" true
    (List.mem
       (Ev_ability { kind = "zen.redlight"; player = 0; num = 0; target = Some (1, 1) })
       !events);
  (* the charge is spent: a later turn must not strike again *)
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "no second strike" 10 (life_at u 1 1)

let redlight_holds_charge_without_a_target () =
  let u = new_game ~mana0:10 ~special:Zen.house () in
  play u 0 "zen.redlight" ~input:(Some 0) ~cost:8;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "charge kept with no target" true (redlight_charged u 0 0);
  (* a target appears; the next turn the light is finally thrown *)
  place u 1 "anger.werewolf" 1;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "werewolf struck" 10 (life_at u 1 1);
  check_bool "charge now spent" false (redlight_charged u 0 0)

(* The Electric Guard hits back the creature that wounded its owner, and the
   board shows the arc of electricity between them. *)
let eguard_zaps_the_attacker () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.guard" 2;
  place u 1 "anger.werewolf" 2;
  let life = life_at u 1 2 in
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  step u (fun () ->
      Player.inflict (player u 0) (mk_damage 4 { ctx_player = 1; ctx_card = None; ctx_selected = 2 }));
  check_bool "the zap is announced" true
    (List.mem
       (Ev_ability { kind = "zen.guard.zap"; player = 0; num = 2; target = Some (1, 2) })
       !events);
  check_int "the attacker is hit" (life - 3) (life_at u 1 2);
  check_int "the owner still took the blow" 56 (player_life u 0)

(* A spell that hits the owner carries the slot it targeted, which belongs to
   the owner, so the guard must not mistake it for the attacking creature. *)
let eguard_ignores_spells_hitting_the_owner () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.guard" 2;
  place u 0 "anger.werewolf" 3;
  place u 1 "joy.faery" 3;
  let life = life_at u 1 3 in
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  step u (fun () ->
      Player.inflict (player u 0)
        (mk_damage ~is_spell:true 6 { ctx_player = 1; ctx_card = None; ctx_selected = 3 }));
  check_bool "no zap for a spell" false
    (List.exists
       (function Ev_ability { kind = "zen.guard.zap"; _ } -> true | _ -> false)
       !events);
  check_int "the spellcaster's creature is untouched" life (life_at u 1 3);
  check_int "the owner still took the spell" 54 (player_life u 0)

let focus_reads_the_whole_board () =
  let u = new_game ~special:Zen.house () in
  (* the caster's board: two empty slots at the end *)
  place u 0 "anger.dragon" 0;
  place u 0 "joy.faune" 1;
  place u 0 "fear.harpy" 2;
  place u 0 "joy.nymph" 3;
  (* the opponent's board: five creatures, the two strongest pay the mana *)
  place u 1 "anger.dragon" 0;
  place u 1 "joy.faune" 1;
  place u 1 "fear.harpy" 2;
  place u 1 "joy.nymph" 3;
  place u 1 "anger.werewolf" 4;
  let mana i = (player u 0).houses_update.hu_value.(i).mana in
  let anger = mana 0 and fear = mana 2 and joy = mana 3 in
  let life = player_life u 0 in
  play u 0 "zen.focus" ~input:None ~cost:3;
  check_int "anger mana" (anger + 1) (mana 0);
  check_int "fear mana" (fear + 1) (mana 2);
  check_int "joy mana" (joy + 2) (mana 3);
  check_int "no life" life (player_life u 0)

(* Focus takes the mana from the creatures themselves: the board draws an arc
   from each of them to the house it pays, and a lens closes over the creature
   it reads. *)
let focus_shows_the_mana_arcs () =
  let u = new_game ~special:Zen.house () in
  (* the caster's two weakest, the opponent's two strongest *)
  place u 0 "anger.werewolf" 0;
  place u 0 "anger.werewolf" 1;
  place u 1 "joy.faune" 0;
  place u 1 "joy.faune" 1;
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  play u 0 "zen.focus" ~input:None ~cost:3;
  let arcs =
    List.filter_map
      (function Ev_mana { player; num; target; deltas } -> Some (player, num, target, deltas) | _ -> None)
      !events
  in
  check_int "an arc per creature" 4 (List.length arcs);
  check_bool "the werewolf pays its house" true (List.mem (0, 0, 0, [ (0, 1) ]) arcs);
  check_bool "the troll pays its house" true (List.mem (1, 0, 0, [ (3, 1) ]) arcs);
  let lenses =
    List.filter_map
      (function Ev_ability { kind = "zen.focus"; player; num; _ } -> Some (player, num) | _ -> None)
      !events
  in
  check_int "a lens per creature" 4 (List.length lenses);
  check_bool "the lens is on the werewolf" true (List.mem (0, 0) lenses);
  check_bool "the lens is on the troll" true (List.mem (1, 0) lenses)

(* High Priest's Babi answers a house raise by wounding its opposite.  Focus
   reads its weakest creatures first and raises a house per creature, so the
   first one's mana can kill a creature still queued to pay: Focus must skip
   the now-empty slot instead of reading it (regression: "empty slot 2"). *)
let focus_skips_a_slot_a_mana_reaction_killed () =
  let u = new_duel ~special0:Highpriest.house ~special1:Zen.house () in
  place u 0 "highpriest.Babi" 2;
  place u 1 "zen.guard" 0;
  place u 1 "zen.dreamer" 2;
  (* one point of Babi's wound is lethal to the creature it faces *)
  Slot.update (slot u 1 2) (fun st -> { st with life = 1 });
  play u 1 "zen.focus" ~input:None ~cost:5;
  check_bool "the opposite creature died" true (is_empty u 1 2);
  Alcotest.(check (option string)) "the weakest paid and lived" (Some "zen.guard") (occupant u 1 0)

let dreamer_pays_for_the_shock () =
  let u = new_game ~special:Zen.house () in
  place u 0 "zen.dreamer" 0;
  place u 1 "anger.werewolf" 2;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "werewolf hit" 12 (life_at u 1 2);
  check_int "dreamer pays" 25 (life_at u 0 0);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "dreamer pays twice" 23 (life_at u 0 0)

(* ------------------------------------------------------------------- wind *)

let winged_grants_run () =
  let u = new_game ~special:Wind.house () in
  play u 0 "wind.WingedWarrior.name" ~input:(Some 1) ~cost:1;
  place ~run:false u 0 "anger.werewolf" 3;
  check_bool "werewolf ran" true
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
  place u 1 "anger.werewolf" 2;
  play u 0 "chronomaster.freeze" ~input:(Some 2) ~cost:1;
  check_bool "stunned" true (stunned u 1 2)

let sprites_unblockable () =
  let u = new_game ~special:Chronomaster.house () in
  (player u 1).pstate.ps_life <- 40;
  place u 0 "chronomaster.sprites" 2;
  place u 1 "anger.werewolf" 2;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "hits the player" 35 (player_life u 1);
  check_int "werewolf untouched" 16 (life_at u 1 2)

let accelerate_extra_swings () =
  let u = new_game ~special:Chronomaster.house () in
  (player u 1).pstate.ps_life <- 40;
  place u 0 "anger.werewolf" 2;
  play u 0 "chronomaster.accelerate" ~input:(Some 2) ~cost:3;
  let extras = extra_sum u 0 2 in
  check_bool "1 or 2 extras" true (extras >= 1 && extras <= 2);
  step u (fun () -> Player.run_slots (player u 0));
  check_int "swings" (40 - (4 * (1 + extras))) (player_life u 1)

let wall_shields_player () =
  let u = new_game ~special:Chronomaster.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "chronomaster.wall" 2;
  place u 0 "anger.werewolf" 3;
  step u (fun () ->
      Player.inflict (player u 0)
        (mk_damage ~is_spell:true 10 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "player shielded" 40 (player_life u 0);
  step u (fun () ->
      Slot.inflict (slot u 0 3) (mk_damage 4 { ctx_player = 1; ctx_card = None; ctx_selected = 0 }));
  check_int "werewolf still hit" 12 (life_at u 0 3)

let aging_minus_three () =
  let u = new_game ~special:Chronomaster.house () in
  place u 1 "anger.werewolf" 2;
  play u 0 "chronomaster.aging" ~input:(Some 2) ~cost:5;
  check_int "attack 1" 1 (attack_at u 1 2);
  check_bool "malus" true (has_aging u 1 2)

let groundhog_rewinds () =
  let u = new_game ~mana:[ 10; 10; 10; 10; 10 ] ~special:Chronomaster.house () in
  (player u 0).pstate.ps_life <- 40;
  (player u 1).pstate.ps_life <- 40;
  step u (fun () -> Player.apply_effects (player u 0) On_start);
  place u 0 "anger.werewolf" 2;
  place u 1 "joy.faery" 2;
  step u (fun () -> Player.apply_effects (player u 1) On_turn);
  step u (fun () -> Slot.destroy (slot u 0 2));
  place u 1 "anger.werewolf" 4;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "werewolf gone" true (is_empty u 0 2);
  check_bool "opp werewolf" true (not (is_empty u 1 4));
  (player u 0).pstate.ps_life <- 22;
  (player u 1).pstate.ps_life <- 18;
  play u 0 "chronomaster.groundhog" ~input:None ~cost:6;
  Alcotest.(check (option string)) "werewolf restored" (Some "anger.werewolf") (occupant u 0 2);
  Alcotest.(check (option string)) "sprite kept" (Some "joy.faery") (occupant u 1 2);
  check_bool "late werewolf undone" true (is_empty u 1 4);
  check_int "life kept" 22 (player_life u 0);
  check_int "opp life kept" 18 (player_life u 1);
  check_int "basics taxed" 9 (player u 0).houses_update.hu_value.(0).mana;
  check_int "special after cost and tax" 3 (player u 0).houses_update.hu_value.(4).mana;
  check_int "opp taxed" 9 (player u 1).houses_update.hu_value.(0).mana

let queen_extra () =
  let u = new_game ~mana0:10 ~special:Chronomaster.house () in
  place u 0 "anger.werewolf" 1;
  play u 0 "chronomaster.queen" ~input:(Some 3) ~cost:7;
  check_int "werewolf extra" 1 (extra_sum u 0 1)

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

(* The Dark Monk taxes the opponent's Fire cards while it lives *)
let dark_monk_taxes_the_opponents_fire () =
  let u = new_game ~special:Disciples.house () in
  let tax () = malus_amount (player u 1).pstate Common.fire_cost_malus in
  place u 0 "disciples.monk" 1;
  check_int "one point of fire tax" 1 (tax ());
  step u (fun () -> Slot.destroy (slot u 0 1));
  check_int "the tax is gone with the monk" 0 (tax ())

(* The Falconer sends its bird over the foe row: the board flies it from the
   event, so the card has to announce one *)
let falconer_sends_its_bird () =
  let u = new_game ~special:Disciples.house () in
  place u 0 "disciples.falconer" 2;
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_bool "the bird is announced" true
    (List.mem
       (Ev_ability { kind = "disciples.falconer"; player = 0; num = 2; target = None })
       !events)

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
      ( "sadness",
        [
          test_case "blizzard ignores abilities" `Quick blizzard_ignores_ability;
          test_case "blizzard takes combat" `Quick blizzard_takes_combat;
          test_case "shelly coat absorb" `Quick shellycoat_absorbs_five;
          test_case "stag" `Quick stag_halves_player_damage;
          test_case "greywolf growth" `Quick greywolf_air_growth;
          test_case "victim" `Quick victim_adds_fire;
          test_case "banshee run" `Quick banshee_gives_run;
        ] );
      ( "fear",
        [
          test_case "harpy fires" `Quick harpy_fires_when_air_ge_5;
          test_case "harpy skips" `Quick harpy_skips_when_air_lt_5;
          test_case "mimic reborn" `Quick mimic_reborn;
          test_case "mimic stays dead" `Quick mimic_stays_dead_without_fire;
          test_case "on_dead_hook override" `Quick dead_hook_override_suppresses_on_my_death;
          test_case "fear" `Quick fear_destroys;
          test_case "clouds multi" `Quick cloud_is_multi;
          test_case "malediction" `Quick malediction_hits_opposite;
          test_case "spirit desc" `Quick spirit_desc_fills_air_damage;
        ] );
      ( "joy",
        [
          test_case "kitsune growth" `Quick kitsune_earth_growth;
          test_case "fusion" `Quick fusion_sums_top_two;
          test_case "fusion before payment" `Quick fusion_counts_elemental_before_payment;
          test_case "nymphea" `Quick nymphea_fills_adjacent;
          test_case "numphea blocked slot" `Quick nymphea_skips_blocked_slot;
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
          test_case "matter scales with empty slots" `Quick matter_strike_scales_with_empty_slots;
          test_case "matter skips strike on a full board" `Quick matter_strike_skips_on_a_full_board;
          test_case "inertia heal flow" `Quick inertia_heal_shows_the_life_travelling;
          test_case "inertia heals its owner" `Quick inertia_heals_its_owner;
          test_case "inertia skips heal on a full board" `Quick inertia_does_not_heal_on_a_full_board;
          test_case "fighter owner hits" `Quick fighter_grows_when_the_owner_is_hit;
          test_case "inertia ignores spells with matter" `Quick inertia_ignores_spells_with_matter;
          test_case "inertia spell hits without matter" `Quick inertia_spell_hits_without_matter;
          test_case "redlight strikes opponent strongest" `Quick redlight_strikes_opponent_strongest;
          test_case "redlight strikes leftmost on a tie" `Quick redlight_strikes_leftmost_on_a_tie;
          test_case "redlight shows arc and fires once" `Quick redlight_shows_the_arc_and_fires_once;
          test_case "redlight holds charge without a target" `Quick redlight_holds_charge_without_a_target;
          test_case "eguard zaps the attacker" `Quick eguard_zaps_the_attacker;
          test_case "eguard ignores spells" `Quick eguard_ignores_spells_hitting_the_owner;
          test_case "focus reads the whole board" `Quick focus_reads_the_whole_board;
          test_case "focus shows the mana arcs" `Quick focus_shows_the_mana_arcs;
          test_case "focus skips a killed payer" `Quick focus_skips_a_slot_a_mana_reaction_killed;
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
          test_case "dark monk tax" `Quick dark_monk_taxes_the_opponents_fire;
          test_case "falconer sends its bird" `Quick falconer_sends_its_bird;
        ] );
      ("rng", [ test_case "cocoons use updater rng" `Quick cocoons_follow_updater_rng ]);
    ]
