(* Focused checks on the Fire mechanics that exercise the tricky parts of the
   port: reaction records, attack bonuses, mana-growth hooks, spell targeting,
   mass damage and the SpellMod multiplier.

   Each case builds a real game state and drives it through the engine, so
   this tests the update graph, not just pure functions. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

(* ------------------------------------------------------------------ combat *)

let basic_combat () =
  let u = new_game () in
  place u 0 "fire.goblin" 2;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "goblin hits the player through an empty slot" (60 - 4) (player_life u 1)

let wall_zero_attack () =
  let u = new_game () in
  place u 0 "fire.wall" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "wall (attack 0) does not swing" 60 (player_life u 1)

let kill_and_death () =
  let u = new_game () in
  (* a 9-attack dragon kills the 5-life wall opposite *)
  place u 1 "fire.wall" 0;
  place u 0 "fire.dragon" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_bool "wall dies" true (is_empty u 1 0);
  (* SingleTargetAttack: the slot took the hit, so nothing spills over *)
  check_int "no spill-over damage to the player" 60 (player_life u 1);
  check_bool "death was recorded" true ((player u 1).stats.nb_dead > 0)

let no_swing_without_run_flag () =
  let u = new_game () in
  place ~run:false u 0 "fire.goblin" 1;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "no run flag -> no swing" 60 (player_life u 1);
  step u (fun () -> Slots.toggle_run (player u 0).slots_update);
  step u (fun () -> Player.run_slots (player u 0));
  check_int "after toggle_run it swings" (60 - 4) (player_life u 1)

(* ------------------------------------------------------- attack and mana *)

let orc_attack_bonus () =
  let u = new_game () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.orc" 1;
  step u (fun () -> Player.refresh_slots (player u 0));
  check_int "adjacent goblin gets +2" 6 (attack_at u 0 0)

let bull_attack_bonus () =
  let u = new_game () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.bull" 3;
  step u (fun () -> Player.refresh_slots (player u 0));
  check_int "distant goblin gets +1" 5 (attack_at u 0 0)

let orc_skips_zero_attack () =
  let u = new_game () in
  (* AttackUpdate.add ignores creatures whose printed attack is 0 *)
  place u 0 "fire.wall" 0;
  place u 0 "fire.orc" 1;
  step u (fun () -> Player.refresh_slots (player u 0));
  check_int "wall stays at 0" 0 (attack_at u 0 0)

let orc_death_removes_bonus () =
  let u = new_game () in
  (* removing a keyed bonus used to raise "compare: functional value" *)
  place u 0 "fire.goblin" 0;
  place u 0 "fire.orc" 1;
  step u (fun () -> Slot.do_priv_destroy (slot u 0 1));
  check_int "goblin back to 4" 4 (attack_at u 0 0)

let two_bulls_stack_and_unstack () =
  let u = new_game () in
  (* each bull's bonus is keyed by its slot, so killing one removes its own *)
  place u 0 "fire.goblin" 0;
  place u 0 "fire.bull" 2;
  place u 0 "fire.bull" 4;
  step u (fun () -> Player.refresh_slots (player u 0));
  check_int "+1 from each bull" 6 (attack_at u 0 0);
  step u (fun () -> Slot.do_priv_destroy (slot u 0 2));
  check_int "one bull dies, one bonus left" 5 (attack_at u 0 0)

let monk_mana_growth () =
  let u = new_game () in
  let growth () = (player u 0).houses_update.hu_value.(0).growth in
  let before = growth () in
  place u 0 "fire.monk" 0;
  check_int "fire growth +1" (before + 1) (growth ())

let mana_attack_elemental () =
  let u = new_game ~mana0:6 () in
  place u 0 "fire.elemental" 0;
  step u (fun () -> Player.refresh_slots (player u 0));
  check_int "attack = fire mana" 6 (attack_at u 0 0)

let spellmod_dragon () =
  let u = new_game () in
  place u 0 "fire.dragon" 1;
  step u (fun () -> Player.refresh_slots (player u 0));
  check_int "printed attack is not modded" 9 (attack_at u 0 1);
  let spell = mk_damage ~is_spell:true 10 { ctx_player = 0; ctx_card = None; ctx_selected = 0 } in
  check_int "spell damage 10 -> 15" 15 (Player.mod_damage (player u 1) spell).amount

(* -------------------------------------------------- abilities and spells *)

let wall_ability_mass () =
  let u = new_game () in
  place u 0 "fire.wall" 0;
  place u 1 "fire.goblin" 0;
  place u 1 "fire.drake" 3;
  step u (fun () -> Player.apply_effects (player u 0) Direct);
  check_int "goblin 16 -> 11" 11 (life_at u 1 0);
  check_int "drake 18 -> 13" 13 (life_at u 1 3)

let goblin_berserker () =
  let u = new_game () in
  place u 0 "fire.goblin" 1;
  place u 0 "fire.drake" 2;
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "adjacent drake takes 2" 16 (life_at u 0 2)

let inferno_targeting () =
  let u = new_game () in
  place u 1 "fire.wall" 2;
  play u 0 "fire.inferno" ~input:(Some 2) ~cost:9;
  check_bool "targeted wall dies" true (is_empty u 1 2);
  check_int "no damage leaks to the player" 60 (player_life u 1)

let blargl_mass_damage () =
  let u = new_game () in
  place u 0 "fire.blargl" 3;
  place u 0 "fire.goblin" 0;
  place u 1 "fire.goblin" 1;
  play u 0 "fire.blargl" ~input:(Some 3) ~cost:8;
  check_int "own side hit" 12 (life_at u 0 0);
  check_int "opposing side hit" 12 (life_at u 1 1)

let apocalypse_hits_both_sides () =
  let u = new_game ~mana0:11 () in
  place u 0 "fire.goblin" 0;
  place u 1 "fire.goblin" 0;
  play u 0 "fire.apocalypse" ~input:None ~cost:11;
  (* 0 fire mana left + 11 paid + 8 = 19 *)
  check_int "opponent takes 19" (60 - 19) (player_life u 1);
  check_bool "enemy goblin dies" true (is_empty u 1 0);
  check_bool "caster's goblin dies" true (is_empty u 0 0)

let summon_over_occupied_slot () =
  let u = new_game () in
  (* default onSpawnOver destroys the occupant, then the new creature lands *)
  place u 0 "fire.goblin" 0;
  place u 0 "fire.drake" 0;
  Alcotest.(check (option string)) "drake replaces the goblin" (Some "fire.drake") (occupant u 0 0);
  (* privDestroy finishes the death at once; nbDead only counts logged deaths *)
  step u ignore;
  check_int "overwrite is not counted in nb_dead" 0 (player u 0).stats.nb_dead

let () =
  let open Alcotest in
  run "fire"
    [
      ( "combat",
        [
          test_case "basic combat" `Quick basic_combat;
          test_case "wall has no attack" `Quick wall_zero_attack;
          test_case "kill and death" `Quick kill_and_death;
          test_case "run flag" `Quick no_swing_without_run_flag;
        ] );
      ( "attack and mana",
        [
          test_case "orc bonus" `Quick orc_attack_bonus;
          test_case "bull bonus" `Quick bull_attack_bonus;
          test_case "no bonus on 0 attack" `Quick orc_skips_zero_attack;
          test_case "orc death removes bonus" `Quick orc_death_removes_bonus;
          test_case "two bulls" `Quick two_bulls_stack_and_unstack;
          test_case "monk growth" `Quick monk_mana_growth;
          test_case "elemental mana attack" `Quick mana_attack_elemental;
          test_case "dragon SpellMod" `Quick spellmod_dragon;
        ] );
      ( "abilities and spells",
        [
          test_case "wall ability" `Quick wall_ability_mass;
          test_case "goblin berserker" `Quick goblin_berserker;
          test_case "inferno" `Quick inferno_targeting;
          test_case "blargl" `Quick blargl_mass_damage;
          test_case "apocalypse" `Quick apocalypse_hits_both_sides;
          test_case "summon over occupied slot" `Quick summon_over_occupied_slot;
        ] );
    ]
