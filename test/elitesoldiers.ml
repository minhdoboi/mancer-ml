(* Mechanics for the Elite Soldiers: a soldier replaces its card with a cheap
   upgrade while it holds the board, the upgrade drills it permanently, and
   the training dies with the soldier. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)

let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana

let run_at u pid num =
  step u (fun () ->
      Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let stunned u pid num = match (slot u pid num).value with Some st -> has_flag st stun_flag | None -> false

let cast_spell u pid n =
  step u (fun () ->
      Player.inflict (player u (1 - pid))
        (mk_damage ~is_spell:true n { ctx_player = pid; ctx_card = None; ctx_selected = -1 }))

(* The desc entry the game would offer for [key] after the player's mods: while
   a soldier stands, its card reads as the matching upgrade. *)
let effective_card u pid key =
  let pd = (player u pid).pstate.ps_desc in
  let h =
    Option.get
      (Array.find_opt
         (fun (h : player_house_desc) -> h.phd_house.h_index = special_house_index)
         pd.init_desc.pd_houses)
  in
  let probe = [| { cd_card = card_by_key key; cd_cost = 0; cd_enabled = true; cd_extra_mana = 0 } |] in
  let out = List.fold_left (fun cs m -> m h cs) probe pd.desc_mods in
  out.(0).cd_card.key

(* Is the card's entry gated off by a desc mod (hide_spell)? *)
let banned u pid key =
  let pd = (player u pid).pstate.ps_desc in
  let h =
    Option.get
      (Array.find_opt
         (fun (h : player_house_desc) -> List.exists (fun c -> c.key = key) h.phd_house.h_cards)
         pd.init_desc.pd_houses)
  in
  let probe = [| { cd_card = card_by_key key; cd_cost = 0; cd_enabled = true; cd_extra_mana = 0 } |] in
  let out = List.fold_left (fun cs m -> m h cs) probe pd.desc_mods in
  not out.(0).cd_enabled

(* ------------------------------------------------- soldier <-> upgrade *)

let soldier_turns_into_upgrade () =
  let u = new_game ~special:Elitesoldiers.house () in
  check_string "soldier offered at first" "elitesoldiers.clubman"
    (effective_card u 0 "elitesoldiers.clubman");
  place u 0 "elitesoldiers.clubman" 2;
  check_string "card becomes the upgrade" "elitesoldiers.bolster"
    (effective_card u 0 "elitesoldiers.clubman")

let death_restores_the_card () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.clubman" 2;
  step u (fun () -> Slot.destroy (slot u 0 2));
  check_string "soldier can be summoned again" "elitesoldiers.clubman"
    (effective_card u 0 "elitesoldiers.clubman")

(* ----------------------------------------------------------- the upgrades *)

let bolster_buffs_permanently () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.clubman" 2;
  check_int "base attack" 3 (attack_at u 0 2);
  check_int "base life" 17 (life_at u 0 2);
  play u 0 "elitesoldiers.bolster" ~input:None ~cost:1;
  check_int "attack +2" 5 (attack_at u 0 2);
  check_int "life +6" 23 (life_at u 0 2)

let training_dies_with_the_soldier () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.clubman" 2;
  play u 0 "elitesoldiers.bolster" ~input:None ~cost:1;
  play u 0 "elitesoldiers.bolster" ~input:None ~cost:1;
  check_int "trained" 7 (attack_at u 0 2);
  step u (fun () -> Slot.destroy (slot u 0 2));
  place u 0 "elitesoldiers.clubman" 2;
  check_int "fresh recruit attack" 3 (attack_at u 0 2);
  check_int "fresh recruit life" 17 (life_at u 0 2)

let wide_shield_heals_and_stuns () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.spearman" 2;
  place u 0 "anger.werewolf" 3;
  step u (fun () ->
      Slot.inflict (slot u 0 3) (mk_damage 6 { ctx_player = 1; ctx_card = None; ctx_selected = -1 }));
  play u 0 "elitesoldiers.wideshield" ~input:None ~cost:1;
  check_int "spearman healed" (26 + 14) (life_at u 0 2);
  check_int "wounded neighbour healed" (16 - 6 + 4) (life_at u 0 3);
  check_bool "spearman is stunned" true (stunned u 0 2)

let ballistic_moves_the_soldier () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.crossbowman" 2;
  play u 0 "elitesoldiers.ballistic" ~input:(Some 4) ~cost:2;
  check_bool "left the old slot" true (is_empty u 0 2);
  check_string "lands in the chosen slot" "elitesoldiers.crossbowman" (Option.get (occupant u 0 4));
  check_int "attack +1" 3 (attack_at u 0 4);
  check_int "life +4" 25 (life_at u 0 4)

let battle_shout_stuns_a_target () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.axeman" 2;
  place u 1 "anger.werewolf" 3;
  play u 0 "elitesoldiers.battleshout" ~input:(Some 3) ~cost:2;
  check_int "axeman attack +3" 12 (attack_at u 0 2);
  check_int "axeman life +12" 58 (life_at u 0 2);
  check_bool "target stunned" true (stunned u 1 3)

let sniper_shoots_a_target () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 1 "anger.werewolf" 3;
  place u 0 "elitesoldiers.shotgunner" 2;
  play u 0 "elitesoldiers.sniper" ~input:(Some 3) ~cost:2;
  check_int "shotgunner attack +3" 8 (attack_at u 0 2);
  check_int "target took 13" (16 - 13) (life_at u 1 3)

let war_backpack_locks_spells () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.commando" 2;
  play u 0 "elitesoldiers.warbackpack" ~input:None ~cost:3;
  check_bool "opponent spells hang" true (banned u 1 "anger.wave");
  check_bool "own spells untouched" false (banned u 0 "anger.wave");
  check_int "commando attack +2" 7 (attack_at u 0 2);
  on_end u 1;
  check_bool "lifted at their end of turn" false (banned u 1 "anger.wave")

(* ------------------------------------------------------------- abilities *)

let spearman_weakens_the_facing_creature () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 1 "anger.werewolf" 2;
  place u 0 "elitesoldiers.spearman" 2;
  step u (fun () -> ());
  check_int "attack down 2" (4 - 2) (attack_at u 1 2);
  step u (fun () -> Slot.destroy (slot u 0 2));
  step u (fun () -> ());
  check_int "attack restored" 4 (attack_at u 1 2)

let clubman_falls_on_its_sword () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.clubman" 2;
  place u 1 "anger.werewolf" 2;
  step u (fun () -> Slot.destroy (slot u 0 2));
  check_int "facing creature took 7" (16 - 7) (life_at u 1 2)

let slinger_splashes_on_a_creature () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.slinger" 2;
  place u 1 "anger.werewolf" 2;
  run_at u 0 2;
  check_int "creature took 3" (16 - 3) (life_at u 1 2);
  check_int "opponent took 3" (60 - 3) (player_life u 1)

let slinger_direct_hit_doubles () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.slinger" 2;
  place u 1 "sadness.greywolf" 3;
  run_at u 0 2;
  check_int "opponent took 3" (60 - 3) (player_life u 1);
  check_int "biggest creature took 6" (15 - 6) (life_at u 1 3)

(* The unopposed stone is announced for the animation: the front end throws a
   rock from the Slinger to the creature with the most life. *)
let slinger_announces_its_stone () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.slinger" 2;
  place u 1 "sadness.greywolf" 3;
  let events = ref [] in
  Updater.listen u (fun e -> events := e :: !events);
  run_at u 0 2;
  let thrown =
    List.filter_map
      (function
        | Ev_ability { kind = "elitesoldiers.sling"; player = 0; num = 2; target = Some (1, n) } -> Some n
        | _ -> None)
      !events
  in
  match thrown with
  | [ n ] -> check_int "the stone found the biggest creature" 3 n
  | _ -> Alcotest.fail "the slinger announced no stone"

let crossbow_sweeps () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.crossbowman" 2;
  place u 1 "anger.werewolf" 3;
  place u 1 "sadness.greywolf" 4;
  run_at u 0 2;
  check_int "werewolf took 2" (16 - 2) (life_at u 1 3);
  check_int "greywolf took 2" (15 - 2) (life_at u 1 4);
  check_int "opponent took 2" (60 - 2) (player_life u 1)

let crossbow_pays_for_a_double_kill () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.crossbowman" 2;
  place u 1 "anger.werewolf" 3;
  place u 1 "sadness.greywolf" 4;
  (* soften both into range of the volley *)
  step u (fun () -> Slot.inflict (slot u 1 3) (mk_damage 14 { ctx_player = 0; ctx_card = None; ctx_selected = -1 }));
  step u (fun () -> Slot.inflict (slot u 1 4) (mk_damage 14 { ctx_player = 0; ctx_card = None; ctx_selected = -1 }));
  let before = mana u 0 0 in
  run_at u 0 2;
  check_int "anger power +1" (before + 1) (mana u 0 0)

let axeman_doubles_and_withers () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.axeman" 2;
  place u 1 "anger.werewolf" 2;
  run_at u 1 2;
  check_int "took double" (46 - 8) (life_at u 0 2);
  check_int "lost 1 attack" (9 - 1) (attack_at u 0 2)

let swordmaster_caps_damage () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.swordmaster" 2;
  place u 1 "anger.dragon" 2;
  run_at u 1 2;
  check_int "9 damage capped to 2" (14 - 2) (life_at u 0 2)

let swordmaster_feeds_spells () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.swordmaster" 2;
  cast_spell u 0 5;
  check_int "spell dealt 5 + 3" (60 - 8) (player_life u 1)

let shotgunner_punishes_every_summon () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 1 "anger.dragon" 3;
  place u 0 "elitesoldiers.shotgunner" 2;
  place u 1 "anger.werewolf" 4;
  check_int "dragon took 5" (41 - 5) (life_at u 1 3);
  check_int "newcomer took 5" (16 - 5) (life_at u 1 4)

let commando_strikes_twice () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.commando" 2;
  place u 0 "anger.werewolf" 3;
  place u 1 "anger.dragon" 2;
  place u 1 "sadness.greywolf" 1;
  run_at u 0 2;
  check_int "facing dragon took 5" (41 - 5) (life_at u 1 2);
  check_int "flanking greywolf took 5" (15 - 5) (life_at u 1 1);
  check_string "commando came home" "elitesoldiers.commando" (Option.get (occupant u 0 2))

(* The gap must not need an ally on the other side: a lone commando still
   steps into the empty slot that faces a creature and strikes. *)
let commando_flanks_from_an_open_gap () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.commando" 2;
  place u 1 "anger.dragon" 2;
  place u 1 "sadness.greywolf" 1;
  run_at u 0 2;
  check_int "facing dragon took 5" (41 - 5) (life_at u 1 2);
  check_int "flanking greywolf took 5" (15 - 5) (life_at u 1 1);
  check_string "commando came home" "elitesoldiers.commando" (Option.get (occupant u 0 2))

(* With both neighbours filled there is no gap to step into, so only the
   ordinary attack lands. *)
let commando_boxed_in_strikes_once () =
  let u = new_game ~special:Elitesoldiers.house () in
  place u 0 "elitesoldiers.commando" 2;
  place u 0 "anger.werewolf" 1;
  place u 0 "anger.werewolf" 3;
  place u 1 "anger.dragon" 2;
  place u 1 "sadness.greywolf" 1;
  run_at u 0 2;
  check_int "facing dragon took 5" (41 - 5) (life_at u 1 2);
  check_int "greywolf untouched" 15 (life_at u 1 1)

let () =
  Alcotest.run "elitesoldiers"
    [
      ( "soldier",
        [
          Alcotest.test_case "turns into an upgrade" `Quick soldier_turns_into_upgrade;
          Alcotest.test_case "death restores the card" `Quick death_restores_the_card;
        ] );
      ( "upgrades",
        [
          Alcotest.test_case "bolster" `Quick bolster_buffs_permanently;
          Alcotest.test_case "training dies" `Quick training_dies_with_the_soldier;
          Alcotest.test_case "wide shield" `Quick wide_shield_heals_and_stuns;
          Alcotest.test_case "ballistic" `Quick ballistic_moves_the_soldier;
          Alcotest.test_case "battle-shout" `Quick battle_shout_stuns_a_target;
          Alcotest.test_case "sniper bullets" `Quick sniper_shoots_a_target;
          Alcotest.test_case "war backpack" `Quick war_backpack_locks_spells;
        ] );
      ( "abilities",
        [
          Alcotest.test_case "spearman weakens" `Quick spearman_weakens_the_facing_creature;
          Alcotest.test_case "clubman death blow" `Quick clubman_falls_on_its_sword;
          Alcotest.test_case "slinger splash" `Quick slinger_splashes_on_a_creature;
          Alcotest.test_case "slinger direct" `Quick slinger_direct_hit_doubles;
          Alcotest.test_case "slinger announces stone" `Quick slinger_announces_its_stone;
          Alcotest.test_case "crossbow sweep" `Quick crossbow_sweeps;
          Alcotest.test_case "crossbow double kill" `Quick crossbow_pays_for_a_double_kill;
          Alcotest.test_case "axeman doubles" `Quick axeman_doubles_and_withers;
          Alcotest.test_case "swordmaster cap" `Quick swordmaster_caps_damage;
          Alcotest.test_case "swordmaster spells" `Quick swordmaster_feeds_spells;
          Alcotest.test_case "shotgunner volley" `Quick shotgunner_punishes_every_summon;
          Alcotest.test_case "commando double strike" `Quick commando_strikes_twice;
          Alcotest.test_case "commando open gap" `Quick commando_flanks_from_an_open_gap;
          Alcotest.test_case "commando boxed in" `Quick commando_boxed_in_strikes_once;
        ] );
    ]
