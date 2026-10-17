(* Mechanics for the Paganist house. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }
let ten = [ 10; 10; 10; 10; 10 ]

let hit ?(is_ability = false) ?(is_spell = false) u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage ~is_ability ~is_spell amount (ctx (1 - pid))))

let kill u pid num = hit u pid num 1000
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)

let set_life u pid num life = step u (fun () -> Slot.update (slot u pid num) (fun st -> { st with life }))

let cost_of u pid house key =
  let pd = (player u pid).pstate.ps_desc in
  let h = pd.init_desc.pd_houses.(house) in
  let cards = List.fold_left (fun cs m -> m h cs) h.phd_cards pd.desc_mods in
  (List.find (fun cd -> cd.cd_card.key = key) (Array.to_list cards)).cd_cost

let skipped u pid = turn_skipped (player u pid).pstate
let filled u pid = List.filter (fun n -> not (is_empty u pid n)) base_slot_list

(* A duel where both sides own all four basic houses, so a tax on Water, Air and
   Earth can be read back through [cost_of]. *)
let new_full_game () =
  init_all_houses ();
  let houses = [| Fire.house; Water.house; Air.house; Earth.house; Paganist.house |] in
  let desc =
    {
      pd_houses =
        Array.map
          (fun h ->
            {
              phd_house = h;
              phd_cards =
                Array.of_list
                  (List.map
                     (fun c -> { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 })
                     h.h_cards);
            })
          houses;
    }
  in
  let mk () = Game.initial_player_state ten desc in
  Updater.create { players = [| mk (); mk () |] } { gd_players = [| desc; desc |] }

(* ---------------------------------------------------------------- virgin *)

let virgin_heals_after_a_spell () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 0 "paganist.virgin" 0;
  set_life u 0 0 9;
  hit ~is_spell:true u 0 0 20;
  check_bool "dead" true (is_empty u 0 0);
  check_int "heals the life it had" 69 (player_life u 0)

let virgin_ignores_attacks_and_destroy () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 0 "paganist.virgin" 0;
  set_life u 0 0 9;
  hit u 0 0 20;
  check_int "an attack heals nothing" 60 (player_life u 0);
  place u 0 "paganist.virgin" 1;
  set_life u 0 1 9;
  step u (fun () -> Slot.destroy (slot u 0 1));
  check_int "a destroy effect heals nothing" 60 (player_life u 0)

(* ----------------------------------------------------------------- lamb *)

let lamb_summons_on_death () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 0 "paganist.lamb" 0;
  kill u 0 0;
  check_int "one newcomer" 1 (List.length (filled u 0));
  check_bool "not another lamb" false
    (occupant u 0 (List.hd (filled u 0)) = Some "paganist.lamb")

(* --------------------------------------------------------------- offering *)

let final_offering_pays_for_the_top_three () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.wall" 1;
  place u 0 "fire.monk" 2;
  place u 0 "fire.drake" 3;
  play u 0 "paganist.offering" ~input:None ~cost:3;
  check_int "1 + 2 + 3 + 4 - 1" (60 - 9) (player_life u 1);
  check_string "the cheapest survives" "fire.goblin" (Option.get (occupant u 0 0));
  List.iter (fun n -> check_bool "sacrificed" true (is_empty u 0 n)) [ 1; 2; 3 ]

(* --------------------------------------------------------------- contract *)

let unholy_contract_scales_down_with_cost () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 1 "fire.drake" 2;
  play u 0 "paganist.contract" ~input:(Some 2) ~cost:4;
  check_bool "sacrificed" true (is_empty u 1 2);
  check_int "13 - 4" (60 - 9) (player_life u 1)

(* --------------------------------------------------------------- terrorize *)

let terrorize_taxes_the_basic_houses () =
  let u = new_full_game () in
  place u 1 "fire.drake" 2;
  let taxes () =
    [
      cost_of u 1 0 "fire.goblin";
      cost_of u 1 1 "water.meditate";
      cost_of u 1 2 "air.fairy";
      cost_of u 1 3 "earth.elven";
    ]
  in
  let before = taxes () in
  let special = cost_of u 1 4 "paganist.offering" in
  play u 0 "paganist.terrorize" ~input:(Some 2) ~cost:5;
  check_bool "destroyed" true (is_empty u 1 2);
  check_int "13 damage" (60 - 13) (player_life u 1);
  let after = taxes () in
  List.iter2 (fun a b -> check_int "one more" (a + 1) b) before after;
  check_int "special cards are untouched" special (cost_of u 1 4 "paganist.offering");
  (* permanent: it is still there after a full turn of the opponent *)
  step u (fun () -> Player.apply_effects (player u 1) On_end_turn);
  step u (fun () -> Player.prepare_next_turn (player u 1));
  step u (fun () -> Player.apply_effects (player u 1) On_turn);
  check_int "still one more" (List.hd before + 1) (cost_of u 1 0 "fire.goblin")

(* ----------------------------------------------------------------- moloch *)

let moloch_consumes_its_host () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 0 "fire.goblin" 1;
  play u 0 "paganist.moloch" ~input:(Some 1) ~cost:6;
  check_string "moloch took the slot" "paganist.moloch" (Option.get (occupant u 0 1));
  check_int "full life" 48 (life_at u 0 1)

let moloch_heals_everyone () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 0 "paganist.moloch" 2;
  place u 0 "water.lord" 1;
  place u 1 "water.lord" 3;
  set_life u 0 1 10;
  set_life u 1 3 11;
  step u (fun () -> (player u 0).pstate.ps_life <- 40);
  step u (fun () -> (player u 1).pstate.ps_life <- 41);
  on_turn u 0;
  check_int "owner" 44 (player_life u 0);
  check_int "opponent" 45 (player_life u 1);
  check_int "own creature" 14 (life_at u 0 1);
  check_int "opponent creature" 15 (life_at u 1 3)

let moloch_attack_hits_everyone () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 0 "paganist.moloch" 0;
  place u 0 "water.lord" 1;
  place u 1 "water.lord" 0;
  step u (fun () -> Player.run_slot (player u 0) 0 (Slot.get (slot u 0 0)));
  check_int "owner" 53 (player_life u 0);
  check_int "opponent" 53 (player_life u 1);
  check_int "moloch" (48 - 7) (life_at u 0 0);
  check_int "own creature" (34 - 7) (life_at u 0 1);
  check_int "opponent creature" (34 - 7) (life_at u 1 0)

(* ------------------------------------------------------------- obliterate *)

let obliterate_souls_locks_the_caster () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 1 "fire.goblin" 0;
  place u 1 "water.lord" 1;
  play u 0 "paganist.obliterate" ~input:None ~cost:7;
  List.iter (fun n -> check_bool "destroyed" true (is_empty u 1 n)) [ 0; 1 ];
  check_int "3 less fire" 7 (player u 0).houses_update.hu_value.(0).mana;
  check_int "3 less special after paying" 0 (player u 0).houses_update.hu_value.(4).mana;
  check_bool "nothing playable" true (skipped u 0);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_bool "still nothing next turn" true (skipped u 0);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_bool "free again" false (skipped u 0)

(* ----------------------------------------------------------- river of blood *)

let river_of_blood_hits_when_the_caster_survives () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  place u 1 "fire.goblin" 0;
  play u 0 "paganist.blood" ~input:None ~cost:8;
  check_int "caster" 35 (player_life u 0);
  check_int "opponent" 35 (player_life u 1);
  check_bool "opponent creature" true (is_empty u 1 0)

let river_of_blood_stops_at_the_caster () =
  let u = new_game ~mana:ten ~special:Paganist.house () in
  step u (fun () -> (player u 0).pstate.ps_life <- 20);
  place u 1 "fire.goblin" 0;
  play u 0 "paganist.blood" ~input:None ~cost:8;
  check_int "caster died" (20 - 25) (player_life u 0);
  check_int "opponent untouched" 60 (player_life u 1);
  check_string "creature untouched" "fire.goblin" (Option.get (occupant u 1 0))

let () =
  let open Alcotest in
  run "paganist"
    [
      ( "houses",
        [
          test_case "virgin and spells" `Quick virgin_heals_after_a_spell;
          test_case "virgin and other deaths" `Quick virgin_ignores_attacks_and_destroy;
          test_case "lamb" `Quick lamb_summons_on_death;
          test_case "offering" `Quick final_offering_pays_for_the_top_three;
          test_case "contract" `Quick unholy_contract_scales_down_with_cost;
          test_case "terrorize" `Quick terrorize_taxes_the_basic_houses;
          test_case "moloch host" `Quick moloch_consumes_its_host;
          test_case "moloch heal" `Quick moloch_heals_everyone;
          test_case "moloch attack" `Quick moloch_attack_hits_everyone;
          test_case "obliterate" `Quick obliterate_souls_locks_the_caster;
          test_case "river survives" `Quick river_of_blood_hits_when_the_caster_survives;
          test_case "river kills" `Quick river_of_blood_stops_at_the_caster;
        ] );
    ]
