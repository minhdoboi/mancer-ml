(* Mechanics for the Blacksmith house. *)

open Mancer
open Sp
open Engine
open Common
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let ten = [ 10; 10; 10; 10; 10 ]

let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit u pid num amount =
  step u (fun () ->
      Slot.inflict (slot u pid num) (mk_damage amount (ctx (1 - pid))))

let spell_hit u pid num amount =
  step u (fun () ->
      Slot.inflict (slot u pid num) (mk_damage ~is_spell:true amount (ctx (1 - pid))))

let ability_hit u pid num amount =
  step u (fun () ->
      Slot.inflict (slot u pid num) (mk_damage ~is_ability:true amount (ctx (1 - pid))))

let hit_player u pid amount =
  step u (fun () -> Player.inflict (player u pid) (mk_damage ~is_spell:true amount (ctx (1 - pid))))

let kill u pid num = hit u pid num 1000
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)

let run_at u pid num =
  step u (fun () -> Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let max_life_at u pid num = match (slot u pid num).value with Some st -> st.max_life | None -> -1
let marks u pid num = match (slot u pid num).value with Some st -> st.marks | None -> []

let marked u pid num key =
  List.exists (fun (m : blacksmith_mark) -> m.bm_key = key) (marks u pid num)

let mark_used u pid num key =
  List.exists (fun (m : blacksmith_mark) -> m.bm_key = key && m.bm_used) (marks u pid num)

(* ----------------------------------------------------------- wielder gate *)

let wielder_cost_limit () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  play u 0 "earth.elemental" ~input:(Some 0) ~cost:0;
  play u 0 "earth.troll" ~input:(Some 1) ~cost:0;
  let ustate = player u 0 in
  let cmds =
    Game.available_commands u ustate
    |> List.filter (fun c -> c.cmd_card.key = "blacksmith.mithrilspear")
  in
  check_bool "spear offered" true (cmds <> []);
  List.iter
    (fun (c : command) ->
      match c.cmd_input with
      | Some num ->
          check_bool "only the cheap creature" true
            (match (slot u 0 num).value with Some st -> st.card.c_card.cost <= 8 | None -> false)
      | None -> Alcotest.fail "wielder spell without an input")
    cmds

(* -------------------------------------------------------------- mithril *)

let spear_boosts_attack () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  play u 0 "blacksmith.mithrilspear" ~input:(Some 2) ~cost:1;
  check_int "attack" 8 (attack_at u 0 2);
  check_bool "icon" true (marked u 0 2 "spear")

let spear_sprite_ticks () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.sprite" 2;
  place u 1 "fire.goblin" 2;
  let before = attack_at u 0 2 in
  play u 0 "blacksmith.mithrilspear" ~input:(Some 2) ~cost:1;
  check_int "no attack bonus" before (attack_at u 0 2);
  on_turn u 0;
  check_int "sprite damage" 14 (life_at u 1 2)

let bow_runs_and_stuns_once () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place ~run:false u 0 "earth.troll" 2;
  place u 1 "fire.goblin" 2;
  play u 0 "blacksmith.mithrilbow" ~input:(Some 2) ~cost:2;
  check_bool "same-turn attack" true
    (match (slot u 0 2).value with Some st -> has_flag st run_flag | None -> false);
  run_at u 0 2;
  check_bool "stunned" true
    (match (slot u 1 2).value with Some st -> has_flag st stun_flag | None -> false);
  check_bool "used" true (mark_used u 0 2 "bow")

let shield_raises_max_life () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  check_int "before" 26 (max_life_at u 0 2);
  play u 0 "blacksmith.mithrilshield" ~input:(Some 2) ~cost:3;
  check_int "life" 36 (life_at u 0 2);
  check_int "max life" 36 (max_life_at u 0 2)

let hat_stops_effects_only () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  play u 0 "blacksmith.wizardshat" ~input:(Some 2) ~cost:4;
  spell_hit u 0 2 10;
  ability_hit u 0 2 10;
  check_int "spell and ability" 26 (life_at u 0 2);
  hit u 0 2 10;
  check_int "normal attack lands" 16 (life_at u 0 2)

(* --------------------------------------------------------- flying chariot *)

let chariot_hides_and_returns () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  place u 1 "fire.goblin" 2;
  play u 0 "blacksmith.flyingchariot" ~input:(Some 2) ~cost:5;
  check_string "pegasus" "blacksmith.pegasus" (Option.get (occupant u 0 2));
  check_string "rider is carried" "earth.troll"
    (match (slot u 0 2).value with
    | Some { data = Ridden hidden; _ } -> hidden.card.c_card.key
    | _ -> "");
  on_end u 0;
  check_int "X+6" 4 (life_at u 1 2);
  kill u 0 2;
  check_string "wielder returns" "earth.troll" (Option.get (occupant u 0 2))

(* ------------------------------------------------------- cloak and dagger *)

let chariot_survives_the_turn () =
  let desc =
    {
      gd_players =
        [|
          Game.make_player_desc ~special:Blacksmith.house Fire.house;
          Game.make_player_desc ~special:Blacksmith.house Fire.house;
        |];
    }
  in
  let s = Session.create ~mana0:ten ~mana1:ten ~starting:owner desc in
  let u = Session.updater s in
  ignore (Slots.summon (player u 0).slots_update 2 (make_creature (card_by_key "fire.goblin")));
  Session.submit s
    (Some
       {
         cmd_player = 0;
         cmd_card = card_by_key "blacksmith.flyingchariot";
         cmd_input = Some 2;
         cmd_cost = 5;
         cmd_flag = None;
       });
  let state = Session.state s in
  let st = state.players.(0).ps_slots.(2) in
  check_bool "pegasus stays after the turn" true
    (match st with Some st -> st.card.c_card.key = "blacksmith.pegasus" | None -> false);
  check_int "hidden goblin does not attack" 50 state.players.(1).ps_life


let chariot_keeps_wielder_buffs () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  play u 0 "blacksmith.mithrilspear" ~input:(Some 2) ~cost:1;
  play u 0 "blacksmith.flyingchariot" ~input:(Some 2) ~cost:5;
  kill u 0 2;
  check_string "same wielder" "earth.troll" (Option.get (occupant u 0 2));
  check_int "spear bonus kept" 8 (attack_at u 0 2)

let cloak_and_dagger_aura () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  place u 1 "fire.goblin" 2;
  play u 0 "blacksmith.cloakanddagger" ~input:(Some 2) ~cost:6;
  check_string "cloak left" "blacksmith.astralcloak" (Option.get (occupant u 0 1));
  check_string "dagger right" "blacksmith.astraldagger" (Option.get (occupant u 0 3));
  hit_player u 0 5;
  check_int "owner protected" 56 (player_life u 0);
  hit u 0 2 10;
  check_int "wielder protected" 17 (life_at u 0 2);
  hit_player u 1 5;
  check_int "dagger raises opponent damage" 54 (player_life u 1);
  hit u 1 2 10;
  check_int "dagger raises opposing damage" 5 (life_at u 1 2)

let astral_deaths_give_power () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  play u 0 "blacksmith.cloakanddagger" ~input:(Some 2) ~cost:6;
  let water = mana u 0 1 and earth = mana u 0 3 and fire = mana u 0 0 and air = mana u 0 2 in
  kill u 0 1;
  check_int "water" (water + 1) (mana u 0 1);
  check_int "earth" (earth + 1) (mana u 0 3);
  kill u 0 3;
  check_int "fire" (fire + 1) (mana u 0 0);
  check_int "air" (air + 1) (mana u 0 2)

(* --------------------------------------------------------------- excalibur *)

let excalibur_cleaves () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  place u 1 "fire.goblin" 2;
  play u 0 "blacksmith.excalibur" ~input:(Some 2) ~cost:7;
  check_bool "opposite destroyed" true (is_empty u 1 2);
  check_int "attack halved" 3 (attack_at u 0 2);
  place u 1 "fire.goblin" 0;
  run_at u 0 2;
  check_int "player hit" 57 (player_life u 1);
  check_int "all creatures hit" 13 (life_at u 1 0)

let excalibur_unopposed () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  play u 0 "blacksmith.excalibur" ~input:(Some 2) ~cost:7;
  check_int "15 to the player" 45 (player_life u 1)

(* ------------------------------------------------------- bargulian gauntlet *)

let gauntlet_burns_max_life () =
  let u = new_game ~mana:ten ~special:Blacksmith.house () in
  place u 0 "earth.troll" 2;
  place u 1 "earth.elemental" 2;
  play u 0 "blacksmith.barguliangauntlet" ~input:(Some 2) ~cost:8;
  check_int "life" 35 (life_at u 1 2);
  check_int "max life" 35 (max_life_at u 1 2);
  run_at u 0 2;
  check_int "attack life" 29 (life_at u 1 2);
  check_int "attack max life" 29 (max_life_at u 1 2)

let () =
  Alcotest.run "blacksmith"
    [
      ( "wielder",
        [ Alcotest.test_case "cost limit" `Quick wielder_cost_limit ] );
      ( "mithril",
        [
          Alcotest.test_case "spear" `Quick spear_boosts_attack;
          Alcotest.test_case "spear sprite" `Quick spear_sprite_ticks;
          Alcotest.test_case "bow" `Quick bow_runs_and_stuns_once;
          Alcotest.test_case "shield" `Quick shield_raises_max_life;
          Alcotest.test_case "hat" `Quick hat_stops_effects_only;
        ] );
      ( "chariot",
        [
          Alcotest.test_case "rides and returns" `Quick chariot_hides_and_returns;
          Alcotest.test_case "survives turn" `Quick chariot_survives_the_turn;
          Alcotest.test_case "keeps buffs" `Quick chariot_keeps_wielder_buffs;
        ] );
      ( "cloak and dagger",
        [
          Alcotest.test_case "auras" `Quick cloak_and_dagger_aura;
          Alcotest.test_case "death power" `Quick astral_deaths_give_power;
        ] );
      ( "excalibur",
        [
          Alcotest.test_case "cleaves" `Quick excalibur_cleaves;
          Alcotest.test_case "unopposed" `Quick excalibur_unopposed;
        ] );
      ( "gauntlet",
        [ Alcotest.test_case "max life" `Quick gauntlet_burns_max_life ] );
    ]
