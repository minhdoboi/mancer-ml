(* Role and tempo checks from StrategistTest.scala. *)

open Mancer

let check_bool = Alcotest.(check bool)
let check_float = Alcotest.(check (float 0.02))
let check_int = Alcotest.(check int)

let creature ~attack ~life ~cost ?(hasted = false) ?(multi = false) () : Strategist.profile =
  {
    is_spell = false;
    attack;
    mana_attack = false;
    life;
    cost;
    hasted;
    multi;
    invincible = false;
    altar = false;
  }

let spell () : Strategist.profile =
  {
    is_spell = true;
    attack = 0;
    mana_attack = false;
    life = 0;
    cost = 1;
    hasted = false;
    multi = false;
    invincible = false;
    altar = false;
  }

let healer () =
  let r = Strategist.score_profile (creature ~attack:3 ~life:20 ~cost:7 ()) "Every turn heals by 3 owner and his creatures" in
  check_bool "heal > damage" true (r.heal > r.damage);
  check_bool "heal > mana" true (r.heal > r.mana)

let burn_spell () =
  let r = Strategist.score_profile (spell ()) "Deals 9 damage to opponent creatures" in
  check_bool "damage > heal" true (r.damage > r.heal);
  check_bool "damage > presence" true (r.damage > r.presence);
  check_bool "damage > mana" true (r.damage > r.mana)

let mana_grower () =
  let r = Strategist.score_profile (creature ~attack:3 ~life:13 ~cost:3 ()) "Every turn increase fire mana growth by 1" in
  check_bool "mana > damage" true (r.mana > r.damage);
  check_bool "mana > heal" true (r.mana > r.heal)

let big_body () =
  let r = Strategist.score_profile (creature ~attack:9 ~life:41 ~cost:8 ()) "Increase spell damage by 50%" in
  check_bool "presence" true (r.presence > 0.8)

let spawner_presence () =
  let plain = Strategist.score_profile (creature ~attack:4 ~life:16 ~cost:4 ()) "No special ability" in
  let spawner = Strategist.score_profile (creature ~attack:4 ~life:16 ~cost:4 ()) "Spawn 2 forest spiders around him" in
  check_bool "spawn presence" true (spawner.presence > plain.presence)

let tempo_dial () =
  let g_long, d_long, w_long = Strategist.for_tempo 0. 1. in
  let g_short, d_short, w_short = Strategist.for_tempo 1. 1. in
  check_float "long gamma" 0.97 g_long;
  check_int "long depth" 3 d_long;
  check_int "long width" 3 w_long;
  check_float "short gamma" 0.70 g_short;
  check_int "short depth" 2 d_short;
  check_int "short width" 8 w_short;
  check_bool "short gamma lower" true (g_short < g_long);
  check_bool "short wider" true (w_short > w_long)

let houses_on_scale () =
  let burn =
    [
      Strategist.score_profile (spell ()) "Deals 9 damage to opponent creatures";
      Strategist.score_profile
        (creature ~attack:4 ~life:18 ~cost:4 ~hasted:true ())
        "Attack the turn he is summoned";
    ]
  in
  let engine =
    [
      Strategist.score_profile (creature ~attack:3 ~life:20 ~cost:7 ()) "Every turn heals by 3 owner and his creatures";
      Strategist.score_profile (creature ~attack:3 ~life:13 ~cost:3 ()) "Every turn increase fire mana growth by 1";
    ]
  in
  let t_burn, _ = Strategist.read_tempo ~house:burn ~known:[] in
  let t_engine, _ = Strategist.read_tempo ~house:engine ~known:[] in
  check_bool "burn short" true (t_burn > 0.66);
  check_bool "engine long" true (t_engine < 0.33)

let tournament_id () =
  let _ : Game.mover = Tournament.mover ~stats_log:false ~budget:50 "strategist" in
  ()

let () =
  let open Alcotest in
  run "strategist"
    [
      ( "roles",
        [
          test_case "healer" `Quick healer;
          test_case "burn spell" `Quick burn_spell;
          test_case "mana grower" `Quick mana_grower;
          test_case "big body" `Quick big_body;
          test_case "spawner" `Quick spawner_presence;
        ] );
      ( "plan",
        [
          test_case "tempo dial" `Quick tempo_dial;
          test_case "house tempo" `Quick houses_on_scale;
          test_case "tournament id" `Quick tournament_id;
        ] );
    ]
