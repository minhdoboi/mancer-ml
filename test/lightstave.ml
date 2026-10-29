(* Mechanics for the Lightstaves: the Aura lifecycle and each card's trigger. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let ten = [ 10; 10; 10; 10; 10 ]

let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let flush u =
  Player.flush (player u 0);
  Player.flush (player u 1)

let hit u pid num amount =
  step u (fun () ->
      Slot.inflict (slot u pid num) (mk_damage amount (ctx (1 - pid)));
      flush u)

let spell_hit u pid num amount =
  step u (fun () ->
      Slot.inflict (slot u pid num) (mk_damage ~is_spell:true amount (ctx (1 - pid)));
      flush u)

let kill u pid num = hit u pid num 1000
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let run_at u pid num =
  step u (fun () -> Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let mana u pid i = (player u pid).houses_update.hu_value.(i).mana

let auras u pid = Lightstave.auras (player u pid).pstate
let one_aura u pid = match auras u pid with [ a ] -> a | _ -> Alcotest.fail "expected exactly one aura"
let set_player_life u pid n = (player u pid).pstate.ps_life <- n

(* Is the card's desc entry gated off by the player's desc mods? *)
let banned u pid key =
  let pd = (player u pid).pstate.ps_desc in
  let h =
    Option.get
      (Array.find_opt
         (fun h -> List.exists (fun c -> c.Sp.key = key) h.phd_house.Sp.h_cards)
         pd.init_desc.pd_houses)
  in
  let probe = [| { cd_card = card_by_key key; cd_cost = 0; cd_enabled = true; cd_extra_mana = 0 } |] in
  let out = List.fold_left (fun cs m -> m h cs) probe pd.desc_mods in
  not out.(0).cd_enabled

let aura_is_not_a_spell () =
  let c = card_by_key "lightstave.auraoflife" in
  check_bool "aura" true c.is_aura;
  check_bool "not a spell" false c.is_spell

(* ------------------------------------------------------------- Aura of Life *)

let life_heals_on_cast_and_each_later_turn () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.sprite" 1;
  place u 0 "earth.sprite" 2;
  place u 0 "earth.sprite" 3;
  hit u 0 1 10;
  hit u 0 2 10;
  hit u 0 3 10;
  set_player_life u 0 20;
  play u 0 "lightstave.auraoflife" ~input:(Some 2) ~cost:3;
  check_int "caster heals on cast" 25 (player_life u 0);
  check_int "one aura" 1 (List.length (auras u 0));
  check_int "four turns" 4 (one_aura u 0).au_remaining;
  check_bool "second copy banned" true (banned u 0 "lightstave.auraoflife");
  check_int "no tick on the cast turn" 12 (life_at u 0 2);
  check_int "left neighbour untouched on cast" 12 (life_at u 0 1);
  check_int "right neighbour untouched on cast" 12 (life_at u 0 3);
  on_turn u 0;
  check_int "target heals 4" 16 (life_at u 0 2);
  check_int "left neighbour heals 4" 16 (life_at u 0 1);
  check_int "right neighbour heals 4" 16 (life_at u 0 3);
  check_int "still three turns left" 3 (one_aura u 0).au_remaining

let a_same_named_aura_never_stacks () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.troll" 1;
  set_player_life u 0 20;
  play u 0 "lightstave.auraoflife" ~input:(Some 1) ~cost:3;
  check_int "caster heals on cast" 25 (player_life u 0);
  (* submit_command does not consult the desc mod, so a replay or a bot can
     still try: the second copy must be dropped, without a second heal. *)
  play u 0 "lightstave.auraoflife" ~input:(Some 1) ~cost:3;
  check_int "second cast does not heal" 25 (player_life u 0);
  check_int "still one aura" 1 (List.length (auras u 0))

let aura_name_is_exclusive_across_the_table () =
  let u =
    new_duel ~mana:ten ~special0:Lightstave.house ~special1:Lightstave.house ()
  in
  place u 0 "earth.troll" 1;
  place u 1 "earth.troll" 2;
  set_player_life u 1 20;
  play u 0 "lightstave.auraoflife" ~input:(Some 1) ~cost:3;
  check_bool "foe's copy banned" true (banned u 1 "lightstave.auraoflife");
  play u 1 "lightstave.auraoflife" ~input:(Some 2) ~cost:3;
  check_int "no second aura" 0 (List.length (auras u 1));
  check_int "refused cast does not heal" 20 (player_life u 1);
  for _ = 1 to 4 do
    on_turn u 0
  done;
  check_bool "foe's copy unbanned" false (banned u 1 "lightstave.auraoflife");
  check_bool "own copy unbanned" false (banned u 0 "lightstave.auraoflife")

(* Auras belong to their caster's side: the offered commands target only the
   caster's own creatures, never the opponent's. *)
let auras_target_own_creatures_only () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.troll" 2;
  place u 1 "earth.troll" 3;
  step u (fun () -> ());
  let aura_cmds =
    Game.legal_commands 0 u.desc u.state
    |> List.filter (fun c -> c.cmd_card.is_aura && c.cmd_input = Some 2)
  in
  check_bool "own creature targetable" true (aura_cmds <> []);
  check_bool "foe side never offered" true (List.for_all (fun c -> c.cmd_flag = None) aura_cmds);
  let foe_cmds =
    Game.legal_commands 1 u.desc u.state
    |> List.filter (fun c -> c.cmd_card.is_aura && c.cmd_input = Some 3)
  in
  check_bool "other side sees only its own" true (List.for_all (fun c -> c.cmd_flag = None) foe_cmds)

let life_ticks_four_turns_then_dissipates () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.sprite" 2;
  hit u 0 2 20;
  set_player_life u 0 20;
  play u 0 "lightstave.auraoflife" ~input:(Some 2) ~cost:3;
  check_int "cast heal" 25 (player_life u 0);
  check_int "no tick on the cast turn" 2 (life_at u 0 2);
  for _ = 1 to 3 do
    on_turn u 0
  done;
  check_int "three ticks" 14 (life_at u 0 2);
  check_int "last turn left" 1 (one_aura u 0).au_remaining;
  check_int "no dissipate heal yet" 25 (player_life u 0);
  on_turn u 0;
  check_int "four ticks" 18 (life_at u 0 2);
  check_int "aura gone" 0 (List.length (auras u 0));
  check_int "dissipate heal" 30 (player_life u 0);
  check_bool "card unbanned" false (banned u 0 "lightstave.auraoflife")

let life_ends_when_target_dies () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.troll" 2;
  set_player_life u 0 20;
  play u 0 "lightstave.auraoflife" ~input:(Some 2) ~cost:3;
  kill u 0 2;
  check_int "aura gone with target" 0 (List.length (auras u 0));
  check_int "dissipate heal" 30 (player_life u 0)

(* ------------------------------------------------------- Aura of Protection *)

let protection_reduces_target_damage () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.troll" 2;
  play u 0 "lightstave.auraofprotection" ~input:(Some 2) ~cost:4;
  hit u 0 2 10;
  check_int "five points stopped" 21 (life_at u 0 2)

let protection_redirects_neighbour_damage () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.troll" 1;
  place u 0 "earth.troll" 2;
  play u 0 "lightstave.auraofprotection" ~input:(Some 2) ~cost:4;
  hit u 0 1 10;
  check_int "neighbour untouched" 26 (life_at u 0 1);
  (* the redirected hit is damage to the protected target, so it is reduced too *)
  check_int "target took the reduced hit" 21 (life_at u 0 2)

(* ---------------------------------------------------- Aura of Illumination *)

let illumination_heals_own_and_burns_foes () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.sprite" 1;
  place u 0 "earth.sprite" 2;
  hit u 0 1 5;
  place u 1 "fire.goblin" 3;
  place u 1 "fire.goblin" 4;
  let goblin = life_at u 1 3 in
  play u 0 "lightstave.auraofillumination" ~input:(Some 2) ~cost:5;
  check_int "no tick on the cast turn" 17 (life_at u 0 1);
  check_int "foe untouched on the cast turn" goblin (life_at u 1 3);
  on_turn u 0;
  check_int "own creature heals 3" 20 (life_at u 0 1);
  check_int "target heals 3" 22 (life_at u 0 2);
  check_int "foe burns" (goblin - 3) (life_at u 1 3);
  check_int "second foe burns" (goblin - 3) (life_at u 1 4)

(* ------------------------------------------------------------ Prism Lantern *)

let prism_lantern_extends_every_aura () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.troll" 2;
  play u 0 "lightstave.auraoflife" ~input:(Some 2) ~cost:3;
  play u 0 "lightstave.auraofprotection" ~input:(Some 2) ~cost:4;
  play u 0 "lightstave.prismlantern" ~input:(Some 4) ~cost:1;
  List.iter (fun a -> check_int "one more turn" 5 a.au_remaining) (auras u 0)

(* ------------------------------------------------------------- Aurora *)

let aurora_retaliates () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "lightstave.aurora" 2;
  let before = player_life u 1 in
  hit u 0 2 5;
  check_int "opponent takes 2" (before - 2) (player_life u 1)

(* ------------------------------------------------------ Crystalline Palace *)

let palace_attack_and_spell_retaliation () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "lightstave.crystallinepalace" 2;
  place u 1 "fire.goblin" 3;
  place u 1 "fire.goblin" 4;
  let before = player_life u 1 in
  let goblin = life_at u 1 3 in
  run_at u 0 2;
  check_int "attack hits the opponent" (before - 3) (player_life u 1);
  check_int "attack hits every foe" (goblin - 3) (life_at u 1 3);
  check_int "and the second foe" (goblin - 3) (life_at u 1 4);
  spell_hit u 0 2 5;
  check_int "palace keeps the five" 22 (life_at u 0 2);
  check_int "spell damage hits the opponent" (before - 6) (player_life u 1);
  check_int "spell damage hits every foe" (goblin - 6) (life_at u 1 4)

(* -------------------------------------------------------- Priestess of Light *)

let priestess_ends_every_aura_and_burns () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "earth.troll" 2;
  place u 1 "fire.goblin" 3;
  place u 1 "fire.goblin" 4;
  play u 0 "lightstave.auraoflife" ~input:(Some 2) ~cost:3;
  play u 0 "lightstave.auraofprotection" ~input:(Some 2) ~cost:4;
  let before = player_life u 1 in
  let goblin = life_at u 1 3 in
  play u 0 "lightstave.priestessoflight" ~input:(Some 5) ~cost:7;
  check_int "auras ended" 0 (List.length (auras u 0));
  check_int "six per aura to the opponent" (before - 12) (player_life u 1);
  check_int "six per aura to each creature" (goblin - 12) (life_at u 1 4)

let priestess_gains_light_power_on_a_death () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 1 "fire.goblin" 3;
  play u 0 "lightstave.priestessoflight" ~input:(Some 2) ~cost:7;
  let before = mana u 0 4 in
  kill u 1 3;
  check_int "light power" (before + 1) (mana u 0 4)

(* ------------------------------------------------------- Rainbow Dragon *)

let rainbow_dragon_pays_each_element () =
  let u = new_duel ~mana:ten ~special0:Lightstave.house ~special1:Fire.house () in
  place u 0 "fire.goblin" 1;
  place u 0 "water.sprite" 2;
  place u 0 "air.fairy" 3;
  place u 0 "earth.sprite" 4;
  let before i = mana u 0 i in
  let f, w, a, e = (before 0, before 1, before 2, before 3) in
  play u 0 "lightstave.rainbowdragon" ~input:(Some 5) ~cost:8;
  check_int "fire power" (f + 1) (mana u 0 0);
  check_int "water power" (w + 1) (mana u 0 1);
  check_int "air power" (a + 1) (mana u 0 2);
  check_int "earth power" (e + 1) (mana u 0 3)

let () =
  Alcotest.run "lightstave"
    [
      ( "lightstave",
        [
          Alcotest.test_case "aura is not a spell" `Quick aura_is_not_a_spell;
          Alcotest.test_case "life cast and next turns" `Quick life_heals_on_cast_and_each_later_turn;
          Alcotest.test_case "same-named aura" `Quick a_same_named_aura_never_stacks;
          Alcotest.test_case "aura name exclusive" `Quick aura_name_is_exclusive_across_the_table;
          Alcotest.test_case "auras target own creatures" `Quick auras_target_own_creatures_only;
          Alcotest.test_case "life expires" `Quick life_ticks_four_turns_then_dissipates;
          Alcotest.test_case "life target dies" `Quick life_ends_when_target_dies;
          Alcotest.test_case "protection reduces" `Quick protection_reduces_target_damage;
          Alcotest.test_case "protection redirects" `Quick protection_redirects_neighbour_damage;
          Alcotest.test_case "illumination" `Quick illumination_heals_own_and_burns_foes;
          Alcotest.test_case "prism lantern" `Quick prism_lantern_extends_every_aura;
          Alcotest.test_case "aurora" `Quick aurora_retaliates;
          Alcotest.test_case "crystalline palace" `Quick palace_attack_and_spell_retaliation;
          Alcotest.test_case "priestess mass damage" `Quick priestess_ends_every_aura_and_burns;
          Alcotest.test_case "priestess power" `Quick priestess_gains_light_power_on_a_death;
          Alcotest.test_case "rainbow dragon" `Quick rainbow_dragon_pays_each_element;
        ] );
    ]
