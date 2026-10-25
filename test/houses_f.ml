(* Mechanics for Soul Reaper, Sower, Shaman and Mountain King. *)

open Mancer
open Sp
open Engine
open Common
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit ?(is_ability = false) ?(is_spell = false) ?card u pid num amount =
  let c = match card with Some k -> Some (card_by_key k) | None -> None in
  step u (fun () ->
      Slot.inflict (slot u pid num)
        (mk_damage ~is_ability ~is_spell amount { (ctx (1 - pid)) with ctx_card = c }))

let kill u pid num = hit u pid num 1000
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let on_start u pid = step u (fun () -> Player.apply_effects (player u pid) On_start)
let settle u = step u (fun () -> ())
let ten = [ 10; 10; 10; 10; 10 ]
let flagged u pid num flag = has_flag (Option.get (slot u pid num).value) flag

let run_at u pid num =
  step u (fun () -> Option.iter (fun st -> Player.run_slot (player u pid) num st) (slot u pid num).value)

let cost_of u pid key =
  let pd = (player u pid).pstate.ps_desc in
  Array.fold_left
    (fun acc h ->
      let cards = List.fold_left (fun cs m -> m h cs) h.phd_cards pd.desc_mods in
      match Array.find_opt (fun cd -> cd.cd_card.key = key) cards with Some cd -> cd.cd_cost | None -> acc)
    (-1) pd.init_desc.pd_houses

let souls u pid = match (player u pid).pstate.ps_data with Reaper_data x -> x | _ -> 0
let slot_data u pid num = Option.map (fun st -> st.data) (slot u pid num).value

(* ----------------------------------------------------------- soul reaper *)

let kill_gains_soul () =
  let u = new_game ~mana:ten ~special:Soulreaper.house () in
  place u 1 "fire.goblin" 2;
  kill u 1 2;
  check_int "souls" 1 (souls u 0)

let damnation_bonus () =
  let u = new_game ~mana:ten ~special:Soulreaper.house () in
  place u 1 "fire.goblin" 2;
  hit u 1 2 14;
  play u 0 "reaper.damnation" ~input:(Some 2) ~cost:1;
  check_int "goblin dead" (-1) (life_at u 1 2);
  check_int "souls 1+cost" 2 (souls u 0)

let onslaught_skips_souls () =
  let u = new_game ~mana:ten ~special:Soulreaper.house () in
  place u 1 "fire.goblin" 1;
  kill u 1 1;
  check_int "seed" 1 (souls u 0);
  place u 1 "fire.goblin" 2;
  hit u 1 2 15;
  play u 0 "reaper.onslaught" ~input:None ~cost:4;
  check_int "reset" 0 (souls u 0);
  check_int "goblin dead" (-1) (life_at u 1 2)

let eternal_hangs () =
  let u = new_game ~mana:ten ~special:Soulreaper.house () in
  place u 0 "fire.goblin" 2;
  play u 0 "reaper.rage" ~input:(Some 2) ~cost:3;
  kill u 0 2;
  check_string "still there" "fire.goblin" (Option.get (occupant u 0 2));
  check_int "zero" 0 (life_at u 0 2);
  on_end u 0;
  check_bool "hangs" true (Option.is_some (occupant u 0 2));
  on_end u 0;
  check_bool "gone" true (is_empty u 0 2)

let shackles_bridle () =
  let u = new_game ~mana:ten ~special:Soulreaper.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "reaper.shackles" ~input:(Some 2) ~cost:8;
  check_int "attack" 0 (attack_at u 1 2);
  check_bool "blocked" false (List.mem 2 (player u 1).pstate.ps_slot_list);
  on_turn u 0;
  check_int "soul" 1 (souls u 0)

let sentence_reaps () =
  let u = new_game ~mana:ten ~special:Soulreaper.house () in
  place u 0 "fire.goblin" 1;
  place u 1 "fire.goblin" 2;
  play u 0 "reaper.sentence" ~input:None ~cost:7;
  check_bool "own gone" true (is_empty u 0 1);
  check_bool "opp gone" true (is_empty u 1 2);
  check_int "spell+listener" 4 (souls u 0)

let passage_heals () =
  let u = new_game ~mana:ten ~special:Soulreaper.house () in
  place u 0 "fire.goblin" 1;
  place u 0 "fire.goblin" 2;
  hit u 0 1 10;
  hit u 0 2 10;
  place u 1 "fire.goblin" 0;
  kill u 1 0;
  play u 0 "reaper.passage" ~input:(Some 2) ~cost:2;
  check_int "selected" 7 (life_at u 0 2);
  check_int "adj" 7 (life_at u 0 1)

let furious_spends () =
  let u = new_game ~mana:ten ~special:Soulreaper.house () in
  place u 1 "fire.goblin" 2;
  kill u 1 2;
  play u 0 "reaper.furious" ~input:None ~cost:6;
  check_int "player" 52 (player_life u 1);
  check_int "spent" 0 (souls u 0)

let tribute_eats () =
  let u = new_game ~mana:ten ~special:Soulreaper.house () in
  place u 0 "fire.goblin" 2;
  step u (fun () -> Player.inflict (player u 0) (mk_damage 20 (ctx 1)));
  play u 0 "reaper.tribute" ~input:(Some 2) ~cost:5;
  check_int "healed" 56 (player_life u 0);
  check_bool "gone" true (is_empty u 0 2)

(* ----------------------------------------------------------------- sower *)

let tangling_trades () =
  let u = new_game ~mana:ten ~special:Sower.house () in
  place u 0 "fire.goblin" 2;
  place u 1 "fire.goblin" 2;
  hit u 0 2 8;
  play u 0 "sower.tangling" ~input:(Some 2) ~cost:1;
  check_int "opp" 12 (life_at u 1 2);
  check_int "own" 12 (life_at u 0 2)

let devour_doubles () =
  let u = new_game ~mana:ten ~special:Sower.house () in
  place u 0 "fire.goblin" 1;
  place u 0 "fire.goblin" 2;
  play u 0 "sower.devouring" ~input:(Some 1) ~cost:2;
  check_bool "eaten" true (is_empty u 0 1);
  check_int "doubled" 8 (attack_at u 0 2);
  on_end u 0;
  check_int "faded" 4 (attack_at u 0 2)

let pollinate_monster () =
  let u = new_game ~mana:ten ~special:Sower.house () in
  place u 0 "sower.predator" 2;
  play u 0 "sower.pollination" ~input:(Some 2) ~cost:4;
  check_string "monster" "sower.monster" (Option.get (occupant u 0 2));
  check_int "healed" 66 (player_life u 0)

let flower_burns () =
  let u = new_game ~mana:ten ~special:Sower.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "sower.flower" ~input:(Some 0) ~cost:8;
  check_int "direct" 50 (player_life u 1);
  on_turn u 0;
  check_int "half" 8 (life_at u 1 2);
  check_int "fire" 11 (mana u 0 0)

(* ---------------------------------------------------------------- shaman *)

let wolf_starts () =
  let u = new_game ~mana:ten ~special:Shaman.house () in
  on_start u 0;
  check_string "wolf" "shaman.ghost" (Option.get (occupant u 0 4));
  check_int "atk" 2 (attack_at u 0 4)

let hunger_buffs () =
  let u = new_game ~mana:ten ~special:Shaman.house () in
  on_start u 0;
  place u 0 "fire.goblin" 1;
  play u 0 "shaman.hunger" ~input:None ~cost:1;
  check_int "wolf" 6 (attack_at u 0 4);
  on_end u 0;
  check_int "faded" 2 (attack_at u 0 4)

let moon_heals () =
  let u = new_game ~mana:ten ~special:Shaman.house () in
  on_start u 0;
  place u 0 "fire.goblin" 3;
  hit u 0 3 10;
  play u 0 "shaman.moon" ~input:None ~cost:3;
  check_int "adj" 14 (life_at u 0 3);
  check_int "player" 68 (player_life u 0)

let fury_pokes () =
  let u = new_game ~mana:ten ~special:Shaman.house () in
  on_start u 0;
  place u 1 "fire.goblin" 2;
  play u 0 "shaman.fury" ~input:None ~cost:4;
  check_int "goblin" 9 (life_at u 1 2);
  kill u 1 2;
  settle u;
  check_int "wolf bonus" 3 (attack_at u 0 4)

let mate_cheapens () =
  let u = new_game ~mana:ten ~special:Shaman.house () in
  play u 0 "shaman.mate" ~input:(Some 2) ~cost:8;
  check_int "player" 52 (player_life u 1);
  check_int "hunger" 0 (cost_of u 0 "shaman.hunger")

(* ---------------------------------------------------------- mountain king *)

let hird_forms () =
  let u = new_game ~mana:ten ~special:Mountainking.house () in
  play u 0 "mountainking.DwarvenSoldier.name" ~input:(Some 2) ~cost:1;
  check_bool "alone" true (slot_data u 0 2 <> Some Hird);
  play u 0 "mountainking.DwarvenSoldier.name" ~input:(Some 3) ~cost:1;
  check_bool "left" true (slot_data u 0 2 = Some Hird);
  check_bool "right" true (slot_data u 0 3 = Some Hird)

let ballista_shots () =
  let u = new_game ~mana:ten ~special:Mountainking.house () in
  play u 0 "mountainking.Ballista.name" ~input:(Some 1) ~cost:6;
  place u 1 "fire.goblin" 3;
  check_int "half" 8 (life_at u 1 3);
  check_int "self" 30 (life_at u 0 1)

let crossbow_hits_both () =
  let u = new_game ~mana:ten ~special:Mountainking.house () in
  place u 0 "mountainking.DwarvenCrossbowman.name" 2;
  place u 1 "fire.goblin" 2;
  run_at u 0 2;
  check_int "player" 57 (player_life u 1);
  check_int "goblin" 13 (life_at u 1 2)

let mountain_stuns () =
  let u = new_game ~mana:ten ~special:Mountainking.house () in
  place u 1 "fire.goblin" 1;
  place u 1 "fire.goblin" 4;
  play u 0 "mountainking.MountainKingCard.name" ~input:(Some 2) ~cost:8;
  check_bool "stun" true (flagged u 1 1 stun_flag || flagged u 1 4 stun_flag)

let soldier_lowers () =
  let u = new_game ~mana:ten ~special:Mountainking.house () in
  play u 0 "mountainking.DwarvenSoldier.name" ~input:(Some 2) ~cost:1;
  play u 0 "mountainking.DwarvenSoldier.name" ~input:(Some 3) ~cost:1;
  place u 1 "fire.goblin" 2;
  settle u;
  check_int "lowered" 2 (attack_at u 1 2)

let () =
  Alcotest.run "houses_f"
    [
      ( "soulreaper",
        [
          Alcotest.test_case "kill soul" `Quick kill_gains_soul;
          Alcotest.test_case "damnation" `Quick damnation_bonus;
          Alcotest.test_case "onslaught" `Quick onslaught_skips_souls;
          Alcotest.test_case "eternal" `Quick eternal_hangs;
          Alcotest.test_case "shackles" `Quick shackles_bridle;
          Alcotest.test_case "sentence" `Quick sentence_reaps;
          Alcotest.test_case "passage" `Quick passage_heals;
          Alcotest.test_case "furious" `Quick furious_spends;
          Alcotest.test_case "tribute" `Quick tribute_eats;
        ] );
      ( "sower",
        [
          Alcotest.test_case "tangling" `Quick tangling_trades;
          Alcotest.test_case "devour" `Quick devour_doubles;
          Alcotest.test_case "pollinate" `Quick pollinate_monster;
          Alcotest.test_case "flower" `Quick flower_burns;
        ] );
      ( "shaman",
        [
          Alcotest.test_case "wolf start" `Quick wolf_starts;
          Alcotest.test_case "hunger" `Quick hunger_buffs;
          Alcotest.test_case "moon" `Quick moon_heals;
          Alcotest.test_case "fury" `Quick fury_pokes;
          Alcotest.test_case "mate" `Quick mate_cheapens;
        ] );
      ( "mountainking",
        [
          Alcotest.test_case "hird" `Quick hird_forms;
          Alcotest.test_case "ballista" `Quick ballista_shots;
          Alcotest.test_case "crossbow" `Quick crossbow_hits_both;
          Alcotest.test_case "stun" `Quick mountain_stuns;
          Alcotest.test_case "soldier lower" `Quick soldier_lowers;
        ] );
    ]
