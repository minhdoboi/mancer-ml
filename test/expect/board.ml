(* Board snapshots after scripted plays.  Each block prints both players
   (fire/water/air/earth/special mana, life, slots as name/life) so any change
   in the rules shows up as a diff.  Run `dune test`, review, `dune promote`. *)

open Mancer
open Sp
open Engine
open Support

let%expect_test "apocalypse clears both boards" =
  let u = new_game ~mana0:11 () in
  place u 0 "anger.werewolf" 0;
  place u 0 "anger.dragon" 2;
  place u 1 "anger.gorilla" 1;
  step u ignore;
  print_board u;
  [%expect
    {|
    houses 11,4,4,4,2, life 60, slots anger.werewolf/16,.,anger.dragon/41,.,.,.
    houses 11,4,4,4,2, life 60, slots .,anger.gorilla/20,.,.,.,.
    |}];
  play u 0 "anger.apocalypse" ~input:None ~cost:11;
  print_board u;
  [%expect
    {|
    houses 0,4,4,4,2, life 60, slots .,.,anger.dragon/12,.,.,.
    houses 11,4,4,4,2, life 31, slots .,.,.,.,.,.
    |}]

let%expect_test "orc and bull bonuses come and go" =
  let u = new_game () in
  place u 0 "anger.werewolf" 0;
  place u 0 "anger.chainbreaker" 1;
  place u 0 "anger.gorilla" 3;
  place u 0 "anger.hotspot" 4;
  step u ignore;
  let attacks () =
    List.init nb_slots (fun n -> string_of_int (attack_at u 0 n)) |> String.concat " " |> print_endline
  in
  attacks ();
  [%expect {| 7 4 -1 6 0 -1 |}];
  step u (fun () -> Slot.do_priv_destroy (slot u 0 1));
  attacks ();
  [%expect {| 5 -1 -1 6 0 -1 |}]

let%expect_test "one full turn of combat" =
  let u = new_game () in
  place u 0 "anger.dragon" 0;
  place u 0 "anger.werewolf" 1;
  place u 1 "anger.hotspot" 0;
  place u 1 "anger.oni" 1;
  step u ignore;
  print_board u;
  [%expect
    {|
    houses 6,4,4,4,2, life 60, slots anger.dragon/41,anger.werewolf/16,.,.,.,.
    houses 6,4,4,4,2, life 60, slots anger.hotspot/5,anger.oni/19,.,.,.,.
    |}];
  step u (fun () -> Player.run_slots (player u 0));
  step u (fun () -> Player.prepare_next_turn (player u 1));
  step u (fun () -> Player.apply_effects (player u 1) On_turn);
  print_board u;
  [%expect
    {|
    houses 6,4,4,4,2, life 60, slots anger.dragon/41,anger.werewolf/16,.,.,.,.
    houses 7,5,5,5,3, life 60, slots .,anger.oni/15,.,.,.,.
    |}]

let%expect_test "headless game trace" =
  Game.init_cards Anger.house;
  let d = Game.make_player_desc Anger.house in
  let r = Session.play_game { gd_players = [| d; d |] } ~max_turns:6 ~verbose:true in
  Printf.printf "winner %s after %d turns\n"
    (match r.winner with Some p -> string_of_int p | None -> "none")
    r.turns;
  [%expect
    {|
    summon 0 anger.werewolf
    summon 0 anger.werewolf
    summon 1 anger.werewolf
    summon 1 anger.werewolf
    focus 0
    focus 1
    summon 2 anger.werewolf
    focus 0
    focus 1
    summon 2 anger.werewolf
    focus 0
    focus 1
    focus 2
    winner none after 6 turns
    |}]
