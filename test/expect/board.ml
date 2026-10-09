(* Board snapshots after scripted plays.  Each block prints both players
   (fire/water/air/earth/special mana, life, slots as name/life) so any change
   in the rules shows up as a diff.  Run `dune test`, review, `dune promote`. *)

open Mancer
open Sp
open Engine
open Support

let%expect_test "apocalypse clears both boards" =
  let u = new_game ~mana0:11 () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.dragon" 2;
  place u 1 "fire.bull" 1;
  step u ignore;
  print_board u;
  [%expect
    {|
    houses 11,4,4,4,2, life 60, slots fire.goblin/16,.,fire.dragon/41,.,.,.
    houses 11,4,4,4,2, life 60, slots .,fire.bull/20,.,.,.,.
    |}];
  play u 0 "fire.apocalypse" ~input:None ~cost:11;
  print_board u;
  [%expect
    {|
    houses 0,4,4,4,2, life 60, slots .,.,fire.dragon/12,.,.,.
    houses 11,4,4,4,2, life 31, slots .,.,.,.,.,.
    |}]

let%expect_test "orc and bull bonuses come and go" =
  let u = new_game () in
  place u 0 "fire.goblin" 0;
  place u 0 "fire.orc" 1;
  place u 0 "fire.bull" 3;
  place u 0 "fire.wall" 4;
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
  place u 0 "fire.dragon" 0;
  place u 0 "fire.goblin" 1;
  place u 1 "fire.wall" 0;
  place u 1 "fire.drake" 1;
  step u ignore;
  print_board u;
  [%expect
    {|
    houses 6,4,4,4,2, life 60, slots fire.dragon/41,fire.goblin/16,.,.,.,.
    houses 6,4,4,4,2, life 60, slots fire.wall/5,fire.drake/18,.,.,.,.
    |}];
  step u (fun () -> Player.run_slots (player u 0));
  step u (fun () -> Player.prepare_next_turn (player u 1));
  step u (fun () -> Player.apply_effects (player u 1) On_turn);
  print_board u;
  [%expect
    {|
    houses 6,4,4,4,2, life 60, slots fire.dragon/41,fire.goblin/16,.,.,.,.
    houses 7,5,5,5,3, life 60, slots .,fire.drake/14,.,.,.,.
    |}]

let%expect_test "headless game trace" =
  Game.init_cards Fire.house;
  let d = Game.make_player_desc Fire.house in
  let r = Game.play_game { gd_players = [| d; d |] } ~max_turns:6 ~verbose:true in
  Printf.printf "winner %s after %d turns\n"
    (match r.winner with Some p -> string_of_int p | None -> "none")
    r.turns;
  [%expect
    {|
    summon 0 fire.goblin
    summon 0 fire.goblin
    summon 1 fire.goblin
    summon 1 fire.goblin
    summon 2 fire.goblin
    summon 2 fire.goblin
    winner none after 6 turns
    |}]
