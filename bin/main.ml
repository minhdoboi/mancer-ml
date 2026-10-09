(* Headless driver: plays Fire-vs-Fire games and prints the outcome.
   Mirrors what `Tournament --bots ... --games N` does on the Scala side. *)

open Mancer
open Sp

let () =
  let verbose = Array.length Sys.argv > 1 && Sys.argv.(1) = "-v" in
  let games = if verbose then 1 else 20 in
  Game.init_cards Fire.house;
  Game.init_cards Water.house;
  Game.init_cards Air.house;
  Game.init_cards Earth.house;
  Game.init_cards ~cost_fn:Troopers.conscription_cost Troopers.house;
  let desc = { gd_players = [| Game.make_player_desc Fire.house; Game.make_player_desc Fire.house |] } in
  let wins = ref 0 and losses = ref 0 and draws = ref 0 in
  for i = 1 to games do
    let r = Game.play_game desc ~max_turns:400 ~verbose in
    let outcome =
      match r.winner with
      | Some 0 ->
          incr wins;
          "player 0"
      | Some _ ->
          incr losses;
          "player 1"
      | None ->
          incr draws;
          "draw"
    in
    Printf.printf "game %d: %s after %d turns\n" i outcome r.turns
  done;
  Printf.printf "\np0 %d / p1 %d / draws %d over %d games\n" !wins !losses !draws games
