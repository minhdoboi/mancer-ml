(* Headless driver: shuffles two Troopers decks and plays them with the greedy
   bot.  Mirrors `Tournament --bots ... --games N` on the Scala side. *)

open Mancer
open Sp

let basics () = [ Fire.house; Water.house; Air.house; Earth.house ]

let init () =
  Game.init_cards Fire.house;
  Game.init_cards Water.house;
  Game.init_cards Air.house;
  Game.init_cards Earth.house;
  Game.init_cards ~cost_fn:Troopers.conscription_cost Troopers.house

let () =
  let verbose = ref false and seed = ref None in
  Arg.parse
    [
      ("-v", Arg.Set verbose, " play one game with an event trace");
      ("--seed", Arg.Int (fun n -> seed := Some n), "N replay the deals of a previous run");
    ]
    (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
    "main.exe [-v] [--seed N]";
  let verbose = !verbose in
  let games = if verbose then 1 else 20 in
  let seed = match !seed with Some n -> n | None -> Random.State.bits (Random.State.make_self_init ()) in
  Printf.printf "seed %d\n" seed;
  let rng = Random.State.make [| seed |] in
  init ();
  let wins = ref 0 and losses = ref 0 and draws = ref 0 in
  for i = 1 to games do
    let p1, p2 = Shuffle.two_players ~rng ~specials:(Troopers.house, Troopers.house) (basics ()) in
    let desc = { gd_players = [| p1.desc; p2.desc |] } in
    let r =
      Game.play_game desc ~mana0:(Array.to_list p1.mana) ~mana1:(Array.to_list p2.mana) ~max_turns:400
        ~verbose
    in
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
