(* Search sanity for the Lecturer (UCT) bot. *)

open Mancer
open Sp
open Engine
open Support

let check_bool = Alcotest.(check bool)

let card_descs (h : house) =
  h.h_cards
  |> List.map (fun c -> { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 })
  |> Array.of_list

let full_desc special =
  {
    pd_houses =
      [|
        { phd_house = Fire.house; phd_cards = card_descs Fire.house };
        { phd_house = Water.house; phd_cards = card_descs Water.house };
        { phd_house = Air.house; phd_cards = card_descs Air.house };
        { phd_house = Earth.house; phd_cards = card_descs Earth.house };
        { phd_house = special; phd_cards = card_descs special };
      |];
  }

let empty mana =
  init_all_houses ();
  let d = full_desc Troopers.house in
  let g = { gd_players = [| d; d |] } in
  let p () = Game.initial_player_state (List.init 5 (fun _ -> mana)) d in
  (g, { players = [| p (); p () |] })

(* A deck of two fire creatures and nothing else: no spell to fall back on,
   so the only way to survive the turn is to block. *)
let two_creatures_desc () =
  let pick k = List.find (fun (c : card) -> c.key = k) Fire.house.h_cards in
  let descs =
    [ pick "fire.goblin"; pick "fire.drake" ]
    |> List.map (fun c -> { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 })
    |> Array.of_list
  in
  {
    pd_houses =
      [|
        { phd_house = Fire.house; phd_cards = descs };
        { phd_house = Water.house; phd_cards = [||] };
        { phd_house = Air.house; phd_cards = [||] };
        { phd_house = Earth.house; phd_cards = [||] };
        { phd_house = Troopers.house; phd_cards = [||] };
      |];
  }

let settings budget =
  let s = Lecturer.default_settings () in
  s.duration <- budget;
  s.rng <- Random.State.make [| 42 |];
  s

let is_legal g st pid (c : command) =
  c.cmd_player = pid && List.exists (fun x -> x.cmd_card.id = c.cmd_card.id) (Game.legal_commands pid g st)

let legal_on_fresh () =
  let g, st = empty 5 in
  let cmd = Lecturer.execute (settings 300) ~pid:1 ~desc:g st in
  Option.iter (fun c -> check_bool "legal" true (is_legal g st 1 c)) cmd

let pass_when_broke () =
  init_all_houses ();
  let d = Game.make_player_desc Fire.house in
  let g = { gd_players = [| d; d |] } in
  let p () = Game.initial_player_state [ 0; 0; 0; 0; 0 ] d in
  let st = { players = [| p (); p () |] } in
  Alcotest.(check (option string))
    "pass" None
    (Lecturer.execute (settings 100) ~pid:0 ~desc:g st |> Option.map (fun c -> c.cmd_card.key))

let takes_lethal () =
  (* A drake on board, opponent at 3: any move wins, including pass. The bot
     must pick a move that ends the game in its favour. *)
  let g, st = empty 8 in
  let u = Updater.create st g in
  place u 0 "fire.drake" 2;
  (player u 1).pstate.ps_life <- 3;
  let board = Updater.export u in
  let cmd = Lecturer.execute (settings 300) ~pid:0 ~desc:g board in
  let s = Game.simulate g board 0 cmd in
  check_bool "wins" true (s.ended = Some 0)

let root_covers_every_move () =
  let g, st = empty 6 in
  let root = Lecturer.search (settings 200) ~pid:0 ~desc:g st in
  let kids = Lecturer.root_children root in
  Alcotest.(check int)
    "one child per legal move + pass"
    (1 + List.length (Game.legal_commands 0 g st))
    (List.length kids);
  check_bool "values in [-1, 1]" true (List.for_all (fun (_, _, v) -> v >= -1.0 && v <= 1.0) kids)

let search_does_not_touch_input () =
  let g, st = empty 6 in
  let u = Updater.create st g in
  place u 0 "fire.goblin" 0;
  let snap = Updater.export u in
  let before = state_to_string snap in
  ignore (Lecturer.execute (settings 150) ~pid:0 ~desc:g snap);
  Alcotest.(check string) "state unchanged" before (state_to_string snap)

(* The opponent has a blargl (8 attack) in slot 3 and a goblin behind it; the
   bot is at 8 life, so the blargl kills it.  The bot's deck holds two
   creatures and no spell, so the only way to survive the turn is to block:
   a creature in slot 3, in front of the blargl.  The block does not save the
   bot — the goblin hits it and the blargl finishes it the turn after — but
   the bot has to try: it is the move that keeps it alive this turn, and a
   bot that stops trying is a bot that gives up.

   The score alone cannot rank these lines.  The game is decided (the bot
   dies either way), so every leaf says the same thing and the root children
   tie; the search then falls back on the visit count, which is noise, and
   plays an arbitrary move — usually a creature dropped in a free slot, and
   the bot dies.  The life margin is what separates the lines the score
   cannot, so the block has to come out of every search. *)
let blocks_the_lethal_attacker () =
  init_all_houses ();
  let d = two_creatures_desc () in
  let g = { gd_players = [| d; d |] } in
  let p () = Game.initial_player_state (List.init 5 (fun _ -> 5)) d in
  let st = { players = [| p (); p () |] } in
  let u = Updater.create st g in
  place u 1 "fire.blargl" 3;
  place u 1 "fire.goblin" 4;
  let board = Updater.export u in
  board.players.(0).ps_life <- 8;
  board.players.(1).ps_life <- 60;
  List.iter
    (fun seed ->
      let s = Lecturer.default_settings () in
      s.duration <- 500;
      s.rng <- Random.State.make [| seed |];
      match Lecturer.execute s ~pid:0 ~desc:g board with
      | Some c -> Alcotest.(check int) (Printf.sprintf "blocks slot 3 (seed %d)" seed) 3 (Option.value ~default:(-1) c.cmd_input)
      | None -> Alcotest.(check bool) (Printf.sprintf "plays a card (seed %d)" seed) false true)
    [ 1; 2; 3 ]

let () =
  let open Alcotest in
  run "lecturer"
    [
      ( "search",
        [
          test_case "legal move" `Quick legal_on_fresh;
          test_case "pass when broke" `Quick pass_when_broke;
          test_case "lethal" `Quick takes_lethal;
          test_case "root covers every move" `Quick root_covers_every_move;
          test_case "input untouched" `Quick search_does_not_touch_input;
          test_case "blocks the lethal attacker" `Quick blocks_the_lethal_attacker;
        ] );
    ]
