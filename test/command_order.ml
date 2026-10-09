(* Grouping and round-robin ordering of legal commands. *)

open Mancer

let cmd key input =
  {
    Sp.cmd_player = 0;
    cmd_card = Support.card_by_key key;
    cmd_input = Some input;
    cmd_cost = 0;
    cmd_flag = None;
  }

let label (c : Sp.command) = Printf.sprintf "%s@%d" c.cmd_card.name (Option.get c.cmd_input)

let a0 = cmd "fire.blargl" 0
and a1 = cmd "fire.blargl" 1
and a2 = cmd "fire.blargl" 2
and b0 = cmd "water.sprite" 0
and b1 = cmd "water.sprite" 1

let groups () =
  let gs = Game.group_by_card [ a0; b0; a1; b1; a2 ] in
  Alcotest.(check (list (list string)))
    "first-seen order, command order kept"
    [ List.map label [ a0; a1; a2 ]; List.map label [ b0; b1 ] ]
    (List.map (List.map label) gs)

let interleave () =
  Alcotest.(check (list string))
    "round robin"
    (List.map label [ a0; b0; a1; b1; a2 ])
    (List.map label (Game.interleave (Game.group_by_card [ a0; a1; a2; b0; b1 ])));
  Alcotest.(check (list string)) "empty" [] (List.map label (Game.interleave []))

let () =
  Alcotest.run "command_order"
    [
      ( "order",
        [ Alcotest.test_case "groups" `Quick groups; Alcotest.test_case "interleave" `Quick interleave ] );
    ]
