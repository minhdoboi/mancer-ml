(* Table: seats, seeded deals and the per-card choices a UI offers. *)

open Mancer
open Sp

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let greedy = Table.Bot "greedy"

let rec play_out t =
  match Table.to_move t with
  | None -> ()
  | Some _ ->
      Table.play_bot t;
      play_out t

(* A fixed seed deals and plays the same game *)
let seed_replays () =
  let setup =
    { Table.default_setup with seats = (greedy, greedy); specials = ("zen", "wind"); seed = Some 3 }
  in
  let game () =
    let t = Table.start setup in
    play_out t;
    Table.record t
  in
  let a = game () and b = game () in
  check_int "seed" 3 (Table.seed (Table.start setup));
  check_int "turns" a.turns b.turns;
  check_bool "moves" true (a.moves = b.moves);
  check_bool "winner" true (a.winner_player = b.winner_player);
  check_bool "specials" true (a.special_names = ("zen", "wind"))

(* A human seat is played through choices; play_bot refuses it *)
let human_choices () =
  let t = Table.start { Table.default_setup with seats = (Table.Human, greedy); seed = Some 1 } in
  Alcotest.check_raises "human to move" (Invalid_argument "Table.bot_command: a human is to move") (fun () ->
      Table.play_bot t);
  let playable =
    Table.hand t owner
    |> List.concat_map (fun h -> Array.to_list h.phd_cards |> List.filter (fun cd -> Table.playable t cd h))
  in
  let cd = List.find (fun cd -> cd.cd_card.input_spec = Select_owner_slot) playable in
  let cmds = Table.choices t cd.cd_card in
  check_int "one per open slot" nb_slots (List.length cmds);
  List.iter
    (fun c ->
      check_bool "same card" true (c.cmd_card.key = cd.cd_card.key);
      check_bool "own side" true (Table.target_side c = Some owner))
    cmds;
  Table.submit t (Some (List.hd cmds));
  (* the greedy bot is to move now *)
  check_bool "bot to move" true (Table.to_move t = Some opponent);
  Table.play_bot t;
  check_bool "human again" true (Table.to_move t = Some owner)

let () =
  Alcotest.run "table"
    [
      ( "table",
        [
          Alcotest.test_case "seed replays" `Quick seed_replays;
          Alcotest.test_case "human choices" `Quick human_choices;
        ] );
    ]
