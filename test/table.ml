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

(* The duel dialog's AI roster: faces, levels and their bots *)
let duel_roster () =
  let opps = Table.duel_opponents in
  check_int "scala faces" 13 (List.length opps);
  check_bool "level order" true
    (let rec nondec = function
       | [] | [ _ ] -> true
       | a :: b :: rest -> a.Table.opp_level <= b.Table.opp_level && nondec (b :: rest)
     in
     nondec opps);
  List.iter
    (fun (o : Table.opponent) ->
      check_bool ("kind face for " ^ o.Table.opp_kind) true
        (List.mem o.Table.opp_portrait (Table.faces_for_bot o.Table.opp_kind));
      check_bool ("bot of " ^ o.Table.opp_portrait) true
        (Table.bot_of_portrait o.Table.opp_portrait = Some o.Table.opp_bot))
    opps;
  check_bool "lecturer not gambler" true (not (List.mem "gambler" (Table.faces_for_bot "lecturer")));
  check_bool "unknown face" true (Table.bot_of_portrait "ember" = None);
  check_bool "unknown bot" true (Table.faces_for_bot "nope" = Table.faces_for_bot "greedy");
  check_int "twelve player faces" 12 (List.length Table.player_portraits)

(* The settings classes tab groups special houses by author like Scala. *)
let house_groups () =
  let grouped = List.map (fun (h : house) -> h.h_key) (List.concat_map snd Table.house_groups) in
  let specials = List.map (fun (h : house) -> h.h_key) (Table.special_houses ()) in
  List.iter (fun h -> check_bool h true (List.mem h grouped)) specials;
  Alcotest.(check (option string)) "Troopers" (Some "olds") (Table.group_of_house "Troopers");
  Alcotest.(check (option string)) "zen" (Some "badsleep") (Table.group_of_house "zen");
  Alcotest.(check (option string)) "Wind" (Some "sinist") (Table.group_of_house "Wind");
  check_bool "Chronomaster present" true (List.exists (fun (a, _) -> a = "panderson") Table.house_groups)

(* The fight resolves each side's checked house set to one house. *)
let resolve_special () =
  check_bool "singleton" true (Table.resolve_special [ "zen" ] = "zen");
  let all = List.map (fun (h : house) -> h.h_key) (Table.special_houses ()) in
  for _ = 1 to 20 do
    check_bool "empty means all" true (List.mem (Table.resolve_special []) all);
    check_bool "unknown dropped" true (Table.resolve_special [ "nope"; "zen" ] = "zen")
  done

let () =
  Alcotest.run "table"
    [
      ( "table",
        [
          Alcotest.test_case "seed replays" `Quick seed_replays;
          Alcotest.test_case "human choices" `Quick human_choices;
          Alcotest.test_case "duel roster" `Quick duel_roster;
          Alcotest.test_case "house groups" `Quick house_groups;
          Alcotest.test_case "resolve special" `Quick resolve_special;
        ] );
    ]
