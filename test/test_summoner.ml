(* Search sanity for Summoner. *)

open Mancer
open Sp
open Engine
open Support

let check_bool = Alcotest.(check bool)

let card_descs (h : house) =
  h.h_cards
  |> List.map (fun c -> { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 })
  |> Array.of_list

let empty mana =
  init_all_houses ();
  let d =
    {
      pd_houses =
        [|
          { phd_house = Fire.house; phd_cards = card_descs Fire.house };
          { phd_house = Water.house; phd_cards = card_descs Water.house };
          { phd_house = Air.house; phd_cards = card_descs Air.house };
          { phd_house = Earth.house; phd_cards = card_descs Earth.house };
          { phd_house = Troopers.house; phd_cards = card_descs Troopers.house };
        |];
    }
  in
  let g = { gd_players = [| d; d |] } in
  let p () = Game.initial_player_state (List.init 5 (fun _ -> mana)) d in
  (g, { players = [| p (); p () |] })

let settings budget =
  let s = Summoner.default_settings () in
  s.duration <- budget;
  s.logging <- false;
  s.stats_log <- false;
  s

let is_legal g st pid (c : command) =
  c.cmd_player = pid && List.exists (fun x -> x.cmd_card.id = c.cmd_card.id) (Game.legal_commands pid g st)

let legal_on_fresh () =
  let g, st = empty 5 in
  let cmd = Summoner.execute (settings 300) ~pid:1 ~desc:g st in
  Option.iter (fun c -> check_bool "legal" true (is_legal g st 1 c)) cmd

let pass_when_broke () =
  init_all_houses ();
  let d = Game.make_player_desc Fire.house in
  let g = { gd_players = [| d; d |] } in
  let p () = Game.initial_player_state [ 0; 0; 0; 0; 0 ] d in
  let st = { players = [| p (); p () |] } in
  Alcotest.(check (option string))
    "pass" None
    (Summoner.execute (settings 100) ~pid:0 ~desc:g st |> Option.map (fun c -> c.cmd_card.key))

let takes_lethal () =
  let g, st = empty 8 in
  let u = Updater.create st g in
  place u 0 "fire.drake" 2;
  (player u 1).pstate.ps_life <- 3;
  let board = Updater.export u in
  let cmd = Summoner.execute (settings 300) ~pid:0 ~desc:g board in
  let s = Game.simulate g board 0 cmd in
  check_bool "wins" true (s.ended = Some 0)

let search_does_not_touch_input () =
  let g, st = empty 6 in
  let u = Updater.create st g in
  place u 0 "fire.goblin" 0;
  let snap = Updater.export u in
  let before = state_to_string snap in
  ignore (Summoner.execute (settings 150) ~pid:0 ~desc:g snap);
  Alcotest.(check string) "state unchanged" before (state_to_string snap)

let tournament_id () =
  let _ : Game.mover = Tournament.mover ~stats_log:false ~budget:50 "summoner" in
  ()

let () =
  let open Alcotest in
  run "summoner"
    [
      ( "search",
        [
          test_case "legal move" `Quick legal_on_fresh;
          test_case "pass when broke" `Quick pass_when_broke;
          test_case "lethal" `Quick takes_lethal;
          test_case "input untouched" `Quick search_does_not_touch_input;
          test_case "tournament id" `Quick tournament_id;
        ] );
    ]
