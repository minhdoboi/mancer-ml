(* Preference checks and search sanity for the Oracle bot.  Mirrors
   OracleTest.scala / OracleEvalTest.scala on the houses we have. *)

open Mancer
open Sp
open Engine
open Support

let check_float = Alcotest.(check (float 1e-6))
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

let desc () =
  init_all_houses ();
  let d = full_desc Troopers.house in
  (d, { gd_players = [| d; d |] })

let empty mana =
  let d, g = desc () in
  let p () = Game.initial_player_state (List.init 5 (fun _ -> mana)) d in
  (g, { players = [| p (); p () |] })

let board mana placed =
  let g, st = empty mana in
  let u = Updater.create st g in
  List.iter (fun (pid, num, key) -> place u pid key num) placed;
  (g, Updater.export u)

let v g st = Oracle.leaf_value ~desc:g st

let settings budget =
  let s = Oracle.default_settings () in
  s.duration <- budget;
  s.logging <- false;
  s.stats_log <- false;
  s

let is_legal g st pid (c : command) =
  c.cmd_player = pid && List.exists (fun x -> x.cmd_card.id = c.cmd_card.id) (Game.legal_commands pid g st)

(* ------------------------------------------------------------------ eval *)

let symmetric () =
  let g, st = empty 5 in
  check_float "empty" 0.0 (v g st);
  let g, st = board 5 [ (0, 2, "fire.bull"); (1, 3, "fire.bull") ] in
  check_float "mirror bulls" 0.0 (v g st)

let sprites_hurt_more_than_bulls () =
  let walls = List.init 5 (fun n -> (1, n, "water.guard")) in
  let g, bulls = board 5 (List.init 5 (fun n -> (0, n, "fire.bull")) @ walls) in
  let _, sprites = board 5 (List.init 5 (fun n -> (0, n, "water.sprite")) @ walls) in
  check_bool "sprites read below bulls" true (v g sprites < v g bulls -. 0.1)

let block_titan () =
  let g, blocking = board 5 [ (1, 3, "air.titan"); (0, 3, "fire.bull") ] in
  let _, open_ = board 5 [ (1, 3, "air.titan"); (0, 0, "fire.bull") ] in
  check_bool "block a harder hitter" true (v g blocking > v g open_)

let race_spider () =
  let g, blocking = board 5 [ (1, 3, "earth.hugespider"); (0, 3, "fire.bull") ] in
  let _, open_ = board 5 [ (1, 3, "earth.hugespider"); (0, 0, "fire.bull") ] in
  check_bool "race when the race is favourable" true (v g open_ > v g blocking)

let bigger_creature () =
  let g, big = board 5 [ (0, 2, "fire.bull") ] in
  let _, small = board 5 [ (0, 2, "fire.goblin") ] in
  check_bool "bull > goblin" true (v g big > v g small)

let play_the_bull () =
  let g, held = empty 8 in
  let g, played =
    let _, st = empty 8 in
    let u = Updater.create st g in
    place u 0 "fire.bull" 2;
    let bull = card_by_key "fire.bull" in
    Houses.incr_mana (player u 0).houses_update (-bull.cost) [ bull.house_index ];
    (g, Updater.export u)
  in
  check_bool "played bull beats the mana" true (v g played > v g held)

let fear_wide () =
  let g, wide =
    board 5
      [
        (1, 0, "fire.goblin");
        (1, 1, "fire.goblin");
        (1, 2, "earth.hugespider");
        (1, 3, "fire.goblin");
        (1, 4, "fire.goblin");
        (0, 2, "fire.bull");
      ]
  in
  check_bool "wide enemy board is scary" true (v g wide < -0.3)

(* ------------------------------------------------------------------ search *)

let legal_on_fresh () =
  let g, st = empty 5 in
  let cmd = Oracle.execute (settings 400) ~pid:1 ~desc:g st in
  Option.iter (fun c -> check_bool "legal" true (is_legal g st 1 c)) cmd

let pass_when_broke () =
  (* Conscription costs 0, so a Troopers hand is never fully broke. Fire-only is. *)
  init_all_houses ();
  let d = Game.make_player_desc Fire.house in
  let g = { gd_players = [| d; d |] } in
  let p () = Game.initial_player_state [ 0; 0; 0; 0; 0 ] d in
  let st = { players = [| p (); p () |] } in
  Alcotest.(check (option string))
    "pass" None
    (Oracle.execute (settings 200) ~pid:0 ~desc:g st |> Option.map (fun c -> c.cmd_card.key))

let lethal () =
  let g, st = empty 8 in
  let u = Updater.create st g in
  place u 0 "fire.drake" 2;
  (player u 1).pstate.ps_life <- 3;
  let board = Updater.export u in
  let cmd = Oracle.execute (settings 300) ~pid:0 ~desc:g board in
  Option.iter (fun c -> check_bool "legal lethal" true (is_legal g board 0 c)) cmd

let base_policy_bounds () =
  let g, st = empty 6 in
  let moves = Oracle.base_moves ~desc:g 0 st in
  check_bool "starts with pass" true (List.hd moves = None);
  let per_card = Hashtbl.create 8 in
  List.iter
    (function
      | None -> ()
      | Some c ->
          let id = c.cmd_card.id in
          Hashtbl.replace per_card id (1 + (Hashtbl.find_opt per_card id |> Option.value ~default:0)))
    moves;
  Hashtbl.iter
    (fun _ n -> check_bool "placements per card" true (n <= !Oracle.Params.placements_per_card))
    per_card

let simulate_does_not_touch_live () =
  let g, st = empty 6 in
  let u = Updater.create st g in
  place u 0 "fire.goblin" 0;
  let snap = Updater.export u in
  let cmd =
    {
      cmd_player = 0;
      cmd_card = card_by_key "fire.goblin";
      cmd_input = Some 1;
      cmd_cost = 1;
      cmd_flag = None;
    }
  in
  ignore (Game.simulate g snap 0 (Some cmd));
  check_bool "live goblin still in slot 0" true (occupant u 0 0 = Some "fire.goblin");
  check_bool "live slot 1 still empty" true (is_empty u 0 1)

let () =
  let open Alcotest in
  run "oracle"
    [
      ( "eval",
        [
          test_case "symmetric" `Quick symmetric;
          test_case "sprite self-damage" `Quick sprites_hurt_more_than_bulls;
          test_case "block titan" `Quick block_titan;
          test_case "race spider" `Quick race_spider;
          test_case "bigger creature" `Quick bigger_creature;
          test_case "play the bull" `Quick play_the_bull;
          test_case "fear a wide board" `Quick fear_wide;
        ] );
      ( "search",
        [
          test_case "legal move" `Quick legal_on_fresh;
          test_case "pass when broke" `Quick pass_when_broke;
          test_case "lethal" `Quick lethal;
          test_case "base policy" `Quick base_policy_bounds;
          test_case "simulate isolation" `Quick simulate_does_not_touch_live;
        ] );
    ]
