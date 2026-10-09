(* Race leaf from SummonerEvalTest.scala. *)

open Mancer
open Sp
open Engine
open Support

let check_float = Alcotest.(check (float 1e-5))
let check_bool = Alcotest.(check bool)

let card_descs (h : house) =
  h.h_cards
  |> List.map (fun c -> { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 })
  |> Array.of_list

let desc () =
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
  { gd_players = [| d; d |] }

let empty () =
  let g = desc () in
  let p () = Game.initial_player_state (List.init 5 (fun _ -> 5)) g.gd_players.(0) in
  (g, { players = [| p (); p () |] })

let board ~awake placed =
  let g, st = empty () in
  let u = Updater.create st g in
  List.iter (fun (pid, num, key) -> place ~run:awake u pid key num) placed;
  (g, Updater.export u)

(* Opponent to move, as Summoner scores a leaf after the bot has played. *)
let v st = Heuris.of_state ~bot_id:0 ~to_move:1 st

let symmetric () =
  let _, st = empty () in
  check_float "empty" 0. (v st);
  let _, mirror = board ~awake:true [ (0, 2, "fire.bull"); (1, 3, "fire.bull") ] in
  check_float "mirror bulls" 0. (v mirror)

let life_lead () =
  let g, st = empty () in
  st.players.(0).ps_life <- 52;
  st.players.(1).ps_life <- 31;
  check_bool "ahead" true (v st > 0.2);
  let from_opp = Heuris.of_state ~bot_id:1 ~to_move:0 st in
  check_float "antisymmetric" (-.from_opp) (Heuris.of_state ~bot_id:0 ~to_move:1 st);
  ignore g

let awake_beats_sleep () =
  let _, awake = board ~awake:true [ (0, 2, "fire.bull") ] in
  let _, sleeping = board ~awake:false [ (0, 2, "fire.bull") ] in
  check_bool "awake" true (v awake > v sleeping)

let held_out_of_race () =
  let g, ready = board ~awake:true [ (0, 2, "fire.bull") ] in
  let u = Updater.create ready g in
  Slot.toggle (slot u 0 2) blocked_flag;
  let held = Updater.export u in
  check_bool "held" true (v ready > v held)

let block_titan () =
  let _, blocking = board ~awake:true [ (1, 3, "air.titan"); (0, 3, "fire.bull") ] in
  let _, open_ = board ~awake:true [ (1, 3, "air.titan"); (0, 0, "fire.bull") ] in
  check_bool "block harder hitter" true (v blocking > v open_)

let race_spider () =
  let _, blocking = board ~awake:true [ (1, 3, "earth.hugespider"); (0, 3, "fire.bull") ] in
  let _, open_ = board ~awake:true [ (1, 3, "earth.hugespider"); (0, 0, "fire.bull") ] in
  check_bool "race when favourable" true (v open_ > v blocking)

let multi_through_blocker () =
  let _, multi = board ~awake:true [ (0, 2, "air.cloud"); (1, 2, "air.fence") ] in
  let _, single = board ~awake:true [ (0, 2, "fire.goblin"); (1, 2, "air.fence") ] in
  check_bool "multi hits face" true (v multi > v single)

let bigger_creature () =
  let _, big = board ~awake:true [ (0, 2, "fire.bull") ] in
  let _, small = board ~awake:true [ (0, 2, "fire.goblin") ] in
  check_bool "bull > goblin" true (v big > v small)

let () =
  let open Alcotest in
  run "heuris"
    [
      ( "race",
        [
          test_case "symmetric" `Quick symmetric;
          test_case "life lead" `Quick life_lead;
          test_case "awake" `Quick awake_beats_sleep;
          test_case "held" `Quick held_out_of_race;
          test_case "block titan" `Quick block_titan;
          test_case "race spider" `Quick race_spider;
          test_case "multi" `Quick multi_through_blocker;
          test_case "bigger" `Quick bigger_creature;
        ] );
    ]
