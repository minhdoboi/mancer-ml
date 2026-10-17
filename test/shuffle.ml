(* Checks that the ported CardShuffle constraints hold on real deals.  The
   rule checks run over many seeds: they must hold for every deck, not just
   one. *)

open Mancer
open Sp
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let seeds = List.init 200 (fun i -> i + 1)
let rng seed = Random.State.make [| seed |]
let indexes (h : player_house_desc) = Array.to_list h.phd_cards |> List.map (fun cd -> card_index1 cd.cd_card)
let has xs n = List.mem n xs
let count xs = List.fold_left (fun n b -> if b then n + 1 else n) 0 xs
let maximum xs = List.fold_left max 0 xs
let basics () = [ Fire.house; Water.house; Air.house; Earth.house ]

let deal ?(first = true) seed =
  init_all_houses ();
  Shuffle.create_player ~rng:(rng seed) ~first ~special:Troopers.house (basics ())

let deal_both ?(starting = owner) seed =
  init_all_houses ();
  Shuffle.two_players ~rng:(rng seed) ~starting ~specials:(Troopers.house, Troopers.house) (basics ())

let deck_keys (d : Shuffle.deal) =
  Array.to_list d.desc.pd_houses
  |> List.concat_map (fun h -> Array.to_list h.phd_cards |> List.map (fun cd -> cd.cd_card.key))

let mana_total (d : Shuffle.deal) = Array.fold_left ( + ) 0 d.mana
let has_cost (d : Shuffle.deal) i c = Array.exists (fun cd -> cd.cd_cost = c) d.desc.pd_houses.(i).phd_cards

let require_forces_cards () =
  init_all_houses ();
  for seed = 1 to 20 do
    let require = [| [ 11 ]; [ 8 ]; []; []; [] |] in
    let d = Shuffle.create_player ~rng:(rng seed) ~require ~first:true ~special:Troopers.house (basics ()) in
    let idx i = Array.to_list d.desc.pd_houses.(i).phd_cards |> List.map (fun cd -> card_index1 cd.cd_card) in
    Alcotest.(check bool) "fire 11" true (List.mem 11 (idx 0));
    Alcotest.(check bool) "water 8" true (List.mem 8 (idx 1))
  done

(* ------------------------------------------------------------------ decks *)

let four_cards_each () =
  List.iter
    (fun seed ->
      let d = deal seed in
      check_int "five houses" 5 (Array.length d.desc.pd_houses);
      Array.iteri
        (fun i h ->
          check_int (Printf.sprintf "seed %d: house %d has 4 cards" seed i) 4 (Array.length h.phd_cards))
        d.desc.pd_houses)
    seeds

let basic_rules () =
  List.iter
    (fun seed ->
      let d = deal seed in
      let f, w, a, e =
        ( indexes d.desc.pd_houses.(0),
          indexes d.desc.pd_houses.(1),
          indexes d.desc.pd_houses.(2),
          indexes d.desc.pd_houses.(3) )
      in
      let ok name b = check_bool (Printf.sprintf "seed %d: %s" seed name) true b in
      List.iteri
        (fun i xs ->
          let s = List.fold_left ( + ) 0 xs in
          ok (Printf.sprintf "house %d sum in (20,30)" i) (s > 20 && s < 30))
        [ f; w; a; e ];
      ok "one mana gen" (count [ has f 3; has w 5; has e 5 ] = 1);
      ok "one wipe" (count [ has f 6; has f 9; has a 8 ] = 1);
      let finish = count [ has f 11; has a 6; has a 8; has e 6 ] in
      ok "1 or 2 finishers" (finish > 0 && finish < 3);
      let defense = count [ has w 6; has w 10; has a 4; has e 1; has e 2; has e 4; has e 11 ] in
      ok "2 or 3 defense" (defense > 1 && defense < 4);
      ok "max fire > 9" (maximum f > 9);
      ok "max earth > 9" (maximum e > 9);
      ok "max water > 8" (maximum w > 8);
      ok "max air > 8" (maximum a > 8);
      ok "no fire 9 with 11" (not (has f 9 && has f 11));
      ok "no fire 5 with earth 3" (not (has f 5 && has e 3));
      ok "no water 1 with earth 9" (not (has w 1 && has e 9));
      ok "no earth 5 with 6" (not (has e 5 && has e 6));
      if has a 7 then (
        ok "air 7 needs exactly one mass" (count [ has f 11; has e 9; has w 8 ] = 1);
        ok "air 7 alt" (has w 5 || has w 3 || has f 11)))
    seeds

let special_quarters () =
  List.iter
    (fun seed ->
      let idx = indexes (deal seed).desc.pd_houses.(4) in
      List.iter
        (fun (lo, hi) ->
          check_bool
            (Printf.sprintf "seed %d: one trooper in %d-%d" seed lo hi)
            true
            (count (List.map (fun i -> i >= lo && i <= hi) idx) = 1))
        [ (1, 2); (3, 4); (5, 6); (7, 8) ])
    seeds

let exclusion () =
  List.iter
    (fun seed ->
      let p1, p2 = deal_both seed in
      for i = 0 to 4 do
        let b = indexes p2.desc.pd_houses.(i) in
        List.iter
          (fun n ->
            check_bool (Printf.sprintf "seed %d: house %d index %d not shared" seed i n) false (has b n))
          (indexes p1.desc.pd_houses.(i))
      done)
    seeds

let wrong_house_order () =
  init_all_houses ();
  match
    Shuffle.create_player ~first:true ~special:Troopers.house
      [ Water.house; Fire.house; Air.house; Earth.house ]
  with
  | _ -> Alcotest.fail "accepted houses out of order"
  | exception Invalid_argument _ -> ()

(* ------------------------------------------------------------------- mana *)

let opening_mana () =
  List.iter
    (fun seed ->
      List.iter
        (fun first ->
          let d = deal ~first seed in
          check_int "five mana pools" 5 (Array.length d.mana);
          check_int "special mana 2" 2 d.mana.(4);
          check_int "basics total 19 or 18, plus 2" (if first then 21 else 20) (mana_total d);
          Array.iteri (fun i m -> if i < 4 then check_bool "mana in 2..6" true (m >= 2 && m <= 6)) d.mana)
        [ true; false ])
    seeds

(* ManaShuffler: the mana gen's own house must cover its cost; the first
   player pays it in full, the second one less *)
let mana_gen_covers_its_cost () =
  List.iter
    (fun seed ->
      List.iter
        (fun first ->
          let d = deal ~first seed in
          match List.find_opt (fun (idx, cost) -> has_cost d idx cost) Shuffle.mana_gens with
          | None -> Alcotest.fail (Printf.sprintf "seed %d: deck has no mana gen" seed)
          | Some (idx, cost) ->
              let need = if first then cost else cost - 1 in
              check_bool (Printf.sprintf "seed %d: mana[%d] >= %d" seed idx need) true (d.mana.(idx) >= need))
        [ true; false ])
    seeds

(* fire 10 + water 3 caps fire's opening pool below 6 *)
let fire_ten_water_three_caps_fire () =
  let hits =
    List.filter
      (fun seed ->
        let d = deal seed in
        let trips = has_cost d 0 10 && has_cost d 1 3 in
        if trips then check_bool (Printf.sprintf "seed %d: fire mana below 6" seed) true (d.mana.(0) < 6);
        trips)
      seeds
  in
  check_bool "some deck trips the ban" true (hits <> [])

(* Only the named player draws the 19; who starts does not change the decks *)
let starting_player () =
  let o1, o2 = deal_both ~starting:owner 9 in
  let s1, s2 = deal_both ~starting:opponent 9 in
  check_int "starting=owner: owner draws 19" 21 (mana_total o1);
  check_int "starting=owner: opponent draws 18" 20 (mana_total o2);
  check_int "starting=opponent: owner draws 18" 20 (mana_total s1);
  check_int "starting=opponent: opponent draws 19" 21 (mana_total s2);
  Alcotest.(check (list string)) "owner deck unchanged" (deck_keys o1) (deck_keys s1);
  Alcotest.(check (list string)) "opponent deck unchanged" (deck_keys o2) (deck_keys s2)

let seeded_repeatable () =
  let a1, a2 = deal_both 42 and b1, b2 = deal_both 42 in
  Alcotest.(check (list string))
    "same seed, same decks"
    (deck_keys a1 @ deck_keys a2)
    (deck_keys b1 @ deck_keys b2);
  Alcotest.(check (array int)) "same seed, same mana" a1.mana b1.mana

let () =
  let open Alcotest in
  run "shuffle"
    [
      ( "decks",
        [
          test_case "four cards per house" `Quick four_cards_each;
          test_case "basic house rules" `Quick basic_rules;
          test_case "special quarters" `Quick special_quarters;
          test_case "exclusion" `Quick exclusion;
          test_case "house order checked" `Quick wrong_house_order;
        ] );
      ( "mana",
        [
          test_case "opening mana" `Quick opening_mana;
          test_case "mana gen covers cost" `Quick mana_gen_covers_its_cost;
          test_case "fire 10 water 3 caps fire" `Quick fire_ten_water_three_caps_fire;
          test_case "starting player" `Quick starting_player;
          test_case "seeded" `Quick seeded_repeatable;
          test_case "require" `Quick require_forces_cards;
        ] );
    ]
