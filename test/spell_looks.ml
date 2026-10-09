(* Every look/custom key is a real card, and every spell has an effect. *)

open Mancer
open Sp
open Support

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)

let all_cards () =
  init_all_houses ();
  List.concat_map (fun (h : house) -> h.h_cards @ h.h_additional_cards) all_houses

let keys cards = List.map (fun (c : card) -> c.key) cards
let known_key cards key = List.exists (fun (c : card) -> c.key = key) cards

let looks_are_real () =
  let cards = all_cards () in
  List.iter
    (fun (key, _) ->
      check_bool ("look " ^ key) true (known_key cards key);
      check_bool (key ^ " is a spell") true (card_by_key key).is_spell)
    Spell_looks.looks

let custom_are_real () =
  let cards = all_cards () in
  List.iter
    (fun key ->
      check_bool ("custom " ^ key) true (known_key cards key);
      check_bool (key ^ " is a spell") true (card_by_key key).is_spell)
    Spell_looks.custom

let every_spell_covered () =
  let missing =
    all_cards () |> List.filter (fun (c : card) -> c.is_spell && not (Spell_looks.covered c.key)) |> keys
  in
  check_string "uncovered spells" "" (String.concat ", " missing)

let no_overlap () =
  List.iter
    (fun key -> check_bool (key ^ " not in both tables") false (List.mem_assoc key Spell_looks.looks))
    Spell_looks.custom

let () =
  Alcotest.run "spell_looks"
    [
      ( "table",
        [
          Alcotest.test_case "look keys exist" `Quick looks_are_real;
          Alcotest.test_case "custom keys exist" `Quick custom_are_real;
          Alcotest.test_case "every spell is covered" `Quick every_spell_covered;
          Alcotest.test_case "looks and custom do not overlap" `Quick no_overlap;
        ] );
    ]
