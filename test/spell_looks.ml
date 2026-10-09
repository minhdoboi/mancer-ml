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

(* [custom] promises a bespoke effect, but the bespoke effects live in the GUI
   library, which these tests do not link. [for_spell] in gui/fx_stage.ml is a
   match on the card key that falls through to the generic table, so a key with
   no case of its own is simply never drawn -- the spell plays silently. Read the
   source and check every custom key really has one. *)
let stage_source () =
  let rec up d =
    let here = Filename.concat d "gui" in
    if Sys.file_exists (Filename.concat here "fx_stage.ml") then Some here
    else
      let parent = Filename.dirname d in
      if parent = d then None else up parent
  in
  Option.map (fun dir -> Filename.concat dir "fx_stage.ml") (up (Sys.getcwd ()))

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let custom_all_dispatched () =
  match stage_source () with
  | None -> () (* no GUI sources here (installed tree); nothing to check *)
  | Some path ->
      let src = read_file path in
      (* the case has to name the key itself, not merely appear somewhere *)
      List.iter
        (fun key ->
          let case = "| \"" ^ key ^ "\" ->" in
          check_bool (key ^ " dispatched in fx_stage") true
            (let needle = String.length case in
             let rec scan i =
               if i + needle > String.length src then false
               else if String.sub src i needle = case then true
               else scan (i + 1)
             in
             scan 0))
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
          Alcotest.test_case "custom spells are dispatched" `Quick custom_all_dispatched;
        ] );
    ]
