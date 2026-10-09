(* Asset text helpers. *)

open Mancer

let check = Alcotest.(check string)

let format () =
  check "plain" "a 3 b 4" (Assets.format "a {0} b {1}" [ "3"; "4" ]);
  check "two digits" "x" (Assets.format "{10}" (List.init 11 (fun i -> if i = 10 then "x" else "-")));
  check "unknown index stays" "{2} and 5" (Assets.format "{2} and {0}" [ "5" ]);
  check "literal braces stay" "{ x } {" (Assets.format "{ x } {" [ "1" ]);
  check "empty braces stay" "{}" (Assets.format "{}" [ "1" ]);
  check "arguments are not re-substituted" "{1} B" (Assets.format "{0} {1}" [ "{1}"; "B" ])

(* The card text marks a basic house with [@0] … [@3] (its name) and its
   power with [#0] … [#3] (the noun the power takes); the front end fills
   them in. *)
let fill () =
  let houses = [| ("Anger", "anger"); ("Sadness", "sadness"); ("Fear", "fear"); ("Joy", "joy") |] in
  check "name marker" "Anger and Joy" (Assets.fill_basic_houses "[@0] and [@3]" houses);
  check "power marker" "anger and joy" (Assets.fill_basic_houses "[#0] and [#3]" houses);
  check "no marker" "plain text" (Assets.fill_basic_houses "plain text" houses);
  check "unknown marker" "[@4]" (Assets.fill_basic_houses "[@4]" houses)

(* The settings dialog picks another language without a restart: dropping the
   cached bundle must make the next lookup read the language again. *)
let reload () =
  let bundle lang = Filename.concat (Filename.concat (Assets.root ()) "i18n") (lang ^ ".properties") in
  let expected lang key =
    match Hashtbl.find_opt (Assets.load_properties (bundle lang)) key with
    | Some v -> Assets.clean v
    | None -> key
  in
  (* mirror the GUI: no env override, the language lives in the prefs file *)
  Unix.putenv "MANCER_LANG" "";
  let path = Filename.temp_file "mancer-assets-prefs" ".prefs" in
  Unix.putenv "MANCER_USER_PREFS" path;
  Prefs.set_language (Prefs.load ()) "fr";
  Assets.reload ();
  check "fr after reload" (expected "fr" "settings.lang") (Assets.text "settings.lang");
  Prefs.set_language (Prefs.load ()) "en";
  Assets.reload ();
  check "en after reload" (expected "en" "settings.lang") (Assets.text "settings.lang")

let () =
  Alcotest.run "assets"
    [
      ("format", [ Alcotest.test_case "placeholders" `Quick format ]);
      ("basic names", [ Alcotest.test_case "fill" `Quick fill ]);
      ("language", [ Alcotest.test_case "reload" `Quick reload ]);
    ]
