(* Settings persistence: the ~/.mancer/user.prefs round-trip, in the Scala
   Storage format. *)

open Mancer

let check_string = Alcotest.(check string)
let check_float = Alcotest.(check (float 0.001))
let check_bool = Alcotest.(check bool)

let with_temp f =
  let path = Filename.temp_file "mancer-prefs" ".prefs" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ()) (fun () -> f path)

let defaults () =
  with_temp (fun path ->
      Sys.remove path;
      let p = Prefs.load ~path () in
      check_string "language" "en" (Prefs.language p);
      check_float "sound" 1.0 (Prefs.sound_level p);
      check_string "portrait" "ember" (Prefs.player_portrait p);
      check_string "ai" "summoner" (Prefs.ai_opponent p);
      check_bool "debug" false (Prefs.debug_mode p);
      check_string "campaign" "day_start" (Prefs.campaign_position p))

let round_trip () =
  with_temp (fun path ->
      Sys.remove path;
      let p = Prefs.load ~path () in
      Prefs.set_language p "fr";
      Prefs.set_sound_level p 0.5;
      Prefs.set_setup_seats p ("human", "lecturer");
      Prefs.set_setup_specials p ("zen", "wind");
      Prefs.set_setup_budget p 500;
      Prefs.set_setup_portraits p ("gale", "oracle-f");
      let q = Prefs.load ~path () in
      check_string "language" "fr" (Prefs.language q);
      check_float "sound" 0.5 (Prefs.sound_level q);
      check_string "seats" "human,lecturer"
        (let a, b = Prefs.setup_seats q in
         a ^ "," ^ b);
      check_string "specials" "zen,wind"
        (let a, b = Prefs.setup_specials q in
         a ^ "," ^ b);
      Alcotest.(check int) "budget" 500 (Prefs.setup_budget q);
      check_string "portrait shared" "gale" (Prefs.player_portrait q))

(* A file written by the Scala game (or by hand) loads the same way. *)
let scala_format () =
  with_temp (fun path ->
      Out_channel.with_open_text path (fun oc ->
          output_string oc
            "#user preferences\n\
             #Fri Oct 09 15:23:14 CEST 2026\n\
             language=fr\n\
             sound.level=0.50\n\
             player.portrait=gale\n");
      let p = Prefs.load ~path () in
      check_string "language" "fr" (Prefs.language p);
      check_float "sound" 0.5 (Prefs.sound_level p);
      check_string "portrait" "gale" (Prefs.player_portrait p))

let clamp_and_fallback () =
  with_temp (fun path ->
      Sys.remove path;
      let p = Prefs.load ~path () in
      Prefs.set_sound_level p 9.0;
      check_float "clamped" 1.0 (Prefs.sound_level (Prefs.load ~path ()));
      check_bool "unknown seat" true
        (Prefs.parse_seat ~allowed:[ "oracle" ] ~fallback:`Human "greedy" = `Human);
      check_bool "known bot" true
        (Prefs.parse_seat ~allowed:[ "oracle" ] ~fallback:`Human "oracle" = `Bot "oracle"))

let () =
  Alcotest.run "prefs"
    [
      ( "prefs",
        [
          Alcotest.test_case "defaults" `Quick defaults;
          Alcotest.test_case "round trip" `Quick round_trip;
          Alcotest.test_case "scala format" `Quick scala_format;
          Alcotest.test_case "clamp and fallback" `Quick clamp_and_fallback;
        ] );
    ]
