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
      check_bool "ambient" true (Prefs.ambient_sound p);
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
      Prefs.set_ambient_sound p false;
      Prefs.set_setup_seats p ("human", "lecturer");
      Prefs.set_setup_specials p ([ "zen"; "wind" ], [ "Troopers" ]);
      Prefs.set_setup_budget p 500;
      Prefs.set_setup_portraits p ("gale", "oracle-f");
      let q = Prefs.load ~path () in
      check_string "language" "fr" (Prefs.language q);
      check_float "sound" 0.5 (Prefs.sound_level q);
      check_bool "ambient" false (Prefs.ambient_sound q);
      check_string "seats" "human,lecturer"
        (let a, b = Prefs.setup_seats q in
         a ^ "," ^ b);
      check_string "specials" "zen;wind,Troopers"
        (let a, b = Prefs.setup_specials q in
         String.concat ";" a ^ "," ^ String.concat ";" b);
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
      check_bool "ambient default" true (Prefs.ambient_sound p);
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

let campaign_progress () =
  with_temp (fun path ->
      Sys.remove path;
      let p = Prefs.load ~path () in
      Prefs.complete_campaign p ~extras:[ Campaign.go_city ] "duckwoman";
      let q = Prefs.load ~path () in
      Alcotest.(check (list string))
        "completed" [ "duckwoman"; Campaign.go_city ]
        (List.sort String.compare (Prefs.campaign_completed q));
      check_string "position" "duckwoman" (Prefs.campaign_position q);
      Prefs.complete_campaign q "wind";
      let r = Prefs.load ~path () in
      check_string "night" Campaign.night_start_id (Prefs.campaign_position r);
      Prefs.reset_night_campaign r;
      check_bool "kept wind" true (List.mem "wind" (Prefs.campaign_completed (Prefs.load ~path ())));
      Prefs.reset_campaign (Prefs.load ~path ());
      check_string "reset" Campaign.day_start_id (Prefs.campaign_position (Prefs.load ~path ())))

let old_singles () =
  (* Files written before multi-select keep loading as one house per side. *)
  with_temp (fun path ->
      Out_channel.with_open_text path (fun oc -> output_string oc "setup.specials=zen,wind\n");
      let p = Prefs.load ~path () in
      let a, b = Prefs.setup_specials p in
      Alcotest.(check (list string)) "P1" [ "zen" ] a;
      Alcotest.(check (list string)) "P2" [ "wind" ] b)

let continuation_and_backslash () =
  (* An odd trailing run of backslashes continues the line; an even run is a literal backslash. *)
  with_temp (fun path ->
      Out_channel.with_open_text path (fun oc ->
          output_string oc "user.name=ab\\\n    cd\nai.opponent=x\\\\\n");
      let p = Prefs.load ~path () in
      Alcotest.(check (option string)) "continued" (Some "abcd") (Prefs.user_name p);
      Prefs.set_ai_opponent p "back\\slash";
      check_string "backslash round trip" "back\\slash" (Prefs.ai_opponent (Prefs.load ~path ())))

let comments_not_keys () =
  (* [#] and [!] lines are comments; saving must not turn the header into junk keys. *)
  with_temp (fun path ->
      Out_channel.with_open_text path (fun oc ->
          output_string oc "#user preferences\n! other comment\n\nlanguage=fr\n");
      let p = Prefs.load ~path () in
      Prefs.set_sound_level p 0.5;
      let lines = In_channel.with_open_text path In_channel.input_lines in
      let keys =
        List.filter_map
          (fun l -> if l = "" || l.[0] = '#' then None else Some (List.hd (String.split_on_char '=' l)))
          lines
      in
      Alcotest.(check (list string)) "keys" [ "language"; "sound.level" ] keys)

let unwritable_dir () =
  (* Saving is best effort: an unwritable location must not raise. *)
  let p = Prefs.load ~path:"/proc/mancer-nope/user.prefs" () in
  Prefs.set_language p "fr";
  check_string "in memory" "fr" (Prefs.language p)

let () =
  Alcotest.run "prefs"
    [
      ( "prefs",
        [
          Alcotest.test_case "defaults" `Quick defaults;
          Alcotest.test_case "round trip" `Quick round_trip;
          Alcotest.test_case "old singles" `Quick old_singles;
          Alcotest.test_case "scala format" `Quick scala_format;
          Alcotest.test_case "clamp and fallback" `Quick clamp_and_fallback;
          Alcotest.test_case "campaign progress" `Quick campaign_progress;
          Alcotest.test_case "continuation and backslash" `Quick continuation_and_backslash;
          Alcotest.test_case "comments are not keys" `Quick comments_not_keys;
          Alcotest.test_case "unwritable dir" `Quick unwritable_dir;
        ] );
    ]
