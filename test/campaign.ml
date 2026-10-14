(* Campaign map: unlocks, forks, houses and i18n, from CampaignTest.scala. *)

open Mancer

let check_bool = Alcotest.(check bool)
let ss = Alcotest.(list string)

let sorted xs = List.sort String.compare xs
let available done_ = sorted (Campaign.available done_)
let has done_ id = List.mem id (Campaign.available done_)
let has_not done_ id = not (has done_ id)

let open_at_start () =
  Alcotest.check ss "start" [ "entomologist"; "meteomancer" ] (available [])

let duckwoman_fork () =
  let after = [ "entomologist" ] in
  Alcotest.check ss "after entomologist"
    [ "duckwoman"; "meteomancer" ]
    (available after);
  check_bool "no farmer" true (has_not after "farmer");
  check_bool "no disciples" true (has_not after "disciples");
  check_bool "no dudes" true (has_not after "dudes");
  let met = [ "entomologist"; "duckwoman" ] in
  check_bool "meeting alone no dudes" true (has_not met "dudes");
  check_bool "meeting alone no disciples" true (has_not met "disciples");
  let city = Campaign.go_city :: met in
  check_bool "go city dudes" true (has city "dudes");
  check_bool "go city no disciples" true (has_not city "disciples");
  let avoid = Campaign.avoid_city :: met in
  check_bool "avoid disciples" true (has avoid "disciples");
  check_bool "avoid no dudes" true (has_not avoid "dudes");
  check_bool "seasons opens dudes" true (has [ "meteomancer"; "seasons" ] "dudes");
  check_bool "farmer not a fight" true (not (Campaign.is_fight (Campaign.node "farmer")));
  Alcotest.(check (option string))
    "farmer portrait" (Some "farmer")
    (Option.map (fun m -> m.Campaign.portrait_id) (Campaign.node "farmer").meeting);
  check_bool "duckwoman not a fight" true (not (Campaign.is_fight (Campaign.node "duckwoman")));
  Alcotest.check ss "duckwoman choices"
    [ Campaign.go_city; Campaign.avoid_city ]
    (match (Campaign.node "duckwoman").meeting with
    | Some m -> List.map (fun (c : Campaign.choice) -> c.id) m.choices
    | None -> [])

let mountain_approaches () =
  check_bool "not before quantumancer" true
    (has_not [ "entomologist"; "disciples"; "junk"; "spiritshaman" ] "mountainking");
  check_bool "quantumancer" true
    (has [ "entomologist"; "disciples"; "junk"; "spiritshaman"; "quantumancer" ] "mountainking");
  check_bool "dreamweaver" true
    (has [ "meteomancer"; "seasons"; "sower"; "fairy_knight"; "dreamweaver" ] "mountainking");
  check_bool "powermancer" true
    (has [ "entomologist"; "seasons"; "dudes"; "colors"; "elementalist"; "powermancer" ] "mountainking")

let wind_after_king () =
  check_bool "wind" true
    (has [ "entomologist"; "disciples"; "spiritshaman"; "quantumancer"; "mountainking" ] "wind")

let disciples_path () =
  let fork = [ "entomologist"; "duckwoman"; Campaign.avoid_city ] in
  check_bool "disciples" true (has fork "disciples");
  check_bool "farmer after disciples" true (has (fork @ [ "disciples" ]) "farmer");
  check_bool "no junk yet" true (has_not (fork @ [ "disciples" ]) "junk");
  check_bool "junk after farmer" true (has (fork @ [ "disciples"; "farmer" ]) "junk");
  check_bool "beaver after junk" true (has (fork @ [ "disciples"; "farmer"; "junk" ]) "beaver");
  check_bool "spirit after beaver" true
    (has (fork @ [ "disciples"; "farmer"; "junk"; "beaver" ]) "spiritshaman");
  check_bool "seasons no spirit" true (has_not [ "meteomancer"; "seasons" ] "spiritshaman");
  check_bool "city no spirit" true (has_not [ "entomologist"; Campaign.go_city; "dudes" ] "spiritshaman");
  Alcotest.(check (option int))
    "spirit life" (Some 50)
    (Option.bind (Campaign.node "spiritshaman").fight (fun f -> f.life))

let meteomancer_path () =
  let done_ = [ "meteomancer" ] in
  check_bool "seasons" true (has done_ "seasons");
  check_bool "sower" true (has (done_ @ [ "seasons" ]) "sower");
  check_bool "fairy" true (has (done_ @ [ "seasons"; "sower" ]) "fairy_knight");
  check_bool "ram not dreamweaver" true
    (has_not (done_ @ [ "seasons"; "sower"; "fairy_knight" ]) "dreamweaver");
  check_bool "ram" true (has (done_ @ [ "seasons"; "sower"; "fairy_knight" ]) "ram");
  check_bool "dreamweaver" true
    (has (done_ @ [ "seasons"; "sower"; "fairy_knight"; "ram" ]) "dreamweaver");
  check_bool "king" true
    (has (done_ @ [ "seasons"; "sower"; "fairy_knight"; "ram"; "dreamweaver" ]) "mountainking");
  check_bool "ram meeting" true (not (Campaign.is_fight (Campaign.node "ram")))

let dudes_path () =
  let done_ = [ "entomologist"; "dudes" ] in
  check_bool "rabbit" true (has done_ "rabbit");
  check_bool "no colors yet" true (has_not done_ "colors");
  check_bool "no junk" true (has_not done_ "junk");
  check_bool "colors" true (has (done_ @ [ "rabbit" ]) "colors");
  check_bool "elementalist" true (has (done_ @ [ "rabbit"; "colors" ]) "elementalist");
  check_bool "powermancer" true (has (done_ @ [ "rabbit"; "colors"; "elementalist" ]) "powermancer");
  check_bool "king" true
    (has (done_ @ [ "rabbit"; "colors"; "elementalist"; "powermancer" ]) "mountainking")

let junk_to_king () =
  let done_ = [ "entomologist"; "duckwoman"; Campaign.avoid_city; "disciples" ] in
  check_bool "farmer" true (has done_ "farmer");
  check_bool "quantumancer after spirit" true
    (has (done_ @ [ "farmer"; "junk"; "beaver"; "spiritshaman" ]) "quantumancer");
  check_bool "king after quantumancer" true
    (has (done_ @ [ "farmer"; "junk"; "beaver"; "spiritshaman"; "quantumancer" ]) "mountainking");
  check_bool "beaver is day" true ((Campaign.node "beaver").chapter = Campaign.Day)

let early_life () =
  Alcotest.(check (option int))
    "entomologist" (Some 30)
    (Option.bind (Campaign.node "entomologist").fight (fun f -> f.life));
  Alcotest.(check (option int))
    "disciples" (Some 40)
    (Option.bind (Campaign.node "disciples").fight (fun f -> f.life));
  Alcotest.(check (option int))
    "mountainking" (Some 50)
    (Option.bind (Campaign.node "mountainking").fight (fun f -> f.life));
  Alcotest.(check (option int)) "wind" None (Option.bind (Campaign.node "wind").fight (fun f -> f.life))

let night_unlock () =
  check_bool "night after wind" true (Campaign.chapter_of [ "wind" ] = Campaign.Night);
  Alcotest.check ss "night start" [ "faerie_druid"; "rogue"; "warp" ] (available [ "wind" ]);
  check_bool "no antimancer" true (has_not [ "wind" ] "antimancer")

let rogue_path () =
  let done_ = [ "wind"; "rogue" ] in
  check_bool "fishman" true (has done_ "fishman");
  check_bool "no elder" true (has_not done_ "elder");
  check_bool "no tinker yet" true (has_not done_ "tinker");
  check_bool "tinker" true (has (done_ @ [ "fishman" ]) "tinker");
  check_bool "battlemage" true (has (done_ @ [ "fishman"; "tinker" ]) "battlemage");
  check_bool "snowblood" true (has (done_ @ [ "fishman"; "tinker"; "battlemage" ]) "snowblood");
  check_bool "limbo" true (has (done_ @ [ "fishman"; "tinker"; "battlemage"; "snowblood" ]) "limbo")

let shadow_path () =
  let done_ = [ "wind"; "faerie_druid"; "soulbinder"; "catboy"; Campaign.stay_land ] in
  check_bool "shadowmage" true (has done_ "shadowmage");
  check_bool "no crowd" true (has_not done_ "crowd");
  check_bool "panther" true (has (done_ @ [ "shadowmage" ]) "panther");
  check_bool "shadowcaster" true (has (done_ @ [ "shadowmage"; "panther" ]) "shadowcaster");
  check_bool "darkpriest" true (has (done_ @ [ "shadowmage"; "panther"; "shadowcaster" ]) "darkpriest");
  check_bool "vampires" true (has (done_ @ [ "shadowmage"; "panther"; "shadowcaster"; "darkpriest" ]) "vampires");
  check_bool "deer" true (has (done_ @ [ "shadowmage"; "panther"; "shadowcaster"; "darkpriest"; "vampires" ]) "deer");
  check_bool "no reaper yet" true
    (has_not (done_ @ [ "shadowmage"; "panther"; "shadowcaster"; "darkpriest"; "vampires" ]) "soulreaper");
  check_bool "soulreaper" true
    (has (done_ @ [ "shadowmage"; "panther"; "shadowcaster"; "darkpriest"; "vampires"; "deer" ]) "soulreaper");
  check_bool "limbo" true
    (has
       (done_ @ [ "shadowmage"; "shadowcaster"; "darkpriest"; "vampires"; "deer"; "soulreaper" ])
       "limbo")

let antimancer_path () =
  let fork = [ "wind"; "faerie_druid"; "soulbinder"; "catboy"; Campaign.bring_city ] in
  check_bool "crowd" true (has fork "crowd");
  check_bool "no shadow" true (has_not fork "shadowmage");
  check_bool "antimancer after crowd" true (has (fork @ [ "crowd" ]) "antimancer");
  let done_ = fork @ [ "crowd"; "antimancer" ] in
  Alcotest.check ss "isolated to zen" [ "zen" ] (available done_);
  check_bool "no warp" true (has_not done_ "warp");
  Alcotest.(check string) "next" "zen" (Campaign.next_position "antimancer");
  Alcotest.check ss "after zen" [ "fisherman" ] (available (done_ @ [ "zen" ]));
  Alcotest.check ss "after fisherman" [ "conjurer" ] (available (done_ @ [ "zen"; "fisherman" ]));
  Alcotest.check ss "after conjurer" [ "chronomaster" ]
    (available (done_ @ [ "zen"; "fisherman"; "conjurer" ]));
  Alcotest.check ss "after chrono" [ "limbo" ]
    (available (done_ @ [ "zen"; "fisherman"; "conjurer"; "chronomaster" ]));
  check_bool "crowd meeting" true (not (Campaign.is_fight (Campaign.node "crowd")))

let faerie_path () =
  let done_ = [ "wind"; "faerie_druid" ] in
  check_bool "soulbinder" true (has done_ "soulbinder");
  check_bool "no catboy yet" true (has_not done_ "catboy");
  check_bool "catboy" true (has (done_ @ [ "soulbinder" ]) "catboy");
  check_bool "no limbo" true (has_not (done_ @ [ "soulbinder"; "catboy" ]) "limbo");
  Alcotest.check ss "catboy choices"
    [ Campaign.bring_city; Campaign.stay_land ]
    (match (Campaign.node "catboy").meeting with
    | Some m -> List.map (fun (c : Campaign.choice) -> c.id) m.choices
    | None -> [])

let warp_fork () =
  let done_ = [ "wind"; "warp" ] in
  check_bool "elder" true (has done_ "elder");
  check_bool "no kinetician yet" true (has_not done_ "kinetician");
  check_bool "kinetician" true (has (done_ @ [ "elder" ]) "kinetician");
  check_bool "girl" true (has (done_ @ [ "elder"; "kinetician" ]) "girl");
  let girl = done_ @ [ "elder"; "kinetician"; "girl" ] in
  check_bool "no shaman yet" true (has_not girl "shaman");
  check_bool "mountain shaman" true (has (girl @ [ Campaign.to_mountain ]) "shaman");
  check_bool "mountain no darksider" true (has_not (girl @ [ Campaign.to_mountain ]) "darksider");
  check_bool "valley darksider" true (has (girl @ [ Campaign.to_valley ]) "darksider");
  check_bool "patou" true (has [ "wind"; "darksider" ] "patou");
  check_bool "patou then limbo" true (has [ "wind"; "darksider"; "patou" ] "limbo")

let highpriest_gate () =
  check_bool "no priest before limbo" true
    (has_not [ "wind"; "rogue"; "tinker"; "battlemage"; "snowblood" ] "highpriest");
  check_bool "priest" true
    (has [ "wind"; "rogue"; "tinker"; "battlemage"; "snowblood"; "limbo" ] "highpriest")

let positions () =
  Alcotest.(check string) "after wind" Campaign.night_start_id (Campaign.next_position "wind");
  Alcotest.(check string) "after priest" "highpriest" (Campaign.next_position "highpriest");
  check_bool "finished" true (Campaign.is_finished [ "highpriest" ])

let night_restart () =
  let done_ = [ "wind"; "entomologist"; "warp"; "kinetician"; "girl"; "limbo" ] in
  Alcotest.check ss "day progress" [ "entomologist"; "wind" ] (sorted (Campaign.day_progress done_));
  check_bool "still night" true (Campaign.chapter_of (Campaign.day_progress done_) = Campaign.Night);
  Alcotest.check ss "night opens again" [ "faerie_druid"; "rogue"; "warp" ]
    (available (Campaign.day_progress done_))

let houses_resolve () =
  Catalog.init ();
  List.iter
    (fun (n : Campaign.node) ->
      match n.fight with
      | None -> ()
      | Some f ->
          check_bool f.house_key true (Option.is_some (Campaign.house_of Catalog.specials f.house_key)))
    Campaign.nodes

let portraits_exist () =
  let known =
    List.map (fun (o : Table.opponent) -> o.Table.opp_portrait) Table.duel_opponents @ Campaign.campaign_portraits
  in
  let have_files = Option.is_some (Assets.portrait "ember") in
  List.iter
    (fun (n : Campaign.node) ->
      match n.fight with
      | None -> ()
      | Some f ->
          check_bool (f.portrait_id ^ " known") true
            (List.mem f.portrait_id known || List.mem f.portrait_id (Table.faces_for_bot f.bot_id)
            || List.mem f.portrait_id Campaign.npcs);
          if have_files then
            match Assets.portrait f.portrait_id with
            | Some _ -> ()
            | None -> Alcotest.fail (f.portrait_id ^ " missing portrait file"))
    Campaign.nodes;
  List.iter
    (fun (n : Campaign.node) ->
      match n.meeting with
      | None -> ()
      | Some m ->
          check_bool (m.portrait_id ^ " npc") true (List.mem m.portrait_id Campaign.npcs);
          if have_files then
            match Assets.portrait m.portrait_id with
            | Some _ -> ()
            | None -> Alcotest.fail (m.portrait_id ^ " missing npc portrait"))
    Campaign.nodes

let end_lines () =
  let keys = function
    | "campaign.wind.win" | "campaign.end.lecturer.lose" | "campaign.end.win" | "campaign.end.lose" -> true
    | _ -> false
  in
  Alcotest.(check string)
    "node win"
    "campaign.wind.win"
    (Campaign.end_message_key ~node_id:"wind" ~bot_id:"lecturer" ~won:true ~has_key:keys);
  Alcotest.(check string)
    "bot lose"
    "campaign.end.lecturer.lose"
    (Campaign.end_message_key ~node_id:"wind" ~bot_id:"lecturer" ~won:false ~has_key:keys);
  Alcotest.(check string)
    "generic win" "campaign.end.win"
    (Campaign.end_message_key ~node_id:"zen" ~bot_id:"oracle" ~won:true ~has_key:keys);
  Alcotest.(check string)
    "generic lose" "campaign.end.lose"
    (Campaign.end_message_key ~node_id:"zen" ~bot_id:"oracle" ~won:false ~has_key:keys)

let i18n_keys () =
  let langs = [ "en"; "fr"; "es"; "ru"; "vi" ] in
  let bots =
    Campaign.nodes
    |> List.filter_map (fun (n : Campaign.node) ->
        Option.map (fun (f : Campaign.fight) -> f.bot_id) n.fight)
    |> List.sort_uniq compare
  in
  let required =
    [
      "campaign.end.win.title";
      "campaign.end.lose.title";
      "campaign.end.map";
      "campaign.end.replay";
      "campaign.end.win";
      "campaign.end.lose";
      "campaign.continue";
      "campaign.epilogue";
      "campaign.epilogue.menu";
      "campaign.night.rest.title";
      "campaign.night.rest";
      "campaign.antimancer.wake.title";
      "campaign.antimancer.wake";
    ]
    @ List.concat_map (fun b -> [ "campaign.end." ^ b ^ ".win"; "campaign.end." ^ b ^ ".lose" ]) bots
    @ List.concat_map
        (fun (n : Campaign.node) ->
          if Campaign.is_fight n then [ "campaign." ^ n.id ^ ".win"; "campaign." ^ n.id ^ ".lose" ] else [])
        Campaign.nodes
    @ List.concat_map
        (fun (n : Campaign.node) ->
          match n.meeting with
          | Some _ -> [ "campaign." ^ n.id ^ ".name"; "campaign." ^ n.id ^ ".message" ]
          | None -> [])
        Campaign.nodes
    @ List.concat_map
        (fun (n : Campaign.node) ->
          match n.meeting with
          | Some m ->
              List.map (fun (c : Campaign.choice) -> "campaign." ^ n.id ^ ".choice." ^ c.id) m.choices
          | None -> [])
        Campaign.nodes
  in
  List.iter
    (fun lang ->
      let file = Filename.concat (Assets.root ()) (Filename.concat "i18n" (lang ^ ".properties")) in
      if Sys.file_exists file then
        let props = Assets.load_properties file in
        List.iter
          (fun key -> if not (Hashtbl.mem props key) then Alcotest.fail (lang ^ " missing " ^ key))
          required)
    langs

(* Letters the GUI atlas rasterizes (see [load_fonts] in bin/gui.ml). A
   translation that steps outside this set shows tofu / missing glyphs. *)
let font_covers cp =
  (cp >= 32 && cp <= 126)
  || (cp >= 160 && cp <= 255)
  || (cp >= 0x100 && cp <= 0x17F)
  || cp = 0x1A0 || cp = 0x1A1 || cp = 0x1AF || cp = 0x1B0
  || (cp >= 0x400 && cp <= 0x45F)
  || (cp >= 0x1EA0 && cp <= 0x1EF9)
  || List.mem cp [ 0x2013; 0x2014; 0x2018; 0x2019; 0x201C; 0x201D; 0x2022; 0x2026 ]

let i18n_font_coverage () =
  List.iter
    (fun lang ->
      let file = Filename.concat (Assets.root ()) (Filename.concat "i18n" (lang ^ ".properties")) in
      if Sys.file_exists file then
        Hashtbl.iter
          (fun key v ->
            let rec go i =
              if i < String.length v then
                let d = String.get_utf_8_uchar v i in
                let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
                if cp >= 0xA0 && not (font_covers cp) then
                  Alcotest.fail
                    (Printf.sprintf "%s / %s: U+%04X not in the rasterized font charset" lang key cp)
                else go (i + Uchar.utf_decode_length d)
            in
            go 0)
          (Assets.load_properties file))
    [ "en"; "fr"; "es"; "ru"; "vi" ]

let play_bot_map () =
  Alcotest.(check string) "summoner" "summoner" (Campaign.play_bot "summoner");
  Alcotest.(check string) "strategist" "strategist" (Campaign.play_bot "strategist");
  Alcotest.(check string) "gambler" "lecturer" (Campaign.play_bot "gambler");
  Alcotest.(check string) "diviner" "diviner" (Campaign.play_bot "diviner");
  Alcotest.(check string) "lecturer" "lecturer" (Campaign.play_bot "lecturer");
  Alcotest.(check string) "oracle" "oracle" (Campaign.play_bot "oracle")

let () =
  Alcotest.run "campaign"
    [
      ( "day",
        [
          Alcotest.test_case "start" `Quick open_at_start;
          Alcotest.test_case "duckwoman" `Quick duckwoman_fork;
          Alcotest.test_case "mountain approaches" `Quick mountain_approaches;
          Alcotest.test_case "wind" `Quick wind_after_king;
          Alcotest.test_case "disciples path" `Quick disciples_path;
          Alcotest.test_case "meteomancer path" `Quick meteomancer_path;
          Alcotest.test_case "dudes path" `Quick dudes_path;
          Alcotest.test_case "junk to king" `Quick junk_to_king;
          Alcotest.test_case "early life" `Quick early_life;
        ] );
      ( "night",
        [
          Alcotest.test_case "unlock" `Quick night_unlock;
          Alcotest.test_case "rogue path" `Quick rogue_path;
          Alcotest.test_case "shadow path" `Quick shadow_path;
          Alcotest.test_case "antimancer path" `Quick antimancer_path;
          Alcotest.test_case "faerie path" `Quick faerie_path;
          Alcotest.test_case "warp fork" `Quick warp_fork;
          Alcotest.test_case "highpriest" `Quick highpriest_gate;
          Alcotest.test_case "positions" `Quick positions;
          Alcotest.test_case "night restart" `Quick night_restart;
        ] );
      ( "content",
        [
          Alcotest.test_case "houses" `Quick houses_resolve;
          Alcotest.test_case "portraits" `Quick portraits_exist;
          Alcotest.test_case "end lines" `Quick end_lines;
          Alcotest.test_case "i18n" `Quick i18n_keys;
          Alcotest.test_case "i18n font" `Quick i18n_font_coverage;
          Alcotest.test_case "play bot" `Quick play_bot_map;
        ] );
    ]
