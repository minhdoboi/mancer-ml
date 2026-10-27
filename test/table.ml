(* Table: seats, seeded deals and the per-card choices a UI offers. *)

open Mancer
open Sp

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let greedy = Table.Bot "greedy"

let rec play_out t =
  match Table.to_move t with
  | None -> ()
  | Some _ ->
      Table.play_bot t;
      play_out t

(* A fixed seed deals and plays the same game *)
let seed_replays () =
  let setup =
    { Table.default_setup with seats = (greedy, greedy); specials = ("zen", "wind"); seed = Some 3 }
  in
  let game () =
    let t = Table.start setup in
    play_out t;
    Table.record t
  in
  let a = game () and b = game () in
  check_int "seed" 3 (Table.seed (Table.start setup));
  check_int "turns" a.turns b.turns;
  check_bool "moves" true (a.moves = b.moves);
  check_bool "winner" true (a.winner_player = b.winner_player);
  check_bool "specials" true (a.special_names = ("zen", "wind"))

(* A human seat is played through choices; play_bot refuses it *)
let human_choices () =
  let t = Table.start { Table.default_setup with seats = (Table.Human, greedy); seed = Some 1 } in
  Alcotest.check_raises "human to move" (Invalid_argument "Table.bot_command: a human is to move") (fun () ->
      Table.play_bot t);
  let playable =
    Table.hand t owner
    |> List.concat_map (fun h -> Array.to_list h.phd_cards |> List.filter (fun cd -> Table.playable t cd h))
  in
  let cd = List.find (fun cd -> cd.cd_card.input_spec = Select_owner_slot) playable in
  let cmds = Table.choices t cd.cd_card in
  check_int "one per open slot" nb_slots (List.length cmds);
  List.iter
    (fun c ->
      check_bool "same card" true (c.cmd_card.key = cd.cd_card.key);
      check_bool "own side" true (Table.target_side c = Some owner))
    cmds;
  Table.submit t (Some (List.hd cmds));
  (* the greedy bot is to move now *)
  check_bool "bot to move" true (Table.to_move t = Some opponent);
  Table.play_bot t;
  check_bool "human again" true (Table.to_move t = Some owner)

(* The duel dialog's AI roster: faces, levels and their bots *)
let duel_roster () =
  let opps = Table.duel_opponents in
  check_int "scala faces" 13 (List.length opps);
  check_bool "level order" true
    (let rec nondec = function
       | [] | [ _ ] -> true
       | a :: b :: rest -> a.Table.opp_level <= b.Table.opp_level && nondec (b :: rest)
     in
     nondec opps);
  List.iter
    (fun (o : Table.opponent) ->
      check_bool
        ("kind face for " ^ o.Table.opp_kind)
        true
        (List.mem o.Table.opp_portrait (Table.faces_for_bot o.Table.opp_kind));
      check_bool ("bot of " ^ o.Table.opp_portrait) true
        (Table.bot_of_portrait o.Table.opp_portrait = Some o.Table.opp_bot))
    opps;
  check_bool "lecturer not gambler" true (not (List.mem "gambler" (Table.faces_for_bot "lecturer")));
  check_bool "unknown face" true (Table.bot_of_portrait "ember" = None);
  check_bool "unknown bot" true (Table.faces_for_bot "nope" = Table.faces_for_bot "greedy");
  check_int "sixteen player faces" 16 (List.length Table.player_portraits)

(* The settings classes tab groups special houses by author like Scala. *)
let house_groups () =
  let grouped = List.map (fun (h : house) -> h.h_key) (List.concat_map snd Table.house_groups) in
  let specials = List.map (fun (h : house) -> h.h_key) (Table.special_houses ()) in
  List.iter (fun h -> check_bool h true (List.mem h grouped)) specials;
  Alcotest.(check (option string)) "Troopers" (Some "garcia1000") (Table.group_of_house "Troopers");
  (* the "olds" and "garcia1000" rows are merged: garcia1000 owns Dudes and Troopers *)
  Alcotest.(check (option string)) "Dudes" (Some "garcia1000") (Table.group_of_house "Dudes");
  Alcotest.(check (option string)) "zen" (Some "badsleep") (Table.group_of_house "zen");
  Alcotest.(check (option string)) "Wind" (Some "sinist") (Table.group_of_house "Wind");
  Alcotest.(check (option string)) "emotion" (Some "Floofy") (Table.group_of_house "emotion");
  check_bool "Chronomaster present" true (List.exists (fun (a, _) -> a = "panderson") Table.house_groups)

(* The fight resolves each side's checked house set to one house. *)
let resolve_special () =
  check_bool "singleton" true (Table.resolve_special [ "zen" ] = "zen");
  let all = List.map (fun (h : house) -> h.h_key) (Table.special_houses ()) in
  for _ = 1 to 20 do
    check_bool "empty means all" true (List.mem (Table.resolve_special []) all);
    check_bool "unknown dropped" true (Table.resolve_special [ "nope"; "zen" ] = "zen")
  done

let card_keys t pid =
  (Session.desc (Table.session t)).gd_players.(pid).pd_houses |> Array.to_list
  |> List.concat_map (fun h -> Array.to_list h.phd_cards |> List.map (fun cd -> cd.cd_card.key))

(* The row a card sits in, in one list of houses. *)
let row_of houses key =
  List.find_map
    (fun (h : player_house_desc) ->
      let i = ref (-1) in
      Array.iteri (fun k cd -> if cd.cd_card.key = key then i := k) h.phd_cards;
      if !i < 0 then None else Some !i)
    houses

(* A house can spend a card for good: the Seasons specials may be used once
   apiece, and the engine then takes the card out of the deck.  The board
   still draws it, in the row it was dealt at, or a card the opponent played
   would leave the column it was played from. *)
let spent_card_keeps_its_row () =
  let setup =
    { Table.default_setup with seats = (greedy, greedy); specials = ("zen", "seasons"); seed = Some 1 }
  in
  let t = Table.start setup in
  let special pid =
    List.find (fun (h : player_house_desc) -> h.phd_house.h_index = special_house_index) (Table.hand t pid)
  in
  (* the owner passes, so the opponent is the one to move *)
  Table.submit t None;
  let cd = special opponent |> fun h -> h.phd_cards.(0) in
  let input = match cd.cd_card.input_spec with Select_owner_slot -> Some 0 | _ -> None in
  Table.submit t
    (Some { cmd_player = opponent; cmd_card = cd.cd_card; cmd_input = input; cmd_cost = cd.cd_cost; cmd_flag = None });
  (* the engine spent the card: it is gone from the deck *)
  check_bool "gone from the deck" true (row_of (Table.hand t opponent) cd.cd_card.key = None);
  (* the board still draws it, in the row it was dealt at *)
  check_int "same row" (Option.value (row_of (Table.dealt_hand t opponent) cd.cd_card.key) ~default:(-1))
    (Option.value (row_of (Table.hand_rows t opponent) cd.cd_card.key) ~default:(-1));
  check_bool "still drawn" true (row_of (Table.hand_rows t opponent) cd.cd_card.key <> None);
  (* and it cannot be played again *)
  let spent =
    List.find (fun (h : player_house_desc) -> h.phd_house.h_index = special_house_index)
      (Table.hand_rows t opponent)
  in
  check_bool "spent" true
    (Array.exists (fun c -> c.cd_card.key = cd.cd_card.key && not c.cd_enabled) spent.phd_cards)

(* The Dark Monk that replaces the False Prophet takes its row: the Monk in
   the deck says the Prophet was played, so the row shows only the Monk. *)
let swap_takes_the_row () =
  let house = List.find (fun (h : house) -> h.h_key = "disciples") (Table.special_houses ()) in
  let card key = List.find (fun (c : card) -> c.key = key) (house.h_cards @ house.h_additional_cards) in
  let cd c = { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 } in
  let preacher = card "disciples.preacher" in
  let dealt = { phd_house = house; phd_cards = [| cd (card "disciples.prophet"); cd preacher |] } in
  let live = { phd_house = house; phd_cards = [| cd (card "disciples.monk"); cd preacher |] } in
  let rows = Table.house_rows live dealt in
  check_int "two rows" 2 (Array.length rows);
  check_string "the monk took the row" "disciples.monk" rows.(0).cd_card.key;
  check_string "the other card kept its row" "disciples.preacher" rows.(1).cd_card.key

(* Switch cards: houses and opening hands move; seats stay. *)
let switch_swaps_houses_and_cards () =
  let setup =
    {
      Table.default_setup with
      seats = (Table.Human, greedy);
      specials = ("zen", "wind");
      seed = Some 7;
      starting = owner;
    }
  in
  let t = Table.start setup in
  let t' = Table.start_swapped t in
  let a = Table.record t and b = Table.record t' in
  check_bool "seats stay" true ((Table.setup t').seats = (Table.Human, greedy));
  check_bool "specials swap" true ((Table.setup t').specials = ("wind", "zen"));
  check_bool "starter flips" true ((Table.setup t').starting = opponent);
  check_bool "owner cards were opponent's" true (card_keys t' owner = card_keys t opponent);
  check_bool "opponent cards were owner's" true (card_keys t' opponent = card_keys t owner);
  check_bool "mana follows the cards" true (b.mana = [| a.mana.(1); a.mana.(0) |]);
  let t'' = Table.start_swapped ~keep_starter:true t in
  check_bool "campaign starter stays" true ((Table.setup t'').starting = owner)

(* Restart with the same seed must deal the same cards. *)
let restart_keeps_deal () =
  let setup =
    { Table.default_setup with seats = (greedy, greedy); specials = ("zen", "wind"); seed = Some 7 }
  in
  let t = Table.start setup in
  let t2 = Table.start { (Table.setup t) with seed = Some (Table.seed t) } in
  check_bool "same seed" true (Table.seed t = Table.seed t2);
  check_bool "same owner cards" true (card_keys t owner = card_keys t2 owner);
  check_bool "same opponent cards" true (card_keys t opponent = card_keys t2 opponent)

(* The campaign class is the house the player keeps all through a campaign. *)
let campaign_class () =
  Alcotest.(check string) "default" "shaman" (Table.campaign_class_of "shaman");
  Alcotest.(check string) "by name" "shaman" (Table.campaign_class_of "Shaman");
  Alcotest.(check string) "upper case" "Troopers" (Table.campaign_class_of "troopers");
  Alcotest.(check string) "unknown" Prefs.default_campaign_class (Table.campaign_class_of "nope")

(* Mode picks the basic houses: standard swaps fire for anger. *)
let mode_picks_basics () =
  let desc t = Session.desc (Table.session t) in
  let basic t pid = (desc t).gd_players.(pid).pd_houses.(0).phd_house.h_key in
  let base = { Table.default_setup with seats = (greedy, greedy); seed = Some 5 } in
  let standard = Table.start { base with mode = Catalog.Standard } in
  check_bool "standard anger" true (basic standard owner = "anger");
  let classic = Table.start { base with mode = Catalog.Classic } in
  check_bool "classic fire" true (basic classic owner = "fire")

(* The names of the basic houses follow the mode: a card that reads a basic
   house's power names it, so the text has to name the house the deck has. *)
let mode_names_basics () =
  let names mode =
    let base = { Table.default_setup with seats = (greedy, greedy); seed = Some 5 } in
    let t = Table.start { base with mode } in
    Assets.basic_house_names (Session.state (Table.session t)) owner
  in
  let standard = names Catalog.Standard in
  check_string "standard anger" (Assets.text "anger") standard.(0);
  check_string "standard sadness" (Assets.text "sadness") standard.(1);
  check_string "standard fear" (Assets.text "fear") standard.(2);
  check_string "standard joy" (Assets.text "joy") standard.(3);
  let classic = names Catalog.Classic in
  check_string "classic fire" (Assets.text "fire") classic.(0);
  check_string "classic water" (Assets.text "water") classic.(1);
  check_string "classic air" (Assets.text "air") classic.(2);
  check_string "classic earth" (Assets.text "earth") classic.(3)

(* An explicit mode overrides the Game tab's mode for a campaign setup. *)
let campaign_mode_override () =
  let n = Campaign.node "entomologist" in
  let standard = Table.campaign_setup ~mode:Catalog.Standard ~player_special:"zen" n in
  check_bool "explicit standard" true (standard.mode = Catalog.Standard);
  let classic = Table.campaign_setup ~mode:Catalog.Classic ~player_special:"zen" n in
  check_bool "explicit classic" true (classic.mode = Catalog.Classic)

(* The hard difficulty gives every campaign fight the full life and hands the
   day chapter to the diviner; the other difficulties keep the map. *)
let campaign_hard_difficulty () =
  let n = Campaign.node "entomologist" in
  let normal = Table.campaign_setup ~difficulty:"normal" ~player_special:"zen" n in
  Alcotest.(check (option int)) "normal life" (Some 30) normal.life;
  check_string "normal bot" "summoner" (Table.seat_name (snd normal.seats));
  let hard = Table.campaign_setup ~difficulty:"hard" ~player_special:"zen" n in
  Alcotest.(check (option int)) "hard life" (Some 60) hard.life;
  check_string "hard bot" "diviner" (Table.seat_name (snd hard.seats));
  let night = Campaign.node "zen" in
  let hard_night = Table.campaign_setup ~difficulty:"hard" ~player_special:"zen" night in
  Alcotest.(check (option int)) "hard night life" (Some 60) hard_night.life;
  check_string "hard night bot" "lecturer" (Table.seat_name (snd hard_night.seats))

(* The easy difficulty gives the player more life on the day chapter, but not
   against the wind: the bonus is the human's alone. *)
let campaign_easy_difficulty () =
  let setup = Table.campaign_setup ~difficulty:"easy" ~player_special:"zen" (Campaign.node "entomologist") in
  check_int "bonus" 10 setup.player_life_bonus;
  let st = Session.state (Table.session (Table.start setup)) in
  check_int "player life" 40 st.players.(owner).ps_life;
  check_int "bot life" 30 st.players.(opponent).ps_life;
  let other = Table.campaign_setup ~difficulty:"easy" ~player_special:"zen" (Campaign.node "junk") in
  check_int "other bonus" 5 other.player_life_bonus;
  let sto = Session.state (Table.session (Table.start other)) in
  check_int "other player life" 45 sto.players.(owner).ps_life;
  check_int "other bot life" 40 sto.players.(opponent).ps_life;
  let wind = Table.campaign_setup ~difficulty:"easy" ~player_special:"zen" (Campaign.node "wind") in
  check_int "wind bonus" 0 wind.player_life_bonus;
  let stw = Session.state (Table.session (Table.start wind)) in
  check_int "wind player life" 60 stw.players.(owner).ps_life;
  check_int "wind bot life" 60 stw.players.(opponent).ps_life;
  let normal = Table.campaign_setup ~difficulty:"normal" ~player_special:"zen" (Campaign.node "entomologist") in
  check_int "normal bonus" 0 normal.player_life_bonus;
  let stn = Session.state (Table.session (Table.start normal)) in
  check_int "normal player life" 30 stn.players.(owner).ps_life;
  check_int "normal bot life" 30 stn.players.(opponent).ps_life

let () =
  Alcotest.run "table"
    [
      ( "table",
        [
          Alcotest.test_case "seed replays" `Quick seed_replays;
          Alcotest.test_case "human choices" `Quick human_choices;
          Alcotest.test_case "duel roster" `Quick duel_roster;
          Alcotest.test_case "house groups" `Quick house_groups;
          Alcotest.test_case "resolve special" `Quick resolve_special;
          Alcotest.test_case "campaign class" `Quick campaign_class;
          Alcotest.test_case "switch swaps houses and cards" `Quick switch_swaps_houses_and_cards;
          Alcotest.test_case "restart keeps deal" `Quick restart_keeps_deal;
          Alcotest.test_case "spent card keeps its row" `Quick spent_card_keeps_its_row;
          Alcotest.test_case "swap takes the row" `Quick swap_takes_the_row;
          Alcotest.test_case "mode picks basics" `Quick mode_picks_basics;
          Alcotest.test_case "mode names basics" `Quick mode_names_basics;
          Alcotest.test_case "campaign mode override" `Quick campaign_mode_override;
          Alcotest.test_case "campaign hard difficulty" `Quick campaign_hard_difficulty;
          Alcotest.test_case "campaign easy difficulty" `Quick campaign_easy_difficulty;
        ] );
    ]
