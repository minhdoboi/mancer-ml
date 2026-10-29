(* Two seats around a Session: the setup a UI offers (who plays, which
   specials), dealing as Tournament.play_one does, and per-card move choices
   for a human. *)

open Sp

type seat = Human | Bot of string

type setup = {
  seats : seat * seat;
  specials : string * string;
  budget_ms : int;
  seed : int option;
  max_turns : int;
  starting : player_id;
  life : int option;
  player_life_bonus : int;  (** Extra life for the human side, as the easy campaign gives. *)
  mode : Catalog.mode;  (** Which basic houses are dealt: standard (anger) or classic (fire). *)
}

let default_setup =
  {
    seats = (Human, Bot "oracle");
    specials = ("Troopers", "Troopers");
    budget_ms = 1000;
    seed = None;
    max_turns = 300;
    starting = owner;
    life = None;
    player_life_bonus = 0;
    mode = Catalog.Standard;
  }

let seat_name = function Human -> "human" | Bot id -> id

(* Scala [Local] picks the duel starter with [Random.nextInt(2)]: a coin flip,
   not always the owner. Campaign fights keep their scripted starter. *)
let random_starter ?(rng = Random.State.make_self_init ()) () =
  if Random.State.bool rng then owner else opponent

(* Portrait rosters for the duel dialog, mirroring Scala [Portraits]. *)
let player_portraits =
  [
    "ember";
    "tide";
    "gale";
    "grove";
    "willow-wise";
    "bramble-bellow";
    "hootwise";
    "ash-wanderer";
    "mist-seer";
    "lamp-scribe";
    "river-aunt";
    "grove-crone";
    "reeve-an";
    "khan-jure";
    "scowl-master";
    "nox-tyro";
    "lotus";
    "ma-ji";
  ]

type opponent = { opp_bot : string; opp_kind : string; opp_portrait : string; opp_level : int }

(* The AI faces, in Scala [AiLevel] order. Gambler is not ported, so those
   faces still play lecturer. *)
let duel_opponents =
  [
    { opp_bot = "summoner"; opp_kind = "summoner"; opp_portrait = "summoner"; opp_level = 1 };
    { opp_bot = "summoner"; opp_kind = "summoner"; opp_portrait = "summoner-f"; opp_level = 1 };
    { opp_bot = "strategist"; opp_kind = "strategist"; opp_portrait = "strategist"; opp_level = 2 };
    { opp_bot = "strategist"; opp_kind = "strategist"; opp_portrait = "strategist-f"; opp_level = 2 };
    { opp_bot = "strategist"; opp_kind = "strategist"; opp_portrait = "strategist-2"; opp_level = 2 };
    { opp_bot = "lecturer"; opp_kind = "gambler"; opp_portrait = "gambler"; opp_level = 3 };
    { opp_bot = "lecturer"; opp_kind = "gambler"; opp_portrait = "gambler-f"; opp_level = 3 };
    { opp_bot = "lecturer"; opp_kind = "lecturer"; opp_portrait = "lecturer"; opp_level = 4 };
    { opp_bot = "lecturer"; opp_kind = "lecturer"; opp_portrait = "lecturer-f"; opp_level = 4 };
    { opp_bot = "oracle"; opp_kind = "oracle"; opp_portrait = "oracle"; opp_level = 5 };
    { opp_bot = "oracle"; opp_kind = "oracle"; opp_portrait = "oracle-f"; opp_level = 5 };
    { opp_bot = "diviner"; opp_kind = "diviner"; opp_portrait = "diviner"; opp_level = 6 };
    { opp_bot = "diviner"; opp_kind = "diviner"; opp_portrait = "diviner-f"; opp_level = 6 };
  ]

(* Kind faces first so lecturer is not mixed with the gambler stand-ins. *)
let faces_for_bot bot =
  match List.filter_map (fun o -> if o.opp_kind = bot then Some o.opp_portrait else None) duel_opponents with
  | [] -> (
      match
        List.filter_map (fun o -> if o.opp_bot = bot then Some o.opp_portrait else None) duel_opponents
      with
      | [] -> [ "summoner"; "summoner-f" ]
      | faces -> faces)
  | faces -> faces

let bot_of_portrait portrait =
  List.find_opt (fun o -> o.opp_portrait = portrait) duel_opponents |> Option.map (fun o -> o.opp_bot)

(* Special houses by author, in the Scala settings order ([House.sinist],
   ...). Only groups with ported houses appear, keyed by house key so newly
   ported houses land in their group on their own. *)
let house_groups =
  let by_key = List.map (fun (h : house) -> (Catalog.token h.h_key, h)) Catalog.specials in
  let find keys = List.filter_map (fun k -> List.assoc_opt (Catalog.token k) by_key) keys in
  List.filter_map
    (fun (author, keys) -> match find keys with [] -> None | hs -> Some (author, hs))
    [
      ( "sinist",
        [
          "darkpriest";
          "darksider";
          "elementalist";
          "fairy";
          "highpriest";
          "kinetician";
          "mountainking";
          "shaman";
          "sower";
          "vampires";
          "wind";
          "reaper";
        ] );
      ("Wavelength", [ "seasons"; "meteomancer"; "dreamweaver"; "quantumancer"; "faerie" ]);
      ("sfault", [ "conjurer" ]);
      ("Redrook", [ "tinker"; "soulbinder"; "entomologist"; "shadowcaster"; "spiritshaman"; "lightstave" ]);
      ("valentyne", [ "rogue"; "kahjivir" ]);
      ("fuzzer", [ "limbo" ]);
      ("Madhatter97", [ "colors" ]);
      ("lordvanmaru", [ "shadowmage" ]);
      ("tiapoka", [ "antimancer"; "powermancer" ]);
      ("CyberneticPony", [ "battlemage" ]);
      ("panderson", [ "chronomaster" ]);
      ("garcia1000", [ "Dudes"; "Troopers" ]);
      ("badsleep", [ "junk"; "disciples"; "snowblood"; "warp"; "zen" ]);
      ("Floofy", [ "emotion" ]);
      ("Zannoland", [ "paganist" ]);
      ("bZanno", [ "blacksmith" ]);
      ("zhupons", [ "trappers" ]);
      ("Fail", [ "beastlord" ]);
      ("DonnieDarkcore", [ "barbarians" ]);
    ]

let group_of_house key =
  List.find_map
    (fun (author, hs) ->
      if List.exists (fun (h : house) -> Catalog.token h.h_key = Catalog.token key) hs then Some author
      else None)
    house_groups

(* Game.init_cards must run once per house *)
let catalog = lazy (Catalog.init ())

let special_houses () =
  Lazy.force catalog;
  Catalog.specials

(* The campaign mode is the duel mode from the Game tab: a campaign is dealt
   from the same basics a duel is. *)

let campaign_setup ?(budget_ms = Prefs.campaign_difficulty_ms (Prefs.load ()))
    ?(difficulty = Prefs.campaign_difficulty (Prefs.load ())) ?seed ?mode ~player_special
    (n : Campaign.node) =
  match n.Campaign.fight with
  | None -> failwith ("campaign node has no fight: " ^ n.id)
  | Some f ->
      {
        default_setup with
        seats = (Human, Bot (Campaign.fight_bot ~difficulty n f));
        specials = (player_special, f.house_key);
        budget_ms;
        seed;
        starting = Campaign.starter_player n.id;
        life = Campaign.fight_life ~difficulty f;
        player_life_bonus = Campaign.player_life_bonus ~difficulty n;
        mode = Option.value mode ~default:(Catalog.mode_of_string (Prefs.setup_mode (Prefs.load ())));
      }

(* The player's campaign class: the special house the map's class selector
   stores. A stale or unknown name falls back to the default class. *)
let campaign_class_of name =
  match (try Some (Catalog.find_special name) with Failure _ -> None) with
  | Some h -> h.h_key
  | None -> Prefs.default_campaign_class

let campaign_class () = campaign_class_of (Prefs.campaign_class (Prefs.load ()))

(* Like Scala resolveChoices: a side plays a random house from its checked
   set; an empty set means every ported special. Unknown names are dropped. *)
let resolve_special =
  let rng = Random.State.make_self_init () in
  fun names ->
    let name (h : house) = h.h_name in
    let pool =
      match
        List.filter_map (fun n -> try Some (name (Catalog.find_special n)) with Failure _ -> None) names
      with
      | [] -> List.map name (special_houses ())
      | ok -> ok
    in
    List.nth pool (Random.State.int rng (List.length pool))

type t = {
  setup : setup;
  seed : int;
  session : Session.t;
  movers : Game.mover option array;
  mana : int list array;
  rng_seed : int;
}

let movers_of setup =
  let mover = function
    | Human -> None
    | Bot id -> Some (Tournament.mover ~stats_log:false ~budget:setup.budget_ms id)
  in
  [| mover (fst setup.seats); mover (snd setup.seats) |]

(* The seat the human sits in, if any: the life bonus follows the human, not
   the seat. *)
let human_pid = function (Human, _) -> Some owner | (_, Human) -> Some opponent | _ -> None

(* The life each side starts with: the setup's life for both of them, plus the
   player's bonus on the human's side. *)
let lives_of setup =
  let base = Option.value setup.life ~default:Game.default_life in
  match human_pid setup.seats with
  | Some p when p = owner -> (base + setup.player_life_bonus, base)
  | Some _ -> (base, base + setup.player_life_bonus)
  | None -> (base, base)

let open_session ?on_event setup ~seed ~desc ~mana ~rng_seed =
  let owner_life, opponent_life = lives_of setup in
  let session =
    Session.create desc ~mana0:mana.(0) ~mana1:mana.(1) ~rng:(Dice.make rng_seed) ~max_turns:setup.max_turns
      ~starting:setup.starting ~owner_life ~opponent_life ?on_event
  in
  { setup; seed; session; movers = movers_of setup; mana; rng_seed }

let start ?on_event (setup : setup) =
  Lazy.force catalog;
  let seed =
    match setup.seed with Some n -> n | None -> Random.State.bits (Random.State.make_self_init ())
  in
  let rng = Random.State.make [| seed |] in
  let s0, s1 = setup.specials in
  let specials = (Catalog.find_special s0, Catalog.find_special s1) in
  let p0, p1 = Shuffle.two_players ~rng ~starting:setup.starting ~specials (Catalog.basics_of setup.mode) in
  let desc = { gd_players = [| p0.desc; p1.desc |] } in
  let rng_seed = Random.State.bits rng in
  let mana = [| Array.to_list p0.mana; Array.to_list p1.mana |] in
  open_session ?on_event setup ~seed ~desc ~mana ~rng_seed

(* Scala Local.swapped: the same opening hands move across the table. Seats
   stay put; who starts flips unless this is a campaign fight. *)
let start_swapped ?on_event ?(keep_starter = false) ?budget_ms t =
  let s0, s1 = t.setup.specials in
  let starting = if keep_starter then t.setup.starting else other t.setup.starting in
  let budget_ms = Option.value budget_ms ~default:t.setup.budget_ms in
  let setup = { t.setup with specials = (s1, s0); starting; seed = Some t.seed; budget_ms } in
  let old = Session.desc t.session in
  let desc = { gd_players = [| old.gd_players.(1); old.gd_players.(0) |] } in
  let mana = [| t.mana.(1); t.mana.(0) |] in
  open_session ?on_event setup ~seed:t.seed ~desc ~mana ~rng_seed:t.rng_seed

let session t = t.session
let setup t = t.setup
let seed t = t.seed
let seat t pid = if pid = owner then fst t.setup.seats else snd t.setup.seats
let to_move t = match Session.status t.session with To_move pid -> Some pid | Over _ -> None
let hand t pid = Game.player_houses pid (Session.desc t.session) (Session.state t.session)

(* The deck as it was dealt.  A house can spend a card for good (Seasons: its
   specials may be used once apiece) and the engine then takes the card out of
   the deck; the rows of the hand come from the dealt deck, or a card the
   opponent played would leave the column it was played from. *)
let dealt_hand t pid = Array.to_list (Session.desc t.session).gd_players.(pid).pd_houses

(* The cards of one house in the rows the hand draws them: the dealt deck
   gives the row of every card, the live deck the cost of a card that is
   still in play. *)
let house_rows (live : player_house_desc) (dealt : player_house_desc) =
  let live_of key = Array.find_opt (fun l -> l.cd_card.key = key) live.phd_cards in
  let in_dealt key = Array.exists (fun d -> d.cd_card.key = key) dealt.phd_cards in
  (* The cards the deck gained: one that replaces another (the Dark Monk for
     the False Prophet). *)
  let gained = Array.to_list live.phd_cards |> List.filter (fun l -> not (in_dealt l.cd_card.key)) in
  (* A card that replaces another takes its row: the Dark Monk in the deck
     says the False Prophet was played.  A card that is simply gone keeps its
     row, disabled, so the board still shows what the opponent played. *)
  let rec rows gained = function
    | [] -> []
    | d :: rest ->
        let cd, gained =
          match live_of d.cd_card.key with
          | Some l -> (l, gained)
          | None -> (
              match gained with
              | g :: gs -> (g, gs)
              | [] -> ({ d with cd_enabled = false }, gained))
        in
        cd :: rows gained rest
  in
  let rows = rows gained (Array.to_list dealt.phd_cards) in
  (* A card the deck gained and that replaced nothing has no row of its own:
     it goes last. *)
  let extra =
    Array.to_list live.phd_cards
    |> List.filter (fun l -> not (List.exists (fun r -> r.cd_card.key = l.cd_card.key) rows))
  in
  Array.of_list (rows @ extra)

(* The hand as a board draws it: every card keeps the row it was dealt at,
   even the ones the houses took away. *)
let hand_rows t pid =
  let dealt = dealt_hand t pid in
  List.map
    (fun (live : player_house_desc) ->
      match
        List.find_opt (fun (d : player_house_desc) -> d.phd_house.h_index = live.phd_house.h_index) dealt
      with
      | Some d -> { live with phd_cards = house_rows live d }
      | None -> live)
    (hand t pid)

let playable t cd h =
  match to_move t with
  | None -> false
  | Some pid -> is_available cd (Session.state t.session).players.(pid).ps_houses.(h.phd_house.h_index)

let choices t (card : card) =
  match to_move t with
  | None -> []
  | Some pid ->
      Game.legal_commands pid (Session.desc t.session) (Session.state t.session)
      |> List.filter (fun c -> c.cmd_card.key = card.key)
      |> List.sort (fun a b -> compare (a.cmd_flag, a.cmd_input) (b.cmd_flag, b.cmd_input))

let target_side (c : command) =
  match c.cmd_input with
  | None -> None
  | Some _ -> (
      match c.cmd_card.input_spec with
      | Select_target_slot | Select_target_creature | Select_target _ -> Some (other c.cmd_player)
      | Select_any_creature when c.cmd_flag = Some Target_other -> Some (other c.cmd_player)
      | _ -> Some c.cmd_player)

let submit t cmd = Session.submit t.session cmd

let bot_command t =
  match to_move t with
  | None -> invalid_arg "Table.bot_command: the game is over"
  | Some pid -> (
      match t.movers.(pid) with
      | None -> invalid_arg "Table.bot_command: a human is to move"
      | Some mover ->
          let u = Session.updater t.session in
          let p = Engine.Updater.player u pid in
          if is_disabled p.pstate then None else mover u pid)

let play_bot t = submit t (bot_command t)

let record t : Game_record.t =
  let desc = Session.desc t.session in
  let winner, reason =
    match Session.status t.session with
    | Over { winner; reason } -> (winner, reason)
    | To_move _ -> (None, "unfinished")
  in
  let name pid = seat_name (seat t pid) in
  {
    game = Printf.sprintf "tui-%d" t.seed;
    bot_names = (name owner, name opponent);
    starting_player = t.setup.starting;
    special_names =
      (Game_record.special_name desc.gd_players.(0), Game_record.special_name desc.gd_players.(1));
    decks = desc.gd_players;
    mana = t.mana;
    rng_seed = t.rng_seed;
    winner_bot = Option.map name winner;
    winner_player = winner;
    draw = Option.is_none winner;
    turns = Session.turns t.session;
    reason;
    moves = Session.moves t.session;
  }

let event_line ?(name = fun (c : card) -> c.name) =
  let p pid = Printf.sprintf "P%d" (pid + 1) in
  function
  | Ev_to_move { t; player } -> Some (Printf.sprintf "-- turn %d: %s to move" t (p player))
  | Ev_submitted { player; command = None; _ } -> Some (Printf.sprintf "%s passes" (p player))
  | Ev_submitted { player; command = Some c; _ } ->
      let at =
        match (c.cmd_input, target_side c) with
        | Some n, Some side -> Printf.sprintf " at %s slot %d" (p side) n
        | _ -> ""
      in
      Some (Printf.sprintf "%s plays %s (%d)%s" (p player) (name c.cmd_card) c.cmd_cost at)
  | Ev_summon { num; player; creature } ->
      Some (Printf.sprintf "%s summons %s in slot %d" (p player) (name creature.c_card) num)
  | Ev_replaced { num; player; creature } ->
      Some (Printf.sprintf "%s slot %d becomes %s" (p player) num (name creature.c_card))
  | Ev_die { num; player } -> Some (Printf.sprintf "%s slot %d dies" (p player) num)
  | Ev_move { num; dest; player; dest_player } ->
      Some (Printf.sprintf "%s slot %d moves to %s slot %d" (p player) num (p dest_player) dest)
  | Ev_swap { num; dest; player; dest_player } ->
      Some (Printf.sprintf "%s slot %d swaps with %s slot %d" (p player) num (p dest_player) dest)
  | Ev_ability { kind; player; num; target } ->
      let on = match target with Some (tp, tn) -> Printf.sprintf " on %s slot %d" (p tp) tn | None -> "" in
      Some (Printf.sprintf "%s slot %d: %s%s" (p player) num kind on)
  | Ev_mana { player; num; target; deltas } ->
      let moves =
        deltas
        |> List.map (fun (i, d) -> Printf.sprintf "%+d h%d" d i)
        |> String.concat ", "
      in
      Some (Printf.sprintf "%s slot %d mana for %s: %s" (p player) num (p target) moves)
  | Ev_game_over { winner = Some w; reason } -> Some (Printf.sprintf "== %s wins (%s)" (p w) reason)
  | Ev_game_over { winner = None; reason } -> Some (Printf.sprintf "== draw (%s)" reason)
  | Ev_spell_played _ | Ev_run_slot _ | Ev_focus _ | Ev_refresh _ -> None

let save t ~dir =
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let path = Filename.concat dir (Printf.sprintf "game-%d-%.0f.json" t.seed (Unix.gettimeofday ())) in
  Out_channel.with_open_text path (fun oc -> output_string oc (Game_record.to_json (record t)));
  path

(* ---- lobby (shared by the TUI and the GUI) ---- *)

let budget_options = [ 100; 250; 500; 1000; 2000; 5000 ]
let languages = [ "en"; "fr"; "es"; "ru"; "vi" ]
let faces_for_seat = function Human -> player_portraits | Bot id -> faces_for_bot id

let clamp_portrait seat id =
  let ids = faces_for_seat seat in
  if List.mem id ids then id else List.hd ids

let seat_of_string ~fallback s =
  match Prefs.parse_seat ~allowed:Tournament.bot_ids ~fallback s with `Human -> Human | `Bot id -> Bot id

let duel_of_prefs ?(bot_opponent = false) prefs =
  let fb0, fb1 =
    match default_setup.seats with
    | Human, Bot id -> (`Human, `Bot id)
    | Bot a, Bot b -> (`Bot a, `Bot b)
    | Bot a, Human -> (`Bot a, `Human)
    | Human, Human -> (`Human, `Human)
  in
  let s0, s1 = Prefs.setup_seats prefs in
  let p0, p1 = Prefs.setup_portraits prefs in
  let seat0 = seat_of_string ~fallback:fb0 s0 in
  let ai_face = Prefs.ai_opponent prefs in
  (* The AI side follows its saved face and that face's bot. *)
  let seat1, port1 =
    match bot_of_portrait ai_face with
    | Some b when List.mem b Tournament.bot_ids -> (Bot b, ai_face)
    | _ ->
        let seat =
          match (seat_of_string ~fallback:fb1 s1, bot_opponent) with
          | Human, true -> Bot "oracle"
          | seat, _ -> seat
        in
        (seat, p1)
  in
  let port1 = clamp_portrait seat1 port1 in
  ((seat0, clamp_portrait seat0 p0), (seat1, port1))

let persist_duel ((seat0, port0), (seat1, port1)) =
  let p = Prefs.load () in
  Prefs.set_setup_seats p (seat_name seat0, seat_name seat1);
  Prefs.set_setup_portraits p (port0, port1);
  match seat1 with Bot _ -> Prefs.set_ai_opponent p port1 | Human -> ()

(* ---- card play (shared by the TUI and the GUI) ---- *)

let human_to_move t = match to_move t with Some pid when seat t pid = Human -> Some pid | _ -> None

type pick = Not_enough_mana | No_target | Play of command | Choose of command list

let pick_card t (cd : card_desc) hd =
  if not (playable t cd hd) then Not_enough_mana
  else
    match choices t cd.cd_card with
    | [] -> No_target
    | [ c ] when match cd.cd_card.input_spec with Select_target _ -> false | _ -> true -> Play c
    | cmds -> Choose cmds
