(* Terminal UI for playing or watching a game: a setup screen (seats,
   specials, bot budget, seed), then the board. Built on Table, so a graphical
   front end can share everything but the drawing. *)

open Notty
open Notty_unix
open Mancer
open Sp

(* ------------------------------------------------------------- card text *)

let clean = Assets.clean
let card_name = Assets.card_name
let describe = Assets.describe

let wrap width text =
  let words = String.split_on_char ' ' text |> List.filter (( <> ) "") in
  let lines, cur =
    List.fold_left
      (fun (lines, cur) w ->
        if cur = "" then (lines, w)
        else if String.length cur + 1 + String.length w <= width then (lines, cur ^ " " ^ w)
        else (cur :: lines, w))
      ([], "") words
  in
  List.rev (if cur = "" then lines else cur :: lines)

(* ---------------------------------------------------------------- drawing *)

let dim = A.(fg lightblack)
let bold = A.(st bold)
let rev = A.(st reverse)
let accent = A.(fg lightcyan)
let warn = A.(fg lightyellow)
let bad = A.(fg lightred)
let good = A.(fg lightgreen)

(* exactly [w] columns: cropped or padded *)
let text ?(attr = A.empty) w s = I.hsnap ~align:`Left w (I.string attr (clean s))
let lines attr w ss = I.vcat (List.map (text ~attr w) ss)
let cap s = String.capitalize_ascii s
let player_label pid = Printf.sprintf "P%d" (pid + 1)

(* --------------------------------------------------------------- lobby ui *)

let seat_options = Table.Human :: List.map (fun id -> Table.Bot id) Tournament.bot_ids
let budget_options = [ 100; 250; 500; 1000; 2000; 5000 ]
let languages = [ "en"; "fr"; "es"; "ru"; "vi" ]

let cycle options current delta =
  let a = Array.of_list options in
  let n = Array.length a in
  let i = Option.value ~default:0 (Array.find_index (( = ) current) a) in
  a.((((i + delta) mod n) + n) mod n)

let special_names () = List.map (fun (h : house) -> cap h.h_name) (Table.special_houses ())
let faces_for = function Table.Human -> Table.player_portraits | Table.Bot id -> Table.faces_for_bot id

let clamp_face seat id =
  let ids = faces_for seat in
  if List.mem id ids then id else List.hd ids

(* Duel dialog: seats with portraits (a bot seat wears its own faces). *)
type duel_ui = {
  mutable seat0 : Table.seat;
  mutable seat1 : Table.seat;
  mutable port0 : string;
  mutable port1 : string;
  mutable drow : int;
}

(* Game settings: house choices, bot budget, seed and language. *)
type settings_ui = {
  mutable sp0 : string;
  mutable sp1 : string;
  mutable budget : int;
  mutable seed_text : string;
  mutable lang : string;
  mutable srow : int;
}

type lobby_ui = { mutable row : int; duel : duel_ui; gs : settings_ui }

let seat_of_string ~fallback s =
  match Prefs.parse_seat ~allowed:Tournament.bot_ids ~fallback s with
  | `Human -> Table.Human
  | `Bot id -> Table.Bot id

let valid_special s fb =
  try
    ignore (Catalog.find_special s);
    s
  with Failure _ -> fb

let settings_from_prefs () =
  let prefs = Prefs.load () in
  let d = Table.default_setup in
  let l0, l1 = Prefs.setup_specials prefs in
  let ds0, ds1 = d.specials in
  (* The TUI picks a single house per side; a set written by the GUI shows its first pick. *)
  let head fb = function h :: _ -> valid_special h fb | [] -> fb in
  {
    sp0 = head ds0 l0;
    sp1 = head ds1 l1;
    budget = Prefs.setup_budget prefs;
    seed_text = "";
    lang = Prefs.language prefs;
    srow = 0;
  }

let duel_from_prefs () =
  let prefs = Prefs.load () in
  let d = Table.default_setup in
  let s0, s1 = Prefs.setup_seats prefs in
  let fb0, fb1 =
    match d.seats with
    | Table.Human, Table.Bot id -> (`Human, `Bot id)
    | Table.Bot a, Table.Bot b -> (`Bot a, `Bot b)
    | Table.Bot a, Table.Human -> (`Bot a, `Human)
    | Table.Human, Table.Human -> (`Human, `Human)
  in
  let p0, p1 = Prefs.setup_portraits prefs in
  let seat0 = seat_of_string ~fallback:fb0 s0 in
  (* The AI side follows its saved face and the face's bot. *)
  let seat1, port1 =
    match Table.bot_of_portrait (Prefs.ai_opponent prefs) with
    | Some b when List.mem b Tournament.bot_ids ->
        let seat = Table.Bot b in
        (seat, clamp_face seat (Prefs.ai_opponent prefs))
    | _ ->
        let seat = seat_of_string ~fallback:fb1 s1 in
        (seat, clamp_face seat p1)
  in
  { seat0; seat1; port0 = clamp_face seat0 p0; port1; drow = 0 }

let new_lobby () = { row = 1; duel = duel_from_prefs (); gs = settings_from_prefs () }

let persist_duel lobby =
  try
    let d = lobby.duel in
    let p = Prefs.load () in
    Prefs.set_setup_seats p (Table.seat_name d.seat0, Table.seat_name d.seat1);
    Prefs.set_setup_portraits p (d.port0, d.port1);
    match d.seat1 with Table.Bot _ -> Prefs.set_ai_opponent p d.port1 | Table.Human -> ()
  with _ -> ()

let menu_row current i ~enabled label =
  let selected = current = i in
  let mark = if selected then "> " else "  " in
  I.string (if not enabled then dim else if selected then accent else A.empty) (mark ^ label)

let render_lobby (w, _) ui =
  I.vcat
    [
      I.string A.(st bold ++ fg lightcyan) (clean (Assets.text "lobby.title"));
      text ~attr:dim w (Assets.text "lobby.message");
      I.void 1 1;
      menu_row ui.row 0 ~enabled:true (Assets.text "button.campaign");
      menu_row ui.row 1 ~enabled:true (Assets.text "button.duel");
      menu_row ui.row 2 ~enabled:true (Assets.text "button.settings");
      menu_row ui.row 3 ~enabled:true (Assets.text "button.quit");
      I.void 1 1;
      text ~attr:dim w "up/down select   enter open   q quit";
    ]
  |> I.pad ~l:2 ~t:1

let duel_rows = 6

let duel_row drow i label value =
  let selected = drow = i in
  let mark = if selected then "> " else "  " in
  let value_attr = if selected then rev else A.empty in
  I.(
    string (if selected then accent else A.empty) (mark ^ label)
    |> hsnap ~align:`Left 16
    <|> string value_attr (" " ^ value ^ " ")
    <|> if selected && i < 4 then string dim "  <- ->" else empty)

let render_duel (w, _) ui =
  let d = ui.duel in
  let go i label =
    let selected = d.drow = i in
    I.(
      string (if selected then accent else A.empty) (if selected then "> " else "  ")
      <|> string (if selected then A.(rev ++ st bold) else bold) (" " ^ label ^ " "))
  in
  I.vcat
    [
      I.string A.(st bold ++ fg lightcyan) (clean (Assets.text "button.duel"));
      I.void 1 1;
      duel_row d.drow 0 "P1 seat" (Table.seat_name d.seat0);
      duel_row d.drow 1 "P1 portrait" d.port0;
      duel_row d.drow 2 "P2 seat" (Table.seat_name d.seat1);
      duel_row d.drow 3 "P2 portrait" d.port1;
      I.void 1 1;
      go 4 (Assets.text "button.fight");
      go 5 (Assets.text "button.back");
      I.void 1 1;
      text ~attr:dim w "up/down select   left/right change   enter fight   esc back";
    ]
  |> I.pad ~l:2 ~t:1

let change_duel ui delta =
  let d = ui.duel in
  match d.drow with
  | 0 ->
      d.seat0 <- cycle seat_options d.seat0 delta;
      d.port0 <- clamp_face d.seat0 d.port0
  | 1 -> d.port0 <- cycle (faces_for d.seat0) d.port0 delta
  | 2 ->
      d.seat1 <- cycle seat_options d.seat1 delta;
      d.port1 <- clamp_face d.seat1 d.port1
  | 3 -> d.port1 <- cycle (faces_for d.seat1) d.port1 delta
  | _ -> ()

let settings_rows = 6
let special_label sp = match Table.group_of_house sp with Some g -> cap sp ^ " (" ^ g ^ ")" | None -> cap sp

let render_settings (w, _) ui =
  let gs = ui.gs in
  let seed = if gs.seed_text = "" then "random" else gs.seed_text in
  I.vcat
    [
      I.string A.(st bold ++ fg lightcyan) (clean (Assets.text "settings.title"));
      I.void 1 1;
      duel_row gs.srow 0 "P1 special" (special_label gs.sp0);
      duel_row gs.srow 1 "P2 special" (special_label gs.sp1);
      duel_row gs.srow 2 "Bot budget" (Printf.sprintf "%d ms" gs.budget);
      duel_row gs.srow 3 "Seed" (seed ^ if gs.srow = 3 then "_" else "");
      duel_row gs.srow 4 "Language" gs.lang;
      duel_row gs.srow 5 "Back" "";
      I.void 1 1;
      text ~attr:dim w (Assets.text "settings.lang.restart");
      I.void 1 1;
      text ~attr:dim w "up/down select   left/right change   0-9 seed   enter back   esc back";
    ]
  |> I.pad ~l:2 ~t:1

let persist_settings gs =
  try
    let p = Prefs.load () in
    Prefs.set_setup_specials p ([ gs.sp0 ], [ gs.sp1 ]);
    Prefs.set_setup_budget p gs.budget;
    Prefs.set_language p gs.lang
  with _ -> ()

let change_settings ui delta =
  let gs = ui.gs in
  (match gs.srow with
  | 0 -> gs.sp0 <- cycle (special_names ()) (cap gs.sp0) delta
  | 1 -> gs.sp1 <- cycle (special_names ()) (cap gs.sp1) delta
  | 2 -> gs.budget <- cycle budget_options gs.budget delta
  | 4 -> gs.lang <- cycle languages gs.lang delta
  | _ -> ());
  persist_settings gs

(* ---------------------------------------------------------------- game ui *)

type mode = Browse | Target of { cmds : command array; mutable i : int }

type game_ui = {
  table : Table.t;
  log : string list ref; (* newest first *)
  mutable col : int;
  mutable card : int;
  mutable mode : mode;
  mutable paused : bool;
  mutable message : string;
  campaign : Campaign.node option;
}

type campaign_ui = { mutable i : int; mutable message : string }

let human_to_move ui =
  match Table.to_move ui.table with Some pid when Table.seat ui.table pid = Human -> Some pid | _ -> None

let bot_to_move ui =
  match Table.to_move ui.table with
  | Some pid -> ( match Table.seat ui.table pid with Bot id -> Some (pid, id) | Human -> None)
  | None -> None

(* The hand shown: the human to move, else P1's *)
let hand_pid ui = Option.value (human_to_move ui) ~default:owner
let hand ui = Array.of_list (Table.hand ui.table (hand_pid ui))

let selected_card ui =
  let h = hand ui in
  if ui.col < Array.length h then
    let cards = h.(ui.col).phd_cards in
    if ui.card < Array.length cards then Some (cards.(ui.card), h.(ui.col)) else None
  else None

let new_game ?(campaign = None) setup =
  let log = ref [] in
  let on_event e = Option.iter (fun l -> log := l :: !log) (Table.event_line ~name:card_name e) in
  let table = Table.start ~on_event setup in
  { table; log; col = 0; card = 0; mode = Browse; paused = false; message = ""; campaign }

let fight lobby =
  persist_duel lobby;
  let d = lobby.duel and gs = lobby.gs in
  let seed = if gs.seed_text = "" then None else int_of_string_opt gs.seed_text in
  let dflt = Table.default_setup in
  new_game { dflt with seats = (d.seat0, d.seat1); specials = (gs.sp0, gs.sp1); budget_ms = gs.budget; seed }

let flag_names st =
  [
    (stun_flag, "stun");
    (blocked_flag, "block");
    (paused_flag, "pause");
    (invincible_flag, "invinc");
    (cursed_flag, "curse");
  ]
  |> List.filter_map (fun (f, n) -> if has_flag st f then Some n else None)
  |> String.concat " "

let render_slot ui ~cw pid num (p : player_state) =
  let inner = cw - 2 in
  let target =
    match ui.mode with
    | Browse -> `None
    | Target { cmds; i } ->
        let hit c = c.cmd_input = Some num && Table.target_side c = Some pid in
        if hit cmds.(i) then `Current else if Array.exists hit cmds then `Option else `None
  in
  let frame =
    match target with `Current -> A.(fg lightyellow ++ st bold) | `Option -> warn | `None -> dim
  in
  let body =
    if not (List.mem num p.ps_slot_list) then [ text ~attr:dim inner "  x"; I.void inner 1; I.void inner 1 ]
    else
      match p.ps_slots.(num) with
      | None ->
          let hint =
            match target with
            | `Current -> text ~attr:rev inner "  > here"
            | `Option -> text ~attr:warn inner "  (empty)"
            | `None -> I.void inner 1
          in
          [ hint; I.void inner 1; I.void inner 1 ]
      | Some st ->
          let name_attr = if target = `Current then rev else bold in
          [
            text ~attr:name_attr inner (card_name st.card.c_card);
            I.(
              string bad (Printf.sprintf "%d" st.attack)
              <|> string dim " atk  "
              <|> string good (Printf.sprintf "%d" st.life)
              <|> string dim (Printf.sprintf "/%d" st.max_life))
            |> I.hsnap ~align:`Left inner;
            text ~attr:warn inner (flag_names st);
          ]
  in
  let bar l r = I.string frame (l ^ String.concat "" (List.init inner (fun _ -> "─")) ^ r) in
  let side = I.vcat (List.init 3 (fun _ -> I.string frame "│")) in
  I.(bar "┌" "┐" <-> (side <|> vcat body <|> side) <-> bar "└" "┘")

let render_player ui ~cw ~w pid =
  let s = Session.state (Table.session ui.table) in
  let desc = Session.desc (Table.session ui.table) in
  let p = s.players.(pid) in
  let seat = Table.seat ui.table pid in
  let special = Game_record.special_name desc.gd_players.(pid) in
  let moving = Table.to_move ui.table = Some pid in
  let mana =
    Array.to_list desc.gd_players.(pid).pd_houses
    |> List.map (fun h ->
        I.(
          string dim (" " ^ cap h.phd_house.h_name ^ " ")
          <|> string accent (string_of_int p.ps_houses.(h.phd_house.h_index).mana)))
  in
  let header =
    I.hcat
      ([
         I.string (if moving then A.(st bold ++ fg lightcyan) else bold) (player_label pid);
         I.string dim (Printf.sprintf " %s · %s  " (Table.seat_name seat) (cap special));
         I.string dim "life ";
         I.string
           A.(st bold ++ if p.ps_life <= 15 then fg lightred else fg lightgreen)
           (string_of_int p.ps_life);
         I.string dim "  ";
       ]
      @ mana
      @ [ (if moving then I.string accent "  ◀ to move" else I.empty) ])
    |> I.hsnap ~align:`Left w
  in
  let slots = I.hcat (List.init nb_slots (fun n -> render_slot ui ~cw pid n p)) in
  (header, slots)

let render_hand ui ~w =
  let h = hand ui in
  let pid = hand_pid ui in
  let st = Session.state (Table.session ui.table) in
  let can_choose = Option.is_some (human_to_move ui) in
  let n = max 1 (Array.length h) in
  let hw = w / n in
  let column ci (hd : player_house_desc) =
    let mana = st.players.(pid).ps_houses.(hd.phd_house.h_index).mana in
    let head = I.(string bold (cap hd.phd_house.h_name) <|> string accent (Printf.sprintf " %d" mana)) in
    let card ki cd =
      let playable = can_choose && Table.playable ui.table cd hd in
      let cursor = can_choose && ci = ui.col && ki = ui.card in
      let attr = if cursor then rev else if playable then A.empty else dim in
      let label = Printf.sprintf " %-*s%2d " (max 1 (hw - 5)) (card_name cd.cd_card) cd.cd_cost in
      text ~attr (hw - 1) label
    in
    I.(hsnap ~align:`Left hw head <-> vcat (List.mapi card (Array.to_list hd.phd_cards)))
  in
  I.hcat (List.mapi column (Array.to_list h))

let input_label = function
  | No_input -> ""
  | Select_owner_slot -> "target: your empty slot"
  | Select_owner_creature -> "target: your creature"
  | Select_target_slot -> "target: enemy empty slot"
  | Select_target_creature -> "target: enemy creature"
  | Select_any_creature -> "target: any creature"
  | Select_owner _ -> "target: your slot"
  | Select_target _ -> "target: enemy slot"

let render_card_info ui ~w =
  match (human_to_move ui, selected_card ui) with
  | Some _, Some (cd, _) ->
      let c = cd.cd_card in
      let kind =
        match c.creature_body with
        | Some b ->
            Printf.sprintf "creature  %s atk  %d life"
              (match b.b_attack.base with Some a -> string_of_int a | None -> "?")
              b.b_life
        | None -> "spell"
      in
      let head =
        I.(
          string bold (card_name c)
          <|> string dim (Printf.sprintf "  cost %d  %s  " cd.cd_cost kind)
          <|> string warn (input_label c.input_spec))
      in
      I.(
        hsnap ~align:`Left w head <-> lines A.empty w (List.filteri (fun i _ -> i < 3) (wrap w (describe c))))
  | _ -> I.empty

let status_line ui =
  match Session.status (Table.session ui.table) with
  | Over { winner; reason } ->
      let who =
        match winner with
        | Some pid ->
            Printf.sprintf "%s (%s) wins" (player_label pid) (Table.seat_name (Table.seat ui.table pid))
        | None -> "Draw"
      in
      I.string
        A.(st bold ++ fg lightgreen)
        (Printf.sprintf "%s — %s after %d turns" who reason (Session.turns (Table.session ui.table)))
  | To_move pid -> (
      match Table.seat ui.table pid with
      | Bot id when ui.paused ->
          I.string warn (Printf.sprintf "paused — %s (%s) to move" (player_label pid) id)
      | Bot id -> I.string accent (Printf.sprintf "%s (%s) is thinking…" (player_label pid) id)
      | Human -> (
          match ui.mode with
          | Target _ -> I.string accent "choose a target"
          | Browse ->
              let playable h = Array.exists (fun cd -> Table.playable ui.table cd h) h.phd_cards in
              if not (List.exists playable (Table.hand ui.table pid)) then
                I.string warn (Printf.sprintf "%s: nothing playable — p to pass" (player_label pid))
              else I.string accent (Printf.sprintf "%s: your move" (player_label pid))))

let keys_line ui =
  let k =
    match (Session.status (Table.session ui.table), ui.mode) with
    | Over _, _ -> "r replay seed   n new deal   w save game   l lobby   q quit"
    | _, Target _ -> "arrows/0-5 choose   enter play   esc cancel"
    | _, Browse ->
        let both_bots = Option.is_none (human_to_move ui) in
        if both_bots then "space pause/resume   w save game   l lobby   q quit"
        else "arrows select   enter play   p pass   w save   l lobby   q quit"
  in
  I.string dim k

let render_game (w, h) ui =
  let main_w = min w 110 in
  let cw = max 10 (min 18 (main_w / nb_slots)) in
  let board_w = cw * nb_slots in
  let opp_head, opp_slots = render_player ui ~cw ~w:board_w opponent in
  let own_head, own_slots = render_player ui ~cw ~w:board_w owner in
  let numbers = I.hcat (List.init nb_slots (fun n -> I.hsnap cw (I.string dim (string_of_int n)))) in
  let seed =
    I.string dim
      (Printf.sprintf "seed %d   turn %d" (Table.seed ui.table) (Session.turns (Table.session ui.table)))
  in
  let main =
    I.vcat
      [
        seed;
        opp_head;
        opp_slots;
        numbers;
        own_slots;
        own_head;
        I.void 1 1;
        render_hand ui ~w:board_w;
        I.void 1 1;
        render_card_info ui ~w:board_w;
        I.void 1 1;
        status_line ui;
        text ~attr:bad board_w ui.message;
        keys_line ui;
      ]
  in
  let log_lines n = List.filteri (fun i _ -> i < n) !(ui.log) |> List.rev in
  let side_w = w - board_w - 3 in
  if side_w >= 28 then
    let log = I.(string bold "Log" <-> lines dim side_w (log_lines (h - 2))) in
    I.(pad ~l:1 main <|> pad ~l:2 log)
  else
    let rest = h - I.height main - 1 in
    I.(pad ~l:1 (main <-> lines dim (w - 2) (log_lines (max 0 rest))))

(* ------------------------------------------------------------- actions *)

let save_record (ui : game_ui) = ui.message <- "saved " ^ Table.save ui.table ~dir:"tui-games"

let guarded (ui : game_ui) f =
  try
    ui.message <- "";
    f ()
  with e ->
    ui.paused <- true;
    ui.mode <- Browse;
    ui.message <- "error: " ^ Printexc.to_string e

let play ui cmd =
  guarded ui (fun () ->
      ui.mode <- Browse;
      Table.submit ui.table cmd)

let activate ui =
  match (human_to_move ui, selected_card ui) with
  | Some _, Some (cd, hd) -> (
      if not (Table.playable ui.table cd hd) then ui.message <- "not enough mana"
      else
        match Table.choices ui.table cd.cd_card with
        | [] -> ui.message <- "no legal target"
        | [ ({ cmd_input = None; _ } as c) ] -> play ui (Some c)
        | cmds -> ui.mode <- Target { cmds = Array.of_list cmds; i = 0 })
  | _ -> ()

let move_cursor ui dcol dcard =
  let h = hand ui in
  let n = Array.length h in
  if n > 0 then (
    ui.col <- (ui.col + dcol + n) mod n;
    let m = Array.length h.(ui.col).phd_cards in
    if m > 0 then ui.card <- (min ui.card (m - 1) + dcard + m) mod m)

type screen =
  | Lobby of lobby_ui
  | Duel of lobby_ui
  | Settings of lobby_ui
  | Campaign of campaign_ui
  | Game of game_ui
  | Quit

let campaign_open () =
  let p = Prefs.load () in
  Campaign.available (Prefs.campaign_completed p) |> List.filter_map Campaign.by_id

let new_campaign () = { i = 0; message = "" }

let campaign_fight node =
  let p = Prefs.load () in
  let player_special = Table.resolve_special (fst (Prefs.setup_specials p)) in
  let setup = Table.campaign_setup ~budget_ms:(Prefs.setup_budget p) ~player_special node in
  new_game ~campaign:(Some node) setup

let on_lobby_key ui = function
  | `Key (`Arrow `Up, _) ->
      ui.row <- (if ui.row <= 0 then 3 else ui.row - 1);
      Lobby ui
  | `Key (`Arrow `Down, _) | `Key (`Tab, _) ->
      ui.row <- (if ui.row >= 3 then 0 else ui.row + 1);
      Lobby ui
  | `Key (`Enter, _) -> (
      match ui.row with 0 -> Campaign (new_campaign ()) | 1 -> Duel ui | 2 -> Settings ui | _ -> Quit)
  | `Key (`ASCII 'q', _) | `Key (`Escape, _) -> Quit
  | _ -> Lobby ui

let on_duel_key ui = function
  | `Key (`Arrow `Up, _) ->
      ui.duel.drow <- (ui.duel.drow + duel_rows - 1) mod duel_rows;
      Duel ui
  | `Key (`Arrow `Down, _) ->
      ui.duel.drow <- (ui.duel.drow + 1) mod duel_rows;
      Duel ui
  | `Key (`Arrow `Left, _) ->
      change_duel ui (-1);
      Duel ui
  | `Key (`Arrow `Right, _) ->
      change_duel ui 1;
      Duel ui
  | `Key (`Enter, _) -> (
      match ui.duel.drow with
      | 4 -> ( match fight ui with game -> Game game | exception Failure _ -> Duel ui)
      | 5 -> Lobby ui
      | _ ->
          change_duel ui 1;
          Duel ui)
  | `Key (`Escape, _) -> Lobby ui
  | _ -> Duel ui

let on_settings_key ui = function
  | `Key (`Arrow `Up, _) ->
      ui.gs.srow <- (ui.gs.srow + settings_rows - 1) mod settings_rows;
      Settings ui
  | `Key (`Arrow `Down, _) ->
      ui.gs.srow <- (ui.gs.srow + 1) mod settings_rows;
      Settings ui
  | `Key (`Arrow `Left, _) ->
      change_settings ui (-1);
      Settings ui
  | `Key (`Arrow `Right, _) ->
      change_settings ui 1;
      Settings ui
  | `Key (`ASCII ('0' .. '9' as c), _) when ui.gs.srow = 3 ->
      if String.length ui.gs.seed_text < 9 then ui.gs.seed_text <- ui.gs.seed_text ^ String.make 1 c;
      Settings ui
  | `Key (`Backspace, _) when ui.gs.srow = 3 ->
      let n = String.length ui.gs.seed_text in
      if n > 0 then ui.gs.seed_text <- String.sub ui.gs.seed_text 0 (n - 1);
      Settings ui
  | `Key (`Enter, _) -> if ui.gs.srow = 5 then Lobby ui else Settings ui
  | `Key (`Escape, _) -> Lobby ui
  | _ -> Settings ui

let on_game_key ui ev =
  let over = Option.is_none (Table.to_move ui.table) in
  match (ui.mode, ev) with
  | _, `Key (`ASCII 'q', _) -> Quit
  | _, `Key (`ASCII 'l', _) -> (
      match ui.campaign with Some _ -> Campaign (new_campaign ()) | None -> Lobby (new_lobby ()))
  | _, `Key (`ASCII 'w', _) ->
      guarded ui (fun () -> save_record ui);
      Game ui
  | _, `Key (`ASCII 'r', _) when over ->
      Game
        (new_game ~campaign:ui.campaign
           (Table.setup ui.table |> fun s -> { s with seed = Some (Table.seed ui.table) }))
  | _, `Key (`ASCII 'n', _) when over ->
      Game (new_game ~campaign:ui.campaign { (Table.setup ui.table) with seed = None })
  | Target t, `Key (`Arrow (`Left | `Up), _) ->
      t.i <- (t.i + Array.length t.cmds - 1) mod Array.length t.cmds;
      Game ui
  | Target t, `Key (`Arrow (`Right | `Down), _) ->
      t.i <- (t.i + 1) mod Array.length t.cmds;
      Game ui
  | Target t, `Key (`ASCII ('0' .. '5' as c), _) ->
      let num = Char.code c - Char.code '0' in
      let n = Array.length t.cmds in
      (* from the one after the current, so a second press reaches the other side's slot *)
      (match
         List.find_opt (fun k -> t.cmds.((t.i + k) mod n).cmd_input = Some num) (List.init n (fun k -> k + 1))
       with
      | Some k -> t.i <- (t.i + k) mod n
      | None -> ());
      Game ui
  | Target t, `Key (`Enter, _) ->
      play ui (Some t.cmds.(t.i));
      Game ui
  | Target _, `Key (`Escape, _) ->
      ui.mode <- Browse;
      Game ui
  | Browse, `Key (`Arrow `Left, _) ->
      move_cursor ui (-1) 0;
      Game ui
  | Browse, `Key (`Arrow `Right, _) ->
      move_cursor ui 1 0;
      Game ui
  | Browse, `Key (`Arrow `Up, _) ->
      move_cursor ui 0 (-1);
      Game ui
  | Browse, `Key (`Arrow `Down, _) ->
      move_cursor ui 0 1;
      Game ui
  | Browse, `Key (`Enter, _) ->
      activate ui;
      Game ui
  | Browse, `Key (`ASCII 'p', _) when Option.is_some (human_to_move ui) ->
      play ui None;
      Game ui
  | _, `Key (`ASCII ' ', _) ->
      ui.paused <- not ui.paused;
      Game ui
  | _ -> Game ui

(* --------------------------------------------------------------- main loop *)

(* true when a key is waiting; a signal (resize) counts as no key *)
let input_ready term secs =
  Term.pending term
  ||
  match Unix.select [ fst (Term.fds term) ] [] [] secs with
  | [], _, _ -> false
  | _ -> true
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> false

let render_campaign (w, _) ui =
  let p = Prefs.load () in
  let completed = Prefs.campaign_completed p in
  let chapter = Campaign.chapter_of completed in
  let open_ = campaign_open () in
  let n = List.length open_ in
  if n > 0 then ui.i <- ((ui.i mod n) + n) mod n;
  let rows =
    List.mapi
      (fun i (node : Campaign.node) ->
        let kind = if Campaign.is_fight node then "fight" else "meeting" in
        menu_row ui.i i ~enabled:true
          (Printf.sprintf "%s  (%s)" (Assets.text ("campaign." ^ node.id ^ ".name")) kind))
      open_
  in
  I.vcat
    ([
       I.string A.(st bold ++ fg lightcyan)
         (Assets.text (if Campaign.is_finished completed then "campaign.complete" else "campaign.title"));
       I.string dim
         (Assets.text (if chapter = Campaign.Night then "campaign.night" else "campaign.day")
         ^ "  @ " ^ Prefs.campaign_position p);
       I.void 1 1;
     ]
    @ (if rows = [] then [ I.string dim "(no open stops)" ] else rows)
    @ [
        I.void 1 1;
        (if ui.message <> "" then I.string bad ui.message else I.empty);
        text ~attr:dim w "up/down select   enter open   R reset   esc lobby";
      ])
  |> I.pad ~l:2 ~t:1

let on_campaign_key ui = function
  | `Key (`Arrow `Up, _) ->
      let n = List.length (campaign_open ()) in
      if n > 0 then ui.i <- (ui.i + n - 1) mod n;
      Campaign ui
  | `Key (`Arrow `Down, _) ->
      let n = List.length (campaign_open ()) in
      if n > 0 then ui.i <- (ui.i + 1) mod n;
      Campaign ui
  | `Key (`Enter, _) -> (
      match List.nth_opt (campaign_open ()) ui.i with
      | None -> Campaign ui
      | Some node ->
          if Campaign.is_fight node then
            match campaign_fight node with game -> Game game | exception Failure msg -> (ui.message <- msg; Campaign ui)
          else (
            (match node.meeting with
            | Some m -> (
                match m.choices with
                | [] -> Prefs.complete_campaign (Prefs.load ()) node.id
                | c :: _ -> Prefs.complete_campaign (Prefs.load ()) ~extras:[ c.id ] node.id)
            | None -> ());
            Campaign ui))
  | `Key (`ASCII 'R', _) ->
      Prefs.reset_campaign (Prefs.load ());
      Campaign ui
  | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> Lobby (new_lobby ())
  | _ -> Campaign ui

let render term = function
  | Lobby ui -> Term.image term (render_lobby (Term.size term) ui)
  | Duel ui -> Term.image term (render_duel (Term.size term) ui)
  | Settings ui -> Term.image term (render_settings (Term.size term) ui)
  | Campaign ui -> Term.image term (render_campaign (Term.size term) ui)
  | Game ui -> Term.image term (render_game (Term.size term) ui)
  | Quit -> ()

let rec loop term screen =
  render term screen;
  let on_event = function
    | `End -> Quit
    | `Resize _ -> screen
    | ev -> (
        match screen with
        | Lobby ui -> on_lobby_key ui ev
        | Duel ui -> on_duel_key ui ev
        | Settings ui -> on_settings_key ui ev
        | Campaign ui -> on_campaign_key ui ev
        | Game ui ->
            (match (Session.status (Table.session ui.table), ui.campaign) with
            | Over { winner = Some w; _ }, Some n when w = owner ->
                Prefs.complete_campaign (Prefs.load ()) n.id
            | _ -> ());
            on_game_key ui ev
        | Quit -> Quit)
  in
  match screen with
  | Quit -> ()
  | Game ui when Option.is_some (bot_to_move ui) && not ui.paused ->
      (* bot vs bot slowly enough to watch; vs a human just long enough to draw *)
      let delay =
        if
          Option.is_none (human_to_move ui)
          && Table.seat ui.table owner <> Human
          && Table.seat ui.table opponent <> Human
        then 0.4
        else 0.05
      in
      if input_ready term delay then loop term (on_event (Term.event term))
      else (
        guarded ui (fun () -> Table.play_bot ui.table);
        (* a human cursor may point past a hand that changed size *)
        move_cursor ui 0 0;
        loop term screen)
  | _ -> loop term (on_event (Term.event term))

let () =
  (* Bots and engine debug output print to stdout; keep them off the screen. *)
  let tty = Unix.dup Unix.stdout in
  let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 null Unix.stdout;
  let term = Term.create ~output:tty () in
  Fun.protect ~finally:(fun () -> Term.release term) (fun () -> loop term (Lobby (new_lobby ())))
