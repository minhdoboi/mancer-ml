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

(* --------------------------------------------------------------- setup ui *)

let seat_options = Table.Human :: List.map (fun id -> Table.Bot id) Tournament.bot_ids
let budget_options = [ 100; 250; 500; 1000; 2000; 5000 ]

type row = Seat0 | Special0 | Seat1 | Special1 | Budget | Seed | Start

let rows = [| Seat0; Special0; Seat1; Special1; Budget; Seed; Start |]

type setup_ui = {
  mutable row : int;
  mutable setup : Table.setup;
  mutable seed_text : string;
  mutable error : string;
}

let cycle options current delta =
  let a = Array.of_list options in
  let n = Array.length a in
  let i = Option.value ~default:0 (Array.find_index (( = ) current) a) in
  a.((((i + delta) mod n) + n) mod n)

let special_names () = List.map (fun (h : house) -> cap h.h_name) (Table.special_houses ())

let change_setup ui delta =
  let s = ui.setup in
  let seat0, seat1 = s.seats and sp0, sp1 = s.specials in
  let special cur = cycle (special_names ()) (cap cur) delta in
  ui.setup <-
    (match rows.(ui.row) with
    | Seat0 -> { s with seats = (cycle seat_options seat0 delta, seat1) }
    | Seat1 -> { s with seats = (seat0, cycle seat_options seat1 delta) }
    | Special0 -> { s with specials = (special sp0, sp1) }
    | Special1 -> { s with specials = (sp0, special sp1) }
    | Budget -> { s with budget_ms = cycle budget_options s.budget_ms delta }
    | Seed | Start -> s)

let render_setup (w, _) ui =
  let s = ui.setup in
  let seat0, seat1 = s.seats and sp0, sp1 = s.specials in
  let row i label value =
    let selected = ui.row = i in
    let mark = if selected then "> " else "  " in
    let value_attr = if selected then rev else A.empty in
    I.(
      string (if selected then accent else A.empty) (mark ^ label)
      |> hsnap ~align:`Left 22
      <|> string value_attr (" " ^ value ^ " ")
      <|> if selected && rows.(i) <> Seed && rows.(i) <> Start then string dim "  <- ->" else empty)
  in
  let seed = if ui.seed_text = "" then "random" else ui.seed_text in
  let start =
    let selected = rows.(ui.row) = Start in
    I.(
      string (if selected then accent else A.empty) (if selected then "> " else "  ")
      <|> string (if selected then A.(rev ++ st bold) else bold) " Start game ")
  in
  I.vcat
    [
      I.string A.(st bold ++ fg lightcyan) "mancer";
      I.string dim "new game";
      I.void 1 1;
      row 0 "P1 (bottom, first)" (Table.seat_name seat0);
      row 1 "P1 special" (cap sp0);
      row 2 "P2 (top)" (Table.seat_name seat1);
      row 3 "P2 special" (cap sp1);
      row 4 "Bot budget" (Printf.sprintf "%d ms" s.budget_ms);
      row 5 "Seed" (seed ^ if rows.(ui.row) = Seed then "_" else "");
      I.void 1 1;
      start;
      I.void 1 1;
      text ~attr:bad w ui.error;
      I.void 1 1;
      text ~attr:dim w
        "up/down select   left/right change   0-9 seed (backspace clears)   enter start   q quit";
    ]
  |> I.pad ~l:2 ~t:1

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
}

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

let new_game setup =
  let log = ref [] in
  let on_event e = Option.iter (fun l -> log := l :: !log) (Table.event_line ~name:card_name e) in
  let table = Table.start ~on_event setup in
  { table; log; col = 0; card = 0; mode = Browse; paused = false; message = "" }

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
    | Over _, _ -> "r replay seed   n new deal   w save game   s setup   q quit"
    | _, Target _ -> "arrows/0-5 choose   enter play   esc cancel"
    | _, Browse ->
        let both_bots = Option.is_none (human_to_move ui) in
        if both_bots then "space pause/resume   w save game   s setup   q quit"
        else "arrows select   enter play   p pass   w save   s setup   q quit"
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

let save_record ui = ui.message <- "saved " ^ Table.save ui.table ~dir:"tui-games"

let guarded ui f =
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

type screen = Setup of setup_ui | Game of game_ui | Quit

let start_from_setup su =
  let seed = if su.seed_text = "" then None else int_of_string_opt su.seed_text in
  let setup = { su.setup with seed } in
  match new_game setup with
  | ui -> Game ui
  | exception Failure msg ->
      su.error <- msg;
      Setup su

let setup_of_game ui =
  let s = Table.setup ui.table in
  Setup
    {
      row = 0;
      setup = s;
      seed_text = (match s.seed with Some n -> string_of_int n | None -> "");
      error = "";
    }

let on_setup_key su = function
  | `Key (`Arrow `Up, _) ->
      su.row <- (su.row + Array.length rows - 1) mod Array.length rows;
      Setup su
  | `Key (`Arrow `Down, _) | `Key (`Tab, _) ->
      su.row <- (su.row + 1) mod Array.length rows;
      Setup su
  | `Key (`Arrow `Left, _) ->
      change_setup su (-1);
      Setup su
  | `Key (`Arrow `Right, _) ->
      change_setup su 1;
      Setup su
  | `Key (`ASCII ('0' .. '9' as c), _) when rows.(su.row) = Seed ->
      if String.length su.seed_text < 9 then su.seed_text <- su.seed_text ^ String.make 1 c;
      Setup su
  | `Key (`Backspace, _) when rows.(su.row) = Seed ->
      let n = String.length su.seed_text in
      if n > 0 then su.seed_text <- String.sub su.seed_text 0 (n - 1);
      Setup su
  | `Key (`Enter, _) -> start_from_setup su
  | `Key (`ASCII 'q', _) | `Key (`Escape, _) -> Quit
  | _ -> Setup su

let on_game_key ui ev =
  let over = Option.is_none (Table.to_move ui.table) in
  match (ui.mode, ev) with
  | _, `Key (`ASCII 'q', _) -> Quit
  | _, `Key (`ASCII 's', _) -> setup_of_game ui
  | _, `Key (`ASCII 'w', _) ->
      guarded ui (fun () -> save_record ui);
      Game ui
  | _, `Key (`ASCII 'r', _) when over ->
      Game (new_game (Table.setup ui.table |> fun s -> { s with seed = Some (Table.seed ui.table) }))
  | _, `Key (`ASCII 'n', _) when over -> Game (new_game { (Table.setup ui.table) with seed = None })
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

let render term = function
  | Setup su -> Term.image term (render_setup (Term.size term) su)
  | Game ui -> Term.image term (render_game (Term.size term) ui)
  | Quit -> ()

let rec loop term screen =
  render term screen;
  let on_event = function
    | `End -> Quit
    | `Resize _ -> screen
    | ev -> (
        match screen with Setup su -> on_setup_key su ev | Game ui -> on_game_key ui ev | Quit -> Quit)
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
  let su = { row = 0; setup = Table.default_setup; seed_text = ""; error = "" } in
  Fun.protect ~finally:(fun () -> Term.release term) (fun () -> loop term (Setup su))
