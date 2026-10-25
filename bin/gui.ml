(* Raylib UI: the same Table as the terminal UI, drawn with the vibe card art.
   Layout uses Scala's 832×768 world, scaled to a window that fills the
   monitor height. Bots search on a second domain so the window keeps
   drawing; their move is submitted here. *)

open Mancer
open Sp
module R = Raylib
open Gui_draw
open Gui_lobby
open Gui_campaign
open Gui_board

(* ------------------------------------------------------------- main loop *)

type screen = Lobby of lobby_ui | Campaign of campaign_ui | Game of game_ui | Epilogue

let backdrop_house ui =
  (* A campaign fight is staged on the opponent's ground; a duel shows the
     starting player's ground. *)
  let pid = match ui.campaign with Some _ -> opponent | None -> (Table.setup ui.table).starting in
  (desc ui).gd_players.(pid).pd_houses |> fun hs -> hs.(Array.length hs - 1).phd_house

(* Scala Board.Background: a layered backdrop ("-sky" over "-ground") when the
   house ships one, else the flat painting. *)
let background ui =
  let house = backdrop_house ui in
  let flat () = Option.map (fun t -> (None, t)) (texture (Assets.background house)) in
  let layered sky ground =
    match (texture sky, texture ground) with Some sky, Some ground -> Some (Some sky, ground) | _ -> flat ()
  in
  match (Assets.background_layer house "-sky", Assets.background_layer house "-ground") with
  | Some sky, Some ground -> layered (Some sky) (Some ground)
  | _ -> flat ()

(* the ambience setting, remembered outside the lobby (read once at start) *)
let ambient_on = ref (try Prefs.ambient_sound (Prefs.load ()) with _ -> true)
let window_title = "The Mancer's echo"
let chrome_btn_w = 54.
let chrome_dragging = ref false
let chrome_bar_click = ref 0.

let chrome_mouse () =
  let m = R.get_mouse_position () in
  (R.Vector2.x m, R.Vector2.y m)

let chrome_in_bar () =
  let _, my = chrome_mouse () in
  my >= 0. && my < chrome_h

let chrome_buttons () =
  let sw = float (R.get_screen_width ()) in
  let x i = sw -. (float (3 - i) *. chrome_btn_w) in
  [ (`Min, x 0); (`Max, x 1); (`Close, x 2) ]

let chrome_hit_button () =
  if not (chrome_in_bar ()) then None
  else
    let mx, _ = chrome_mouse () in
    List.find_map
      (fun (kind, x) -> if mx >= x && mx < x +. chrome_btn_w then Some kind else None)
      (chrome_buttons ())

let handle_chrome ~quit =
  let over = chrome_in_bar () in
  if over then hovering := true;
  (if R.is_mouse_button_pressed R.MouseButton.Left then
     match chrome_hit_button () with
     | Some `Close -> quit := true
     | Some `Max -> if R.is_window_maximized () then R.restore_window () else R.maximize_window ()
     | Some `Min -> R.minimize_window ()
     | None when over ->
         let now = R.get_time () in
         if now -. !chrome_bar_click < 0.35 then
           if R.is_window_maximized () then R.restore_window () else R.maximize_window ()
         else if not (R.is_window_maximized ()) then chrome_dragging := true;
         chrome_bar_click := now
     | None -> ());
  if not (R.is_mouse_button_down R.MouseButton.Left) then chrome_dragging := false;
  if !chrome_dragging && not (R.is_window_maximized ()) then
    let d = R.get_mouse_delta () in
    let p = R.get_window_position () in
    R.set_window_position
      (int_of_float (R.Vector2.x p +. R.Vector2.x d))
      (int_of_float (R.Vector2.y p +. R.Vector2.y d))

let draw_chrome () =
  let sw = R.get_screen_width () in
  R.draw_rectangle 0 0 sw (int_of_float chrome_h) (rgba 12 12 12 255);
  let fs = 20. in
  R.draw_text_ex !regular window_title (R.Vector2.create 10. ((chrome_h -. fs) /. 2.)) fs 0. fg;
  List.iter
    (fun (kind, x) ->
      let hot = chrome_hit_button () = Some kind in
      let bg =
        match (kind, hot) with
        | `Close, true -> rgba 180 40 40 255
        | _, true -> rgba 48 48 48 255
        | _ -> rgba 12 12 12 255
      in
      R.draw_rectangle (int_of_float x) 0 (int_of_float chrome_btn_w) (int_of_float chrome_h) bg;
      let cx = x +. (chrome_btn_w /. 2.) and cy = chrome_h /. 2. in
      let ink = if hot then R.Color.white else muted in
      match kind with
      | `Min ->
          R.draw_line
            (int_of_float (cx -. 7.))
            (int_of_float cy)
            (int_of_float (cx +. 7.))
            (int_of_float cy) ink
      | `Max -> R.draw_rectangle_lines (int_of_float (cx -. 7.)) (int_of_float (cy -. 7.)) 14 14 ink
      | `Close ->
          R.draw_line
            (int_of_float (cx -. 6.))
            (int_of_float (cy -. 6.))
            (int_of_float (cx +. 6.))
            (int_of_float (cy +. 6.))
            ink;
          R.draw_line
            (int_of_float (cx +. 6.))
            (int_of_float (cy -. 6.))
            (int_of_float (cx -. 6.))
            (int_of_float (cy +. 6.))
            ink)
    (chrome_buttons ())

(* Scala DesktopLauncher: display height − 80 so the window stays under the
   panel instead of filling the monitor and picking up a large header. *)
let size_window_to_monitor () =
  let mon = R.get_current_monitor () in
  let mw = R.get_monitor_width mon and mh = R.get_monitor_height mon in
  let mx = int_of_float (R.Vector2.x (R.get_monitor_position mon)) in
  let my = int_of_float (R.Vector2.y (R.get_monitor_position mon)) in
  let inner_h = mh - 80 in
  let w = int_of_float (Float.round (float inner_h *. vw /. vh)) in
  let w, inner_h = if w > mw then (mw, int_of_float (Float.round (float mw *. vh /. vw))) else (w, inner_h) in
  let h = inner_h + int_of_float chrome_h in
  R.set_window_size w h;
  R.set_window_position (mx + ((mw - w) / 2)) (my + ((mh - h) / 2))

let fx_demo = ref 0

(* Headless-ish check of the drawing: `gui.exe --shot out.png` starts a game
   (or stays on the setup screen with --setup), waits, saves the window and
   exits. --bots lets two bots play so the board fills up. *)
type shot = {
  file : string;
  wait : float;
  bots : bool;
  setup_only : bool;
  seed : int option;
  special : string option;
}

(* The self-test of a build: `gui.exe --check` opens the window the game uses,
   loads the fonts and the art the launcher points at, and plays a few
   seconds of a real game. It is what CI runs on the packaged macOS app and
   on the Windows exe, so a build that cannot start fails the pipeline
   instead of the player's desktop. --no-window keeps the check to the binary
   and its assets, for a machine with no display.

   It exits 0 when the build runs, 1 when the package is broken, and 3 when
   the machine has no display and no OpenGL 3.3 to draw on — that is the
   machine's fault, not the package's. *)
type check = { secs : float; window : bool }

type mode = Play of shot option | Check of check

let parse_mode () =
  let file = ref "" and wait = ref 2. and bots = ref false and setup_only = ref false in
  let seed = ref None and special = ref None in
  let do_check = ref false and secs = ref 3. and window = ref true in
  Arg.parse
    [
      ("--shot", Arg.Set_string file, "FILE  save a screenshot to FILE and exit");
      ("--wait", Arg.Set_float wait, "SECS  seconds to wait before the shot (default 2)");
      ("--bots", Arg.Set bots, " two oracle bots play, so the board fills up");
      ("--setup", Arg.Set setup_only, " shoot the lobby instead of a game");
      ("--seed", Arg.Int (fun n -> seed := Some n), "N  seed for the deal");
      ("--special", Arg.String (fun h -> special := Some h), "NAME  special house for both players");
      ("--check", Arg.Set do_check, " check this build (window, fonts, assets, a short game) and exit");
      ("--check-secs", Arg.Set_float secs, "SECS  seconds the check plays (default 3)");
      ("--no-window", Arg.Clear window, " check the binary and its assets only (no display needed)");
    ]
    (fun a -> raise (Arg.Bad a))
    "gui [--shot FILE [--wait SECS] [--bots] [--setup] [--seed N] [--special NAME]]\n\
    \  gui --check [--check-secs SECS] [--no-window]";
  if !do_check then Check { secs = !secs; window = !window }
  else
    Play
      (if !file = "" then None
       else
         Some
           { file = !file; wait = !wait; bots = !bots; setup_only = !setup_only; seed = !seed; special = !special })

(* ------------------------------------------------------- the self-test *)

let say fmt = Printf.ksprintf (fun s -> print_endline s; flush stdout) fmt

(* What a packaged build must find next to it: the launcher points
   MANCER_ASSETS at that tree, and the game reads its art and its text from
   there. A package that lost a folder is the usual reason a build does not
   start, so the check names every missing one. *)
let check_assets () =
  let root = Assets.root () in
  let missing = ref [] in
  let need what path = if not (Sys.file_exists path) then missing := (what, path) :: !missing in
  let need_dir what path =
    if not (Sys.file_exists path) then missing := (what, path) :: !missing
    else if Sys.is_directory path && Array.length (Sys.readdir path) = 0 then missing := (what, path) :: !missing
  in
  let under dir name = Filename.concat (Filename.concat root dir) name in
  need "i18n/en.properties" (under "i18n" "en.properties");
  need "fonts/IMFellEnglish.ttf" (under "fonts" "IMFellEnglish.ttf");
  need_dir "cards" (under "" "cards");
  need_dir "portraits" (under "" "portraits");
  need_dir "sounds" (under "" "sounds");
  need_dir "backgrounds" (under "" "backgrounds");
  need_dir "combat" (under "" "combat");
  need_dir "ui" (under "" "ui");
  List.rev !missing

(* What the build runs on: the OS the runtime knows, and how wide it is. *)
let platform () = Printf.sprintf "%s %d-bit" Sys.os_type Sys.word_size

(* The report of the self-test: what the build found, and whether it runs.
   It answers with the state of the package alone: a machine with no display
   is not a broken package, and the caller tells the two apart. *)
let check_report ~missing ~text_ok ~window ~fonts ~game =
  say "mancer check";
  say "  version: %s" (Version.current ());
  say "  platform: %s" (platform ());
  say "  assets: %s" (Assets.root ());
  (match missing with
  | [] -> say "  assets: ok"
  | l -> List.iter (fun (what, path) -> say "  assets: missing %s (%s)" what path) l);
  say "  text: %s" (if text_ok then "ok" else "missing (the i18n bundle)");
  say "  window: %s"
    (match window with `ok -> "ok" | `none -> "skipped" | `no -> "unavailable (no display, or no OpenGL 3.3)");
  say "  fonts: %s" (match fonts with `ok -> "ok" | `no -> "failed" | `none -> "skipped");
  say "  game: %s"
    (match game with
    | `ok s -> Printf.sprintf "ok (played %s s)" s
    | `no -> "failed"
    | `none -> "skipped");
  let broken = missing <> [] || not text_ok || fonts = `no || game = `no in
  say "mancer check: %s" (if broken then "FAILED" else "ok");
  broken

let () =
  let mode = parse_mode () in
  let shot = match mode with Play sh -> sh | Check _ -> None in
  R.set_trace_log_level R.TraceLogLevel.Warning;
  (* GNOME on Wayland paints a large libdecor header even around GLFW. Prefer
     X11 so the window stays undecorated and the thin bar is the chrome. *)
  (match Sys.getenv_opt "WAYLAND_DISPLAY" with
  | Some _ -> Unix.putenv "WAYLAND_DISPLAY" ""
  | None -> ());
  (* The check keeps its window hidden: it must not take over the desktop
     while CI runs it. *)
  let flags = R.ConfigFlags.(window_resizable + window_undecorated + msaa_4x_hint + vsync_hint) in
  let flags = match mode with Check _ -> R.ConfigFlags.( flags + window_hidden ) | Play _ -> flags in
  let no_window = match mode with Check c -> not c.window | Play _ -> false in
  let missing = ref [] and text_ok = ref false and fonts_ok = ref true in
  (match mode with
  | Play _ -> ()
  | Check _ ->
      missing := check_assets ();
      (* A bundle that did not load leaves the key itself in place. *)
      text_ok := (try Assets.text "lobby.title" <> "lobby.title" with _ -> false));
  if not no_window then (
    R.set_config_flags flags;
    R.init_window 832 (768 + int_of_float chrome_h) window_title;
    if not (R.is_window_ready ()) then (
      (* No display, or no OpenGL 3.3. raylib would die inside GLFW a moment
         later, so say what is wrong and stop. The macOS CI runners have no
         OpenGL at all, and that is the machine's fault, not the package's:
         the check leaves 3 for that, and 1 for a broken package. *)
      (match mode with
      | Check _ ->
          let broken =
            check_report ~missing:!missing ~text_ok:!text_ok ~window:`no ~fonts:`none ~game:`none
          in
          exit (if broken then 1 else 3)
      | Play _ -> ());
      prerr_endline "mancer: no window (no display, or no OpenGL 3.3)";
      exit 1);
    size_window_to_monitor ();
    R.set_window_min_size 416 (384 + int_of_float chrome_h);
    R.set_exit_key R.Key.Null;
    R.set_target_fps 60;
    R.init_audio_device ();
    load_fonts ();
    (* the atlas of the paper face: without it the board has no text *)
    fonts_ok := R.is_texture_valid (R.Font.texture !paper_font);
    Fx.set_font !bold;
    Fx.load ());
  (* Without a display only the binary and its assets can be checked. *)
  (match mode with
  | Check c when not c.window ->
      let bad = check_report ~missing:!missing ~text_ok:!text_ok ~window:`none ~fonts:`none ~game:`none in
      exit (if bad then 1 else 0)
  | _ -> ());
  let screen = ref (Lobby (new_lobby ())) in
  let fight ?(persist = true) lobby =
    if persist then persist_duel lobby;
    drop_dialog ();
    lobby.show_duel <- false;
    lobby.message <- "";
    let s0, s1 = lobby.duel.sides in
    let seed = if lobby.gs.seed_text = "" then None else int_of_string_opt lobby.gs.seed_text in
    let d = Table.default_setup in
    let l0, l1 = lobby.gs.specials in
    let setup =
      {
        d with
        seats = (s0.seat, s1.seat);
        specials = (Table.resolve_special l0, Table.resolve_special l1);
        mode = lobby.gs.mode;
        budget_ms = lobby.gs.budget_ms;
        seed;
        starting = Table.random_starter ();
      }
    in
    match new_game setup (s0.portrait, s1.portrait) with
    | ui -> screen := Game ui
    | exception Failure msg -> lobby.message <- msg
  in
  let campaign_fight node =
    drop_dialog ();
    let p = Prefs.load () in
    (* The player keeps the campaign class all the way through. *)
    let player_special = Table.campaign_class () in
    let setup = Table.campaign_setup ~budget_ms:(Prefs.campaign_difficulty_ms p) ~player_special node in
    let you = Prefs.player_portrait p in
    let opp = match node.Campaign.fight with Some f -> f.portrait_id | None -> "summoner" in
    match new_game ~campaign:(Some node) setup (you, opp) with
    | ui -> screen := Game ui
    | exception Failure msg -> ( match !screen with Lobby l -> l.message <- msg | _ -> ())
  in
  let quit = ref false in
  (* A game shot skips the lobby without persisting its flags. The check
     starts a game the same way, with two oracle bots so the board fills. *)
  let start_game ~bots ~seed ~special =
    match !screen with
    | Lobby lobby ->
        let s0, s1 = lobby.duel.sides in
        if bots then (
          s0.seat <- Table.Bot "oracle";
          s0.portrait <- clamp_portrait s0.seat "ember";
          s1.seat <- Table.Bot "oracle";
          s1.portrait <- clamp_portrait s1.seat "oracle");
        (match special with Some h -> lobby.gs.specials <- ([ h ], [ h ]) | None -> ());
        if bots then lobby.gs.budget_ms <- 100;
        lobby.gs.seed_text <- (match seed with Some n -> string_of_int n | None -> "");
        fight ~persist:false lobby
    | _ -> ()
  in
  (match mode with
  | Play (Some sh) when not sh.setup_only -> start_game ~bots:sh.bots ~seed:sh.seed ~special:sh.special
  | Check _ -> start_game ~bots:true ~seed:None ~special:None
  | _ -> ());
  let t0 = R.get_time () in
  (* TEMP-DEBUG: FX_DEMO=rose spawns one demo effect 0.2 s in, so --shot can catch it *)
  let demo_env = Sys.getenv_opt "FX_DEMO" in
  let demo_at =
    match Sys.getenv_opt "FX_DEMO_DELAY" with Some s -> float_of_string_opt s | None -> None
  in
  let demo_done = ref false in
  while not (!quit || R.window_should_close ()) do
    (match (demo_env, !screen) with
    | Some name, Game ui
      when (not !demo_done) && R.get_time () -. t0 >= Option.value ~default:0.2 demo_at ->
        lay := make_layout ui.table;
        Fx.spawn_demo (fx_layout ui) name;
        demo_done := true
    | _ -> ());
    (match !screen with
    | Lobby l -> ambient_on := l.gs.ambient_enabled
    | Game ui -> ( match ui.settings with Some gs -> ambient_on := gs.ambient_enabled | None -> ())
    | _ -> ());
    let wind_board = match !screen with Game ui -> (backdrop_house ui).h_key = "wind" | _ -> false in
    Ocean.update
      ~active:(match !screen with Lobby _ -> !ambient_on | Game _ -> !ambient_on && wind_board | _ -> false)
      ~wind:wind_board ~volume:sound_level;
    let quake_board =
      match !screen with Game ui -> (backdrop_house ui).h_key = "elementalist" | _ -> false
    in
    Rumble.update ~active:(!ambient_on && quake_board) ~volume:sound_level;
    let limbo_board = match !screen with Game ui -> (backdrop_house ui).h_key = "limbo" | _ -> false in
    Choir.update ~active:(!ambient_on && limbo_board) ~volume:sound_level;
    fit_frame ();
    Fx.set_transform ~scale:!scale ~off_x:!off_x ~off_y:!off_y;
    pending := None;
    hovering := false;
    handle_chrome ~quit;
    (match !screen with
    | Lobby lobby ->
        if lobby.show_campaign then (
          lobby.show_campaign <- false;
          screen := Campaign (new_campaign ()));
        if lobby.want_quit then quit := true;
        if lobby.show_settings then settings_seed_keys lobby.gs;
        (* The campaign intro's class list lives only while the dialog is up. *)
        if not lobby.show_campaign_intro then lobby.class_select.sel_open := false;
        if not lobby.show_settings then lobby.gs.campaign_class_select.sel_open := false;
        let wheel = R.get_mouse_wheel_move () in
        if wheel <> 0. && lobby.show_settings && lobby.gs.tab = 0
           && inside (class_wheel settings_class_layout)
        then class_scroll_by settings_class_layout lobby.gs wheel;
        if wheel <> 0. && lobby.show_duel && lobby.duel_tab = 1
           && inside (class_wheel duel_class_layout)
        then class_scroll_by duel_class_layout lobby.gs wheel;
        select_scroll lobby.class_select wheel;
        select_scroll lobby.gs.campaign_class_select wheel;
        if R.is_key_pressed R.Key.Escape then
          if lobby.show_campaign_intro then
            close_dialog "campaign-intro" (fun () -> lobby.show_campaign_intro <- false)
          else if lobby.show_settings then
            close_dialog "settings" (fun () ->
                lobby.show_settings <- false;
                lobby.gs.seed_focus <- false)
          else if lobby.show_duel then close_dialog "duel" (fun () -> lobby.show_duel <- false)
          else if lobby.show_update then close_dialog "update" (fun () -> lobby.show_update <- false)
          else quit := true
        else if lobby.show_duel && R.is_key_pressed R.Key.Enter then fight lobby
        else if (not lobby.show_duel) && (not lobby.show_settings) && not lobby.show_campaign_intro then
          lobby_keys lobby
    | Campaign camp ->
        if not camp.show_settings then camp.gs.campaign_class_select.sel_open := false;
        let wheel = R.get_mouse_wheel_move () in
        if wheel <> 0. && camp.show_settings && camp.gs.tab = 0
           && inside (class_wheel settings_class_layout)
        then class_scroll_by settings_class_layout camp.gs wheel;
        select_scroll camp.gs.campaign_class_select wheel;
        (match camp.pending_lobby with
        | true ->
            camp.pending_lobby <- false;
            screen := Lobby (new_lobby ())
        | false -> ());
        (match camp.pending_epilogue with
        | true ->
            camp.pending_epilogue <- false;
            Prefs.reset_campaign (Prefs.load ());
            screen := Epilogue
        | false -> ());
        (match camp.pending_fight with
        | Some n ->
            camp.pending_fight <- None;
            campaign_fight n
        | None -> ());
        if R.is_key_pressed R.Key.Escape then
          if camp.show_settings then
            close_dialog "settings" (fun () ->
                camp.show_settings <- false;
                camp.gs.seed_focus <- false)
          else if dlg_open camp.dlg then close_dialog "campaign" (fun () -> camp.dlg <- Camp_none)
          else camp.pending_lobby <- true
    | Epilogue -> if R.is_key_pressed R.Key.Escape then screen := Lobby (new_lobby ())
    | Game ui ->
        drive_bot ui;
        tick_slides ui;
        let wheel = R.get_mouse_wheel_move () in
        (if wheel <> 0. then
           match (ui.settings, ui.house_dlg) with
           | Some gs, _ when gs.tab = 0 && inside (class_wheel settings_class_layout) ->
               class_scroll_by settings_class_layout gs wheel
           | None, Some dlg -> house_scroll_by dlg wheel
           | _ ->
               ui.scroll_y <- Float.min max_scroll (Float.max 0. (ui.scroll_y +. (wheel *. 48.)));
               Fx.set_scroll ui.scroll_y);
        (match ui.settings with
        | Some gs ->
            select_scroll gs.campaign_class_select wheel
        | None -> ());
        if R.is_key_pressed R.Key.Escape then
          match (ui.settings, ui.called_dlg, ui.house_dlg, ui.campaign_end) with
          | Some _, _, _, _ -> close_dialog "settings" (fun () -> ui.settings <- None)
          | None, Some _, _, _ -> close_called_dlg ui
          | None, None, Some _, _ -> close_house_dlg ui
          | None, None, None, Some _ ->
              settle ui;
              screen := Campaign (new_campaign ~from_game:true ())
          | None, None, None, None -> ui.mode <- Browse
        else if R.is_mouse_button_pressed R.MouseButton.Right then (
          close_called_dlg ui;
          ui.mode <- Browse);
        if R.is_key_pressed R.Key.Space && both_bots ui then ui.paused <- not ui.paused;
        if R.is_key_pressed R.Key.F8 then (
          lay := make_layout ui.table;
          let names = Fx.demo_names in
          let i = !fx_demo mod List.length names in
          incr fx_demo;
          Fx.spawn_demo (fx_layout ui) (List.nth names i)));
    R.begin_drawing ();
    R.clear_background bg;
    (match !screen with
    | Lobby lobby ->
        draw_lobby lobby;
        run_dialog "duel" lobby.show_duel (fun () -> draw_duel lobby ~fight:(fun () -> fight lobby));
        run_dialog "settings" lobby.show_settings (fun () ->
            draw_game_settings lobby.gs ~show_seed:true ~on_budget:ignore ~close:(fun () ->
                lobby.show_settings <- false;
                lobby.gs.seed_focus <- false));
        run_dialog "campaign-intro" lobby.show_campaign_intro (fun () ->
            draw_campaign_intro lobby ~start:(fun () ->
                drop_dialog ();
                lobby.show_campaign_intro <- false;
                screen := Campaign (new_campaign ())));
        run_dialog "update" lobby.show_update (fun () -> draw_update lobby)
    | Campaign camp -> draw_campaign camp
    | Epilogue -> draw_epilogue ~menu:(fun () -> screen := Lobby (new_lobby ()))
    | Game ui ->
        (match background ui with
        | Some (sky, ground) ->
            let a = fade_smooth ((R.get_time () -. ui.bg_at) /. 2.6) in
            with_alpha a (fun () ->
                Option.iter (fun t -> draw_sky ~at:(R.get_time () -. ui.bg_at) t ground) sky;
                cover ground)
        | None -> ());
        draw_eclipse ui;
        ui.hover <- None;
        ui.hover_opp <- false;
        lay := make_layout ui.table;
        (match peek_smoke_card ui with Some r -> smoke_show r | None -> smoke_hide ());
        Fx.Hover.draw ();
        (match !(ui.last_played) with
        | Some { card = Some c; player } when inside (last_played_rect ui) ->
            ui.hover <- Some (Hover_card (c, player, None))
        | _ -> ());
        (* New picks houses from the settings dialog (or the prefs it already
           persisted). Restart and switch keep this fight's resolved pair. *)
        let chosen_specials () =
          match ui.settings with
          | Some gs -> gs.specials
          | None -> ( try Prefs.setup_specials (Prefs.load ()) with _ -> ([], []))
        in
        let setup_now () =
          let base = { (Table.setup ui.table) with budget_ms = ui.budget_ms } in
          let l0, l1 = chosen_specials () in
          let you, opp =
            match ui.campaign with
            | Some _ -> (Table.campaign_class (), snd base.specials)
            | None -> (Table.resolve_special l0, Table.resolve_special l1)
          in
          { base with specials = (you, opp) }
        in
        let to_lobby () =
          settle ui;
          screen :=
            match ui.campaign with
            | Some _ -> Campaign (new_campaign ~from_game:true ())
            | None -> Lobby (new_lobby ())
        in
        let start_fight setup portraits =
          settle ui;
          screen := Game (new_game ~campaign:ui.campaign setup portraits)
        in
        let switch () =
          settle ui;
          screen :=
            Game
              (new_swapped_game ~campaign:ui.campaign ui.table
                 { (Table.setup ui.table) with budget_ms = ui.budget_ms }
                 ui.portraits)
        in
        (* Smoke and the like, drawn over the backdrop but behind the cards. *)
        Fx.draw_underlay ~now:(R.get_time ());
        draw_board ui;
        draw_pass ui;
        draw_hands ui;
        draw_slides ui;
        draw_spell_shows ui;
        (* After the cards so bolts overlay them; Raylib later-draws-win. *)
        Fx.draw_all ~now:(R.get_time ());
        draw_pops ui;
        draw_preview ui;
        draw_last_played ui;
        draw_menu ui
          ~quit:(fun () -> to_lobby ())
          ~settings:(fun () -> ui.settings <- Some (game_settings_from_prefs ()))
          ~switch
          ~replay:(fun () ->
            start_fight
              { (Table.setup ui.table) with seed = Some (Table.seed ui.table); budget_ms = ui.budget_ms }
              ui.portraits)
          ~start_new:(fun () ->
            let base = setup_now () in
            (* A fresh duel deal flips the starter like Scala's new [Local];
               a campaign rematch keeps its scripted starter. *)
            let starting =
              match ui.campaign with
              | Some _ -> (Table.setup ui.table).starting
              | None -> Table.random_starter ()
            in
            start_fight { base with seed = None; starting } ui.portraits);
        (match Session.status (session ui) with
        | Over { winner; _ } -> (
            if over_ready ui && ui.ended_at = None then ui.ended_at <- Some (R.get_time ());
            match ui.campaign with
            | None -> ()
            | Some node -> (
                let won = winner = Some owner in
                if (not ui.campaign_recorded) && won then (
                  Prefs.complete_campaign (Prefs.load ()) node.id;
                  ui.campaign_recorded <- true);
                let pause = if won then 3. else 1.6 in
                (match ui.ended_at with
                | Some t when R.get_time () -. t >= pause && ui.settings = None && ui.campaign_end = None ->
                    ui.campaign_end <- Some won
                | _ -> ());
                match ui.campaign_end with
                | None -> ()
                | Some won ->
                    (* The last duel gets its message like any other; the
                       epilogue waits until the player dismisses it. *)
                    let final = won && node.id = Campaign.night_last in
                    draw_campaign_end ~node ~won ~final
                      ~replay:(fun () ->
                        settle ui;
                        campaign_fight node)
                      ~next:(fun () ->
                        settle ui;
                        if final then (
                          Prefs.reset_campaign (Prefs.load ());
                          screen := Epilogue)
                        else screen := Campaign (new_campaign ~from_game:true ()))))
        | To_move _ ->
            ui.ended_at <- None;
            ui.campaign_end <- None);
        run_dialog "called" (Option.is_some ui.called_dlg) (fun () ->
            match ui.called_dlg with Some dlg -> draw_called_dlg ui dlg | None -> ());
        run_dialog "house" (Option.is_some ui.house_dlg) (fun () ->
            match ui.house_dlg with Some dlg -> draw_house_details ui dlg | None -> ());
        run_dialog "settings" (Option.is_some ui.settings) (fun () ->
            match ui.settings with
            | Some gs ->
                draw_game_settings gs ~show_seed:false
                  ~on_budget:(fun ms -> ui.budget_ms <- ms)
                  ~close:(fun () -> ui.settings <- None)
            | None -> ()));
    draw_chrome ();
    R.end_drawing ();
    (match shot with
    | Some sh when R.get_time () -. t0 >= sh.wait ->
        (* take_screenshot prefixes the working directory, so export by hand *)
        let img = R.load_image_from_screen () in
        if not (R.export_image img sh.file) then prerr_endline ("cannot write " ^ sh.file);
        R.unload_image img;
        quit := true
    | _ -> ());
    (* The check plays for a few seconds, then stops: it is the game running
       that it wants to see, not a screenshot of it. *)
    (match mode with
    | Check c when R.get_time () -. t0 >= c.secs -> quit := true
    | _ -> ());
    R.set_mouse_cursor (if !hovering then R.MouseCursor.Pointing_hand else R.MouseCursor.Default);
    (* only humans submit from a click, never while a search reads the board;
       leaving a game settles the search first *)
    Option.iter (fun f -> f ()) !pending
  done;
  (match !screen with Game ui -> settle ui | _ -> ());
  Hashtbl.iter (fun _ s -> Option.iter R.unload_sound s) sounds;
  Ocean.unload ();
  Scream.unload ();
  Warcry.unload ();
  Rumble.unload ();
  Choir.unload ();
  Fx.unload ();
  R.close_audio_device ();
  R.close_window ();
  (* The check reports what it found and whether the build runs. *)
  match mode with
  | Check c ->
      let fonts = if !fonts_ok then `ok else `no in
      let game = `ok (string_of_float c.secs) in
      let bad = check_report ~missing:!missing ~text_ok:!text_ok ~window:`ok ~fonts ~game in
      exit (if bad then 1 else 0)
  | Play _ -> ()
