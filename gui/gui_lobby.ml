(* Raylib UI: portraits, the lobby, duel and settings dialogs. *)

open Mancer
open Sp
module R = Raylib
open Gui_draw

(* ------------------------------------------------------------- portraits *)

let portraits_for = Table.faces_for_seat
let cap = String.capitalize_ascii

let portrait_label id =
  id |> String.split_on_char '-' |> List.filter (( <> ) "f") |> List.map cap |> String.concat " "

let cycle_portrait ids current delta =
  let n = List.length ids in
  let i = match List.find_index (( = ) current) ids with Some i -> i | None -> 0 in
  List.nth ids ((((i + delta) mod n) + n) mod n)

let clamp_portrait = Table.clamp_portrait

let draw_face ?tint id r =
  match texture (Assets.portrait id) with
  | Some t -> image ?tint t r
  | None ->
      fill r panel;
      text_c ~size:11. ~color:muted r (r.y +. (r.h /. 2.) -. 6.) "?"

let draw_portrait_chooser ~x ~y ~size ~col_w ?(sub = "") ?(name_size = 14.) ?(sub_size = 13.) id ids set =
  let ctrl = 22. and gap = 6. and ctrl_h = 26. in
  let total = ctrl +. gap +. size +. gap +. ctrl in
  let x0 = x +. ((col_w -. total) /. 2.) in
  let mid_y = y +. ((size -. ctrl_h) /. 2.) in
  pbutton ~size:16. (rect x0 mid_y ctrl ctrl_h) "<" (fun () -> set (cycle_portrait ids id (-1)));
  let face = rect (x0 +. ctrl +. gap) y size size in
  draw_face id face;
  outline ~round:0.04 face (if inside face then ink_soft else paper_line);
  on_click face (fun () -> set (cycle_portrait ids id 1));
  pbutton ~size:16.
    (rect (face.x +. size +. gap) mid_y ctrl ctrl_h)
    ">"
    (fun () -> set (cycle_portrait ids id 1));
  text_c ~font:paper_font ~size:name_size ~color:ink_soft
    (rect x (y +. size) col_w 0.)
    (y +. size +. 8.)
    (portrait_label id);
  if sub <> "" then
    text_c ~font:paper_font ~size:sub_size ~color:ink_soft
      (rect x (y +. size) col_w 0.)
      (y +. size +. 8. +. name_size +. 4.)
      sub

(* ------------------------------------------------------------ lobby ui *)

let ai_portrait_ids = List.map (fun (o : Table.opponent) -> o.Table.opp_portrait) Table.duel_opponents
let budget_options = Table.budget_options
let languages = Table.languages

(* [ai.<kind>] strings take the level, like Scala's [AiFace.name]. *)
let fill_level s level =
  let want = string_of_int level in
  let b = Buffer.create (String.length s + 2) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else if i + 2 < n && s.[i] = '{' && s.[i + 1] = '0' && s.[i + 2] = '}' then (
      Buffer.add_string b want;
      go (i + 3))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let ai_label portrait =
  match List.find_opt (fun (o : Table.opponent) -> o.Table.opp_portrait = portrait) Table.duel_opponents with
  | Some o -> fill_level (Assets.text ("ai." ^ o.Table.opp_kind)) o.Table.opp_level
  | None -> portrait_label portrait

let ai_description portrait =
  match List.find_opt (fun (o : Table.opponent) -> o.Table.opp_portrait = portrait) Table.duel_opponents with
  | Some o -> Assets.text ("ai." ^ o.Table.opp_kind ^ ".description")
  | None -> ""

(* One side of the duel dialog: a human/bot seat with its portrait. A bot
   seat wears one of its own faces, so picking an AI portrait always brings
   its bot along. *)
type duel_side = { mutable seat : Table.seat; mutable portrait : string }
type duel_ui = { sides : duel_side * duel_side }

let set_ai_side (sd : duel_side) id =
  sd.portrait <- id;
  sd.seat <- (match Table.bot_of_portrait id with Some b -> Table.Bot b | None -> Table.Bot "oracle")

(* Game settings (the Scala Settings dialog): house choices, bot budget,
   seed, sound and language. Everything persists except the seed. Each side
   checks a set of houses; the fight plays a random pick, an empty set
   meaning every ported special. *)
type game_settings = {
  mutable specials : string list * string list;
  mutable budget_ms : int;
  mutable seed_text : string;
  mutable seed_focus : bool;
  mutable sound : float;
  mutable ambient_enabled : bool;
  mutable lang : string;
  mutable debug : bool;
  mutable tab : int;
  mutable class_scroll : float;
  mutable class_drag : bool;
  mutable class_grab_y : float option;
  mutable class_grab_scroll : float;
  mutable class_panning : bool;
}

type lobby_ui = {
  duel : duel_ui;
  gs : game_settings;
  mutable menu : int;
  mutable show_duel : bool;
  mutable show_settings : bool;
  mutable show_campaign_intro : bool;
  mutable show_campaign : bool;
  mutable want_quit : bool;
  mutable message : string;
  class_select : select_state;
}

let valid_specials names =
  List.filter_map (fun s -> try Some (Catalog.find_special s).h_name with Failure _ -> None) names

let game_settings_from_prefs () =
  let prefs = Prefs.load () in
  let sp0, sp1 = Prefs.setup_specials prefs in
  {
    specials = (valid_specials sp0, valid_specials sp1);
    budget_ms = Prefs.setup_budget prefs;
    seed_text = "";
    seed_focus = false;
    sound = Prefs.sound_level prefs;
    ambient_enabled = Prefs.ambient_sound prefs;
    lang = Prefs.language prefs;
    debug = Prefs.debug_mode prefs;
    tab = 0;
    class_scroll = 0.;
    class_drag = false;
    class_grab_y = None;
    class_grab_scroll = 0.;
    class_panning = false;
  }

let duel_from_prefs () =
  let (seat0, portrait0), (seat1, portrait1) = Table.duel_of_prefs ~bot_opponent:true (Prefs.load ()) in
  { sides = ({ seat = seat0; portrait = portrait0 }, { seat = seat1; portrait = portrait1 }) }

let new_lobby () =
  {
    duel = duel_from_prefs ();
    gs = game_settings_from_prefs ();
    menu = 1;
    show_duel = false;
    show_settings = false;
    show_campaign_intro = false;
    show_campaign = false;
    want_quit = false;
    message = "";
    class_select = select_state ();
  }

let persist_duel lobby =
  let s0, s1 = lobby.duel.sides in
  Table.persist_duel ((s0.seat, s0.portrait), (s1.seat, s1.portrait))

let lobby_menu = [ "campaign"; "duel"; "settings" ]

let campaign_is_fresh () =
  let p = Prefs.load () in
  Prefs.campaign_completed p = [] && Prefs.campaign_position p = Campaign.day_start_id

let open_campaign lobby =
  if campaign_is_fresh () then lobby.show_campaign_intro <- true else lobby.show_campaign <- true

(* Exit sign: an open rectangle with an arrow leaving it. *)
let draw_door_icon r color =
  let c = alpha_color color and thick = 2.4 *. !scale in
  let seg (x0, y0) (x1, y1) =
    R.draw_line_ex (pos (r.x +. x0) (r.y +. y0)) (pos (r.x +. x1) (r.y +. y1)) thick c
  in
  let h = r.h and w = r.w in
  let my = h /. 2. in
  seg (11., 3.) (1., 3.);
  seg (1., 3.) (1., h -. 3.);
  seg (1., h -. 3.) (11., h -. 3.);
  seg (7., my) (w, my);
  seg (w -. 6., my -. 5.) (w, my);
  seg (w -. 6., my +. 5.) (w, my)

let draw_lobby lobby =
  (match texture (Assets.ui "lobby-elements") with Some t -> cover t | None -> ());
  (* Plain text over the art, like Scala's text menu: no boxes, with a dark
     offset so it reads over the bright vista. *)
  text_c ~font:bold ~size:44. ~color:(rgba 0 0 0 200) (rect 2. 2. vw 0.) 42. (Assets.text "lobby.title");
  text_c ~font:bold ~size:44. (rect 0. 0. vw 0.) 40. (Assets.text "lobby.title");
  List.iteri
    (fun i l ->
      text_c ~size:15. ~color:(rgba 0 0 0 200) (rect 1. 1. vw 0.) (97. +. (float i *. 20.)) l;
      text_c ~size:15. (rect 0. 0. vw 0.) (96. +. (float i *. 20.)) l)
    (List.filteri (fun i _ -> i < 2) (wrap 15. 700. (Assets.text "lobby.message")));
  (* A soft wash behind the text menu, like Scala's menu box. *)
  fill ~round:0.04 (rect 28. (vh -. 184.) 208. 144.) (rgba 0 0 0 115);
  List.iteri
    (fun i key ->
      let r = rect 40. (vh -. 176. +. (float i *. 40.)) 184. 36. in
      let hot = inside r in
      if hot then hovering := true;
      let color = if hot then rgba 255 214 89 255 else R.Color.white in
      let label = Assets.text ("button." ^ key) in
      text ~size:24. ~color:(rgba 0 0 0 200) 50. (r.y +. 10.) label;
      text ~size:24. ~color 48. (r.y +. 8.) label;
      on_click r (fun () ->
          lobby.menu <- i;
          if key = "campaign" then open_campaign lobby
          else if key = "duel" then lobby.show_duel <- true
          else if key = "settings" then (
            lobby.show_settings <- true;
            lobby.gs.tab <- 0)))
    lobby_menu;
  (* Quit sits alone at the bottom right, no backdrop. *)
  (let label = Assets.text "button.quit" in
   let tw = text_w 24. label in
   let w = 22. +. 10. +. tw in
   let r = rect (vw -. w -. 40.) (vh -. 76.) w 36. in
   let hot = inside r in
   if hot then hovering := true;
   let color = if hot then rgba 255 214 89 255 else R.Color.white in
   draw_door_icon (rect r.x (r.y +. 4.) 22. 28.) color;
   text ~size:24. ~color:(rgba 0 0 0 200) (r.x +. 34.) (r.y +. 10.) label;
   text ~size:24. ~color (r.x +. 32.) (r.y +. 8.) label;
   on_click r (fun () -> lobby.want_quit <- true));
  if lobby.message <> "" then text ~size:13. ~color:red 48. (vh -. 60.) lobby.message;
  text_c ~size:14. ~color:faint (rect 0. 0. vw 0.) (vh -. 28.) "↑↓ choose · Enter opens · Esc quits"

let lobby_keys lobby =
  if R.is_key_pressed R.Key.Up then lobby.menu <- (if lobby.menu <= 0 then 2 else lobby.menu - 1);
  if R.is_key_pressed R.Key.Down then lobby.menu <- (if lobby.menu >= 2 then 0 else lobby.menu + 1);
  if R.is_key_pressed R.Key.Enter then
    match lobby.menu with
    | 0 -> open_campaign lobby
    | 1 -> lobby.show_duel <- true
    | _ ->
        lobby.show_settings <- true;
        lobby.gs.tab <- 0

(* Duel setup (Scala DuelDialog): portraits pick the faces, and an AI
   portrait brings its bot along. Dev mode can put a bot in seat one. *)
let draw_duel lobby ~fight =
  click_full (fun () -> close_dialog "duel" (fun () -> lobby.show_duel <- false));
  let debug = debug_mode () in
  let box = rect 106. 150. 620. (if debug then 440. else 400.) in
  paper box;
  on_click box (fun () -> ());
  text_c ~font:paper_font ~size:26. ~color:ink box (box.y +. 16.) (Assets.text "button.duel");
  let s0, s1 = lobby.duel.sides in
  if (not debug) && match s0.seat with Table.Bot _ -> true | Table.Human -> false then (
    s0.seat <- Table.Human;
    s0.portrait <- clamp_portrait Table.Human s0.portrait);
  (match s1.portrait with
  | id when List.mem id ai_portrait_ids -> set_ai_side s1 id
  | _ -> set_ai_side s1 (List.hd ai_portrait_ids));
  let col_w = 280. in
  let side ~x ~label ~(sd : duel_side) ~you =
    text_c ~font:paper_font ~size:22. ~color:ink (rect x (box.y +. 52.) col_w 0.) (box.y +. 52.) label;
    let y = ref (box.y +. 82.) in
    if you && debug then (
      let hw = paper_btn_w ~size:16. "Human" and bw = paper_btn_w ~size:16. "Bot" in
      let gap = 6. in
      let x0 = x +. ((col_w -. hw -. gap -. bw) /. 2.) in
      pbutton ~selected:(sd.seat = Table.Human) ~size:16. (rect x0 !y hw 26.) "Human" (fun () ->
          sd.seat <- Table.Human;
          sd.portrait <- clamp_portrait Table.Human sd.portrait);
      pbutton
        ~selected:(match sd.seat with Table.Bot _ -> true | Table.Human -> false)
        ~size:16.
        (rect (x0 +. hw +. gap) !y bw 26.)
        "Bot"
        (fun () ->
          let id = if List.mem sd.portrait ai_portrait_ids then sd.portrait else List.hd ai_portrait_ids in
          set_ai_side sd id);
      y := !y +. 32.);
    let ids, sub, on_pick =
      match sd.seat with
      | Table.Human -> (Table.player_portraits, "", fun v -> sd.portrait <- v)
      | Table.Bot _ -> (ai_portrait_ids, ai_label sd.portrait, set_ai_side sd)
    in
    draw_portrait_chooser ~x ~y:!y ~size:120. ~col_w ~sub ~name_size:18. ~sub_size:16. sd.portrait ids on_pick;
    match sd.seat with
    | Table.Bot _ when debug ->
        let size = 14. in
        List.iteri
          (fun i l ->
            text_c ~font:paper_font ~size ~color:ink_soft (rect x 0. col_w 0.)
              (!y +. 120. +. 56. +. (float i *. (size +. 3.)))
              l)
          (wrap ~font:paper_font size col_w (ai_description sd.portrait))
    | Table.Bot _ | Table.Human -> ()
  in
  side ~x:126. ~label:(Assets.text "lobby.you") ~sd:s0 ~you:true;
  side ~x:426. ~label:(Assets.text "lobby.opponent") ~sd:s1 ~you:false;
  paper_btn_row ~size:20. ~h:30.
    ~cx:(box.x +. (box.w /. 2.))
    ~y:(box.y +. box.h -. 48.)
    [
      (Assets.text "button.fight", fight);
      (Assets.text "button.cancel", fun () -> close_dialog "duel" (fun () -> lobby.show_duel <- false));
    ]

(* Game settings (the classes/game/sound/language tabs of the Scala Settings
   dialog). The game tab holds the bot budget and seed from the old setup
   screen. House choices are parchment checkboxes, like Scala. *)
let settings_box = rect 76. 104. 680. 560.
let gs_seed_field = rect 200. 294. 200. 34.

(* Toggle one house in a side's checked set, like the Scala checkboxes. *)
let set_side gs which set =
  let a, b = gs.specials in
  let next = if which = 0 then (set, b) else (a, set) in
  gs.specials <- next;
  try Prefs.set_setup_specials (Prefs.load ()) next with _ -> ()

let toggle_special gs which name =
  let a, b = gs.specials in
  let set = if which = 0 then a else b in
  let mem = List.exists (fun n -> Catalog.token n = Catalog.token name) set in
  let next =
    if mem then List.filter (fun n -> Catalog.token n <> Catalog.token name) set else set @ [ name ]
  in
  set_side gs which next

(* Group headers toggle the whole group: all checked clears it, otherwise it checks them all. *)
let toggle_group gs which houses =
  let names = List.map (fun (h : house) -> h.h_name) houses in
  let same a b = Catalog.token a = Catalog.token b in
  let known n = List.exists (same n) names in
  let a, b = gs.specials in
  let set = if which = 0 then a else b in
  let selected n = List.exists (same n) set in
  let next =
    if names <> [] && List.for_all selected names then List.filter (fun n -> not (known n)) set
    else set @ List.filter (fun m -> not (selected m)) names
  in
  set_side gs which next

let all_specials () = List.map (fun (h : house) -> h.h_name) (Table.special_houses ())

(* Classes tab: two player columns, two house checkboxes per row, like Scala. *)
let class_cols = 2
let class_x0 = 100.
let class_col_w = 304.
let class_gap = 8.
let class_item_w = (class_col_w -. class_gap) /. float class_cols
let class_row_h = 26.
let class_head_h = 32.
let class_sep = 10.
let class_x1 = class_x0 +. class_col_w +. 12.
let class_y0 = 214.
let class_y1 = 608.
let class_bar = rect (class_x1 +. class_col_w +. 4.) class_y0 12. (class_y1 -. class_y0)

let class_wheel =
  rect class_x0 (class_y0 -. 18.) (class_x1 -. class_x0 +. class_col_w +. 20.) (class_y1 -. class_y0 +. 18.)

let class_view = rect class_x0 class_y0 (class_x1 -. class_x0 +. class_col_w) (class_y1 -. class_y0)
let class_rows n = (n + class_cols - 1) / class_cols

let class_content_h () =
  let raw =
    List.fold_left
      (fun acc (_, houses) ->
        acc +. class_head_h +. (float (class_rows (List.length houses)) *. class_row_h) +. class_sep)
      0. Table.house_groups
  in
  Float.max 0. (raw -. class_sep)

let class_maxscroll () = Float.max 0. (class_content_h () -. (class_y1 -. class_y0))

let class_scroll_by gs w =
  gs.class_scroll <- Float.min (class_maxscroll ()) (Float.max 0. (gs.class_scroll -. (w *. 48.)))

let class_clamp gs = gs.class_scroll <- Float.min (class_maxscroll ()) (Float.max 0. gs.class_scroll)

(* Drag the list itself to scroll. A press that moves past [slop] is a pan,
   so the checkbox under the pointer waits for mouse-up and stays put. *)
let class_pan_slop = 5.

let handle_class_pan gs =
  let max = class_maxscroll () in
  let down = R.is_mouse_button_down R.MouseButton.Left in
  let released = R.is_mouse_button_released R.MouseButton.Left in
  let _, my = mouse () in
  (if (not down) && not released then (
     gs.class_grab_y <- None;
     gs.class_panning <- false)
   else if max > 0. && not gs.class_drag then
     match gs.class_grab_y with
     | None when down && inside class_view ->
         gs.class_grab_y <- Some my;
         gs.class_grab_scroll <- gs.class_scroll;
         gs.class_panning <- false
     | Some y0 ->
         let dy = my -. y0 in
         if Float.abs dy > class_pan_slop then gs.class_panning <- true;
         if gs.class_panning && down then (
           gs.class_scroll <- Float.min max (Float.max 0. (gs.class_grab_scroll -. dy));
           hovering := true)
     | None -> ());
  if inside class_view then hovering := true

let draw_class_col gs which x0 =
  let set = if which = 0 then fst gs.specials else snd gs.specials in
  let sel h = List.exists (fun n -> Catalog.token n = Catalog.token h.h_name) set in
  let y = ref (class_y0 -. gs.class_scroll) in
  let seen (r : rect) = r.y +. r.h > class_y0 && r.y < class_y1 in
  List.iter
    (fun (author, houses) ->
      let hr = rect x0 !y class_col_w class_head_h in
      if seen hr then
        checkbox ~size:22. ~box_s:16. ~release:true ~armed:(not gs.class_panning) ~clip:class_view
          ~checked:(List.for_all sel houses) ~x:x0 ~y:!y ~max_w:class_col_w author (fun () ->
            toggle_group gs which houses);
      y := !y +. class_head_h;
      List.iteri
        (fun k (h : house) ->
          let r =
            rect
              (x0 +. (float (k mod class_cols) *. (class_item_w +. class_gap)))
              (!y +. (float (k / class_cols) *. class_row_h))
              class_item_w class_row_h
          in
          if seen r then
            checkbox ~size:15. ~box_s:14. ~release:true ~armed:(not gs.class_panning) ~clip:class_view
              ~checked:(sel h) ~x:r.x ~y:r.y ~max_w:r.w
              (if Assets.has_key h.h_key then Assets.text h.h_key else cap h.h_name)
              (fun () -> toggle_special gs which h.h_name))
        houses;
      y := !y +. (float (class_rows (List.length houses)) *. class_row_h);
      let sr = rect x0 (!y +. 3.) class_col_w 1. in
      if seen sr then fill sr (rgba 140 107 41 140);
      y := !y +. class_sep)
    Table.house_groups

let set_budget gs ms on_budget =
  gs.budget_ms <- ms;
  (try Prefs.set_setup_budget (Prefs.load ()) ms with _ -> ());
  on_budget ms

let set_sound gs v =
  let v = Float.max 0. (Float.min 1. v) in
  gs.sound <- v;
  try Prefs.set_sound_level (Prefs.load ()) v with _ -> ()

let set_lang gs code =
  gs.lang <- code;
  try Prefs.set_language (Prefs.load ()) code with _ -> ()

let draw_game_settings gs ~show_seed ~on_budget ~close =
  click_full (fun () -> close_dialog "settings" close);
  let box = settings_box in
  paper box;
  on_click box (fun () -> ());
  text_c ~font:paper_font ~size:26. ~color:ink box (box.y +. 14.) (Assets.text "settings.title");
  ptabs ~x:(box.x +. 24.) ~y:(box.y +. 46.) ~w:(box.w -. 48.) ~selected:gs.tab
    [ "settings.duelclasses"; "Game"; "settings.sound"; "settings.lang" ] (fun k -> gs.tab <- k);
  (match gs.tab with
  | 0 ->
      let side_head x0 which =
        let aw = paper_btn_w ~size:16. "all" in
        pbutton ~size:16.
          (rect x0 (box.y +. 82.) aw 26.)
          "all"
          (fun () -> set_side gs which (all_specials ()));
        pbutton ~size:16.
          (rect (x0 +. aw +. 6.) (box.y +. 82.) (paper_btn_w ~size:16. "clear") 26.)
          "clear"
          (fun () -> set_side gs which [])
      in
      side_head class_x0 0;
      side_head class_x1 1;
      handle_class_pan gs;
      begin_clip class_view;
      draw_class_col gs 0 class_x0;
      draw_class_col gs 1 class_x1;
      end_clip ();
      let max = class_maxscroll () in
      if max > 0. then (
        fill class_bar (rgba 140 107 41 90);
        let view_h = class_y1 -. class_y0 in
        let th = Float.max 24. (view_h *. view_h /. class_content_h ()) in
        let ty = class_y0 +. (gs.class_scroll /. max *. (view_h -. th)) in
        let thumb = rect class_bar.x ty class_bar.w th in
        fill ~round:0.06 thumb (rgba 122 90 40 255);
        if inside class_bar then hovering := true;
        if gs.class_panning then gs.class_drag <- false
        else if R.is_mouse_button_down R.MouseButton.Left then (
          if gs.class_drag || inside class_bar then (
            gs.class_drag <- true;
            gs.class_grab_y <- None;
            hovering := true;
            let _, my = mouse () in
            let span = Float.max 1. (view_h -. th) in
            let f = (my -. class_y0 -. (th /. 2.)) /. span in
            gs.class_scroll <- Float.min max (Float.max 0. (f *. max))))
        else gs.class_drag <- false)
      else (
        gs.class_drag <- false;
        class_clamp gs)
  | 1 ->
      text ~font:paper_font ~size:20. ~color:ink_soft (box.x +. 24.) (box.y +. 96.) "Bot time per move";
      let bx = ref (box.x +. 24.) in
      List.iter
        (fun ms ->
          let label = if ms < 1000 then Printf.sprintf "%d ms" ms else Printf.sprintf "%d s" (ms / 1000) in
          let w = paper_btn_w ~size:16. label in
          pbutton ~selected:(ms = gs.budget_ms) ~size:16.
            (rect !bx (box.y +. 122.) w 28.)
            label
            (fun () -> set_budget gs ms on_budget);
          bx := !bx +. w +. 6.)
        budget_options;
      if show_seed then (
        text ~font:paper_font ~size:20. ~color:ink_soft (box.x +. 24.) (box.y +. 170.) "Seed";
        fill gs_seed_field (if gs.seed_focus then rgba 255 252 242 255 else rgba 235 209 140 255);
        outline gs_seed_field (if gs.seed_focus then accent else paper_line);
        let caret = if gs.seed_focus && Float.rem (R.get_time ()) 1. < 0.5 then "|" else "" in
        text ~font:paper_font ~size:20.
          ~color:(if gs.seed_text = "" && not gs.seed_focus then ink_soft else ink)
          (gs_seed_field.x +. 10.) (gs_seed_field.y +. 5.)
          (if gs.seed_text = "" && not gs.seed_focus then "random" else gs.seed_text ^ caret);
        on_click gs_seed_field (fun () -> gs.seed_focus <- true);
        pbutton ~size:18.
          (rect (gs_seed_field.x +. 210.) gs_seed_field.y (paper_btn_w ~size:18. "Random") 28.)
          "Random"
          (fun () ->
            gs.seed_text <- "";
            gs.seed_focus <- false));
      let debug_y = if show_seed then box.y +. 248. else box.y +. 170. in
      checkbox ~checked:gs.debug ~x:(box.x +. 24.) ~y:debug_y ~max_w:(box.w -. 48.)
        (Assets.text "settings.debug") (fun () ->
          gs.debug <- not gs.debug;
          try Prefs.set_debug_mode (Prefs.load ()) gs.debug with _ -> ())
  | 2 ->
      text ~font:paper_font ~size:20. ~color:ink_soft (box.x +. 24.) (box.y +. 220.)
        (Assets.text "sound.level");
      let minus_w = paper_btn_w ~size:18. "-" in
      pbutton ~size:18.
        (rect (box.x +. 24.) (box.y +. 248.) minus_w 30.)
        "-"
        (fun () -> set_sound gs (gs.sound -. 0.1));
      text_c ~font:paper_font ~size:20. ~color:ink
        (rect (box.x +. 24. +. minus_w) (box.y +. 248.) 64. 30.)
        (box.y +. 252.)
        (Printf.sprintf "%d%%" (int_of_float ((gs.sound *. 100.) +. 0.5)));
      let plus_x = box.x +. 24. +. minus_w +. 64. in
      let plus_w = paper_btn_w ~size:18. "+" in
      pbutton ~size:18.
        (rect plus_x (box.y +. 248.) plus_w 30.)
        "+"
        (fun () -> set_sound gs (gs.sound +. 0.1));
      pbutton ~size:16.
        (rect (plus_x +. plus_w +. 6.) (box.y +. 248.) (paper_btn_w ~size:16. "test") 30.)
        "test"
        (fun () ->
          set_sound gs gs.sound;
          play_sound "move");
      checkbox ~checked:gs.ambient_enabled ~x:(box.x +. 24.) ~y:(box.y +. 296.) ~max_w:(box.w -. 48.)
        (Assets.text "sound.ambient") (fun () ->
          gs.ambient_enabled <- not gs.ambient_enabled;
          try Prefs.set_ambient_sound (Prefs.load ()) gs.ambient_enabled with _ -> ())
  | _ ->
      let lx = ref (box.x +. 24.) in
      List.iter
        (fun code ->
          let w = paper_btn_w ~size:18. code in
          pbutton ~selected:(code = gs.lang) ~size:18.
            (rect !lx (box.y +. 220.) w 30.)
            code
            (fun () -> set_lang gs code);
          lx := !lx +. w +. 6.)
        languages;
      text ~font:paper_font ~size:18. ~color:ink_soft (box.x +. 24.) (box.y +. 264.)
        (Assets.text "settings.lang.restart"));
  paper_btn_row ~size:18.
    ~cx:(box.x +. (box.w /. 2.))
    ~y:(box.y +. box.h -. 52.)
    [ ("Close", fun () -> close_dialog "settings" close) ]

let settings_seed_keys gs =
  if gs.seed_focus then (
    let rec chars () =
      let c = Uchar.to_int (R.get_char_pressed ()) in
      if c <> 0 then (
        if c >= Char.code '0' && c <= Char.code '9' && String.length gs.seed_text < 9 then
          gs.seed_text <- gs.seed_text ^ String.make 1 (Char.chr c);
        chars ())
    in
    chars ();
    if R.is_key_pressed R.Key.Backspace && gs.seed_text <> "" then
      gs.seed_text <- String.sub gs.seed_text 0 (String.length gs.seed_text - 1));
  if R.is_mouse_button_pressed R.MouseButton.Left && not (inside gs_seed_field) then gs.seed_focus <- false
