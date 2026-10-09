(* Raylib UI: the same Table as the terminal UI, drawn with the vibe card art.
   Layout uses Scala's 832×768 world, scaled to a window that fills the
   monitor height. Bots search on a second domain so the window keeps
   drawing; their move is submitted here. *)

open Mancer
open Sp
module R = Raylib

(* ------------------------------------------------------------ frame scale *)

let vw = 832.
let vh = 768.

type rect = { x : float; y : float; w : float; h : float }

let rect x y w h = { x; y; w; h }
let scale = ref 1.
let off_x = ref 0.
let off_y = ref 0.

let fit_frame () =
  let sw = float (R.get_screen_width ()) and sh = float (R.get_screen_height ()) in
  scale := Float.min (sw /. vw) (sh /. vh);
  off_x := (sw -. (vw *. !scale)) /. 2.;
  off_y := (sh -. (vh *. !scale)) /. 2.

let screen r =
  R.Rectangle.create (!off_x +. (r.x *. !scale)) (!off_y +. (r.y *. !scale)) (r.w *. !scale) (r.h *. !scale)

let pos x y = R.Vector2.create (!off_x +. (x *. !scale)) (!off_y +. (y *. !scale))

let mouse () =
  let m = R.get_mouse_position () in
  ((R.Vector2.x m -. !off_x) /. !scale, (R.Vector2.y m -. !off_y) /. !scale)

let inside r =
  let mx, my = mouse () in
  mx >= r.x && mx < r.x +. r.w && my >= r.y && my < r.y +. r.h

(* ----------------------------------------------------------------- colors *)

let rgba r g b a = R.Color.create r g b a
let bg = rgba 14 16 24 255
let panel = rgba 24 27 38 225
let panel_hi = rgba 40 45 62 235
let line = rgba 70 76 98 255
let fg = rgba 225 228 236 255
let muted = rgba 140 146 166 255
let faint = rgba 90 95 112 255
let accent = rgba 94 196 255 255
let gold = rgba 246 196 82 255
let red = rgba 236 98 92 255
let green = rgba 108 210 128 255
let shade = rgba 0 0 0 150

(* fire, water, air, earth, special *)
let house_color = function
  | 0 -> rgba 232 104 60 255
  | 1 -> rgba 74 144 238 255
  | 2 -> rgba 150 214 236 255
  | 3 -> rgba 126 186 82 255
  | _ -> rgba 188 132 236 255

(* ------------------------------------------------------------------ text *)

let regular = ref (R.get_font_default ())
let bold = ref (R.get_font_default ())

let load_fonts () =
  let codepoints =
    List.init (127 - 32) (fun i -> i + 32)
    @ List.init (256 - 160) (fun i -> i + 160)
    @ [ 0x2013; 0x2014; 0x2018; 0x2019; 0x201C; 0x201D; 0x2022; 0x2026 ]
  in
  let load paths =
    match List.find_opt Sys.file_exists paths with
    | None -> R.get_font_default ()
    | Some p ->
        let f = R.load_font_ex p 64 (Some (Ctypes.CArray.of_list Ctypes.int codepoints)) in
        R.set_texture_filter (R.Font.texture f) R.TextureFilter.Bilinear;
        f
  in
  let fonts = "/usr/share/fonts/truetype/" in
  regular := load [ fonts ^ "noto/NotoSans-Regular.ttf"; fonts ^ "dejavu/DejaVuSans.ttf" ];
  bold := load [ fonts ^ "noto/NotoSans-Bold.ttf"; fonts ^ "dejavu/DejaVuSans-Bold.ttf" ]

let text_w ?(font = regular) size s = R.Vector2.x (R.measure_text_ex !font s size 0.)

let text ?(font = regular) ?(size = 18.) ?(color = fg) x y s =
  R.draw_text_ex !font s (pos x y) (size *. !scale) 0. color

let text_c ?font ?size ?color r y s =
  let size' = Option.value size ~default:18. in
  text ?font ?size ?color (r.x +. ((r.w -. text_w ?font size' s) /. 2.)) y s

(* cropped to [w] with an ellipsis *)
let fit ?font size w s =
  if text_w ?font size s <= w then s
  else
    let rec go n =
      if n = 0 then ""
      else
        let t = String.sub s 0 n ^ "…" in
        if text_w ?font size t <= w then t else go (n - 1)
    in
    go (String.length s)

let wrap ?font size w s =
  let words = String.split_on_char ' ' s |> List.filter (( <> ) "") in
  let lines, cur =
    List.fold_left
      (fun (lines, cur) word ->
        let next = if cur = "" then word else cur ^ " " ^ word in
        if cur <> "" && text_w ?font size next > w then (cur :: lines, word) else (lines, next))
      ([], "") words
  in
  List.rev (if cur = "" then lines else cur :: lines)

(* ---------------------------------------------------------------- shapes *)

let fill ?(round = 0.12) r c = R.draw_rectangle_rounded (screen r) round 6 c

let outline ?(round = 0.12) ?(thick = 1.5) r c =
  R.draw_rectangle_rounded_lines_ex (screen r) round 6 (thick *. !scale) c

(* ---------------------------------------------------------------- images *)

let textures : (string, R.Texture.t option) Hashtbl.t = Hashtbl.create 64

let texture = function
  | None -> None
  | Some path -> (
      match Hashtbl.find_opt textures path with
      | Some t -> t
      | None ->
          let t = R.load_texture path in
          let t =
            if R.is_texture_valid t then (
              R.set_texture_filter t R.TextureFilter.Bilinear;
              Some t)
            else None
          in
          Hashtbl.replace textures path t;
          t)

let sounds : (string, R.Sound.t option) Hashtbl.t = Hashtbl.create 16

let sound name =
  match Hashtbl.find_opt sounds name with
  | Some s -> s
  | None ->
      let s =
        match Assets.sound name with
        | None -> None
        | Some path ->
            let s = R.load_sound path in
            if R.is_sound_valid s then Some s else None
      in
      Hashtbl.replace sounds name s;
      s

let play_sound ?(volume = 0.7) name =
  Option.iter
    (fun s ->
      R.set_sound_volume s volume;
      R.play_sound s)
    (sound name)

(* Scala SpellCast.playSound: English name, spaces stripped, looked up in sounds{}. *)
let play_card_sound (c : card) =
  let name = Assets.card_name c |> String.lowercase_ascii |> String.split_on_char ' ' |> String.concat "" in
  play_sound ~volume:0.9 name

(* [r] covered, cropped to its aspect ratio *)
let image ?(tint = R.Color.white) tex r =
  let tw = float (R.Texture.width tex) and th = float (R.Texture.height tex) in
  let ra = r.w /. r.h in
  let sw, sh = if tw /. th > ra then (th *. ra, th) else (tw, tw /. ra) in
  let src = R.Rectangle.create ((tw -. sw) /. 2.) ((th -. sh) /. 2.) sw sh in
  R.draw_texture_pro tex src (screen r) (R.Vector2.create 0. 0.) 0. tint

(* [r] filled, texture stretched (frames keep native aspect because [r] does) *)
let image_at ?(tint = R.Color.white) tex r =
  let src = R.Rectangle.create 0. 0. (float (R.Texture.width tex)) (float (R.Texture.height tex)) in
  R.draw_texture_pro tex src (screen r) (R.Vector2.create 0. 0.) 0. tint

(* Scala combat/slot is 112×140 with the gold well only in the middle. Crop
   to that well so an empty slot can be drawn at card size. *)
let slot_src tex =
  let tw = float (R.Texture.width tex) and th = float (R.Texture.height tex) in
  if int_of_float tw = 112 && int_of_float th = 140 then R.Rectangle.create 15. 24. 81. 95.
  else R.Rectangle.create 0. 0. tw th

let image_slot ?(tint = R.Color.white) tex r =
  R.draw_texture_pro tex (slot_src tex) (screen r) (R.Vector2.create 0. 0.) 0. tint

let card_texture c = texture (Assets.card_art c)

(* Scala combat frames: creature raka 90x102, spell rakaSpell 94x104. Art and
   labels are laid out in that native space (libGDX origin was bottom-left;
   these numbers are top-left). *)
let creature_native = (90., 102.)
let spell_native = (94., 104.)
let creature_art_box = (3., 8., 80., 80.)
let spell_art_box = (0., 2., 86., 94.)
let cost_blue = rgba 40 72 220 255
let life_ok = rgba 0 160 0 255

let fit_in r nw nh =
  let s = Float.min (r.w /. nw) (r.h /. nh) in
  let w, h = (nw *. s, nh *. s) in
  (rect (r.x +. ((r.w -. w) /. 2.)) (r.y +. ((r.h -. h) /. 2.)) w h, s)

let box fr (nx, ny, nw, nh) s = rect (fr.x +. (nx *. s)) (fr.y +. (ny *. s)) (nw *. s) (nh *. s)

(* Scala [application.conf]: label pos is the bottom-left of an ~18px actor. *)
let label_at fr s ~nh ~bl_x ~bl_y ~color str =
  let size = Float.max 13. (16. *. s) in
  let lh = 18. *. s in
  let x = fr.x +. (bl_x *. s) in
  let y = fr.y +. (((nh -. bl_y) *. s) -. lh) in
  text ~font:bold ~size ~color:(rgba 0 0 0 190) (x +. 1.) (y +. 1.) str;
  text ~font:bold ~size ~color x y str

(* Frame + card art, then cost (top right), attack (bottom left), life (bottom
   right). No name — same as the Scala card button. *)
let draw_card_face desc r (c : card) ~cost ~attack ~life ~life_color ~tint =
  let spell = c.is_spell in
  let nw, nh = if spell then spell_native else creature_native in
  let fr, s = fit_in r nw nh in
  let ax, ay, aw, ah = if spell then spell_art_box else creature_art_box in
  let art = box fr (ax, ay, aw, ah) s in
  (match card_texture c with Some t -> image ~tint t art | None -> fill ~round:0.04 art panel_hi);
  (match texture (if spell then Assets.spell_frame () else Assets.creature_frame ()) with
  | Some t -> image_at ~tint t fr
  | None -> outline ~round:0.08 ~thick:2. fr (rgba 180 168 140 255));
  Option.iter
    (fun n ->
      label_at fr s ~nh ~bl_x:68. ~bl_y:(if spell then 77. else 83.) ~color:cost_blue (string_of_int n))
    cost;
  Option.iter (fun str -> label_at fr s ~nh ~bl_x:2. ~bl_y:5. ~color:red str) attack;
  Option.iter (fun n -> label_at fr s ~nh ~bl_x:67. ~bl_y:5. ~color:life_color (string_of_int n)) life

let printed_attack (c : card) =
  match c.creature_body with
  | Some b -> ( match b.b_attack.base with Some a -> Some (string_of_int a) | None -> Some "X")
  | None -> None

let printed_life (c : card) = match c.creature_body with Some b -> Some b.b_life | None -> None

(* Hover smoke: Scala HoveredActor, particle/smoke.p, drawn under the board. *)
type puff = {
  mutable x : float;
  mutable y : float;
  vx : float;
  mutable vy : float;
  born : float;
  life : float;
}

type smoke = {
  rng : Random.State.t;
  mutable on : bool;
  mutable key : float * float;
  mutable puffs : puff list;
  mutable emit : float;
}

let smoke = { rng = Random.State.make_self_init (); on = false; key = (0., 0.); puffs = []; emit = 0. }

let smoke_reset () =
  smoke.puffs <- [];
  smoke.emit <- 0.

let smoke_hide () =
  smoke.on <- false;
  smoke_reset ()

(* Card group origin is the top-left; Scala places the emitter at +40,+50 in Y-up. *)
let smoke_show (r : rect) =
  let cx = r.x +. 40. and cy = r.y +. r.h -. 50. in
  if (not smoke.on) || smoke.key <> (cx, cy) then (
    smoke_reset ();
    smoke.key <- (cx, cy));
  smoke.on <- true

let smoke_tint u =
  let lerp a b t = a +. ((b -. a) *. t) in
  let r, g, b =
    if u < 0.546 then
      let t = u /. 0.546 in
      (lerp 0.533 0.588 t, lerp 0.537 0.635 t, lerp 0.561 0.659 t)
    else
      let t = (u -. 0.546) /. 0.454 in
      (lerp 0.588 0.153 t, lerp 0.635 0.165 t, lerp 0.659 0.314 t)
  in
  let a = if u < 0.555 then lerp 0.316 0.982 (u /. 0.555) else lerp 0.982 0.035 ((u -. 0.555) /. 0.445) in
  let byte x = int_of_float (Float.max 0. (Float.min 255. (x *. 255.))) in
  rgba (byte r) (byte g) (byte b) (byte a)

let draw_smoke () =
  let dt = R.get_frame_time () in
  let now = R.get_time () in
  if not smoke.on then ()
  else (
    smoke.emit <- smoke.emit +. (dt *. 30.);
    let cx, cy = smoke.key in
    while smoke.emit >= 1. && List.length smoke.puffs < 100 do
      smoke.emit <- smoke.emit -. 1.;
      let ang = Random.State.float smoke.rng (Float.pi *. 2.) in
      smoke.puffs <-
        {
          x = cx +. (Random.State.float smoke.rng 40. -. 20.);
          y = cy +. (Random.State.float smoke.rng 60. -. 30.);
          vx = cos ang *. 10.;
          vy = -.sin ang *. 10.;
          born = now;
          life = 2.;
        }
        :: smoke.puffs
    done;
    List.iter
      (fun p ->
        p.vy <- p.vy -. (10. *. dt);
        p.x <- p.x +. (p.vx *. dt);
        p.y <- p.y +. (p.vy *. dt))
      smoke.puffs;
    smoke.puffs <- List.filter (fun p -> now -. p.born < p.life) smoke.puffs;
    match texture (Assets.particle "smoke") with
    | None -> ()
    | Some tex ->
        let tw = float (R.Texture.width tex) and th = float (R.Texture.height tex) in
        let src = R.Rectangle.create 0. 0. tw th in
        List.iter
          (fun p ->
            let u = (now -. p.born) /. p.life in
            let size = 70. *. !scale in
            let dest =
              R.Rectangle.create
                (!off_x +. ((p.x -. 35.) *. !scale))
                (!off_y +. ((p.y -. 35.) *. !scale))
                size size
            in
            R.draw_texture_pro tex src dest (R.Vector2.create 0. 0.) 0. (smoke_tint u))
          smoke.puffs)

(* ---------------------------------------------------------------- widgets *)

(* Clicks are collected while drawing and run after the frame *)
let pending : (unit -> unit) option ref = ref None
let hovering = ref false
let clicked r = inside r && R.is_mouse_button_pressed R.MouseButton.Left

let on_click r f =
  if inside r then hovering := true;
  if clicked r then pending := Some f

let button ?(selected = false) ?(enabled = true) ?(size = 17.) r label f =
  let hot = enabled && inside r in
  fill r (if selected then rgba 40 98 140 240 else if hot then panel_hi else panel);
  outline r (if selected then accent else if hot then muted else line);
  text_c ~font:bold ~size ~color:(if enabled then fg else faint) r (r.y +. ((r.h -. size) /. 2.) -. 2.) label;
  if enabled then on_click r f

(* ------------------------------------------------------------- portraits *)

let player_portraits =
  [
    "ember";
    "tide";
    "gale";
    "grove";
    "old-thinker";
    "stout-hearth";
    "night-scholar";
    "ash-wanderer";
    "mist-seer";
    "lamp-scribe";
    "river-aunt";
    "grove-crone";
  ]

let portraits_for = function
  | Table.Human -> player_portraits
  | Table.Bot "lecturer" -> [ "lecturer"; "lecturer-f" ]
  | Table.Bot "oracle" -> [ "oracle"; "oracle-f" ]
  | Table.Bot _ -> [ "summoner"; "summoner-f" ]

let cap = String.capitalize_ascii

let portrait_label id =
  id |> String.split_on_char '-' |> List.filter (( <> ) "f") |> List.map cap |> String.concat " "

let cycle_portrait ids current delta =
  let n = List.length ids in
  let i = match List.find_index (( = ) current) ids with Some i -> i | None -> 0 in
  List.nth ids ((((i + delta) mod n) + n) mod n)

let clamp_portrait seat id =
  let ids = portraits_for seat in
  if List.mem id ids then id else List.hd ids

let draw_face ?tint id r =
  match texture (Assets.portrait id) with
  | Some t -> image ?tint t r
  | None ->
      fill r panel;
      text_c ~size:11. ~color:muted r (r.y +. (r.h /. 2.) -. 6.) "?"

let draw_portrait_chooser ~x ~y ~size ~col_w id ids set =
  let ctrl = 28. and gap = 8. in
  let total = ctrl +. gap +. size +. gap +. ctrl in
  let x0 = x +. ((col_w -. total) /. 2.) in
  let mid_y = y +. ((size -. 32.) /. 2.) in
  button ~size:16. (rect x0 mid_y ctrl 32.) "<" (fun () -> set (cycle_portrait ids id (-1)));
  let face = rect (x0 +. ctrl +. gap) y size size in
  draw_face id face;
  outline ~round:0.04 face (if inside face then muted else line);
  on_click face (fun () -> set (cycle_portrait ids id 1));
  button ~size:16. (rect (face.x +. size +. gap) mid_y ctrl 32.) ">" (fun () -> set (cycle_portrait ids id 1));
  text_c ~size:13. ~color:muted (rect x (y +. size) col_w 0.) (y +. size +. 8.) (portrait_label id)

(* ------------------------------------------------------------- setup ui *)

let seat_options = Table.Human :: List.map (fun id -> Table.Bot id) Tournament.bot_ids
let budget_options = [ 100; 250; 500; 1000; 2000; 5000 ]

type setup_ui = {
  mutable setup : Table.setup;
  mutable portraits : string * string;
  mutable seed_text : string;
  mutable seed_focus : bool;
  mutable error : string;
}

let seed_field = rect 430. 560. 180. 34.

let draw_setup su ~start =
  let s = su.setup in
  text_c ~font:bold ~size:40. (rect 0. 0. vw 0.) 28. "mancer";
  text_c ~size:16. ~color:muted (rect 0. 0. vw 0.) 72. "new game";
  let specials = Table.special_houses () in
  let side i label seat special set_seat set_special =
    let x = if i = 0 then 16. else 424. in
    let box = rect x 96. 392. 300. in
    fill ~round:0.04 box panel;
    outline ~round:0.04 box line;
    text ~font:bold ~size:18. (x +. 16.) 108. label;
    text ~size:14. ~color:muted (x +. 16.) 140. "Seat";
    List.iteri
      (fun k opt ->
        button ~selected:(opt = seat) ~size:13.
          (rect (x +. 16. +. (float k *. 90.)) 160. 84. 32.)
          (cap (Table.seat_name opt))
          (fun () -> set_seat opt))
      seat_options;
    text ~size:14. ~color:muted (x +. 16.) 206. "Special house";
    List.iteri
      (fun k (h : house) ->
        let r = rect (x +. 16. +. (float k *. 72.)) 226. 66. 96. in
        let selected = Catalog.token h.h_name = Catalog.token special in
        (match texture (Assets.background h) with
        | Some t -> image ~tint:(if selected then R.Color.white else rgba 150 150 150 255) t r
        | None -> fill r panel);
        R.draw_rectangle_rec (screen (rect r.x (r.y +. r.h -. 24.) r.w 24.)) shade;
        text_c ~font:bold ~size:12. r (r.y +. r.h -. 20.) (cap h.h_name);
        outline ~round:0.06
          ~thick:(if selected then 3. else 1.5)
          r
          (if selected then gold else if inside r then muted else line);
        on_click r (fun () -> set_special h.h_name))
      specials
  in
  let seat0, seat1 = s.seats and sp0, sp1 = s.specials in
  let p0, p1 = su.portraits in
  let set_seat i v =
    let a, b = su.setup.seats and pa, pb = su.portraits in
    if i = 0 then (
      su.setup <- { su.setup with seats = (v, b) };
      su.portraits <- (clamp_portrait v pa, pb))
    else (
      su.setup <- { su.setup with seats = (a, v) };
      su.portraits <- (pa, clamp_portrait v pb))
  in
  side 0 "P1 · bottom, first" seat0 sp0 (set_seat 0) (fun v ->
      su.setup <- { su.setup with specials = (v, snd su.setup.specials) });
  side 1 "P2 · top" seat1 sp1 (set_seat 1) (fun v ->
      su.setup <- { su.setup with specials = (fst su.setup.specials, v) });
  draw_portrait_chooser ~x:16. ~y:408. ~size:96. ~col_w:392. p0 (portraits_for seat0) (fun v ->
      su.portraits <- (v, p1));
  draw_portrait_chooser ~x:424. ~y:408. ~size:96. ~col_w:392. p1 (portraits_for seat1) (fun v ->
      su.portraits <- (p0, v));
  text ~size:14. ~color:muted 16. 540. "Bot time per move";
  List.iteri
    (fun k ms ->
      button ~selected:(ms = s.budget_ms) ~size:13.
        (rect (16. +. (float k *. 66.)) 560. 60. 32.)
        (if ms < 1000 then Printf.sprintf "%d ms" ms else Printf.sprintf "%d s" (ms / 1000))
        (fun () -> su.setup <- { su.setup with budget_ms = ms }))
    budget_options;
  text ~size:14. ~color:muted 430. 540. "Seed";
  fill seed_field (if su.seed_focus then panel_hi else panel);
  outline seed_field (if su.seed_focus then accent else line);
  let caret = if su.seed_focus && Float.rem (R.get_time ()) 1. < 0.5 then "|" else "" in
  text ~size:16.
    ~color:(if su.seed_text = "" && not su.seed_focus then muted else fg)
    440. 566.
    (if su.seed_text = "" && not su.seed_focus then "random" else su.seed_text ^ caret);
  on_click seed_field (fun () -> su.seed_focus <- true);
  button ~size:14. (rect 618. 560. 90. 34.) "Random" (fun () ->
      su.seed_text <- "";
      su.seed_focus <- false);
  button ~size:20. (rect 316. 640. 200. 48.) "Start" start;
  if su.error <> "" then text_c ~color:red (rect 0. 0. vw 0.) (vh -. 90.) su.error;
  text_c ~size:14. ~color:faint (rect 0. 0. vw 0.) (vh -. 28.) "Enter starts   ·   Esc quits"

let setup_keys su ~start =
  if su.seed_focus then (
    let rec chars () =
      let c = Uchar.to_int (R.get_char_pressed ()) in
      if c <> 0 then (
        if c >= Char.code '0' && c <= Char.code '9' && String.length su.seed_text < 9 then
          su.seed_text <- su.seed_text ^ String.make 1 (Char.chr c);
        chars ())
    in
    chars ();
    if R.is_key_pressed R.Key.Backspace && su.seed_text <> "" then
      su.seed_text <- String.sub su.seed_text 0 (String.length su.seed_text - 1));
  if R.is_mouse_button_pressed R.MouseButton.Left && not (inside seed_field) then su.seed_focus <- false;
  if R.is_key_pressed R.Key.Enter then start ()

(* --------------------------------------------------------------- game ui *)

type thinking = { domain : unit Domain.t; result : (command option, exn) result option Atomic.t }
type mode = Browse | Target of command list
type last_played = { player : player_id; card : card option }

type slide = {
  card : card;
  cost : int option;
  attack : string option;
  life : int option;
  max_life : int option;
  sx : float;
  sy : float;
  tx : float;
  ty : float;
  start : float;
  duration : float;
  hides : (player_id * int) list;
  sound : string option;
  mutable sounded : bool;
}

(* A creature's attack (Scala card.run): straight toward the opposite row
   and back, linear both ways. *)
type attack = { a_pid : player_id; a_num : int; a_start : float }

(* Scala Focus: a 0.5 s scale pulse on the card whose effect fires *)
type focus = { f_pid : player_id; f_num : int; f_start : float }

(* Scala DamagableInt: a value that changed between two refreshes *)
type pop_at = Slot_life of player_id * int | Player_life of player_id | Mana of player_id * int
type pop = { at : pop_at; amount : int; p_start : float; mutable p_sounded : bool }

type game_ui = {
  table : Table.t;
  portraits : string * string;
  flashes : (player_id * int * float) list ref; (* summons, for a short highlight *)
  last_played : last_played option ref;
  discovered : (player_id * string, unit) Hashtbl.t;
  mutable mode : mode;
  mutable paused : bool;
  mutable settings_open : bool;
  mutable budget_ms : int;
  mutable message : string;
  mutable thinking : thinking option;
  mutable last_move : float;
  mutable hover : card option;
  mutable hover_opp : bool;
  mutable scroll_y : float;
  mutable slides : slide list;
  mutable anim_at : float;
  (* The engine resolves a move at once; the board is replayed from the
     snapshot taken at each refresh, on the same timeline as the slides. *)
  mutable frames : (float * game_state) list; (* oldest first, not shown yet *)
  mutable shown : game_state;
  mutable last_frame : game_state;
  mutable attacks : attack list;
  mutable focuses : focus list;
  mutable pops : pop list;
}

(* Filled once the board layout helpers exist, so submit can queue slides. *)
let board_event : (game_ui -> game_event -> unit) ref = ref (fun _ _ -> ())

let new_game setup portraits =
  let flashes = ref [] and last_played = ref None in
  let discovered = Hashtbl.create 16 in
  let ui_holder = ref None in
  let on_event e =
    (match e with
    | Ev_submitted { player; command; _ } ->
        last_played := Some { player; card = Option.map (fun c -> c.cmd_card) command };
        Option.iter (fun c -> Hashtbl.replace discovered (player, c.cmd_card.key) ()) command
    | Ev_summon { num; player; _ } | Ev_replaced { num; player; _ } ->
        flashes := (player, num, R.get_time ()) :: !flashes
    | Ev_spell_played c -> play_card_sound c.cmd_card
    | _ -> ());
    Option.iter (fun ui -> !board_event ui e) !ui_holder
  in
  let table = Table.start ~on_event setup in
  let ui =
    {
      table;
      portraits;
      flashes;
      last_played;
      discovered;
      mode = Browse;
      paused = false;
      settings_open = false;
      budget_ms = setup.budget_ms;
      message = "";
      thinking = None;
      last_move = R.get_time ();
      hover = None;
      hover_opp = false;
      scroll_y = 0.;
      slides = [];
      anim_at = 0.;
      frames = [];
      shown = Session.peek (Table.session table);
      last_frame = Session.peek (Table.session table);
      attacks = [];
      focuses = [];
      pops = [];
    }
  in
  ui_holder := Some ui;
  ui

let session ui = Table.session ui.table
let state ui = Session.state (session ui)
let desc ui = Session.desc (session ui)

(* The board as the animation has reached it *)
let view ui = ui.shown

(* Scala application.conf card.run is 1 s; its 5 px peak is hard to see at
   this scale, so the lunge goes further *)
let attack_duration = 1.
let attack_reach = 18.
let focus_duration = 0.5
let pop_duration = 1.
let pop_rise = 20.

let slides_busy ui =
  let now = R.get_time () in
  List.exists (fun s -> now < s.start +. s.duration) ui.slides
  || List.exists (fun a -> now < a.a_start +. attack_duration) ui.attacks
  || List.exists (fun f -> now < f.f_start +. focus_duration) ui.focuses
  || ui.frames <> []

let input_ok ui = not (slides_busy ui)

let human_to_move ui =
  match Table.to_move ui.table with Some pid when Table.seat ui.table pid = Human -> Some pid | _ -> None

let both_bots ui = Table.seat ui.table owner <> Human && Table.seat ui.table opponent <> Human

(* Blocks until a running search ends: nothing may submit while it reads the game *)
let settle ui =
  Option.iter (fun th -> Domain.join th.domain) ui.thinking;
  ui.thinking <- None

let guarded ui f =
  try
    ui.message <- "";
    f ()
  with e ->
    ui.paused <- true;
    ui.mode <- Browse;
    ui.message <- "error: " ^ Printexc.to_string e

let play ui cmd =
  if input_ok ui then
    guarded ui (fun () ->
        ui.mode <- Browse;
        Table.submit ui.table cmd;
        ui.last_move <- R.get_time ())

let pick_card ui cd hd =
  if not (input_ok ui) then ()
  else if not (Table.playable ui.table cd hd) then ui.message <- "not enough mana"
  else
    match Table.choices ui.table cd.cd_card with
    | [] -> ui.message <- "no legal target"
    | [ ({ cmd_input = None; _ } as c) ] -> play ui (Some c)
    | cmds ->
        ui.message <- "";
        ui.mode <- Target cmds

(* Starts or collects the bot's search *)
let drive_bot ui =
  match ui.thinking with
  | Some th -> (
      match Atomic.get th.result with
      | None -> ()
      | Some r -> (
          Domain.join th.domain;
          ui.thinking <- None;
          match r with
          | Ok cmd -> play ui cmd
          | Error e ->
              ui.paused <- true;
              ui.message <- "bot error: " ^ Printexc.to_string e))
  | None -> (
      match Table.to_move ui.table with
      | Some pid when Table.seat ui.table pid <> Human && (not ui.paused) && input_ok ui ->
          let delay = if both_bots ui then 0.6 else 0.35 in
          if R.get_time () -. ui.last_move >= delay then
            let result = Atomic.make None in
            let table = ui.table in
            let domain =
              Domain.spawn (fun () ->
                  Atomic.set result (Some (try Ok (Table.bot_command table) with e -> Error e)))
            in
            ui.thinking <- Some { domain; result }
      | _ -> ())

(* ---------------------------------------------------------------- board *)

(* Scala raka is 90×102. Empty slots use the same rect; the gold well is
   cropped/scaled to that size so it matches the creature card. *)
let card_w = 90.
let card_h = 102.
let card_gap = 0.
let house_col = 92.
let slot_w = card_w
let slot_h = card_h
let slot_gap = 10.
let pane_margin = 6.
let row_y pid = if pid = opponent then 66. else 224.
let hand_header_y = 334.
let hand_y = 356.
let n_hand_rows = 4
let house_header_h = 22.
let cards_block_h = (float n_hand_rows *. (card_h +. card_gap)) -. card_gap
let opp_header_y = 42.
let opp_hand_y0 = opp_header_y -. 4. -. cards_block_h
let max_scroll = -.opp_hand_y0 +. 40.
let last_played_h = 48.
let oy ui y = y +. ui.scroll_y

type layout = {
  pane_x : float;
  pane_w : float;
  slot_x0 : float;
  hand_x0 : float;
  preview_x : float;
  preview_w : float;
}

let lay =
  ref { pane_x = 800.; pane_w = 620.; slot_x0 = 800.; hand_x0 = 800.; preview_x = 420.; preview_w = 360. }

let make_layout table =
  let n = max (List.length (Table.hand table owner)) (List.length (Table.hand table opponent)) in
  let hand_w = float n *. house_col in
  let slots_w = (float nb_slots *. slot_w) +. (float (nb_slots - 1) *. slot_gap) in
  let pane_w = Float.max hand_w slots_w in
  let pane_x = vw -. pane_margin -. pane_w in
  let slot_x0 = pane_x +. pane_w -. slots_w in
  let hand_x0 = pane_x +. pane_w -. hand_w in
  let preview_x = 8. in
  let preview_w = Float.min 340. (Float.max 120. (hand_x0 -. 16.)) in
  { pane_x; pane_w; slot_x0; hand_x0; preview_x; preview_w }

let slot_x n = !lay.slot_x0 +. (float n *. (slot_w +. slot_gap))
let slot_rect ui pid num = rect (slot_x num) (oy ui (row_y pid)) slot_w slot_h

let hand_card_rect ui pid col k =
  let x = !lay.hand_x0 +. (float col *. house_col) in
  let cx = x +. ((house_col -. card_w) /. 2.) in
  let cards_y = if pid = opponent then opp_hand_y0 else hand_y in
  rect cx (oy ui (cards_y +. (float k *. (card_h +. card_gap)))) card_w card_h

let find_hand_rect ui pid (card : card) =
  let rec cols i = function
    | [] -> None
    | hd :: rest ->
        let rec rows k =
          if k >= Array.length hd.phd_cards || k >= n_hand_rows then cols (i + 1) rest
          else if hd.phd_cards.(k).cd_card.key = card.key then Some (hand_card_rect ui pid i k)
          else rows (k + 1)
        in
        rows 0
  in
  cols 0 (Table.hand ui.table pid)

let slot_hidden ui pid num =
  let now = R.get_time () in
  List.exists
    (fun s -> now < s.start +. s.duration && List.exists (fun (p, n) -> p = pid && n = num) s.hides)
    ui.slides

let schedule ui duration =
  let now = R.get_time () in
  if ui.anim_at < now then ui.anim_at <- now;
  let start = ui.anim_at in
  ui.anim_at <- start +. duration;
  start

let push_slide ui ~start ~duration ~hides ~(from_r : rect) ~(to_r : rect) ~sound card cost attack life
    max_life =
  ui.slides <-
    {
      card;
      cost;
      attack;
      life;
      max_life;
      sx = from_r.x;
      sy = from_r.y;
      tx = to_r.x;
      ty = to_r.y;
      start;
      duration;
      hides;
      sound;
      sounded = false;
    }
    :: ui.slides

let face_of_slot ui pid num =
  match (state ui).players.(pid).ps_slots.(num) with
  | None -> None
  | Some st ->
      Some
        ( st.card.c_card,
          Some st.card.c_card.cost,
          Some (string_of_int st.attack),
          Some st.life,
          Some st.max_life )

(* Life always pops; mana only on loud refreshes, so paying for a card is
   quiet (Scala CardPanel.refresh(silent)). *)
let diff_pops ~silent ~start (a : game_state) (b : game_state) =
  let pops = ref [] in
  let add at amount =
    if amount <> 0 then pops := { at; amount; p_start = start; p_sounded = false } :: !pops
  in
  Array.iteri
    (fun pid (pb : player_state) ->
      let pa = a.players.(pid) in
      add (Player_life pid) (pb.ps_life - pa.ps_life);
      if not silent then
        Array.iteri
          (fun i (h : house_state) -> add (Mana (pid, i)) (h.mana - pa.ps_houses.(i).mana))
          pb.ps_houses;
      Array.iteri
        (fun n sb ->
          match (pa.ps_slots.(n), sb) with
          | Some x, Some y when x.slot_id = y.slot_id -> add (Slot_life (pid, n)) (y.life - x.life)
          | _ -> ())
        pb.ps_slots)
    b.players;
  !pops

let push_frame ui ~silent =
  let start = Float.max (R.get_time ()) ui.anim_at in
  let st = Session.peek (session ui) in
  ui.pops <- diff_pops ~silent ~start ui.last_frame st @ ui.pops;
  ui.frames <- ui.frames @ [ (start, st) ];
  ui.last_frame <- st

let queue_slides ui e =
  lay := make_layout ui.table;
  match e with
  | Ev_summon { num; player; creature } ->
      let card = creature.c_card in
      let from_r =
        Option.value (find_hand_rect ui player card) ~default:(rect 0. (vh -. 600.) card_w card_h)
      in
      let to_r = slot_rect ui player num in
      let cost, attack, life, max_life =
        match face_of_slot ui player num with
        | Some (_, cost, attack, life, max_life) -> (cost, attack, life, max_life)
        | None -> (Some card.cost, printed_attack card, printed_life card, printed_life card)
      in
      let start = schedule ui 0.5 in
      push_slide ui ~start ~duration:0.5
        ~hides:[ (player, num) ]
        ~from_r ~to_r ~sound:(Some "move") card cost attack life max_life
  | Ev_move { num; dest; player; dest_player } ->
      Option.iter
        (fun (card, cost, attack, life, max_life) ->
          let start = schedule ui 1. in
          push_slide ui ~start ~duration:1.
            ~hides:[ (dest_player, dest) ]
            ~from_r:(slot_rect ui player num) ~to_r:(slot_rect ui dest_player dest) ~sound:(Some "move") card
            cost attack life max_life)
        (face_of_slot ui player num)
  | Ev_swap { num; dest; player; dest_player } -> (
      match (face_of_slot ui player num, face_of_slot ui dest_player dest) with
      | Some (a, ac, aa, al, am), Some (b, bc, ba, bl, bm) ->
          let start = schedule ui 1. in
          let src = slot_rect ui player num and dst = slot_rect ui dest_player dest in
          let hides = [ (player, num); (dest_player, dest) ] in
          push_slide ui ~start ~duration:1. ~hides ~from_r:src ~to_r:dst ~sound:(Some "move") a ac aa al am;
          push_slide ui ~start ~duration:1. ~hides ~from_r:dst ~to_r:src ~sound:None b bc ba bl bm
      | _ -> ())
  | Ev_run_slot { num; player } ->
      let a_start = schedule ui attack_duration in
      ui.attacks <- { a_pid = player; a_num = num; a_start } :: ui.attacks
  | Ev_focus { num; player; blocking } ->
      (* a blocking focus holds the timeline, as Scala's waitAction does *)
      let f_start = if blocking then schedule ui focus_duration else Float.max (R.get_time ()) ui.anim_at in
      ui.focuses <- { f_pid = player; f_num = num; f_start } :: ui.focuses
  | Ev_refresh { silent } -> push_frame ui ~silent
  (* the turn's mana income lands here: silent, as in Scala; card effects
     already popped on their own loud refresh *)
  | Ev_to_move _ | Ev_game_over _ -> push_frame ui ~silent:true
  | _ -> ()

let () = board_event := queue_slides

let tick_slides ui =
  let now = R.get_time () in
  List.iter
    (fun s ->
      if (not s.sounded) && now >= s.start then (
        s.sounded <- true;
        Option.iter (fun n -> play_sound n) s.sound))
    ui.slides;
  ui.slides <- List.filter (fun s -> now < s.start +. s.duration) ui.slides;
  let rec advance = function
    | (t, st) :: rest when t <= now ->
        ui.shown <- st;
        advance rest
    | rest -> rest
  in
  ui.frames <- advance ui.frames;
  ui.attacks <- List.filter (fun a -> now < a.a_start +. attack_duration) ui.attacks;
  ui.focuses <- List.filter (fun f -> now < f.f_start +. focus_duration) ui.focuses;
  List.iter
    (fun p ->
      if (not p.p_sounded) && now >= p.p_start then (
        p.p_sounded <- true;
        match p.at with
        | Mana _ -> ()
        | Player_life _ -> play_sound (if p.amount < 0 then "damage" else "heal")
        | Slot_life _ -> play_sound (if p.amount < 0 then "damage" else "creatureheal")))
    ui.pops;
  ui.pops <- List.filter (fun p -> now < p.p_start +. pop_duration) ui.pops

let draw_slides ui =
  let now = R.get_time () in
  List.iter
    (fun s ->
      if now >= s.start then
        let t = Float.min 1. ((now -. s.start) /. s.duration) in
        let x = s.sx +. ((s.tx -. s.sx) *. t) and y = s.sy +. ((s.ty -. s.sy) *. t) in
        let life_color = match (s.life, s.max_life) with Some l, Some m when l < m -> gold | _ -> life_ok in
        draw_card_face (desc ui) (rect x y card_w card_h) s.card ~cost:s.cost ~attack:s.attack ~life:s.life
          ~life_color ~tint:R.Color.white)
    ui.slides

let peek_smoke_card ui =
  if human_to_move ui <> Some owner then None
  else
    let selected = match ui.mode with Target (c :: _) -> Some c.cmd_card.key | _ -> None in
    let found = ref None in
    List.iteri
      (fun col hd ->
        Array.iteri
          (fun k cd ->
            if k < n_hand_rows && Option.is_none !found then
              let r = hand_card_rect ui owner col k in
              if inside r && Table.playable ui.table cd hd && selected <> Some cd.cd_card.key && input_ok ui
              then found := Some r)
          hd.phd_cards)
      (Table.hand ui.table owner);
    !found

let flag_names st =
  [
    (stun_flag, "stunned");
    (blocked_flag, "blocked");
    (paused_flag, "paused");
    (invincible_flag, "invincible");
    (cursed_flag, "cursed");
  ]
  |> List.filter_map (fun (f, n) -> if has_flag st f then Some n else None)
  |> String.concat " · "

(* 0 → reach → 0, toward the other row: P1 is at the bottom and goes up *)
let attack_offset ui pid num =
  let now = R.get_time () in
  match List.find_opt (fun a -> a.a_pid = pid && a.a_num = num && now >= a.a_start) ui.attacks with
  | None -> 0.
  | Some a ->
      let t = (now -. a.a_start) /. attack_duration in
      let d = attack_reach *. (1. -. Float.abs (1. -. (2. *. t))) in
      if pid = owner then -.d else d

(* Scala CardActors Focus: 1 + 0.05 sin(pi t) *)
let focus_scale ui pid num =
  let now = R.get_time () in
  match List.find_opt (fun f -> f.f_pid = pid && f.f_num = num && now >= f.f_start) ui.focuses with
  | None -> 1.
  | Some f -> 1. +. (0.05 *. sin (Float.pi *. Float.min 1. ((now -. f.f_start) /. focus_duration)))

let draw_slot ui pid num =
  let r = rect (slot_x num) (oy ui (row_y pid)) slot_w slot_h in
  let p = (view ui).players.(pid) in
  let targets =
    match ui.mode with
    | Target cmds -> List.filter (fun c -> c.cmd_input = Some num && Table.target_side c = Some pid) cmds
    | Browse -> []
  in
  let flash =
    List.exists (fun (fp, fn, t) -> fp = pid && fn = num && R.get_time () -. t < 0.9) !(ui.flashes)
  in
  if flash then fill ~round:0.06 r (rgba 70 64 30 180);
  (match targets with _ :: _ when inside r -> fill ~round:0.08 r (rgba 246 196 82 70) | _ -> ());
  (if not (List.mem num p.ps_slot_list) then text_c ~size:28. ~color:faint r (r.y +. (r.h /. 2.) -. 16.) "×"
   else
     match p.ps_slots.(num) with
     | None -> (
         match texture (Assets.slot_frame ()) with
         | Some t -> image_slot t r
         | None ->
             fill ~round:0.08 r panel;
             outline ~round:0.08 r line)
     | Some _ when slot_hidden ui pid num -> (
         match texture (Assets.slot_frame ()) with
         | Some t -> image_slot t r
         | None ->
             fill ~round:0.08 r panel;
             outline ~round:0.08 r line)
     | Some st ->
         let c = st.card.c_card in
         if inside r then ui.hover <- Some c;
         let life_color = if st.life < st.max_life then gold else life_ok in
         let r = { r with y = r.y +. attack_offset ui pid num } in
         let k = focus_scale ui pid num in
         let r =
           {
             x = r.x -. (r.w *. (k -. 1.) /. 2.);
             y = r.y -. (r.h *. (k -. 1.) /. 2.);
             w = r.w *. k;
             h = r.h *. k;
           }
         in
         draw_card_face (desc ui) r c ~cost:(Some c.cost)
           ~attack:(Some (string_of_int st.attack))
           ~life:(Some st.life) ~life_color ~tint:R.Color.white;
         let flags = flag_names st in
         if flags <> "" then text_c ~size:11. ~color:gold r (r.y +. r.h -. 2.) (fit 11. (r.w +. 8.) flags));
  match targets with
  | [] -> ()
  | c :: _ ->
      let hot = inside r in
      outline ~round:0.08 ~thick:(if hot then 3.5 else 2.5) r gold;
      if input_ok ui then on_click r (fun () -> play ui (Some c))

let portrait_size = 84.

(* Scala PortraitFlash: red or green, easing back to white over 0.5 s *)
let portrait_flash ui pid =
  let now = R.get_time () in
  List.find_map
    (fun p ->
      match p.at with
      | Player_life q when q = pid && now >= p.p_start && now -. p.p_start < 0.5 ->
          let u = (now -. p.p_start) /. 0.5 in
          let k = 1. -. ((1. -. u) *. (1. -. u)) in
          let r, g, b = if p.amount < 0 then (1., 0.28, 0.28) else (0.4, 1., 0.48) in
          let ch c = int_of_float (255. *. (c +. ((1. -. c) *. k))) in
          Some (rgba (ch r) (ch g) (ch b) 255)
      | _ -> None)
    ui.pops

let draw_player_bar ui pid =
  let x = 8. in
  let y = oy ui (row_y pid) in
  let face_id = (if pid = owner then fst else snd) ui.portraits in
  let face = rect x y portrait_size portrait_size in
  draw_face ?tint:(portrait_flash ui pid) face_id face;
  outline ~round:0.02 ~thick:1. face line;
  let seat = Table.seat ui.table pid in
  (match seat with
  | Table.Bot id -> text_c ~font:bold ~size:12. ~color:fg face (y +. portrait_size +. 2.) (cap id)
  | Table.Human -> ());
  let tx = x +. portrait_size +. 8. in
  let p = (view ui).players.(pid) in
  let special = Game_record.special_name (desc ui).gd_players.(pid) in
  text ~font:bold ~size:22. ~color:fg tx (y +. 8.) (string_of_int p.ps_life);
  text ~size:12. ~color:muted tx (y +. 36.) (fit 12. (!lay.slot_x0 -. tx -. 8.) (cap special))

let draw_board ui =
  draw_player_bar ui opponent;
  List.iter (fun n -> draw_slot ui opponent n) base_slot_list;
  List.iter
    (fun n ->
      text_c ~size:12. ~color:faint
        (rect (slot_x n) 0. slot_w 0.)
        (oy ui (row_y opponent +. slot_h +. 8.))
        (string_of_int n))
    base_slot_list;
  List.iter (fun n -> draw_slot ui owner n) base_slot_list;
  draw_player_bar ui owner

(* ----------------------------------------------------------------- hands *)

let reveal_hand ui pid = Table.seat ui.table pid = Human || (both_bots ui && pid = owner)

let draw_house_header ui pid hd col header_y =
  let x = !lay.hand_x0 +. (float col *. house_col) in
  let y = oy ui header_y in
  let p = (view ui).players.(pid) in
  let i = hd.phd_house.h_index in
  let name = cap hd.phd_house.h_name in
  let mana = string_of_int p.ps_houses.(i).mana in
  let pad_x = 3. in
  let max_w = house_col -. (pad_x *. 2.) in
  let rec fit size =
    if size <= 11. then size
    else
      let tw = text_w size (name ^ " ") +. text_w ~font:bold size mana in
      if tw <= max_w then size else fit (size -. 1.)
  in
  let size = fit 16. in
  let plate = rect x (y +. 1.) house_col (house_header_h -. 2.) in
  fill ~round:0.04 plate (rgba 10 8 5 148);
  let name_s = name ^ " " in
  let total = text_w size name_s +. text_w ~font:bold size mana in
  let tx = x +. ((house_col -. total) /. 2.) in
  let ty = y +. ((house_header_h -. size) /. 2.) -. 1. in
  text ~size tx ty name_s;
  text ~font:bold ~size (tx +. text_w size name_s) ty mana

let draw_hand_at ui pid ~cards_y ~header_y =
  let reveal_all = reveal_hand ui pid in
  let can_choose = human_to_move ui = Some pid in
  let selected = match ui.mode with Target (c :: _) -> Some c.cmd_card.key | _ -> None in
  List.iteri
    (fun col hd ->
      draw_house_header ui pid hd col header_y;
      let x = !lay.hand_x0 +. (float col *. house_col) in
      let cx = x +. ((house_col -. card_w) /. 2.) in
      Array.iteri
        (fun k cd ->
          if k < n_hand_rows then
            let r = rect cx (oy ui (cards_y +. (float k *. (card_h +. card_gap)))) card_w card_h in
            let c = cd.cd_card in
            let known = reveal_all || Hashtbl.mem ui.discovered (pid, c.key) in
            if known then (
              let playable = Table.playable ui.table cd hd in
              let active = can_choose && playable && input_ok ui in
              let is_selected = selected = Some c.key in
              let tint = if (not reveal_all) || playable then R.Color.white else rgba 110 110 110 255 in
              if is_selected then fill ~round:0.08 r (rgba 40 98 140 70)
              else if active && inside r then fill ~round:0.08 r (rgba 255 255 255 28);
              draw_card_face (desc ui) r c ~cost:(Some cd.cd_cost) ~attack:(printed_attack c)
                ~life:(printed_life c) ~life_color:life_ok ~tint;
              if is_selected then outline ~round:0.08 ~thick:3. r accent
              else if active && inside r then outline ~round:0.08 ~thick:2. r accent;
              if inside r then (
                ui.hover <- Some c;
                if pid = opponent then ui.hover_opp <- true);
              if active then on_click r (fun () -> pick_card ui cd hd)))
        hd.phd_cards)
    (Table.hand ui.table pid)

(* Scala DamageComponent: green "+X" or red "-X" rising 20 px in 1 s *)
let draw_pops ui =
  let now = R.get_time () in
  List.iter
    (fun p ->
      if now >= p.p_start then
        let anchor =
          match p.at with
          | Slot_life (pid, n) ->
              let r = slot_rect ui pid n in
              Some (r.x +. r.w -. 30., r.y +. r.h -. 30.)
          | Player_life pid -> Some (8. +. portrait_size +. 48., oy ui (row_y pid) +. 8.)
          | Mana (pid, i) ->
              let header_y = if pid = opponent then opp_header_y else hand_header_y in
              List.find_index (fun hd -> hd.phd_house.h_index = i) (Table.hand ui.table pid)
              |> Option.map (fun col ->
                  (!lay.hand_x0 +. (float (col + 1) *. house_col) -. 24., oy ui header_y -. 2.))
        in
        Option.iter
          (fun (x, y) ->
            let y = y -. (pop_rise *. Float.min 1. ((now -. p.p_start) /. pop_duration)) in
            let s = if p.amount > 0 then "+" ^ string_of_int p.amount else string_of_int p.amount in
            text ~font:bold ~size:18. ~color:(rgba 0 0 0 200) (x +. 1.) (y +. 1.) s;
            text ~font:bold ~size:18. ~color:(if p.amount > 0 then green else red) x y s)
          anchor)
    ui.pops

let draw_hands ui =
  (* Scala: opponent cards, then house labels toward the slots; player the reverse. *)
  draw_hand_at ui opponent ~cards_y:opp_hand_y0 ~header_y:opp_header_y;
  draw_hand_at ui owner ~cards_y:hand_y ~header_y:hand_header_y

(* ---------------------------------------------------------- left preview *)

let preview_top ui = oy ui (if ui.hover_opp then opp_hand_y0 else hand_y)
let last_played_rect _ui = rect 8. (vh -. last_played_h -. 8.) 180. last_played_h

let draw_card_art desc r (c : card) =
  match card_texture c with Some t -> image t r | None -> fill ~round:0.04 r panel_hi

let draw_preview ui =
  match ui.hover with
  | None -> ()
  | Some c ->
      let x = !lay.preview_x and w = !lay.preview_w in
      let y = preview_top ui in
      let art_s = if c.is_spell then 130. else 120. in
      let title = Printf.sprintf "%s (%d)" (Assets.card_name c) c.cost in
      let subtitle =
        match c.creature_body with
        | Some b ->
            Printf.sprintf "Life : %d  Attack : %s" b.b_life
              (match b.b_attack.base with Some a -> string_of_int a | None -> "X")
        | None -> "Spell"
      in
      let desc_text =
        let d = Assets.describe c in
        if d = "" || d = c.description then "" else d
      in
      let lines = if desc_text = "" then [] else wrap 14. (w -. 16.) desc_text in
      let text_h = 44. +. (float (min 5 (List.length lines)) *. 17.) in
      let box_h = Float.max art_s text_h +. 16. in
      let box = rect x y w box_h in
      fill ~round:0.04 box (rgba 0 0 0 90);
      let art = rect (x +. 8.) (y +. 8.) art_s art_s in
      draw_card_art (desc ui) art c;
      let tx = x +. 8. +. art_s +. 8. in
      let tw = w -. art_s -. 24. in
      List.iteri
        (fun i l -> text ~font:bold ~size:16. tx (y +. 10. +. (float i *. 18.)) l)
        (List.filteri (fun i _ -> i < 2) (wrap ~font:bold 16. tw title));
      text ~size:13. ~color:muted tx (y +. 50.) subtitle;
      List.iteri
        (fun i l -> text ~size:14. (x +. 8.) (y +. 8. +. art_s +. 4. +. (float i *. 17.)) l)
        (List.filteri (fun i _ -> i < 5) lines)

let draw_last_played ui =
  match !(ui.last_played) with
  | None -> ()
  | Some lp -> (
      let r = last_played_rect ui in
      fill ~round:0.06 r (rgba 10 8 5 148);
      if inside r then hovering := true;
      match lp.card with
      | Some c ->
          if inside r then ui.hover <- Some c;
          let thumb = 40. in
          draw_card_art (desc ui) (rect (r.x +. 6.) (r.y +. ((r.h -. thumb) /. 2.)) thumb thumb) c;
          text ~size:14. ~color:fg (r.x +. 52.) (r.y +. ((r.h -. 14.) /. 2.)) "Last played"
      | None ->
          text ~size:14. ~color:muted (r.x +. 12.)
            (r.y +. ((r.h -. 14.) /. 2.))
            (Printf.sprintf "P%d passed" (lp.player + 1)))

let game_over_text ui =
  match Session.status (session ui) with
  | Over { winner = Some w; reason } ->
      Some (Printf.sprintf "P%d (%s) wins · %s" (w + 1) (Table.seat_name (Table.seat ui.table w)) reason)
  | Over { winner = None; reason } -> Some ("Draw · " ^ reason)
  | To_move _ -> None

let icon_button r name f =
  let hot = inside r in
  if hot then hovering := true;
  (match texture (Assets.ui name) with
  | Some t ->
      let tint = if hot then gold else R.Color.white in
      image ~tint t r
  | None -> button ~size:11. r (cap name) f);
  if Option.is_some (texture (Assets.ui name)) then on_click r f

let draw_menu ui ~quit ~settings ~switch ~replay ~start_new =
  let icon, gap, pad = (40., 4., 4.) in
  let items =
    [ ("quit", quit); ("settings", settings); ("switch", switch); ("restart", replay); ("new", start_new) ]
  in
  let n = float (List.length items) in
  let bar = rect 8. 4. ((n *. icon) +. ((n -. 1.) *. gap) +. (pad *. 2.)) (icon +. (pad *. 2.)) in
  fill ~round:0.08 bar (rgba 18 13 5 178);
  outline ~round:0.08 ~thick:1.5 bar (rgba 107 82 31 217);
  List.iteri
    (fun i (name, f) ->
      let r = rect (bar.x +. pad +. (float i *. (icon +. gap))) (bar.y +. pad) icon icon in
      icon_button r name f)
    items;
  Option.iter
    (fun s ->
      text ~font:bold ~size:14. ~color:green
        (bar.x +. bar.w +. 10.)
        (bar.y +. 14.)
        (fit ~font:bold 14. (vw -. bar.x -. bar.w -. 18.) s))
    (game_over_text ui);
  if ui.message <> "" then text ~size:12. ~color:red 8. (bar.y +. bar.h +. 4.) (fit 12. 400. ui.message)

let draw_pass ui =
  let bw, bh = (70., 28.) in
  let x = 8. +. ((portrait_size -. bw) /. 2.) in
  let y0 = oy ui (row_y opponent +. slot_h) in
  let y1 = oy ui (row_y owner) in
  let y = ((y0 +. y1) /. 2.) -. (bh /. 2.) in
  let r = rect x y bw bh in
  let enabled = Option.is_some (human_to_move ui) && input_ok ui in
  let icon = if enabled then "play-turn" else "play" in
  match texture (Assets.ui icon) with
  | Some t ->
      let ir = rect (x +. ((bw -. 26.) /. 2.)) (y +. ((bh -. 26.) /. 2.)) 26. 26. in
      if enabled && inside r then hovering := true;
      image ~tint:(if enabled then R.Color.white else rgba 160 160 160 180) t ir;
      if enabled then on_click r (fun () -> play ui None)
  | None -> button ~enabled ~size:14. r "Pass" (fun () -> play ui None)

let draw_settings ui =
  fill (rect 0. 0. vw vh) (rgba 0 0 0 120);
  on_click (rect 0. 0. vw vh) (fun () -> ui.settings_open <- false);
  let box = rect 206. 250. 420. 200. in
  fill ~round:0.04 box panel;
  outline ~round:0.04 box gold;
  on_click box (fun () -> ());
  text_c ~font:bold ~size:20. box (box.y +. 16.) "Settings";
  text ~size:14. ~color:muted (box.x +. 24.) (box.y +. 56.) "Bot time per move (next game)";
  List.iteri
    (fun k ms ->
      button ~selected:(ms = ui.budget_ms) ~size:13.
        (rect (box.x +. 24. +. (float k *. 62.)) (box.y +. 80.) 56. 32.)
        (if ms < 1000 then Printf.sprintf "%d ms" ms else Printf.sprintf "%d s" (ms / 1000))
        (fun () -> ui.budget_ms <- ms))
    budget_options;
  button ~size:16.
    (rect (box.x +. 150.) (box.y +. 140.) 120. 36.)
    "Close"
    (fun () -> ui.settings_open <- false)

(* ------------------------------------------------------------- main loop *)

type screen = Setup of setup_ui | Game of game_ui

let background ui =
  let house = (desc ui).gd_players.(owner).pd_houses |> fun hs -> hs.(Array.length hs - 1).phd_house in
  texture (Assets.background house)

let size_window_to_monitor () =
  let mon = R.get_current_monitor () in
  let mw = R.get_monitor_width mon and mh = R.get_monitor_height mon in
  let mx = int_of_float (R.Vector2.x (R.get_monitor_position mon)) in
  let my = int_of_float (R.Vector2.y (R.get_monitor_position mon)) in
  let h = mh in
  let w = int_of_float (Float.round (float h *. vw /. vh)) in
  let w, h = if w > mw then (mw, int_of_float (Float.round (float mw *. vh /. vw))) else (w, h) in
  R.set_window_size w h;
  R.set_window_position (mx + ((mw - w) / 2)) my

let () =
  R.set_trace_log_level R.TraceLogLevel.Warning;
  R.set_config_flags R.ConfigFlags.(window_resizable + msaa_4x_hint + vsync_hint);
  R.init_window 832 768 "mancer";
  size_window_to_monitor ();
  R.set_window_min_size 416 384;
  R.set_exit_key R.Key.Null;
  R.set_target_fps 60;
  R.init_audio_device ();
  load_fonts ();
  let screen =
    ref
      (Setup
         {
           setup = Table.default_setup;
           portraits = ("ember", "oracle");
           seed_text = "";
           seed_focus = false;
           error = "";
         })
  in
  let start su () =
    let seed = if su.seed_text = "" then None else int_of_string_opt su.seed_text in
    match new_game { su.setup with seed } su.portraits with
    | ui -> screen := Game ui
    | exception Failure msg -> su.error <- msg
  in
  let quit = ref false in
  while not (!quit || R.window_should_close ()) do
    fit_frame ();
    pending := None;
    hovering := false;
    (match !screen with
    | Setup su ->
        setup_keys su ~start:(start su);
        if R.is_key_pressed R.Key.Escape then quit := true
    | Game ui ->
        drive_bot ui;
        tick_slides ui;
        let wheel = R.get_mouse_wheel_move () in
        if wheel <> 0. then ui.scroll_y <- Float.min max_scroll (Float.max 0. (ui.scroll_y +. (wheel *. 48.)));
        if R.is_key_pressed R.Key.Escape then
          if ui.settings_open then ui.settings_open <- false else ui.mode <- Browse
        else if R.is_mouse_button_pressed R.MouseButton.Right then ui.mode <- Browse;
        if R.is_key_pressed R.Key.Space && both_bots ui then ui.paused <- not ui.paused);
    R.begin_drawing ();
    R.clear_background bg;
    (match !screen with
    | Setup su -> draw_setup su ~start:(start su)
    | Game ui ->
        (match background ui with
        | Some t -> image ~tint:(rgba 90 90 100 255) t (rect 0. 0. vw vh)
        | None -> ());
        ui.hover <- None;
        ui.hover_opp <- false;
        lay := make_layout ui.table;
        (match peek_smoke_card ui with Some r -> smoke_show r | None -> smoke_hide ());
        draw_smoke ();
        (match !(ui.last_played) with
        | Some { card = Some c; _ } when inside (last_played_rect ui) -> ui.hover <- Some c
        | _ -> ());
        let setup_now () = { (Table.setup ui.table) with budget_ms = ui.budget_ms } in
        let to_setup () =
          settle ui;
          let s = setup_now () in
          screen :=
            Setup
              {
                setup = s;
                portraits = ui.portraits;
                seed_text = (match s.seed with Some n -> string_of_int n | None -> "");
                seed_focus = false;
                error = "";
              }
        in
        let restart seed portraits =
          settle ui;
          screen := Game (new_game { (setup_now ()) with seed } portraits)
        in
        let switch () =
          let a, b = (setup_now ()).seats and sa, sb = (setup_now ()).specials and pa, pb = ui.portraits in
          settle ui;
          screen :=
            Game (new_game { (setup_now ()) with seats = (b, a); specials = (sb, sa); seed = None } (pb, pa))
        in
        draw_board ui;
        draw_pass ui;
        draw_hands ui;
        draw_slides ui;
        draw_pops ui;
        draw_preview ui;
        draw_last_played ui;
        draw_menu ui
          ~quit:(fun () -> to_setup ())
          ~settings:(fun () -> ui.settings_open <- not ui.settings_open)
          ~switch
          ~replay:(fun () -> restart (Some (Table.seed ui.table)) ui.portraits)
          ~start_new:(fun () -> restart None ui.portraits);
        if ui.settings_open then draw_settings ui);
    R.end_drawing ();
    R.set_mouse_cursor (if !hovering then R.MouseCursor.Pointing_hand else R.MouseCursor.Default);
    (* only humans submit from a click, never while a search reads the board;
       leaving a game settles the search first *)
    Option.iter (fun f -> f ()) !pending
  done;
  (match !screen with Game ui -> settle ui | Setup _ -> ());
  Hashtbl.iter (fun _ s -> Option.iter R.unload_sound s) sounds;
  R.close_audio_device ();
  R.close_window ()
