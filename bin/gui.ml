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

(* Thin black title bar in screen pixels, drawn in place of the desktop's
   large CSD so the window reads like Scala's "The Mancer's echo" chrome. *)
let chrome_h = 22.

type rect = { x : float; y : float; w : float; h : float }

let rect x y w h = { x; y; w; h }
let scale = ref 1.
let off_x = ref 0.
let off_y = ref 0.

let fit_frame () =
  let sw = float (R.get_screen_width ()) in
  let sh = Float.max 1. (float (R.get_screen_height ()) -. chrome_h) in
  scale := Float.min (sw /. vw) (sh /. vh);
  off_x := (sw -. (vw *. !scale)) /. 2.;
  off_y := chrome_h +. ((sh -. (vh *. !scale)) /. 2.)

let screen r =
  R.Rectangle.create (!off_x +. (r.x *. !scale)) (!off_y +. (r.y *. !scale)) (r.w *. !scale) (r.h *. !scale)

let pos x y = R.Vector2.create (!off_x +. (x *. !scale)) (!off_y +. (y *. !scale))

let mouse () =
  let m = R.get_mouse_position () in
  ((R.Vector2.x m -. !off_x) /. !scale, (R.Vector2.y m -. !off_y) /. !scale)

let inside r =
  let mx, my = mouse () in
  mx >= r.x && mx < r.x +. r.w && my >= r.y && my < r.y +. r.h

let overlap a b =
  let x = Float.max a.x b.x and y = Float.max a.y b.y in
  let x2 = Float.min (a.x +. a.w) (b.x +. b.w) and y2 = Float.min (a.y +. a.h) (b.y +. b.h) in
  if x2 > x && y2 > y then Some (rect x y (x2 -. x) (y2 -. y)) else None

(* Raylib has no z-index; scissor cuts later draws to [r] in world space. *)
let begin_clip r =
  let x = !off_x +. (r.x *. !scale) and y = !off_y +. (r.y *. !scale) in
  R.begin_scissor_mode
    (int_of_float (Float.floor x))
    (int_of_float (Float.floor y))
    (int_of_float (Float.ceil (r.w *. !scale)))
    (int_of_float (Float.ceil (r.h *. !scale)))

let end_clip () = R.end_scissor_mode ()

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

(* Dark ink on paper dialogs (Scala uiskin ink/inkSoft). *)
let ink = rgba 46 28 23 255
let ink_soft = rgba 87 64 54 255
let paper_line = rgba 122 90 40 255

(* Multiplies every later fill/text/image. The battle backdrop fade uses this
   the same way libGDX parentAlpha tints a group. *)
let draw_alpha = ref 1.

let with_alpha a f =
  let prev = !draw_alpha in
  draw_alpha := prev *. Float.max 0. (Float.min 1. a);
  Fun.protect ~finally:(fun () -> draw_alpha := prev) f

let alpha_color c =
  let k = !draw_alpha in
  if k >= 0.999 then c else R.fade c k

(* libGDX Interpolation.fade: smootherstep, used by fadeIn/fadeOut. *)
let fade_smooth u =
  let a = Float.max 0. (Float.min 1. u) in
  a *. a *. a *. ((a *. ((a *. 6.) -. 15.)) +. 10.)

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
let paper_font = ref (R.get_font_default ())

let finish_font ?(mips = false) f =
  let tex = R.Font.texture f in
  if mips then (
    R.gen_texture_mipmaps (R.addr tex);
    R.Font.set_texture f tex;
    R.set_texture_filter (R.Font.texture f) R.TextureFilter.Trilinear)
  else R.set_texture_filter tex R.TextureFilter.Bilinear;
  f

(* Walk back to the start of the UTF-8 character that contains [i]. *)
let utf8_floor s i =
  let n = String.length s in
  let rec go i =
    if i <= 0 then 0
    else if i >= n then n
    else if Char.code s.[i] land 0xC0 = 0x80 then go (i - 1)
    else i
  in
  go i

let load_fonts () =
  (* Scala rasterizes a language-specific FreeType charset. Latin-1 is enough
     for en; French needs œ; Russian and Vietnamese sit outside Latin-1, so
     those glyphs were never in the atlas (Cyrillic vanished; Vietnamese lost
     ă/ơ/ư and tones). *)
  let range a b = List.init (b - a + 1) (fun i -> a + i) in
  let script =
    match Assets.lang () with
    | "ru" -> range 0x400 0x45F
    | "vi" -> range 0x100 0x17F @ [ 0x1A0; 0x1A1; 0x1AF; 0x1B0 ] @ range 0x1EA0 0x1EF9
    | "fr" | "es" -> range 0x100 0x17F
    | _ -> []
  in
  let codepoints =
    range 32 126 @ range 160 255 @ script
    @ [ 0x2013; 0x2014; 0x2018; 0x2019; 0x201C; 0x201D; 0x2022; 0x2026 ]
  in
  let codes = Ctypes.CArray.of_list Ctypes.int codepoints in
  let load ?(mips = false) size paths =
    match List.find_opt Sys.file_exists paths with
    | None -> R.get_font_default ()
    | Some p -> finish_font ~mips (R.load_font_ex p size (Some codes))
  in
  let fonts = "/usr/share/fonts/truetype/" in
  regular := load 64 [ fonts ^ "noto/NotoSans-Regular.ttf"; fonts ^ "dejavu/DejaVuSans.ttf" ];
  bold := load 64 [ fonts ^ "noto/NotoSans-Bold.ttf"; fonts ^ "dejavu/DejaVuSans-Bold.ttf" ];
  (* IM Fell English has neither Cyrillic nor Vietnamese, so those locales use
     a system serif on parchment too (Scala [I18n.useCommonSkin]). *)
  let paper_paths =
    let system_serif =
      [
        fonts ^ "noto/NotoSerif-Regular.ttf";
        fonts ^ "dejavu/DejaVuSerif.ttf";
        fonts ^ "liberation/LiberationSerif-Regular.ttf";
        fonts ^ "noto/NotoSans-Regular.ttf";
        fonts ^ "dejavu/DejaVuSans.ttf";
      ]
    in
    match Assets.lang () with
    | "ru" | "vi" -> system_serif
    | _ -> Filename.concat (Assets.root ()) "fonts/IMFellEnglish.ttf" :: system_serif
  in
  (* Regular IM Fell (or the common face), rasterized 3× like Scala so 18pt stays sharp. *)
  paper_font := load ~mips:true 54 paper_paths

let text_w ?(font = regular) size s = R.Vector2.x (R.measure_text_ex !font s size 0.)

let text ?(font = regular) ?(size = 18.) ?(color = fg) x y s =
  R.draw_text_ex !font s (pos x y) (size *. !scale) 0. (alpha_color color)

let text_c ?font ?size ?color r y s =
  let size' = Option.value size ~default:18. in
  text ?font ?size ?color (r.x +. ((r.w -. text_w ?font size' s) /. 2.)) y s

(* cropped to [w] with an ellipsis; [n] is a UTF-8 boundary so we never split a letter *)
let fit ?font size w s =
  if text_w ?font size s <= w then s
  else
    let rec go n =
      if n = 0 then ""
      else
        let t = String.sub s 0 n ^ "…" in
        if text_w ?font size t <= w then t else go (utf8_floor s (n - 1))
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

let fill ?(round = 0.12) r c = R.draw_rectangle_rounded (screen r) round 6 (alpha_color c)

let outline ?(round = 0.12) ?(thick = 1.5) r c =
  R.draw_rectangle_rounded_lines_ex (screen r) round 6 (thick *. !scale) (alpha_color c)

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

(* Scala ScreenResources.playSound: clip volume scaled by the persisted sound
   level. The prefs file is tiny; read it per play like Storage does. *)
let sound_level () = try Prefs.load () |> Prefs.sound_level with _ -> 1.

let play_sound ?(volume = 0.7) name =
  Option.iter
    (fun s ->
      R.set_sound_volume s (volume *. sound_level ());
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
  R.draw_texture_pro tex src (screen r) (R.Vector2.create 0. 0.) 0. (alpha_color tint)

(* [r] filled, texture stretched (frames keep native aspect because [r] does) *)
let image_at ?(tint = R.Color.white) tex r =
  let src = R.Rectangle.create 0. 0. (float (R.Texture.width tex)) (float (R.Texture.height tex)) in
  R.draw_texture_pro tex src (screen r) (R.Vector2.create 0. 0.) 0. (alpha_color tint)

(* Backdrop filling the whole window without stretching. Crop to the window
   aspect so letterbox bands are covered and the art keeps its proportions. *)
let cover tex =
  let sw = float (R.get_screen_width ()) and sh = float (R.get_screen_height ()) in
  let tw = float (R.Texture.width tex) and th = float (R.Texture.height tex) in
  let ra = sw /. sh in
  let src_w, src_h = if tw /. th > ra then (th *. ra, th) else (tw, tw /. ra) in
  let src = R.Rectangle.create ((tw -. src_w) /. 2.) ((th -. src_h) /. 2.) src_w src_h in
  R.draw_texture_pro tex src (R.Rectangle.create 0. 0. sw sh) (R.Vector2.create 0. 0.) 0.
    (alpha_color R.Color.white)

(* Scala combat/slot is 112×140; the gold well is 81×95 at (15, 24). Crop to
   that well. A 90×102 override is the well already cropped. *)
let slot_well_w = 81.
let slot_well_h = 95.

let slot_src tex =
  let tw = float (R.Texture.width tex) and th = float (R.Texture.height tex) in
  if int_of_float tw = 112 && int_of_float th = 140 then R.Rectangle.create 15. 24. slot_well_w slot_well_h
  else R.Rectangle.create 0. 0. tw th

let slot_well_rect r =
  rect (r.x +. ((r.w -. slot_well_w) /. 2.)) (r.y +. ((r.h -. slot_well_h) /. 2.)) slot_well_w slot_well_h

let image_slot ?(tint = R.Color.white) tex r =
  R.draw_texture_pro tex (slot_src tex) (screen r) (R.Vector2.create 0. 0.) 0. (alpha_color tint)

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
let smoke_hide () = Fx.Hover.hide ()

(* Card group origin is the top-left; Scala places the emitter at +40,+50 in Y-up. *)
let smoke_show (r : rect) = Fx.Hover.show ~x:(r.x +. 40.) ~y:(r.y +. r.h -. 50.)

(* ---------------------------------------------------------------- widgets *)

(* Clicks are collected while drawing and run after the frame *)
let pending : (unit -> unit) option ref = ref None
let hovering = ref false
let clicked r = inside r && R.is_mouse_button_pressed R.MouseButton.Left

let on_click r f =
  if inside r then hovering := true;
  if clicked r then pending := Some f

let click_full f =
  let m = R.get_mouse_position () in
  let sw = float (R.get_screen_width ()) and sh = float (R.get_screen_height ()) in
  let x = R.Vector2.x m and y = R.Vector2.y m in
  if x >= 0. && x < sw && y >= 0. && y < sh then (
    hovering := true;
    if R.is_mouse_button_pressed R.MouseButton.Left then pending := Some f)

let button ?(selected = false) ?(enabled = true) ?(size = 17.) r label f =
  let hot = enabled && inside r in
  fill r (if selected then rgba 40 98 140 240 else if hot then panel_hi else panel);
  outline r (if selected then accent else if hot then muted else line);
  text_c ~font:bold ~size ~color:(if enabled then fg else faint) r (r.y +. ((r.h -. size) /. 2.) -. 2.) label;
  if enabled then on_click r f

(* Soft drop shadow around a parchment sheet. The map stays undimmed;
   stacked low-alpha rings read as a small blur, not a hard outline. *)
let paper_shadow r =
  let n = 9 in
  let step = 2.2 in
  for i = n downto 1 do
    let pad = float i *. step in
    let dy = float i *. 0.5 in
    fill ~round:0.2
      (rect (r.x -. pad) (r.y -. pad +. dy) (r.w +. (2. *. pad)) (r.h +. (2. *. pad)))
      (rgba 22 12 6 6)
  done

(* Paper scroll dialog box, like the Scala dialogs: parchment.png is a
   nine-patch with a 48px deckled edge, stretched middle. *)
let paper r =
  paper_shadow r;
  match texture (Assets.ui "parchment") with
  | None -> fill r panel
  | Some t ->
      let b = 32. in
      let xs = [| r.x; r.x +. b; r.x +. r.w -. b |] and ws = [| b; r.w -. (2. *. b); b |] in
      let ys = [| r.y; r.y +. b; r.y +. r.h -. b |] and hs = [| b; r.h -. (2. *. b); b |] in
      let t0 = [| 0.; 48.; 144. |] and tw = [| 48.; 96.; 48. |] in
      for i = 0 to 2 do
        for j = 0 to 2 do
          let src = R.Rectangle.create t0.(i) t0.(j) tw.(i) tw.(j) in
          let dst = screen { x = xs.(i); y = ys.(j); w = ws.(i); h = hs.(j) } in
          R.draw_texture_pro t src dst (R.Vector2.create 0. 0.) 0. (alpha_color R.Color.white)
        done
      done

(* Scala parchment TextButton: amber fill + IM Fell ink. *)
let pbutton ?(selected = false) ?(enabled = true) ?(size = 16.) r label f =
  let hot = enabled && inside r in
  if hot then hovering := true;
  let fill_c =
    if not enabled then rgba 184 132 55 140
    else if selected then rgba 168 112 38 255
    else if hot then rgba 199 143 61 255
    else rgba 184 132 55 255
  in
  fill ~round:0.12 r fill_c;
  outline ~round:0.12 ~thick:1.2 r (if hot || selected then rgba 92 54 18 255 else rgba 110 70 24 230);
  text_c ~font:paper_font ~size
    ~color:(if enabled then ink else ink_soft)
    r
    (r.y +. ((r.h -. size) /. 2.))
    label;
  if enabled then on_click r f

let btn_h = 28.
let paper_btn_w ?(size = 16.) label = Float.max 28. (text_w ~font:paper_font size label +. 16.)

(* Notebook tabs on parchment: labels on a rule, not amber buttons. The
   active tab is ink plus a short underline that sits on the strip. *)
let ptabs ~x ~y ~w ~selected keys set =
  let size = 18. and h = 30. and pad = 14. and gap = 8. in
  let items =
    List.mapi
      (fun k key ->
        let label = Assets.text key in
        (k, label, text_w ~font:paper_font size label +. (pad *. 2.)))
      keys
  in
  fill ~round:0. (rect x (y +. h -. 1.) w 1.) (rgba 122 90 40 130);
  let cx = ref x in
  List.iter
    (fun (k, label, tw) ->
      let r = rect !cx y tw h in
      let on = k = selected in
      let hot = inside r in
      if on then
        fill ~round:0.6
          (rect (r.x +. 8.) (r.y +. r.h -. 4.) (r.w -. 16.) 3.)
          (rgba 122 90 40 255);
      text_c ~font:paper_font ~size ~color:(if on || hot then ink else ink_soft) r
        (r.y +. ((h -. size) /. 2.))
        label;
      on_click r (fun () -> set k);
      cx := !cx +. tw +. gap)
    items

let paper_btn_row ?(size = 16.) ?(h = btn_h) ?(gap = 8.) ~cx ~y items =
  let widths = List.map (fun (label, _) -> paper_btn_w ~size label) items in
  let total = List.fold_left ( +. ) 0. widths +. (float (max 0 (List.length items - 1)) *. gap) in
  let x = ref (cx -. (total /. 2.)) in
  List.iter2
    (fun (label, f) w ->
      pbutton ~size (rect !x y w h) label f;
      x := !x +. w +. gap)
    items widths

let debug_mode () = try Prefs.debug_mode (Prefs.load ()) with _ -> false

(* Parchment checkbox, like Scala's CheckBox("parchment"): a square plus the
   label, not a selected button. *)
let checkbox ?(size = 18.) ~checked ~x ~y ~max_w label f =
  let box_s = 24. and gap = 10. in
  let text_x = x +. box_s +. gap in
  let text_max = Float.max 40. (max_w -. box_s -. gap) in
  let lines = wrap ~font:paper_font size text_max label in
  let line_h = size +. 4. in
  let h = Float.max box_s (float (max 1 (List.length lines)) *. line_h) in
  let hit = rect x y max_w h in
  let box = rect x (y +. ((h -. box_s) /. 2.)) box_s box_s in
  let hot = inside hit in
  if hot then hovering := true;
  fill ~round:0.16 box (if checked then rgba 168 112 38 255 else rgba 255 252 242 230);
  outline ~round:0.16
    ~thick:(if checked || hot then 2. else 1.5)
    box
    (if checked || hot then rgba 92 54 18 255 else paper_line);
  if checked then (
    let thick = 2.6 *. !scale in
    let c = alpha_color (rgba 255 244 214 255) in
    R.draw_line_ex
      (pos (box.x +. (box.w *. 0.22)) (box.y +. (box.h *. 0.52)))
      (pos (box.x +. (box.w *. 0.42)) (box.y +. (box.h *. 0.74)))
      thick c;
    R.draw_line_ex
      (pos (box.x +. (box.w *. 0.42)) (box.y +. (box.h *. 0.74)))
      (pos (box.x +. (box.w *. 0.78)) (box.y +. (box.h *. 0.26)))
      thick c);
  List.iteri
    (fun i l -> text ~font:paper_font ~size ~color:ink text_x (y +. (float i *. line_h) +. 1.) l)
    lines;
  on_click hit f

let close_dialog _id k = k ()
let drop_dialog () = ()

let draw_dialog_dim () =
  let sw = R.get_screen_width () and sh = R.get_screen_height () in
  R.draw_rectangle 0 0 sw sh (rgba 0 0 0 (int_of_float ((255. *. 0.45) +. 0.5)))

let run_dialog _id shown draw =
  if shown then (
    draw_dialog_dim ();
    draw ())

(* ------------------------------------------------------------- portraits *)

let portraits_for = function Table.Human -> Table.player_portraits | Table.Bot id -> Table.faces_for_bot id
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
let budget_options = [ 100; 250; 500; 1000; 2000; 5000 ]
let languages = [ "en"; "fr"; "es"; "ru"; "vi" ]

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
  mutable lang : string;
  mutable debug : bool;
  mutable tab : int;
  mutable class_scroll : float;
  mutable class_drag : bool;
}

type lobby_ui = {
  duel : duel_ui;
  gs : game_settings;
  mutable menu : int;
  mutable show_duel : bool;
  mutable show_settings : bool;
  mutable show_campaign_intro : bool;
  mutable show_campaign : bool;
  mutable message : string;
}

let seat_of_string ~fallback s =
  match Prefs.parse_seat ~allowed:Tournament.bot_ids ~fallback s with
  | `Human -> Table.Human
  | `Bot id -> Table.Bot id

let valid_specials names =
  List.filter_map (fun s -> try Some (Catalog.find_special s).h_name with Failure _ -> None) names

let game_settings_from_prefs () =
  let prefs = Prefs.load () in
  let d = Table.default_setup in
  let sp0, sp1 = Prefs.setup_specials prefs in
  let _ds0, _ds1 = d.specials in
  {
    specials = (valid_specials sp0, valid_specials sp1);
    budget_ms = Prefs.setup_budget prefs;
    seed_text = "";
    seed_focus = false;
    sound = Prefs.sound_level prefs;
    lang = Prefs.language prefs;
    debug = Prefs.debug_mode prefs;
    tab = 0;
    class_scroll = 0.;
    class_drag = false;
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
  let as_bot seat = match seat with Table.Human -> Table.Bot "oracle" | Table.Bot _ as b -> b in
  let seat1, portrait1 =
    match Table.bot_of_portrait (Prefs.ai_opponent prefs) with
    | Some b when List.mem b Tournament.bot_ids ->
        let seat = Table.Bot b in
        (seat, clamp_portrait seat (Prefs.ai_opponent prefs))
    | _ ->
        let seat = as_bot (seat_of_string ~fallback:fb1 s1) in
        (seat, clamp_portrait seat p1)
  in
  { sides = ({ seat = seat0; portrait = clamp_portrait seat0 p0 }, { seat = seat1; portrait = portrait1 }) }

let new_lobby () =
  {
    duel = duel_from_prefs ();
    gs = game_settings_from_prefs ();
    menu = 1;
    show_duel = false;
    show_settings = false;
    show_campaign_intro = false;
    show_campaign = false;
    message = "";
  }

let persist_duel lobby =
  try
    let s0, s1 = lobby.duel.sides in
    let p = Prefs.load () in
    Prefs.set_setup_seats p (Table.seat_name s0.seat, Table.seat_name s1.seat);
    Prefs.set_setup_portraits p (s0.portrait, s1.portrait);
    match s1.seat with Table.Bot _ -> Prefs.set_ai_opponent p s1.portrait | Table.Human -> ()
  with _ -> ()

(* Split a single-word house name near the middle so a larger label still fits. *)
let house_label_lines ?font size w s =
  if text_w ?font size s <= w then [ s ]
  else
    let len = String.length s in
    let rec from n =
      let n = utf8_floor s n in
      if n < 1 then None
      else
        let a = String.sub s 0 n and b = String.sub s n (len - n) in
        if text_w ?font size a <= w && text_w ?font size b <= w then Some [ a; b ]
        else from (n - 1)
    in
    match from ((len + 1) / 2) with
    | Some lines -> lines
    | None ->
        let rec longest n =
          let n = utf8_floor s n in
          if n <= 1 then n
          else if text_w ?font size (String.sub s 0 n) <= w then n
          else longest (n - 1)
        in
        let n = longest len in
        [ String.sub s 0 n; fit ?font size w (String.sub s n (len - n)) ]

(* One special-house card that keeps its background picture. The name bar
   covers the lower part of the art so the label can stay readable. *)
let draw_special_card ?(name_size = 13.) ?(bar_h = 32.) ?clip r (h : house) selected set =
  let hit =
    match clip with
    | None -> inside r
    | Some view -> ( match overlap r view with Some c -> inside c | None -> false)
  in
  let pic = rect r.x r.y r.w (Float.max 8. (r.h -. bar_h)) in
  (match texture (Assets.background h) with
  | Some t -> image ~tint:(if selected then R.Color.white else rgba 150 150 150 255) t pic
  | None -> fill pic panel);
  let bar = rect r.x (r.y +. r.h -. bar_h) r.w bar_h in
  R.draw_rectangle_rec (screen bar) (alpha_color shade);
  let lines = house_label_lines ~font:paper_font name_size (r.w -. 4.) (cap h.h_name) in
  let line_h = name_size +. 1. in
  let block = (float (List.length lines) *. line_h) -. 1. in
  let y0 = bar.y +. ((bar.h -. block) /. 2.) in
  List.iteri (fun i s -> text_c ~font:paper_font ~size:name_size r (y0 +. (float i *. line_h)) s) lines;
  outline ~round:0.06
    ~thick:(if selected then 3. else 1.5)
    r
    (if selected then accent else if hit then ink_soft else paper_line);
  match clip with
  | None -> on_click r set
  | Some view -> Option.iter (fun c -> on_click c set) (overlap r view)

let lobby_menu = [ "campaign"; "duel"; "settings" ]

let campaign_is_fresh () =
  let p = Prefs.load () in
  Prefs.campaign_completed p = [] && Prefs.campaign_position p = Campaign.day_start_id

let open_campaign lobby =
  if campaign_is_fresh () then lobby.show_campaign_intro <- true else lobby.show_campaign <- true

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
    draw_portrait_chooser ~x ~y:!y ~size:120. ~col_w ~sub ~name_size:18. ~sub_size:16. sd.portrait ids on_pick
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
   screen. House choices keep their background pictures. *)
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

(* Classes tab: two checked-set columns sharing one scrollbar, like the
   Scala lists. Three pictured cards per row so the house names stay readable. *)
let class_cols = 3
let class_x0 = 100.
let class_col_w = 304.
let class_gap = 8.
let class_card_w = (class_col_w -. (float (class_cols - 1) *. class_gap)) /. float class_cols
let class_card_h = 118.
let class_row_h = 126.
let class_head_h = 26.
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
        acc +. class_head_h +. (float (class_rows (List.length houses)) *. class_row_h) -. 8. +. 14.)
      0. Table.house_groups
  in
  Float.max 0. (raw -. 14.)

let class_maxscroll () = Float.max 0. (class_content_h () -. (class_y1 -. class_y0))

let class_scroll_by gs w =
  gs.class_scroll <- Float.min (class_maxscroll ()) (Float.max 0. (gs.class_scroll -. (w *. 48.)))

let draw_class_col gs which x0 =
  let set = if which = 0 then fst gs.specials else snd gs.specials in
  let sel h = List.exists (fun n -> Catalog.token n = Catalog.token h.h_name) set in
  let y = ref (class_y0 -. gs.class_scroll) in
  let seen (r : rect) = r.y +. r.h > class_y0 && r.y < class_y1 in
  List.iter
    (fun (author, houses) ->
      let hr = rect x0 !y class_col_w class_head_h in
      if seen hr then (
        let hit = match overlap hr class_view with Some c -> inside c | None -> false in
        text ~font:paper_font ~size:18. ~color:(if hit then ink else ink_soft) (x0 +. 4.) (!y +. 3.) author;
        Option.iter (fun c -> on_click c (fun () -> toggle_group gs which houses)) (overlap hr class_view));
      y := !y +. class_head_h;
      List.iteri
        (fun k (h : house) ->
          let r =
            rect
              (x0 +. (float (k mod class_cols) *. (class_card_w +. class_gap)))
              (!y +. (float (k / class_cols) *. class_row_h))
              class_card_w class_card_h
          in
          if seen r then
            draw_special_card ~name_size:18. ~bar_h:42. ~clip:class_view r h (sel h) (fun () ->
                toggle_special gs which h.h_name))
        houses;
      y := !y +. (float (class_rows (List.length houses)) *. class_row_h) -. 8.;
      let sr = rect x0 (!y +. 3.) class_col_w 2. in
      if seen sr then fill sr (rgba 140 107 41 180);
      y := !y +. 14.)
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
    [ "settings.classes"; "Game"; "settings.sound"; "settings.lang" ]
    (fun k -> gs.tab <- k);
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
        if R.is_mouse_button_down R.MouseButton.Left then (
          if gs.class_drag || inside thumb then (
            gs.class_drag <- true;
            hovering := true;
            let _, my = mouse () in
            let f = (my -. class_y0 -. (th /. 2.)) /. (view_h -. th) in
            gs.class_scroll <- Float.min max (Float.max 0. (f *. max))))
        else gs.class_drag <- false)
      else gs.class_drag <- false
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
          play_sound "move")
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

(* ----------------------------------------------------------- campaign *)

type campaign_dlg =
  | Camp_none
  | Camp_fight of Campaign.node
  | Camp_meeting of Campaign.node
  | Camp_restart
  | Camp_note of { title : string; body : string; ack : unit -> unit }
  | Camp_end of { node : Campaign.node; won : bool }

type campaign_ui = {
  mutable dlg : campaign_dlg;
  mutable night : float;
  mutable night_target : float;
  mutable pending_fight : Campaign.node option;
  mutable pending_epilogue : bool;
  mutable pending_lobby : bool;
}

let map_x x = x *. vw
let map_y y = (1. -. y) *. vh
let hit_r = 17.

let circle_at x y radius color =
  let p = pos x y in
  R.draw_circle
    (int_of_float (R.Vector2.x p +. 0.5))
    (int_of_float (R.Vector2.y p +. 0.5))
    (radius *. !scale) color

let bez a c b t =
  let u = 1. -. t in
  (u *. u *. a) +. (2. *. u *. t *. c) +. (t *. t *. b)

let draw_curve (from : Campaign.node) (toward : Campaign.node) width color =
  let x0 = map_x from.x and y0 = map_y from.y in
  let x1 = map_x toward.x and y1 = map_y toward.y in
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let len = Float.max 16. (sqrt ((dx *. dx) +. (dy *. dy))) in
  let sign = if (Hashtbl.hash from.id + Hashtbl.hash toward.id) mod 2 = 0 then 1. else -1. in
  let bulge = Float.min 58. (len *. 0.24) *. sign in
  let cx = ((x0 +. x1) *. 0.5) -. (dy /. len *. bulge) in
  let cy = ((y0 +. y1) *. 0.5) +. (dx /. len *. bulge) in
  let steps = 22 in
  for i = 0 to steps - 1 do
    let t0 = float i /. float steps and t1 = float (i + 1) /. float steps in
    R.draw_line_ex
      (pos (bez x0 cx x1 t0) (bez y0 cy y1 t0))
      (pos (bez x0 cx x1 t1) (bez y0 cy y1 t1))
      (width *. !scale) color
  done

let campaign_progress () =
  let p = Prefs.load () in
  (Prefs.campaign_completed p, Prefs.campaign_position p)

let mage_line (n : Campaign.node) =
  match n.fight with
  | None -> Assets.text ("campaign." ^ n.id ^ ".name")
  | Some f ->
      let mage =
        Assets.format (Assets.text "campaign.mage")
          [ Assets.text f.house_key; Assets.text ("campaign.role." ^ f.bot_id) ]
      in
      let life = match f.life with Some n -> n | None -> 60 in
      Assets.text ("campaign." ^ n.id ^ ".name")
      ^ " (" ^ mage ^ ")   "
      ^ Assets.format (Assets.text "campaign.life") [ string_of_int life ]

let end_line ~node_id ~bot_id ~won =
  Assets.text (Campaign.end_message_key ~node_id ~bot_id ~won ~has_key:Assets.has_key)

let parchment_card ~title ~portrait ~subtitle ~message buttons =
  click_full (fun () -> ());
  let msg_w = 400. in
  let face = 112. in
  let type_size = 20. in
  let line_h = 26. in
  let lines = wrap ~font:paper_font type_size msg_w message in
  let nlines = min 8 (List.length lines) in
  let edge = 36. in
  let title_h = 32. in
  let sub_h = if subtitle = "" then 0. else 28. in
  let msg_h = float nlines *. line_h in
  let box_w = 520. in
  let box_h = edge +. title_h +. 8. +. face +. 12. +. sub_h +. 8. +. msg_h +. 16. +. btn_h +. edge in
  let box = rect ((vw -. box_w) /. 2.) ((vh -. box_h) /. 2.) box_w box_h in
  paper box;
  on_click box (fun () -> ());
  text ~font:paper_font ~size:22. ~color:ink (box.x +. 40.) (box.y +. 22.) title;
  draw_face portrait (rect (box.x +. ((box.w -. face) /. 2.)) (box.y +. edge +. title_h) face face);
  let y = ref (box.y +. edge +. title_h +. face +. 12.) in
  if subtitle <> "" then (
    text_c ~font:paper_font ~size:type_size ~color:ink box !y subtitle;
    y := !y +. sub_h);
  y := !y +. 8.;
  List.iteri
    (fun i l ->
      if i < nlines then
        text_c ~font:paper_font ~size:type_size ~color:ink (rect box.x !y box.w 0.)
          (!y +. (float i *. line_h))
          l)
    lines;
  paper_btn_row ~size:16. ~cx:(box.x +. (box.w /. 2.)) ~y:(box.y +. box.h -. edge -. btn_h) buttons

let draw_campaign_end ~node ~won ~replay ~map =
  match node.Campaign.fight with
  | None -> ()
  | Some f ->
      let buttons =
        (if won then [] else [ (Assets.text "campaign.end.replay", replay) ])
        @ [ (Assets.text "campaign.end.map", map) ]
      in
      parchment_card
        ~title:(Assets.text (if won then "campaign.end.win.title" else "campaign.end.lose.title"))
        ~portrait:f.portrait_id ~subtitle:(mage_line node)
        ~message:(end_line ~node_id:node.id ~bot_id:f.bot_id ~won)
        buttons

let dlg_open = function Camp_none -> false | _ -> true

let maybe_notes (camp : campaign_ui) =
  if dlg_open camp.dlg then ()
  else
    try
      let p = Prefs.load () in
      let completed = Prefs.campaign_completed p in
      let chapter = Campaign.chapter_of completed in
      if
        chapter = Campaign.Night
        && Prefs.campaign_position p = Campaign.night_start_id
        && not (Prefs.campaign_night_rest p)
      then
        camp.dlg <-
          Camp_note
            {
              title = Assets.text "campaign.night.rest.title";
              body = Assets.text "campaign.night.rest";
              ack = (fun () -> Prefs.mark_night_rested (Prefs.load ()));
            }
      else
        match List.assoc_opt "antimancer" Campaign.isolate_path with
        | Some (only :: _)
          when List.mem "antimancer" completed
               && (not (List.mem only completed))
               && not (Prefs.campaign_wake_far p) ->
            camp.dlg <-
              Camp_note
                {
                  title = Assets.text "campaign.antimancer.wake.title";
                  body = Assets.text "campaign.antimancer.wake";
                  ack = (fun () -> Prefs.mark_wake_far (Prefs.load ()));
                }
        | _ -> ()
    with _ -> ()

let new_campaign ?(from_game = false) () =
  let completed, _ = campaign_progress () in
  let night = if Campaign.chapter_of completed = Campaign.Night then 1. else 0. in
  let camp =
    {
      dlg = Camp_none;
      night = (if from_game then night else if night = 1. then 0. else 0.);
      night_target = night;
      pending_fight = None;
      pending_epilogue = false;
      pending_lobby = false;
    }
  in
  maybe_notes camp;
  camp

let draw_campaign_intro lobby ~start =
  click_full (fun () -> close_dialog "campaign-intro" (fun () -> lobby.show_campaign_intro <- false));
  let box = rect 156. 80. 520. 560. in
  paper box;
  on_click box (fun () -> ());
  text_c ~font:paper_font ~size:22. ~color:ink box (box.y +. 16.) (Assets.text "campaign.title");
  let p = Prefs.load () in
  let id = Prefs.player_portrait p in
  draw_portrait_chooser ~x:box.x ~y:(box.y +. 56.) ~size:96. ~col_w:box.w id Table.player_portraits (fun v ->
      Prefs.set_player_portrait (Prefs.load ()) v);
  let lines = wrap ~font:paper_font 20. 440. (Assets.text "campaign.intro") in
  List.iteri
    (fun i l ->
      if i < 8 then text_c ~font:paper_font ~size:20. ~color:ink box (box.y +. 214. +. (float i *. 26.)) l)
    lines;
  paper_btn_row ~size:16.
    ~cx:(box.x +. (box.w /. 2.))
    ~y:(box.y +. box.h -. 52.)
    [
      ( Assets.text "button.startcampaign",
        fun () ->
          drop_dialog ();
          lobby.show_campaign_intro <- false;
          start () );
      ( Assets.text "button.cancel",
        fun () -> close_dialog "campaign-intro" (fun () -> lobby.show_campaign_intro <- false) );
    ]

let draw_campaign (camp : campaign_ui) =
  camp.night <- camp.night +. ((camp.night_target -. camp.night) *. Float.min 1. (R.get_frame_time () *. 1.6));
  if abs_float (camp.night -. camp.night_target) < 0.002 then camp.night <- camp.night_target;
  let completed, here = campaign_progress () in
  let chapter = Campaign.chapter_of completed in
  let open_ids = Campaign.available completed in
  let pulse = 8.4 +. (sin (R.get_time () *. 4.) *. 1.6) in
  (match texture (Assets.campaign "map") with
  | Some t -> Fx.with_night camp.night (fun () -> cover t)
  | None -> fill (rect 0. 0. vw vh) bg);
  List.iter
    (fun (e : Campaign.edge) ->
      match (Campaign.by_id e.from, Campaign.by_id e.toward) with
      | Some a, Some b when a.chapter = chapter && b.chapter = chapter ->
          let via_ok = match e.via with None -> true | Some v -> List.mem v completed in
          let lit = ((not (Campaign.is_playable a)) || List.mem a.id completed) && via_ok in
          draw_curve a b (if lit then 3.1 else 2.2) (if lit then rgba 237 199 82 217 else rgba 46 41 31 115)
      | _ -> ())
    Campaign.edges;
  List.iter
    (fun (n : Campaign.node) ->
      if n.chapter = chapter then (
        let x = map_x n.x and y = map_y n.y in
        if not (Campaign.is_playable n) then circle_at x y 5. (rgba 242 230 178 140)
        else if List.mem n.id completed then (
          circle_at x y 7.5 (rgba 107 82 36 230);
          circle_at x y 4.2 (rgba 209 173 77 242))
        else if List.mem n.id open_ids then (
          circle_at x y (pulse +. 6.) (rgba 242 209 71 71);
          circle_at x y pulse (rgba 250 219 87 242);
          circle_at x y 3.4 (rgba 51 31 10 230))
        else circle_at x y 5.5 (rgba 31 28 23 140);
        if n.id = here then (
          circle_at x y 12.5 (rgba 242 242 255 217);
          circle_at x y 9.5 (rgba 38 46 71 89))))
    Campaign.nodes;
  (match Campaign.by_id here with
  | None -> ()
  | Some n ->
      let face = rect (map_x n.x -. 14.) (map_y n.y -. 24.) 28. 28. in
      draw_face (Prefs.player_portrait (Prefs.load ())) face);
  List.iter
    (fun (n : Campaign.node) ->
      if n.chapter = chapter && Campaign.is_playable n && List.mem n.id open_ids then
        let r = rect (map_x n.x -. hit_r) (map_y n.y -. hit_r) (hit_r *. 2.) (hit_r *. 2.) in
        on_click r (fun () -> camp.dlg <- (if Campaign.is_fight n then Camp_fight n else Camp_meeting n)))
    Campaign.nodes;
  let finished = Campaign.is_finished completed in
  let back = Assets.text "button.back" in
  pbutton ~size:16.
    (rect 16. 12. (paper_btn_w ~size:16. back) 26.)
    back
    (fun () -> camp.pending_lobby <- true);
  let restart = Assets.text "campaign.restart" in
  let rw = paper_btn_w ~size:16. restart in
  pbutton ~size:16. (rect (vw -. 16. -. rw) 12. rw 26.) restart (fun () -> camp.dlg <- Camp_restart);
  text_c ~font:bold ~size:22. ~color:fg (rect 0. 0. vw 0.) 16.
    (Assets.text (if finished then "campaign.complete" else "campaign.title"));
  text_c ~size:14. ~color:muted (rect 0. 0. vw 0.) 44.
    (Assets.text (if chapter = Campaign.Night then "campaign.night" else "campaign.day"));
  (if finished then
     let epi = Assets.text "campaign.epilogue" in
     let ew = paper_btn_w ~size:16. epi in
     pbutton ~size:16.
       (rect ((vw -. ew) /. 2.) (vh -. 48.) ew 28.)
       epi
       (fun () -> camp.pending_epilogue <- true));
  run_dialog "campaign" (camp.dlg <> Camp_none) (fun () ->
      match camp.dlg with
      | Camp_none -> ()
      | Camp_note n ->
          parchment_card ~title:n.title
            ~portrait:(Prefs.player_portrait (Prefs.load ()))
            ~subtitle:"" ~message:n.body
            [
              ( Assets.text "campaign.continue",
                fun () ->
                  n.ack ();
                  camp.dlg <- Camp_none );
            ]
      | Camp_restart ->
          let night = chapter = Campaign.Night in
          let buttons =
            (if night then
               [
                 ( Assets.text "campaign.restart.all",
                   fun () ->
                     Prefs.reset_campaign (Prefs.load ());
                     camp.dlg <- Camp_none;
                     camp.night_target <- 0. );
                 ( Assets.text "campaign.restart.night",
                   fun () ->
                     Prefs.reset_night_campaign (Prefs.load ());
                     camp.dlg <- Camp_none;
                     camp.night_target <- 1. );
               ]
             else
               [
                 ( Assets.text "button.yes",
                   fun () ->
                     Prefs.reset_campaign (Prefs.load ());
                     camp.dlg <- Camp_none;
                     camp.night_target <- 0. );
               ])
            @ [ (Assets.text "button.cancel", fun () -> camp.dlg <- Camp_none) ]
          in
          parchment_card ~title:(Assets.text "campaign.restart")
            ~portrait:(Prefs.player_portrait (Prefs.load ()))
            ~subtitle:""
            ~message:(Assets.text (if night then "campaign.restart.choice" else "campaign.restart.confirm"))
            buttons
      | Camp_fight n -> (
          match n.fight with
          | None -> camp.dlg <- Camp_none
          | Some f ->
              parchment_card
                ~title:(Assets.text ("campaign." ^ n.id ^ ".name"))
                ~portrait:f.portrait_id ~subtitle:(mage_line n)
                ~message:(Assets.text ("campaign." ^ n.id ^ ".message"))
                (( Assets.text "button.fight",
                   fun () ->
                     camp.dlg <- Camp_none;
                     camp.pending_fight <- Some n )
                 ::
                 (if debug_mode () then
                    [
                      ( Assets.text "button.debugwin",
                        fun () ->
                          Prefs.complete_campaign (Prefs.load ()) n.id;
                          camp.dlg <- Camp_none;
                          let completed = Prefs.campaign_completed (Prefs.load ()) in
                          if Campaign.is_finished completed then camp.pending_epilogue <- true
                          else if Campaign.chapter_of completed = Campaign.Night then camp.night_target <- 1.
                      );
                    ]
                  else [])
                @ [ (Assets.text "button.cancel", fun () -> camp.dlg <- Camp_none) ]))
      | Camp_meeting n -> (
          match n.meeting with
          | None -> camp.dlg <- Camp_none
          | Some m ->
              let choices =
                match m.choices with
                | [] ->
                    [
                      ( Assets.text "campaign.continue",
                        fun () ->
                          Prefs.complete_campaign (Prefs.load ()) n.id;
                          camp.dlg <- Camp_none );
                    ]
                | cs ->
                    List.map
                      (fun (c : Campaign.choice) ->
                        ( Assets.text ("campaign." ^ n.id ^ ".choice." ^ c.id),
                          fun () ->
                            Prefs.complete_campaign (Prefs.load ()) ~extras:[ c.id ] n.id;
                            camp.dlg <- Camp_none ))
                      cs
              in
              parchment_card
                ~title:(Assets.text ("campaign." ^ n.id ^ ".name"))
                ~portrait:m.portrait_id
                ~subtitle:(Assets.text ("campaign." ^ n.id ^ ".name"))
                ~message:(Assets.text ("campaign." ^ n.id ^ ".message"))
                choices)
      | Camp_end { node; won } ->
          draw_campaign_end ~node ~won
            ~replay:(fun () ->
              camp.dlg <- Camp_none;
              camp.pending_fight <- Some node)
            ~map:(fun () -> camp.dlg <- Camp_none))

let draw_epilogue ~menu =
  (match texture (Assets.campaign "epilogue") with Some t -> cover t | None -> fill (rect 0. 0. vw vh) bg);
  let box = rect 120. (vh -. 240.) 592. 208. in
  paper box;
  let lines = wrap ~font:paper_font 20. 520. (Assets.text "campaign.epilogue") in
  List.iteri
    (fun i l ->
      if i < 5 then text_c ~font:paper_font ~size:20. ~color:ink box (box.y +. 24. +. (float i *. 26.)) l)
    lines;
  paper_btn_row ~size:16.
    ~cx:(box.x +. (box.w /. 2.))
    ~y:(box.y +. box.h -. 48.)
    [ (Assets.text "campaign.epilogue.menu", menu) ]

(* --------------------------------------------------------------- game ui *)

type thinking = { domain : unit Domain.t; result : (command option, exn) result option Atomic.t }
type mode = Browse | Target of command list
type last_played = { player : player_id; card : card option }
type hover = Hover_card of card | Hover_house of house
type house_dlg = { house : house; mutable scroll : float; mutable drag : bool; mutable content_h : float }

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
  mutable settings : game_settings option;
  mutable budget_ms : int;
  mutable message : string;
  mutable thinking : thinking option;
  mutable last_move : float;
  mutable hover : hover option;
  mutable hover_opp : bool;
  mutable house_dlg : house_dlg option;
  mutable scroll_y : float;
  mutable bg_at : float;
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
  mutable campaign : Campaign.node option;
  mutable campaign_recorded : bool;
  mutable ended_at : float option;
  (* Wall time when YOU WON / YOU LOSE may appear: after attacks and life pops. *)
  mutable over_at : float option;
  (* Some won: end parchment is up on the board, like Scala CampaignEndDialog. *)
  mutable campaign_end : bool option;
}

(* Filled once the board layout helpers exist, so submit can queue slides. *)
let board_event : (game_ui -> game_event -> unit) ref = ref (fun _ _ -> ())

let new_game ?(campaign = None) setup portraits =
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
      settings = None;
      budget_ms = setup.budget_ms;
      message = "";
      thinking = None;
      last_move = R.get_time ();
      hover = None;
      hover_opp = false;
      house_dlg = None;
      scroll_y = 0.;
      bg_at = R.get_time ();
      slides = [];
      anim_at = 0.;
      frames = [];
      shown = Session.peek (Table.session table);
      last_frame = Session.peek (Table.session table);
      attacks = [];
      focuses = [];
      pops = [];
      campaign;
      campaign_recorded = false;
      ended_at = None;
      over_at = None;
      campaign_end = None;
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

(* Scala raka is 90×102. Empty slots share that hit rect; the gold well is
   drawn at its native 81×95 so it sits inside the card, not around it. *)
let card_w = 90.
let card_h = 102.
let portrait_size = 84.
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

let fx_layout ui : Fx.layout =
  let occupied pid =
    let p = (view ui).players.(pid) in
    List.filter (fun n -> Option.is_some p.ps_slots.(n)) p.ps_slot_list
  in
  let scarecrow pid =
    let p = (view ui).players.(pid) in
    List.find_opt
      (fun n ->
        match p.ps_slots.(n) with Some st -> st.card.c_card.key = "disciples.scarecrow" | None -> false)
      p.ps_slot_list
  in
  let house pid i =
    let header_y = if pid = opponent then opp_header_y else hand_header_y in
    match List.find_index (fun hd -> hd.phd_house.h_index = i) (Table.hand ui.table pid) with
    | None -> { Fx.x = !lay.hand_x0; y = oy ui header_y }
    | Some col ->
        {
          Fx.x = !lay.hand_x0 +. (float col *. house_col) +. (house_col /. 2.);
          y = oy ui header_y +. (house_header_h /. 2.);
        }
  in
  {
    slot_center =
      (fun pid n ->
        let r = slot_rect ui pid n in
        { x = r.x +. (r.w /. 2.); y = r.y +. (r.h /. 2.) });
    slot_floor =
      (fun pid n ->
        let well = slot_well_rect (slot_rect ui pid n) in
        { x = well.x +. (well.w /. 2.); y = well.y +. well.h });
    flame_base =
      (fun pid n ->
        let r = slot_rect ui pid n in
        { x = r.x +. (r.w /. 2.); y = r.y +. r.h -. 38. });
    life = (fun pid -> { x = 8. +. (portrait_size /. 2.); y = oy ui (row_y pid) +. (portrait_size /. 2.) });
    house;
    occupied;
    scarecrow;
    source_of =
      (fun c ->
        find_hand_rect ui c.cmd_player c.cmd_card
        |> Option.map (fun r -> { Fx.x = r.x +. (r.w /. 2.); y = r.y +. (r.h /. 2.) }));
  }

let run_fx ui recipe =
  let start =
    if recipe.Fx.blocking then schedule ui recipe.duration else Float.max (R.get_time ()) ui.anim_at
  in
  Fx.spawn (recipe.make ~start)

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
        ~from_r ~to_r ~sound:(Some "move") card cost attack life max_life;
      (* Keep the landed creature on the board if a later blocking focus
         (Wolf Spider, Marine, …) delays the submit refresh. *)
      push_frame ui ~silent:true
  | Ev_move { num; dest; player; dest_player } ->
      Option.iter
        (fun (card, cost, attack, life, max_life) ->
          let start = schedule ui 1. in
          push_slide ui ~start ~duration:1.
            ~hides:[ (dest_player, dest) ]
            ~from_r:(slot_rect ui player num) ~to_r:(slot_rect ui dest_player dest) ~sound:(Some "move") card
            cost attack life max_life;
          push_frame ui ~silent:true)
        (face_of_slot ui player num)
  | Ev_swap { num; dest; player; dest_player } -> (
      match (face_of_slot ui player num, face_of_slot ui dest_player dest) with
      | Some (a, ac, aa, al, am), Some (b, bc, ba, bl, bm) ->
          let start = schedule ui 1. in
          let src = slot_rect ui player num and dst = slot_rect ui dest_player dest in
          let hides = [ (player, num); (dest_player, dest) ] in
          push_slide ui ~start ~duration:1. ~hides ~from_r:src ~to_r:dst ~sound:(Some "move") a ac aa al am;
          push_slide ui ~start ~duration:1. ~hides ~from_r:dst ~to_r:src ~sound:None b bc ba bl bm;
          push_frame ui ~silent:true
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
  | Ev_to_move _ -> push_frame ui ~silent:true
  | Ev_spell_played c -> Option.iter (run_fx ui) (Fx.for_spell (fx_layout ui) c)
  | Ev_ability { kind; player; num; target } ->
      Option.iter (run_fx ui) (Fx.for_ability (fx_layout ui) ~kind ~player ~num ~target)
  | Ev_game_over { winner; _ } ->
      push_frame ui ~silent:true;
      (* Engine already set Over; Scala waitAction holds endGame until the
         attack finishes. Hold the banner until that timeline and the life
         pops have played. *)
      let after_anims = schedule ui 0. in
      let after_pops =
        List.fold_left
          (fun acc p ->
            match p.at with
            | Player_life _ | Slot_life _ -> Float.max acc (p.p_start +. pop_duration)
            | Mana _ -> acc)
          after_anims ui.pops
      in
      if ui.anim_at < after_pops then ui.anim_at <- after_pops;
      ui.over_at <- Some after_pops;
      Fx.victory (fx_layout ui) ~winner ~you:(Table.seat ui.table owner = Human) ~start:after_pops
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
         | Some t -> image_slot t (slot_well_rect r)
         | None ->
             fill ~round:0.08 (slot_well_rect r) panel;
             outline ~round:0.08 (slot_well_rect r) line)
     | Some _ when slot_hidden ui pid num -> (
         match texture (Assets.slot_frame ()) with
         | Some t -> image_slot t (slot_well_rect r)
         | None ->
             fill ~round:0.08 (slot_well_rect r) panel;
             outline ~round:0.08 (slot_well_rect r) line)
     | Some st ->
         let c = st.card.c_card in
         if inside r then ui.hover <- Some (Hover_card c);
         let life_color = if st.life < st.max_life then gold else life_ok in
         let shake_x, shake_y = Fx.slot_offset pid num in
         let r = { r with x = r.x +. shake_x; y = r.y +. attack_offset ui pid num +. shake_y } in
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
  text ~font:bold ~size:22. ~color:fg tx (y +. 8.) (string_of_int p.ps_life)

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
let house_title (h : house) = if Assets.has_key h.h_key then Assets.text h.h_key else cap h.h_name

let house_blurb (h : house) =
  let key = h.h_key ^ ".description" in
  if Assets.has_key key then Assets.text key else ""

let open_house_dlg ui (h : house) =
  ui.house_dlg <- Some { house = h; scroll = 0.; drag = false; content_h = 0. }

let card_preview_bits (c : card) =
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
  (title, subtitle, desc_text)

let draw_house_header ui pid hd col header_y =
  let x = !lay.hand_x0 +. (float col *. house_col) in
  let y = oy ui header_y in
  let p = (view ui).players.(pid) in
  let i = hd.phd_house.h_index in
  let name = house_title hd.phd_house in
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
  let hot = inside plate in
  fill ~round:0.04 plate (rgba 10 8 5 148);
  let name_s = name ^ " " in
  let total = text_w size name_s +. text_w ~font:bold size mana in
  let tx = x +. ((house_col -. total) /. 2.) in
  let ty = y +. ((house_header_h -. size) /. 2.) -. 1. in
  let name_color = if hot then accent else fg in
  text ~size ~color:name_color tx ty name_s;
  text ~font:bold ~size (tx +. text_w size name_s) ty mana;
  if hot then (
    ui.hover <- Some (Hover_house hd.phd_house);
    if pid = opponent then ui.hover_opp <- true);
  on_click plate (fun () -> open_house_dlg ui hd.phd_house)

let draw_hand_at ui pid ~cards_y ~header_y =
  let reveal_all = reveal_hand ui pid in
  let can_choose = human_to_move ui = Some pid in
  (* In a game with a human, only the side to move looks active; a bot-only
     table keeps the informative tint on both sides. *)
  let human_game = Table.seat ui.table owner = Human || Table.seat ui.table opponent = Human in
  let turn_on = (not human_game) || Table.to_move ui.table = Some pid in
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
              let grey = reveal_all && not playable in
              if (not is_selected) && active && inside r then fill ~round:0.08 r (rgba 255 255 255 28);
              Fx.with_grey grey (fun () ->
                  draw_card_face (desc ui) r c ~cost:(Some cd.cd_cost) ~attack:(printed_attack c)
                    ~life:(printed_life c) ~life_color:life_ok ~tint:R.Color.white);
              (* Gray-out veil while another side is to move, over art and text alike. *)
              if not turn_on then fill ~round:0.08 r (rgba 168 168 178 125);
              if is_selected then Fx.draw_sel ~x:r.x ~y:r.y ~w:r.w ~h:r.h
              else if active && inside r then outline ~round:0.08 ~thick:2. r accent;
              if inside r then (
                ui.hover <- Some (Hover_card c);
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

let draw_card_art r (c : card) =
  match card_texture c with Some t -> image t r | None -> fill ~round:0.04 r panel_hi

let draw_preview ui =
  let x = !lay.preview_x and w = !lay.preview_w in
  let y = preview_top ui in
  let pad = 8. in
  match ui.hover with
  | None -> ()
  | Some (Hover_house h) ->
      let title_size = 20. and desc_size = 18. in
      let title_lh = 24. and desc_lh = 22. in
      let title = house_title h in
      let desc_text = house_blurb h in
      let title_lines =
        List.filteri (fun i _ -> i < 2) (wrap ~font:bold title_size (w -. (pad *. 2.)) title)
      in
      let desc_lines =
        if desc_text = "" then []
        else List.filteri (fun i _ -> i < 8) (wrap desc_size (w -. (pad *. 2.)) desc_text)
      in
      let desc_h = if desc_lines = [] then 0. else 8. +. (float (List.length desc_lines) *. desc_lh) in
      let box_h = pad +. (float (List.length title_lines) *. title_lh) +. desc_h +. pad in
      let box = rect x y w box_h in
      fill ~round:0.04 box (rgba 0 0 0 90);
      List.iteri
        (fun i l -> text ~font:bold ~size:title_size (x +. pad) (y +. pad +. (float i *. title_lh)) l)
        title_lines;
      List.iteri
        (fun i l ->
          text ~size:desc_size (x +. pad)
            (y +. pad +. (float (List.length title_lines) *. title_lh) +. 8. +. (float i *. desc_lh))
            l)
        desc_lines
  | Some (Hover_card c) ->
      let art_s = if c.is_spell then 130. else 120. in
      let title_size = 20. and sub_size = 16. and desc_size = 18. in
      let title_lh = 24. and desc_lh = 22. in
      let title, subtitle, desc_text = card_preview_bits c in
      let info_w = Float.max 80. (w -. art_s -. (pad *. 3.)) in
      let title_lines = List.filteri (fun i _ -> i < 2) (wrap ~font:bold title_size info_w title) in
      let desc_lines =
        if desc_text = "" then []
        else List.filteri (fun i _ -> i < 8) (wrap desc_size (w -. (pad *. 2.)) desc_text)
      in
      let info_h = (float (List.length title_lines) *. title_lh) +. 6. +. sub_size in
      let header_h = Float.max art_s info_h in
      let desc_h = if desc_lines = [] then 0. else 8. +. (float (List.length desc_lines) *. desc_lh) in
      let box_h = pad +. header_h +. desc_h +. pad in
      let box = rect x y w box_h in
      fill ~round:0.04 box (rgba 0 0 0 90);
      let art = rect (x +. pad) (y +. pad) art_s art_s in
      draw_card_art art c;
      let tx = x +. pad +. art_s +. pad in
      List.iteri
        (fun i l -> text ~font:bold ~size:title_size tx (y +. pad +. (float i *. title_lh)) l)
        title_lines;
      let sub_y = y +. pad +. (float (List.length title_lines) *. title_lh) +. 4. in
      text ~size:sub_size ~color:muted tx sub_y subtitle;
      List.iteri
        (fun i l -> text ~size:desc_size (x +. pad) (y +. pad +. header_h +. 8. +. (float i *. desc_lh)) l)
        desc_lines

let draw_last_played ui =
  match !(ui.last_played) with
  | None -> ()
  | Some lp -> (
      let r = last_played_rect ui in
      fill ~round:0.06 r (rgba 10 8 5 148);
      if inside r then hovering := true;
      match lp.card with
      | Some c ->
          if inside r then ui.hover <- Some (Hover_card c);
          let thumb = 40. in
          draw_card_art (rect (r.x +. 6.) (r.y +. ((r.h -. thumb) /. 2.)) thumb thumb) c;
          text ~size:14. ~color:fg (r.x +. 52.) (r.y +. ((r.h -. 14.) /. 2.)) "Last played"
      | None ->
          text ~size:14. ~color:muted (r.x +. 12.)
            (r.y +. ((r.h -. 14.) /. 2.))
            (Printf.sprintf "P%d passed" (lp.player + 1)))

(* Scala HouseDescription: parchment list of every card in the house. *)
let house_dlg_box = rect 56. 36. 720. 696.

let house_dlg_view =
  rect (house_dlg_box.x +. 20.) (house_dlg_box.y +. 56.) (house_dlg_box.w -. 52.) (house_dlg_box.h -. 116.)

let house_dlg_bar = rect (house_dlg_view.x +. house_dlg_view.w +. 4.) house_dlg_view.y 12. house_dlg_view.h

let house_scroll_by dlg wheel =
  let max = Float.max 0. (dlg.content_h -. house_dlg_view.h) in
  dlg.scroll <- Float.min max (Float.max 0. (dlg.scroll -. (wheel *. 48.)))

let house_card_art_size (c : card) = if c.is_spell then (100., 108.) else (96., 96.)
let house_card_title_size = 19.
let house_card_sub_size = 16.
let house_card_desc_size = 17.
let house_card_title_lh = 23.
let house_card_desc_lh = 21.

let measure_house_card col_w (c : card) =
  let title, subtitle, desc_text = card_preview_bits c in
  let art_w, art_h = house_card_art_size c in
  let pad = 10. in
  let info_w = Float.max 60. (col_w -. art_w -. (pad *. 3.)) in
  let title_lines = wrap ~font:paper_font house_card_title_size info_w title in
  let desc_lines =
    if desc_text = "" then [] else wrap ~font:paper_font house_card_desc_size (col_w -. (pad *. 2.)) desc_text
  in
  let info_h = (float (List.length title_lines) *. house_card_title_lh) +. 4. +. house_card_sub_size in
  let header_h = Float.max art_h info_h in
  let desc_h = if desc_lines = [] then 0. else 6. +. (float (List.length desc_lines) *. house_card_desc_lh) in
  (title_lines, subtitle, desc_lines, art_w, art_h, pad +. header_h +. desc_h +. pad)

let draw_house_card r (c : card) title_lines subtitle desc_lines art_w art_h =
  let pad = 10. in
  fill ~round:0.04 r (rgba 140 107 41 70);
  outline ~round:0.04 ~thick:1. r (rgba 140 107 41 90);
  let art = rect (r.x +. pad) (r.y +. pad) art_w art_h in
  draw_card_art art c;
  let tx = r.x +. pad +. art_w +. 8. in
  List.iteri
    (fun i l ->
      text ~font:paper_font ~size:house_card_title_size ~color:ink tx
        (r.y +. pad +. (float i *. house_card_title_lh))
        l)
    title_lines;
  let sub_y = r.y +. pad +. (float (List.length title_lines) *. house_card_title_lh) +. 2. in
  text ~font:paper_font ~size:house_card_sub_size ~color:ink_soft tx sub_y subtitle;
  let header_h =
    Float.max art_h ((float (List.length title_lines) *. house_card_title_lh) +. 4. +. house_card_sub_size)
  in
  List.iteri
    (fun i l ->
      text ~font:paper_font ~size:house_card_desc_size ~color:ink (r.x +. pad)
        (r.y +. pad +. header_h +. 6. +. (float i *. house_card_desc_lh))
        l)
    desc_lines

let rec house_card_pairs = function
  | a :: b :: rest -> (Some a, Some b) :: house_card_pairs rest
  | [ a ] -> [ (Some a, None) ]
  | [] -> []

let close_house_dlg ui = ui.house_dlg <- None

let draw_house_details ui dlg =
  click_full (fun () -> close_house_dlg ui);
  let box = house_dlg_box in
  paper box;
  on_click box (fun () -> ());
  text_c ~font:paper_font ~size:30. ~color:ink box (box.y +. 16.) (house_title dlg.house);
  let view = house_dlg_view in
  let col_gap = 12. in
  let col_w = (view.w -. col_gap) /. 2. in
  let blurb = house_blurb dlg.house in
  let blurb_size = 18. and blurb_lh = 22. in
  let blurb_lines = if blurb = "" then [] else wrap ~font:paper_font blurb_size view.w blurb in
  let blurb_h = if blurb_lines = [] then 0. else (float (List.length blurb_lines) *. blurb_lh) +. 10. in
  let cards = dlg.house.h_cards @ dlg.house.h_additional_cards in
  let measured = List.map (fun c -> (c, measure_house_card col_w c)) cards in
  let rows = house_card_pairs measured in
  let row_gap = 12. in
  let content_h =
    List.fold_left
      (fun acc -> function
        | Some (_, (_, _, _, _, _, h0)), Some (_, (_, _, _, _, _, h1)) -> acc +. Float.max h0 h1 +. row_gap
        | Some (_, (_, _, _, _, _, h0)), None -> acc +. h0 +. row_gap
        | None, _ -> acc)
      blurb_h rows
  in
  dlg.content_h <- content_h;
  let max = Float.max 0. (content_h -. view.h) in
  dlg.scroll <- Float.min max (Float.max 0. dlg.scroll);
  begin_clip view;
  let y = ref (view.y -. dlg.scroll) in
  if blurb_lines <> [] then (
    List.iteri
      (fun i l -> text ~font:paper_font ~size:blurb_size ~color:ink view.x (!y +. (float i *. blurb_lh)) l)
      blurb_lines;
    y := !y +. blurb_h);
  let seen (r : rect) = r.y +. r.h > view.y && r.y < view.y +. view.h in
  let draw_one x (c, (title_lines, subtitle, desc_lines, art_w, art_h, h)) =
    let r = rect x !y col_w h in
    if seen r then draw_house_card r c title_lines subtitle desc_lines art_w art_h;
    h
  in
  List.iter
    (fun pair ->
      let h =
        match pair with
        | Some left, Some right ->
            Float.max (draw_one view.x left) (draw_one (view.x +. col_w +. col_gap) right)
        | Some left, None -> draw_one view.x left
        | None, _ -> 0.
      in
      y := !y +. h +. row_gap)
    rows;
  end_clip ();
  if max > 0. then (
    fill house_dlg_bar (rgba 140 107 41 90);
    let th = Float.max 24. (view.h *. view.h /. content_h) in
    let ty = view.y +. (dlg.scroll /. max *. (view.h -. th)) in
    let thumb = rect house_dlg_bar.x ty house_dlg_bar.w th in
    fill ~round:0.06 thumb (rgba 122 90 40 255);
    if R.is_mouse_button_down R.MouseButton.Left then (
      if dlg.drag || inside thumb then (
        dlg.drag <- true;
        hovering := true;
        let _, my = mouse () in
        let f = (my -. view.y -. (th /. 2.)) /. (view.h -. th) in
        dlg.scroll <- Float.min max (Float.max 0. (f *. max))))
    else dlg.drag <- false)
  else dlg.drag <- false;
  paper_btn_row ~size:20.
    ~cx:(box.x +. (box.w /. 2.))
    ~y:(box.y +. box.h -. 48.)
    [ ("ok", fun () -> close_house_dlg ui) ]

let over_ready ui = match ui.over_at with Some t -> R.get_time () >= t | None -> false

let game_over_text ui =
  if not (over_ready ui) then None
  else
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
  (* Scala actionBar sits on the board group, so the camera scroll moves it. *)
  let bar = rect 8. (oy ui 4.) ((n *. icon) +. ((n -. 1.) *. gap) +. (pad *. 2.)) (icon +. (pad *. 2.)) in
  (* FantasyFrame: gold rim then a lighter wash than the old 0.70 fill. *)
  fill ~round:0.08 bar (rgba 107 82 31 140);
  fill ~round:0.08 (rect (bar.x +. 2.) (bar.y +. 2.) (bar.w -. 4.) (bar.h -. 4.)) (rgba 18 13 5 112);
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

(* ------------------------------------------------------------- main loop *)

type screen = Lobby of lobby_ui | Campaign of campaign_ui | Game of game_ui | Epilogue

let background ui =
  let house = (desc ui).gd_players.(owner).pd_houses |> fun hs -> hs.(Array.length hs - 1).phd_house in
  texture (Assets.background house)

let window_title = "The Mancer's echo"
let chrome_btn_w = 36.
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
  let fs = 13. in
  R.draw_text_ex !regular window_title (R.Vector2.create 8. ((chrome_h -. fs) /. 2.)) fs 0. fg;
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
            (int_of_float (cx -. 5.))
            (int_of_float cy)
            (int_of_float (cx +. 5.))
            (int_of_float cy) ink
      | `Max -> R.draw_rectangle_lines (int_of_float (cx -. 5.)) (int_of_float (cy -. 5.)) 10 10 ink
      | `Close ->
          R.draw_line
            (int_of_float (cx -. 4.))
            (int_of_float (cy -. 4.))
            (int_of_float (cx +. 4.))
            (int_of_float (cy +. 4.))
            ink;
          R.draw_line
            (int_of_float (cx +. 4.))
            (int_of_float (cy -. 4.))
            (int_of_float (cx -. 4.))
            (int_of_float (cy +. 4.))
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

let parse_shot () =
  let file = ref "" and wait = ref 2. and bots = ref false and setup_only = ref false in
  let seed = ref None and special = ref None in
  Arg.parse
    [
      ("--shot", Arg.Set_string file, "FILE  save a screenshot to FILE and exit");
      ("--wait", Arg.Set_float wait, "SECS  seconds to wait before the shot (default 2)");
      ("--bots", Arg.Set bots, " two oracle bots play, so the board fills up");
      ("--setup", Arg.Set setup_only, " shoot the lobby instead of a game");
      ("--seed", Arg.Int (fun n -> seed := Some n), "N  seed for the deal");
      ("--special", Arg.String (fun h -> special := Some h), "NAME  special house for both players");
    ]
    (fun a -> raise (Arg.Bad a))
    "gui [--shot FILE [--wait SECS] [--bots] [--setup] [--seed N] [--special NAME]]";
  if !file = "" then None
  else
    Some
      { file = !file; wait = !wait; bots = !bots; setup_only = !setup_only; seed = !seed; special = !special }

let () =
  let shot = parse_shot () in
  R.set_trace_log_level R.TraceLogLevel.Warning;
  (* GNOME on Wayland paints a large libdecor header even around GLFW. Prefer
     X11 so the window stays undecorated and the thin bar is the chrome. *)
  (match Sys.getenv_opt "WAYLAND_DISPLAY" with
  | Some _ -> Unix.putenv "WAYLAND_DISPLAY" ""
  | None -> ());
  R.set_config_flags R.ConfigFlags.(window_resizable + window_undecorated + msaa_4x_hint + vsync_hint);
  R.init_window 832 (768 + int_of_float chrome_h) window_title;
  size_window_to_monitor ();
  R.set_window_min_size 416 (384 + int_of_float chrome_h);
  R.set_exit_key R.Key.Null;
  R.set_target_fps 60;
  R.init_audio_device ();
  load_fonts ();
  Fx.set_font !bold;
  Fx.load ();
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
        budget_ms = lobby.gs.budget_ms;
        seed;
      }
    in
    match new_game setup (s0.portrait, s1.portrait) with
    | ui -> screen := Game ui
    | exception Failure msg -> lobby.message <- msg
  in
  let campaign_fight node =
    drop_dialog ();
    let p = Prefs.load () in
    let player_special = Table.resolve_special (fst (Prefs.setup_specials p)) in
    let setup = Table.campaign_setup ~budget_ms:(Prefs.setup_budget p) ~player_special node in
    let you = Prefs.player_portrait p in
    let opp = match node.Campaign.fight with Some f -> f.portrait_id | None -> "summoner" in
    match new_game ~campaign:(Some node) setup (you, opp) with
    | ui -> screen := Game ui
    | exception Failure msg -> ( match !screen with Lobby l -> l.message <- msg | _ -> ())
  in
  let quit = ref false in
  (* A game shot skips the lobby without persisting its flags. *)
  (match shot with
  | Some sh when not sh.setup_only -> (
      match !screen with
      | Lobby lobby ->
          let s0, s1 = lobby.duel.sides in
          if sh.bots then (
            s0.seat <- Table.Bot "oracle";
            s0.portrait <- clamp_portrait s0.seat "ember";
            s1.seat <- Table.Bot "oracle";
            s1.portrait <- clamp_portrait s1.seat "oracle");
          (match sh.special with Some h -> lobby.gs.specials <- ([ h ], [ h ]) | None -> ());
          if sh.bots then lobby.gs.budget_ms <- 100;
          lobby.gs.seed_text <- (match sh.seed with Some n -> string_of_int n | None -> "");
          fight ~persist:false lobby
      | _ -> ())
  | _ -> ());
  let t0 = R.get_time () in
  while not (!quit || R.window_should_close ()) do
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
        if lobby.show_settings then settings_seed_keys lobby.gs;
        let wheel = R.get_mouse_wheel_move () in
        if wheel <> 0. && lobby.show_settings && lobby.gs.tab = 0 && inside class_wheel then
          class_scroll_by lobby.gs wheel;
        if R.is_key_pressed R.Key.Escape then
          if lobby.show_campaign_intro then
            close_dialog "campaign-intro" (fun () -> lobby.show_campaign_intro <- false)
          else if lobby.show_settings then
            close_dialog "settings" (fun () ->
                lobby.show_settings <- false;
                lobby.gs.seed_focus <- false)
          else if lobby.show_duel then close_dialog "duel" (fun () -> lobby.show_duel <- false)
          else quit := true
        else if lobby.show_duel && R.is_key_pressed R.Key.Enter then fight lobby
        else if (not lobby.show_duel) && (not lobby.show_settings) && not lobby.show_campaign_intro then
          lobby_keys lobby
    | Campaign camp ->
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
          if dlg_open camp.dlg then close_dialog "campaign" (fun () -> camp.dlg <- Camp_none)
          else camp.pending_lobby <- true
    | Epilogue -> if R.is_key_pressed R.Key.Escape then screen := Lobby (new_lobby ())
    | Game ui ->
        drive_bot ui;
        tick_slides ui;
        let wheel = R.get_mouse_wheel_move () in
        (if wheel <> 0. then
           match (ui.settings, ui.house_dlg) with
           | Some gs, _ when gs.tab = 0 && inside class_wheel -> class_scroll_by gs wheel
           | None, Some dlg -> house_scroll_by dlg wheel
           | _ -> ui.scroll_y <- Float.min max_scroll (Float.max 0. (ui.scroll_y +. (wheel *. 48.))));
        if R.is_key_pressed R.Key.Escape then
          match (ui.settings, ui.house_dlg, ui.campaign_end) with
          | Some _, _, _ -> close_dialog "settings" (fun () -> ui.settings <- None)
          | None, Some _, _ -> close_house_dlg ui
          | None, None, Some _ ->
              settle ui;
              screen := Campaign (new_campaign ~from_game:true ())
          | None, None, None -> ui.mode <- Browse
        else if R.is_mouse_button_pressed R.MouseButton.Right then ui.mode <- Browse;
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
                screen := Campaign (new_campaign ())))
    | Campaign camp -> draw_campaign camp
    | Epilogue -> draw_epilogue ~menu:(fun () -> screen := Lobby (new_lobby ()))
    | Game ui ->
        (match background ui with
        | Some t ->
            let a = fade_smooth ((R.get_time () -. ui.bg_at) /. 2.6) in
            with_alpha a (fun () -> cover t)
        | None -> ());
        ui.hover <- None;
        ui.hover_opp <- false;
        lay := make_layout ui.table;
        (match peek_smoke_card ui with Some r -> smoke_show r | None -> smoke_hide ());
        Fx.Hover.draw ();
        (match !(ui.last_played) with
        | Some { card = Some c; _ } when inside (last_played_rect ui) -> ui.hover <- Some (Hover_card c)
        | _ -> ());
        (* Rematch/new/switch must pick houses from the settings dialog (or the
           prefs it already persisted), not the single pair this fight resolved. *)
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
            | Some _ -> (Table.resolve_special l0, snd base.specials)
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
        let restart seed portraits =
          settle ui;
          screen := Game (new_game ~campaign:ui.campaign { (setup_now ()) with seed } portraits)
        in
        let switch () =
          let a, b = (setup_now ()).seats and sa, sb = (setup_now ()).specials and pa, pb = ui.portraits in
          settle ui;
          screen :=
            Game
              (new_game ~campaign:ui.campaign
                 { (setup_now ()) with seats = (b, a); specials = (sb, sa); seed = None }
                 (pb, pa))
        in
        draw_board ui;
        draw_pass ui;
        draw_hands ui;
        draw_slides ui;
        (* After the cards so bolts overlay them; Raylib later-draws-win. *)
        Fx.draw_all ~now:(R.get_time ());
        draw_pops ui;
        draw_preview ui;
        draw_last_played ui;
        draw_menu ui
          ~quit:(fun () -> to_lobby ())
          ~settings:(fun () -> ui.settings <- Some (game_settings_from_prefs ()))
          ~switch
          ~replay:(fun () -> restart (Some (Table.seed ui.table)) ui.portraits)
          ~start_new:(fun () -> restart None ui.portraits);
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
                    if won && node.id = Campaign.night_last then (
                      settle ui;
                      Prefs.reset_campaign (Prefs.load ());
                      screen := Epilogue)
                    else ui.campaign_end <- Some won
                | _ -> ());
                match ui.campaign_end with
                | None -> ()
                | Some won ->
                    draw_campaign_end ~node ~won
                      ~replay:(fun () ->
                        settle ui;
                        campaign_fight node)
                      ~map:(fun () ->
                        settle ui;
                        screen := Campaign (new_campaign ~from_game:true ()))))
        | To_move _ ->
            ui.ended_at <- None;
            ui.campaign_end <- None);
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
    R.set_mouse_cursor (if !hovering then R.MouseCursor.Pointing_hand else R.MouseCursor.Default);
    (* only humans submit from a click, never while a search reads the board;
       leaving a game settles the search first *)
    Option.iter (fun f -> f ()) !pending
  done;
  (match !screen with Game ui -> settle ui | _ -> ());
  Hashtbl.iter (fun _ s -> Option.iter R.unload_sound s) sounds;
  Fx.unload ();
  R.close_audio_device ();
  R.close_window ()
