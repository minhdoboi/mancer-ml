(* Raylib UI drawing kit: frame scale, colours, text, images and widgets. *)

open Mancer
open Sp
module R = Raylib

(* ------------------------------------------------------------ frame scale *)

let vw = 832.
let vh = 768.

(* Title bar in screen pixels, drawn in place of the desktop's large CSD so
   the window reads like Scala's "The Mancer's echo" chrome. *)
let chrome_h = 33.

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
let ink_selected = rgba 244 182 115 255
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
    if i <= 0 then 0 else if i >= n then n else if Char.code s.[i] land 0xC0 = 0x80 then go (i - 1) else i
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
    range 32 126 @ range 160 255 @ script @ [ 0x2013; 0x2014; 0x2018; 0x2019; 0x201C; 0x201D; 0x2022; 0x2026 ]
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

(* Scala SpellCast.playSound: English name, spaces stripped, looked up in sounds{}.
   A card with no clip of its own borrows its base house's: anger is fire's variant
   and fear is air's, so their spells sound like the fire/air cards they mirror,
   and the other variant houses' spells take the clip of the card they mirror
   (Retirement's calm chime is Meditate's, Rain of Tears is Acidic shower's, Care
   is Natures care's, Sexual Bond is Plant therapy's). *)
let card_sound_name (c : card) =
  match c.key with
  | "sadness.retirement" -> "meditate"
  | "sadness.tears" -> "acidicshower"
  | "joy.care" -> "naturescare"
  | "joy.bond" -> "planttherapy"
  | "joy.burst" -> "solarburst"
  | "anger.wave" -> "flamewave"
  | "anger.inferno" -> "inferno"
  | "anger.apocalypse" -> "apocalypse"
  | "fear.strike" -> "calltothunder"
  | "fear.spirit" -> "lightnin"
  | "fear.invasion" -> "onslaughtofsouls"
  | "fear.fear" -> "twister"
  (* no clip of its own: the smith's blow borrows the gauntlet's heavy impact *)
  | "barbarians.dwarvenhammer" -> "barguliangauntlet"
  | _ -> Assets.english_card_name c |> String.lowercase_ascii |> String.split_on_char ' ' |> String.concat ""

let play_card_sound (c : card) = play_sound ~volume:0.9 (card_sound_name c)

(* [r] covered, cropped to its aspect ratio *)
let image ?(tint = R.Color.white) tex r =
  let tw = float (R.Texture.width tex) and th = float (R.Texture.height tex) in
  let ra = r.w /. r.h in
  let sw, sh = if tw /. th > ra then (th *. ra, th) else (tw, tw /. ra) in
  let src = R.Rectangle.create ((tw -. sw) /. 2.) ((th -. sh) /. 2.) sw sh in
  R.draw_texture_pro tex src (screen r) (R.Vector2.create 0. 0.) 0. (alpha_color tint)

(* Scala SpellIcon: keep only the circular window of the spell art. *)
let circle_vs =
  {|#version 330
in vec3 vertexPosition;
in vec2 vertexTexCoord;
in vec4 vertexColor;
out vec2 fragTexCoord;
out vec4 fragColor;
uniform mat4 mvp;
void main() {
  fragTexCoord = vertexTexCoord;
  fragColor = vertexColor;
  gl_Position = mvp * vec4(vertexPosition, 1.0);
}
|}

let circle_fs =
  {|#version 330
in vec2 fragTexCoord;
in vec4 fragColor;
uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform vec4 u_bounds;
out vec4 finalColor;
void main() {
  vec2 span = u_bounds.zw - u_bounds.xy;
  vec2 local = (fragTexCoord - u_bounds.xy) / span;
  vec2 d = local - vec2(0.5);
  float e = dot(d, d);
  float mask = smoothstep(0.25, 0.22, e);
  if (mask < 0.02) discard;
  vec4 color = texture(texture0, fragTexCoord) * fragColor * colDiffuse;
  color.a *= mask;
  finalColor = color;
}
|}

let circle_sh : R.Shader.t option option ref = ref None

let circle_shader () =
  match !circle_sh with
  | Some s -> s
  | None ->
      let s =
        let sh = R.load_shader_from_memory circle_vs circle_fs in
        if R.is_shader_valid sh then Some sh else None
      in
      circle_sh := Some s;
      s

let image_circle ?(tint = R.Color.white) tex r =
  let tw = float (R.Texture.width tex) and th = float (R.Texture.height tex) in
  let ra = r.w /. r.h in
  let sw, src_h = if tw /. th > ra then (th *. ra, th) else (tw, tw /. ra) in
  let sx = (tw -. sw) /. 2. and sy = (th -. src_h) /. 2. in
  let src = R.Rectangle.create sx sy sw src_h in
  match circle_shader () with
  | None -> image ~tint tex r
  | Some shader ->
      let l = R.get_shader_location shader "u_bounds" in
      if R.ShaderLoc.to_int l >= 0 then (
        let p = Ctypes.(allocate_n float ~count:4) in
        Ctypes.(p <-@ sx /. tw);
        Ctypes.(p +@ 1 <-@ sy /. th);
        Ctypes.(p +@ 2 <-@ (sx +. sw) /. tw);
        Ctypes.(p +@ 3 <-@ (sy +. src_h) /. th);
        R.set_shader_value shader l (Ctypes.to_voidp p) R.ShaderUniformDataType.Vec4);
      R.begin_shader_mode shader;
      R.draw_texture_pro tex src (screen r) (R.Vector2.create 0. 0.) 0. (alpha_color tint);
      R.end_shader_mode ()

(* [r] filled, texture stretched (frames keep native aspect because [r] does) *)
let image_at ?(tint = R.Color.white) tex r =
  let src = R.Rectangle.create 0. 0. (float (R.Texture.width tex)) (float (R.Texture.height tex)) in
  R.draw_texture_pro tex src (screen r) (R.Vector2.create 0. 0.) 0. (alpha_color tint)

(* Backdrop filling the whole window without stretching. Crop to the window
   aspect so letterbox bands are covered and the art keeps its proportions. *)
let cover_src tex =
  let sw = float (R.get_screen_width ()) and sh = float (R.get_screen_height ()) in
  let tw = float (R.Texture.width tex) and th = float (R.Texture.height tex) in
  let ra = sw /. sh in
  let src_w, src_h = if tw /. th > ra then (th *. ra, th) else (tw, tw /. ra) in
  R.Rectangle.create ((tw -. src_w) /. 2.) ((th -. src_h) /. 2.) src_w src_h

let cover tex =
  let sw = float (R.get_screen_width ()) and sh = float (R.get_screen_height ()) in
  R.draw_texture_pro tex (cover_src tex) (R.Rectangle.create 0. 0. sw sh) (R.Vector2.create 0. 0.) 0.
    (alpha_color R.Color.white)

(* Scala Board.ScrollingSky: the "-sky" layer behind a "-ground" painting is the
   original panorama doubled and mirrored, so Repeat has no seam. A window one
   panorama wide slides across it at [sky_speed] textures per second. The
   window is cut like the ground's own [cover_src] crop, so both land in the
   same place on screen. *)
let sky_speed = 0.008

let draw_sky ~at sky ground =
  let sw = float (R.get_screen_width ()) and sh = float (R.get_screen_height ()) in
  let crop = cover_src ground in
  let tw = float (R.Texture.width sky) and th = float (R.Texture.height sky) in
  let gw = float (R.Texture.width ground) and gh = float (R.Texture.height ground) in
  let k = tw /. (2. *. gw) in
  let src =
    R.Rectangle.create
      (mod_float (at *. sky_speed) 1. *. tw)
      (R.Rectangle.y crop *. (th /. gh))
      (R.Rectangle.width crop *. k)
      (R.Rectangle.height crop *. (th /. gh))
  in
  R.set_texture_wrap sky R.TextureWrap.Repeat;
  R.draw_texture_pro sky src (R.Rectangle.create 0. 0. sw sh) (R.Vector2.create 0. 0.) 0.
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
   right). No name — same as the Scala card button. [decor] paints the combat
   overlay (mortal, sand, …) over the art but under the frame and the numbers:
   Scala SlotCardActors.updateZIndex gives decorators zIndex 1, below the frame
   (2) and the labels (3), so the stone crops them. *)
let draw_card_face desc r (c : card) ~cost ~attack ~life ~life_color ~tint ~decor =
  let spell = c.is_spell || c.is_aura in
  let nw, nh = if spell then spell_native else creature_native in
  let fr, s = fit_in r nw nh in
  let ax, ay, aw, ah = if spell then spell_art_box else creature_art_box in
  let art = box fr (ax, ay, aw, ah) s in
  (match card_texture c with Some t -> image ~tint t art | None -> fill ~round:0.04 art panel_hi);
  decor ();
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
let released r = inside r && R.is_mouse_button_released R.MouseButton.Left

let on_click r f =
  if inside r then hovering := true;
  if clicked r then pending := Some f

(* A click outside [keep]: the handler is registered after the widgets under
   the pointer, so it wins over them, while the rects in [keep] keep theirs. *)
let on_click_outside ~keep f =
  if clicked (rect 0. 0. vw vh) && not (List.exists inside keep) then pending := Some f

let on_release r f =
  if inside r then hovering := true;
  if released r then pending := Some f

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

(* Scala TextTooltip on the action-bar icons: a small label under the button. *)
let tooltip_below anchor label =
  let size = 14. and pad_x = 8. and pad_y = 5. in
  let tw = text_w ~font:paper_font size label in
  let w = tw +. (pad_x *. 2.) and h = size +. (pad_y *. 2.) +. 1. in
  let x = Float.min (vw -. w -. 4.) (Float.max 4. (anchor.x +. ((anchor.w -. w) /. 2.))) in
  let y = anchor.y +. anchor.h +. 6. in
  let box = rect x y w h in
  fill ~round:0.16 box (rgba 18 13 5 235);
  outline ~round:0.16 ~thick:1. box (rgba 246 196 82 170);
  text ~font:paper_font ~size ~color:gold (x +. pad_x) (y +. pad_y) label

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
    else if selected then rgba 108 52 0 195
    else if hot then rgba 199 143 61 255
    else rgba 184 132 55 255
  in
  fill ~round:0.12 r fill_c;
  outline ~round:0.12 ~thick:1.2 r (if hot || selected then rgba 92 54 18 255 else rgba 110 70 24 230);
  text_c ~font:paper_font ~size
    ~color:(if enabled then if selected then ink_selected else ink else ink_soft)
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
      if on then fill ~round:0.6 (rect (r.x +. 8.) (r.y +. r.h -. 4.) (r.w -. 16.) 3.) (rgba 122 90 40 255);
      text_c ~font:paper_font ~size
        ~color:(if on || hot then ink else ink_soft)
        r
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

(* A parchment select box: the closed box shows the current choice, clicking
   it drops a scrollable list of the options. Raylib has no combo box, so this
   is drawn by hand like the rest of the kit. *)
type select_state = {
  sel_open : bool ref;
  sel_scroll : float ref;
  sel_drag : (float * float) option ref;  (** the grab point and the scroll when the drag started. *)
  mutable sel_popup : rect option;
}

let select_state () = { sel_open = ref false; sel_scroll = ref 0.; sel_drag = ref None; sel_popup = None }
let select_item_h = 22.
let select_rows = 9

(* The parchment edge the list is inset by. *)
let select_edge = 32.

(* Close the list and forget a drag in progress. *)
let select_close st =
  st.sel_open := false;
  st.sel_drag := None

(* Scroll an open list with the wheel, when the pointer is over it. *)
let select_scroll st wheel =
  if wheel <> 0. && !(st.sel_open) then
    match st.sel_popup with
    | Some r when inside r -> st.sel_scroll := !(st.sel_scroll) -. (wheel *. 48.)
    | _ -> ()

(* Bring the current choice into view when the list opens. *)
let select_reveal st items selected =
  let rows = min select_rows (List.length items) in
  let view_h = float rows *. select_item_h in
  match List.find_index (fun (v, _) -> v = selected) items with
  | None -> ()
  | Some i ->
      let y = float i *. select_item_h in
      let scroll = !(st.sel_scroll) in
      let scroll =
        if y < scroll then y
        else if y +. select_item_h > scroll +. view_h then y +. select_item_h -. view_h
        else scroll
      in
      st.sel_scroll := Float.max 0. scroll

(* The bar scrolls the list: drag the thumb, or click the bar to jump there. *)
let select_bar st ~bar ~view_h ~content_h ~max_scroll =
  let th = Float.max 20. (view_h *. view_h /. content_h) in
  let span = Float.max 1. (view_h -. th) in
  let ty = bar.y +. (!(st.sel_scroll) /. max_scroll *. span) in
  let thumb = rect bar.x ty bar.w th in
  fill bar (rgba 140 107 41 90);
  fill ~round:0.4 thumb (rgba 140 107 41 220);
  let _, my = mouse () in
  match !(st.sel_drag) with
  | Some (grab, scroll0) ->
      if R.is_mouse_button_released R.MouseButton.Left then st.sel_drag := None
      else
        st.sel_scroll := Float.min max_scroll (Float.max 0. (scroll0 +. ((my -. grab) *. max_scroll /. span)))
  | None ->
      if R.is_mouse_button_pressed R.MouseButton.Left then
        if inside thumb then st.sel_drag := Some (my, !(st.sel_scroll))
        else if inside bar then
          let y = Float.min (Float.max 0. (my -. bar.y -. (th /. 2.))) span in
          st.sel_scroll := Float.min max_scroll (Float.max 0. (y *. max_scroll /. span))

(* [items] are (value, label) pairs; picking one hands its value to [on_pick]. *)
let pselect ~size ~width ~state ~x ~y ~items ~selected ~on_pick =
  let h = 26. in
  let box = rect x y width h in
  let label =
    match List.find_opt (fun (v, _) -> v = selected) items with Some (_, l) -> l | None -> selected
  in
  let hot = inside box in
  if hot then hovering := true;
  fill ~round:0.12 box (if hot then rgba 199 143 61 255 else rgba 184 132 55 255);
  outline ~round:0.12 ~thick:1.2 box (if hot then rgba 92 54 18 255 else rgba 110 70 24 230);
  text ~font:paper_font ~size ~color:ink (box.x +. 10.)
    (box.y +. ((box.h -. size) /. 2.))
    (fit ~font:paper_font size (box.w -. 34.) label);
  (* the drop arrow, drawn as a chevron: the atlas has no arrow glyph *)
  let ax = box.x +. box.w -. 20. and ay = box.y +. (box.h /. 2.) -. 3. in
  let tip = pos (ax +. 5.5) (ay +. 7.) in
  R.draw_line_ex (pos ax ay) tip (1.6 *. !scale) (alpha_color ink);
  R.draw_line_ex tip (pos (ax +. 11.) ay) (1.6 *. !scale) (alpha_color ink);
  on_click box (fun () ->
      if !(state.sel_open) then select_close state
      else (
        state.sel_open := true;
        select_reveal state items selected));
  if !(state.sel_open) then (
    let count = List.length items in
    let rows = min select_rows (max 1 count) in
    let view_h = float rows *. select_item_h in
    let content_h = float count *. select_item_h in
    let max_scroll = Float.max 0. (content_h -. view_h) in
    state.sel_scroll := Float.min max_scroll (Float.max 0. !(state.sel_scroll));
    let panel_w = box.w +. 40. in
    let panel = rect (box.x +. box.w -. panel_w) (box.y +. h +. 4.) panel_w (view_h +. (2. *. select_edge)) in
    paper panel;
    let view =
      rect (panel.x +. select_edge) (panel.y +. select_edge) (panel.w -. (2. *. select_edge)) view_h
    in
    state.sel_popup <- Some view;
    let bar = rect (view.x +. view.w +. 2.) view.y 8. view_h in
    (* A click outside the box, the list and its bar closes it. The rows
       register their own handler after this one, so a row still picks. *)
    on_click_outside ~keep:[ box; bar ] (fun () -> select_close state);
    begin_clip view;
    List.iteri
      (fun i (value, label) ->
        let r =
          rect view.x (view.y +. (float i *. select_item_h) -. !(state.sel_scroll)) view.w select_item_h
        in
        let on = value = selected in
        let hot = inside r in
        if hot then fill ~round:0.2 r (rgba 199 143 61 90);
        if on then fill ~round:0.2 r (rgba 108 52 0 130);
        text ~font:paper_font ~size
          ~color:(if on then ink_selected else ink)
          (r.x +. 6.) (r.y +. 3.)
          (fit ~font:paper_font size (r.w -. 12.) label);
        (* a scrolled-out row is not clickable *)
        match overlap r view with
        | Some hit ->
            on_click hit (fun () ->
                select_close state;
                on_pick value)
        | None -> ())
      items;
    end_clip ();
    if max_scroll > 0. then select_bar state ~bar ~view_h ~content_h ~max_scroll)
  else (
    state.sel_popup <- None;
    state.sel_drag := None)

let debug_mode () = try Prefs.debug_mode (Prefs.load ()) with _ -> false

(* Parchment checkbox, like Scala's CheckBox("parchment"): a dark square
   with a gold tick, plus the label. [clip] limits the click to a viewport. *)
let checkbox ?(size = 18.) ?(box_s = 16.) ?(release = false) ?(armed = true) ?clip ~checked ~x ~y ~max_w label
    f =
  let gap = 8. in
  let text_x = x +. box_s +. gap in
  let text_max = Float.max 40. (max_w -. box_s -. gap) in
  let lines = wrap ~font:paper_font size text_max label in
  let line_h = size +. 4. in
  let h = Float.max box_s (float (max 1 (List.length lines)) *. line_h) in
  let label_w = List.fold_left (fun acc l -> Float.max acc (text_w ~font:paper_font size l)) 0. lines in
  let hit_w = Float.min max_w (box_s +. gap +. label_w +. 4.) in
  let hit0 = rect x y hit_w h in
  let hit = match clip with None -> Some hit0 | Some view -> overlap hit0 view in
  let hot = match hit with Some r -> inside r | None -> false in
  let box = rect x (y +. ((h -. box_s) /. 2.)) box_s box_s in
  fill ~round:0.16 box (rgba 28 18 12 230);
  outline ~round:0.16
    ~thick:(if checked || hot then 1.8 else 1.3)
    box
    (if checked || hot then rgba 92 54 18 255 else rgba 70 46 22 220);
  if checked then (
    let thick = Float.max 1.8 (box_s *. 0.14) *. !scale in
    let c = alpha_color gold in
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
  Option.iter
    (fun r ->
      if armed then if release then on_release r f else on_click r f else if inside r then hovering := true)
    hit

let close_dialog _id k = k ()
let drop_dialog () = ()

let draw_dialog_dim () =
  let sw = R.get_screen_width () and sh = R.get_screen_height () in
  R.draw_rectangle 0 0 sw sh (rgba 0 0 0 (int_of_float ((255. *. 0.45) +. 0.5)))

let run_dialog _id shown draw =
  if shown then (
    draw_dialog_dim ();
    draw ())
