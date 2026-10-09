(* Spell and ability visuals: types, frame transform, timeline, shaders, shakes and the style table.
   Board coordinates, same transform as gui.ml. *)

open Mancer
open Sp
module R = Raylib

type spot = { x : float; y : float }
type t = { start : float; duration : float; draw : age:float -> unit }
type recipe = { duration : float; blocking : bool; make : start:float -> t }

type layout = {
  slot_center : player_id -> int -> spot;
  slot_floor : player_id -> int -> spot;
  flame_base : player_id -> int -> spot;
  life : player_id -> spot;
  house : player_id -> int -> spot;
  occupied : player_id -> int list;
  scarecrow : player_id -> int option;
  source_of : command -> spot option;
  slot_house : player_id -> int -> int option;
  slot_life : player_id -> int -> int option;
  slot_card : player_id -> int -> string option;
  house_cards : player_id -> int -> spot list;
}

let scale = ref 1.
let off_x = ref 0.
let off_y = ref 0.

let set_transform ~scale:s ~off_x:ox ~off_y:oy =
  scale := s;
  off_x := ox;
  off_y := oy

let banner_font = ref (R.get_font_default ())
let set_font f = banner_font := f
let sx x = !off_x +. (x *. !scale)
let sy y = !off_y +. (y *. !scale)
let v x y = R.Vector2.create (sx x) (sy y)
let clamp01 u = if u < 0. then 0. else if u > 1. then 1. else u
let ease_out u = 1. -. ((1. -. u) *. (1. -. u))
let ease_out_quad = ease_out

let swing_out u =
  let u = clamp01 u in
  let x = u -. 1. in
  1. +. (x *. x *. ((2.70158 *. x) +. 1.70158))

let byte x = int_of_float (Float.max 0. (Float.min 255. (x *. 255.)))
let rgb r g b a = R.Color.create (byte r) (byte g) (byte b) (byte a)
let _fade_a a fade = a *. fade
let circle x y rad c = if rad *. !scale > 0.4 then R.draw_circle_v (v x y) (rad *. !scale) c

let ellipse x y rx ry c =
  if rx *. !scale > 0.4 && ry *. !scale > 0.4 then R.draw_ellipse_v (v x y) (rx *. !scale) (ry *. !scale) c

let line x0 y0 x1 y1 thick c =
  if thick *. !scale > 0.3 then R.draw_line_ex (v x0 y0) (v x1 y1) (thick *. !scale) c

let tri x1 y1 x2 y2 x3 y3 c =
  R.draw_triangle (v x1 y1) (v x2 y2) (v x3 y3) c;
  R.draw_triangle (v x1 y1) (v x3 y3) (v x2 y2) c

let rect x y w h c = R.draw_rectangle_rec (R.Rectangle.create (sx x) (sy y) (w *. !scale) (h *. !scale)) c

let additive f =
  R.begin_blend_mode R.BlendMode.Additive;
  f ();
  R.end_blend_mode ()

let noise a b c =
  let ( + ) = Int32.add and ( * ) = Int32.mul in
  let n = (Int32.of_int a * 374761393l) + (Int32.of_int b * 668265263l) + (Int32.of_int c * 1442695041l) in
  let n = Int32.mul (Int32.logxor n (Int32.shift_right_logical n 13)) 1274126177l in
  Int32.to_float (Int32.logand n 0xffffffl) /. 16777215.

let lerp a b t = a +. ((b -. a) *. t)
let hypot dx dy = sqrt ((dx *. dx) +. (dy *. dy))
let slots_of pid f = Array.init nb_slots (fun n -> f pid n)

(* ---------------------------------------------------------------- timeline *)

(* Board scroll when each effect spawned: its layout spots already include that
   scroll, so drawing shifts by how far the board has scrolled since. *)
let scroll_y = ref 0.
let set_scroll s = scroll_y := s
let active : (float * t) list ref = ref []
let spawn e = active := (!scroll_y, e) :: !active

let draw_all ~now =
  let base_y = !off_y in
  active :=
    List.filter
      (fun (spawn_scroll, e) ->
        let age = now -. e.start in
        if age >= 0. && age < e.duration then (
          off_y := base_y +. ((!scroll_y -. spawn_scroll) *. !scale);
          e.draw ~age;
          off_y := base_y);
        now < e.start +. e.duration)
      !active

let mk ~start ~duration draw = { start; duration; draw }
let recipe ~duration ~blocking make = { duration; blocking; make }

(* ---------------------------------------------------------------- shaders *)

type shader_fx = { sh : R.Shader.t; locs : (string, R.ShaderLoc.t) Hashtbl.t }

let shaders : (string, shader_fx option) Hashtbl.t = Hashtbl.create 8
let textures : (string, R.Texture.t option) Hashtbl.t = Hashtbl.create 8

let tex name =
  match Hashtbl.find_opt textures name with
  | Some t -> t
  | None ->
      let t =
        match Assets.particle name with
        | None -> None
        | Some path ->
            let t = R.load_texture path in
            if R.is_texture_valid t then (
              R.set_texture_filter t R.TextureFilter.Bilinear;
              Some t)
            else None
      in
      Hashtbl.replace textures name t;
      t

let loc sh name =
  match Hashtbl.find_opt sh.locs name with
  | Some l -> l
  | None ->
      let l = R.get_shader_location sh.sh name in
      Hashtbl.add sh.locs name l;
      l

let loc_ok l = R.ShaderLoc.to_int l >= 0

let set_float sh name v =
  let l = loc sh name in
  if loc_ok l then
    let p = Ctypes.(allocate float v) in
    R.set_shader_value sh.sh l (Ctypes.to_voidp p) R.ShaderUniformDataType.Float

let set_vec2 sh name x y =
  let l = loc sh name in
  if loc_ok l then (
    let p = Ctypes.(allocate_n float ~count:2) in
    Ctypes.(p <-@ x);
    Ctypes.(p +@ 1 <-@ y);
    R.set_shader_value sh.sh l (Ctypes.to_voidp p) R.ShaderUniformDataType.Vec2)

let load_named name =
  match Hashtbl.find_opt shaders name with
  | Some s -> s
  | None ->
      let s =
        match Assets.shader_pair name with
        | None -> None
        | Some (vs, fs) ->
            let sh = R.load_shader vs fs in
            if R.is_shader_valid sh then Some { sh; locs = Hashtbl.create 16 } else None
      in
      Hashtbl.replace shaders name s;
      s

let white_px : R.Texture.t option ref = ref None

let white_tex () =
  match !white_px with
  | Some t -> t
  | None ->
      let img = R.gen_image_color 1 1 (rgb 1. 1. 1. 1.) in
      let t = R.load_texture_from_image img in
      R.unload_image img;
      white_px := Some t;
      t

let load () =
  ignore (white_tex ());
  List.iter (fun n -> ignore (load_named n)) [ "shockwave"; "sel"; "grey"; "night" ]

let unload () =
  Hashtbl.iter (fun _ s -> Option.iter (fun s -> R.unload_shader s.sh) s) shaders;
  Hashtbl.clear shaders;
  Hashtbl.iter (fun _ t -> Option.iter R.unload_texture t) textures;
  Hashtbl.clear textures;
  Option.iter R.unload_texture !white_px;
  white_px := None

let with_grey on f =
  match (on, load_named "grey") with
  | true, Some s ->
      R.begin_shader_mode s.sh;
      f ();
      R.end_shader_mode ()
  | _ -> f ()

let with_night amount f =
  match (amount > 0.01, load_named "night") with
  | true, Some s ->
      set_float s "u_night" amount;
      R.begin_shader_mode s.sh;
      f ();
      R.end_shader_mode ()
  | _ -> f ()

let draw_sel ~x ~y ~w ~h =
  (* Scala card.selection: 200×200 mesh, offset −25, translated another −25,
     cursor = time×10 mod 60. Normal alpha, not additive. *)
  let pad = 50. and size = 200. in
  let dx = x -. pad and dy = y -. pad in
  match load_named "sel" with
  | Some s ->
      set_float s "size" size;
      set_float s "cursor" (Float.rem (R.get_time () *. 10.) 60.);
      R.begin_shader_mode s.sh;
      let src = R.Rectangle.create 0. 0. 1. 1. in
      let dst = R.Rectangle.create (sx dx) (sy dy) (size *. !scale) (size *. !scale) in
      R.draw_texture_pro (white_tex ()) src dst (R.Vector2.create 0. 0.) 0. (rgb 1. 1. 1. 1.);
      R.end_shader_mode ()
  | None ->
      let cx = x +. (w *. 0.5) and cy = y +. (h *. 0.5) in
      let hw = (w *. 0.5) +. 4. and hh = (h *. 0.5) +. 4. in
      let spin = R.get_time () *. 2.2 in
      let n = 28 in
      for i = 0 to n - 1 do
        let u = (float i /. float n) +. (spin /. (Float.pi *. 2.)) in
        let u = u -. floor u in
        let ang = u *. Float.pi *. 2. in
        let c, s = (cos ang, sin ang) in
        let px = cx +. (c *. hw *. 1.05) and py = cy +. (s *. hh *. 1.05) in
        let spoke = 0.35 +. (0.55 *. (0.5 +. (0.5 *. cos ((ang *. 8.) -. spin)))) in
        circle px py 2.4 (rgb 0.9 0.95 1. spoke)
      done

(* ---------------------------------------------------------------- shakes *)

type shake = { pid : player_id; num : int; start : float; duration : float; strength : float }

let shakes : shake list ref = ref []

let add_shake pid num ~start ~duration ~strength =
  shakes := { pid; num; start; duration; strength } :: !shakes

let slot_offset pid num =
  let now = R.get_time () in
  shakes := List.filter (fun s -> now < s.start +. s.duration) !shakes;
  match List.find_opt (fun s -> s.pid = pid && s.num = num && now >= s.start) !shakes with
  | None -> (0., 0.)
  | Some s ->
      let percent = clamp01 ((now -. s.start) /. s.duration) in
      let ramp = percent *. percent in
      let freq = 10. +. (ramp *. (28. +. (42. *. s.strength))) in
      let amp = (0.7 +. (ramp *. 6.2)) *. s.strength in
      let phase = percent *. s.duration *. freq in
      (sin (phase *. 2.17) *. amp, cos (phase *. 3.41) *. amp *. 0.42)

(* A rematch starts from a clean board: drop the previous game's overlay
   effects (the victory banner and portrait halo never expire) and shakes. *)
let clear () =
  active := [];
  scroll_y := 0.;
  shakes := []

(* ---------------------------------------------------------------- styles *)

let points_of look ~own ~foe ~own_life ~target =
  match look.Spell_looks.aim with
  | Spell_looks.All_slots -> Array.append own foe
  | Spell_looks.Foe_slots -> foe
  | Spell_looks.Own_slots -> own
  | Spell_looks.Own_life -> [| own_life |]
  | Spell_looks.Target -> (
      match target with Some p -> [| p |] | None -> if Array.length foe > 2 then [| foe.(2) |] else own)
  | Spell_looks.Own_target -> (
      match target with Some p -> [| p |] | None -> if Array.length own > 4 then [| own.(4) |] else own)

let style_duration look = if look.Spell_looks.style = Spell_looks.Stones then 1.5 else 1.05

let additive_style = function
  | Spell_looks.Stones | Spell_looks.Sprout | Spell_looks.Eyes | Spell_looks.Spikes -> false
  | _ -> true

(* An angular chunk of stone: a ragged polygon, tumbling, with a lit top-left
   facet and a shadowed bottom-right one so it reads as rock, not a ball. *)
let rock ?(tint = (0.42, 0.38, 0.34)) ~seed x y rad ang a =
  let tr, tg, tb = tint in
  let nv = 6 + (seed mod 3) in
  let vx = Array.make nv 0. and vy = Array.make nv 0. in
  for v = 0 to nv - 1 do
    let th = ang +. (float v /. float nv *. Float.pi *. 2.) +. ((noise seed v 3 -. 0.5) *. 0.5) in
    let r = rad *. (0.6 +. (0.5 *. noise seed v 5)) in
    vx.(v) <- x +. (cos th *. r *. 1.15);
    vy.(v) <- y +. (sin th *. r *. 0.9)
  done;
  let facet shade k =
    let k2 = (k + 1) mod nv in
    tri x y vx.(k) vy.(k) vx.(k2) vy.(k2) (rgb (tr *. shade) (tg *. shade) (tb *. shade) a)
  in
  for k = 0 to nv - 1 do
    let mx = (vx.(k) +. vx.((k + 1) mod nv)) /. 2. -. x and my = (vy.(k) +. vy.((k + 1) mod nv)) /. 2. -. y in
    (* light from the upper left *)
    let lit = ((-.mx -. my) /. (rad *. 1.4)) in
    facet (Float.max 0.35 (Float.min 1.45 (0.85 +. (0.6 *. lit)))) k
  done

let leaf x y ang len fade r g b =
  if len >= 1.5 then
    let tip_x = x +. (cos ang *. len) and tip_y = y +. (sin ang *. len) in
    let wide = len *. 0.42 in
    let nx = -.sin ang *. wide and ny = cos ang *. wide in
    tri x y (tip_x +. nx) (tip_y +. ny) (tip_x -. nx) (tip_y -. ny) (rgb r g b fade)

let sprout_at x y open_ fade i =
  let h = (15. +. (float (i mod 2) *. 5.)) *. Float.min 1. (open_ *. 1.35) in
  if h > 2. then line x y x (y -. h) 2.3 (rgb 0.16 0.4 0.12 fade);
  let spread = open_ *. open_ in
  if spread > 0.08 then (
    let right = 1.35 -. (spread *. 1.0) in
    leaf x (y -. (h *. 0.42)) (-.right) (12. *. spread) fade 0.2 0.62 0.16;
    leaf x (y -. (h *. 0.42)) (-.(Float.pi -. right)) (12. *. spread) fade 0.15 0.52 0.12;
    let upper = 1.15 -. (spread *. 0.55) in
    leaf x (y -. (h *. 0.7)) (-.upper) (8. *. spread) fade 0.4 0.78 0.22;
    leaf x (y -. (h *. 0.7)) (-.(Float.pi -. upper)) (8. *. spread) fade 0.32 0.68 0.18);
  if open_ > 0.4 then
    let bud = (open_ -. 0.4) /. 0.6 in
    circle x (y -. h -. 1.5) (2. +. (bud *. 1.8)) (rgb 0.55 0.88 0.28 (fade *. bud))

let tiger_eye cx cy open_ fade =
  let rx = 14. and ry = 2. +. (9. *. open_) in
  ellipse cx cy (rx +. 2.5) (ry +. 2.5) (rgb 0.06 0.04 0.02 (fade *. 0.9));
  ellipse cx cy rx ry (rgb 0.95 0.52 0.04 fade);
  let ix = rx *. 0.58 and iy = ry *. 0.58 in
  ellipse cx cy ix iy (rgb 1. 0.78 0.22 fade);
  if open_ > 0.2 then (
    let pupil_h = ry *. 0.8 in
    ellipse cx cy 2.2 pupil_h (rgb 0.03 0.015 0. fade);
    circle (cx -. 4.) (cy -. (ry *. 0.22)) 1.8 (rgb 1. 0.96 0.8 (fade *. 0.95)))

let draw_style look ~own ~foe ~own_life ~foe_life ~target ~age fade =
  let pts = points_of look ~own ~foe ~own_life ~target in
  let t = age in
  let lr, lg, lb, pow = (look.r, look.g, look.b, look.power) in
  match look.style with
  | Spell_looks.Stones ->
      let tint = (lr *. 1.05, lg *. 1.05, lb *. 1.05) in
      Array.iteri
        (fun i p ->
          for k = 0 to 8 do
            let seed = (i * 16) + k in
            let n = noise i k 4 in
            (* a few heavy chunks, mostly medium rocks and small flying shards *)
            let size = if k = 0 then 13. else if k < 4 then 8.5 else 4.5 in
            let rad = (size +. (n *. 3.)) *. pow in
            let delay = (float i *. 0.025) +. (noise i k 7 *. 0.5) in
            let age = t -. delay in
            if age > 0. then (
              let fall_t = 0.3 +. (n *. 0.12) in
              let ground = p.y -. (noise i k 12 *. 8.) in
              let x1 = p.x +. ((noise i k 9 -. 0.5) *. 58.) in
              let drift = 26. +. (noise i k 13 *. 24.) in
              let u = Float.min 1. (age /. fall_t) in
              if u < 1. then (
                (* fast, accelerating drop on a slant, tumbling *)
                let fall = u ** 1.7 in
                let x = x1 -. (drift *. (1. -. fall)) in
                let y = ground -. ((200. +. (n *. 90.)) *. (1. -. fall)) in
                let ang = (age *. (6. +. (n *. 10.)) *. (if k mod 2 = 0 then 1. else -1.)) +. (n *. 6.) in
                (* motion streak so it feels heavy and fast *)
                line (x -. (drift *. 0.35)) (y -. 34.) x y (rad *. 0.8) (rgb 0.55 0.5 0.44 (0.16 *. fade));
                rock ~tint ~seed x y rad ang fade)
              else (
                let ia = age -. fall_t in
                (* impact: the rock bursts into chips, dust rolls out; heavy ones leave rubble *)
                if k < 4 && ia < 0.7 then
                  rock ~tint ~seed:(seed + 50) (x1 +. ((n -. 0.5) *. 8.)) (ground +. 2.) (rad *. 0.55) (n *. 3.)
                    (fade *. Float.min 1. ((0.7 -. ia) /. 0.25));
                if ia < 0.4 then (
                  let q = ia /. 0.4 in
                  for c = 0 to 4 do
                    let th = -.(0.25 +. (noise seed c 21 *. 2.6)) in
                    let sp = 60. +. (noise seed c 22 *. 110.) in
                    let cx = x1 +. (cos th *. sp *. ia) in
                    let cy = ground +. (sin th *. sp *. ia) +. (260. *. ia *. ia) in
                    rock ~tint ~seed:(seed + c) cx cy
                      (Float.max 1.6 (rad *. 0.32 *. (1. -. (q *. 0.4))))
                      (ia *. 14. *. (1. +. float c))
                      (fade *. (1. -. q))
                  done;
                  let w = 8. +. (q *. 30. *. (rad /. 9.)) in
                  ellipse x1 (ground -. 2.) w (w *. 0.38) (rgb 0.6 0.55 0.46 ((1. -. q) *. 0.38 *. fade)))))
          done)
        pts
  | Spell_looks.Sprout ->
      let x0 = own_life.x and y0 = own_life.y +. 4. in
      for i = 0 to 2 do
        let age = t -. (float i *. 0.08) in
        if age > 0. then sprout_at (x0 +. (float (i - 1) *. 14.)) y0 (Float.min 1. (age /. 0.4)) fade i
      done
  | Spell_looks.Eyes ->
      let open_ = ease_out (Float.min 1. (t /. 0.3)) in
      let x = own_life.x +. 6. and y = own_life.y -. 16. in
      tiger_eye (x -. 16.) y open_ fade;
      tiger_eye (x +. 16.) y open_ fade
  | Spell_looks.Rise ->
      Array.iteri
        (fun i p ->
          for k = 0 to 6 do
            let born = noise i k 1 *. 0.45 in
            let life = 0.4 +. (noise i k 2 *. 0.35) in
            let age = t -. born in
            if age > 0. && age < life then
              let u = age /. life in
              let x = p.x +. (sin ((t *. 6.) +. float k +. float i) *. 10.) in
              let y = p.y -. (u *. (50. +. (30. *. pow))) in
              let a = (1. -. u) *. (1. -. u) *. fade in
              circle x y (2.4 +. ((1. -. u) *. 2.5)) (rgb lr lg lb (a *. 0.85))
          done;
          circle p.x (p.y -. 6.) (16. *. pow)
            (rgb lr lg lb (fade *. 0.18 *. (0.6 +. (0.4 *. sin ((t *. 8.) +. float i))))))
        pts
  | Spell_looks.Strike ->
      let x0, y0, x1, y1 = (own_life.x, own_life.y, foe_life.x, foe_life.y) in
      let u = Float.min 1. (t /. 0.28) in
      let x = x0 +. ((x1 -. x0) *. u) and y = y0 +. ((y1 -. y0) *. u) in
      if u > 0.02 then (
        line x0 y0 x y 14. (rgb lr lg lb (fade *. 0.35));
        line x0 y0 x y 3.5 (rgb 1. 0.95 0.85 (fade *. 0.8)));
      if u >= 1. then (
        let puff = Float.min 1. ((t -. 0.28) /. 0.35) in
        circle x1 y1 (10. +. (puff *. 28.)) (rgb lr lg lb ((1. -. puff) *. fade *. 0.7));
        circle x1 y1 6. (rgb 1. 0.95 0.9 ((1. -. puff) *. fade)))
  | Spell_looks.Swirl ->
      Array.iteri
        (fun i p ->
          for arm = 0 to 2 do
            let phase = (t *. (7. +. pow)) +. (float arm *. 2.1) +. float i in
            let radius = (16. +. (float arm *. 12.)) *. pow in
            for s = 0 to 6 do
              let ang = phase +. (float s *. 0.42) in
              circle
                (p.x +. (cos ang *. radius))
                (p.y +. (sin ang *. radius *. 0.55))
                3.2
                (rgb lr lg lb (fade *. (0.25 +. (0.1 *. float s))))
            done
          done)
        pts
  | Spell_looks.Ice ->
      Array.iteri
        (fun i p ->
          for k = 0 to 3 do
            let n = noise i k 6 in
            let delay = (float (i mod 6) *. 0.04) +. (float k *. 0.07) in
            let age = t -. delay in
            let fall_t = 0.4 in
            if age > 0. && age < fall_t +. 0.25 then (
              let u = Float.min 1. (age /. fall_t) in
              let ground = p.y -. 10. in
              let y = ground -. ((150. +. (n *. 40.)) *. (1. -. (u *. u))) in
              let x = p.x +. ((n -. 0.5) *. 40.) in
              let a = fade *. if u < 1. then 0.9 else Float.max 0. (1. -. ((age -. fall_t) /. 0.25)) in
              let lean = (n -. 0.5) *. 10. in
              line x y (x +. lean) (y -. 16.) 4. (rgb 0.55 0.8 1. (a *. 0.45));
              line x (y -. 2.) (x +. (lean *. 0.6)) (y -. 14.) 1.6 (rgb 0.9 0.97 1. a))
          done)
        pts
  | Spell_looks.Shadow ->
      Array.iteri
        (fun i p ->
          for k = 0 to 4 do
            let phase = (t *. 3.5) +. (float k *. 1.3) +. float i in
            let h = (20. +. (50. *. pow)) *. (0.5 +. (0.5 *. sin phase)) in
            let x = p.x +. (sin (phase *. 1.4) *. 12.) in
            circle x (p.y -. (h *. 0.45)) (10. +. (4. *. pow)) (rgb lr lg lb (fade *. 0.28));
            circle x (p.y -. h) 5. (rgb 0.15 0.02 0.2 (fade *. 0.45))
          done)
        pts
  | Spell_looks.Rays ->
      Array.iteri
        (fun i p ->
          let pulse = 0.75 +. (0.25 *. sin ((t *. 6.) +. float i)) in
          circle p.x p.y (22. *. pow) (rgb lr lg lb (fade *. 0.22 *. pulse));
          circle p.x p.y 7. (rgb 1. 0.97 0.85 (fade *. 0.35 *. pulse));
          for k = 0 to 5 do
            let ang = (t *. 1.4) +. (float k *. (Float.pi /. 3.)) +. float i in
            let len = (28. +. (10. *. sin ((t *. 5.) +. float k))) *. pow in
            line p.x p.y (p.x +. (cos ang *. len)) (p.y +. (sin ang *. len)) 2.4 (rgb lr lg lb (fade *. 0.4))
          done)
        pts
  | Spell_looks.Drops ->
      Array.iteri
        (fun i p ->
          for k = 0 to 5 do
            let n = noise i k 5 in
            let delay = (float k *. 0.06) +. (n *. 0.15) in
            let age = t -. delay in
            let fall_t = 0.32 +. (n *. 0.1) in
            if age > 0. then (
              let u = Float.min 1. (age /. fall_t) in
              let x = p.x +. ((noise i k 8 -. 0.5) *. 28.) in
              let y = p.y -. ((120. +. (n *. 40.)) *. (1. -. (u *. u))) in
              let a = fade *. if u < 1. then 0.9 else Float.max 0. (1. -. ((age -. fall_t) /. 0.2)) in
              circle x y (3.2 *. pow) (rgb lr lg lb a);
              if u >= 1. && age < fall_t +. 0.18 then
                let puff = (age -. fall_t) /. 0.18 in
                circle x p.y (3. +. (puff *. 10.)) (rgb lr lg lb ((1. -. puff) *. fade *. 0.6)))
          done)
        pts
  | Spell_looks.Fireburst ->
      Array.iteri
        (fun i p ->
          let flick = 0.82 +. (0.18 *. sin ((t *. 23.) +. (float i *. 1.7))) in
          let h = 78. *. pow *. flick in
          for s = 0 to 6 do
            let vv = float s /. 6. in
            let rad = 16. *. pow *. (1. -. (vv *. 0.72)) in
            let x = p.x +. (sin ((t *. 8.) +. float s +. float i) *. (3. +. (6. *. vv))) in
            let y = p.y -. (vv *. h) in
            circle x y rad (rgb 1. (0.28 +. (0.35 *. (1. -. vv))) 0.03 (fade *. 0.32));
            circle x y (rad *. 0.4) (rgb 1. 0.85 0.4 (fade *. 0.4 *. (1. -. vv)))
          done)
        pts
  | Spell_looks.Ring ->
      Array.iteri
        (fun i p ->
          for wave = 0 to 1 do
            let shift = float wave *. 0.28 in
            let u = mod_float (t +. shift) 0.7 /. 0.7 in
            let radius = (8. +. (u *. 36.)) *. pow in
            let a = (1. -. u) *. fade *. 0.75 in
            for k = 0 to 13 do
              let ang = (float k *. (Float.pi *. 2. /. 14.)) +. t in
              circle (p.x +. (cos ang *. radius)) (p.y +. (sin ang *. radius *. 0.7)) 2.6 (rgb lr lg lb a)
            done
          done;
          circle p.x p.y 6. (rgb lr lg lb (fade *. 0.25)))
        pts
  | Spell_looks.Spikes ->
      Array.iteri
        (fun i p ->
          for k = 0 to 2 do
            let n = noise i k 7 in
            let delay = (float i *. 0.035) +. (float k *. 0.07) in
            let grow = ease_out (clamp01 ((t -. delay) /. 0.28)) in
            if grow > 0.04 then (
              let h = (36. +. (float k *. 18.) +. (n *. 10.)) *. pow *. grow in
              let lean = (n -. 0.5) *. (6. +. (float k *. 3.)) in
              let bx = p.x +. (float (k - 1) *. 16.) in
              let tip_x = bx +. lean and tip_y = p.y -. h in
              let half = (5.5 +. (float k *. 1.4)) *. (0.55 +. (0.45 *. grow)) in
              tri (bx -. half) p.y (bx +. half) p.y tip_x tip_y
                (rgb (lr *. 0.35) (lg *. 0.2) (lb *. 0.2) (fade *. 0.9));
              tri
                (bx -. (half *. 0.45))
                (p.y -. 3.)
                (bx +. (half *. 0.22))
                (p.y -. 3.) tip_x tip_y (rgb lr lg lb fade);
              if grow > 0.35 then
                tri (bx -. 1.1)
                  (p.y -. (h *. 0.35))
                  (bx +. 0.7)
                  (p.y -. (h *. 0.35))
                  tip_x tip_y
                  (rgb 1. (0.55 +. (0.2 *. n)) 0.45 (fade *. 0.85)))
          done)
        pts

let spawn_style look ~own ~foe ~own_life ~foe_life ~target ~start =
  let duration = style_duration look in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.22) in
      if fade > 0.02 then
        let paint () = draw_style look ~own ~foe ~own_life ~foe_life ~target ~age fade in
        if additive_style look.style then additive paint else paint ())
