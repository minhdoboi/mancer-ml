(* Spell and ability visuals. Board coordinates, same transform as gui.ml. *)

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
}

let scale = ref 1.
let off_x = ref 0.
let off_y = ref 0.

let set_transform ~scale:s ~off_x:ox ~off_y:oy =
  scale := s;
  off_x := ox;
  off_y := oy

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

let active : t list ref = ref []
let spawn e = active := e :: !active

let draw_all ~now =
  active :=
    List.filter
      (fun e ->
        let age = now -. e.start in
        if age >= 0. && age < e.duration then e.draw ~age;
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

let load () = List.iter (fun n -> ignore (load_named n)) [ "shockwave"; "sel"; "grey" ]

let unload () =
  Hashtbl.iter (fun _ s -> Option.iter (fun s -> R.unload_shader s.sh) s) shaders;
  Hashtbl.clear shaders;
  Hashtbl.iter (fun _ t -> Option.iter R.unload_texture t) textures;
  Hashtbl.clear textures

let with_grey on f =
  match (on, load_named "grey") with
  | true, Some s ->
      R.begin_shader_mode s.sh;
      f ();
      R.end_shader_mode ()
  | _ -> f ()

let draw_sel ~x ~y ~w ~h =
  match load_named "sel" with
  | None -> ()
  | Some s ->
      let size = Float.max w h *. 1.35 in
      let dx = x +. ((w -. size) /. 2.) and dy = y +. ((h -. size) /. 2.) in
      set_float s "size" size;
      set_float s "cursor" (R.get_time () *. 60.);
      R.begin_blend_mode R.BlendMode.Additive;
      R.begin_shader_mode s.sh;
      R.draw_rectangle_rec
        (R.Rectangle.create (sx dx) (sy dy) (size *. !scale) (size *. !scale))
        (rgb 1. 1. 1. 1.);
      R.end_shader_mode ();
      R.end_blend_mode ()

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

let style_duration look = if look.Spell_looks.style = Spell_looks.Stones then 1.15 else 1.05

let additive_style = function
  | Spell_looks.Stones | Spell_looks.Sprout | Spell_looks.Eyes | Spell_looks.Spikes -> false
  | _ -> true

let rock x y rad a =
  circle x y rad (rgb 0.27 0.24 0.21 a);
  circle (x -. (rad *. 0.22)) (y -. (rad *. 0.18)) (rad *. 0.5) (rgb 0.5 0.46 0.4 a);
  circle (x +. (rad *. 0.28)) (y +. (rad *. 0.16)) (rad *. 0.38) (rgb 0.12 0.1 0.09 a)

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
      Array.iteri
        (fun i p ->
          for k = 0 to 1 do
            let n = noise i k 4 in
            let delay = (float i *. 0.03) +. (float k *. 0.11) +. (n *. 0.06) in
            let age = t -. delay in
            if age > 0. then (
              let fall_t = 0.36 +. (n *. 0.1) in
              let u = Float.min 1. (age /. fall_t) in
              let ground = p.y in
              let start = ground -. (130. +. (n *. 80.)) in
              let x = p.x +. ((noise i k 9 -. 0.5) *. 34.) in
              let y =
                if u < 1. then start +. ((ground -. start) *. u *. u)
                else
                  let bounce =
                    sin ((age -. fall_t) *. 16.) *. 5. *. Float.max 0. (1. -. ((age -. fall_t) /. 0.28))
                  in
                  ground -. Float.abs bounce
              in
              let a =
                fade
                *. if age < fall_t +. 0.4 then 1. else Float.max 0. (1. -. ((age -. fall_t -. 0.4) /. 0.2))
              in
              let rad = (7.5 +. (n *. 5.)) *. pow in
              rock x y rad a;
              if u >= 1. && age < fall_t +. 0.3 then
                let puff = (age -. fall_t) /. 0.3 in
                circle x (ground +. 3.)
                  (5. +. (puff *. 18.))
                  (rgb 0.62 0.56 0.46 ((1. -. puff) *. 0.4 *. fade)))
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

(* ---------------------------------------------------------------- lightning *)

type bolt_style = {
  glow : float * float * float;
  body : float * float * float;
  core : float * float * float;
  orb : bool;
}

let electric = { glow = (0.48, 0.22, 1.); body = (0.35, 0.78, 1.); core = (0.95, 0.98, 1.); orb = false }
let frost = { glow = (0.25, 0.55, 1.); body = (0.7, 0.95, 1.); core = (0.92, 1., 1.); orb = false }
let ball = { glow = (0.55, 0.3, 1.); body = (0.45, 0.8, 1.); core = (1., 0.97, 0.85); orb = true }
let spark = { glow = (1., 0.82, 0.12); body = (1., 0.92, 0.28); core = (1., 0.98, 0.82); orb = false }
let drain = { glow = (0.82, 0.88, 1.); body = (0.95, 0.97, 1.); core = (1., 1., 1.); orb = false }
let hop_delay = 0.07
let lightning_hold = 0.7

let jag rng a b generations sway =
  let pts = ref [| a; b |] in
  let disp = ref sway in
  for _ = 1 to generations do
    let old = !pts in
    let next = Array.make ((Array.length old * 2) - 1) a in
    next.(0) <- old.(0);
    let w = ref 1 in
    for i = 0 to Array.length old - 2 do
      let p = old.(i) and q = old.(i + 1) in
      let mx = (p.x +. q.x) *. 0.5 and my = (p.y +. q.y) *. 0.5 in
      let dx = q.x -. p.x and dy = q.y -. p.y in
      let len2 = (dx *. dx) +. (dy *. dy) in
      let mid =
        if len2 > 64. then
          let inv = 1. /. sqrt len2 in
          let off = (Random.State.float rng 1. -. 0.5) *. 2. *. !disp in
          { x = mx +. (-.dy *. inv *. off); y = my +. (dx *. inv *. off) }
        else { x = mx; y = my }
      in
      next.(!w) <- mid;
      next.(!w + 1) <- q;
      w := !w + 2
    done;
    pts := next;
    disp := !disp *. 0.55
  done;
  !pts

let forks rng path span =
  let n = Array.length path in
  if n < 8 || span < 50. then [||]
  else
    let count = if span > 220. then 2 else 1 in
    Array.init count (fun _ ->
        let i = 2 + Random.State.int rng (n - 4) in
        let p = path.(i) and prev = path.(i - 1) in
        let ang =
          atan2 (p.y -. prev.y) (p.x -. prev.x)
          +. (if Random.State.bool rng then 0.85 else -0.85)
          +. ((Random.State.float rng 1. -. 0.5) *. 0.5)
        in
        let bl = Float.min 78. (span *. (0.16 +. Random.State.float rng 0.18)) in
        jag rng p { x = p.x +. (cos ang *. bl); y = p.y +. (sin ang *. bl) } 3 (bl *. 0.4))

type strike = { primary : spot array; secondary : spot array; forks : spot array array }

let build rng a b =
  let len = hypot (b.x -. a.x) (b.y -. a.y) in
  let sway = Float.min 62. (Float.max 14. (len *. 0.22)) in
  let gens = if len > 220. then 6 else if len > 100. then 5 else 4 in
  let primary = jag rng a b gens sway in
  let secondary = if len > 120. then jag rng a b (max 3 (gens - 1)) (sway *. 0.5) else [||] in
  { primary; secondary; forks = forks rng primary len }

let stroke pts width r g b a caps =
  if Array.length pts >= 2 && a > 0.02 && width > 0.4 then (
    let c = rgb r g b a in
    for i = 0 to Array.length pts - 2 do
      let p = pts.(i) and q = pts.(i + 1) in
      let dx = q.x -. p.x and dy = q.y -. p.y in
      if (dx *. dx) +. (dy *. dy) > 0.36 then (
        if caps then circle p.x p.y (width *. 0.5) c;
        line p.x p.y q.x q.y width c)
    done;
    if caps then
      let last = pts.(Array.length pts - 1) in
      circle last.x last.y (width *. 0.5) c)

let hop_alpha ~duration ~index ~t =
  let born = float index *. hop_delay in
  if t < born then 0.
  else
    let age = t -. born in
    let strike = if age < 0.045 then 1. else 0.8 in
    let flick = if (int_of_float (t *. 20.) + index) mod 4 = 0 then 0.4 else 1. in
    let fade = Float.min 1. ((duration -. t) /. 0.22) in
    strike *. flick *. Float.max 0. fade

let paint_strike style strike from_p to_p alpha t =
  if alpha <= 0.02 then ()
  else
    let span = hypot (to_p.x -. from_p.x) (to_p.y -. from_p.y) in
    let glow_w = Float.min 18. (8. +. (span *. 0.015)) in
    let gr, gg, gb = style.glow and br, bg, bb = style.body and cr, cg, cb = style.core in
    stroke strike.secondary (glow_w *. 0.65) gr gg gb (alpha *. 0.28) true;
    stroke strike.primary glow_w gr gg gb (alpha *. 0.42) true;
    Array.iter (fun f -> stroke f (glow_w *. 0.45) gr gg gb (alpha *. 0.3) true) strike.forks;
    stroke strike.secondary 4.2 br bg bb (alpha *. 0.55) false;
    stroke strike.primary 5.2 br bg bb (alpha *. 0.8) false;
    Array.iter (fun f -> stroke f 2.4 br bg bb (alpha *. 0.7) false) strike.forks;
    stroke strike.primary 2. cr cg cb alpha false;
    Array.iter (fun f -> stroke f 1.3 cr cg cb (alpha *. 0.9) false) strike.forks;
    circle to_p.x to_p.y 11. (rgb br bg bb (alpha *. 0.45));
    circle to_p.x to_p.y 4.5 (rgb cr cg cb alpha);
    if style.orb then (
      let u = clamp01 ((t -. 0.02) /. 0.28) in
      let x = from_p.x +. ((to_p.x -. from_p.x) *. u) in
      let y = from_p.y +. ((to_p.y -. from_p.y) *. u) in
      circle x y 18. (rgb gr gg gb (alpha *. 0.55));
      circle x y 7. (rgb cr cg cb alpha))

let lightning_duration n_pts =
  let hops = max 1 (n_pts - 1) in
  (float hops *. hop_delay) +. lightning_hold

let spawn_lightning ?(style = electric) points ~start =
  let pts = Array.of_list points in
  let n = Array.length pts in
  if n < 2 then mk ~start ~duration:0. (fun ~age:_ -> ())
  else
    let hops = Array.init (n - 1) (fun i -> (pts.(i), pts.(i + 1))) in
    let duration = lightning_duration n in
    let rng = Random.State.make_self_init () in
    let frame = ref (-1) in
    let strikes = ref [||] in
    mk ~start ~duration (fun ~age ->
        let next = int_of_float (age /. 0.05) in
        if next <> !frame then (
          frame := next;
          strikes :=
            Array.mapi
              (fun i (a, b) -> if age < float i *. hop_delay then None else Some (build rng a b))
              hops);
        additive (fun () ->
            Array.iteri
              (fun i s ->
                match s with
                | None -> ()
                | Some strike ->
                    let a, b = hops.(i) in
                    paint_strike style strike a b (hop_alpha ~duration ~index:i ~t:age) age)
              !strikes))

(* ---------------------------------------------------------------- particles *)

type spawn_shape = Point | Square of float * float | Line of float * float | Ellipse of float * float
type color_stop = { u : float; r : float; g : float; b : float }
type alpha_stop = { u : float; a : float }

type emitter_cfg = {
  duration : float;
  rate : float;
  life : float * float;
  max_particles : int;
  spawn : spawn_shape;
  ox : float;
  oy : float;
  speed : float * float;
  angle : float * float;
  gravity : float;
  wind : float * float;
  size : float * float;
  colors : color_stop list;
  alphas : alpha_stop list;
  additive : bool;
  texture : string;
}

type puff = {
  mutable px : float;
  mutable py : float;
  vx : float;
  mutable vy : float;
  wind : float;
  born : float;
  life : float;
  size : float;
  cr : float;
  cg : float;
  cb : float;
}

let rec pair_at u (stops : color_stop list) =
  match stops with
  | a :: b :: rest -> if u <= b.u then (a, b) else pair_at u (b :: rest)
  | [ a ] -> (a, a)
  | [] -> ({ u = 0.; r = 1.; g = 1.; b = 1. }, { u = 1.; r = 1.; g = 1.; b = 1. })

let rec alpha_pair_at u (stops : alpha_stop list) =
  match stops with
  | a :: b :: rest -> if u <= b.u then (a, b) else alpha_pair_at u (b :: rest)
  | [ a ] -> (a, a)
  | [] -> ({ u = 0.; a = 1. }, { u = 1.; a = 1. })

let mix_t a_u b_u u = if b_u <= a_u then 0. else (u -. a_u) /. (b_u -. a_u)

let color_at cfg u =
  let a, b = pair_at u cfg.colors in
  let t = mix_t a.u b.u u in
  (lerp a.r b.r t, lerp a.g b.g t, lerp a.b b.b t)

let alpha_at cfg u =
  let a, b = alpha_pair_at u cfg.alphas in
  lerp a.a b.a (mix_t a.u b.u u)

let spawn_puff rng cfg cx cy now =
  let sx, sy =
    match cfg.spawn with
    | Point -> (0., 0.)
    | Square (w, h) -> (Random.State.float rng w -. (w *. 0.5), Random.State.float rng h -. (h *. 0.5))
    | Line (w, h) -> (Random.State.float rng w -. (w *. 0.5), Random.State.float rng h -. (h *. 0.5))
    | Ellipse (w, h) ->
        let ang = Random.State.float rng (Float.pi *. 2.) in
        let k = sqrt (Random.State.float rng 1.) in
        (cos ang *. w *. 0.5 *. k, sin ang *. h *. 0.5 *. k)
  in
  let lo, hi = cfg.angle in
  let ang = (lo +. Random.State.float rng (hi -. lo)) *. Float.pi /. 180. in
  let s0, s1 = cfg.speed in
  let speed = s0 +. Random.State.float rng (Float.max 0. (s1 -. s0)) in
  let l0, l1 = cfg.life in
  let z0, z1 = cfg.size in
  let w0, w1 = cfg.wind in
  {
    px = cx +. cfg.ox +. sx;
    py = cy +. cfg.oy +. sy;
    vx = cos ang *. speed;
    vy = -.sin ang *. speed;
    wind = w0 +. Random.State.float rng (Float.max 0. (w1 -. w0));
    born = now;
    life = l0 +. Random.State.float rng (Float.max 0. (l1 -. l0));
    size = z0 +. Random.State.float rng (Float.max 0. (z1 -. z0));
    cr = 1.;
    cg = 1.;
    cb = 1.;
  }

type emitter = {
  cfg : emitter_cfg;
  rng : Random.State.t;
  cx : float;
  cy : float;
  mutable puffs : puff list;
  mutable emit : float;
  mutable last : float;
}

let make_emitter cfg ~x ~y =
  { cfg; rng = Random.State.make_self_init (); cx = x; cy = y; puffs = []; emit = 0.; last = 0. }

let tick_emitter e ~age =
  let dt = if e.last = 0. then 0.016 else Float.max 0. (age -. e.last) in
  e.last <- age;
  if age < e.cfg.duration then e.emit <- e.emit +. (dt *. e.cfg.rate);
  while e.emit >= 1. && List.length e.puffs < e.cfg.max_particles do
    e.emit <- e.emit -. 1.;
    e.puffs <- spawn_puff e.rng e.cfg e.cx e.cy age :: e.puffs
  done;
  List.iter
    (fun p ->
      p.vy <- p.vy -. (e.cfg.gravity *. dt);
      p.px <- p.px +. ((p.vx +. p.wind) *. dt);
      p.py <- p.py +. (p.vy *. dt))
    e.puffs;
  e.puffs <- List.filter (fun p -> age -. p.born < p.life) e.puffs

let draw_emitter e ~age ~fade =
  tick_emitter e ~age;
  match tex e.cfg.texture with
  | None ->
      List.iter
        (fun p ->
          let u = clamp01 ((age -. p.born) /. p.life) in
          let r, g, b = color_at e.cfg u in
          circle p.px p.py (p.size *. 0.35) (rgb r g b (alpha_at e.cfg u *. fade)))
        e.puffs
  | Some tex ->
      let tw = float (R.Texture.width tex) and th = float (R.Texture.height tex) in
      let src = R.Rectangle.create 0. 0. tw th in
      let paint () =
        List.iter
          (fun p ->
            let u = clamp01 ((age -. p.born) /. p.life) in
            let r, g, b = color_at e.cfg u in
            let a = alpha_at e.cfg u *. fade in
            let size = p.size *. !scale in
            let dest =
              R.Rectangle.create (sx (p.px -. (p.size *. 0.5))) (sy (p.py -. (p.size *. 0.5))) size size
            in
            R.draw_texture_pro tex src dest (R.Vector2.create 0. 0.) 0. (rgb r g b a))
          e.puffs
      in
      if e.cfg.additive then additive paint else paint ()

let spawn_particles cfg ~x ~y ~start =
  let e = make_emitter cfg ~x ~y in
  let duration = Float.min 3. (cfg.duration +. snd cfg.life) in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.2) in
      if fade > 0.02 then draw_emitter e ~age ~fade)

let nature_heal =
  {
    duration = 1.;
    rate = 75.;
    life = (1., 2.);
    max_particles = 200;
    spawn = Square (50., 50.);
    ox = 0.;
    oy = 0.;
    speed = (30., 100.);
    angle = (45., 135.);
    gravity = 0.;
    wind = (0., 0.);
    size = (3., 7.);
    colors = [ { u = 0.; r = 0.21; g = 1.; b = 0.05 } ];
    alphas = [ { u = 0.; a = 0. }; { u = 0.2; a = 1. }; { u = 0.8; a = 0.75 }; { u = 1.; a = 0. } ];
    additive = true;
    texture = "particle";
  }

let faerie_fire =
  {
    duration = 1.;
    rate = 350.;
    life = (0.5, 1.2);
    max_particles = 500;
    spawn = Line (800., 0.);
    ox = 0.;
    oy = 0.;
    speed = (30., 300.);
    angle = (0., 45.);
    gravity = 0.;
    wind = (0., 0.);
    size = (30., 30.);
    colors =
      [
        { u = 0.; r = 0.05; g = 0.36; b = 1. };
        { u = 0.34; r = 1.; g = 0.24; b = 0.05 };
        { u = 0.64; r = 0.11; g = 1.; b = 0.05 };
        { u = 1.; r = 0.83; g = 1.; b = 0.05 };
      ];
    alphas = [ { u = 0.; a = 0. }; { u = 0.12; a = 1. }; { u = 0.6; a = 0.12 }; { u = 1.; a = 0.02 } ];
    additive = true;
    texture = "particle";
  }

let acid_rain =
  {
    duration = 3.;
    rate = 250.;
    life = (0.5, 1.);
    max_particles = 200;
    spawn = Square (500., 100.);
    ox = 250.;
    oy = -70.;
    speed = (30., 300.);
    angle = (-90., -60.);
    gravity = 0.;
    wind = (20., 20.);
    size = (2., 5.);
    colors = [ { u = 0.; r = 0.05; g = 1.; b = 0.11 } ];
    alphas = [ { u = 0.; a = 0. }; { u = 0.2; a = 1. }; { u = 0.8; a = 0.75 }; { u = 1.; a = 0. } ];
    additive = false;
    texture = "particle";
  }

let nether_grasp =
  {
    duration = 1.;
    rate = 300.;
    life = (0.5, 0.5);
    max_particles = 500;
    spawn = Line (600., 30.);
    ox = 0.;
    oy = 0.;
    speed = (30., 300.);
    angle = (45., 135.);
    gravity = 0.;
    wind = (-50., 50.);
    size = (32., 32.);
    colors =
      [
        { u = 0.; r = 0.05; g = 0.07; b = 1. };
        { u = 0.48; r = 0.; g = 0.; b = 0.02 };
        { u = 1.; r = 0.08; g = 0.09; b = 0.98 };
      ];
    alphas = [ { u = 0.; a = 0. }; { u = 0.2; a = 1. }; { u = 0.8; a = 0.75 }; { u = 1.; a = 0. } ];
    additive = true;
    texture = "thin";
  }

let redemption =
  {
    duration = 1.;
    rate = 50.;
    life = (0.4, 0.6);
    max_particles = 200;
    spawn = Ellipse (50., 50.);
    ox = 0.;
    oy = 0.;
    speed = (50., 50.);
    angle = (0., 360.);
    gravity = 0.;
    wind = (-100., 100.);
    size = (32., 32.);
    colors = [ { u = 0.; r = 1.; g = 0.91; b = 0.91 } ];
    alphas = [ { u = 0.; a = 0. }; { u = 0.2; a = 1. }; { u = 0.8; a = 0.75 }; { u = 1.; a = 0. } ];
    additive = false;
    texture = "thin";
  }

let blood_burst =
  {
    duration = 2.;
    rate = 250.;
    life = (0.5, 1.);
    max_particles = 200;
    spawn = Line (0., 100.);
    ox = 0.;
    oy = 0.;
    speed = (30., 300.);
    angle = (0., 180.);
    gravity = -200.;
    wind = (0., 0.);
    size = (10., 10.);
    colors = [ { u = 0.; r = 0.54; g = 0.02; b = 0.02 } ];
    alphas = [ { u = 0.; a = 0. }; { u = 0.21; a = 0.79 }; { u = 1.; a = 0.77 } ];
    additive = true;
    texture = "particle";
  }

let fire_wall = { faerie_fire with colors = [ { u = 0.; r = 1.; g = 0.48; b = 0.05 } ] }

let smoke_cfg =
  {
    duration = 99.;
    rate = 30.;
    life = (2., 2.);
    max_particles = 100;
    spawn = Square (40., 60.);
    ox = 0.;
    oy = 0.;
    speed = (10., 10.);
    angle = (0., 360.);
    gravity = 10.;
    wind = (0., 0.);
    size = (70., 70.);
    colors =
      [
        { u = 0.; r = 0.533; g = 0.537; b = 0.561 };
        { u = 0.546; r = 0.588; g = 0.635; b = 0.659 };
        { u = 1.; r = 0.153; g = 0.165; b = 0.314 };
      ];
    alphas = [ { u = 0.; a = 0.316 }; { u = 0.555; a = 0.982 }; { u = 1.; a = 0.035 } ];
    additive = false;
    texture = "smoke";
  }

module Hover = struct
  let cur : emitter option ref = ref None
  let key = ref (0., 0.)
  let born = ref 0.
  let hide () = cur := None

  let show ~x ~y =
    match !cur with
    | Some _ when !key = (x, y) -> ()
    | _ ->
        key := (x, y);
        born := R.get_time ();
        cur := Some (make_emitter smoke_cfg ~x ~y)

  let draw () = match !cur with None -> () | Some e -> draw_emitter e ~age:(R.get_time () -. !born) ~fade:1.
end

(* ---------------------------------------------------------------- flames *)

let flame_column ~x ~y ~height ~fat ~hot ~alpha ~t ~id ~licks =
  if alpha >= 0.03 && height >= 6. then (
    let bloom = Float.min 34. ((12. *. fat) +. 10.) in
    circle x (y -. 6.) bloom (rgb 1. 0.32 0.02 (alpha *. 0.2));
    for k = 0 to licks - 1 do
      let lean = sin ((t *. 3.2) +. (float id *. 0.73) +. (float k *. 1.4)) *. 0.45 in
      let h =
        height *. (0.78 +. (0.22 *. (0.5 +. (0.5 *. sin ((t *. 11.) +. float id +. (float k *. 2.2))))))
      in
      let steps = max 6 (min 14 (int_of_float (h /. 7.))) in
      let mid = float (licks - 1) *. 0.5 in
      let side = (float k -. mid) *. fat *. 6.5 in
      let phase = (t *. 7.5) +. (float id *. 0.6) +. (float k *. 1.7) in
      let px = Array.make steps 0. and py = Array.make steps 0. and pr = Array.make steps 0. in
      for s = 0 to steps - 1 do
        let vv = float s /. float (steps - 1) in
        let pinch = (1. -. vv) ** 0.62 in
        let wob = sin (phase +. (vv *. 4.6)) *. (2.5 +. (9. *. vv)) in
        px.(s) <- x +. side +. (lean *. vv *. 34.) +. wob;
        py.(s) <- y -. (vv *. h);
        pr.(s) <- fat *. (6.2 +. (11. *. pinch))
      done;
      let paint r g b a rs reach =
        if a >= 0.02 then
          let last =
            if reach >= 0.99 then steps else max 1 (min steps (int_of_float (float steps *. reach)))
          in
          for s = 0 to last - 1 do
            let rad = pr.(s) *. rs in
            if rad > 0.7 then circle px.(s) py.(s) rad (rgb r g b a)
          done
      in
      paint 1. 0.2 0.02 (alpha *. 0.26) 1.2 1.;
      paint 1. 0.5 0.06 (alpha *. 0.38) 0.74 0.84;
      paint 1. 0.88 0.32 (alpha *. 0.48) 0.42 0.58;
      if hot > 0.15 then paint 1. 0.97 0.9 (alpha *. 0.4 *. hot) 0.24 (0.28 +. (0.18 *. hot))
    done)

let spawn_flame_wave bases ~start =
  let xs = Array.map (fun p -> p.x) bases in
  let y = if Array.length bases = 0 then 0. else bases.(0).y in
  let left = if Array.length xs = 0 then 0. else Array.fold_left min xs.(0) xs -. 34. in
  let right = if Array.length xs = 0 then 0. else Array.fold_left max xs.(0) xs +. 34. in
  let span = Float.max 1. (right -. left) in
  let duration = 1.15 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.22) in
      if fade > 0.02 && Array.length xs > 0 then
        additive (fun () ->
            let t = age in
            let front = t /. 0.52 in
            for i = 0 to 13 do
              let u = float i /. 13. in
              let behind = front -. u in
              let ignite = clamp01 ((behind +. 0.03) /. 0.07) in
              let body = exp (-.Float.max 0. (behind -. 0.1) *. 0.5) in
              let heat = ignite *. Float.max (ignite *. 0.4) body in
              if heat > 0.05 then
                let crest = exp (-.(behind *. behind) *. 20.) in
                let flick = 0.9 +. (0.1 *. sin ((t *. 30.) +. float i)) in
                let h = (34. +. (78. *. heat)) *. (0.86 +. (0.55 *. crest)) *. flick in
                flame_column
                  ~x:(left +. (u *. span))
                  ~y ~height:h
                  ~fat:(0.82 +. (0.25 *. heat))
                  ~hot:(Float.min 1. (heat +. (crest *. 0.35)))
                  ~alpha:(fade *. heat) ~t ~id:i ~licks:3
            done;
            let reached = clamp01 front in
            let x_front = left +. (span *. reached) in
            if x_front -. left > 6. then (
              let bed = fade *. Float.min 1. (front *. 3.) in
              line left (y -. 8.) x_front (y -. 8.) 14. (rgb 1. 0.38 0.04 (0.5 *. bed));
              line left (y -. 10.) x_front (y -. 10.) 5. (rgb 1. 0.82 0.28 (0.55 *. bed)));
            circle x_front (y -. 12.)
              (11. +. (4. *. Float.abs (sin (t *. 28.))))
              (rgb 1. 0.96 0.8 (0.7 *. fade));
            for e = 0 to 41 do
              let u = noise e 2 5 in
              let born = u *. 0.52 in
              let life = 0.26 +. (0.2 *. noise e 3 1) in
              let a = t -. born in
              if a > 0. && a < life then
                let p = a /. life in
                let ex = left +. (u *. span) +. ((noise e 8 2 -. 0.5) *. 22.) in
                let ey = y -. 10. -. ((46. +. (80. *. noise e 4 6)) *. p) in
                let col =
                  if noise e 9 3 > 0.4 then rgb 1. 0.92 0.55 ((1. -. p) *. (1. -. p) *. fade)
                  else rgb 1. 0.45 0.05 ((1. -. p) *. (1. -. p) *. fade *. 0.9)
                in
                circle ex ey (1.5 +. ((1. -. p) *. 2.1)) col
            done))

let envelope age rise hold fade =
  if age < 0. then 0.
  else if age < rise then age /. rise
  else if age < rise +. hold then 1.
  else Float.max 0. (1. -. ((age -. rise -. hold) /. fade))

let spawn_inferno ~target ~others ~start =
  let duration = 1.1 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.22) in
      if fade > 0.02 then
        additive (fun () ->
            let t = age and tx = target.x and ty = target.y in
            let env = envelope t 0.07 0.52 0.3 in
            flame_column ~x:tx ~y:ty ~height:(158. *. env) ~fat:1.65 ~hot:1. ~alpha:(fade *. env) ~t ~id:1
              ~licks:5;
            flame_column ~x:tx ~y:ty ~height:(72. *. env) ~fat:2.15 ~hot:1.
              ~alpha:(fade *. env *. 0.9)
              ~t ~id:2 ~licks:4;
            Array.iteri
              (fun i o ->
                let dist = Float.abs (o.x -. tx) in
                let delay = 0.08 +. (dist *. 0.0004) in
                let splash = envelope (t -. delay) 0.08 0.34 0.26 in
                if splash > 0.04 then
                  flame_column ~x:o.x ~y:o.y ~height:(84. *. splash) ~fat:1. ~hot:0.4 ~alpha:(fade *. splash)
                    ~t ~id:(20 + i) ~licks:3)
              others;
            if t < 0.38 then (
              let p = t /. 0.38 in
              let ring = 16. +. (120. *. p) in
              let a = (1. -. p) *. fade in
              for k = 0 to 15 do
                let ang = float k *. (Float.pi *. 2. /. 16.) in
                circle
                  (tx +. (cos ang *. ring))
                  (ty -. 16. +. (sin ang *. ring *. 0.42))
                  (2.2 +. ((1. -. p) *. 2.4))
                  (rgb 1. 0.55 0.08 (a *. 0.65))
              done;
              circle tx (ty -. 18.) (20. *. (1.15 -. (0.5 *. p))) (rgb 1. 0.96 0.82 (a *. 0.75)));
            for s = 0 to 21 do
              let life = 0.32 +. (0.18 *. noise s 1 4) in
              let a = t -. 0.03 in
              if a > 0. && a < life then
                let p = a /. life in
                let ang = -.(Float.pi /. 2.) +. ((noise s 2 8 -. 0.5) *. Float.pi *. 1.35) in
                let speed = 70. +. (150. *. noise s 6 2) in
                circle
                  (tx +. (cos ang *. speed *. a))
                  (ty -. 8. +. (sin ang *. speed *. a) -. (36. *. a))
                  ((2.4 *. (1. -. p)) +. 1.2)
                  (rgb 1. 0.78 0.25 ((1. -. p) *. fade))
            done))

(* ---------------------------------------------------------------- dedicated *)

let spawn_black_mass ~source ~target ~w ~h ~start =
  let duration = 1. in
  mk ~start ~duration (fun ~age ->
      if age < duration then
        for _ = 0 to 8 do
          let px = target.x +. Random.float w and py = target.y +. Random.float h in
          let dx = (source.x -. px) *. 0.05 *. 10. *. age in
          let dy = (source.y -. py) *. 0.05 *. 10. *. age in
          line (px +. dx) (py +. dy) (px +. (dx *. 1.2)) (py +. (dy *. 1.2)) 1.2 (rgb 0.6 0.6 0.6 1.)
        done)

let spawn_poison ~own ~facing ~smoke ~start =
  let duration = 1.2 in
  let dx = facing.x -. own.x and dy = facing.y -. own.y in
  let span = Float.max 36. (hypot dx dy) in
  let nx = dx /. span and ny = dy /. span in
  let px = -.ny and py = nx in
  let stem0x = own.x +. (nx *. span *. 0.28) and stem0y = own.y +. (ny *. span *. 0.28) in
  let bloom_x = own.x +. (nx *. span *. 0.5) and bloom_y = own.y +. (ny *. span *. 0.5) in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.22) in
      if fade > 0.02 then (
        let t = age in
        let grow = ease_out (clamp01 (t /. 0.38)) in
        if grow > 0.02 then (
          let tip_x = stem0x +. ((bloom_x -. stem0x) *. grow) in
          let tip_y = stem0y +. ((bloom_y -. stem0y) *. grow) in
          let steps = 10 in
          let rec stem i x0 y0 =
            if i < steps then (
              let u = float (i + 1) /. float steps in
              let wobble = sin ((u *. 6.2) +. (t *. 2.4)) *. 5.5 *. u in
              let x = stem0x +. ((tip_x -. stem0x) *. u) +. (px *. wobble) in
              let y = stem0y +. ((tip_y -. stem0y) *. u) +. (py *. wobble) in
              let thick = (4.4 -. (u *. 2.2)) *. grow in
              line x0 y0 x y thick (rgb 0.1 0.2 0.07 (fade *. 0.95));
              line
                (x0 +. (px *. 1.2))
                (y0 +. (py *. 1.2))
                (x +. (px *. 1.2))
                (y +. (py *. 1.2))
                (thick *. 0.35)
                (rgb 0.22 0.38 0.1 (fade *. 0.55));
              (if i mod 2 = 1 && grow > 0.35 then
                 let side = if i mod 4 = 1 then 1. else -1. in
                 tri x y
                   (x +. (((px *. 8.) +. (nx *. 3.)) *. side))
                   (y +. (((py *. 8.) +. (ny *. 3.)) *. side))
                   (x +. (nx *. 3.))
                   (y +. (ny *. 3.))
                   (rgb 0.28 0.08 0.1 (fade *. 0.85)));
              stem (i + 1) x y)
          in
          stem 0 stem0x stem0y;
          let open_ = ease_out (clamp01 ((t -. 0.2) /. 0.38)) in
          if open_ > 0.04 then (
            let len = 16. *. open_ in
            let spread = 0.55 +. (open_ *. 0.45) in
            for k = 0 to 5 do
              let ang = (float k *. (Float.pi *. 2. /. 6.)) +. (t *. 0.35) +. atan2 ny nx in
              let twist = ang +. (sin (float k *. 1.7) *. 0.18 *. (1. -. open_)) in
              let tx = tip_x +. (cos twist *. len *. spread) in
              let ty = tip_y +. (sin twist *. len *. spread) in
              let wide = len *. 0.38 in
              let qx = -.sin twist *. wide and qy = cos twist *. wide in
              tri tip_x tip_y (tx +. qx) (ty +. qy) (tx -. qx) (ty -. qy) (rgb 0.16 0.04 0.1 (fade *. 0.95));
              let mx = tip_x +. ((tx -. tip_x) *. 0.55) and my = tip_y +. ((ty -. tip_y) *. 0.55) in
              tri tip_x tip_y
                (mx +. (qx *. 0.55))
                (my +. (qy *. 0.55))
                (mx -. (qx *. 0.55))
                (my -. (qy *. 0.55))
                (rgb 0.42 0.1 0.18 (fade *. 0.8))
            done;
            let bud = 3.2 +. (open_ *. 4.5) in
            circle tip_x tip_y bud (rgb 0.2 0.55 0.08 fade);
            circle tip_x tip_y (bud *. 0.45) (rgb 0.7 0.95 0.25 (fade *. 0.85))));
        if t >= 0.32 then
          additive (fun () ->
              let age = t -. 0.32 in
              Array.iteri
                (fun s p ->
                  let dist = hypot (p.x -. bloom_x) (p.y -. bloom_y) in
                  let local = age -. (dist *. 0.0012) in
                  if local >= 0. then (
                    for k = 0 to 6 do
                      let born = float k *. 0.055 in
                      let life = 0.38 +. (noise s k 3 *. 0.12) in
                      let a = local -. born in
                      if a > 0. && a < life then (
                        let u = a /. life in
                        let wobble = sin ((u *. 9.) +. float k +. float s) *. 10. *. (1. -. u) in
                        let x = bloom_x +. ((p.x -. bloom_x) *. u) +. (px *. wobble) in
                        let y = bloom_y +. ((p.y -. bloom_y) *. u) +. (py *. wobble) in
                        let al = (1. -. u) *. fade *. 0.7 in
                        circle x y (3.4 +. ((1. -. u) *. 3.2)) (rgb 0.45 0.9 0.18 al);
                        circle (x +. 2.) (y +. 1.5) 2.2 (rgb 0.2 0.35 0.06 (al *. 0.7)))
                    done;
                    if local >= 0.22 then
                      let dens = Float.min 1. ((local -. 0.22) /. 0.18) in
                      for k = 0 to 8 do
                        let n = noise s k 7 in
                        let phase = ((local -. 0.22) *. (1.6 +. n)) +. (float k *. 0.9) +. float s in
                        let rad = (10. +. (n *. 16.)) *. dens in
                        let x = p.x +. (sin phase *. rad) in
                        let y = p.y +. (cos (phase *. 0.85) *. rad *. 0.7) -. ((local -. 0.22) *. 8.) in
                        let puff = 7. +. (n *. 9.) +. (sin (phase *. 1.4) *. 2.5) in
                        let a = fade *. dens *. (0.22 +. (0.12 *. (1. -. n))) in
                        circle x y puff (rgb 0.28 0.72 0.12 a);
                        circle
                          (x -. (puff *. 0.2))
                          (y -. (puff *. 0.15))
                          (puff *. 0.45)
                          (rgb 0.55 0.95 0.22 (a *. 0.55))
                      done))
                smoke)))

let bezier a b c d u =
  let o = 1. -. u in
  (o *. o *. o *. a) +. (3. *. o *. o *. u *. b) +. (3. *. o *. u *. u *. c) +. (u *. u *. u *. d)

let spawn_green_flux ~from_ ~to_ ~start =
  let duration = 0.95 in
  let dx = to_.x -. from_.x and dy = to_.y -. from_.y in
  let lift = 18. +. Float.min 28. (Float.abs dx *. 0.08) in
  let c1x = from_.x +. (dx *. 0.28) and c1y = from_.y +. (dy *. 0.18) -. lift in
  let c2x = from_.x +. (dx *. 0.72) and c2y = from_.y +. (dy *. 0.78) -. (lift *. 0.25) in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.16) in
      if fade > 0.02 then
        additive (fun () ->
            for i = 0 to 27 do
              let born = (float (i mod 7) *. 0.028) +. (noise i 1 0 *. 0.02) in
              let travel = 0.48 +. (noise i 2 0 *. 0.14) in
              let a = age -. born in
              if a >= 0. && a <= travel +. 0.08 then (
                let u = Float.min 1. (a /. travel) in
                let side =
                  (noise i 3 0 -. 0.5) *. 7. *. (1. -. u) *. (0.4 +. (0.6 *. sin ((a *. 11.) +. float i)))
                in
                let x = bezier from_.x c1x c2x to_.x u in
                let y = bezier from_.y c1y c2y to_.y u in
                let tx = bezier from_.x c1x c2x to_.x (Float.min 1. (u +. 0.04)) -. x in
                let ty = bezier from_.y c1y c2y to_.y (Float.min 1. (u +. 0.04)) -. y in
                let len = Float.max 0.001 (hypot tx ty) in
                let px = x +. (-.ty /. len *. side) and py = y +. (tx /. len *. side) in
                let al = fade *. Float.min 1. (a /. 0.06) *. Float.min 1. ((travel +. 0.08 -. a) /. 0.08) in
                let rad = 1.3 +. ((1. -. u) *. 0.9) in
                circle px py (rad *. 2.1) (rgb 0.2 0.95 0.35 (al *. 0.35));
                circle px py rad (rgb 0.55 1. 0.55 (al *. 0.85));
                circle px py (rad *. 0.4) (rgb 0.9 1. 0.85 al))
            done;
            let pulse = 0.7 +. (0.3 *. sin (age *. 12.)) in
            circle to_.x to_.y (10. +. (pulse *. 4.)) (rgb 0.35 1. 0.45 (fade *. 0.18 *. pulse));
            circle to_.x to_.y 3.5 (rgb 0.75 1. 0.7 (fade *. 0.28))))

let spawn_phosphor ~from_ ~targets ~start =
  let duration = 0.7 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.16) in
      if fade > 0.02 then
        additive (fun () ->
            let t = age in
            let open_ = Float.min 1. (t /. 0.12) in
            let pulse = 0.7 +. (0.3 *. sin (t *. 18.)) in
            circle from_.x from_.y (22. +. (10. *. open_)) (rgb 0.2 1. 0.35 (fade *. 0.28 *. open_ *. pulse));
            circle from_.x from_.y 7. (rgb 0.7 1. 0.55 (fade *. 0.5 *. open_));
            for k = 0 to 7 do
              let ang = (t *. 2.2) +. (float k *. (Float.pi /. 4.)) in
              let len = (18. +. (8. *. sin ((t *. 9.) +. float k))) *. open_ in
              line from_.x from_.y
                (from_.x +. (cos ang *. len))
                (from_.y +. (sin ang *. len))
                2.2
                (rgb 0.35 1. 0.4 (fade *. 0.45))
            done;
            Array.iteri
              (fun id to_ ->
                let grow = Float.min 1. ((t -. 0.05 -. (float id *. 0.02)) /. 0.16) in
                if grow > 0.02 then (
                  let x1 = from_.x +. ((to_.x -. from_.x) *. grow) in
                  let y1 = from_.y +. ((to_.y -. from_.y) *. grow) in
                  let wobble = sin ((t *. 22.) +. float id) *. 4.5 in
                  let nx = -.to_.y +. from_.y and ny = to_.x -. from_.x in
                  let len = hypot nx ny in
                  let ox = if len < 1. then 0. else nx /. len *. wobble in
                  let oy = if len < 1. then 0. else ny /. len *. wobble in
                  let mx = ((from_.x +. x1) *. 0.5) +. ox and my = ((from_.y +. y1) *. 0.5) +. oy in
                  line from_.x from_.y mx my 7.5 (rgb 0.12 0.85 0.22 (fade *. 0.28));
                  line mx my x1 y1 7.5 (rgb 0.12 0.85 0.22 (fade *. 0.28));
                  line from_.x from_.y mx my 2.6 (rgb 0.45 1. 0.4 (fade *. 0.7));
                  line mx my x1 y1 2.6 (rgb 0.45 1. 0.4 (fade *. 0.7));
                  line from_.x from_.y mx my 1.1 (rgb 0.9 1. 0.75 fade);
                  line mx my x1 y1 1.1 (rgb 0.9 1. 0.75 fade);
                  if grow > 0.85 then (
                    let hit = (grow -. 0.85) /. 0.15 in
                    circle to_.x to_.y (8. +. (hit *. 16.)) (rgb 0.4 1. 0.35 (fade *. (1. -. hit) *. 0.55));
                    circle to_.x to_.y 4. (rgb 0.85 1. 0.7 (fade *. (1. -. hit))))))
              targets))

let spawn_bats ~from_ ~to_ ~start =
  let duration = 1.02 in
  let dx = to_.x -. from_.x and dy = to_.y -. from_.y in
  let lift = 42. +. Float.min 36. (Float.abs dx *. 0.12) in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.14) in
      if fade > 0.02 then
        for i = 0 to 11 do
          let born = (float (i mod 5) *. 0.045) +. (noise i 1 0 *. 0.04) in
          let life = 0.62 +. (noise i 2 0 *. 0.22) in
          let a = age -. born in
          if a >= 0. && a <= life then (
            let u = a /. life in
            let side = (if i mod 2 = 0 then 1. else -1.) *. (8. +. (noise i 3 0 *. 16.)) in
            let cx = bezier from_.x (from_.x +. (dx *. 0.3)) (from_.x +. (dx *. 0.72)) to_.x u in
            let cy = bezier from_.y (from_.y -. lift) (from_.y +. (dy *. 0.55) -. (lift *. 0.35)) to_.y u in
            let px = cx +. (side *. sin (u *. Float.pi)) in
            let py = cy +. ((noise i 4 0 -. 0.5) *. 10.) in
            let tx =
              bezier from_.x
                (from_.x +. (dx *. 0.3))
                (from_.x +. (dx *. 0.72))
                to_.x
                (Float.min 1. (u +. 0.04))
            in
            let ty =
              bezier from_.y (from_.y -. lift)
                (from_.y +. (dy *. 0.55) -. (lift *. 0.35))
                to_.y
                (Float.min 1. (u +. 0.04))
            in
            let ang = atan2 (ty -. py) (tx -. px) in
            let open_ = 0.55 +. (0.45 *. sin ((a *. (14. +. float i)) +. float i)) in
            let span = (9. +. (noise i 5 0 *. 4.)) *. (0.7 +. (0.3 *. (1. -. u))) in
            let appear = Float.min 1. (a /. 0.08) *. Float.min 1. ((life -. a) /. 0.1) *. fade in
            let fx = cos ang and fy = sin ang in
            let lx = -.fy and ly = fx in
            let spread = 0.55 +. (0.7 *. open_) in
            let tip = span *. 0.55 in
            tri
              (px -. (fx *. 2.))
              (py -. (fy *. 2.))
              (px +. (fx *. tip *. 0.15) +. (lx *. span *. spread))
              (py +. (fy *. tip *. 0.15) +. (ly *. span *. spread))
              (px +. (fx *. tip *. 0.55))
              (py +. (fy *. tip *. 0.55))
              (rgb 0.04 0.03 0.06 (appear *. 0.92));
            tri
              (px -. (fx *. 2.))
              (py -. (fy *. 2.))
              (px +. (fx *. tip *. 0.15) -. (lx *. span *. spread))
              (py +. (fy *. tip *. 0.15) -. (ly *. span *. spread))
              (px +. (fx *. tip *. 0.55))
              (py +. (fy *. tip *. 0.55))
              (rgb 0.04 0.03 0.06 (appear *. 0.92));
            circle px py 2.1 (rgb 0.12 0.08 0.14 appear);
            circle (px +. (fx *. 1.4)) (py +. (fy *. 1.4)) 0.9 (rgb 0.7 0.12 0.12 (appear *. 0.7)))
        done)

let spawn_harvest ~from_ ~to_ ~souls ~start =
  let duration = 0.95 in
  let wisps = min 8 (3 + souls - 1) in
  let dx = to_.x -. from_.x and dy = to_.y -. from_.y in
  let lift = 55. +. Float.min 40. (Float.abs dx *. 0.08) in
  let c1x = from_.x +. (dx *. 0.22) and c1y = from_.y +. (dy *. 0.12) -. lift in
  let c2x = from_.x +. (dx *. 0.72) and c2y = from_.y +. (dy *. 0.88) -. (lift *. 0.35) in
  let cx u = bezier from_.x c1x c2x to_.x u in
  let cy u = bezier from_.y c1y c2y to_.y u in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.18) in
      if fade > 0.02 then (
        let puff = Float.min 1. (age /. 0.22) in
        let a = (1. -. puff) *. fade *. 0.45 in
        if a > 0.03 then (
          circle from_.x (from_.y -. (puff *. 18.)) (8. +. (puff *. 22.)) (rgb 0.55 0.7 0.85 a);
          circle (from_.x +. 6.)
            (from_.y -. (puff *. 10.))
            (6. +. (puff *. 14.))
            (rgb 0.35 0.2 0.55 (a *. 0.7)));
        additive (fun () ->
            let arrived = ref false in
            for i = 0 to wisps - 1 do
              let delay = float i *. 0.045 in
              let a = age -. delay in
              let travel = 0.62 +. (float (i mod 3) *. 0.04) in
              if a > 0. then (
                let raw = Float.min 1. (a /. travel) in
                let u = raw *. raw in
                let n = noise i 3 7 in
                let wobble = sin ((a +. float i) *. 9.) *. (8. *. (1. -. u)) in
                let px = cx u +. (wobble *. (n -. 0.5)) in
                let py = cy u +. (cos ((a +. float i) *. 7.) *. (6. *. (1. -. u))) in
                let vx = cx (Float.min 1. (u +. 0.04)) -. px in
                let vy = cy (Float.min 1. (u +. 0.04)) -. py in
                let len = Float.max 0.001 (hypot vx vy) in
                let tx = vx /. len and ty = vy /. len in
                for s = 1 to 5 do
                  let tu = Float.max 0. (u -. (float s *. 0.035)) in
                  circle
                    (cx tu -. (tx *. float s *. 4.))
                    (cy tu -. (ty *. float s *. 4.))
                    (5.5 -. (float s *. 0.6))
                    (rgb 0.45 0.75 1. (fade *. (1. -. (float s /. 6.)) *. (0.18 +. (0.2 *. (1. -. u)))))
                done;
                let size = (1. -. (u *. 0.25)) *. (10. +. (float (i mod 2) *. 2.)) in
                let al = fade *. (0.55 +. (0.25 *. (1. -. u))) in
                circle
                  (px -. (tx *. size *. 1.8))
                  (py -. (ty *. size *. 1.8))
                  (size *. 0.95)
                  (rgb 0.35 0.12 0.7 (al *. 0.45));
                circle px py (size *. 0.72) (rgb 0.55 0.82 1. (al *. 0.7));
                circle (px +. (tx *. 2.)) (py +. (ty *. 2.)) (size *. 0.32) (rgb 0.9 0.97 1. al);
                if size *. 0.18 > 0.8 && u < 0.85 then (
                  let ox = -.ty and oy = tx in
                  circle
                    (px +. (ox *. 2.2))
                    (py +. (oy *. 2.2))
                    (size *. 0.18)
                    (rgb 0.15 0.35 0.55 (al *. 0.85));
                  circle
                    (px -. (ox *. 2.2))
                    (py -. (oy *. 2.2))
                    (size *. 0.18)
                    (rgb 0.15 0.35 0.55 (al *. 0.85)));
                if u > 0.92 then arrived := true)
            done;
            if !arrived then (
              let pulse = 0.5 +. (0.5 *. sin (age *. 18.)) in
              circle to_.x to_.y (18. +. (pulse *. 8.)) (rgb 0.5 0.8 1. (fade *. 0.35 *. pulse));
              circle to_.x to_.y 7. (rgb 0.85 0.95 1. (fade *. 0.55))))))

(* ---------------------------------------------------------------- madden / shockwave / victory *)

let madden_duration = 1.28

let spawn_madden ~foes ~allies ~eye ~start =
  List.iter (fun (pid, n, _) -> add_shake pid n ~start ~duration:madden_duration ~strength:1.) foes;
  List.iter (fun (pid, n, _) -> add_shake pid n ~start ~duration:madden_duration ~strength:0.38) allies;
  let foe_pts = Array.of_list (List.map (fun (_, _, p) -> p) foes) in
  let ally_pts = Array.of_list (List.map (fun (_, _, p) -> p) allies) in
  mk ~start ~duration:madden_duration (fun ~age ->
      let fade = Float.min 1. ((madden_duration -. age) /. 0.2) in
      if fade > 0.02 then (
        let t = age in
        let open_ = ease_out (clamp01 (t /. 0.28)) in
        let madness = ease_out (clamp01 ((t -. 0.1) /. 0.35)) in
        let ramp = clamp01 (t /. madden_duration) in
        let shake = ramp *. ramp in
        let glitch pts fade_m shake_m id0 =
          Array.iteri
            (fun i p ->
              let fade = fade_m and id = i + id0 in
              let split = (3. +. (shake_m *. 10.)) *. (0.6 +. (0.4 *. sin (t *. (18. +. float id)))) in
              let w = 78. and h = 88. in
              let x = p.x -. (w *. 0.5) and y = p.y -. (h *. 0.48) in
              rect (x +. split) y w h (rgb 0.95 0.08 0.1 (fade *. 0.22));
              rect (x -. split) y w h (rgb 0.05 0.75 0.85 (fade *. 0.16));
              let speed = 1.6 +. (shake_m *. 4.5) in
              let u = mod_float ((t *. speed) +. noise id 3 2) 1. in
              let ty = y +. (u *. h) in
              let band = 5. +. (shake_m *. 7.) in
              rect (x -. 4.) ty (w +. 8.) band (rgb 0.95 0.85 0.9 (fade *. (0.18 +. (0.2 *. shake_m))));
              let n = noise id 8 4 in
              if n > 0.45 then (
                let slice = 8. +. (n *. 16.) in
                let sy = y +. (mod_float ((t *. (2.4 +. float id)) +. n) 1. *. h) in
                rect (x +. ((n -. 0.5) *. 18.)) sy (w *. 0.7) slice (rgb 0.15 0.02 0.04 (fade *. 0.35));
                for k = 0 to 8 do
                  let n = noise id k 6 in
                  let on =
                    0.55 +. (0.45 *. sin ((t *. (30. +. (shake_m *. 40.))) +. (float k *. 1.7) +. float id))
                  in
                  if on > 0.15 then
                    rect x
                      (y +. (float k /. 8. *. h) +. ((n -. 0.5) *. 6.))
                      w
                      (1.4 +. (shake_m *. 1.2))
                      (rgb 0.7 0.08 0.1 (fade *. 0.12 *. on))
                done))
            pts
        in
        glitch foe_pts (fade *. madness) shake 0;
        glitch ally_pts (fade *. madness *. 0.38) (shake *. 0.4) 17;
        if open_ >= 0.04 then (
          let rx = 40. and ry = 4. +. (22. *. open_) in
          let x = eye.x and y = eye.y in
          let a = fade *. (0.88 +. (0.12 *. sin (t *. 9.))) in
          let slit = 1. +. (0.8 *. sin ((t *. 22.) +. 1.2) *. clamp01 ((t -. 0.45) /. 0.1)) in
          ellipse x y (rx +. 10.) (ry +. 8.) (rgb 0.35 0.02 0.04 (a *. 0.55));
          ellipse x y rx ry (rgb 0.55 0.05 0.06 (a *. 0.9));
          ellipse x y (rx *. 0.82) (ry *. 0.72) (rgb 0.72 0.12 0.1 (a *. 0.7));
          let irx = rx *. 0.48 and iry = ry *. 0.78 in
          ellipse x y irx iry (rgb 0.85 0.22 0.05 a);
          ellipse x y (irx *. 0.7) (iry *. 0.7) (rgb 1. 0.55 0.12 (a *. 0.85));
          for k = 0 to 9 do
            let ang = (float k *. (Float.pi /. 5.)) +. (t *. 0.4) in
            let len = irx *. (0.55 +. (0.35 *. noise k 2 1)) in
            line x y
              (x +. (cos ang *. len))
              (y +. (sin ang *. len *. (iry /. Float.max irx 1.)))
              1.6
              (rgb 0.35 0.04 0.02 (a *. 0.55))
          done;
          ellipse x y (5.2 *. slit) (iry *. 0.72) (rgb 0.04 0.01 0.01 a);
          circle (x -. 7.) (y -. (ry *. 0.18)) 2.4 (rgb 1. 0.85 0.45 (a *. 0.55)));
        additive (fun () ->
            if open_ >= 0.08 then (
              let pulse = 0.55 +. (0.45 *. sin (t *. 11.)) in
              circle eye.x eye.y (28. +. (10. *. open_)) (rgb 0.9 0.12 0.04 (fade *. 0.18 *. open_ *. pulse));
              circle eye.x eye.y 14. (rgb 1. 0.4 0.08 (fade *. 0.12 *. open_))))))

let spawn_shockwave slots ~start =
  let duration = 0.9 in
  let stretch_y = 1.55 in
  let n = Array.length slots in
  let mid_x, mid_y, max_r, origin_x, origin_y, width, height =
    if n = 0 then (0., 0., 1., 0., 0., 1., 1.)
    else
      let sx = Array.fold_left (fun s p -> s +. p.x) 0. slots /. float n in
      let sy = Array.fold_left (fun s p -> s +. p.y) 0. slots /. float n in
      let xmin = Array.fold_left (fun m p -> min m p.x) slots.(0).x slots in
      let xmax = Array.fold_left (fun m p -> max m p.x) slots.(0).x slots in
      let span = Float.max 80. (xmax -. xmin) in
      let reach = (span *. 0.58) +. 90. in
      let w = (reach +. 70.) *. 2. and h = ((reach /. stretch_y) +. 70.) *. 2. in
      (sx, sy, reach, sx -. (w *. 0.5), sy -. (h *. 0.5), w, h)
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.2) in
      if fade > 0.02 && n > 0 then
        match load_named "shockwave" with
        | None ->
            additive (fun () ->
                let u = ease_out (Float.min 1. (age /. 0.68)) in
                circle mid_x mid_y (u *. max_r) (rgb 0.96 0.68 0.26 (fade *. 0.25)))
        | Some s ->
            let u = ease_out (Float.min 1. (age /. 0.68)) in
            let radius = u *. max_r in
            let u2 = ease_out (clamp01 ((age -. 0.14) /. 0.62)) in
            let radius2 = if age <= 0.14 then 0. else u2 *. max_r *. 0.92 in
            let flash = Float.max 0. (1. -. (age /. 0.16)) in
            let width_ring = 14. +. (10. *. (1. -. u)) in
            set_vec2 s "u_size" width height;
            set_vec2 s "u_center" (width *. 0.5) (height *. 0.5);
            set_vec2 s "u_stretch" 1. stretch_y;
            set_float s "u_radius" radius;
            set_float s "u_radius2" radius2;
            set_float s "u_width" width_ring;
            set_float s "u_fade" fade;
            set_float s "u_flash" flash;
            R.begin_blend_mode R.BlendMode.Additive;
            R.begin_shader_mode s.sh;
            R.draw_rectangle_rec
              (R.Rectangle.create (sx origin_x) (sy origin_y) (width *. !scale) (height *. !scale))
              (rgb 1. 1. 1. 1.);
            R.end_shader_mode ();
            R.end_blend_mode ())

type confetti = {
  x0 : float;
  y0 : float;
  vx : float;
  vy : float;
  spin : float;
  rot0 : float;
  w : float;
  h : float;
  r : float;
  g : float;
  b : float;
  born : float;
  life : float;
}

let confetti_colors =
  [|
    (0.95, 0.20, 0.22);
    (0.18, 0.62, 0.95);
    (0.22, 0.82, 0.34);
    (0.98, 0.80, 0.16);
    (0.95, 0.38, 0.78);
    (0.96, 0.96, 0.96);
    (0.68, 0.32, 0.95);
    (1.00, 0.52, 0.12);
  |]

let spawn_confetti (slots : spot array) ~start =
  let rng = Random.State.make [| 0xC0FFEE |] in
  let piece sx sy born =
    let r, g, b = confetti_colors.(Random.State.int rng (Array.length confetti_colors)) in
    let strip = Random.State.bool rng in
    {
      x0 = sx +. ((Random.State.float rng 1. -. 0.5) *. 70.);
      y0 = sy +. ((Random.State.float rng 1. -. 0.2) *. 36.);
      vx = (Random.State.float rng 1. -. 0.5) *. 260.;
      vy = 160. +. Random.State.float rng 260.;
      spin = (Random.State.float rng 1. -. 0.5) *. 14.;
      rot0 = Random.State.float rng 6.28;
      w = (if strip then 3.2 +. Random.State.float rng 2.2 else 5.5 +. Random.State.float rng 3.5);
      h = (if strip then 9. +. Random.State.float rng 7. else 4.5 +. Random.State.float rng 3.);
      r;
      g;
      b;
      born;
      life = 1.6 +. Random.State.float rng 1.3;
    }
  in
  let pieces =
    Array.concat
      (Array.to_list
         (Array.mapi
            (fun i s ->
              Array.append
                (Array.init 18 (fun k -> piece s.x s.y ((0.012 *. float k) +. (0.01 *. float i))))
                (Array.init 6 (fun k -> piece s.x s.y (0.18 +. (0.07 *. float k) +. (0.02 *. float i)))))
            slots))
  in
  let duration = 3.4 and gravity = 460. in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.35) in
      if fade > 0.02 then
        if age <= 0.28 then (
          let p = age /. 0.28 in
          let a = (1. -. p) *. fade *. 0.55 in
          Array.iter
            (fun s ->
              circle s.x (s.y -. 8.) (22. +. (38. *. p)) (rgb 1. 0.86 0.3 (a *. 0.35));
              circle s.x (s.y -. 8.) (8. +. (10. *. (1. -. p))) (rgb 1. 0.95 0.7 a))
            slots;
          Array.iter
            (fun p ->
              let a = age -. p.born in
              if a >= 0. && a <= p.life then
                let drag = exp (-.a *. 0.55) in
                let x = p.x0 +. (p.vx *. a *. drag) in
                let y = p.y0 -. (p.vy *. a) +. (0.5 *. gravity *. a *. a) in
                let rot = p.rot0 +. (p.spin *. a) in
                let tail =
                  if a > p.life *. 0.62 then 1. -. ((a -. (p.life *. 0.62)) /. (p.life *. 0.38)) else 1.
                in
                let al = fade *. tail in
                if al >= 0.03 then (
                  let c = cos rot and s = sin rot in
                  let hw = p.w *. 0.5 and hh = p.h *. 0.5 in
                  let x1 = x +. ((-.hw *. c) -. (-.hh *. s)) and y1 = y +. ((-.hw *. s) +. (-.hh *. c)) in
                  let x2 = x +. ((hw *. c) -. (-.hh *. s)) and y2 = y +. ((hw *. s) +. (-.hh *. c)) in
                  let x3 = x +. ((hw *. c) -. (hh *. s)) and y3 = y +. ((hw *. s) +. (hh *. c)) in
                  let x4 = x +. ((-.hw *. c) -. (hh *. s)) and y4 = y +. ((-.hw *. s) +. (hh *. c)) in
                  tri x1 y1 x2 y2 x3 y3 (rgb p.r p.g p.b al);
                  tri x1 y1 x3 y3 x4 y4 (rgb p.r p.g p.b al)))
            pieces))

let spawn_banner ~msg ~(at : spot) ~won ~start =
  let duration = 2.4 in
  mk ~start ~duration (fun ~age ->
      let appear = Float.min 1. (age /. 0.12) in
      let scale_k =
        if age < 0.28 then lerp 0.2 1.7 (swing_out (age /. 0.28))
        else if age < 0.44 then lerp 1.7 1.25 (ease_out_quad ((age -. 0.28) /. 0.16))
        else 1.25
      in
      let fade = Float.min appear (Float.min 1. ((duration -. age) /. 0.3)) in
      if fade > 0.02 then (
        let w = 220. *. scale_k and h = 36. *. scale_k in
        rect (at.x -. (w *. 0.5)) (at.y -. (h *. 0.5)) w h (rgb 0.04 0.03 0.02 (0.62 *. fade));
        let size = 18. *. scale_k *. !scale in
        let tw = R.Vector2.x (R.measure_text_ex (R.get_font_default ()) msg size 0.) in
        R.draw_text_ex (R.get_font_default ()) msg
          (R.Vector2.create (sx at.x -. (tw *. 0.5)) (sy at.y -. (size *. 0.45)))
          size 0.
          (if won then rgb 1. 0.84 0.28 fade else rgb 0.92 0.92 0.95 fade)))

let spawn_halo ~(center : spot) ~half ~start =
  let duration = 8. in
  mk ~start ~duration (fun ~age ->
      let appear = Float.min 1. (age /. 0.28) in
      if appear > 0.02 then
        additive (fun () ->
            let pulse = 0.9 +. (0.1 *. sin (age *. 2.6)) in
            let a = appear *. Float.min 1. ((duration -. age) /. 0.4) in
            let inner = half +. 2.5 in
            circle center.x center.y (inner +. (15. *. pulse)) (rgb 1. 0.88 0.45 (0.14 *. a));
            circle center.x center.y (inner +. 8.) (rgb 1. 0.95 0.7 (0.22 *. a));
            circle center.x center.y (inner +. 3.2) (rgb 1. 1. 0.94 (0.34 *. a))))

let victory layout ~winner ~you =
  let now = R.get_time () in
  let own = slots_of owner layout.slot_center in
  let foe = slots_of opponent layout.slot_center in
  let between : spot = { x = (own.(2).x +. own.(3).x) *. 0.5; y = (own.(0).y +. foe.(0).y) *. 0.5 } in
  let won = match winner with Some w -> w = owner = you | None -> false in
  let msg =
    match winner with
    | None -> "DRAW"
    | Some w when you && w = owner -> "YOU WON"
    | Some _ when you -> "YOU LOSE"
    | Some w -> Printf.sprintf "P%d WINS" (w + 1)
  in
  spawn (spawn_banner ~msg ~at:between ~won ~start:now);
  Option.iter (fun w -> spawn (spawn_halo ~center:(layout.life w) ~half:42. ~start:now)) winner;
  if won then spawn (spawn_confetti own ~start:now)

(* ---------------------------------------------------------------- stage helpers *)

let own_foe layout player = (slots_of player layout.slot_center, slots_of (other player) layout.slot_center)

let oppress_foe layout player =
  let foe = other player in
  match layout.scarecrow player with
  | None -> slots_of foe layout.slot_center
  | Some n ->
      List.filter in_slot_range [ n - 1; n; n + 1 ] |> Array.of_list |> Array.map (layout.slot_center foe)

let source_or_life layout (c : command) =
  Option.value (layout.source_of c) ~default:(layout.life c.cmd_player)

let target_spot layout (c : command) =
  match (c.cmd_input, Table.target_side c) with
  | Some n, Some pid -> Some (layout.slot_center pid n)
  | _ -> None

let look_recipe layout (c : command) look =
  let own, foe0 = own_foe layout c.cmd_player in
  let foe = if c.cmd_card.key = "disciples.wind" then oppress_foe layout c.cmd_player else foe0 in
  let foe =
    if c.cmd_card.key = "shadowmage.spike" then slots_of (other c.cmd_player) layout.slot_floor else foe
  in
  let own_life = layout.life c.cmd_player and foe_life = layout.life (other c.cmd_player) in
  let target = target_spot layout c in
  let duration = style_duration look in
  recipe ~duration ~blocking:true (fun ~start ->
      spawn_style look ~own ~foe ~own_life ~foe_life ~target ~start)

let for_spell layout (c : command) =
  let player = c.cmd_player in
  let other = other player in
  let source = source_or_life layout c in
  let target = target_spot layout c in
  let foe_life = layout.life other in
  match c.cmd_card.key with
  | "air.call" ->
      Option.map
        (fun t ->
          let pts = [ source; t; foe_life ] in
          recipe ~duration:(lightning_duration 3) ~blocking:true (fun ~start -> spawn_lightning pts ~start))
        target
  | "air.lightnin" ->
      Some
        (recipe ~duration:(lightning_duration 2) ~blocking:true (fun ~start ->
             spawn_lightning [ source; foe_life ] ~start))
  | "air.chain" ->
      let pts =
        List.rev
          (foe_life
          :: List.init nb_slots (fun i ->
              let p = layout.slot_center other i in
              { p with y = p.y +. (20. *. float (i mod 2)) }))
      in
      Some
        (recipe
           ~duration:(lightning_duration (List.length pts))
           ~blocking:true
           (fun ~start -> spawn_lightning pts ~start))
  | "wind.BallLightning.name" ->
      Some
        (recipe ~duration:(lightning_duration 2) ~blocking:true (fun ~start ->
             spawn_lightning ~style:ball [ source; foe_life ] ~start))
  | "fire.wave" ->
      Some
        (recipe ~duration:1.15 ~blocking:true (fun ~start ->
             spawn_flame_wave (slots_of other layout.flame_base) ~start))
  | "fire.inferno" ->
      Option.map
        (fun n ->
          let bases = slots_of other layout.flame_base in
          let others = Array.init (nb_slots - 1) (fun i -> bases.(if i < n then i else i + 1)) in
          recipe ~duration:1.1 ~blocking:true (fun ~start -> spawn_inferno ~target:bases.(n) ~others ~start))
        c.cmd_input
  | "disciples.madden" ->
      let foe_occ = layout.occupied other in
      let foe_row = slots_of other layout.slot_center in
      let own_row = slots_of player layout.slot_center in
      let eye =
        {
          x = Array.fold_left (fun s p -> s +. p.x) 0. foe_row /. float nb_slots;
          y = (foe_row.(0).y +. own_row.(0).y) *. 0.5;
        }
      in
      let nums = if foe_occ = [] then List.init nb_slots Fun.id else foe_occ in
      let foe_ids = List.map (fun n -> (other, n, layout.slot_center other n)) nums in
      let ally_ids = List.map (fun n -> (player, n, layout.slot_center player n)) (layout.occupied player) in
      Some
        (recipe ~duration:madden_duration ~blocking:true (fun ~start ->
             spawn_madden ~foes:foe_ids ~allies:ally_ids ~eye ~start))
  | "earth.care" ->
      Option.map
        (fun t ->
          recipe
            ~duration:(Float.min 3. (nature_heal.duration +. snd nature_heal.life))
            ~blocking:true
            (fun ~start -> spawn_particles nature_heal ~x:t.x ~y:t.y ~start))
        target
  | "faerie.fire" ->
      let p = layout.slot_center other 0 in
      Some
        (recipe
           ~duration:(Float.min 3. (faerie_fire.duration +. snd faerie_fire.life))
           ~blocking:true
           (fun ~start -> spawn_particles faerie_fire ~x:p.x ~y:p.y ~start))
  | "water.shower" ->
      let p = layout.slot_center other 0 in
      Some
        (recipe ~duration:(Float.min 3. acid_rain.duration) ~blocking:true (fun ~start ->
             spawn_particles acid_rain ~x:p.x ~y:p.y ~start))
  | "elementalist.frost" ->
      let mid = match target_spot layout c with Some t -> t | None -> layout.slot_center other 2 in
      Some
        (recipe ~duration:(lightning_duration 3) ~blocking:true (fun ~start ->
             spawn_lightning ~style:frost [ source; mid; foe_life ] ~start))
  | "darkpriest.BlackMass.name" ->
      let origin = match target_spot layout c with Some t -> t | None -> source in
      let corner = layout.slot_center other 0 in
      Some
        (recipe ~duration:1. ~blocking:true (fun ~start ->
             spawn_black_mass ~source:origin ~target:corner ~w:500. ~h:100. ~start))
  | _ -> Option.map (look_recipe layout c) (Spell_looks.get c.cmd_card.key)

let for_ability layout ~kind ~player ~num ~target =
  match kind with
  | "zen.spark" ->
      Option.map
        (fun (tp, tn) ->
          recipe ~duration:(lightning_duration 2) ~blocking:true (fun ~start ->
              spawn_lightning ~style:spark [ layout.slot_center player num; layout.slot_center tp tn ] ~start))
        target
  | "phosphor_rays" | "phosphor.rays" ->
      let foe = match target with Some (tp, _) -> tp | None -> other player in
      let hits =
        match layout.occupied foe with
        | [] -> slots_of foe layout.slot_center
        | ns -> Array.of_list (List.map (layout.slot_center foe) ns)
      in
      Some
        (recipe ~duration:0.7 ~blocking:true (fun ~start ->
             spawn_phosphor ~from_:(layout.slot_center player num) ~targets:hits ~start))
  | "dark_flock_bats" | "vampires.flock" ->
      Option.map
        (fun (_, dest) ->
          recipe ~duration:1.02 ~blocking:false (fun ~start ->
              spawn_bats ~from_:(layout.slot_center player num) ~to_:(layout.slot_center player dest) ~start))
        target
  | "soul_taken" | "soul.taken" ->
      let dead_p, slot = match target with Some t -> t | None -> (other player, num) in
      Some
        (recipe ~duration:0.95 ~blocking:false (fun ~start ->
             spawn_harvest ~from_:(layout.slot_center dead_p slot) ~to_:(layout.life player) ~souls:1 ~start))
  | _ -> None

let demo_names =
  [
    "stones";
    "sprout";
    "eyes";
    "rise";
    "strike";
    "swirl";
    "ice";
    "shadow";
    "rays";
    "drops";
    "fireburst";
    "ring";
    "spikes";
    "lightning";
    "ball";
    "flames";
    "inferno";
    "heal";
    "faerie";
    "rain";
    "madden";
    "shockwave";
    "bats";
    "harvest";
    "phosphor";
    "victory";
    "blackmass";
    "poison";
    "flux";
    "nether";
    "blood";
    "redemption";
    "frost";
    "drain";
    "firewall";
  ]

let spawn_demo layout name =
  let now = R.get_time () in
  let own = slots_of owner layout.slot_center in
  let foe = slots_of opponent layout.slot_center in
  let own_life = layout.life owner and foe_life = layout.life opponent in
  let target = Some foe.(2) in
  let look style aim r g b power = { Spell_looks.style; aim; r; g; b; power } in
  let go e = spawn e in
  match name with
  | "stones" ->
      go
        (spawn_style
           (look Spell_looks.Stones Spell_looks.All_slots 0.4 0.36 0.3 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "sprout" ->
      go
        (spawn_style
           (look Spell_looks.Sprout Spell_looks.Own_life 0.2 0.75 0.2 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "eyes" ->
      go
        (spawn_style
           (look Spell_looks.Eyes Spell_looks.Own_life 1. 0.55 0.05 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "rise" ->
      go
        (spawn_style
           (look Spell_looks.Rise Spell_looks.Own_life 0.35 0.7 1. 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "strike" ->
      go
        (spawn_style
           (look Spell_looks.Strike Spell_looks.Foe_slots 0.7 0.15 0.9 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "swirl" ->
      go
        (spawn_style
           (look Spell_looks.Swirl Spell_looks.Foe_slots 0.75 0.9 1. 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "ice" ->
      go
        (spawn_style
           (look Spell_looks.Ice Spell_looks.All_slots 0.7 0.9 1. 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "shadow" ->
      go
        (spawn_style
           (look Spell_looks.Shadow Spell_looks.Target 0.45 0.1 0.7 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "rays" ->
      go
        (spawn_style
           (look Spell_looks.Rays Spell_looks.All_slots 0.55 0.95 0.7 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "drops" ->
      go
        (spawn_style
           (look Spell_looks.Drops Spell_looks.Target 0.6 0.75 0.2 1.3)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "fireburst" ->
      go
        (spawn_style
           (look Spell_looks.Fireburst Spell_looks.All_slots 1. 0.4 0.05 1.35)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "ring" ->
      go
        (spawn_style
           (look Spell_looks.Ring Spell_looks.Target 0.4 0.75 1. 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "spikes" ->
      go
        (spawn_style
           (look Spell_looks.Spikes Spell_looks.Foe_slots 0.92 0.08 0.1 1.2)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "lightning" -> go (spawn_lightning [ own_life; foe.(2); foe_life ] ~start:now)
  | "ball" -> go (spawn_lightning ~style:ball [ own_life; foe_life ] ~start:now)
  | "flames" -> go (spawn_flame_wave (slots_of opponent layout.flame_base) ~start:now)
  | "inferno" ->
      let bases = slots_of opponent layout.flame_base in
      go
        (spawn_inferno ~target:bases.(2)
           ~others:(Array.append (Array.sub bases 0 2) (Array.sub bases 3 3))
           ~start:now)
  | "heal" -> go (spawn_particles nature_heal ~x:own.(2).x ~y:own.(2).y ~start:now)
  | "faerie" -> go (spawn_particles faerie_fire ~x:foe.(0).x ~y:foe.(0).y ~start:now)
  | "rain" -> go (spawn_particles acid_rain ~x:foe.(0).x ~y:foe.(0).y ~start:now)
  | "madden" ->
      go
        (spawn_madden
           ~foes:(List.init nb_slots (fun n -> (opponent, n, foe.(n))))
           ~allies:(List.init nb_slots (fun n -> (owner, n, own.(n))))
           ~eye:{ x = (foe.(2).x +. foe.(3).x) *. 0.5; y = (own.(0).y +. foe.(0).y) *. 0.5 }
           ~start:now)
  | "shockwave" -> go (spawn_shockwave foe ~start:now)
  | "bats" -> go (spawn_bats ~from_:own.(1) ~to_:own.(4) ~start:now)
  | "harvest" -> go (spawn_harvest ~from_:foe.(2) ~to_:own_life ~souls:3 ~start:now)
  | "phosphor" -> go (spawn_phosphor ~from_:own.(3) ~targets:foe ~start:now)
  | "victory" -> victory layout ~winner:(Some owner) ~you:true
  | "blackmass" -> go (spawn_black_mass ~source:own.(2) ~target:foe.(0) ~w:500. ~h:100. ~start:now)
  | "poison" -> go (spawn_poison ~own:own.(2) ~facing:foe.(2) ~smoke:[| own.(2); foe.(2) |] ~start:now)
  | "flux" -> go (spawn_green_flux ~from_:(layout.house owner 3) ~to_:own_life ~start:now)
  | "nether" -> go (spawn_particles nether_grasp ~x:own.(0).x ~y:own.(0).y ~start:now)
  | "blood" -> go (spawn_particles blood_burst ~x:foe.(2).x ~y:foe.(2).y ~start:now)
  | "redemption" -> go (spawn_particles redemption ~x:own.(2).x ~y:own.(2).y ~start:now)
  | "frost" -> go (spawn_lightning ~style:frost [ own_life; foe.(2); foe_life ] ~start:now)
  | "drain" -> go (spawn_lightning ~style:drain [ layout.house opponent 0; foe_life ] ~start:now)
  | "firewall" -> go (spawn_particles fire_wall ~x:foe.(0).x ~y:foe.(0).y ~start:now)
  | _ -> ()
