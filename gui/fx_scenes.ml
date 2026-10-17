(* Spell and ability visuals: madden, shockwave and victory. *)

open Mancer
open Sp
module R = Raylib
open Fx_core
open Fx_lightning
open Fx_particles
open Fx_flames
open Fx_dedicated

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
      if fade > 0.02 then (
        (if age <= 0.28 then
           let p = age /. 0.28 in
           let a = (1. -. p) *. fade *. 0.55 in
           Array.iter
             (fun s ->
               circle s.x (s.y -. 8.) (22. +. (38. *. p)) (rgb 1. 0.86 0.3 (a *. 0.35));
               circle s.x (s.y -. 8.) (8. +. (10. *. (1. -. p))) (rgb 1. 0.95 0.7 a))
             slots);
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
  mk ~start ~duration:Float.infinity (fun ~age ->
      let appear = Float.min 1. (age /. 0.12) in
      let scale_k =
        if age < 0.28 then lerp 0.2 1.7 (swing_out (age /. 0.28))
        else if age < 0.44 then lerp 1.7 1.25 (ease_out_quad ((age -. 0.28) /. 0.16))
        else 1.25
      in
      if appear > 0.02 then (
        let font = !banner_font in
        let size = 22. *. scale_k *. !scale in
        let tw = R.Vector2.x (R.measure_text_ex font msg size 0.) in
        let pad_x = 14. *. scale_k and pad_y = 6. *. scale_k in
        let w = (tw /. !scale) +. (pad_x *. 2.) and h = (size /. !scale) +. (pad_y *. 2.) in
        rect (at.x -. (w *. 0.5)) (at.y -. (h *. 0.5)) w h (rgb 0.04 0.03 0.02 (0.62 *. appear));
        R.draw_text_ex font msg
          (R.Vector2.create (sx at.x -. (tw *. 0.5)) (sy at.y -. (size *. 0.45)))
          size 0.
          (if won then rgb 1. 0.84 0.28 appear else rgb 0.92 0.92 0.95 appear)))

(* Rounded-rect ring around the face (Scala AvatarHalo.frame), not a filled
   disc over it. *)
let halo_ring ~cx ~cy ~inner ~outer color =
  if outer <= inner then ()
  else
    let segs = 5 in
    let path half radius =
      let r = Float.min radius (half *. 0.35) in
      let n = segs * 4 in
      let pts = Array.make n (0., 0.) in
      let corners =
        [|
          (half -. r, half -. r, 0.);
          (-.half +. r, half -. r, Float.pi *. 0.5);
          (-.half +. r, -.half +. r, Float.pi);
          (half -. r, -.half +. r, Float.pi *. 1.5);
        |]
      in
      let k = ref 0 in
      Array.iter
        (fun (ox, oy, start) ->
          for s = 0 to segs - 1 do
            let ang = start +. (float s /. float segs *. (Float.pi *. 0.5)) in
            pts.(!k) <- (cx +. ox +. (cos ang *. r), cy +. oy +. (sin ang *. r));
            incr k
          done)
        corners;
      pts
    in
    let inn = path inner (Float.min 7. (inner *. 0.35)) in
    let out = path outer (Float.min (7. +. (outer -. inner)) (outer *. 0.35)) in
    let n = Array.length inn in
    for i = 0 to n - 1 do
      let j = (i + 1) mod n in
      let ix0, iy0 = inn.(i) and ix1, iy1 = inn.(j) in
      let ox0, oy0 = out.(i) and ox1, oy1 = out.(j) in
      tri ix0 iy0 ix1 iy1 ox1 oy1 color;
      tri ix0 iy0 ox1 oy1 ox0 oy0 color
    done

let spawn_halo ~(center : spot) ~half ~start =
  mk ~start ~duration:Float.infinity (fun ~age ->
      let appear = Float.min 1. (age /. 0.28) in
      if appear > 0.02 then
        additive (fun () ->
            let pulse = 0.9 +. (0.1 *. sin (age *. 2.6)) in
            let inner = half +. 2.5 in
            let ring outer r g b al =
              if al > 0.02 then halo_ring ~cx:center.x ~cy:center.y ~inner ~outer (rgb r g b al)
            in
            ring (inner +. (15. *. pulse)) 1. 0.88 0.45 (0.14 *. appear);
            ring (inner +. 8.) 1. 0.95 0.7 (0.22 *. appear);
            ring (inner +. 3.2) 1. 1. 0.94 (0.34 *. appear)))

let victory layout ~winner ~you ~start =
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
  spawn (spawn_banner ~msg ~at:between ~won ~start);
  Option.iter (fun w -> spawn (spawn_halo ~center:(layout.life w) ~half:42. ~start)) winner;
  if won then spawn (spawn_confetti own ~start)
