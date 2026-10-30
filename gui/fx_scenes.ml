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

let shockwave_duration = 1.2

(* Chaos Priest's Shockwave: the dice spare one foe creature and stun the rest.
   The spared slot is the emitter, not a hole to hide.  Out of it run trains of
   parallel arcs -- crescents that bow the way they travel, like the fronts of a
   shock wave -- one train to the left when there are lanes there and one to the
   right when there are lanes there, each arc born at the emitter and running out
   over the stunned slots.  The arcs fade in as they are born and never light
   past half, so the row sees a wave, not a wall; every stunned lane shudders as
   the front reaches it, the way the warp quake makes a row tremble. *)
let spawn_shockwave ~slots ~spared ~foe_pid ~start =
  let duration = shockwave_duration in
  let n = Array.length slots in
  if n = 0 then mk ~start ~duration:0. (fun ~age -> ())
  else begin
    let center = slots.(spared) in
    let left = Array.to_list (Array.sub slots 0 spared) in
    let right = Array.to_list (Array.sub slots (spared + 1) (n - spared - 1)) in
    (* the farthest lane on a side is where that side's wave runs out *)
    let side list =
      match list with
      | [] -> None
      | first :: _ ->
          let dist (p : spot) = hypot (p.x -. center.x) (p.y -. center.y) in
          let tip, d =
            List.fold_left
              (fun (b, bd) p ->
                let dd = dist p in
                if dd > bd then (p, dd) else (b, bd))
              (first, dist first) list
          in
          Some (tip, d)
    in
    let left_side = side left and right_side = side right in
    let span =
      List.fold_left (fun m -> function Some (_, d) -> Float.max m d | None -> m) 0.
        [ left_side; right_side ]
    in
    (* the wave crosses the whole row in [sweep] seconds; a lane shudders at the
       moment the front reaches it *)
    let sweep = 0.72 in
    let speed = if span < 1. then 1. else span /. sweep in
    Array.iteri
      (fun i (p : spot) ->
        if i <> spared then
          let t = hypot (p.x -. center.x) (p.y -. center.y) /. speed in
          add_shake foe_pid i ~start:(start +. t) ~duration:0.6 ~strength:1.1)
      slots;
    (* [train tip] is the wave running from the emitter to [tip]: [waves] parallel
       crescents, spaced by [gap], all marching outward together.  A new one is
       born at the emitter as the leading ones run on, so the train never ends
       until it has crossed the side. *)
    let waves = 5 and gap = 30. in
    let train ~age ~fade (tip : spot) =
      let ex = tip.x -. center.x in
      let reach = Float.max 1. (hypot ex (tip.y -. center.y)) in
      let dir = if ex < 0. then -1. else 1. in
      let front = age *. speed in
      for k = 0 to waves - 1 do
        let d = front -. (float k *. gap) in
        if d > 0. then begin
          let spring = clamp01 (d /. 34.) in
          let out = clamp01 ((reach +. 60. -. d) /. 70.) in
          (* the leading arc is the crest; the ones behind are its fading wake *)
          let a = fade *. spring *. out *. (0.42 -. (0.055 *. float k)) in
          if a > 0.01 then begin
            let cx = center.x +. (dir *. d) in
            let h = 50. +. (noise k 1 5 *. 4.) in
            let bow = 22. +. (3. *. noise k 3 5) in
            (* a slow shudder runs along the crest so the wave is never a clean
               stencil -- this house serves chaos *)
            let shiver = sin ((age *. 20.) +. (float k *. 2.3)) *. 1.4 in
            let steps = 16 in
            for s = 0 to steps - 1 do
              let t0 = ((float s /. float steps) *. 2.) -. 1. in
              let t1 = ((float (s + 1) /. float steps) *. 2.) -. 1. in
              let ax = cx +. (dir *. bow *. cos (t0 *. Float.pi /. 2.)) +. (shiver *. t0) in
              let ay = center.y +. (h *. t0) in
              let bx = cx +. (dir *. bow *. cos (t1 *. Float.pi /. 2.)) +. (shiver *. t1) in
              let by = center.y +. (h *. t1) in
              (* a soft glow behind the bright core, both translucent *)
              line ax ay bx by 3.6 (rgb 0.5 0.45 0.95 (a *. 0.32));
              line ax ay bx by 1.3 (rgb 0.86 0.82 1. a)
            done
          end
        end
      done
    in
    mk ~start ~duration (fun ~age ->
        let fade = Float.min 1. ((duration -. age) /. 0.3) in
        if fade > 0.02 then
          additive (fun () ->
              (* the emitter: a quiet point the arcs leave from, on the spared lane *)
              let pulse = 0.5 +. (0.5 *. sin (age *. 9.)) in
              circle center.x center.y (6. +. (pulse *. 3.)) (rgb 0.7 0.64 1. (fade *. 0.2));
              circle center.x center.y 3. (rgb 1. 0.96 1. (fade *. 0.45));
              Option.iter (fun (tip, _) -> train ~age ~fade tip) left_side;
              Option.iter (fun (tip, _) -> train ~age ~fade tip) right_side))
  end

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
