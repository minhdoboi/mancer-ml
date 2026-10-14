(* Spell and ability visuals: effects with their own choreography (poison, rain, ...). *)

open Mancer
open Sp
module R = Raylib
open Fx_core
open Fx_lightning
open Fx_particles
open Fx_flames

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

(* Shadowcaster scythe: a big blade sweeps across the foe's row, cutting every
   lane, and ends on the foe's portrait. *)
let spawn_scythe ~foe ~foe_life ~start =
  let duration = 1.15 and sweep = 0.75 in
  let ys =
    Array.fold_left (fun acc (p : spot) -> acc +. p.y) foe_life.y foe /. float (Array.length foe + 1)
  in
  let xs = Array.map (fun (p : spot) -> p.x) foe in
  let x_from = Array.fold_left Float.max foe_life.x xs +. 110. in
  let x_to = foe_life.x -. 10. in
  let radius = 78. in
  let blade cx cy alpha =
    let a0 = 3.6 and a1 = 2.1 and n = 18 in
    let pt r a = (cx +. (cos a *. r), cy +. (sin a *. r)) in
    let ccx = cx +. (radius *. 0.25) in
    let pt r a =
      let x, y = pt r a in
      (x +. (ccx -. cx), y)
    in
    let rec go k =
      if k < n then (
        let u0 = float k /. float n and u1 = float (k + 1) /. float n in
        let th u = radius *. 0.3 *. sin (Float.pi *. u) in
        let a_of u = a0 +. ((a1 -. a0) *. u) in
        let ox0, oy0 = pt radius (a_of u0) and ox1, oy1 = pt radius (a_of u1) in
        let ix0, iy0 = pt (radius -. th u0) (a_of u0) and ix1, iy1 = pt (radius -. th u1) (a_of u1) in
        tri ox0 oy0 ox1 oy1 ix0 iy0 (rgb 0.62 0.55 0.85 (alpha *. 0.9));
        tri ox1 oy1 ix1 iy1 ix0 iy0 (rgb 0.62 0.55 0.85 (alpha *. 0.9));
        line ox0 oy0 ox1 oy1 2.5 (rgb 0.95 0.92 1. alpha);
        go (k + 1))
    in
    go 0;
    let tx, ty = pt radius a0 in
    line tx ty (tx +. 150.) (ty -. 130.) 5. (rgb 0.2 0.12 0.28 alpha);
    line tx ty (tx +. 150.) (ty -. 130.) 1.8 (rgb 0.55 0.4 0.75 (alpha *. 0.8))
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.25) in
      let u = clamp01 (age /. sweep) in
      let pos u = x_from +. ((x_to -. x_from) *. ease_out u) in
      let cx = pos u in
      (* cut flashes on every lane the blade has passed *)
      Array.iter
        (fun (p : spot) ->
          if cx < p.x +. 20. then
            let t = clamp01 ((x_from -. p.x) /. (x_from -. x_to)) in
            let hit_age = age -. (sweep *. (1. -. sqrt (1. -. t))) in
            if hit_age > 0. && hit_age < 0.4 then (
              let k = hit_age /. 0.4 in
              line (p.x +. 40.) (p.y +. 40.) (p.x -. 40.) (p.y -. 40.)
                (6. *. (1. -. k))
                (rgb 1. 0.95 1. ((1. -. k) *. fade));
              circle p.x p.y (12. +. (k *. 30.)) (rgb 0.6 0.4 0.9 ((1. -. k) *. 0.5 *. fade))))
        foe;
      for g = 3 downto 1 do
        let ug = clamp01 ((age -. (float g *. 0.035)) /. sweep) in
        if ug > 0. then blade (pos ug) ys (fade *. 0.18 *. float (4 - g))
      done;
      if age > 0. then blade cx ys fade;
      if age > sweep then (
        let k = clamp01 ((age -. sweep) /. 0.35) in
        circle foe_life.x foe_life.y (14. +. (k *. 40.)) (rgb 0.7 0.5 1. ((1. -. k) *. 0.7 *. fade));
        circle foe_life.x foe_life.y 8. (rgb 1. 0.95 1. ((1. -. k) *. fade))))

(* Shaman phantom fury: a pack of claws rakes the whole foe row at once, several
   triple gashes slashing in different directions and crossing each other. *)
let spawn_claws ~foe ~start =
  let duration = 1.2 and sweep = 0.22 in
  let n = Array.length foe in
  let xs = Array.map (fun (p : spot) -> p.x) foe in
  let cy = if n = 0 then 0. else Array.fold_left (fun a (p : spot) -> a +. p.y) 0. foe /. float n in
  let x_min = Array.fold_left Float.min infinity xs -. 60.
  and x_max = Array.fold_left Float.max neg_infinity xs +. 60. in
  let h = 62. in
  (* (x0, y0, x1, y1 as fractions of the row box, delay) *)
  let swipes =
    [|
      (0., 0.5, 1., 0.5, 0.);
      (1., 0.65, 0., 0.4, 0.04);
      (0., 0., 1., 1., 0.08);
      (0., 1., 1., 0., 0.12);
      (1., 0., 0., 1., 0.18);
      (0.1, 0.2, 0.95, 0.85, 0.24);
      (0.9, 1., 0.05, 0.1, 0.3);
      (0., 0.3, 1., 0.3, 0.36);
    |]
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.35) in
      if n > 0 && fade > 0.02 then
        additive (fun () ->
            Array.iteri
              (fun si (fx0, fy0, fx1, fy1, delay) ->
                let a = age -. delay in
                if a > 0. then
                  let u = clamp01 (a /. sweep) in
                  let x0 = x_min +. ((x_max -. x_min) *. fx0) and x1 = x_min +. ((x_max -. x_min) *. fx1) in
                  let y0 = cy -. h +. (2. *. h *. fy0) and y1 = cy -. h +. (2. *. h *. fy1) in
                  let dx = x1 -. x0 and dy = y1 -. y0 in
                  let len = Float.max 1. (hypot dx dy) in
                  let nx = -.dy /. len and ny = dx /. len in
                  let life = clamp01 (1. -. ((a -. sweep) /. 0.6)) in
                  let head = ease_out u in
                  for k = 0 to 2 do
                    let off = ((float k -. 1.) *. 15.) +. ((noise si k 1 -. 0.5) *. 6.) in
                    let seg = 14 in
                    for m = 0 to seg - 1 do
                      let t0 = head *. float m /. float seg and t1 = head *. float (m + 1) /. float seg in
                      let w = float (m + 1) /. float seg in
                      let al = fade *. life *. (0.3 +. (0.7 *. w)) in
                      let px t = x0 +. (dx *. t) +. (nx *. off) and py t = y0 +. (dy *. t) +. (ny *. off) in
                      line (px t0) (py t0) (px t1) (py t1) (8. *. w) (rgb 0.7 0.02 0.05 (al *. 0.45));
                      line (px t0) (py t0) (px t1) (py t1) ((3.5 *. w) +. 0.6) (rgb 1. 0.3 0.15 (al *. 0.9));
                      line (px t0) (py t0) (px t1) (py t1) (1.3 *. w) (rgb 1. 0.95 0.8 al)
                    done;
                    if a < sweep +. 0.08 then
                      circle
                        (x0 +. (dx *. head) +. (nx *. off))
                        (y0 +. (dy *. head) +. (ny *. off))
                        4. (rgb 1. 0.9 0.7 fade)
                  done)
              swipes;
            (* every lane flashes blood-red as the claws land *)
            Array.iteri
              (fun j (p : spot) ->
                let hit = age -. 0.05 -. (float j *. 0.05) in
                if hit > 0. && hit < 0.5 then (
                  let k = hit /. 0.5 in
                  circle p.x cy (16. +. (k *. 34.)) (rgb 0.9 0.08 0.08 ((1. -. k) *. 0.5 *. fade));
                  circle p.x cy 8. (rgb 1. 0.6 0.4 ((1. -. k) *. 0.6 *. fade))))
              foe))

(* Dreamweaver aurora: shimmering green, teal and violet curtains of light wave
   across the whole board, then sparkles settle on every creature they heal. *)
let spawn_aurora ~bounds ~foe ~own ~start =
  let duration = 2.4 in
  let all = Array.append foe own in
  let n = Array.length bounds in
  let xs = Array.map (fun (p : spot) -> p.x) bounds and ys = Array.map (fun (p : spot) -> p.y) bounds in
  let x_min = Array.fold_left Float.min infinity xs -. 70.
  and x_max = Array.fold_left Float.max neg_infinity xs +. 70. in
  let y_top = Array.fold_left Float.min infinity ys -. 90.
  and y_bot = Array.fold_left Float.max neg_infinity ys +. 90. in
  let curtains = [| (0.25, 1., 0.55, 0.0, 0.9); (0.2, 0.85, 0.95, 1.7, 0.7); (0.65, 0.4, 1., 3.1, 0.6) |] in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min (Float.min 1. (age /. 0.5)) (Float.min 1. ((duration -. age) /. 0.6)) in
      if n > 0 && fade > 0.02 then
        additive (fun () ->
            let span = y_bot -. y_top in
            Array.iteri
              (fun ci (r, g, b, ph, amp) ->
                let x = ref x_min in
                while !x <= x_max do
                  let wave =
                    sin ((!x *. 0.018) +. (age *. 1.3) +. ph)
                    +. (0.5 *. sin ((!x *. 0.045) -. (age *. 2.1) +. ph))
                  in
                  let top = y_top +. (span *. (0.05 +. (0.12 *. float ci))) +. (wave *. 22.) in
                  let len = span *. (0.55 +. (0.2 *. sin ((!x *. 0.03) +. (age *. 0.9) +. ph))) in
                  let shim = 0.55 +. (0.45 *. sin ((!x *. 0.11) +. (age *. 3.2) +. ph)) in
                  let seg = 8 in
                  for m = 0 to seg - 1 do
                    let u0 = float m /. float seg and u1 = float (m + 1) /. float seg in
                    let al =
                      fade *. amp *. shim *. 0.2 *. ((1. -. u0) ** 1.6) *. Float.min 1. ((u0 *. 8.) +. 0.15)
                    in
                    line !x (top +. (len *. u0)) !x (top +. (len *. u1)) 8. (rgb r g b al)
                  done;
                  x := !x +. 7.
                done)
              curtains;
            (* soft glow wash on the whole field *)
            circle ((x_min +. x_max) /. 2.) y_top ((x_max -. x_min) *. 0.45) (rgb 0.3 1. 0.7 (fade *. 0.06));
            (* healing sparkles settle on every slot *)
            Array.iteri
              (fun j (p : spot) ->
                let a = age -. 0.7 -. (noise j 0 3 *. 0.25) in
                if a > 0. && a < 1.3 then
                  for i = 0 to 7 do
                    let k = Float.min 1. (a /. 1.3) in
                    let ph = noise j i 4 in
                    let px = p.x +. ((noise j i 5 -. 0.5) *. 70.) in
                    let py = p.y -. 50. +. (k *. 95.) +. (sin ((k *. 6.) +. (ph *. 6.)) *. 4.) in
                    let al = fade *. sin (Float.pi *. k) in
                    circle px py 4.5 (rgb 0.4 1. 0.7 (al *. 0.25));
                    circle px py 1.8 (rgb 0.9 1. 0.95 (al *. 0.9))
                  done)
              all))

(* Powermancer infernal bargain: a wave of flame-edged pages flips over every card
   in the hand, left to right, shedding embers as each one turns. *)
let spawn_page_fire ~cards ~start =
  let n = Array.length cards in
  let x_min = Array.fold_left (fun a (p : spot) -> Float.min a p.x) infinity cards in
  let x_max = Array.fold_left (fun a (p : spot) -> Float.max a p.x) neg_infinity cards in
  let flip = 0.55 in
  let duration = 0.7 +. flip +. 0.5 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.3) in
      if n > 0 && fade > 0.02 then
        Array.iteri
          (fun i (p : spot) ->
            let wave = if x_max > x_min then (p.x -. x_min) /. (x_max -. x_min) else 0. in
            let a = age -. (wave *. 0.7) -. (noise i 0 6 *. 0.06) in
            if a > 0. && a < flip +. 0.45 then (
              let u = clamp01 (a /. flip) in
              let ang = Float.pi *. ease_out u in
              let hw = 45. and hh = 51. in
              let edge = p.x -. hw +. (hw *. (1. -. cos ang)) in
              let spine = p.x -. hw in
              let lift = sin ang *. 7. in
              let life = if a > flip then clamp01 (1. -. ((a -. flip) /. 0.45)) else 1. in
              if a < flip then (
                (* the page *)
                tri spine (p.y -. hh) edge
                  (p.y -. hh -. lift)
                  edge
                  (p.y +. hh +. lift)
                  (rgb 0.35 0.12 0.06 (0.7 *. fade));
                tri spine (p.y -. hh) edge
                  (p.y +. hh +. lift)
                  spine (p.y +. hh)
                  (rgb 0.35 0.12 0.06 (0.7 *. fade)));
              additive (fun () ->
                  if a < flip then (
                    (* burning edge *)
                    line edge
                      (p.y -. hh -. lift)
                      edge
                      (p.y +. hh +. lift)
                      7.
                      (rgb 1. 0.35 0.05 (0.55 *. fade));
                    line edge (p.y -. hh -. lift) edge (p.y +. hh +. lift) 3. (rgb 1. 0.8 0.3 (0.9 *. fade));
                    for f = 0 to 5 do
                      let fy = p.y -. hh +. (float f *. (2. *. hh /. 5.)) in
                      let fl = 6. +. (noise i f 7 *. 8.) +. (sin ((age *. 30.) +. float f) *. 3.) in
                      circle edge (fy -. (fl *. 0.6)) (fl *. 0.8) (rgb 1. 0.5 0.1 (0.4 *. fade));
                      circle edge (fy -. fl) (fl *. 0.4) (rgb 1. 0.9 0.5 (0.5 *. fade))
                    done);
                  (* embers drifting up from the turned page *)
                  for e = 0 to 9 do
                    let ea = a -. (float e *. 0.045) in
                    if ea > 0. && ea < 0.7 then (
                      let k = ea /. 0.7 in
                      let ex = p.x +. ((noise i e 8 -. 0.5) *. 80.) +. (sin ((k *. 5.) +. float e) *. 5.) in
                      let ey = p.y +. 30. -. (k *. 90.) in
                      circle ex ey 3.5 (rgb 1. 0.4 0.05 ((1. -. k) *. 0.3 *. fade *. life));
                      circle ex ey 1.4 (rgb 1. 0.85 0.4 ((1. -. k) *. 0.9 *. fade *. life)))
                  done;
                  if a >= flip then
                    (* scorch glow left on the card *)
                    circle p.x p.y 40. (rgb 1. 0.35 0.05 (life *. 0.22 *. fade)))))
          cards)

(* Powermancer eternal burial: the sacrificed creature is wrapped in a rising
   pyre; flames lick up its slot and heavy grey smoke curls off into the air. *)
let spawn_burial ~at ~start =
  let duration = 2.0 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min (Float.min 1. (age /. 0.2)) (Float.min 1. ((duration -. age) /. 0.5)) in
      if fade > 0.02 then (
        (* smoke first, plain blending so it reads as dark against the board *)
        for i = 0 to 17 do
          let born = float i *. 0.07 in
          let a = age -. born in
          if a > 0. && a < 1.1 then (
            let k = a /. 1.1 in
            let x = at.x +. ((noise i 0 9 -. 0.5) *. 26.) +. (sin ((k *. 4.) +. float i) *. 10. *. k) in
            let y = at.y +. 10. -. (k *. 95.) in
            let rad = 10. +. (k *. 24.) in
            let al = sin (Float.pi *. Float.min 1. (k *. 1.15)) *. 0.42 *. fade in
            circle x y rad (rgb 0.2 0.19 0.2 al);
            circle (x +. 3.) (y -. 3.) (rad *. 0.6) (rgb 0.34 0.32 0.32 (al *. 0.7)))
        done;
        additive (fun () ->
            (* base glow *)
            circle at.x (at.y +. 22.) 34. (rgb 1. 0.3 0.05 (0.22 *. fade));
            (* flame tongues *)
            for i = 0 to 13 do
              let phase = noise i 1 9 *. 6.28 in
              let flick = 0.6 +. (0.4 *. sin ((age *. 14.) +. phase)) in
              let bx = at.x +. ((noise i 2 9 -. 0.5) *. 46.) in
              let h =
                (28. +. (noise i 3 9 *. 30.)) *. flick *. (1. -. (0.25 *. Float.min 1. (age /. duration)))
              in
              for s = 0 to 4 do
                let u = float s /. 4. in
                let y = at.y +. 34. -. (h *. u) in
                let x = bx +. (sin ((age *. 9.) +. phase +. (u *. 3.)) *. 4. *. u) in
                let r = (9. -. (7. *. u)) *. (0.6 +. (0.4 *. flick)) in
                circle x y r (rgb 1. (0.25 +. (0.5 *. u)) (0.02 +. (0.25 *. u)) (0.28 *. fade));
                if s < 3 then circle x y (r *. 0.45) (rgb 1. 0.9 0.5 (0.4 *. fade))
              done
            done;
            (* embers *)
            for e = 0 to 11 do
              let a = Float.rem (age +. (noise e 4 9 *. 1.4)) 1.4 in
              let k = a /. 1.4 in
              let x = at.x +. ((noise e 5 9 -. 0.5) *. 50.) +. (sin ((k *. 6.) +. float e) *. 6.) in
              let y = at.y +. 30. -. (k *. 100.) in
              circle x y 1.6 (rgb 1. 0.7 0.25 ((1. -. k) *. 0.9 *. fade))
            done)))

(* Faerie druid faerie fire: cold magical flames of shifting blue, violet, green
   and gold ignite in a ripple along the foe row, with fairy lights spiralling up. *)
let spawn_faerie_fire ~floors ~start =
  let duration = 1.9 in
  let n = Array.length floors in
  let hue u =
    (* blue -> violet -> green -> gold *)
    let u = Float.rem (Float.abs u) 1. in
    if u < 0.33 then (0.2, 0.45 +. (u *. 0.6), 1.)
    else if u < 0.66 then
      (0.55 -. ((u -. 0.33) *. 1.2), 0.3 +. ((u -. 0.33) *. 2.), 1. -. ((u -. 0.33) *. 1.8))
    else (0.2 +. ((u -. 0.66) *. 2.), 1., 0.4 -. ((u -. 0.66) *. 0.6))
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.45) in
      if n > 0 && fade > 0.02 then
        additive (fun () ->
            Array.iteri
              (fun j (p : spot) ->
                let a = age -. (float j *. 0.09) in
                if a > 0. then (
                  let rise = Float.min 1. (a /. 0.25) in
                  (* flame tongues, each its own hue that drifts over time *)
                  for i = 0 to 9 do
                    let ph = noise i j 1 *. 6.28 in
                    let flick = 0.65 +. (0.35 *. sin ((age *. 13.) +. ph)) in
                    let bx = p.x +. ((noise i j 2 -. 0.5) *. 62.) in
                    let h = (40. +. (noise i j 3 *. 34.)) *. flick *. rise in
                    let r0, g0, b0 = hue (noise i j 4 +. (age *. 0.35)) in
                    for s = 0 to 5 do
                      let u = float s /. 5. in
                      let x = bx +. (sin ((age *. 8.) +. ph +. (u *. 4.)) *. 6. *. u) in
                      let y = p.y -. (h *. u) in
                      let r = (10. -. (8. *. u)) *. (0.55 +. (0.45 *. flick)) in
                      circle x y r (rgb r0 g0 b0 (0.3 *. fade));
                      if s < 4 then circle x y (r *. 0.4) (rgb 0.95 0.97 1. (0.45 *. fade))
                    done
                  done;
                  (* fairy lights spiralling up the lane *)
                  for f = 0 to 5 do
                    let k = Float.rem ((a *. 0.8) +. (noise f j 5 *. 1.)) 1. in
                    let ang = (k *. 9.) +. (noise f j 6 *. 6.28) in
                    let x = p.x +. (cos ang *. (16. +. (k *. 10.))) in
                    let y = p.y -. (k *. 100.) in
                    let r0, g0, b0 = hue (noise f j 7 +. k) in
                    circle x y 5. (rgb r0 g0 b0 (sin (Float.pi *. k) *. 0.3 *. fade));
                    circle x y 1.8 (rgb 1. 1. 1. (sin (Float.pi *. k) *. 0.95 *. fade))
                  done))
              floors))

(* Fire apocalypse: one huge blast in the middle of the board. A white-hot
   fireball boils outward, a shockwave rolls across the field and bursts into
   flame on every creature it reaches, then a column of smoke climbs away. *)
let spawn_apocalypse ~center ~targets ~start =
  let duration = 2.6 and big = 470. and wave_t = 0.8 in
  let ring r thick c =
    let seg = 56 in
    for i = 0 to seg - 1 do
      let a0 = float i /. float seg *. 2. *. Float.pi and a1 = float (i + 1) /. float seg *. 2. *. Float.pi in
      line (center.x +. (cos a0 *. r)) (center.y +. (sin a0 *. r *. 0.7))
        (center.x +. (cos a1 *. r)) (center.y +. (sin a1 *. r *. 0.7)) thick c
    done
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.6) in
      if fade > 0.02 then (
        (* smoke column and the cooling fireball skin, plain blending *)
        if age > 0.25 then
          for i = 0 to 23 do
            let a = age -. 0.25 -. (float i *. 0.04) in
            if a > 0. && a < 1.7 then (
              let k = a /. 1.7 in
              let x = center.x +. ((noise i 0 11 -. 0.5) *. (50. +. (k *. 90.))) +. (sin ((k *. 3.) +. float i) *. 14.) in
              let y = center.y -. (k *. 230.) +. (noise i 1 11 *. 20.) in
              let rad = 36. +. (k *. 60.) in
              let al = sin (Float.pi *. Float.min 1. (k *. 1.1)) *. 0.5 *. fade in
              circle x y rad (rgb 0.14 0.12 0.12 al);
              circle (x +. 6.) (y -. 8.) (rad *. 0.6) (rgb 0.3 0.2 0.15 (al *. 0.7)))
          done;
        additive (fun () ->
            (* radial streaks at the instant of detonation *)
            let burst = clamp01 (1. -. (age /. 0.35)) in
            if burst > 0. then
              for i = 0 to 17 do
                let ang = (float i /. 18. *. 2. *. Float.pi) +. (noise i 2 11 *. 0.3) in
                let len = (180. +. (noise i 3 11 *. 260.)) *. (1. -. (burst *. 0.4)) in
                line center.x center.y (center.x +. (cos ang *. len)) (center.y +. (sin ang *. len *. 0.7))
                  (6. +. (burst *. 14.)) (rgb 1. 0.85 0.5 (burst *. 0.7 *. fade))
              done;
            (* fireball: boiling blobs thrown outward, white -> yellow -> red *)
            for i = 0 to 59 do
              let grow = clamp01 (age /. (0.55 +. (noise i 4 11 *. 0.5))) in
              let ang = noise i 5 11 *. 2. *. Float.pi in
              let dist = big *. 0.55 *. noise i 6 11 *. ease_out grow in
              let x = center.x +. (cos ang *. dist) and y = center.y +. (sin ang *. dist *. 0.7) -. (age *. 14.) in
              let heat = clamp01 (1. -. (age /. (1.1 +. (noise i 7 11 *. 0.7)))) in
              let rad = (28. +. (noise i 8 11 *. 40.)) *. (0.5 +. (0.6 *. ease_out grow)) *. (0.5 +. (0.5 *. heat)) in
              let g = 0.25 +. (0.65 *. heat) and b = 0.03 +. (0.5 *. heat *. heat) in
              circle x y rad (rgb 1. g b (0.2 *. heat *. fade));
              circle x y (rad *. 0.5) (rgb 1. (g +. 0.1) (b +. 0.2) (0.26 *. heat *. fade))
            done;
            (* white-hot core *)
            let core = clamp01 (1. -. (age /. 0.7)) in
            if core > 0. then (
              circle center.x center.y (70. +. (ease_out (age /. 0.5) *. 90.)) (rgb 1. 0.95 0.75 (core *. 0.55 *. fade));
              circle center.x center.y 40. (rgb 1. 1. 1. (core *. 0.8 *. fade)));
            (* shockwave *)
            if age < wave_t +. 0.5 then (
              let r = big *. ease_out (Float.min 1. (age /. wave_t)) in
              let f = clamp01 (1. -. (age /. (wave_t +. 0.5))) in
              ring r (14. *. f +. 2.) (rgb 1. 0.45 0.1 (0.35 *. f *. fade));
              ring r (5. *. f +. 1.) (rgb 1. 0.9 0.6 (0.85 *. f *. fade)));
            (* sparks and debris *)
            for i = 0 to 49 do
              let a = age -. (noise i 9 11 *. 0.12) in
              if a > 0. && a < 1.3 then (
                let k = a /. 1.3 in
                let ang = noise i 10 11 *. 2. *. Float.pi in
                let sp = 240. +. (noise i 11 11 *. 380.) in
                let d = sp *. (1. -. ((1. -. k) *. (1. -. k))) in
                let x = center.x +. (cos ang *. d) and y = center.y +. (sin ang *. d *. 0.7) +. (k *. k *. 120.) in
                let tx = x -. (cos ang *. 14. *. (1. -. k)) and ty = y -. (sin ang *. 10. *. (1. -. k)) in
                line tx ty x y 2.2 (rgb 1. (0.75 -. (0.4 *. k)) 0.25 ((1. -. k) *. fade)))
            done;
            (* flames bursting on each creature as the shockwave reaches it *)
            Array.iteri
              (fun j (p : spot) ->
                let d = hypot (p.x -. center.x) ((p.y -. center.y) /. 0.7) in
                let hit_t = wave_t *. (1. -. sqrt (1. -. Float.min 1. (d /. big))) in
                let a = age -. hit_t in
                if a > 0. && a < 0.9 then (
                  let k = a /. 0.9 in
                  for i = 0 to 7 do
                    let ang = (noise i j 12 *. 2. *. Float.pi) in
                    let dist = (10. +. (noise i j 13 *. 36.)) *. ease_out k in
                    let x = p.x +. (cos ang *. dist) and y = p.y +. (sin ang *. dist) -. (k *. 40.) in
                    let r = (14. -. (8. *. k)) in
                    circle x y r (rgb 1. (0.5 -. (0.3 *. k)) 0.05 ((1. -. k) *. 0.45 *. fade));
                    circle x y (r *. 0.45) (rgb 1. 0.9 0.5 ((1. -. k) *. 0.6 *. fade))
                  done))
              targets)))

(* Shadowcaster wave: dark particles rain down on each foe lane in a ripple
   from one end of the row to the other, and splash on the ground. *)
let spawn_shadow_wave ~floors ~start =
  let duration = 1.2 in
  let n = Array.length floors in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.2) in
      Array.iteri
        (fun i (p : spot) ->
          let front = float i /. float (max 1 (n - 1)) *. 0.45 in
          for k = 0 to 17 do
            let r = noise i k 3 in
            let delay = front +. (r *. 0.12) in
            let fall = 0.28 +. (noise i k 5 *. 0.1) in
            let a = age -. delay in
            let x = p.x +. ((noise i k 7 -. 0.5) *. 62.) in
            let top = p.y -. 40. -. (110. *. (0.5 +. noise i k 9)) in
            if a > 0. && a < fall then (
              let u = a /. fall in
              let y = top +. ((p.y -. top) *. u *. u) in
              line x (y -. 12.) x y 3. (rgb 0.3 0.1 0.45 (fade *. 0.6));
              circle x y 3.2 (rgb 0.7 0.5 0.95 fade))
            else if a >= fall then
              let s = clamp01 ((a -. fall) /. 0.4) in
              if s < 1. then (
                let al = (1. -. s) *. fade in
                ellipse x (p.y +. 1.) (5. +. (s *. 11.)) (2. +. (s *. 3.)) (rgb 0.35 0.12 0.5 (al *. 0.7));
                let dir = if noise i k 11 > 0.5 then 1. else -1. in
                let hx = x +. (dir *. s *. 18.) and hy = p.y -. (sin (s *. Float.pi) *. 12.) in
                circle hx hy 2. (rgb 0.75 0.55 1. al))
          done)
        floors)

(* Vampires blood ties: the sacrificed creature bursts and its blood arcs across
   to each adjacent creature, which pulses red as it takes the power. *)
let spawn_blood_ties ~from_ ~targets ~start =
  let duration = 1.1 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.2) in
      if fade > 0.02 then (
        (if age < 0.3 then
           let k = age /. 0.3 in
           for i = 0 to 13 do
             let ang = noise i 0 1 *. 2. *. Float.pi and dist = (10. +. (noise i 1 1 *. 30.)) *. ease_out k in
             circle
               (from_.x +. (cos ang *. dist))
               (from_.y +. (sin ang *. dist))
               ((3. *. (1. -. k)) +. 1.)
               (rgb 0.75 0.05 0.1 ((1. -. k) *. fade))
           done);
        Array.iteri
          (fun j (t : spot) ->
            let dx = t.x -. from_.x and dy = t.y -. from_.y in
            let lift = 34. +. Float.min 20. (Float.abs dx *. 0.1) in
            let c1x = from_.x +. (dx *. 0.3) and c1y = from_.y +. (dy *. 0.3) -. lift in
            let c2x = from_.x +. (dx *. 0.7) and c2y = from_.y +. (dy *. 0.7) -. lift in
            for i = 0 to 23 do
              let born = 0.12 +. (float (i mod 8) *. 0.035) +. (noise i j 2 *. 0.03) in
              let travel = 0.4 +. (noise i j 3 *. 0.12) in
              let a = age -. born in
              if a >= 0. && a <= travel then (
                let u = a /. travel in
                let x = bezier from_.x c1x c2x t.x u +. ((noise i j 4 -. 0.5) *. 8.) in
                let y = bezier from_.y c1y c2y t.y u +. ((noise i j 5 -. 0.5) *. 8.) in
                let rad = 2.6 -. (u *. 0.8) in
                circle x y (rad *. 1.8) (rgb 0.5 0.02 0.06 (fade *. 0.4));
                circle x y rad (rgb 0.85 0.08 0.12 fade);
                circle (x -. 0.6) (y -. 0.6) (rad *. 0.4) (rgb 1. 0.5 0.5 (fade *. 0.8)))
            done;
            let touch = age -. 0.55 in
            if touch > 0. then (
              let k = clamp01 (touch /. 0.5) in
              circle t.x t.y (14. +. (k *. 26.)) (rgb 0.8 0.05 0.1 ((1. -. k) *. 0.45 *. fade));
              circle t.x t.y 7. (rgb 1. 0.35 0.35 ((1. -. k) *. 0.6 *. fade))))
          targets))

(* Sower tangling: vines of life sap are torn out of the target creature and
   stream across the lane into the opposite creature, which glows as it drinks. *)
let spawn_tangling ~from_ ~to_ ~start =
  let duration = 1.0 in
  let flux = spawn_green_flux ~from_ ~to_ ~start in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.2) in
      if fade > 0.02 then (
        flux.draw ~age;
        additive (fun () ->
            let drain = clamp01 (1. -. (age /. 0.6)) in
            circle from_.x from_.y (12. +. (drain *. 14.)) (rgb 0.55 0.1 0.2 (drain *. 0.4 *. fade));
            for i = 0 to 9 do
              let a = age -. (float i *. 0.04) in
              if a > 0. && a < 0.5 then
                let k = a /. 0.5 in
                let ang = noise i 7 1 *. 2. *. Float.pi in
                circle
                  (from_.x +. (cos ang *. (20. *. (1. -. k))))
                  (from_.y +. (sin ang *. (20. *. (1. -. k))))
                  2.
                  (rgb 1. 0.3 0.4 ((1. -. k) *. fade))
            done)))

(* Avalanche snow: flakes and white clumps tumble down each lane alongside the
   rocks, and burst into powder on the ground. *)
let snow_fall ~(floors : spot array) ~age ~fade =
  Array.iteri
    (fun i (p : spot) ->
      for k = 0 to 15 do
        let n = noise i k 21 in
        let delay = (float i *. 0.03) +. (n *. 0.2) in
        let a = age -. delay in
        let fall = 0.4 +. (noise i k 22 *. 0.2) in
        let x0 = p.x +. ((noise i k 23 -. 0.5) *. 70.) in
        let top = p.y -. 150. -. (noise i k 24 *. 60.) in
        if a > 0. && a < fall then
          let u = a /. fall in
          let x = x0 +. (sin ((a *. 9.) +. float k) *. 5.) in
          let y = top +. ((p.y -. top) *. ((0.3 *. u) +. (0.7 *. u *. u))) in
          let clump = k mod 4 = 0 in
          if clump then (
            circle x y 7. (rgb 0.82 0.9 0.98 fade);
            circle (x -. 2.) (y -. 2.) 3.5 (rgb 1. 1. 1. fade))
          else circle x y 2.4 (rgb 0.95 0.98 1. (fade *. 0.9))
        else if a >= fall && a < fall +. 0.35 && k mod 2 = 0 then
          let s = (a -. fall) /. 0.35 in
          circle x0 (p.y +. 2.) (6. +. (s *. 16.)) (rgb 0.9 0.95 1. ((1. -. s) *. 0.45 *. fade))
      done)
    floors

(* Air twister: a tall funnel of wind and dust that stands on the target's slot,
   wide at the top and pinched to a point at the floor. Rings of air streaks spin
   at different speeds, debris is hauled up the column and the whole thing sways. *)
let spawn_twister ~floor ~start =
  let duration = 1.7 in
  let height = 175. in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.4) in
      if fade > 0.02 then (
        let grow = ease_out (Float.min 1. (age /. 0.3)) in
        let h_now = height *. grow in
        let radius v = (5. +. (40. *. (v ** 1.35))) *. (0.7 +. (0.3 *. grow)) in
        let centre v =
          floor.x +. (sin ((age *. 5.) +. (v *. 3.2)) *. (3. +. (11. *. v))) +. (sin (age *. 2.1) *. 6. *. v)
        in
        let yat v = floor.y -. (v *. h_now) in
        (* body of the funnel: stacked translucent discs *)
        for l = 0 to 24 do
          let v = float l /. 24. in
          circle (centre v) (yat v) (radius v *. 0.95) (rgb 0.52 0.58 0.66 (0.07 *. fade))
        done;
        let streaks front =
          for r = 0 to 17 do
            let v = float r /. 17. in
            let spin = (age *. (13. -. (6. *. v))) +. (v *. 5.) in
            for k = 0 to 3 do
              let a0 = spin +. (float k *. Float.pi /. 2.) +. (noise r k 1 *. 0.5) in
              let a1 = a0 +. 0.55 +. (0.3 *. noise r k 2) in
              let seg a = (centre v +. (cos a *. radius v), yat v +. (sin a *. radius v *. 0.3)) in
              let x0, y0 = seg a0 and x1, y1 = seg a1 in
              (* only the near or the far half of the ring, so it reads as 3-D *)
              let is_front = sin a0 > 0. in
              if is_front = front then
                line x0 y0 x1 y1
                  ((if front then 2.6 else 1.8) *. (0.6 +. (0.7 *. v)))
                  (rgb 0.85 0.93 1. ((if front then 0.62 else 0.3) *. fade *. (0.55 +. (0.45 *. v))))
            done
          done
        in
        streaks false;
        (* dust and debris dragged around and up the funnel *)
        for d = 0 to 27 do
          let life = 1.1 +. (0.4 *. noise d 3 1) in
          let a = Float.rem (age +. (noise d 4 2 *. life)) life in
          let q = a /. life in
          let v = q *. grow in
          let ang = (age *. (9. +. (4. *. noise d 5 3))) +. (noise d 6 4 *. 6.28) in
          let rr = radius v *. (0.85 +. (0.6 *. q)) in
          let x = centre v +. (cos ang *. rr) and y = yat v +. (sin ang *. rr *. 0.3) in
          let dark = noise d 7 5 > 0.5 in
          let al = sin (Float.pi *. q) *. fade in
          if dark then circle x y (1.8 +. (1.6 *. noise d 8 6)) (rgb 0.3 0.26 0.22 (0.85 *. al))
          else circle x y (1.5 +. (1.2 *. noise d 8 6)) (rgb 0.72 0.66 0.55 (0.7 *. al))
        done;
        streaks true;
        (* dust ring torn up at the foot, and a wide rotor cloud at the top *)
        for k = 0 to 9 do
          let ang = (age *. 8.) +. (float k *. 0.628) in
          let rr = 16. +. (22. *. noise k 1 9) in
          circle (floor.x +. (cos ang *. rr)) (floor.y -. 3. +. (sin ang *. rr *. 0.28)) (6. +. (5. *. noise k 2 9))
            (rgb 0.58 0.52 0.42 (0.3 *. fade *. grow))
        done;
        circle (centre 1.) (yat 1.) (radius 1. *. 1.05) (rgb 0.62 0.68 0.76 (0.12 *. fade));
        ellipse (centre 1.) (yat 1.) (radius 1. *. 1.15) (radius 1. *. 0.38) (rgb 0.8 0.88 1. (0.1 *. fade))))

(* Simoom: a hot desert gale on one creature. A tan sand funnel spins up from
   the slot floor, wider at the top, with grains whipping round it, streaks of
   wind blowing across and a dust haze that settles at the end. *)
let spawn_simoom ~target ~start =
  let duration = 1.5 in
  let grains = 70 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.4) in
      let grow = ease_out (clamp01 (age /. 0.4)) in
      if fade > 0.02 then (
        let base_y = target.y +. 38. and top_y = target.y -. 46. in
        (* dust haze *)
        circle target.x (target.y +. 6.) (46. *. grow) (rgb 0.78 0.6 0.3 (fade *. 0.28));
        circle target.x (target.y +. 22.) (36. *. grow) (rgb 0.7 0.5 0.24 (fade *. 0.3));
        (* sand funnel grains *)
        for i = 0 to grains - 1 do
          let u = (float i +. 0.5) /. float grains in
          (* height along the funnel, grains cycle upward over time *)
          let h = Float.rem (u +. (age *. (0.9 +. (0.5 *. noise i 1 7)))) 1. in
          let width = (7. +. (h *. 34.)) *. grow in
          let ang = (age *. (11. -. (h *. 5.))) +. (float i *. 2.399) in
          let x = target.x +. (cos ang *. width) in
          let y = base_y +. ((top_y -. base_y) *. h) +. (sin ang *. width *. 0.28) in
          let behind = sin ang < 0. in
          let size = (1.4 +. (1.8 *. noise i 3 9)) *. (if behind then 0.8 else 1.15) in
          let shade = 0.55 +. (0.4 *. noise i 5 11) in
          circle x y size
            (rgb (0.92 *. shade +. 0.08) (0.72 *. shade +. 0.05) (0.4 *. shade) (fade *. if behind then 0.55 else 0.95))
        done;
        (* wind streaks sweeping across *)
        for k = 0 to 5 do
          let sweep = Float.rem ((age *. 1.8) +. (float k *. 0.17)) 1. in
          let y = target.y -. 34. +. (float k *. 13.) in
          let x0 = target.x -. 60. +. (sweep *. 120.) in
          let len = 18. +. (10. *. noise k 2 4) in
          line x0 y (x0 +. len) (y +. 2.) 1.6 (rgb 0.95 0.82 0.55 (fade *. 0.6 *. sin (sweep *. Float.pi)))
        done))

(* Whirlwind: a gale runs between the two doomed creatures. A column of
   spinning air stands on each slot and two intertwined strands of wind arc
   from one to the other, so the pair reads as one tearing current. *)
let spawn_whirlwind ~a ~b ~start =
  let duration = 1.4 in
  let dx = b.x -. a.x and dy = b.y -. a.y in
  let len = Float.max 1. (hypot dx dy) in
  let nx = dx /. len and ny = dy /. len in
  let px = -.ny and py = nx in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.3) in
      let grow = ease_out (clamp01 (age /. 0.35)) in
      if fade > 0.02 then
        additive (fun () ->
            (* columns on both slots *)
            List.iter
              (fun (c, seed) ->
                for i = 0 to 39 do
                  let h = Float.rem ((float i /. 40.) +. (age *. 0.8)) 1. in
                  let radius = (6. +. (h *. 30.)) *. grow in
                  let ang = (age *. 14.) +. (float i *. 2.399) +. float seed in
                  let x = c.x +. (cos ang *. radius) in
                  let y = c.y +. 34. -. (h *. 76.) +. (sin ang *. radius *. 0.3) in
                  let front = sin ang > 0. in
                  circle x y
                    (if front then 2.6 else 1.8)
                    (rgb 0.82 0.93 1. (fade *. (if front then 0.85 else 0.45)))
                done;
                circle c.x (c.y +. 10.) (30. *. grow) (rgb 0.6 0.8 1. (fade *. 0.18)))
              [ (a, 0); (b, 3) ];
            (* two strands of wind between the slots *)
            for strand = 0 to 1 do
              let sign = if strand = 0 then 1. else -1. in
              let n = 34 in
              let prev = ref None in
              for i = 0 to n do
                let u = float i /. float n in
                let flow = clamp01 ((age *. 2.2) -. (u *. 0.6)) in
                if flow > 0. then (
                  let amp = sin (u *. Float.pi) *. 20. *. grow in
                  let off = sin ((u *. 9.) -. (age *. 16.) +. (sign *. 1.57)) *. amp in
                  let x = a.x +. (dx *. u) +. (px *. off) in
                  let y = a.y +. (dy *. u) +. (py *. off) in
                  (match !prev with
                  | Some (x0, y0) ->
                      line x0 y0 x y 3.4 (rgb 0.55 0.78 1. (fade *. 0.35 *. flow));
                      line x0 y0 x y 1.4 (rgb 0.95 0.99 1. (fade *. 0.85 *. flow))
                  | None -> ());
                  prev := Some (x, y))
              done
            done))

(* Eternal Storm: a strong wind sweeping over every slot of the board. Each slot
   gets a gust of streaks and tumbling dust, and a pale wave runs across the rows. *)
let spawn_eternal_storm ~slots ~start =
  let duration = 1.3 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. (age /. 0.25) *. Float.min 1. ((duration -. age) /. 0.35) in
      if fade > 0.02 then
        additive (fun () ->
            Array.iteri
              (fun i c ->
                for k = 0 to 4 do
                  let sweep = Float.rem ((age *. 1.5) +. (float k *. 0.2) +. (noise i k 3 *. 0.3)) 1. in
                  let x0 = c.x -. 50. +. (sweep *. 100.) in
                  let y = c.y -. 30. +. (float k *. 15.) +. (sin ((age *. 8.) +. float (i + k)) *. 3.) in
                  let a = fade *. sin (sweep *. Float.pi) in
                  line x0 y (x0 +. 24.) (y +. 2.) 1.6 (rgb 0.8 0.92 1. (a *. 0.6));
                  circle (x0 +. 26.) (y +. 2.) 1.8 (rgb 0.95 0.98 1. (a *. 0.7))
                done;
                circle c.x c.y (34. +. (6. *. sin ((age *. 9.) +. float i))) (rgb 0.6 0.8 1. (fade *. 0.12)))
              slots))

(* Where a lasting vortex hangs: the top-right corner of the player's portrait. *)
let vortex_badge_of life = ({ x = life.x +. 30.; y = life.y -. 26. }, 15.)

(* Energetic Vortex: a funnel opens in the middle of the board, air mana streams
   from the opponent's air house into it and out to the player's air house, then
   the vortex shrinks and flies to the player's portrait, where the lasting icon
   takes over. *)
let spawn_energetic_vortex ~foe_air ~own_air ~life ~start =
  let duration = 2.2 in
  let mid = { x = (foe_air.x +. own_air.x) /. 2.; y = (foe_air.y +. own_air.y) /. 2. } in
  let home, home_r = vortex_badge_of life in
  mk ~start ~duration (fun ~age ->
      let grow = ease_out (clamp01 (age /. 0.5)) in
      let travel = ease_out (clamp01 ((age -. 1.6) /. 0.6)) in
      let cx = mid.x +. ((home.x -. mid.x) *. travel) and cy = mid.y +. ((home.y -. mid.y) *. travel) in
      let radius = (46. *. grow) +. ((home_r -. 46.) *. travel) in
      let fade_in = clamp01 (age /. 0.2) in
      draw_vortex cx cy radius (age +. 3.) fade_in;
      additive (fun () ->
          (* mana streaming in from the foe's air house, then out to ours *)
          let stream a b t0 t1 =
            if age > t0 && age < t1 +. 0.3 then
              for i = 0 to 17 do
                let lag = float i *. 0.035 in
                let u = clamp01 ((age -. t0 -. lag) /. (t1 -. t0)) in
                if u > 0. && u < 1. then (
                  let e = u *. u in
                  let curl = sin (u *. Float.pi) *. 28. *. (if i mod 2 = 0 then 1. else -1.) in
                  let dx = b.x -. a.x and dy = b.y -. a.y in
                  let l = Float.max 1. (hypot dx dy) in
                  let x = a.x +. (dx *. e) +. (-.dy /. l *. curl) in
                  let y = a.y +. (dy *. e) +. (dx /. l *. curl) in
                  circle x y 5. (rgb 0.6 0.85 0.15 0.35);
                  circle x y 2.4 (rgb 1. 0.95 0.45 0.95))
              done
          in
          stream foe_air mid 0.15 0.95;
          stream mid own_air 0.8 1.5;
          (* glow on the receiving house *)
          if age > 1.3 && age < 1.9 then
            circle own_air.x own_air.y (22. *. sin ((age -. 1.3) /. 0.6 *. Float.pi)) (rgb 0.8 0.9 0.2 0.4)))

(* Solar Wave: a wave of sun particles rolls from the caster's row across the
   board to the opponent's, each foe slot flares as it passes, and the wave
   breaks in gold on the opponent's portrait. *)
let spawn_solar_wave ~own ~foe ~foe_life ~start =
  let duration = 1.8 in
  let n = Array.length own in
  if n = 0 || Array.length foe = 0 then mk ~start ~duration:0. (fun ~age:_ -> ())
  else
    let min_x = Array.fold_left (fun m p -> Float.min m p.x) infinity own -. 40. in
    let max_x = Array.fold_left (fun m p -> Float.max m p.x) neg_infinity own +. 40. in
    let y0 = own.(0).y and y1 = foe.(0).y in
    let travel = 1.1 in
    mk ~start ~duration (fun ~age ->
        let fade = Float.min 1. ((duration -. age) /. 0.4) in
        additive (fun () ->
            for i = 0 to 179 do
              let lag = noise i 1 41 *. 0.3 in
              let prog = clamp01 ((age /. travel) -. lag) in
              if prog > 0. && prog < 1. then (
                let x = min_x +. (noise i 2 43 *. (max_x -. min_x)) +. (sin ((age *. 6.) +. float i) *. 6.) in
                let y = y0 +. ((y1 -. y0) *. ease_out prog) +. ((noise i 3 47 -. 0.5) *. 18.) in
                let a = sin (prog *. Float.pi) in
                let sz = 1.5 +. (2.5 *. noise i 4 53) in
                circle x y (sz *. 2.4) (rgb 1. 0.7 0.15 (a *. fade *. 0.2));
                circle x y sz (rgb 1. 0.9 0.4 (a *. fade *. 0.95));
                if sz > 3. then (
                  let g = sz *. 2.2 in
                  line (x -. g) y (x +. g) y 1. (rgb 1. 0.97 0.7 (a *. fade *. 0.8));
                  line x (y -. g) x (y +. g) 1. (rgb 1. 0.97 0.7 (a *. fade *. 0.8))))
            done;
            (* each foe slot flares when the wave reaches it *)
            Array.iter
              (fun p ->
                let hit = (age -. (travel *. 0.8)) /. 0.5 in
                if hit > 0. && hit < 1. then (
                  circle p.x p.y (14. +. (36. *. hit)) (rgb 1. 0.8 0.25 ((1. -. hit) *. 0.45 *. fade));
                  circle p.x p.y (8. +. (14. *. hit)) (rgb 1. 0.97 0.7 ((1. -. hit) *. 0.7 *. fade))))
              foe;
            (* the wave breaks on the portrait *)
            let hit = (age -. travel) /. 0.6 in
            if hit > 0. && hit < 1. then (
              let r = 12. +. (50. *. hit) in
              circle foe_life.x foe_life.y r (rgb 1. 0.78 0.2 ((1. -. hit) *. 0.4));
              for k = 0 to 15 do
                let ang = float k *. (Float.pi *. 2. /. 16.) in
                circle (foe_life.x +. (cos ang *. r *. 1.3)) (foe_life.y +. (sin ang *. r *. 1.3)) 2.5
                  (rgb 1. 0.93 0.5 ((1. -. hit) *. 0.9))
              done)))

(* Eternal Summer lets the caster play an extra creature and spell, so the
   warmth falls on the board and the hand: a golden haze drifts over the rows
   while broad rays of sun slant down from a low sun and settle on the hand. *)
let spawn_eternal_summer ~own ~foe ~hand ~start =
  let duration = 2.2 in
  let all = Array.append own foe in
  if Array.length all = 0 then mk ~start ~duration:0. (fun ~age:_ -> ())
  else
    let min_x = Array.fold_left (fun m p -> Float.min m p.x) infinity all -. 50. in
    let max_x = Array.fold_left (fun m p -> Float.max m p.x) neg_infinity all +. 50. in
    let min_y = Array.fold_left (fun m p -> Float.min m p.y) infinity all -. 50. in
    let max_y = Array.fold_left (fun m p -> Float.max m p.y) neg_infinity all +. 50. in
    let sun = { x = min_x -. 10.; y = min_y -. 70. } in
    let targets =
      Array.init 7 (fun k ->
          let u = float k /. 6. in
          { x = min_x +. (u *. (max_x -. min_x)); y = max_y +. 40. })
    in
    mk ~start ~duration (fun ~age ->
        let fade = clamp01 (age /. 0.5) *. clamp01 ((duration -. age) /. 0.6) in
        if fade > 0.02 then
          additive (fun () ->
              (* drifting golden haze *)
              for i = 0 to 9 do
                let drift = Float.rem ((float i /. 10.) +. (age *. 0.06)) 1. in
                let x = min_x -. 40. +. (drift *. (max_x -. min_x +. 80.)) in
                let y = min_y +. (noise i 1 61 *. (max_y -. min_y)) in
                circle x y (60. +. (40. *. noise i 2 67)) (rgb 1. 0.78 0.3 (fade *. 0.07))
              done;
              (* the low sun *)
              circle sun.x sun.y 70. (rgb 1. 0.8 0.3 (fade *. 0.22));
              circle sun.x sun.y 26. (rgb 1. 0.95 0.65 (fade *. 0.7));
              (* slanting rays, each swaying a little *)
              Array.iteri
                (fun k t ->
                  let pulse = 0.6 +. (0.4 *. sin ((age *. 2.2) +. (float k *. 1.7))) in
                  let w = 14. +. (10. *. noise k 3 71) in
                  tri sun.x sun.y (t.x -. w) t.y (t.x +. w) t.y (rgb 1. 0.85 0.4 (fade *. pulse *. 0.09));
                  tri sun.x sun.y (t.x -. (w *. 0.4)) t.y (t.x +. (w *. 0.4)) t.y
                    (rgb 1. 0.95 0.65 (fade *. pulse *. 0.08)))
                targets;
              (* dust motes rising in the warm air *)
              for i = 0 to 39 do
                let h = Float.rem ((noise i 4 73) +. (age *. (0.05 +. (0.06 *. noise i 5 79)))) 1. in
                let x = min_x +. (noise i 6 83 *. (max_x -. min_x)) +. (sin ((age *. 1.2) +. float i) *. 6.) in
                let y = max_y -. (h *. (max_y -. min_y)) in
                let a = sin (h *. Float.pi) in
                circle x y (1.2 +. (1.4 *. noise i 7 89)) (rgb 1. 0.9 0.5 (fade *. a *. 0.8))
              done;
              (* the warmth settles on the hand *)
              let glow = clamp01 ((age -. 0.6) /. 0.6) in
              circle hand.x hand.y (40. +. (10. *. sin (age *. 4.))) (rgb 1. 0.8 0.3 (fade *. glow *. 0.22));
              circle hand.x hand.y 16. (rgb 1. 0.95 0.65 (fade *. glow *. 0.4))))

(* Pain of essence: the creature's essence is torn out of it. A red filter
   throbs over the card while crimson wisps stream off it; a thread of pain
   then runs to the opponent's portrait, which throbs and bleeds wisps too. *)
let spawn_pain_of_essence ~target ~foe_life ~start =
  let duration = 2.0 in
  let wisps ~c ~w ~h ~count ~t0 ~age ~fade ~seed =
    for i = 0 to count - 1 do
      let lag = noise i seed 97 *. 0.7 in
      let u = clamp01 ((age -. t0 -. lag) /. 0.9) in
      if u > 0. && u < 1. then (
        let bx = c.x +. ((noise i (seed + 1) 101 -. 0.5) *. w) in
        let by = c.y +. ((noise i (seed + 2) 103 -. 0.5) *. h *. 0.6) in
        let a = sin (u *. Float.pi) in
        for tail = 0 to 4 do
          let uu = Float.max 0. (u -. (float tail *. 0.04)) in
          let x = bx +. (sin ((uu *. 7.) +. float i) *. 8.) +. ((bx -. c.x) *. uu *. 0.6) in
          let y = by -. (uu *. (h *. 0.9)) in
          circle x y (4. -. (float tail *. 0.6)) (rgb 1. 0.25 0.3 (a *. fade *. 0.3));
          circle x y (2.2 -. (float tail *. 0.35)) (rgb 1. 0.7 0.75 (a *. fade *. (0.9 -. (float tail *. 0.15))))
        done)
    done
  in
  mk ~start ~duration (fun ~age ->
      let fade = clamp01 (age /. 0.25) *. clamp01 ((duration -. age) /. 0.4) in
      if fade > 0.02 then (
        let throb = 0.65 +. (0.35 *. sin (age *. 14.)) in
        (* red filter over the card *)
        rect (target.x -. 40.) (target.y -. 45.) 80. 90. (rgb 0.85 0.05 0.1 (fade *. throb *. 0.38));
        additive (fun () ->
            circle target.x target.y 44. (rgb 1. 0.15 0.2 (fade *. throb *. 0.22));
            wisps ~c:target ~w:70. ~h:90. ~count:22 ~t0:0.05 ~age ~fade ~seed:1;
            (* thread of pain towards the portrait *)
            let u = clamp01 ((age -. 0.5) /. 0.4) in
            if u > 0. then (
              let n = 24 in
              let dx = foe_life.x -. target.x and dy = foe_life.y -. target.y in
              let len = Float.max 1. (hypot dx dy) in
              let px = -.dy /. len and py = dx /. len in
              let prev = ref None in
              for k = 0 to n do
                let f = float k /. float n in
                if f <= u then (
                  let off = sin ((f *. 14.) -. (age *. 18.)) *. 7. *. sin (f *. Float.pi) in
                  let x = target.x +. (dx *. f) +. (px *. off) and y = target.y +. (dy *. f) +. (py *. off) in
                  (match !prev with
                  | Some (x0, y0) ->
                      line x0 y0 x y 4. (rgb 1. 0.15 0.25 (fade *. 0.35));
                      line x0 y0 x y 1.5 (rgb 1. 0.8 0.85 (fade *. 0.9))
                  | None -> ());
                  prev := Some (x, y))
              done));
        (* the same pain on the portrait *)
        let hit = clamp01 ((age -. 0.85) /. 0.25) in
        if hit > 0. then (
          rect (foe_life.x -. 42.) (foe_life.y -. 42.) 84. 84. (rgb 0.85 0.05 0.1 (fade *. throb *. 0.4 *. hit));
          additive (fun () ->
              circle foe_life.x foe_life.y (50. *. hit) (rgb 1. 0.15 0.2 (fade *. throb *. 0.25));
              wisps ~c:foe_life ~w:70. ~h:80. ~count:14 ~t0:0.9 ~age ~fade ~seed:5))))

(* Bravery: golden sparkles flare over each of the caster's creatures as their
   courage (attack) swells for the turn. *)
let spawn_bravery ~spots ~start =
  let duration = 1.5 in
  mk ~start ~duration (fun ~age ->
      let fade = clamp01 (age /. 0.2) *. clamp01 ((duration -. age) /. 0.4) in
      if fade > 0.02 then
        additive (fun () ->
            Array.iteri
              (fun i c ->
                for k = 0 to 13 do
                  let px = c.x +. ((noise (i * 20 + k) 1 17 -. 0.5) *. 72.) in
                  let py = c.y +. ((noise (i * 20 + k) 2 19 -. 0.5) *. 82.) -. (age *. 6.) in
                  let phase = Float.rem ((age *. (1.1 +. (0.6 *. noise (i * 20 + k) 3 23))) +. noise (i * 20 + k) 4 29) 1. in
                  let a = sin (phase *. Float.pi) in
                  let a = a *. a *. fade in
                  if a > 0.03 then (
                    let len = 2. +. (6. *. a) in
                    let col = rgb 1. (0.82 +. (0.12 *. noise k 5 31)) 0.3 (a *. 0.95) in
                    line (px -. len) py (px +. len) py 1.3 col;
                    line px (py -. len) px (py +. len) 1.3 col;
                    circle px py (1.4 +. a) (rgb 1. 0.97 0.7 a))
                done;
                circle c.x c.y 38. (rgb 1. 0.8 0.25 (fade *. 0.1)))
              spots))

(* Lavender nectar: green healing motes and purple lavender motes spiral up
   together around the creature, then a few drift to the caster's house for
   the power gained. *)
let spawn_lavender_nectar ~target ~house ~start =
  let duration = 1.8 in
  mk ~start ~duration (fun ~age ->
      let fade = clamp01 (age /. 0.2) *. clamp01 ((duration -. age) /. 0.4) in
      if fade > 0.02 then
        additive (fun () ->
            let grow = ease_out (clamp01 (age /. 0.4)) in
            circle target.x (target.y +. 10.) (40. *. grow) (rgb 0.4 0.9 0.4 (fade *. 0.12));
            circle target.x (target.y +. 10.) (28. *. grow) (rgb 0.7 0.5 1. (fade *. 0.12));
            for i = 0 to 47 do
              let purple = i mod 2 = 1 in
              let h = Float.rem ((float i /. 48.) +. (age *. 0.55)) 1. in
              let ang = (age *. 4.) +. (float i *. 2.399) +. if purple then Float.pi else 0. in
              let rad = (14. +. (16. *. sin (h *. Float.pi))) *. grow in
              let x = target.x +. (cos ang *. rad) in
              let y = target.y +. 40. -. (h *. 86.) +. (sin ang *. rad *. 0.25) in
              let a = sin (h *. Float.pi) *. fade in
              let sz = 1.6 +. (1.6 *. noise i 1 107) in
              if purple then (
                circle x y (sz *. 2.2) (rgb 0.65 0.4 1. (a *. 0.2));
                circle x y sz (rgb 0.85 0.7 1. (a *. 0.95)))
              else (
                circle x y (sz *. 2.2) (rgb 0.3 0.85 0.3 (a *. 0.2));
                circle x y sz (rgb 0.7 1. 0.6 (a *. 0.95)))
            done;
            (* the power gained flies to the house *)
            Option.iter
              (fun hs ->
                for i = 0 to 5 do
                  let u = clamp01 ((age -. 0.7 -. (float i *. 0.06)) /. 0.8) in
                  if u > 0. && u < 1. then (
                    let e = u *. u *. (3. -. (2. *. u)) in
                    let x = target.x +. ((hs.x -. target.x) *. e) +. (sin (u *. 9. +. float i) *. 10. *. (1. -. u)) in
                    let y = target.y +. ((hs.y -. target.y) *. e) in
                    let col = if i mod 2 = 0 then rgb 0.7 1. 0.6 else rgb 0.85 0.7 1. in
                    circle x y 3.4 (col (fade *. 0.9)))
                done)
              house))

(* Force barrier: a golden hexagon snaps around the creature, edge by edge. A
   window-pane glint sweeps across its glass (twice, the second fainter), then
   the whole shield fades out. *)
let spawn_force_barrier ~target ~start =
  let duration = 2.0 in
  let hex rad = Array.init 6 (fun i ->
      let a = (Float.pi /. 6.) +. (float i *. Float.pi /. 3.) in
      (target.x +. (cos a *. rad), target.y +. 4. +. (sin a *. rad)))
  in
  (* keep the part of a convex polygon where [side p >= 0] *)
  let clip side poly =
    let n = Array.length poly in
    let out = ref [] in
    for i = 0 to n - 1 do
      let p = poly.(i) and q = poly.((i + 1) mod n) in
      let sp = side p and sq = side q in
      if sp >= 0. then out := p :: !out;
      if (sp >= 0.) <> (sq >= 0.) then (
        let t = sp /. (sp -. sq) in
        out := (fst p +. ((fst q -. fst p) *. t), snd p +. ((snd q -. snd p) *. t)) :: !out)
    done;
    Array.of_list (List.rev !out)
  in
  let fan poly c =
    for i = 1 to Array.length poly - 2 do
      tri (fst poly.(0)) (snd poly.(0)) (fst poly.(i)) (snd poly.(i)) (fst poly.(i + 1)) (snd poly.(i + 1)) c
    done
  in
  mk ~start ~duration (fun ~age ->
      let fade = clamp01 ((duration -. age) /. 0.6) in
      if fade > 0.02 then (
        let appear = ease_out (clamp01 (age /. 0.35)) in
        let rad = 68. *. (1.25 -. (0.25 *. appear)) in
        let outer = hex rad and inner = hex (rad *. 0.82) in
        let flash = clamp01 (1. -. (age /. 0.5)) in
        additive (fun () ->
            (* glass tint, brightest at the snap *)
            fan outer (rgb 1. 0.78 0.25 (fade *. appear *. (0.12 +. (0.2 *. flash))));
            (* glints: diagonal bands (direction u) sliding across the pane *)
            let ux = 0.8 and uy = -0.6 in
            let glint t0 width strength =
              let u = clamp01 ((age -. t0) /. 0.7) in
              if u > 0. && u < 1. then (
                let centre = lerp (-.rad *. 1.2) (rad *. 1.2) (ease_out u) in
                let proj (x, y) = ((x -. target.x) *. ux) +. ((y -. target.y) *. uy) -. centre in
                let slab w poly = clip (fun p -> w +. proj p) (clip (fun p -> w -. proj p) poly) in
                let band = slab width outer in
                let a = sin (u *. Float.pi) *. strength *. fade in
                fan band (rgb 1. 0.95 0.7 (a *. 0.55));
                let core = slab (width *. 0.3) outer in
                fan core (rgb 1. 1. 0.9 (a *. 0.6)))
            in
            glint 0.25 11. 1.;
            glint 0.55 5. 0.7;
            (* inner pane outline *)
            for i = 0 to 5 do
              let x0, y0 = inner.(i) and x1, y1 = inner.((i + 1) mod 6) in
              line x0 y0 x1 y1 1. (rgb 1. 0.85 0.4 (fade *. appear *. 0.35))
            done;
            (* frame: edges draw in one after another *)
            for i = 0 to 5 do
              let u = clamp01 ((appear *. 6.) -. float i) in
              if u > 0. then (
                let x0, y0 = outer.(i) and x1, y1 = outer.((i + 1) mod 6) in
                let x1 = lerp x0 x1 u and y1 = lerp y0 y1 u in
                let pulse = 0.85 +. (0.15 *. sin ((age *. 9.) +. float i)) in
                line x0 y0 x1 y1 7. (rgb 1. 0.6 0.1 (fade *. 0.18 *. pulse));
                line x0 y0 x1 y1 3.5 (rgb 1. 0.78 0.25 (fade *. 0.55 *. pulse));
                line x0 y0 x1 y1 1.6 (rgb 1. 0.97 0.75 (fade *. pulse)))
            done;
            (* corner sparks *)
            Array.iteri
              (fun i (x, y) ->
                let tw = 0.5 +. (0.5 *. sin ((age *. 12.) +. (float i *. 1.7))) in
                circle x y 6. (rgb 1. 0.8 0.3 (fade *. appear *. 0.2 *. tw));
                circle x y 2.2 (rgb 1. 1. 0.85 (fade *. appear *. (0.5 +. (0.5 *. tw)))))
              outer)))

(* Warp reality: space buckles around the creature. The card itself is bent by the warp shader (the
   board reads [warp_amount]); here come the violet and cyan rings that collapse onto it, glitch
   bars tearing across, and motes pulled into the spiral. *)
let spawn_warp_reality ~target ~start =
  let duration = 1.6 in
  mk ~start ~duration (fun ~age ->
      let amount = clamp01 (age /. 0.3) *. clamp01 ((duration -. age) /. 0.5) in
      mark_warp target amount;
      if amount > 0.02 then
        additive (fun () ->
            let cy = target.y +. 4. in
            (* rings collapse inward, one after another *)
            for k = 0 to 2 do
              let u = Float.rem ((age *. 0.9) +. (float k /. 3.)) 1. in
              let rad = 78. *. (1. -. u) in
              let a = sin (u *. Float.pi) *. amount in
              let col = if k mod 2 = 0 then rgb 0.6 0.35 1. else rgb 0.3 0.95 1. in
              for i = 0 to 31 do
                let a0 = (float i /. 32.) *. Float.pi *. 2. and a1 = (float (i + 1) /. 32.) *. Float.pi *. 2. in
                let wob r ang = r *. (1. +. (0.08 *. sin ((ang *. 5.) +. (age *. 8.) +. float k))) in
                line
                  (target.x +. (cos a0 *. wob rad a0)) (cy +. (sin a0 *. wob rad a0))
                  (target.x +. (cos a1 *. wob rad a1)) (cy +. (sin a1 *. wob rad a1))
                  2. (col (a *. 0.55))
              done
            done;
            (* glitch bars *)
            let tick = int_of_float (age *. 14.) in
            for i = 0 to 5 do
              if noise i tick 3 > 0.6 then (
                let y = cy -. 40. +. (noise i tick 5 *. 80.) in
                let w = 20. +. (noise i tick 7 *. 50.) in
                let x = target.x -. 40. +. (noise i tick 9 *. 60.) in
                let col = if i mod 2 = 0 then rgb 0.3 0.95 1. else rgb 0.75 0.4 1. in
                rect x y w 2. (col (amount *. 0.5)))
            done;
            (* motes spiral into the centre *)
            for i = 0 to 23 do
              let h = Float.rem ((float i /. 24.) +. (age *. 0.8)) 1. in
              let ang = (float i *. 2.399) +. (age *. 5.) +. (h *. 4.) in
              let rad = 64. *. (1. -. h) in
              let x = target.x +. (cos ang *. rad) and y = cy +. (sin ang *. rad) in
              let a = sin (h *. Float.pi) *. amount in
              circle x y 3.5 (rgb 0.6 0.4 1. (a *. 0.2));
              circle x y 1.4 (rgb 0.85 0.95 1. (a *. 0.9))
            done))

(* Powermancer Equilibrium: taxes the opponent's highest power pool to strike their life,
   and channels vitality from the player's lowest pool to heal the player.
   A fiery/crimson-gold taxing ray surges from the opponent's highest house label into their
   portrait, while a vibrant emerald-green ray flows from the player's lowest house label
   into the player's portrait with rising healing motes. *)
let spawn_equilibrium ~opp_house ~opp_life ~own_house ~own_life ~start =
  let duration = 1.4 in
  mk ~start ~duration (fun ~age ->
      let fade = clamp01 (age /. 0.12) *. clamp01 ((duration -. age) /. 0.22) in
      if fade > 0.01 then
        additive (fun () ->
            let throb = 0.85 +. (0.15 *. sin (age *. 16.)) in
            (* Opponent highest power label aura (amber-crimson) *)
            rect (opp_house.x -. 48.) (opp_house.y -. 14.) 96. 28. (rgb 1. 0.3 0.1 (fade *. throb *. 0.28));
            circle opp_house.x opp_house.y (14. +. (4. *. sin (age *. 20.))) (rgb 1. 0.45 0.15 (fade *. 0.4));
            circle opp_house.x opp_house.y 5. (rgb 1. 0.95 0.8 (fade *. 0.8));

            (* Player lowest power label aura (emerald green) *)
            rect (own_house.x -. 48.) (own_house.y -. 14.) 96. 28. (rgb 0.2 0.95 0.35 (fade *. throb *. 0.28));
            circle own_house.x own_house.y (14. +. (4. *. sin (age *. 18.))) (rgb 0.25 1. 0.4 (fade *. 0.4));
            circle own_house.x own_house.y 5. (rgb 0.9 1. 0.88 (fade *. 0.8));

            (* Opponent taxing ray: opp_house -> opp_life *)
            let dx_opp = opp_life.x -. opp_house.x and dy_opp = opp_life.y -. opp_house.y in
            let len_opp = Float.max 1. (hypot dx_opp dy_opp) in
            let px_opp = -.dy_opp /. len_opp and py_opp = dx_opp /. len_opp in
            let grow_opp = clamp01 (age /. 0.2) in
            if grow_opp > 0. then (
              let steps = 30 in
              let prev = ref None in
              let prev_w1 = ref None in
              let prev_w2 = ref None in
              for k = 0 to steps do
                let f = float k /. float steps in
                if f <= grow_opp then (
                  let x = opp_house.x +. (dx_opp *. f) in
                  let y = opp_house.y +. (dy_opp *. f) in
                  let arch = sin (f *. Float.pi) in
                  let w1 = sin ((f *. 18.) -. (age *. 28.)) *. 6. *. arch in
                  let w2 = sin ((f *. 26.) +. (age *. 32.)) *. 4. *. arch in
                  let x_w1 = x +. (px_opp *. w1) and y_w1 = y +. (py_opp *. w1) in
                  let x_w2 = x +. (px_opp *. w2) and y_w2 = y +. (py_opp *. w2) in
                  (match !prev with
                  | Some (x0, y0) ->
                      line x0 y0 x y 9. (rgb 1. 0.2 0.1 (fade *. 0.3));
                      line x0 y0 x y 3.8 (rgb 1. 0.65 0.2 (fade *. 0.72));
                      line x0 y0 x y 1.6 (rgb 1. 0.96 0.85 (fade *. 0.95))
                  | None -> ());
                  (match !prev_w1 with
                  | Some (x0, y0) ->
                      line x0 y0 x_w1 y_w1 2.2 (rgb 1. 0.35 0.15 (fade *. 0.55));
                      line x0 y0 x_w1 y_w1 1.0 (rgb 1. 0.9 0.7 (fade *. 0.8))
                  | None -> ());
                  (match !prev_w2 with
                  | Some (x0, y0) -> line x0 y0 x_w2 y_w2 1.4 (rgb 0.9 0.15 0.35 (fade *. 0.45))
                  | None -> ());
                  prev := Some (x, y);
                  prev_w1 := Some (x_w1, y_w1);
                  prev_w2 := Some (x_w2, y_w2))
              done;
              for i = 0 to 8 do
                let speed = 1.2 +. (noise i 1 4 *. 0.6) in
                let phase = Float.rem ((age *. speed) +. (float i *. 0.11)) 1.0 in
                if phase <= grow_opp then (
                  let sx = opp_house.x +. (dx_opp *. phase) in
                  let sy = opp_house.y +. (dy_opp *. phase) in
                  let pfade = fade *. sin (phase *. Float.pi) in
                  circle sx sy 3.0 (rgb 1. 0.4 0.1 (pfade *. 0.5));
                  circle sx sy 1.4 (rgb 1. 0.95 0.8 (pfade *. 0.9)))
              done);

            (* Impact on opp_life *)
            if age >= 0.2 then (
              let imp_age = age -. 0.2 in
              let imp_fade = clamp01 ((duration -. age) /. 0.25) in
              let pulse = 0.5 +. (0.5 *. sin (imp_age *. 22.)) in
              circle opp_life.x opp_life.y (24. +. (10. *. pulse)) (rgb 1. 0.2 0.1 (imp_fade *. 0.24));
              circle opp_life.x opp_life.y (13. +. (5. *. pulse)) (rgb 1. 0.6 0.15 (imp_fade *. 0.45));
              circle opp_life.x opp_life.y 5.5 (rgb 1. 0.95 0.85 (imp_fade *. 0.85));
              for i = 0 to 11 do
                let ang = (float i *. (Float.pi *. 2. /. 12.)) +. (noise i 7 1 *. 0.4) in
                let spd = 28. +. (noise i 8 2 *. 36.) in
                let dist = imp_age *. spd in
                let sp_fade = imp_fade *. clamp01 (1. -. (dist /. 48.)) in
                if sp_fade > 0.02 then (
                  let sx = opp_life.x +. (cos ang *. dist) in
                  let sy = opp_life.y +. (sin ang *. dist) in
                  circle sx sy 2.0 (rgb 1. 0.7 0.2 sp_fade);
                  circle sx sy 0.9 (rgb 1. 0.95 0.85 sp_fade))
              done);

            (* Player Healing: particle transfer from own_house to own_life *)
            let dx_own = own_life.x -. own_house.x and dy_own = own_life.y -. own_house.y in
            let lift = 45. +. Float.min 35. (Float.abs dx_own *. 0.08) in
            let c1x = own_house.x +. (dx_own *. 0.25) and c1y = own_house.y +. (dy_own *. 0.15) -. lift in
            let c2x = own_house.x +. (dx_own *. 0.75) and c2y = own_house.y +. (dy_own *. 0.85) -. (lift *. 0.35) in
            let cx u = bezier own_house.x c1x c2x own_life.x u in
            let cy u = bezier own_house.y c1y c2y own_life.y u in
            let len_own = Float.max 1. (hypot dx_own dy_own) in
            let px_own = -.dy_own /. len_own and py_own = dx_own /. len_own in
            for i = 0 to 41 do
              let delay = (float (i mod 14) *. 0.038) +. (noise i 1 7 *. 0.10) in
              let a = age -. delay in
              let travel = 0.50 +. (noise i 2 8 *. 0.16) in
              if a >= 0. && a <= travel then (
                let u = a /. travel in
                let bx = cx u and by = cy u in
                let arch = sin (u *. Float.pi) in
                let side = (noise i 3 9 -. 0.5) *. 18. *. arch in
                let wave = sin ((a *. 14.) +. float i) *. (7. *. arch) in
                let x = bx +. (px_own *. (side +. wave)) in
                let y = by +. (py_own *. (side +. wave)) in
                let p_fade = fade *. arch in
                let rad = 2.4 +. (noise i 4 5 *. 1.4) in
                (* Trail behind mote *)
                let du = 0.025 in
                let tx = cx (Float.min 1. (u +. du)) -. cx (Float.max 0. (u -. du)) in
                let ty = cy (Float.min 1. (u +. du)) -. cy (Float.max 0. (u -. du)) in
                let tlen = Float.max 0.001 (hypot tx ty) in
                let vx = tx /. tlen and vy = ty /. tlen in
                for s = 1 to 3 do
                  let d = float s *. 4.5 in
                  let tr_x = x -. (vx *. d) and tr_y = y -. (vy *. d) in
                  let tr_fade = p_fade *. (1. -. (float s *. 0.28)) in
                  circle tr_x tr_y (rad *. (1. -. (float s *. 0.22))) (rgb 0.25 0.92 0.38 (tr_fade *. 0.35))
                done;
                circle x y (rad *. 2.4) (rgb 0.15 0.9 0.35 (p_fade *. 0.28));
                circle x y (rad *. 1.2) (rgb 0.45 1. 0.55 (p_fade *. 0.75));
                circle x y (rad *. 0.5) (rgb 0.9 1. 0.88 (p_fade *. 0.95)))
            done;

            (* Healing absorption radiance on own_life *)
            if age >= 0.3 then (
              let heal_age = age -. 0.3 in
              let heal_fade = clamp01 ((duration -. age) /. 0.25) in
              let pulse = 0.5 +. (0.5 *. sin (heal_age *. 13.)) in
              circle own_life.x own_life.y (24. +. (10. *. pulse)) (rgb 0.15 0.92 0.38 (heal_fade *. 0.22));
              circle own_life.x own_life.y (13. +. (5. *. pulse)) (rgb 0.4 1. 0.6 (heal_fade *. 0.38));
              circle own_life.x own_life.y 5.5 (rgb 0.9 1. 0.9 (heal_fade *. 0.8));
              for i = 0 to 9 do
                let t_i = heal_age -. (float i *. 0.08) in
                if t_i > 0. then (
                  let u = clamp01 (t_i /. 0.65) in
                  let mote_fade = heal_fade *. (1. -. u) in
                  if mote_fade > 0.02 then (
                    let ox = (noise i 4 5 -. 0.5) *. 38. in
                    let oy = -. (u *. 42.) +. ((noise i 5 6 -. 0.5) *. 10.) in
                    circle (own_life.x +. ox) (own_life.y +. oy) 2.2 (rgb 0.4 1. 0.55 (mote_fade *. 0.7));
                    circle (own_life.x +. ox) (own_life.y +. oy) 1.0 (rgb 0.95 1. 0.95 mote_fade)))
              done)))

(* Arcane Trap: the pentagram flares up in flames over the victim's slots while violet smoke curls off the house
   labels that lose mana. The flames die away and leave the scorch mark drawn by [Fx_core.draw_pentagram]. *)
let spawn_arcane_trap ~houses ~center ~start =
  let duration = 2.6 in
  let strokes = pentagram_strokes (pentagram_points center.x center.y (trap_radius *. 0.9)) in
  mk ~start ~duration (fun ~age ->
      let env = clamp01 (age /. 0.35) *. clamp01 ((duration -. age) /. 1.0) in
      if env > 0.02 then
        List.iteri
          (fun i ((x0, y0), (x1, y1)) ->
            let n = 26 in
            for k = 0 to n do
              let u = float k /. float n in
              (* the fire runs along the stroke from its first corner *)
              let lit = clamp01 ((age *. 1.8) -. (u *. 0.6)) in
              let h = (14. +. (16. *. noise i k 5)) *. env *. lit in
              if h > 1. then draw_flame (lerp x0 x1 u) (lerp y0 y1 u) ~w:(4. +. (3. *. noise i k 7)) ~h age (i * 40 + k) (env *. lit)
            done)
          strokes;
      Array.iteri
        (fun j h ->
          for k = 0 to 11 do
            let t0 = (0.05 *. float k) +. (0.04 *. float j) in
            let u = (age -. t0) /. 1.7 in
            if u > 0. && u < 1. then (
              let fade = Float.min 1. (u /. 0.12) *. ((1. -. u) ** 1.4) in
              let x = h.x +. ((noise j k 1 -. 0.5) *. 70.) +. (sin ((age *. 2.2) +. float (k + j)) *. 6. *. u) in
              let y = h.y +. 4. -. (u *. (30. +. (16. *. noise j k 2))) in
              let rad = 8. +. (18. *. u *. (0.6 +. (0.6 *. noise j k 3))) in
              circle x y rad (rgb 0.2 0.12 0.3 (0.5 *. fade));
              circle (x +. 2.) (y -. 2.) (rad *. 0.6) (rgb 0.5 0.32 0.72 (0.3 *. fade)))
          done)
        houses)

(* Sonorous Warcry: a shout bursts from the caster and gold pressure rings roll
   across the enemy row, slamming every creature and the portrait. *)
let spawn_warcry ~source ~foe ~foe_life ~foe_pid ~start =
  let duration = 1.25 in
  let n = Array.length foe in
  let dest =
    if n = 0 then foe_life
    else
      {
        x = Array.fold_left (fun s p -> s +. p.x) 0. foe /. float n;
        y = Array.fold_left (fun s p -> s +. p.y) 0. foe /. float n;
      }
  in
  let span =
    if n = 0 then 160.
    else
      let xmin = Array.fold_left (fun m p -> min m p.x) foe.(0).x foe in
      let xmax = Array.fold_left (fun m p -> max m p.x) foe.(0).x foe in
      Float.max 140. (xmax -. xmin +. 90.)
  in
  let dx = dest.x -. source.x and dy = dest.y -. source.y in
  let dist = Float.max 1. (hypot dx dy) in
  let nx = dx /. dist and ny = dy /. dist in
  let px = -.ny and py = nx in
  let heading = atan2 ny nx in
  Array.iteri (fun i _ -> add_shake foe_pid i ~start:(start +. 0.28) ~duration:0.42 ~strength:0.9) foe;
  let oval cx cy rx ry thick col =
    let steps = 28 in
    let tau = Float.pi *. 2. in
    for k = 0 to steps - 1 do
      let a0 = tau *. float k /. float steps and a1 = tau *. float (k + 1) /. float steps in
      line
        (cx +. (cos a0 *. rx))
        (cy +. (sin a0 *. ry))
        (cx +. (cos a1 *. rx))
        (cy +. (sin a1 *. ry))
        thick col
    done
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.28) in
      if fade > 0.02 then
        additive (fun () ->
            let burst = Float.max 0. (1. -. (age /. 0.22)) in
            if burst > 0.02 then (
              circle source.x source.y (18. +. ((1. -. burst) *. 36.)) (rgb 1. 0.72 0.22 (fade *. burst *. 0.45));
              circle source.x source.y (8. +. ((1. -. burst) *. 10.)) (rgb 1. 0.95 0.7 (fade *. burst *. 0.9));
              for k = 0 to 11 do
                let ang = heading +. ((float k -. 5.5) *. 0.13) +. ((noise k 1 3 -. 0.5) *. 0.08) in
                let len = (22. +. (float k *. 2.4) +. (noise k 2 5 *. 10.)) *. (1. -. burst *. 0.25) in
                let x1 = source.x +. (cos ang *. len) and y1 = source.y +. (sin ang *. len) in
                line source.x source.y x1 y1 3.2 (rgb 1. 0.7 0.2 (fade *. burst *. 0.45));
                line source.x source.y x1 y1 1.3 (rgb 1. 0.95 0.7 (fade *. burst *. 0.9))
              done);
            for w = 0 to 4 do
              let born = float w *. 0.07 in
              let a = age -. born in
              if a > 0. then (
                let u = ease_out (clamp01 (a /. 0.62)) in
                let life = clamp01 (1. -. ((a -. 0.55) /. 0.4)) in
                let al = fade *. life in
                if al > 0.02 then (
                  let cx = source.x +. (dx *. u *. 0.55) in
                  let cy = source.y +. (dy *. u *. 0.55) in
                  let rx = 16. +. (u *. (span *. 0.62)) in
                  let ry = 10. +. (u *. (dist *. 0.42)) in
                  let thick = 3.6 -. (u *. 1.8) in
                  oval cx cy rx ry thick (rgb 0.95 0.55 0.12 (al *. 0.35));
                  oval cx cy rx ry (thick *. 0.45) (rgb 1. 0.92 0.55 (al *. 0.85));
                  oval cx cy (rx *. 0.92) (ry *. 0.92) 1.1 (rgb 1. 0.78 0.3 (al *. 0.4))))
            done;
            (* the shout flattens into a pressure wall across the enemy row *)
            let wall = clamp01 ((age -. 0.24) /. 0.18) in
            if wall > 0. && n > 0 then (
              let gone = clamp01 ((age -. 0.42) /. 0.55) in
              let al = fade *. (1. -. gone) *. (0.4 +. (0.6 *. sin (wall *. Float.pi))) in
              if al > 0.02 then (
                let rx = span *. 0.52 *. ease_out wall in
                oval dest.x dest.y rx (22. +. (10. *. wall)) 3.4 (rgb 1. 0.62 0.18 (al *. 0.55));
                oval dest.x dest.y rx (18. +. (8. *. wall)) 1.4 (rgb 1. 0.95 0.65 al);
                circle dest.x dest.y (rx *. 0.15) (rgb 1. 0.85 0.4 (al *. 0.18))));
            Array.iteri
              (fun i (p : spot) ->
                let hit = age -. 0.26 -. (float i *. 0.035) in
                if hit > 0. && hit < 0.7 then (
                  let k = hit /. 0.7 in
                  let al = (1. -. k) *. fade in
                  circle p.x p.y (12. +. (k *. 38.)) (rgb 1. 0.55 0.12 (al *. 0.28));
                  oval p.x p.y (10. +. (k *. 32.)) (7. +. (k *. 22.)) 2.4 (rgb 1. 0.78 0.28 (al *. 0.7));
                  oval p.x p.y (8. +. (k *. 24.)) (5. +. (k *. 16.)) 1.2 (rgb 1. 0.95 0.7 al);
                  (* leftover hush: a tight ring as abilities are stripped *)
                  if hit > 0.22 then (
                    let hush = clamp01 ((hit -. 0.22) /. 0.45) in
                    oval p.x p.y (16. +. (hush *. 4.)) (12. +. (hush *. 3.)) 1.4
                      (rgb 0.95 0.7 0.25 ((1. -. hush) *. fade *. 0.55))));
                (* motes thrown off the hit *)
                if hit > 0. && hit < 0.45 then
                  for m = 0 to 5 do
                    let u = hit /. 0.45 in
                    let ang = heading +. ((float m -. 2.5) *. 0.55) +. (noise i m 7 *. 0.4) in
                    let d = (10. +. (noise i m 9 *. 14.)) *. u in
                    let mx = p.x +. (cos ang *. d) +. (px *. ((noise i m 11 -. 0.5) *. 8.)) in
                    let my = p.y +. (sin ang *. d) +. (py *. ((noise i m 13 -. 0.5) *. 8.)) in
                    circle mx my (2.2 *. (1. -. u)) (rgb 1. 0.9 0.5 ((1. -. u) *. fade *. 0.85))
                  done)
              foe;
            let phit = age -. 0.38 in
            if phit > 0. && phit < 0.75 then (
              let k = phit /. 0.75 in
              let al = (1. -. k) *. fade in
              circle foe_life.x foe_life.y (16. +. (k *. 48.)) (rgb 1. 0.5 0.1 (al *. 0.32));
              oval foe_life.x foe_life.y (14. +. (k *. 40.)) (14. +. (k *. 40.)) 2.6
                (rgb 1. 0.8 0.3 (al *. 0.75));
              circle foe_life.x foe_life.y 10. (rgb 1. 0.95 0.7 al))))
