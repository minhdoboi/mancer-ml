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
