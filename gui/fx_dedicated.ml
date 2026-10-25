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

(* Zen's Inertia pours its own substance into the creature it heals: green motes
   stream out of the stone, run along an arc and settle on the healed card in a
   soft green bloom. *)
let life_flow_duration = 1.1

let spawn_life_flow ~from_ ~to_ ~start =
  let duration = life_flow_duration in
  let dx = to_.x -. from_.x and dy = to_.y -. from_.y in
  let dist = hypot dx dy in
  let lift = 24. +. Float.min 30. (Float.abs dx *. 0.1) in
  let c1x = from_.x +. (dx *. 0.25) and c1y = from_.y +. (dy *. 0.2) -. lift in
  let c2x = from_.x +. (dx *. 0.7) and c2y = from_.y +. (dy *. 0.8) -. (lift *. 0.3) in
  let cx u = bezier from_.x c1x c2x to_.x u in
  let cy u = bezier from_.y c1y c2y to_.y u in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.18) in
      if fade > 0.02 then (
        (* the source gathers a green light before letting it go *)
        let gather = Float.min 1. (age /. 0.3) in
        circle from_.x from_.y (5. +. (gather *. 9.)) (rgb 0.16 0.8 0.26 (fade *. gather *. 0.28));
        circle from_.x from_.y 3. (rgb 0.55 1. 0.5 (fade *. gather *. 0.45));
        additive (fun () ->
            (* the motes themselves: each one runs the arc and fades as it goes *)
            if dist > 12. then
              for i = 0 to 21 do
                let born = (float (i mod 7) *. 0.035) +. (noise i 1 0 *. 0.03) in
                let travel = 0.5 +. (noise i 2 0 *. 0.16) in
                let a = age -. born in
                if a >= 0. && a <= travel +. 0.1 then (
                  let u = Float.min 1. (a /. travel) in
                  let side =
                    (noise i 3 0 -. 0.5) *. 9. *. (1. -. u)
                    *. (0.5 +. (0.5 *. sin ((a *. 12.) +. float i)))
                  in
                  let x = cx u and y = cy u in
                  let tx = cx (Float.min 1. (u +. 0.04)) -. x in
                  let ty = cy (Float.min 1. (u +. 0.04)) -. y in
                  let len = Float.max 0.001 (hypot tx ty) in
                  let px = x +. (-.ty /. len *. side) and py = y +. (tx /. len *. side) in
                  let al = fade *. Float.min 1. (a /. 0.06) *. Float.min 1. ((travel +. 0.1 -. a) /. 0.1) in
                  let rad = 1.2 +. ((1. -. u) *. 1.1) in
                  circle px py (rad *. 2.2) (rgb 0.15 0.75 0.25 (al *. 0.3));
                  circle px py rad (rgb 0.45 1. 0.4 (al *. 0.8));
                  circle px py (rad *. 0.42) (rgb 0.85 1. 0.8 al))
              done;
            (* and the life settles on the healed creature *)
            let settle = Float.min 1. ((age -. 0.4) /. 0.4) in
            if settle > 0.02 then (
              let pulse = 0.65 +. (0.35 *. sin (age *. 11.)) in
              circle to_.x to_.y (9. +. (pulse *. 6.)) (rgb 0.25 0.95 0.35 (fade *. settle *. 0.2 *. pulse));
              circle to_.x to_.y 3.5 (rgb 0.7 1. 0.65 (fade *. settle *. 0.35));
              for k = 0 to 5 do
                let born = 0.45 +. (float k *. 0.06) in
                let a = age -. born in
                if a > 0. then (
                  let u = Float.min 1. (a /. 0.7) in
                  let x = to_.x +. ((noise k 4 0 -. 0.5) *. 26.) in
                  let y = to_.y +. 6. -. (34. *. u) in
                  let al = fade *. settle *. (1. -. u) *. 0.7 in
                  circle x y (2.6 *. (1. -. (u *. 0.4))) (rgb 0.4 1. 0.45 al))
              done))))

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

(* Ghost Invasion: the soul reaper's harvest wisps, drained to dark violet, are
   loosed as a host that pours in at the right edge and sweeps left across the
   foe's lanes, each bursting on the creature it reaches. A wave, not a fan:
   every ghost flies the same right-to-left current, so the rightmost lane is
   hit first. The host rides a sine, so the front ripples as it crosses and
   straightens as each ghost closes on its prey. *)
let ghost_invasion_duration = 1.8

let spawn_ghost_invasion ~row ~targets ~start =
  let duration = ghost_invasion_duration in
  (* the host gathers at the right edge of the board and sweeps left *)
  let right_x = world_w -. 8. in
  let entry_y =
    if Array.length row = 0 then 110.
    else Array.fold_left (fun a (p : spot) -> a +. p.y) 0. row /. float (Array.length row)
  in
  (* the whole board must be crossed in the same current, so a step of [travel]
     is drawn at a constant speed; [speed] sets how fast the wave advances *)
  let speed = 620. in
  (* three wisps per target, fanned vertically, so the line has body *)
  let flights =
    Array.concat
      (Array.to_list
         (Array.mapi
            (fun i to_ ->
              Array.init 3 (fun k ->
                  (to_, (i * 3) + k, (float k *. 0.05), (float k -. 1.) *. 16.)))
            targets))
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.24) in
      if fade > 0.02 then (
        (* the cold gate at the right edge, breathing before the host enters *)
        let puff = Float.min 1. (age /. 0.35) in
        let a0 = (1. -. puff) *. fade *. 0.35 in
        if a0 > 0.03 then (
          circle right_x entry_y (20. +. (puff *. 26.)) (rgb 0.35 0.15 0.6 a0);
          circle (right_x -. 16.) (entry_y +. 26.) (13. +. (puff *. 16.)) (rgb 0.15 0.05 0.3 (a0 *. 0.7)));
        additive (fun () ->
            Array.iter
              (fun (to_, seed, delay, off) ->
                let a = age -. delay in
                if a > 0. then (
                  let jitter = (noise seed 1 3 -. 0.5) *. 24. in
                  (* the path of one wisp: straight across the board, but riding
                     a sine whose ripple dies out as the ghost nears its prey *)
                  let path u =
                    let x = right_x +. ((to_.x -. right_x) *. u) in
                    let env = Float.min 1. ((right_x -. x) /. 70.) in
                    let ph = (u *. 2.3 *. Float.pi) +. (a *. 1.5) +. (float seed *. 0.85) in
                    let sway =
                      ((16. *. sin ph) +. (5. *. sin ((ph *. 2.3) +. 1.1))) *. env
                    in
                    let y = to_.y +. ((off +. jitter) *. (1. -. u)) +. sway in
                    (x, y)
                  in
                  let travel = Float.max 0.08 (Float.abs (to_.x -. right_x) /. speed) in
                  let u = clamp01 (a /. travel) in
                  let px, py = path u in
                  let nx, ny = path (Float.min 1. (u +. 0.04)) in
                  let vx = nx -. px and vy = ny -. py in
                  let len = Float.max 0.001 (hypot vx vy) in
                  let tx = vx /. len and ty = vy /. len in
                  for s = 1 to 5 do
                    let tu = Float.max 0. (u -. (float s *. 0.04)) in
                    let sx, sy = path tu in
                    circle
                      (sx -. (tx *. float s *. 4.))
                      (sy -. (ty *. float s *. 4.))
                      (5.5 -. (float s *. 0.6))
                      (rgb 0.5 0.25 0.85 (fade *. (1. -. (float s /. 6.)) *. 0.16))
                  done;
                  let size = (1. -. (u *. 0.2)) *. 11. in
                  let al = fade *. (0.6 +. (0.25 *. (1. -. u))) in
                  circle
                    (px -. (tx *. size *. 1.8))
                    (py -. (ty *. size *. 1.8))
                    (size *. 0.95)
                    (rgb 0.28 0.05 0.45 (al *. 0.5));
                  circle px py (size *. 0.72) (rgb 0.6 0.3 0.95 (al *. 0.7));
                  circle (px +. (tx *. 2.)) (py +. (ty *. 2.)) (size *. 0.32) (rgb 0.92 0.82 1. al);
                  (* hollow eyes so each wisp reads as a face at a glance *)
                  if size *. 0.2 > 0.8 && u < 0.9 then (
                    let ox = -.ty and oy = tx in
                    circle (px +. (ox *. 2.2)) (py +. (oy *. 2.2)) (size *. 0.2) (rgb 0.12 0. 0.2 (al *. 0.9));
                    circle (px -. (ox *. 2.2)) (py -. (oy *. 2.2)) (size *. 0.2) (rgb 0.12 0. 0.2 (al *. 0.9)));
                  if a >= travel then (
                    let hit = clamp01 ((a -. travel) /. 0.18) in
                    circle to_.x to_.y (10. +. (hit *. 16.))
                      (rgb 0.55 0.25 0.95 (fade *. (1. -. hit) *. 0.5));
                    circle to_.x to_.y 4. (rgb 0.9 0.75 1. (fade *. (1. -. hit))))))
              flights)))

(* Mithril Bow: the enchanted wielder raises a mithril bow and looses an arrow
   down the lane at the creature standing opposite.  The shot belongs to the
   wielder -- it starts at the bow and lands on the foe, not on a portrait. *)
let mithril_bow_duration = 1.05

let spawn_mithril_bow ~from_p ~to_p ~start =
  let duration = mithril_bow_duration in
  let loose = 0.26 (* nocking and drawing *) and fly = 0.34 (* the arrow itself *) in
  let dx = to_p.x -. from_p.x and dy = to_p.y -. from_p.y in
  let span = Float.max 1. (hypot dx dy) in
  let fx = dx /. span and fy = dy /. span in
  let ax = -.fy and ay = fx in
  (* the arrow sails on a shallow arc instead of sliding flat *)
  let lift = 16. +. Float.min 26. (Float.abs dy *. 0.3) in
  let px u = from_p.x +. (dx *. u) in
  let py u = from_p.y +. (dy *. u) -. (lift *. 4. *. u *. (1. -. u)) in
  (* the bow stands across the shot, its belly bulging toward the target *)
  let tip a =
    let belly = (1. -. (a *. a)) *. 10. in
    (from_p.x +. (ax *. (a *. 24.)) +. (fx *. belly), from_p.y +. (ay *. (a *. 24.)) +. (fy *. belly))
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.2) in
      let draw = ease_out (clamp01 (age /. loose)) in
      let u = clamp01 ((age -. loose) /. fly) in
      let shot = age >= loose in
      let held = fade *. (if shot then Float.max 0. (1. -. ((age -. loose) /. 0.5)) else 1.) in
      if fade > 0.02 then
        additive (fun () ->
            (* the wielder glows while the bow takes shape, and dims once it is loosed *)
            circle from_p.x from_p.y (26. +. (draw *. 10.))
              (rgb 0.5 0.7 1. (fade *. 0.12 *. (if shot then Float.max 0.35 (1. -. u) else 1.)));
            (* the bow itself: pale limb, taut string, a spark where the nock sits *)
            if held > 0.02 then (
              let pull = if shot then 2. else 4. +. (draw *. 9.) in
              let nx = from_p.x -. (fx *. pull) and ny = from_p.y -. (fy *. pull) in
              for s = 0 to 7 do
                let a0 = -1. +. (float s *. 0.25) in
                let x0, y0 = tip a0 and x1, y1 = tip (a0 +. 0.25) in
                line x0 y0 x1 y1 (2. +. (draw *. 1.2)) (rgb 0.5 0.58 0.74 (held *. 0.5));
                line x0 y0 x1 y1 1. (rgb 0.85 0.92 1. (held *. 0.9))
              done;
              let lx0, ly0 = tip (-1.) and lx1, ly1 = tip 1. in
              line lx0 ly0 nx ny 1.1 (rgb 0.7 0.8 0.95 (held *. 0.85));
              line nx ny lx1 ly1 1.1 (rgb 0.7 0.8 0.95 (held *. 0.85));
              circle nx ny (2. +. (draw *. 1.4)) (rgb 0.9 0.96 1. (held *. (if shot then 0.3 else 0.9))));
            if shot then
              if u < 1. then (
                (* arrowhead, shaft and the motes it drags behind it *)
                let sx = px u and sy = py u in
                let vx = px (Float.min 1. (u +. 0.05)) -. sx and vy = py (Float.min 1. (u +. 0.05)) -. sy in
                let vl = Float.max 0.001 (hypot vx vy) in
                let tx = vx /. vl and ty = vy /. vl in
                for k = 1 to 5 do
                  let tu = Float.max 0. (u -. (float k *. 0.05)) in
                  circle (px tu) (py tu) (3.4 -. (float k *. 0.5))
                    (rgb 0.55 0.75 1. (fade *. 0.42 *. (1. -. (float k /. 6.))))
                done;
                line (sx -. (tx *. 15.)) (sy -. (ty *. 15.)) sx sy 2.4 (rgb 0.68 0.84 1. (fade *. 0.75));
                tri
                  (sx +. (tx *. 5.))
                  (sy +. (ty *. 5.))
                  (sx -. (ty *. 3.2))
                  (sy +. (tx *. 3.2))
                  (sx +. (ty *. 3.2))
                  (sy -. (tx *. 3.2))
                  (rgb 0.9 0.96 1. fade))
              else (
                (* impact: the arrow rings the creature it struck *)
                let k = Float.min 1. ((age -. (loose +. fly)) /. 0.34) in
                circle to_p.x to_p.y (6. +. (k *. 26.)) (rgb 0.6 0.8 1. ((1. -. k) *. fade *. 0.4));
                circle to_p.x to_p.y (3. +. (k *. 12.)) (rgb 0.92 0.97 1. ((1. -. k) *. fade *. 0.7));
                for k = 0 to 5 do
                  let ang = (float k *. (Float.pi /. 3.)) +. 0.35 in
                  let r0 = 8. +. (float k *. 12.) in
                  let r1 = r0 +. (10. *. (1. -. (float k *. 0.12))) in
                  line
                    (to_p.x +. (cos ang *. r0))
                    (to_p.y +. (sin ang *. r0))
                    (to_p.x +. (cos ang *. r1))
                    (to_p.y +. (sin ang *. r1))
                    2.
                    (rgb 0.85 0.94 1. ((1. -. (float k *. 0.14)) *. fade *. 0.6))
                done)))

(* Excalibur: the enchanted wielder does not shoot -- the sword goes through
   them.  It is drawn on the wielder first, held a moment, then hurled down the
   lane, and it comes out the far side of the wielder into the lane standing
   opposite, where it stands a while before it is gone.  With nothing in the
   lane the same blade carries on into the opponent. *)
let excalibur_duration = 2.05
let excalibur_lunge = 1.15 (* when the finished sword is let go *)

let spawn_excalibur ~wielder ~through ~start =
  let duration = excalibur_duration in
  let forge = 0.85 in (* the sword takes shape on the wielder *)
  let hold = excalibur_lunge -. forge in (* and hangs there, complete *)
  let drive = 0.22 and stick = 0.68 in (* the lunge, then the pause in the far card *)
  let dx = through.x -. wielder.x and dy = through.y -. wielder.y in
  let span = Float.max 1. (hypot dx dy) in
  let fx = dx /. span and fy = dy /. span in
  let ax = -.fy and ay = fx in
  (* the sword is longer than half the gap, so the hilt is still in the wielder
     when the point comes out the other side *)
  let blade_len = 104. and grip_len = 36. and guard_span = 38. in
  let blade_w = 7. and guard_t = 2.4 and grip_w = 3. and pommel_r = 6.5 in
  let reach = span +. (blade_len *. 0.55) in
  let px u = wielder.x +. (fx *. (reach *. u)) in
  let py u = wielder.y +. (fy *. (reach *. u)) in
  (* Excalibur: a straight double-edged blade of even width that tapers only to
     the point, a long straight bar for two hands, a long grip and a wheel
     pommel.  [u] is where the point is on the axis; every measure below is in
     board pixels back from it, and [size] grows the whole sword at once. *)
  let poly p q r c = tri (fst p) (snd p) (fst q) (snd q) (fst r) (snd r) c in
  let quad p q r s c =
    poly p q r c;
    poly p r s c
  in
  let sword u size alpha ~glint =
    let back p off =
      let v = u -. ((p *. size) /. reach) in
      (px v +. (ax *. (off *. size)), py v +. (ay *. (off *. size)))
    in
    let edge off p q c =
      let pp = back p off and qq = back q off in
      line (fst pp) (snd pp) (fst qq) (snd qq) 1.1 c
    in
    (* the blade: parallel edges for four fifths of it, a taper for the rest *)
    let t0 = back 0. 0. and t1 = back 20. blade_w and t2 = back 20. (-.blade_w) in
    let b1 = back (blade_len -. 18.) blade_w and b2 = back (blade_len -. 18.) (-.blade_w) in
    let g1 = back blade_len (blade_w *. 0.5) and g2 = back blade_len (-.(blade_w *. 0.5)) in
    poly t0 t1 t2 (rgb 0.6 0.66 0.8 (alpha *. 0.55));
    quad t1 b1 b2 t2 (rgb 0.86 0.91 1. (alpha *. 0.72));
    quad b1 g1 g2 b2 (rgb 0.6 0.66 0.8 (alpha *. 0.55));
    (* the fuller down the middle, and both edges lit: this is a blade of light *)
    edge 0. 26. (blade_len -. 14.) (rgb 1. 1. 1. (alpha *. 0.45));
    edge blade_w 20. (blade_len -. 18.) (rgb 1. 0.98 0.9 (alpha *. 0.9));
    edge (-.blade_w) 20. (blade_len -. 18.)
      (rgb 0.85 0.93 1. (alpha *. (if glint >= 0. then 0.5 else 0.75)));
    (* crossguard: a straight bar for two hands, its ends flared to the point *)
    let gc = back blade_len 0. and gl = back blade_len guard_span and gr = back blade_len (-.guard_span) in
    let cx = back (blade_len -. guard_t) 0. in
    let cw = back (blade_len -. guard_t) guard_span and cw2 = back (blade_len -. guard_t) (-.guard_span) in
    let gold = rgb (if glint >= 0. then 0.7 else 0.85) 0.7 0.33 (alpha *. 0.95) in
    quad gc gl cw cx gold;
    quad gc cx cw2 gr gold;
    poly (back (blade_len -. 3.) (guard_span +. 4.)) (back (blade_len +. 3.) (guard_span -. 3.))
      (back (blade_len +. 6.) (guard_span +. 1.))
      (rgb 0.92 0.79 0.42 (alpha *. 0.9));
    poly (back (blade_len -. 3.) (-.(guard_span +. 4.))) (back (blade_len -. 3.) (-.(guard_span -. 3.)))
      (back (blade_len +. 6.) (-.(guard_span +. 1.)))
      (rgb 0.92 0.79 0.42 (alpha *. 0.9));
    (* the grip, long enough for both hands and wound in leather *)
    let h0 = back (blade_len +. guard_t *. 0.5) grip_w
    and h1 = back (blade_len +. grip_len -. (guard_t *. 0.5)) grip_w in
    let h2 = back (blade_len +. grip_len -. (guard_t *. 0.5)) (-.grip_w)
    and h3 = back (blade_len +. (guard_t *. 0.5)) (-.grip_w) in
    quad h0 h1 h2 h3 (rgb 0.3 0.2 0.13 (alpha *. 0.95));
    for k = 0 to 3 do
      let p = blade_len +. (3. +. (float k *. (grip_len /. 4.6))) in
      let a = back p grip_w and b = back p (-.grip_w) in
      line (fst a) (snd a) (fst b) (snd b) (0.9 *. size) (rgb 0.62 0.5 0.36 (alpha *. 0.7))
    done;
    (* the wheel pommel *)
    let pcx, pcy = back (blade_len +. grip_len +. 2.) 0. in
    circle pcx pcy (pommel_r *. size) (rgb 0.88 0.74 0.36 (alpha *. 0.95));
    circle pcx pcy (pommel_r *. 0.62 *. size) (rgb 0.6 0.48 0.22 (alpha *. 0.9));
    circle pcx pcy (pommel_r *. 0.3 *. size) (rgb 1. 0.92 0.66 (alpha *. 0.95))
  in
  (* the runes the sword is called out of, and the light it drags up the lane *)
  let rune_ring t alpha =
    let rad = 34. -. (16. *. t) in
    for k = 0 to 17 do
      let ang = (float k *. (Float.pi *. 2. /. 18.)) +. (t *. 2.6) in
      let r0 = rad *. 0.8 and r1 = rad *. 1.06 in
      line
        (wielder.x +. (cos ang *. r0))
        (wielder.y +. (sin ang *. r0 *. 0.62))
        (wielder.x +. (cos ang *. r1))
        (wielder.y +. (sin ang *. r1 *. 0.62))
        2.
        (rgb 1. 0.85 0.42 (alpha *. 0.7))
    done;
    ellipse wielder.x wielder.y (rad *. 0.9) (rad *. 0.9 *. 0.62) (rgb 1. 0.9 0.55 (alpha *. 0.5))
  in
  let shaft u alpha =
    for k = 0 to 5 do
      let x0, y0 = (px (u -. (float k *. 0.05)), py (u -. (float k *. 0.05))) in
      let x1, y1 = (px (u -. (float k *. 0.05) -. 0.45), py (u -. (float k *. 0.05) -. 0.45)) in
      line x0 y0 x1 y1 2. (rgb 1. 0.88 0.5 ((alpha *. 0.5) *. (1. -. (float k /. 6.))))
    done
  in
  let home = blade_len /. reach in
  mk ~start ~duration (fun ~age ->
      let age = Float.max 0. age in
      let fade = Float.min 1. ((duration -. age) /. 0.3) in
      let shaped = age < forge in
      let forge_t = clamp01 (age /. forge) in
      let size = if shaped then ease_out forge_t else 1. in
      (* at rest the blade stands on the wielder, point up the lane *)
      let lunge_t = clamp01 ((age -. excalibur_lunge) /. drive) in
      let u = if age < excalibur_lunge then home else home +. ((1. -. home) *. (lunge_t *. lunge_t)) in
      let gone = age -. (excalibur_lunge +. drive) in
      (* it stands in the far card for a beat before the light lets go of it:
         the first fifth of the pause is held at full, the rest is the going *)
      let fade_out = stick *. 0.8 in
      let a = if gone > 0. then Float.max 0. (1. -. (gone /. fade_out)) else 1. in
      (* a glint runs the length of the blade while it hangs whole *)
      let whole = age >= forge && age < excalibur_lunge && a = 1. in
      let glint = if whole then mod_float (age -. forge) (hold /. 2.) /. (hold /. 2.) else -1. in
      if fade > 0.02 then
        additive (fun () ->
            if shaped then (
              (* the runes draw in, the wielder burns gold under them *)
              circle wielder.x wielder.y (26. +. (forge_t *. 16.))
                (rgb 1. 0.82 0.4 (fade *. 0.12 *. forge_t));
              rune_ring forge_t (fade *. 0.9);
              (* motes falling in to feed the blade *)
              for k = 0 to 11 do
                let n = noise k 5 21 in
                let r = 46. *. (1. -. (n *. 0.6 *. size)) +. 16. in
                let ang = (n *. 6.28) +. (age *. 3.) in
                circle (wielder.x +. (cos ang *. r)) (wielder.y +. (sin ang *. r *. 0.62)) (1.6 +. n)
                  (rgb 1. 0.88 0.5 (fade *. 0.5 *. size))
              done);
            (* whole: it hangs at the wielder and the light stands off it *)
            if whole then circle wielder.x wielder.y 30. (rgb 1. 0.85 0.45 (fade *. 0.07));
            if lunge_t > 0. && lunge_t < 1. then shaft u (fade *. lunge_t);
            let sword_a = fade *. a *. (if shaped then 0.35 +. (0.65 *. size) else 1.) in
            if sword_a > 0.02 then (
              (* a shimmer runs the length of the blade, once or twice, while it
                 stands whole at the wielder *)
              if glint >= 0. then (
                let v = u -. (((14. +. (glint *. (blade_len -. 30.))) *. size) /. reach) in
                circle (px v) (py v) 2.6 (rgb 1. 1. 0.95 (fade *. 0.9)));
              sword u size sword_a ~glint));
      (* where the point came out: the far card is cut open by a cross of light *)
      if gone > 0. && gone < 0.5 then
        additive (fun () ->
            let k = gone /. 0.5 in
            let g = (1. -. k) *. fade in
            circle through.x through.y (10. +. (k *. 46.)) (rgb 1. 0.85 0.45 (g *. 0.5));
            circle through.x through.y (4. +. (k *. 18.))
              (rgb 1. 1. 0.95 (g *. Float.max 0. (1. -. ((k -. 0.35) /. 0.65))));
            let arm = 32. +. (k *. 36.) and w = 7. *. (1. -. k) +. 1.5 in
            line (through.x -. arm) through.y (through.x +. arm) through.y w (rgb 1. 0.95 0.75 (g *. 0.9));
            line through.x (through.y -. (arm *. 0.8)) through.x (through.y +. (arm *. 0.8)) w
              (rgb 1. 0.95 0.75 (g *. 0.9));
            (* the light tears into shards as it dies *)
            for s = 0 to 7 do
              let ang = (float s *. (Float.pi /. 4.)) +. 0.4 in
              let r0 = 10. +. (k *. 16.) in
              let r1 = r0 +. (26. *. (1. -. k)) +. 6. in
              line
                (through.x +. (cos ang *. r0))
                (through.y +. (sin ang *. r0 *. 0.8))
                (through.x +. (cos ang *. r1))
                (through.y +. (sin ang *. r1 *. 0.8))
                2.4 (rgb 1. 0.88 0.55 (g *. 0.7))
            done))

(* Bargulian Gauntlet: the wielder's own fist comes up burning, draws back, and
   punches forward; the shock it throws then runs the whole of the opponent's row
   from the lane it came out of, and every creature on that row is hit by it. *)
let gauntlet_duration = 1.65

let spawn_gauntlet ~wielder ~foe ~foe_pid ~start =
  let duration = gauntlet_duration in
  let wind = 0.42 in (* the fist forms on the wielder and draws back *)
  let punch = 0.16 and wave = 0.62 in (* the punch, then the shock crossing the row *)
  let n = Array.length foe in
  let mid_x = if n = 0 then wielder.x else Array.fold_left (fun s (p : spot) -> s +. p.x) 0. foe /. float n in
  let mid_y = if n = 0 then wielder.y -. 140. else Array.fold_left (fun s (p : spot) -> s +. p.y) 0. foe /. float n in
  (* the fist punches up its own lane, then the shock spreads sideways *)
  let dl = Float.max 1. (hypot (mid_x -. wielder.x) (mid_y -. wielder.y)) in
  let ux = (mid_x -. wielder.x) /. dl and uy = (mid_y -. wielder.y) /. dl in
  let vx = -.uy and vy = ux in
  let reach = Float.min 120. (dl *. 0.5) in
  (* the shock runs out to each end of the row in its own time *)
  let far_l =
    Array.fold_left (fun m (p : spot) -> Float.max m (Float.max 40. (wielder.x -. p.x))) 40. foe
  in
  let far_r =
    Array.fold_left (fun m (p : spot) -> Float.max m (Float.max 40. (p.x -. wielder.x))) 40. foe
  in
  let far_of x = if x <= wielder.x then far_l else far_r in
  let hit i =
    let p = foe.(i) in
    wind +. punch +. (Float.abs (p.x -. wielder.x) /. far_of p.x *. wave)
  in
  Array.iteri (fun i _ -> add_shake foe_pid i ~start:(start +. hit i) ~duration:0.45 ~strength:1.05) foe;
  (* one gauntlet: a fist of dark iron with fire in the seams, knuckles first *)
  let fist cx cy s alpha =
    let plate = 27. *. s in
    let knuck = 11. *. s and step = 13.5 *. s in
    let at u o = (cx +. (ux *. u) +. (vx *. o), cy +. (uy *. u) +. (vy *. o)) in
    let poly p q r c = tri (fst p) (snd p) (fst q) (snd q) (fst r) (snd r) c in
    let ln p q w c = line (fst p) (snd p) (fst q) (snd q) w c in
    let dot p r c = circle (fst p) (snd p) r c in
    (* the cuff behind the hand, widening toward the arm *)
    let c0 = at (-.plate) (plate *. 0.55) and c1 = at (-.plate) (-.(plate *. 0.55)) in
    let c2 = at (-.(plate *. 2.1)) (plate *. 0.85) and c3 = at (-.(plate *. 2.1)) (-.(plate *. 0.85)) in
    poly c0 c1 c2 (rgb 0.4 0.11 0.04 (alpha *. 0.8));
    poly c0 c2 c3 (rgb 0.28 0.07 0.03 (alpha *. 0.8));
    let band = at (-.(plate *. 1.25)) (plate *. 0.66) in
    let band2 = at (-.(plate *. 1.25)) (-.(plate *. 0.66)) in
    let band_e = at (-.(plate *. 1.45)) (plate *. 0.7) in
    let band_e2 = at (-.(plate *. 1.45)) (-.(plate *. 0.7)) in
    poly band band2 band_e (rgb 1. 0.5 0.16 (alpha *. 0.7));
    poly band band_e band_e2 (rgb 1. 0.5 0.16 (alpha *. 0.7));
    (* the hand *)
    dot (cx, cy) plate (rgb 0.46 0.14 0.05 (alpha *. 0.85));
    dot (cx, cy) (plate *. 0.78) (rgb 0.34 0.09 0.04 (alpha *. 0.8));
    (* the thumb, wrapped round the side of the hand: the one thing that makes
       the shape read as a fist rather than a block *)
    let th = at (plate *. 0.15) (plate *. 0.92) in
    dot th (knuck *. 0.92) (rgb 0.44 0.13 0.05 (alpha *. 0.9));
    dot th (knuck *. 0.5) (rgb 1. 0.58 0.2 (alpha *. 0.85));
    (* fire in the seams *)
    for k = 0 to 4 do
      let o = ((float k /. 4.) -. 0.5) *. (plate *. 1.5) in
      let p = at (-.(plate *. 0.75)) o and q = at (plate *. 0.5) (o *. 0.7) in
      ln p q (1.6 *. s) (rgb 1. 0.42 0.1 (alpha *. 0.75))
    done;
    (* the knuckles, arced across the striking face so the hand reads as a fist *)
    for k = 0 to 3 do
      let f = (float k /. 3.) -. 0.5 in
      let o = f *. step *. 3. in
      let p = at (plate *. 0.62 -. (Float.abs f *. 5. *. s)) o in
      dot p knuck (rgb 0.5 0.16 0.06 (alpha *. 0.92));
      dot p (knuck *. 0.52) (rgb 1. 0.62 0.22 (alpha *. 0.92));
      (* the light along the top of each knuckle *)
      let a = at (plate *. 0.62 -. (Float.abs f *. 5. *. s)) (o -. (knuck *. 0.55)) in
      let b = at (plate *. 0.62 -. (Float.abs f *. 5. *. s) +. (knuck *. 0.7)) (o -. (knuck *. 0.3)) in
      ln a b (1.5 *. s) (rgb 1. 0.85 0.5 (alpha *. 0.9))
    done
  in
  (* the shock front: a bright wall running sideways across the row *)
  let front ~age fx fy alpha =
    for s = -4 to 4 do
      let u = float s /. 4. in
      let bulge = ((1. -. (u *. u)) *. 13.) +. 4. in
      let y = fy +. (u *. 48.) in
      let wob = sin ((age *. 16.) +. (u *. 5.)) *. 3. in
      line (fx +. (vx *. (bulge *. -1.)) +. (wob *. vx)) (y -. 9.) (fx +. (vx *. bulge)) (y +. 9.)
        11. (rgb 1. 0.42 0.12 (alpha *. 0.22));
      line (fx +. (vx *. bulge)) y (fx +. (vx *. (bulge *. 1.25))) (y +. (9. *. u))
        (2.2) (rgb 1. 0.85 0.55 (alpha *. 0.9))
    done;
    circle fx fy 9. (rgb 1. 0.8 0.45 (alpha *. 0.5))
  in
  mk ~start ~duration (fun ~age ->
      let age = Float.max 0. age in
      let fade = Float.min 1. ((duration -. age) /. 0.3) in
      let wound = clamp01 (age /. wind) in
      let struck = age -. (wind +. punch) in
      (* the fist: back behind the wielder, then driven up the lane *)
      let punch_t = Float.min 1. ((age -. wind) /. punch) in
      (* it draws back behind the wielder, then is driven up the lane *)
      let back = (-26.) +. (26. *. (1. -. (wound *. wound))) in
      let reach_at = if age < wind +. punch then back +. ((reach -. back) *. (punch_t ** 3.)) else reach in
      let fist_a = fade *. (if age < wind then 0.35 +. (0.65 *. wound) else Float.max 0. (1. -. ((age -. (wind +. punch)) /. (wave *. 0.5)))) in
      if fade > 0.02 then
        additive (fun () ->
            if age < wind then (
              (* it gathers on the wielder: embers and a ring closing on the fist *)
              circle wielder.x wielder.y (58. -. (30. *. wound)) (rgb 1. 0.5 0.15 (fade *. 0.16));
              for k = 0 to 13 do
                let n = noise k 3 31 in
                let r = 62. *. (1. -. (n *. wound)) +. 14. in
                let ang = (n *. 6.28) +. (age *. 2.4) in
                circle (wielder.x +. (cos ang *. r *. vx)) (wielder.y +. (sin ang *. r *. vy)) (1.8 +. (n *. 2.))
                  (rgb 1. 0.62 0.22 (fade *. 0.55 *. wound))
              done);
            if fist_a > 0.02 then (
              circle wielder.x wielder.y (26. *. fist_a) (rgb 1. 0.45 0.12 (fade *. 0.1 *. fist_a));
              fist
                (wielder.x +. (ux *. reach_at))
                (wielder.y +. (uy *. reach_at))
                (0.55 +. (0.45 *. wound))
                fist_a);
            (* the shock leaves the fist and runs the row *)
            if struck > 0. && struck < wave then (
              let spread = ease_out (clamp01 (struck /. wave)) in
              let life = fade *. Float.max 0. (1. -. (struck /. wave)) in
              front ~age (wielder.x -. (((spread *. far_l) +. 40.) *. vx)) mid_y life;
              front ~age (wielder.x +. (((spread *. far_r) +. 40.) *. vx)) mid_y life;
              (* sparks thrown ahead of the front *)
              for k = 0 to 9 do
                let n = noise k 7 13 in
                let dy = ((n -. 0.5) *. 90.) *. vy in
                let off = ((n -. 0.5) *. 60.) *. vx in
                line (wielder.x +. (off *. (struck /. wave))) (mid_y +. (dy *. (struck /. wave)))
                  (wielder.x +. (off *. (struck /. wave)) +. (vx *. 14.))
                  (mid_y +. (dy *. (struck /. wave)) +. (vy *. 14.))
                  2. (rgb 1. 0.7 0.3 (life *. 0.6))
              done);
            (* and every lane it crosses is struck *)
            Array.iteri
              (fun i (p : spot) ->
                let a = age -. hit i in
                if a > 0. && a < 0.5 then (
                  let k = a /. 0.5 in
                  let g = fade *. (1. -. k) in
                  circle p.x p.y (9. +. (k *. 40.)) (rgb 1. 0.45 0.12 (g *. 0.45));
                  circle p.x p.y (4. +. (k *. 14.)) (rgb 1. 0.92 0.7 (g *. (1. -. (k *. 0.5))));
                  (* the fist leaves its mark: two gashes and a burst of shards *)
                  line (p.x -. 34.) (p.y -. 26.) (p.x +. 30.) (p.y +. 34.) (6. *. (1. -. k) +. 1.)
                    (rgb 1. 0.75 0.4 (g *. 0.9));
                  line (p.x +. 34.) (p.y -. 26.) (p.x -. 30.) (p.y +. 34.) (6. *. (1. -. k) +. 1.)
                    (rgb 1. 0.75 0.4 (g *. 0.9));
                  for s = 0 to 7 do
                    let ang = (float s *. (Float.pi /. 4.)) +. 0.3 +. (noise i s 9 *. 0.4) in
                    let r0 = 12. +. (k *. 14.) in
                    let r1 = r0 +. (24. *. (1. -. k)) +. 5. in
                    line
                      (p.x +. (cos ang *. r0))
                      (p.y +. (sin ang *. r0 *. 0.85))
                      (p.x +. (cos ang *. r1))
                      (p.y +. (sin ang *. r1 *. 0.85))
                      2.6 (rgb 1. 0.6 0.22 (g *. 0.7))
                  done))
              foe)
        )

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
      line
        (center.x +. (cos a0 *. r))
        (center.y +. (sin a0 *. r *. 0.7))
        (center.x +. (cos a1 *. r))
        (center.y +. (sin a1 *. r *. 0.7))
        thick c
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
              let x =
                center.x
                +. ((noise i 0 11 -. 0.5) *. (50. +. (k *. 90.)))
                +. (sin ((k *. 3.) +. float i) *. 14.)
              in
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
                line center.x center.y
                  (center.x +. (cos ang *. len))
                  (center.y +. (sin ang *. len *. 0.7))
                  (6. +. (burst *. 14.))
                  (rgb 1. 0.85 0.5 (burst *. 0.7 *. fade))
              done;
            (* fireball: boiling blobs thrown outward, white -> yellow -> red *)
            for i = 0 to 59 do
              let grow = clamp01 (age /. (0.55 +. (noise i 4 11 *. 0.5))) in
              let ang = noise i 5 11 *. 2. *. Float.pi in
              let dist = big *. 0.55 *. noise i 6 11 *. ease_out grow in
              let x = center.x +. (cos ang *. dist)
              and y = center.y +. (sin ang *. dist *. 0.7) -. (age *. 14.) in
              let heat = clamp01 (1. -. (age /. (1.1 +. (noise i 7 11 *. 0.7)))) in
              let rad =
                (28. +. (noise i 8 11 *. 40.)) *. (0.5 +. (0.6 *. ease_out grow)) *. (0.5 +. (0.5 *. heat))
              in
              let g = 0.25 +. (0.65 *. heat) and b = 0.03 +. (0.5 *. heat *. heat) in
              circle x y rad (rgb 1. g b (0.2 *. heat *. fade));
              circle x y (rad *. 0.5) (rgb 1. (g +. 0.1) (b +. 0.2) (0.26 *. heat *. fade))
            done;
            (* white-hot core *)
            let core = clamp01 (1. -. (age /. 0.7)) in
            if core > 0. then (
              circle center.x center.y
                (70. +. (ease_out (age /. 0.5) *. 90.))
                (rgb 1. 0.95 0.75 (core *. 0.55 *. fade));
              circle center.x center.y 40. (rgb 1. 1. 1. (core *. 0.8 *. fade)));
            (* shockwave *)
            if age < wave_t +. 0.5 then (
              let r = big *. ease_out (Float.min 1. (age /. wave_t)) in
              let f = clamp01 (1. -. (age /. (wave_t +. 0.5))) in
              ring r ((14. *. f) +. 2.) (rgb 1. 0.45 0.1 (0.35 *. f *. fade));
              ring r ((5. *. f) +. 1.) (rgb 1. 0.9 0.6 (0.85 *. f *. fade)));
            (* sparks and debris *)
            for i = 0 to 49 do
              let a = age -. (noise i 9 11 *. 0.12) in
              if a > 0. && a < 1.3 then
                let k = a /. 1.3 in
                let ang = noise i 10 11 *. 2. *. Float.pi in
                let sp = 240. +. (noise i 11 11 *. 380.) in
                let d = sp *. (1. -. ((1. -. k) *. (1. -. k))) in
                let x = center.x +. (cos ang *. d)
                and y = center.y +. (sin ang *. d *. 0.7) +. (k *. k *. 120.) in
                let tx = x -. (cos ang *. 14. *. (1. -. k)) and ty = y -. (sin ang *. 10. *. (1. -. k)) in
                line tx ty x y 2.2 (rgb 1. (0.75 -. (0.4 *. k)) 0.25 ((1. -. k) *. fade))
            done;
            (* flames bursting on each creature as the shockwave reaches it *)
            Array.iteri
              (fun j (p : spot) ->
                let d = hypot (p.x -. center.x) ((p.y -. center.y) /. 0.7) in
                let hit_t = wave_t *. (1. -. sqrt (1. -. Float.min 1. (d /. big))) in
                let a = age -. hit_t in
                if a > 0. && a < 0.9 then
                  let k = a /. 0.9 in
                  for i = 0 to 7 do
                    let ang = noise i j 12 *. 2. *. Float.pi in
                    let dist = (10. +. (noise i j 13 *. 36.)) *. ease_out k in
                    let x = p.x +. (cos ang *. dist) and y = p.y +. (sin ang *. dist) -. (k *. 40.) in
                    let r = 14. -. (8. *. k) in
                    circle x y r (rgb 1. (0.5 -. (0.3 *. k)) 0.05 ((1. -. k) *. 0.45 *. fade));
                    circle x y (r *. 0.45) (rgb 1. 0.9 0.5 ((1. -. k) *. 0.6 *. fade))
                  done)
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
          circle
            (floor.x +. (cos ang *. rr))
            (floor.y -. 3. +. (sin ang *. rr *. 0.28))
            (6. +. (5. *. noise k 2 9))
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
          let size = (1.4 +. (1.8 *. noise i 3 9)) *. if behind then 0.8 else 1.15 in
          let shade = 0.55 +. (0.4 *. noise i 5 11) in
          circle x y size
            (rgb
               ((0.92 *. shade) +. 0.08)
               ((0.72 *. shade) +. 0.05)
               (0.4 *. shade)
               (fade *. if behind then 0.55 else 0.95))
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
                    (rgb 0.82 0.93 1. (fade *. if front then 0.85 else 0.45))
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
                  let curl = sin (u *. Float.pi) *. 28. *. if i mod 2 = 0 then 1. else -1. in
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
                circle
                  (foe_life.x +. (cos ang *. r *. 1.3))
                  (foe_life.y +. (sin ang *. r *. 1.3))
                  2.5
                  (rgb 1. 0.93 0.5 ((1. -. hit) *. 0.9))
              done)))

(* Solar Burst: joy's answer to Stone Shower.  A curtain of solar light rolls
   across the board the way the aurora does, and every creature slot the wave
   touches is left burning -- flames and embers cling to the cards, own and foe
   alike, until the sun dies down. *)
let solar_burst_duration = 3.0
let solar_burst_travel = 1.15 (* seconds for the wave to cross the board *)

let spawn_solar_burst ~floors ~start =
  let duration = solar_burst_duration in
  let n = Array.length floors in
  if n = 0 then mk ~start ~duration:0. (fun ~age:_ -> ())
  else
    let xs = Array.map (fun (p : spot) -> p.x) floors
    and ys = Array.map (fun (p : spot) -> p.y) floors in
    let x_min = Array.fold_left Float.min infinity xs -. 70.
    and x_max = Array.fold_left Float.max neg_infinity xs +. 70. in
    let y_top = Array.fold_left Float.min infinity ys -. 130.
    and y_bot = Array.fold_left Float.max neg_infinity ys +. 30. in
    let span = Float.max 1. (x_max -. x_min) in
    let speed = span /. solar_burst_travel in
    (* gold, ember-orange and white-hot curtains, like the aurora's greens *)
    let curtains =
      [| (1., 0.82, 0.3, 0.0, 0.9); (1., 0.5, 0.08, 1.6, 0.75); (1., 0.96, 0.72, 3.0, 0.65) |]
    in
    mk ~start ~duration (fun ~age ->
        let fade = Float.min 1. ((duration -. age) /. 0.7) in
        if fade > 0.02 then
          additive (fun () ->
              (* the rolling curtain of light, brightest at its leading edge *)
              Array.iteri
                (fun ci (r, g, b, ph, amp) ->
                  let x = ref x_min in
                  while !x <= x_max do
                    let local = age -. ((!x -. x_min) /. speed) in
                    if local > 0. then (
                      (* the front rises fast, the body of light lingers behind it *)
                      let heat = clamp01 (local /. 0.22) *. exp (-.Float.max 0. (local -. 0.3) /. 0.75) in
                      if heat > 0.03 then (
                        let wave =
                          sin ((!x *. 0.02) +. (age *. 1.1) +. ph)
                          +. (0.5 *. sin ((!x *. 0.05) -. (age *. 2.4) +. ph))
                        in
                        let top =
                          y_top +. ((y_bot -. y_top) *. (0.04 +. (0.13 *. float ci))) +. (wave *. 26.)
                        in
                        let len =
                          (y_bot -. y_top)
                          *. (0.5 +. (0.22 *. sin ((!x *. 0.03) +. (age *. 0.9) +. ph)))
                        in
                        let shim = 0.55 +. (0.45 *. sin ((!x *. 0.12) +. (age *. 3.4) +. ph)) in
                        let seg = 8 in
                        for m = 0 to seg - 1 do
                          let u0 = float m /. float seg and u1 = float (m + 1) /. float seg in
                          let al =
                            fade *. amp *. heat *. shim *. 0.22 *. ((1. -. u0) ** 1.6)
                            *. Float.min 1. ((u0 *. 8.) +. 0.12)
                          in
                          line !x (top +. (len *. u0)) !x (top +. (len *. u1)) 9. (rgb r g b al)
                        done;
                        if local < 0.45 then
                          let crest = (1. -. (local /. 0.45)) *. fade in
                          line !x top !x (top +. len) 3.5 (rgb 1. 0.98 0.86 (crest *. 0.5))));
                    x := !x +. 6.
                  done)
                curtains;
              (* a warm wash over the whole field *)
              circle ((x_min +. x_max) /. 2.) ((y_top +. y_bot) /. 2.) (span *. 0.5)
                (rgb 1. 0.6 0.15 (fade *. 0.05));
              (* each slot the sun touches is left burning *)
              Array.iteri
                (fun j (p : spot) ->
                  let bt = age -. ((p.x -. x_min) /. speed) in
                  if bt > 0. then (
                    let ign = clamp01 (bt /. 0.4) in
                    let life = Float.min 1. ((duration -. age) /. 0.9) in
                    if life > 0.02 then (
                      let lick = sin ((age *. 11.) +. float j) in
                      let h = (72. +. (26. *. lick) +. (noise j 3 5 *. 30.)) *. ign *. life in
                      flame_column ~x:p.x ~y:p.y ~height:h ~fat:1.5 ~hot:0.9
                        ~alpha:(fade *. ign *. life) ~t:age ~id:j ~licks:3 ~push:0.;
                      (* the scorched glow left on the card *)
                      circle p.x (p.y -. 10.) (24. +. (8. *. ign))
                        (rgb 1. 0.4 0.05 (fade *. ign *. life *. 0.22));
                      circle p.x (p.y -. 14.) (10. +. (4. *. ign))
                        (rgb 1. 0.85 0.4 (fade *. ign *. life *. 0.3));
                      (* embers lifting off the burning slot *)
                      for e = 0 to 9 do
                        let ea = bt -. (float e *. 0.09) in
                        if ea > 0. && ea < 0.9 then (
                          let k = ea /. 0.9 in
                          let ex =
                            p.x +. ((noise j e 7 -. 0.5) *. 44.) +. (sin ((k *. 6.) +. float e) *. 6.)
                          in
                          let ey = p.y -. 10. -. ((80. +. (60. *. noise j e 8)) *. k) in
                          circle ex ey 3.2 (rgb 1. 0.4 0.05 ((1. -. k) *. 0.3 *. fade *. life));
                          circle ex ey 1.3 (rgb 1. 0.85 0.4 ((1. -. k) *. 0.85 *. fade *. life)))
                      done)))
                floors))

(* Fusion: joy's strongest two creatures pour their energy into one point, the
   two colours braid and merge into a single orb, and only then is the fused
   strike hurled at the opponent. *)
let fusion_duration = 1.8
let fusion_gather = 0.5 (* when the two energies reach the meeting point *)
let fusion_merge = 0.95 (* when they have become one orb *)
let fusion_hit = 1.35 (* when the orb lands on the opponent *)

(* Joy runs pink, yellow and red. *)
let fusion_color k =
  match k mod 3 with
  | 0 -> (1., 0.45, 0.72)
  | 1 -> (1., 0.86, 0.3)
  | _ -> (1., 0.28, 0.22)

let spawn_fusion ~froms ~to_ ~start =
  let duration = fusion_duration in
  if Array.length froms = 0 then mk ~start ~duration:0. (fun ~age:_ -> ())
  else begin
    let count = Array.length froms in
    (* the two creatures meet in the middle, lifted a little above them *)
    let cx = Array.fold_left (fun s (p : spot) -> s +. p.x) 0. froms /. float count in
    let cy = Array.fold_left (fun s (p : spot) -> s +. p.y) 0. froms /. float count in
    let mx = cx and my = cy -. 46. in
    (* Two creatures fuse: one colour from each.  Only one creature on the
       board still fuses two energies -- two colours streaming from it. *)
    let streams =
      if count >= 2 then Array.to_list (Array.mapi (fun j p -> (p, j)) froms)
      else [ (froms.(0), 0); (froms.(0), 2) ]
    in
    mk ~start ~duration (fun ~age ->
        let fade = Float.min 1. ((duration -. age) /. 0.25) in
        if fade > 0.02 then (
          (* the creatures charge before their energy leaves them *)
          if age < fusion_gather then
            Array.iteri
              (fun j (from_ : spot) ->
                let charge = clamp01 (age /. 0.32) in
                if charge > 0.02 then (
                  let r, g, b = fusion_color j in
                  circle from_.x from_.y (8. +. (20. *. charge))
                    (rgb r g b (charge *. charge *. 0.28 *. fade));
                  circle from_.x from_.y (4. +. (8. *. charge))
                    (rgb 1. 0.95 0.85 (charge *. 0.4 *. fade))))
              froms;
          additive (fun () ->
              (* 1. each colour streams from its creature to the meeting point *)
              if age < fusion_gather then
                List.iteri
                  (fun j ((from_ : spot), ci) ->
                    let dx = mx -. from_.x and dy = my -. from_.y in
                    let lift = 26. +. Float.min 34. (hypot dx dy *. 0.16) in
                    let c1x = from_.x +. (dx *. 0.3) and c1y = from_.y +. (dy *. 0.3) -. lift in
                    let c2x = from_.x +. (dx *. 0.7) and c2y = from_.y +. (dy *. 0.7) -. lift in
                    for i = 0 to 13 do
                      (* every mote leaves a little later, so the colour flies in a stream *)
                      let born = (float (i mod 5) *. 0.05) +. (float j *. 0.05) +. (noise i j 3 *. 0.04) in
                      let a = age -. born in
                      let travel = fusion_gather -. born in
                      if a > 0. && a < travel && travel > 0.05 then (
                        let u = a /. travel in
                        let x = bezier from_.x c1x c2x mx u +. ((noise i j 5 -. 0.5) *. 9.) in
                        let y = bezier from_.y c1y c2y my u +. ((noise i j 6 -. 0.5) *. 9.) in
                        let r, g, b = fusion_color ci in
                        let rad = 2.4 +. (noise i j 7 *. 1.6) in
                        let alpha = fade *. (0.35 +. (0.65 *. sin (Float.pi *. u))) in
                        circle x y (rad *. 2.2) (rgb r g b (alpha *. 0.18));
                        circle x y rad (rgb r g b alpha);
                        circle (x -. (rad *. 0.3)) (y -. (rad *. 0.35)) (rad *. 0.45)
                          (rgb 1. 0.95 0.9 (alpha *. 0.7)))
                    done)
                  streams;
              (* 2. the two energies braid and become one orb *)
              if age >= fusion_gather && age < fusion_merge then (
                let u = clamp01 ((age -. fusion_gather) /. (fusion_merge -. fusion_gather)) in
                let grow = ease_out u in
                let pulse = 1. +. (0.12 *. sin (age *. 22.)) in
                let rad = (10. +. (20. *. grow)) *. pulse in
                circle mx my (rad *. 1.5) (rgb 1. 0.5 0.7 (fade *. 0.16));
                circle mx my rad (rgb 1. 0.8 0.45 (fade *. 0.4));
                circle (mx -. (rad *. 0.25)) (my -. (rad *. 0.3)) (rad *. 0.55)
                  (rgb 1. 0.97 0.9 (fade *. 0.85));
                (* the colours spiralling into the core *)
                for k = 0 to 17 do
                  let ang = (float k *. (Float.pi *. 2. /. 18.)) +. (age *. 5.5) in
                  let rr = 34. *. (1. -. grow) in
                  let r, g, b = fusion_color k in
                  circle (mx +. (cos ang *. rr)) (my +. (sin ang *. rr *. 0.85)) 2.6
                    (rgb r g b (fade *. 0.75))
                done);
              (* 3. the fused strike flies at the opponent *)
              if age >= fusion_merge && age < fusion_hit then (
                let u = clamp01 ((age -. fusion_merge) /. (fusion_hit -. fusion_merge)) in
                let dx = to_.x -. mx and dy = to_.y -. my in
                let lift = 40. +. Float.min 34. (hypot dx dy *. 0.14) in
                let c1x = mx +. (dx *. 0.25) and c1y = my +. (dy *. 0.25) -. lift in
                let c2x = mx +. (dx *. 0.75) and c2y = my +. (dy *. 0.75) -. lift in
                let px = bezier mx c1x c2x to_.x u and py = bezier my c1y c2y to_.y u in
                let rad = 14. +. (8. *. u) in
                (* the trail of fused colour behind the orb *)
                for i = 0 to 14 do
                  let tu = u -. (float i *. 0.035) in
                  if tu > 0. then (
                    let x = bezier mx c1x c2x to_.x tu and y = bezier my c1y c2y to_.y tu in
                    let r, g, b = fusion_color i in
                    let a = 1. -. (float i /. 15.) in
                    circle x y (rad *. 0.8 *. a) (rgb r g b (fade *. 0.45 *. a)))
                done;
                circle px py (rad *. 1.6) (rgb 1. 0.5 0.7 (fade *. 0.28));
                circle px py rad (rgb 1. 0.82 0.5 (fade *. 0.7));
                circle px py (rad *. 0.5) (rgb 1. 0.98 0.92 (fade *. 0.95)));
              (* 4. the fused strike lands *)
              let hit = age -. fusion_hit in
              if hit > 0. then (
                let k = clamp01 (hit /. 0.42) in
                let a = (1. -. k) *. fade in
                circle to_.x to_.y (12. +. (46. *. k)) (rgb 1. 0.5 0.55 (a *. 0.28));
                circle to_.x to_.y (6. +. (22. *. k)) (rgb 1. 0.85 0.55 (a *. 0.55));
                for i = 0 to 17 do
                  let ang = noise i 9 1 *. 2. *. Float.pi in
                  let dist = (14. +. (noise i 9 2 *. 44.)) *. ease_out k in
                  let px = to_.x +. (cos ang *. dist) and py = to_.y +. (sin ang *. dist *. 0.85) in
                  let r, g, b = fusion_color i in
                  circle px py (2.6 +. (noise i 9 3 *. 2.4)) (rgb r g b (a *. 0.85))
                done;
                for i = 0 to 19 do
                  let ang = float i *. (Float.pi *. 2. /. 20.) in
                  let rr = 18. +. (66. *. k) in
                  circle
                    (to_.x +. (cos ang *. rr))
                    (to_.y +. (sin ang *. rr *. 0.72))
                    (0.6 +. (2.8 *. (1. -. k)))
                    (rgb 1. 0.62 0.6 (a *. 0.5))
                done))))
  end

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
                  tri sun.x sun.y
                    (t.x -. (w *. 0.4))
                    t.y
                    (t.x +. (w *. 0.4))
                    t.y
                    (rgb 1. 0.95 0.65 (fade *. pulse *. 0.08)))
                targets;
              (* dust motes rising in the warm air *)
              for i = 0 to 39 do
                let h = Float.rem (noise i 4 73 +. (age *. (0.05 +. (0.06 *. noise i 5 79)))) 1. in
                let x =
                  min_x +. (noise i 6 83 *. (max_x -. min_x)) +. (sin ((age *. 1.2) +. float i) *. 6.)
                in
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
      if u > 0. && u < 1. then
        let bx = c.x +. ((noise i (seed + 1) 101 -. 0.5) *. w) in
        let by = c.y +. ((noise i (seed + 2) 103 -. 0.5) *. h *. 0.6) in
        let a = sin (u *. Float.pi) in
        for tail = 0 to 4 do
          let uu = Float.max 0. (u -. (float tail *. 0.04)) in
          let x = bx +. (sin ((uu *. 7.) +. float i) *. 8.) +. ((bx -. c.x) *. uu *. 0.6) in
          let y = by -. (uu *. (h *. 0.9)) in
          circle x y (4. -. (float tail *. 0.6)) (rgb 1. 0.25 0.3 (a *. fade *. 0.3));
          circle x y
            (2.2 -. (float tail *. 0.35))
            (rgb 1. 0.7 0.75 (a *. fade *. (0.9 -. (float tail *. 0.15))))
        done
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
            if u > 0. then
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
              done);
        (* the same pain on the portrait *)
        let hit = clamp01 ((age -. 0.85) /. 0.25) in
        if hit > 0. then (
          rect (foe_life.x -. 42.) (foe_life.y -. 42.) 84. 84.
            (rgb 0.85 0.05 0.1 (fade *. throb *. 0.4 *. hit));
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
                  let px = c.x +. ((noise ((i * 20) + k) 1 17 -. 0.5) *. 72.) in
                  let py = c.y +. ((noise ((i * 20) + k) 2 19 -. 0.5) *. 82.) -. (age *. 6.) in
                  let phase =
                    Float.rem
                      ((age *. (1.1 +. (0.6 *. noise ((i * 20) + k) 3 23))) +. noise ((i * 20) + k) 4 29)
                      1.
                  in
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
                  if u > 0. && u < 1. then
                    let e = u *. u *. (3. -. (2. *. u)) in
                    let x =
                      target.x +. ((hs.x -. target.x) *. e) +. (sin ((u *. 9.) +. float i) *. 10. *. (1. -. u))
                    in
                    let y = target.y +. ((hs.y -. target.y) *. e) in
                    let col = if i mod 2 = 0 then rgb 0.7 1. 0.6 else rgb 0.85 0.7 1. in
                    circle x y 3.4 (col (fade *. 0.9))
                done)
              house))

(* Force barrier: a golden hexagon snaps around the creature, edge by edge. A
   window-pane glint sweeps across its glass (twice, the second fainter), then
   the whole shield fades out. *)
let spawn_force_barrier ~target ~start =
  let duration = 2.0 in
  let hex rad =
    Array.init 6 (fun i ->
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
      if sp >= 0. <> (sq >= 0.) then
        let t = sp /. (sp -. sq) in
        out := (fst p +. ((fst q -. fst p) *. t), snd p +. ((snd q -. snd p) *. t)) :: !out
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
      if fade > 0.02 then
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
              outer))

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
                let a0 = float i /. 32. *. Float.pi *. 2. and a1 = float (i + 1) /. 32. *. Float.pi *. 2. in
                let wob r ang = r *. (1. +. (0.08 *. sin ((ang *. 5.) +. (age *. 8.) +. float k))) in
                line
                  (target.x +. (cos a0 *. wob rad a0))
                  (cy +. (sin a0 *. wob rad a0))
                  (target.x +. (cos a1 *. wob rad a1))
                  (cy +. (sin a1 *. wob rad a1))
                  2.
                  (col (a *. 0.55))
              done
            done;
            (* glitch bars *)
            let tick = int_of_float (age *. 14.) in
            for i = 0 to 5 do
              if noise i tick 3 > 0.6 then
                let y = cy -. 40. +. (noise i tick 5 *. 80.) in
                let w = 20. +. (noise i tick 7 *. 50.) in
                let x = target.x -. 40. +. (noise i tick 9 *. 60.) in
                let col = if i mod 2 = 0 then rgb 0.3 0.95 1. else rgb 0.75 0.4 1. in
                rect x y w 2. (col (amount *. 0.5))
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
            let c2x = own_house.x +. (dx_own *. 0.75)
            and c2y = own_house.y +. (dy_own *. 0.85) -. (lift *. 0.35) in
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
                if t_i > 0. then
                  let u = clamp01 (t_i /. 0.65) in
                  let mote_fade = heal_fade *. (1. -. u) in
                  if mote_fade > 0.02 then (
                    let ox = (noise i 4 5 -. 0.5) *. 38. in
                    let oy = -.(u *. 42.) +. ((noise i 5 6 -. 0.5) *. 10.) in
                    circle (own_life.x +. ox) (own_life.y +. oy) 2.2 (rgb 0.4 1. 0.55 (mote_fade *. 0.7));
                    circle (own_life.x +. ox) (own_life.y +. oy) 1.0 (rgb 0.95 1. 0.95 mote_fade))
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
              if h > 1. then
                draw_flame (lerp x0 x1 u) (lerp y0 y1 u)
                  ~w:(4. +. (3. *. noise i k 7))
                  ~h age
                  ((i * 40) + k)
                  (env *. lit)
            done)
          strokes;
      Array.iteri
        (fun j h ->
          for k = 0 to 11 do
            let t0 = (0.05 *. float k) +. (0.04 *. float j) in
            let u = (age -. t0) /. 1.7 in
            if u > 0. && u < 1. then (
              let fade = Float.min 1. (u /. 0.12) *. ((1. -. u) ** 1.4) in
              let x =
                h.x +. ((noise j k 1 -. 0.5) *. 70.) +. (sin ((age *. 2.2) +. float (k + j)) *. 6. *. u)
              in
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
              circle source.x source.y
                (18. +. ((1. -. burst) *. 36.))
                (rgb 1. 0.72 0.22 (fade *. burst *. 0.45));
              circle source.x source.y (8. +. ((1. -. burst) *. 10.)) (rgb 1. 0.95 0.7 (fade *. burst *. 0.9));
              for k = 0 to 11 do
                let ang = heading +. ((float k -. 5.5) *. 0.13) +. ((noise k 1 3 -. 0.5) *. 0.08) in
                let len = (22. +. (float k *. 2.4) +. (noise k 2 5 *. 10.)) *. (1. -. (burst *. 0.25)) in
                let x1 = source.x +. (cos ang *. len) and y1 = source.y +. (sin ang *. len) in
                line source.x source.y x1 y1 3.2 (rgb 1. 0.7 0.2 (fade *. burst *. 0.45));
                line source.x source.y x1 y1 1.3 (rgb 1. 0.95 0.7 (fade *. burst *. 0.9))
              done);
            for w = 0 to 4 do
              let born = float w *. 0.07 in
              let a = age -. born in
              if a > 0. then
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
                  oval cx cy (rx *. 0.92) (ry *. 0.92) 1.1 (rgb 1. 0.78 0.3 (al *. 0.4)))
            done;
            (* the shout flattens into a pressure wall across the enemy row *)
            let wall = clamp01 ((age -. 0.24) /. 0.18) in
            (if wall > 0. && n > 0 then
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
                  if hit > 0.22 then
                    let hush = clamp01 ((hit -. 0.22) /. 0.45) in
                    oval p.x p.y
                      (16. +. (hush *. 4.))
                      (12. +. (hush *. 3.))
                      1.4
                      (rgb 0.95 0.7 0.25 ((1. -. hush) *. fade *. 0.55)));
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
              oval foe_life.x foe_life.y
                (14. +. (k *. 40.))
                (14. +. (k *. 40.))
                2.6
                (rgb 1. 0.8 0.3 (al *. 0.75));
              circle foe_life.x foe_life.y 10. (rgb 1. 0.95 0.7 al))))

(* Warp Earthquake: the ground splits along the opponent's row, and every creature
   on it takes the difference between the two mana pools in its own element. The
   creatures tremble as the seam races out from the middle, and the dust it
   throws up rolls in behind them rather than over their faces, so the grit is
   drawn under the cards and the card occludes the middle of every plume. *)
let spawn_quake ~centers ~floors ~foe_pid ~start =
  let duration = 1.9 in
  let n = Array.length floors in
  Array.iteri
    (fun i _ ->
      add_shake foe_pid i ~start:(start +. (0.06 +. (float i *. 0.07))) ~duration:0.9 ~strength:1.25)
    floors;
  let ox = if n = 0 then 0. else floors.(0).x in
  let oy = if n = 0 then 0. else floors.(0).y in
  let width = if n = 0 then 300. else Array.fold_left (fun m (p : spot) -> Float.max m p.x) ox floors -. ox in
  let card = fun i -> if i < Array.length centers then centers.(i) else floors.(i) in
  mk ~under:true ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.35) in
      if fade > 0.02 then (
        let open_ = ease_out (clamp01 (age /. 0.32)) in
        let hold = clamp01 ((1.35 -. age) /. 0.45) in
        (* the seam: a jagged crack that reaches further as it opens *)
        for s = 0 to 30 do
          let u0 = float s /. 30. and u1 = float (s + 1) /. 30. in
          let reach = clamp01 ((age /. 0.45) -. (Float.abs (((u0 +. u1) /. 2.) -. 0.5) *. 0.8)) in
          if reach > 0. then (
            let jag u = ((noise s 1 4 -. 0.5) *. 7.) +. ((noise s 2 7 -. 0.5) *. 4. *. open_) in
            let x0 = ox +. (width *. u0) and x1 = ox +. (width *. u1) in
            let y0 = oy +. jag u0 and y1 = oy +. jag u1 in
            let a = reach *. hold *. fade in
            (* the split: a thin gash with the heat of it showing through, rather
               than a fat black bar lying on the ground *)
            line x0 y0 x1 y1 (0.9 +. (open_ *. 1.1)) (rgb 0.17 0.08 0.03 (a *. 0.8));
            line x0 y0 x1 y1 (0.5 +. (open_ *. 0.8)) (rgb 1. 0.52 0.14 (a *. 0.7));
            line x0 (y0 -. 1.4) x1 (y1 -. 1.4) 0.5 (rgb 1. 0.86 0.52 (a *. 0.45)))
        done;
        (* the dust: grit thrown out of the seam, and a low bank of it rolling
           along the ground. Grit reads as dust; the bank keeps the row from
           looking clean, and both stay small enough to see the seam through. *)
        for k = 0 to 69 do
          let r = noise k 5 31 in
          let mid = noise k 6 37 in
          let a = age -. (float k *. 0.014) in
          let reach = clamp01 ((age /. 0.45) -. (Float.abs (mid -. 0.5) *. 0.8)) in
          if a > 0. && a < 0.9 && reach > 0. then (
            let u = clamp01 (a /. 0.9) in
            let dir = if noise k 7 41 > 0.5 then 1. else -1. in
            let gx = ox +. (width *. mid) +. (dir *. ((8. +. (r *. 42.)) *. u)) in
            let gy = oy -. (((30. +. (r *. 60.)) *. u) *. (1. -. (0.4 *. u *. u))) in
            let al = fade *. reach *. (1. -. u) *. (0.35 +. (r *. 0.5)) in
            if al > 0.01 then circle gx gy (1.2 +. (r *. 2.2)) (rgb 0.7 0.6 0.46 al))
        done;
        Array.iteri
          (fun i _ ->
            let hit = 0.06 +. (float i *. 0.07) in
            let c = card i in
            for k = 0 to 8 do
              let r = noise i k 23 in
              let a = age -. (hit +. (float k *. 0.07)) in
              if a > 0. && a < 1.2 then (
                let span = 0.7 +. (r *. 0.5) in
                let u = clamp01 (a /. span) in
                let rise = ease_out u in
                let hx = c.x +. ((noise i k 29 -. 0.5) *. (24. +. (rise *. 46.))) in
                let hy = oy -. (rise *. (5. +. (r *. 9.))) in
                let rx = (14. +. (r *. 16.)) *. (0.5 +. (rise *. 0.8)) in
                let al = fade *. sin (u *. Float.pi) *. (0.05 +. (r *. 0.05)) in
                if al > 0.004 then ellipse hx hy rx (rx *. 0.26) (rgb 0.64 0.56 0.45 al))
            done)
          floors))

(* Limbo Nether Grasp: every creature the caster owns is dragged under into the
   between, and each soul it lets go is worth 3 damage to the opponent. It reads as
   a hub and its spokes -- each of your creatures throws arcs at the sphere hanging
   between the two rows, and the sphere throws arcs back at each of theirs -- so
   what you give up and what it buys are both on screen, and the sphere swells with
   however many souls it was given. The legs are several bowed arcs rather than one
   ray, which is what makes them read as grasping rather than as a laser. *)
let spawn_nether_grasp ~own_centers ~foe_targets ~hub ~souls ~foe_pid ~start =
  let duration = 1.8 in
  let gather = 0.7 in
  let travel = 0.4 in
  let hub_r = 15. +. (float souls *. 2.4) in
  Array.iteri
    (fun i _ -> add_shake foe_pid i ~start:(start +. gather +. travel +. 0.1) ~duration:0.45 ~strength:0.85)
    foe_targets;
  (* [legs] arcs bowed off the straight line between two points, drawn as far as
     [reach] so they grow along their length, with a bright head at the tip *)
  let arcs ~age ~x0 ~y0 ~x1 ~y1 ~legs ~reach ~width ~a seed =
    let dx = x1 -. x0 and dy = y1 -. y0 in
    let len = Float.max 1. (hypot dx dy) in
    let nx = -.dy /. len and ny = dx /. len in
    let bow_max = 14. +. (len *. 0.14) in
    let steps = 11 in
    for k = 0 to legs - 1 do
      let f = (float k /. float (max 1 (legs - 1))) -. 0.5 in
      let bow = f *. 2. *. bow_max in
      let c1x = x0 +. (dx *. 0.25) +. (nx *. bow) in
      let c1y = y0 +. (dy *. 0.25) +. (ny *. bow) in
      let c2x = x0 +. (dx *. 0.72) +. (nx *. bow *. 0.55) in
      let c2y = y0 +. (dy *. 0.72) +. (ny *. bow *. 0.55) in
      let px u = bezier x0 c1x c2x x1 u in
      let py u = bezier y0 c1y c2y y1 u in
      (* the arc crawls outward, so each leg reads as its own lash *)
      let at = Float.min reach ((age *. 2.6) -. (f *. 0.35)) in
      if at > 0.02 then
        for s = 0 to steps - 1 do
          let u0 = float s /. float steps *. at and u1 = float (s + 1) /. float steps *. at in
          let flick = 0.55 +. (0.45 *. sin (((u1 *. 3.) +. float k +. (seed *. 0.7)) *. Float.pi)) in
          line (px u0) (py u0) (px u1) (py u1) (width *. flick) (rgb 0.62 0.28 1. (a *. 0.55))
        done
    done;
    (* the head at the tip of each arc *)
    for k = 0 to legs - 1 do
      let f = (float k /. float (max 1 (legs - 1))) -. 0.5 in
      let bow = f *. 2. *. bow_max in
      let c1x = x0 +. (dx *. 0.25) +. (nx *. bow) in
      let c1y = y0 +. (dy *. 0.25) +. (ny *. bow) in
      let c2x = x0 +. (dx *. 0.72) +. (nx *. bow *. 0.55) in
      let c2y = y0 +. (dy *. 0.72) +. (ny *. bow *. 0.55) in
      let u = Float.min reach ((age *. 2.6) -. (f *. 0.35)) in
      if u > 0.05 && u < 0.995 then
        circle (bezier x0 c1x c2x x1 u) (bezier y0 c1y c2y y1 u) 2.4 (rgb 0.95 0.85 1. (a *. 0.85))
    done
  in
  (* beat one: each creature flares as it enters Limbo-state and lashes a soul out
     at the sphere, so the cost is countable *)
  let gather_souls age fade =
    Array.iteri
      (fun i (p : spot) ->
        let a = age -. (float i *. 0.055) in
        if a <= 0. || age >= gather +. 0.4 then ()
        else
          let flare = Float.max 0. (1. -. (a /. 0.18)) in
          if flare > 0.02 then (
            circle p.x p.y (9. +. (flare *. 22.)) (rgb 0.5 0.2 0.9 (flare *. fade *. 0.28));
            circle p.x p.y (2.5 +. (flare *. 6.)) (rgb 0.95 0.85 1. (flare *. fade *. 0.8)));
          if a < travel then
            arcs ~age:a ~x0:p.x ~y0:p.y ~x1:hub.x ~y1:hub.y ~legs:3 ~reach:1. ~width:2.2 ~a:(fade *. 0.9)
              (float i)
          else
            (* arriving: the sphere takes it *)
            let hit = Float.max 0. (1. -. ((a -. travel) /. 0.22)) in
            if hit > 0.02 then
              circle hub.x hub.y (hub_r *. (0.5 +. (hit *. 0.8))) (rgb 0.5 0.2 0.9 (hit *. fade *. 0.3)))
      own_centers
  in
  (* the sphere, swelling as it is fed and dimming as it spends itself *)
  let draw_hub age fade =
    let fed = clamp01 (age /. gather) in
    let spent = if age < gather then 0. else clamp01 ((age -. gather) /. (duration -. gather)) in
    let live = Float.min 1. (fed *. 1.3) *. (1. -. (spent *. 0.85)) in
    if live > 0.02 then (
      let r = hub_r *. (0.4 +. (fed *. 0.6)) in
      let pulse = 1. +. (sin (age *. 11.) *. 0.06) in
      circle hub.x hub.y (r *. 1.7 *. pulse) (rgb 0.32 0.08 0.58 (live *. fade *. 0.16));
      circle hub.x hub.y r (rgb 0.55 0.2 0.9 (live *. fade *. 0.3));
      circle hub.x hub.y (r *. 0.6) (rgb 0.82 0.58 1. (live *. fade *. 0.42));
      circle hub.x hub.y (r *. 0.26) (rgb 1. 0.95 1. (live *. fade *. 0.75)))
  in
  (* beat two: the sphere lashes out at each of the opponent's creatures *)
  let spend age fade =
    let ba = age -. gather in
    Array.iteri
      (fun i (f : spot) ->
        let a = ba -. (float i *. 0.06) in
        if a <= 0. then ()
        else if a < travel then
          arcs ~age:a ~x0:hub.x ~y0:hub.y ~x1:f.x ~y1:f.y ~legs:3 ~reach:1. ~width:2.4 ~a:(fade *. 0.9)
            (float (i + 7))
        else
          (* the hit *)
          let hit = Float.max 0. (1. -. ((a -. travel) /. 0.24)) in
          if hit > 0.02 then (
            circle f.x f.y (9. +. (hit *. 26.)) (rgb 0.55 0.2 0.9 (hit *. fade *. 0.32));
            circle f.x f.y (2.5 +. (hit *. 7.)) (rgb 0.95 0.85 1. (hit *. fade *. 0.6))))
      foe_targets
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.3) in
      if fade > 0.01 then
        additive (fun () ->
            if age < gather +. 0.4 then gather_souls age fade;
            draw_hub age fade;
            if age > gather then spend age fade))

(* Limbo Redemption: a creature held between worlds is pulled back out whole.
   Chains and grave dirt fall away as a pale column lifts its soul clear. *)
let spawn_redemption ~target ~start =
  let duration = 1.6 in
  let cy = target.y in
  mk ~start ~duration (fun ~age ->
      let fade = clamp01 (age /. 0.12) *. Float.min 1. ((duration -. age) /. 0.3) in
      if fade > 0.01 then (
        (* the column of light the soul comes up through *)
        let col = ease_out (clamp01 (age /. 0.5)) in
        ellipse target.x cy (16. +. (col *. 16.)) (4. +. (col *. 5.)) (rgb 0.85 0.82 0.7 (fade *. 0.22));
        for k = 0 to 6 do
          let r = noise k 3 5 in
          let ph = Float.rem ((float k /. 7.) +. (age *. 0.7)) 1. in
          rect
            (target.x -. (5. +. (r *. 6.)) +. ((r -. 0.5) *. 10.))
            (cy -. (ph *. 62. *. col))
            ((5. +. (r *. 6.)) *. 2.)
            (16. +. (r *. 20.))
            (rgb 0.95 0.92 0.8 (fade *. 0.07))
        done;
        (* chains and clods of grave dirt dropping away *)
        let drop = clamp01 ((age -. 0.15) /. 0.8) in
        if drop > 0. && drop < 1. then
          for k = 0 to 7 do
            let r = noise k 7 9 in
            let x = target.x +. ((r -. 0.5) *. 44.) in
            let y = cy -. 18. +. (drop *. (30. +. (r *. 40.))) in
            line x (y -. 7.) x y 1.6 (rgb 0.45 0.42 0.4 ((1. -. drop) *. fade *. 0.55));
            ellipse x y
              (2.6 +. (r *. 2.4))
              (2. +. (r *. 1.8))
              (rgb 0.3 0.26 0.22 ((1. -. drop) *. fade *. 0.6))
          done;
        (* the soul flame settling onto the board *)
        let lift = ease_out (clamp01 (age /. 0.65)) in
        let sy = cy -. (6. +. (lift *. 30.)) in
        let fa = fade *. (1. -. (lift *. 0.45)) in
        circle target.x sy (7. +. (lift *. 5.)) (rgb 0.95 0.9 0.75 (fa *. 0.25));
        circle target.x (sy -. 2.) (3.6 +. (lift *. 2.2)) (rgb 1. 0.98 0.9 (fa *. 0.8));
        for i = 0 to 7 do
          let r = noise i 11 13 in
          let u = Float.rem ((float i /. 8.) +. (age *. 0.9)) 1. in
          circle
            (target.x +. ((r -. 0.5) *. 26. *. (1. -. u)))
            (cy -. (u *. 46.))
            (1.6 *. (1. -. (u *. 0.5)))
            (rgb 0.9 0.88 0.7 (fade *. (1. -. u) *. 0.8))
        done))

(* Emotion — Revolt.  The foe's own creatures turn on their master: crimson
   bolts bend back from every occupied foe slot into the foe's life portrait. *)
let revolt_bolt =
  { glow = (0.55, 0.02, 0.06); body = (1., 0.26, 0.08); core = (1., 0.9, 0.6); orb = false }

let spawn_revolt ~from_points ~to_ ~start =
  let duration = 1.15 in
  Array.iter (fun from_ -> spawn (spawn_lightning ~style:revolt_bolt [ from_; to_ ] ~start)) from_points;
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.24) in
      if fade > 0.02 then (
        let hit = clamp01 ((age -. 0.12) /. 0.28) in
        let pulse = 0.65 +. (0.35 *. sin (age *. 18.)) in
        additive (fun () ->
            let a = fade *. (1. -. hit) *. 0.55 in
            circle to_.x to_.y (18. +. (hit *. 32.)) (rgb 0.9 0.04 0.06 (a *. 0.35));
            circle to_.x to_.y (7. +. (hit *. 14.)) (rgb 1. 0.24 0.08 (a *. 0.8));
            circle to_.x to_.y (3. +. (hit *. 5.)) (rgb 1. 0.9 0.65 (a *. pulse));
            for k = 0 to 5 do
              let ang = (float k *. (Float.pi /. 3.)) +. (age *. 1.8) in
              let len = (16. +. (hit *. 24.)) *. (0.7 +. (0.3 *. noise k 3 5)) in
              line to_.x to_.y
                (to_.x +. (cos ang *. len))
                (to_.y +. (sin ang *. len *. 0.55))
                1.4
                (rgb 1. 0.3 0.12 (a *. 0.45))
            done)))

(* Emotion — Loneliness.  Cold violet fog wells up out of every slot whose
   opposite is empty: low banks around the card, wisps rising, and a few pale
   motes that fade into the dark. *)
let spawn_loneliness ~spots ~start =
  let duration = 1.7 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. (age /. 0.2) *. Float.min 1. ((duration -. age) /. 0.3) in
      if fade > 0.02 then
        Array.iteri
          (fun i p ->
            let breathe = 0.5 +. (0.5 *. sin ((age *. 1.5) +. (float i *. 1.3))) in
            (* the bank settling over the slot, then crawling up the card *)
            let rise = Float.min 1. (age /. 0.55) in
            ellipse p.x (p.y +. 24.) (34. +. (8. *. rise)) (9. +. (5. *. rise))
              (rgb 0.34 0.30 0.52 (fade *. 0.18 *. (0.65 +. (0.35 *. breathe))));
            ellipse p.x (p.y +. 18.) (27. +. (6. *. rise)) (7. +. (4. *. rise))
              (rgb 0.62 0.58 0.80 (fade *. 0.12 *. (0.5 +. (0.5 *. breathe))));
            for k = 0 to 7 do
              let n = noise i k 17 and ph = noise i k 31 *. Float.pi *. 2. in
              let u = Float.rem ((age *. 0.38) +. n +. (float k *. 0.12)) 1. in
              let x = p.x +. ((n -. 0.5) *. 54.) +. (sin ((age *. 0.8) +. ph +. float k) *. 7.) in
              let y = p.y +. 22. -. (u *. 42.) +. ((noise i k 43 -. 0.5) *. 16.) in
              let rad = 13. +. (n *. 13.) +. (sin ((age *. 1.2) +. ph) *. 3.) in
              let a = fade *. (0.08 +. (0.08 *. (1. -. u))) *. (0.55 +. (0.45 *. breathe)) in
              circle x y rad (rgb 0.48 0.44 0.68 a);
              circle (x -. (rad *. 0.22)) (y -. (rad *. 0.16)) (rad *. 0.55)
                (rgb 0.70 0.68 0.88 (a *. 0.7))
            done;
            (* a thin cold glimmer in the fog *)
            additive (fun () ->
                let tw = Float.max 0. (sin ((age *. 3.2) +. (float i *. 1.9))) in
                circle
                  (p.x +. (sin ((age *. 0.9) +. float i) *. 19.))
                  (p.y -. 5. -. (age *. 7.))
                  1.6
                  (rgb 0.82 0.80 1. (fade *. 0.45 *. tw));
                circle (p.x -. 17.) (p.y +. 12. -. (age *. 3.)) 1.2
                  (rgb 0.72 0.70 0.95 (fade *. 0.3))))
          spots)

(* Emotion — Hope Warrior.  At death its light breaks out of the empty slot:
   warm arcs curl into every surviving ally, while the same radiance lashes out
   at every foe creature.  The arc geometry follows Nether Grasp: several bowed
   legs per target crawling outward to a bright head. *)
let spawn_hope ~from_ ~allies ~foes ~start =
  let duration = 1.55 in
  let travel = 0.52 in
  let arcs ~age ~x0 ~y0 ~x1 ~y1 ~legs ~reach ~width ~r ~g ~b ~a seed =
    let dx = x1 -. x0 and dy = y1 -. y0 in
    let len = Float.max 1. (hypot dx dy) in
    let nx = -.dy /. len and ny = dx /. len in
    let bow_max = 12. +. (len *. 0.16) in
    let steps = 12 in
    for k = 0 to legs - 1 do
      let f = (float k /. float (max 1 (legs - 1))) -. 0.5 in
      let bow = f *. 2. *. bow_max in
      let c1x = x0 +. (dx *. 0.24) +. (nx *. bow) in
      let c1y = y0 +. (dy *. 0.24) +. (ny *. bow) in
      let c2x = x0 +. (dx *. 0.74) +. (nx *. bow *. 0.55) in
      let c2y = y0 +. (dy *. 0.74) +. (ny *. bow *. 0.55) in
      let px u = bezier x0 c1x c2x x1 u in
      let py u = bezier y0 c1y c2y y1 u in
      let at = Float.min reach ((age *. 2.4) -. (f *. 0.28)) in
      if at > 0.02 then
        for s = 0 to steps - 1 do
          let u0 = float s /. float steps *. at and u1 = float (s + 1) /. float steps *. at in
          let flick = 0.58 +. (0.42 *. sin (((u1 *. 3.) +. float k +. (seed *. 0.7)) *. Float.pi)) in
          line (px u0) (py u0) (px u1) (py u1) (width *. flick) (rgb r g b (a *. 0.5))
        done
    done;
    for k = 0 to legs - 1 do
      let f = (float k /. float (max 1 (legs - 1))) -. 0.5 in
      let bow = f *. 2. *. bow_max in
      let c1x = x0 +. (dx *. 0.24) +. (nx *. bow) in
      let c1y = y0 +. (dy *. 0.24) +. (ny *. bow) in
      let c2x = x0 +. (dx *. 0.74) +. (nx *. bow *. 0.55) in
      let c2y = y0 +. (dy *. 0.74) +. (ny *. bow *. 0.55) in
      let u = Float.min reach ((age *. 2.4) -. (f *. 0.28)) in
      if u > 0.05 && u < 0.995 then (
        circle (bezier x0 c1x c2x x1 u) (bezier y0 c1y c2y y1 u) 2.5 (rgb 1. 0.97 0.8 (a *. 0.9));
        circle (bezier x0 c1x c2x x1 u) (bezier y0 c1y c2y y1 u) 4.4 (rgb r g b (a *. 0.25)))
    done
  in
  let deliver age fade spots ~ally =
    Array.iteri
      (fun i (p : spot) ->
        let a = age -. (float i *. 0.045) in
        if a > 0. then
          let r, g, b = if ally then (1., 0.84, 0.45) else (1., 0.46, 0.14) in
          if a < travel then
            arcs ~age:a ~x0:from_.x ~y0:from_.y ~x1:p.x ~y1:p.y ~legs:3 ~reach:1.
              ~width:(if ally then 2.1 else 2.5) ~r ~g ~b ~a:(fade *. 0.9)
              (float i +. if ally then 0. else 17.)
          else
            let hit = Float.max 0. (1. -. ((a -. travel) /. 0.26)) in
            if hit > 0.02 then
              if ally then (
                circle p.x p.y (10. +. (hit *. 10.)) (rgb 1. 0.86 0.45 (hit *. fade *. 0.3));
                circle p.x p.y (3. +. (hit *. 5.)) (rgb 1. 0.97 0.78 (hit *. fade *. 0.7));
                for k = 0 to 4 do
                  let n = noise i k 23 in
                  circle
                    (p.x +. ((n -. 0.5) *. 28.))
                    (p.y -. 8. -. (hit *. (18. +. (n *. 20.))))
                    (1.5 +. (n *. 1.4))
                    (rgb 1. 0.94 0.65 (hit *. fade *. 0.65))
                done)
              else (
                circle p.x p.y (9. +. (hit *. 24.)) (rgb 1. 0.42 0.1 (hit *. fade *. 0.38));
                circle p.x p.y (2.5 +. (hit *. 7.)) (rgb 1. 0.88 0.58 (hit *. fade *. 0.72));
                for k = 0 to 6 do
                  let ang = (float k *. (Float.pi /. 3.)) +. (age *. 1.5) in
                  let len = (8. +. (hit *. 18.)) *. (0.6 +. (0.4 *. noise i k 29)) in
                  line p.x p.y
                    (p.x +. (cos ang *. len))
                    (p.y +. (sin ang *. len))
                    1.4
                    (rgb 1. 0.64 0.22 (hit *. fade *. 0.6))
                done))
      spots
  in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. (age /. 0.14) *. Float.min 1. ((duration -. age) /. 0.32) in
      if fade > 0.02 then
        additive (fun () ->
            let burst = clamp01 (age /. 0.3) in
            let pulse = 0.62 +. (0.38 *. sin (age *. 15.)) in
            circle from_.x from_.y (12. +. (burst *. 42.))
              (rgb 1. 0.82 0.32 (fade *. (1. -. burst) *. 0.35));
            circle from_.x from_.y (5. +. (burst *. 16.))
              (rgb 1. 0.94 0.7 (fade *. (1. -. burst) *. 0.85));
            for k = 0 to 11 do
              let ang = (float k *. (Float.pi /. 6.)) +. (age *. 0.45) in
              let len = (10. +. (burst *. 34.)) *. (0.55 +. (0.45 *. noise k 4 9)) in
              line from_.x from_.y
                (from_.x +. (cos ang *. len))
                (from_.y +. (sin ang *. len *. 0.8))
                1.5
                (rgb 1. 0.86 0.45 (fade *. (1. -. burst) *. 0.45))
            done;
            circle from_.x from_.y (2.5 +. (burst *. 5.)) (rgb 1. 1. 0.92 (fade *. pulse));
            deliver age fade allies ~ally:true;
            deliver age fade foes ~ally:false))

let scared_duration = 0.58

(* The scared picture of a creature: its slot trembles, sickly green nerves crawl round the frame, and dust and
   sweat fly off it. [age] runs from the start of the fright and the picture fades out by [duration]. *)
let draw_scared ~at ~age ~duration =
  let fade = Float.min 1. (age /. 0.08) *. Float.min 1. ((duration -. age) /. 0.1) in
  if fade > 0.02 then (
    let tremble = 1. +. (1.7 *. Float.abs (sin (age *. 32.))) in
    let tight = ease_out (clamp01 (age /. duration)) in
    let rx = 31. -. (tight *. 7.) and ry = 42. -. (tight *. 9.) in
    let a = fade *. (0.18 +. (0.18 *. tight)) in
    ellipse at.x at.y rx ry (rgb 0.52 0.62 0.22 (a *. 0.4));
    ellipse at.x at.y (rx *. 0.72) (ry *. 0.72) (rgb 0.76 0.82 0.3 (a *. 0.5));
    for k = 0 to 7 do
      let ph = (float k *. 0.83) +. (age *. 19.) in
      let side = if k mod 2 = 0 then -1. else 1. in
      let x = at.x +. (side *. (26. +. (tremble *. 4.))) in
      let y = at.y +. (sin ph *. (24. +. (tremble *. 5.))) in
      let len = 5. +. (3. *. noise k 3 7) in
      line x y (x +. (side *. len)) (y +. (cos ph *. 2.)) 1.2 (rgb 0.8 0.9 0.58 (fade *. 0.4))
    done;
    (* dust kicked up from the slot floor *)
    for k = 0 to 5 do
      let n = noise k 9 11 and u = clamp01 ((age -. (float k *. 0.035)) /. 0.4) in
      if u > 0. then
        circle
          (at.x +. ((n -. 0.5) *. 48.))
          (at.y +. 27. -. (u *. 10.))
          (2. +. (n *. 3.) +. (u *. 2.))
          (rgb 0.52 0.43 0.32 (fade *. (1. -. u) *. 0.35))
    done;
    (* a couple of panicked sweat drops at the brow *)
    for k = 0 to 2 do
      let u = clamp01 ((age -. (0.05 +. (float k *. 0.06))) /. 0.38) in
      if u > 0. then
        ellipse
          (at.x -. 12. +. (float k *. 12.))
          (at.y -. 33. +. (u *. 18.))
          1.8 (2.8 +. (u *. 1.2))
          (rgb 0.7 0.9 0.75 (fade *. (1. -. u) *. 0.55))
    done)

(* Emotion — Coward Warrior.  Before the card dies it shakes apart: the slot
   itself trembles, sickly green nerves crawl around the frame, and a little
   dust and sweat fly off it.  This is a blocking effect so the death fade
   waits until the coward has visibly lost its nerve. *)
let spawn_coward ~at ~pid ~num ~start =
  let duration = scared_duration in
  add_shake pid num ~start ~duration ~strength:1.35;
  mk ~start ~duration (fun ~age -> draw_scared ~at ~age ~duration)

(* Emotion — Love Warrior.  A warm rose-gold heartbeat flashes over the card
   whenever it actually heals, with a soft halo and a few rising glints. *)
let spawn_love_flash ~at ~start =
  let duration = 0.62 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. (age /. 0.05) *. Float.min 1. ((duration -. age) /. 0.18) in
      if fade > 0.02 then
        additive (fun () ->
            let u = clamp01 (age /. duration) in
            let first = Float.max 0. (1. -. (age /. 0.2)) in
            let second =
              if age < 0.16 then 0. else Float.max 0. (1. -. ((age -. 0.16) /. 0.2))
            in
            let pulse = Float.max first second in
            circle at.x at.y (22. +. (u *. 18.)) (rgb 1. 0.36 0.55 (fade *. 0.1));
            circle at.x at.y (12. +. (pulse *. 24.)) (rgb 1. 0.58 0.7 (fade *. pulse *. 0.32));
            circle at.x at.y (4. +. (pulse *. 9.)) (rgb 1. 0.92 0.86 (fade *. pulse *. 0.7));
            for k = 0 to 9 do
              let ang = (float k *. (Float.pi *. 2. /. 10.)) +. (age *. 1.2) in
              let len = (16. +. (pulse *. 18.)) *. (0.65 +. (0.35 *. noise k 4 7)) in
              line at.x at.y
                (at.x +. (cos ang *. len))
                (at.y +. (sin ang *. len *. 0.72))
                1.2
                (rgb 1. 0.52 0.64 (fade *. pulse *. 0.38))
            done;
for k = 0 to 5 do
               let n = noise k 6 13 in
               let rise = u *. (24. +. (n *. 24.)) in
               circle
                 (at.x +. ((n -. 0.5) *. 36.) +. (sin ((age *. 2.) +. float k) *. 4.))
                 (at.y +. 24. -. rise)
                 (1.2 +. (n *. 1.2))
                 (rgb 1. 0.85 0.72 (fade *. (1. -. u) *. 0.6))
             done))

(* ---------------------------------------------------------------- paganist *)

(* The Paganist pays in creatures and in its own blood, so every one of its
   spells reads as a sacrifice being made somewhere: something on a side of the
   board is torn open, and the cost of that lands on the other side. *)

(* Final Offering: the caster's own creatures are split open one after the other
   and everything harvested out of them runs to the foe as one stream. *)
let final_offering_duration = 1.6

let spawn_final_offering ~own ~foe_life ~start =
  let duration = final_offering_duration in
  let gather = 0.5 and flight = 0.6 in
  let n = max 1 (Array.length own) in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.3) in
      if fade > 0.02 then (
        (* the offerings are cut open in turn, each one a little behind the last *)
        additive (fun () ->
            Array.iteri
              (fun i (p : spot) ->
                let a = age -. (float i *. 0.06) in
                if a > 0. && a < 0.55 then (
                  let k = clamp01 (a /. 0.35) in
                  let split = k *. 15. in
                  let jag = (noise i 3 1 -. 0.5) *. 7. in
                  let th = 2. +. (k *. 2.5) in
                  line (p.x -. split) (p.y -. 42. +. jag) (p.x -. split -. jag) (p.y +. 42.) th
                    (rgb 0.9 0.12 0.14 ((1. -. k) *. fade));
                  line (p.x +. split) (p.y -. 42. -. jag) (p.x +. split +. jag) (p.y +. 42.) th
                    (rgb 0.9 0.12 0.14 ((1. -. k) *. fade));
                  circle p.x p.y (6. +. (k *. 20.)) (rgb 0.6 0.03 0.05 ((1. -. k) *. 0.45 *. fade))))
              own);
        (* the harvest crosses to the other side *)
        if age > gather then (
          additive (fun () ->
              for k = 0 to 17 do
                let a = age -. gather -. (float k *. 0.03) in
                if a >= 0. && a <= flight then (
                  let t = ease_out (a /. flight) in
                  let src = own.(k mod n) in
                  let sx = src.x +. ((noise k 5 2 -. 0.5) *. 44.) in
                  let sy = src.y +. ((noise k 7 4 -. 0.5) *. 76.) in
                  (* the stream bows outward as it flies, so it does not read as one line *)
                  let bow = (sin (t *. Float.pi) *. 30.) *. (noise k 11 6 -. 0.5) in
                  let x = sx +. ((foe_life.x -. sx) *. t) -. bow in
                  let y = sy +. ((foe_life.y -. sy) *. t) in
                  let r = (3. +. (noise k 13 8 *. 3.)) *. (1. -. (t *. 0.45)) in
                  circle x y r (rgb 0.8 0.05 0.07 (fade *. 0.9));
                  circle x y (r *. 0.5) (rgb 1. 0.45 0.3 (fade *. 0.8)))
              done);
        if age > gather +. flight then (
          (* and lands *)
          let k = clamp01 ((age -. gather -. flight) /. 0.45) in
          let out = 1. -. k in
          additive (fun () ->
              circle foe_life.x foe_life.y (14. +. (k *. 64.)) (rgb 0.7 0.05 0.08 (out *. 0.5 *. fade));
              circle foe_life.x foe_life.y (7. +. (k *. 30.)) (rgb 1. 0.5 0.35 (out *. 0.65 *. fade));
              for kk = 0 to 9 do
                let ang = (float kk *. 0.628) +. k in
                let rad = k *. 96. *. (0.5 +. noise kk 2 1) in
                circle
                  (foe_life.x +. (cos ang *. rad))
                  (foe_life.y +. (sin ang *. rad))
                  (2. +. (k *. 2.2))
                  (rgb 0.85 0.08 0.1 (out *. 0.65 *. fade))
              done)))))

(* Unholy Contract: the pact is struck on the chosen creature, the creature is
   torn open, and what comes out of it is thrown across the board — a rope of
   blood arcing from the card to the payer's portrait, where it lands as a
   spatter that clings to the face. *)
let unholy_contract_duration = 1.6

let spawn_unholy_contract ~at ~foe_life ~start =
  let duration = unholy_contract_duration in
  let strike = 0.32 and flight = 0.5 in
  let thrown = strike +. flight in
  let tau = Float.pi *. 2. in
  let dx = foe_life.x -. at.x and dy = foe_life.y -. at.y in
  let span = Float.max 1. (hypot dx dy) in
  (* the rope is thrown low and long, sagging through the gap between the rows,
     so it crosses the whole board instead of hugging the victim's own row *)
  let sag = 46. +. Float.min 116. (span *. 0.2) in
  (* each strand rides its own spine: [deep] sags it a little more or less,
     [lag] holds it back so the thin ones whip in behind the main rope *)
  let spine deep =
    let s = sag *. deep in
    ( at.x +. (dx *. 0.18), at.y +. (dy *. 0.18) +. s, at.x +. (dx *. 0.78), at.y +. (dy *. 0.78) +. (s *. 0.55) )
  in
  let strands = [| (0.78, -0.13, 1.5); (0.92, -0.06, 2.5); (1., 0., 4.4) |] in
  let main_x1, main_y1, main_x2, main_y2 = spine 1. in
  let px u = bezier at.x main_x1 main_x2 foe_life.x u in
  let py u = bezier at.y main_y1 main_y2 foe_life.y u in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. (age /. 0.05) *. Float.min 1. ((duration -. age) /. 0.3) in
      if fade > 0.02 then (
        (* the pact is struck on the victim: a ring traced arc by arc, the
           triangle inside it, and then the card is opened *)
        if age < strike then (
          let u = clamp01 (age /. strike) in
          let rad = 32. *. (0.58 +. (0.42 *. swing_out u)) in
          let a = fade *. (0.35 +. (0.65 *. u)) in
          let seg = 26 in
          for m = 0 to seg - 1 do
            if float m /. float seg < u then (
              let t0 = float m /. float seg and t1 = float (m + 1) /. float seg in
              line
                (at.x +. (cos (t0 *. tau) *. rad))
                (at.y +. (sin (t0 *. tau) *. rad))
                (at.x +. (cos (t1 *. tau) *. rad))
                (at.y +. (sin (t1 *. tau) *. rad))
                2. (rgb 0.62 0.03 0.09 a))
          done;
          if u > 0.5 then (
            let tu = clamp01 ((u -. 0.5) /. 0.5) in
            for i = 0 to 2 do
              let a0 = (float i *. 2.0944) +. 0.5 in
              let a1 = a0 +. 2.0944 in
              line
                (at.x +. (cos a0 *. rad *. 0.7))
                (at.y +. (sin a0 *. rad *. 0.7))
                (at.x +. (cos a1 *. rad *. 0.7))
                (at.y +. (sin a1 *. rad *. 0.7))
                1.6 (rgb 0.7 0.06 0.13 (a *. tu))
            done))
        else (
          (* the sigil tears white and the wound wells open under the rope's foot *)
          let k = clamp01 ((age -. strike) /. 0.22) in
          additive (fun () ->
              circle at.x at.y (34. *. (1. +. (k *. 1.3))) (rgb 0.85 0.12 0.14 ((1. -. k) *. 0.4 *. fade));
              circle at.x at.y (15. *. (1. -. (k *. 0.6))) (rgb 1. 0.62 0.42 ((1. -. k) *. fade));
              for m = 0 to 12 do
                let ang = (float m *. 0.5236) +. ((noise m 2 3 -. 0.5) *. 0.7) in
                let len = 8. +. (k *. 30.) in
                circle
                  (at.x +. (cos ang *. len))
                  (at.y +. (sin ang *. len))
                  (2. +. (noise m 5 1 *. 3.4))
                  (rgb 0.7 0.02 0.06 (fade *. (0.55 +. (k *. 0.45))))
              done));
        (* the rope crosses to the other side, thin strands first *)
        if age > strike -. 0.06 then
          additive (fun () ->
              Array.iter
                (fun (deep, lag, width) ->
                  let c1x, c1y, c2x, c2y = spine deep in
                  let sx u = bezier at.x c1x c2x foe_life.x u in
                  let sy u = bezier at.y c1y c2y foe_life.y u in
                  let head = clamp01 (((age -. strike) /. flight) +. lag) in
                  if head > 0.01 then (
                    let steps = 24 in
                    for i = 0 to steps - 1 do
                      let u0 = float i /. float steps *. head and u1 = float (i + 1) /. float steps *. head in
                      (* blood is torn out fat and runs out thin toward the head, with a
                         wave travelling down it so it reads as thrown and not drawn *)
                      let ripple = 1. +. (sin ((age *. 9.) -. (u0 *. 7.) +. deep) *. 0.18) in
                      let th = width *. (1. -. (0.45 *. u0)) *. ripple in
                      line (sx u0) (sy u0) (sx u1) (sy u1) (th *. 2.4) (rgb 0.42 0.02 0.04 (fade *. 0.3));
                      line (sx u0) (sy u0) (sx u1) (sy u1) th (rgb 0.7 0.03 0.06 (fade *. 0.85));
                      line (sx u0) (sy u0) (sx u1) (sy u1) (th *. 0.32) (rgb 1. 0.42 0.28 (fade *. 0.65))
                    done;
                    (* the head runs hot, and the strands behind it are still catching up *)
                    if width > 3. then (
                      circle (sx head) (sy head) (width *. 2.2) (rgb 0.8 0.05 0.07 (fade *. 0.5));
                      circle (sx head) (sy head) (width *. 1.1) (rgb 1. 0.45 0.3 (fade *. 0.85));
                      circle (sx head) (sy head) (width *. 0.45) (rgb 1. 0.9 0.75 (fade *. 0.9)))))
                strands;
              (* drops let go of the rope along the way and fall off it *)
              for i = 0 to 13 do
                let u = (float i +. 1.) /. 14. in
                let born = strike +. (u *. flight) +. ((noise i 3 7 -. 0.5) *. 0.05) in
                let dt = age -. born in
                if dt >= 0. && dt < 0.4 then (
                  (* each keeps the direction the rope had when it let go, then falls *)
                  let u2 = Float.min 1. (u +. 0.02) in
                  let tx = px u2 -. px u and ty = py u2 -. py u in
                  let len = Float.max 1. (hypot tx ty) in
                  let v = span /. flight in
                  let x = px u +. ((tx /. len) *. v *. dt) +. ((noise i 5 11 -. 0.5) *. 70. *. dt) in
                  let y = py u +. ((ty /. len) *. v *. dt) +. (56. *. dt *. dt) in
                  let out = 1. -. (dt /. 0.4) in
                  circle x y (1. +. (out *. 1.8)) (rgb 0.8 0.05 0.08 (fade *. out *. 0.7));
                  circle x y (0.5 +. (out *. 0.9)) (rgb 1. 0.5 0.34 (fade *. out *. 0.8)))
              done;
              (* the leading drop throws a little spray ahead of itself *)
              if age < thrown then (
                let head = clamp01 ((age -. strike) /. flight) in
                let hx = px head and hy = py head in
                for i = 0 to 5 do
                  let ang = (float i *. 1.047) +. (age *. 3.) in
                  let len = 5. +. (head *. 16.) in
                  circle
                    (hx +. (cos ang *. len))
                    (hy +. (sin ang *. len))
                    (1. +. (noise i 2 13 *. 1.2))
                    (rgb 1. 0.5 0.32 (fade *. (0.5 +. (head *. 0.5)) *. 0.6))
                done));
        (* and it lands: the face takes the spatter and keeps it *)
        if age > thrown then (
          let k = clamp01 ((age -. thrown) /. 0.5) in
          let flash = 1. -. clamp01 ((age -. thrown) /. 0.3) in
          let wash = Float.min 1. (k *. 2.6) *. fade in
          additive (fun () ->
              circle foe_life.x foe_life.y (11. +. (k *. 66.)) (rgb 0.7 0.03 0.06 (flash *. 0.42 *. fade));
              circle foe_life.x foe_life.y (7. +. (k *. 34.)) (rgb 1. 0.42 0.28 (flash *. 0.6 *. fade));
              (* drops thrown past the face, each on its own short fall *)
              for i = 0 to 15 do
                let ang = (float i *. 0.418) +. ((noise i 4 9 -. 0.5) *. 0.8) in
                let v = 70. +. (noise i 6 3 *. 110.) in
                let life = 0.3 +. (noise i 8 5 *. 0.4) in
                let dt = (age -. thrown) +. 0.02 in
                if dt < life then (
                  let out = 1. -. (dt /. life) in
                  let x = foe_life.x +. (cos ang *. v *. dt) in
                  let y = foe_life.y +. (sin ang *. v *. dt) +. (46. *. dt *. dt) in
                  circle x y (1. +. (out *. 2.2)) (rgb 0.78 0.04 0.07 (out *. fade *. 0.75));
                  circle x y (0.5 +. (out *. 1.)) (rgb 1. 0.55 0.36 (out *. fade *. 0.8)))
              done;
              (* what stays: a wash, streaks thrown outward, and runs going down *)
              circle foe_life.x foe_life.y (20. +. (k *. 12.)) (rgb 0.5 0.02 0.04 (wash *. 0.4));
              circle foe_life.x foe_life.y 9. (rgb 0.85 0.05 0.08 (wash *. 0.55));
              for i = 0 to 11 do
                let ang = (float i *. 0.5236) +. (noise i 7 2 -. 0.5) in
                let len = (10. +. (noise i 9 4 *. 26.)) *. Float.min 1. (k *. 3.) in
                line
                  (foe_life.x +. (cos ang *. (len *. 0.35)))
                  (foe_life.y +. (sin ang *. (len *. 0.35)))
                  (foe_life.x +. (cos ang *. len))
                  (foe_life.y +. (sin ang *. len))
                  (1.2 +. (noise i 11 6 *. 2.2))
                  (rgb 0.7 0.03 0.06 (wash *. 0.5))
              done;
              for i = 0 to 4 do
                let n = noise i 13 8 in
                let run = k *. (18. +. (n *. 46.)) in
                let x = foe_life.x +. ((n -. 0.5) *. 34.) in
                line x (foe_life.y +. 8.) x (foe_life.y +. 8. +. run) (1.4 +. (n *. 1.6))
                  (rgb 0.62 0.03 0.05 (wash *. 0.45));
                circle x (foe_life.y +. 8. +. run) (1.6 +. (n *. 1.8)) (rgb 0.85 0.05 0.08 (wash *. 0.6))
              done))))

(* Terrorize: the chosen creature is scared the way a coward is, and its owner's portrait shakes with it. *)
let terrorize_duration = scared_duration

let spawn_terrorize ~at ~pid ~num ~start =
  add_portrait_shake pid ~start ~duration:scared_duration ~strength:1.0;
  spawn_coward ~at ~pid ~num ~start

(* Obliterate: the foe's creatures are taken one lane at a time. Each card shudders, then is eaten away from
   within (the board paints it through the dissolve shader, driven by [mark_dissolve]). The souls that leave
   it rise as pale motes, with a few ember sparks, and nothing is cast across the board. *)
let obliterate_duration = 2.4
let obliterate_lane = 0.16 (* the next creature starts this long after the one before it *)
let obliterate_shudder = 0.15
let obliterate_eat = 0.8

let spawn_obliterate ~foe ~occupied ~foe_pid ~start =
  let duration = obliterate_duration in
  List.iteri
    (fun k num ->
      add_shake foe_pid num ~start:(start +. (float k *. obliterate_lane)) ~duration:0.3 ~strength:0.7)
    occupied;
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.35) in
      if fade > 0.02 then
        List.iteri
          (fun k num ->
            let p : spot = foe.(num) in
            let lt = age -. (float k *. obliterate_lane) in
            if lt >= 0. then (
              let eaten = clamp01 ((lt -. obliterate_shudder) /. obliterate_eat) in
              if lt >= obliterate_shudder then mark_dissolve p eaten (noise num 31 7);
              additive (fun () ->
                  for j = 0 to 13 do
                    (* each soul leaves at its own moment, while the card is being eaten *)
                    let s1 = noise j (num + 3) 11 and s2 = noise j (num + 5) 13 in
                    let s3 = noise j (num + 7) 17 and s4 = noise j (num + 9) 19 in
                    let birth = 0.2 +. (s1 *. 0.7) and life = 0.8 +. (s2 *. 0.6) in
                    let ma = lt -. birth in
                    if ma >= 0. && ma < life then (
                      let k' = ma /. life in
                      let rise = (1. -. ((1. -. k') *. (1. -. k'))) *. (34. +. (s2 *. 30.)) in
                      let x = p.x +. ((s3 -. 0.5) *. 64.) +. (sin ((ma *. 3.) +. (s4 *. 6.)) *. 6.) in
                      let y = p.y +. ((s4 -. 0.5) *. 60.) +. 6. -. rise in
                      let a = Float.min 1. (ma /. 0.15) *. (1. -. k') *. 0.9 *. fade in
                      (* most souls are pale; a few are ember sparks *)
                      let ember = s3 < 0.22 in
                      let r = if ember then 1. else 0.9 and g = if ember then 0.5 else 0.86 in
                      let b = if ember then 0.2 else 1. in
                      (* a wisp: a bright head with a short trail left behind it, no halo *)
                      let rad = 0.9 +. (s1 *. 0.9) in
                      line x (y +. 3. +. (s2 *. 3.)) x y (rad *. 0.8) (rgb r g b (a *. 0.3));
                      circle x y (rad *. 1.7) (rgb r g b (a *. 0.12));
                      circle x y rad (rgb r g b a))
                  done))
          )
          occupied)

(* River of Blood: the caster is cut open and blood sprays across its own portrait, and drips run down its face.
   From there a blood stream leaps in an arc to each enemy creature in turn and last to the foe's portrait. Each
   picture it lands on splashes, and a red filter washes over it and holds until the spell is done. *)
(* how far apart the streams leave the caster, and how long each one flies *)
let blood_stagger = 0.07
let blood_flight = 0.5

(* the size of a creature's card and of a portrait, as the board draws them *)
let blood_card = (86., 98.)
let blood_portrait = (84., 84.)

(* a point on the stream from [a] to [b]: a quadratic curve that bows up above them and leans out to the
   right, so that a stream straight up the board still curves *)
let blood_arc (a : spot) (b : spot) t =
  let bow = 36. +. (0.3 *. hypot (b.x -. a.x) (b.y -. a.y)) in
  let cx = ((a.x +. b.x) /. 2.) +. (0.4 *. bow) and cy = Float.min a.y b.y -. bow in
  let u = 1. -. t in
  ( (u *. u *. a.x) +. (2. *. u *. t *. cx) +. (t *. t *. b.x),
    (u *. u *. a.y) +. (2. *. u *. t *. cy) +. (t *. t *. b.y) )

(* A squirt of blood along its arc up to [head] (0..1): a dark stream that thins as it goes, a bright core,
   and the drop at its head. *)
let draw_blood_stream (a : spot) (b : spot) ~head ~fade =
  let n = 20 in
  for j = 0 to n - 1 do
    let t0 = head *. float j /. float n and t1 = head *. float (j + 1) /. float n in
    let x0, y0 = blood_arc a b t0 and x1, y1 = blood_arc a b t1 in
    let w = 7. *. (1. -. (0.5 *. float j /. float n)) in
    line x0 y0 x1 y1 w (rgb 0.42 0.02 0.04 (fade *. 0.9));
    line x0 y0 x1 y1 (w *. 0.4) (rgb 0.85 0.1 0.1 (fade *. 0.75))
  done;
  (* a few drops still hang off the stream behind the head *)
  for k = 1 to 3 do
    let t = head -. (0.07 *. float k) in
    if t > 0. then (
      let x, y = blood_arc a b t in
      circle (x +. ((noise k 6 2 -. 0.5) *. 6.)) (y +. (noise k 7 4 *. 4.)) (2.2 -. (0.3 *. float k))
        (rgb 0.5 0.02 0.04 (fade *. 0.8)))
  done;
  let hx, hy = blood_arc a b head in
  circle hx hy 6. (rgb 0.5 0.02 0.04 fade);
  circle (hx -. 1.8) (hy -. 2.) 2. (rgb 1. 0.5 0.4 (fade *. 0.8))

(* Where a stream lands: a dark pool spreads under the picture and drops fly off it and fall. [s] is the time
   since the landing. *)
let draw_blood_splash (p : spot) ~s ~fade =
  let pool = clamp01 (s /. 0.25) in
  ellipse p.x (p.y +. 34.) (3. +. (pool *. 13.)) (1.2 +. (pool *. 4.)) (rgb 0.38 0.01 0.03 (fade *. 0.7));
  let u = clamp01 (s /. 0.45) in
  for k = 0 to 7 do
    let ang = (float k *. 0.785) +. ((noise k 8 2 -. 0.5) *. 0.6) in
    let d = (8. +. (noise k 9 4 *. 22.)) *. ease_out u in
    circle
      (p.x +. (cos ang *. d))
      (p.y -. (sin ang *. d) +. (u *. u *. 24.))
      (1.8 +. (noise k 3 6 *. 1.2))
      (rgb 0.6 0.03 0.05 (fade *. (1. -. u) *. 0.9))
  done

(* Multiplied blending: the picture underneath is tinted, not lit, so it reads as a filter over it. *)
let multiplied f =
  R.begin_blend_mode R.BlendMode.Multiplied;
  f ();
  R.end_blend_mode ()

(* The red filter over a hit picture: [amount] tints it red (it holds as a stain), and [flash] adds a
   brief bright wash on the hit itself. *)
let draw_blood_stain (p : spot) (w, h) ~amount ~flash =
  let x0 = p.x -. (w /. 2.) and y0 = p.y -. (h /. 2.) in
  if amount > 0.01 then
    multiplied (fun () -> rect x0 y0 w h (rgb 1. (1. -. (0.72 *. amount)) (1. -. (0.72 *. amount)) 1.));
  if flash > 0.01 then additive (fun () -> rect x0 y0 w h (rgb 0.6 0.03 0.03 flash))

(* The cut: a spray of drops flung out of the wound, and drips that run down the face of the portrait. *)
let draw_blood_cut (own : spot) ~age ~fade =
  for k = 0 to 13 do
    let ang = (float k *. 0.449) +. ((noise k 2 6 -. 0.5) *. 0.5) in
    let speed = 40. +. (noise k 4 8 *. 50.) in
    if age < 0.6 then (
      let x = own.x +. (cos ang *. speed *. age) in
      let y = own.y +. (sin ang *. speed *. age) +. (80. *. age *. age) in
      circle x y (2.2 +. (noise k 5 3 *. 1.4)) (rgb 0.5 0.02 0.04 (fade *. (1. -. (age /. 0.6)))))
  done;
  for k = 0 to 9 do
    let a = age -. (0.1 +. (noise k 12 3 *. 0.5)) in
    if a > 0. then (
      let px = own.x +. ((noise k 13 5 -. 0.5) *. 60.) in
      let top = own.y -. 30. +. (noise k 14 7 *. 20.) in
      let len = Float.min (a *. (9. +. (noise k 15 9 *. 9.))) (own.y +. 40. -. top) in
      line px top px (top +. len) 1.6 (rgb 0.42 0.02 0.04 (fade *. 0.85));
      circle px (top +. len) 2.2 (rgb 0.5 0.02 0.04 (fade *. 0.9)))
  done

(* The cut: the caster is wounded at once, and blood sprays from its portrait with drips down its face. *)
let blood_cut_duration = 0.7

let spawn_blood_cut ~own_life ~start =
  let duration = blood_cut_duration in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.25) in
      if fade > 0.02 then (
        draw_blood_stain own_life blood_portrait
          ~amount:(clamp01 (age /. 0.15) *. 0.6)
          ~flash:(clamp01 (1. -. (age /. 0.25)) *. 0.4 *. fade);
        draw_blood_cut own_life ~age ~fade))

(* The flood: sent only when the caster survives its cut. Blood streams from the caster's portrait to each enemy
   creature in turn, then to the foe's portrait. Each picture it lands on splashes and takes the red filter, which
   holds until the spell is done. *)
let blood_flood_duration = 1.6

let spawn_blood_flood ~own_life ~foe ~foe_life ~start =
  let duration = blood_flood_duration in
  (* the enemy creatures in turn, then the foe's portrait last *)
  let targets = Array.append (Array.map (fun p -> (p, blood_card)) foe) [| (foe_life, blood_portrait) |] in
  let launch i = float i *. blood_stagger in
  let arrival i = launch i +. blood_flight in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.35) in
      if fade > 0.02 then (
        (* the caster's portrait stays red while it bleeds *)
        draw_blood_stain own_life blood_portrait ~amount:(0.6 *. fade) ~flash:0.;
        (* the red filter over every picture the blood has landed on *)
        Array.iteri
          (fun i (p, size) ->
            let s = age -. arrival i in
            if s >= 0. then
              draw_blood_stain p size
                ~amount:(clamp01 (s /. 0.12) *. fade)
                ~flash:(clamp01 (1. -. (s /. 0.2)) *. 0.4 *. fade))
          targets;
        (* the streams, and the splash where each one lands *)
        Array.iteri
          (fun i (p, _) ->
            let a = age -. launch i in
            if a >= 0. then (
              let head = clamp01 (ease_out (a /. blood_flight)) in
              let stream_fade = clamp01 ((arrival i +. 0.3 -. age) /. 0.3) *. fade in
              if stream_fade > 0.01 then draw_blood_stream own_life p ~head ~fade:stream_fade;
              let s = age -. arrival i in
              if s >= 0. then draw_blood_splash p ~s ~fade))
          targets))

(* Underworld Rose: a thorned vine leaves the spell card in the hand and curves across the
   board to the creature, the way a climber reaches for something it cannot see round.
   The arc is drawn once, at the cast, with two random bends, so each cast winds
   differently.  The vine grows out to the creature and stays there.  At the hit a bunch
   of petals bursts from the creature, and its card darkens as the life is drunk out. *)
let underworld_rose_duration = 1.9

(* the hit lands when the vine's head reaches the creature *)
let rose_hit = 0.8

(* A barb on the stalk: dark, hooked back toward the stem, lit along one edge. *)
let rose_thorn x y ang len fade =
  if len > 1. then (
    let dx = cos ang and dy = sin ang in
    let px = -.dy and py = dx in
    let hx = x +. (dx *. (len *. 0.8)) and hy = y +. (dy *. (len *. 0.8)) in
    let tx = x +. (dx *. len) and ty = y +. (dy *. len) in
    let bx = hx +. (px *. (len *. 0.26)) and by = hy +. (py *. (len *. 0.26)) in
    let ax = hx -. (px *. (len *. 0.22)) and ay = hy -. (py *. (len *. 0.22)) in
    tri x y ax ay tx ty (rgb 0.1 0.12 0.05 (fade *. 0.95));
    tri x y bx by tx ty (rgb 0.14 0.17 0.07 (fade *. 0.95));
    line ax ay tx ty 1.4 (rgb 0.62 0.75 0.3 (fade *. 0.7)))

(* A cubic bezier from [p0] to [p3] through the two control points, at [t] in 0..1. *)
let bezier (p0 : spot) (p1 : spot) (p2 : spot) (p3 : spot) t : spot =
  let u = 1. -. t in
  let a = u *. u *. u and b = 3. *. u *. u *. t and c = 3. *. u *. t *. t and d = t *. t *. t in
  {
    x = (a *. p0.x) +. (b *. p1.x) +. (c *. p2.x) +. (d *. p3.x);
    y = (a *. p0.y) +. (b *. p1.y) +. (c *. p2.y) +. (d *. p3.y);
  }

(* The two control points of one vine's arc: each bends off the straight line from the
   card to the creature by a random amount, on either side. *)
let rose_arc (src : spot) (dst : spot) : spot * spot =
  let dx = dst.x -. src.x and dy = dst.y -. src.y in
  let len = Float.max 1. (hypot dx dy) in
  let px = -.dy /. len and py = dx /. len in
  let bend () = Random.float 160. -. 80. in
  let b1 = bend () and b2 = bend () in
  ( { x = src.x +. (dx *. 0.3) +. (px *. b1); y = src.y +. (dy *. 0.3) +. (py *. b1) },
    { x = src.x +. (dx *. 0.7) +. (px *. b2); y = src.y +. (dy *. 0.7) +. (py *. b2) } )

(* The vine from the card to the creature, drawn out to [grow] of its length: a dark
   stalk with a lit edge, thinning toward the head, and barbs along it. *)
let draw_rose_vine ~(src : spot) ~p1 ~p2 ~(dst : spot) ~grow ~fade =
  let steps = 40 in
  let at t = bezier src p1 p2 dst t in
  let prev = ref (at 0.) in
  for j = 1 to steps do
    let t = grow *. float j /. float steps in
    let q : spot = at t in
    let p = !prev in
    let th = 4. -. (2. *. float j /. float steps) in
    line p.x p.y q.x q.y th (rgb 0.09 0.11 0.05 (fade *. 0.95));
    line p.x p.y q.x q.y (th *. 0.34) (rgb 0.5 0.9 0.22 (fade *. 0.6));
    if j mod 5 = 0 then (
      let ang = Float.atan2 (q.y -. p.y) (q.x -. p.x) +. if (j / 5) mod 2 = 0 then 1.0 else -1.0 in
      rose_thorn q.x q.y ang 6. fade);
    prev := q
  done;
  (* a dark bud at the head of the vine while it is still reaching *)
  if grow > 0.02 && grow < 1. then (
    let h : spot = at grow in
    circle h.x h.y 2.6 (rgb 0.55 0.05 0.08 (fade *. 0.95)))

(* A petal of the burst at the hit: cut with a random heading, speed and tumble when the
   cast starts.  [tone] picks one of three colours: crimson, wine, or a pale pink. *)
type petal = { ang : float; speed : float; spin : float; born : float; life : float; size : float; tone : int }

let petal_count = 18

let make_petals () =
  Array.init petal_count (fun _ ->
      {
        ang = Random.float (2. *. Float.pi);
        speed = 60. +. Random.float 90.;
        spin = 5. +. Random.float 7.;
        born = Random.float 0.15;
        life = 0.8 +. Random.float 0.5;
        size = 3. +. Random.float 2.;
        tone = Random.int 3;
      })

(* The petals bursting out of the creature at [at] from the hit: each flies out, slows,
   sinks a little under its own weight, and turns over as it goes. *)
let rose_petals ~(at : spot) ~petals ~age ~fade =
  Array.iter
    (fun p ->
      let a = age -. rose_hit -. p.born in
      if a > 0. && a < p.life then (
        let u = a /. p.life in
        let d = p.speed *. ease_out (clamp01 (a /. 0.6)) in
        let x = at.x +. (cos p.ang *. d) in
        let y = at.y +. (sin p.ang *. d *. 0.6) +. (40. *. a *. a) in
        let sp = (a *. p.spin) +. p.ang in
        let ax = cos sp and ay = sin sp in
        let px = -.ay and py = ax in
        let half_len = p.size in
        let half_w = p.size *. 0.45 *. (0.35 +. (0.65 *. Float.abs (cos (a *. p.spin *. 1.7)))) in
        let al = fade *. Float.min 1. (a /. 0.05) *. (1. -. u) *. 0.95 in
        let c =
          match p.tone with
          | 0 -> rgb 0.74 0.08 0.13 al
          | 1 -> rgb 0.45 0.04 0.09 al
          | _ -> rgb 0.92 0.55 0.6 al
        in
        let sx = x +. (px *. half_w) and sy = y +. (py *. half_w) in
        let lx = x -. (px *. half_w) and ly = y -. (py *. half_w) in
        tri (x +. (ax *. half_len)) (y +. (ay *. half_len)) sx sy lx ly c;
        tri (x -. (ax *. half_len)) (y -. (ay *. half_len)) sx sy lx ly c))
    petals

let spawn_underworld_rose ~source ~target ~start =
  let duration = underworld_rose_duration in
  let p1, p2 = rose_arc source target in
  let petals = make_petals () in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.26) in
      if fade > 0.02 then (
        (* the vine leaves the card and reaches the creature by the hit, then stays up *)
        let grow = ease_out (clamp01 ((age -. 0.05) /. (rose_hit -. 0.05))) in
        draw_rose_vine ~src:source ~p1 ~p2 ~dst:target ~grow ~fade;
        let hit = age -. rose_hit in
        if hit > 0. then (
          (* the drink: the card goes dark and bloody as the life is emptied *)
          let u = clamp01 (hit /. 0.4) in
          let throb = 0.72 +. (0.28 *. sin (hit *. 17.)) in
          rect (target.x -. 40.) (target.y -. 45.) 80. 90. (rgb 0.34 0.02 0.04 (u *. throb *. 0.24 *. fade));
          (* and the petals burst out of the creature *)
          rose_petals ~at:target ~petals ~age ~fade)))

(* Underground Poison: the poison comes up through the soil under the creature it
   kills, and the same poison runs out along the roots under the floor to every
   other creature on that side.  Each lane bursts up through its own ground when
   the root reaches it, which is the six damage landing.  The picture stays on the
   ground rather than the card: the seep is a crack in the soil, the roots are
   tunnels that sag between the lanes, and the spores are what rises from them. *)
let underground_poison_duration = 1.8

(* A root of poison under the soil, from [a] to [b], sagging a little between the
   two ends and drawn [u] of the way along, with a bright head while it still grows. *)
let poison_vein ax ay bx by ~sag ~u fade =
  if u > 0.01 then (
    let steps = 14 in
    let at t = (lerp ax bx t, lerp ay by t +. (sag *. sin (t *. Float.pi))) in
    let prev = ref (at 0.) in
    for s = 1 to steps do
      let t = u *. float s /. float steps in
      let x, y = at t in
      let px, py = !prev in
      line px py x y 4.2 (rgb 0.08 0.1 0.03 (fade *. 0.9));
      line px py x y 1.3 (rgb 0.5 0.9 0.2 (fade *. 0.55));
      prev := (x, y)
    done;
    if u < 1. then (
      let hx, hy = !prev in
      additive (fun () -> circle hx hy 4.5 (rgb 0.6 1. 0.25 (fade *. 0.6)))))

(* The poison coming up through the ground at one creature: a pool soaking into the
   soil under its feet, a fume of spores rising through the card, and a dark red tint
   over the creature for the pain.  [size] scales it up for the creature that dies. *)
let poison_burst ~floor ~centre ~size ~seed ~age ~fade =
  let soak = ease_out (clamp01 (age /. 0.22)) in
  let late = clamp01 ((age -. 0.7) /. 0.5) in
  ellipse floor.x floor.y
    (4. +. (22. *. size *. soak))
    (2. +. (6. *. size *. soak))
    (rgb 0.2 0.42 0.08 (fade *. 0.6 *. (1. -. late)));
  for k = 0 to 7 do
    let born = noise seed k 1 *. 0.3 in
    let life = 0.6 +. (noise seed k 2 *. 0.4) in
    let a = age -. born in
    if a > 0. && a < life then (
      let u = a /. life in
      let x = floor.x +. ((noise seed k 3 -. 0.5) *. 40. *. size) +. (sin ((a *. 5.) +. float k) *. 4.) in
      let y = floor.y -. (u *. (50. +. (40. *. noise seed k 4)) *. size) in
      let al = (1. -. u) *. fade in
      circle x y (2.2 +. ((1. -. u) *. 2.4 *. size)) (rgb 0.7 1. 0.3 (al *. 0.85)))
  done;
  (* the hit: the creature is tinted dark red as it takes the poison, then the tint fades *)
  let pain = clamp01 (age /. 0.1) *. (1. -. clamp01 ((age -. 0.1) /. 0.6)) in
  if pain > 0. then
    rect (centre.x -. 40.) (centre.y -. 45.) 80. 90. (rgb 0.45 0.01 0.02 (pain *. fade *. 0.5))

(* The seep at the creature that dies: soil thrown up from a crack, which closes
   again, and ichor bubbling out of it. *)
let poison_seep ~floor ~age ~fade =
  let k = ease_out (clamp01 (age /. 0.3)) in
  let out = 1. -. k in
  for kk = 0 to 6 do
    let ang = (float kk *. 0.9) -. 0.4 in
    let d = (12. +. (noise kk 3 5 *. 16.)) *. k in
    circle
      (floor.x +. (cos ang *. d))
      (floor.y +. (sin ang *. d *. 0.45))
      (2.8 *. out)
      (rgb 0.24 0.17 0.12 (out *. fade *. 0.9))
  done;
  let w = 26. *. k in
  let shut = 1. -. clamp01 ((age -. 0.2) /. 0.4) in
  line (floor.x -. w) floor.y (floor.x +. w) floor.y 2. (rgb 0.2 0.5 0.1 (shut *. fade *. 0.8));
  for b = 0 to 5 do
    let a = age -. (noise b 7 8 *. 0.45) in
    if a > 0. && a < 0.3 then (
      let u = a /. 0.3 in
      let x = floor.x +. ((noise b 9 2 -. 0.5) *. 18.) in
      let y = floor.y -. (u *. 12.) in
      circle x y (1.4 +. (u *. 1.2)) (rgb 0.55 0.95 0.25 ((1. -. u) *. fade *. 0.8)))
  done

let spawn_underground_poison ~target ~floor ~foes ~start =
  let duration = underground_poison_duration in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.3) in
      if fade > 0.02 then (
        poison_seep ~floor ~age ~fade;
        (* the creature it kills sickens and goes under in a green bloom *)
        if age > 0.2 then poison_burst ~floor ~centre:target ~size:1.5 ~seed:11 ~age:(age -. 0.2) ~fade;
        (* the roots run out to every other creature on that side, one after another *)
        Array.iteri
          (fun i (lane_floor, lane_centre) ->
            let delay = 0.25 +. (float i *. 0.05) in
            let grow = 0.42 in
            let u = clamp01 ((age -. delay) /. grow) in
            poison_vein floor.x floor.y lane_floor.x lane_floor.y ~sag:12. ~u fade;
            let arrive = delay +. grow in
            if age > arrive then
              poison_burst ~floor:lane_floor ~centre:lane_centre ~size:1. ~seed:(i + 1) ~age:(age -. arrive) ~fade)
          foes))

(* Poisonous Roots: green smoke puffs up from the floor of each creature the lock lands on,
   at the start of the cast only.  Each puff rises from the floor, widens and thins, and the
   puffs follow one another so the smoke keeps coming for a moment. *)
let roots_smoke_duration = 1.2

let spawn_roots_smoke ~floors ~start =
  let duration = roots_smoke_duration in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.4) in
      if fade > 0.02 then
        Array.iteri
          (fun li (floor : spot) ->
            let seed = li + 1 in
            for i = 0 to 7 do
              let born = (float i *. 0.12) +. (noise seed i 1 *. 0.1) in
              let a = age -. born in
              let life = 1.0 +. (noise seed i 2 *. 0.3) in
              if a > 0. && a < life then (
                let u = a /. life in
                let x = floor.x +. ((noise seed i 3 -. 0.5) *. 16.) +. (sin ((u *. 4.) +. float i) *. 5.) in
                let y = floor.y -. (u *. 60.) in
                let r = 4. +. (14. *. u) in
                let al = fade *. sin (u *. Float.pi) in
                circle x y r (rgb 0.3 0.55 0.2 (al *. 0.35));
                circle x y (r *. 0.55) (rgb 0.2 0.4 0.12 (al *. 0.3)))
            done)
          floors)
(* ---------------------------------------------------------------- prism mage *)

(* Prism Mage: the foe's mana in the Called Element is drawn to the mage as a
   single ray, then split into a fan of rays that strike every foe creature. *)
let prism_duration = 1.2

let prism_element_color = function
  | 0 -> (1., 0.46, 0.12)
  | 1 -> (0.36, 0.72, 1.)
  | 2 -> (0.8, 0.94, 1.)
  | _ -> (0.46, 0.82, 0.3)

(* A straight ray from [a], grown [grow] of the way to [b]. *)
let prism_ray ~a ~b ~grow (r, g, bl) alpha =
  let x1 = a.x +. ((b.x -. a.x) *. grow) and y1 = a.y +. ((b.y -. a.y) *. grow) in
  line a.x a.y x1 y1 7. (rgb r g bl (alpha *. 0.16));
  line a.x a.y x1 y1 2.6 (rgb r g bl (alpha *. 0.5));
  line a.x a.y x1 y1 1. (rgb 1. 1. 1. (alpha *. 0.95))

let spawn_prism ~from_ ~at ~targets ~called ~start =
  let duration = prism_duration in
  let in_dur = 0.3 in
  let out_start = 0.4 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.25) in
      if fade > 0.02 then
        additive (fun () ->
            (* the ray arrives from the foe's Called Element label *)
            let grow_in = clamp01 (age /. in_dur) in
            if age <= in_dur +. 0.16 then
              prism_ray ~a:from_ ~b:at ~grow:grow_in (prism_element_color called)
                (fade *. (1. -. clamp01 ((age -. in_dur) /. 0.16)));
            (* the mage holds the light before splitting it *)
            if age >= in_dur -. 0.06 then (
              let pulse = 0.7 +. (0.3 *. sin (age *. 22.)) in
              let r, g, bl = prism_element_color called in
              circle at.x at.y
                (11. +. (7. *. clamp01 ((age -. in_dur) /. 0.25)))
                (rgb r g bl (fade *. 0.3 *. pulse));
              circle at.x at.y 4.5 (rgb 1. 1. 1. (fade *. 0.6)));
            (* the prism splits it onto every foe creature *)
            Array.iteri
              (fun id to_ ->
                let t0 = out_start +. (float (id mod 6) *. 0.045) in
                let grow = clamp01 ((age -. t0) /. 0.2) in
                if grow > 0.02 then (
                  prism_ray ~a:at ~b:to_ ~grow (prism_element_color called) fade;
                  if grow > 0.85 then (
                    let hit = (grow -. 0.85) /. 0.15 in
                    circle to_.x to_.y (7. +. (hit *. 15.)) (rgb 0.85 0.88 1. (fade *. (1. -. hit) *. 0.5));
                    circle to_.x to_.y 3.5 (rgb 1. 1. 1. (fade *. (1. -. hit))))))
              targets))

(* ---------------------------------------------------------------- astral storm *)

(* Astral Storm: the storm card throws a lash of astral energy at the foe. The
   arc crosses the board and lands on the foe themselves, then the lash wipes
   sideways through the foe's row -- it runs right over every card standing
   there -- so the whole row is wiped in one sweep. *)
let astral_storm_duration = 1.8

let astral_sample path u =
  let n = Array.length path in
  if n = 0 then { x = 0.; y = 0. }
  else
    let f = clamp01 u *. float (n - 1) in
    let i = int_of_float (Float.min (float (n - 1)) f) in
    let frac = f -. float i in
    let p = path.(i) and q = path.(min (n - 1) (i + 1)) in
    { x = p.x +. ((q.x -. p.x) *. frac); y = p.y +. ((q.y -. p.y) *. frac) }

(* The throw of the lash. It leaves the card in a bowed arc that crosses the
   board and lands on the foe, then wipes sideways through the foe's row: the
   wipe runs level with the cards and crosses every one of them, so it reads as
   a sweep and not as a touch. *)
let build_whip ~from_ ~targets ~foe_life =
  let segs = ref [] in
  let starts = ref [] in
  let total = ref 0 in
  let add seg =
    segs := !segs @ [ seg ];
    starts := !starts @ [ !total ];
    total := !total + List.length seg
  in
  let hits = ref [] in
  (* the arc to the foe, bowed aside so it reads as a throw *)
  let bow = 24. in
  let dx = foe_life.x -. from_.x and dy = foe_life.y -. from_.y in
  let l = Float.max 1. (hypot dx dy) in
  let d = { x = dx /. l; y = dy /. l } in
  let n = { x = -.d.y; y = d.x } in
  let ctrl =
    { x = ((from_.x +. foe_life.x) /. 2.) +. (n.x *. bow);
      y = ((from_.y +. foe_life.y) /. 2.) +. (n.y *. bow) }
  in
  let arc_n = 14 in
  add
    (List.init (arc_n + 1) (fun i ->
         let u = float i /. float arc_n in
         let q = 1. -. u in
         {
           x = (q *. q *. from_.x) +. (2. *. q *. u *. ctrl.x) +. (u *. u *. foe_life.x);
           y = (q *. q *. from_.y) +. (2. *. q *. u *. ctrl.y) +. (u *. u *. foe_life.y);
         }));
  let arc_seg = List.length !segs - 1 in
  (* the foe is struck first, where the arc lands *)
  hits := (List.nth !starts arc_seg + arc_n, foe_life) :: !hits;
  (* the wipe runs from the foe out across the row, level with the cards *)
  let ordered = List.sort (fun a b -> compare a.x b.x) (Array.to_list targets) in
  let row_y = match ordered with c :: _ -> c.y | [] -> foe_life.y in
  let last_x = match List.rev ordered with c :: _ -> c.x | [] -> foe_life.x +. 90. in
  let start_x = foe_life.x in
  let span = Float.max 1. ((last_x +. 70.) -. start_x) in
  let wipe_n = 42 in
  let wipe_pt u =
    let rise = Float.min 1. (u *. 4.) in
    { x = start_x +. (u *. span); y = foe_life.y +. ((row_y -. foe_life.y) *. rise) }
  in
  add (List.init (wipe_n + 1) (fun i -> wipe_pt (float i /. float wipe_n)));
  let wipe_seg = List.length !segs - 1 in
  let wipe_start = List.nth !starts wipe_seg in
  List.iter
    (fun c ->
      let u = clamp01 ((c.x -. start_x) /. span) in
      hits := (wipe_start + int_of_float (u *. float wipe_n), c) :: !hits)
    ordered;
  let path = Array.of_list (List.concat !segs) in
  let last = float (max 1 (Array.length path - 1)) in
  let hits = List.map (fun (i, c) -> (float i /. last, c)) !hits in
  (path, hits)

let spawn_astral_storm ~from_ ~targets ~foe_life ~start =
  let duration = astral_storm_duration in
  let path, hits = build_whip ~from_ ~targets ~foe_life in
  let n = Array.length path in
  let travel = 1.25 in (* seconds for the whole sweep *)
  let charge_t = 0.22 in (* the card gathers the storm first *)
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.3) in
      if fade > 0.02 then (
        (* the card gathers the storm before it lets it go *)
        let charge = clamp01 (age /. charge_t) in
        additive (fun () ->
            circle from_.x from_.y (15. +. (13. *. charge)) (rgb 0.36 0.16 0.82 (fade *. charge *. 0.09));
            circle from_.x from_.y (7. +. (5. *. charge)) (rgb 0.56 0.36 1. (fade *. charge *. 0.2));
            circle from_.x from_.y 3. (rgb 0.92 0.86 1. (fade *. charge *. 0.55));
            for k = 0 to 5 do
              let ang = (age *. 5.) +. (float k *. 1.047) in
              let r = 13. +. (6. *. sin ((age *. 6.) +. float k)) in
              circle
                (from_.x +. (cos ang *. r))
                (from_.y +. (sin ang *. r))
                1.8
                (rgb 0.8 0.7 1. (fade *. charge *. 0.5))
            done);
        let p = clamp01 ((age -. charge_t) /. travel) in
        (* the head accelerates: the lash creeps out and then whips *)
        let u = p *. p in
        let drawn = int_of_float (Float.min (float (n - 1)) (u *. float (n - 1))) in
        additive (fun () ->
            (* the lash itself: a thin white core in a violet strand *)
            for i = 0 to drawn - 1 do
              let s = path.(i) and t = path.(i + 1) in
              let k = float i /. float (max 1 (n - 2)) in
              line s.x s.y t.x t.y (2.2 +. (1.6 *. k)) (rgb 0.6 0.4 1. (fade *. 0.5));
              line s.x s.y t.x t.y (0.9 +. (0.9 *. k)) (rgb 0.96 0.93 1. (fade *. 0.95))
            done;
            (* the head of the lash, and the tail it drags *)
            let h = astral_sample path u in
            for s = 1 to 6 do
              let q = astral_sample path (clamp01 (u -. (float s *. 0.02))) in
              circle
                q.x q.y
                (4.6 -. (float s *. 0.55))
                (rgb 0.72 0.55 1. (fade *. (0.4 -. (float s *. 0.055))))
            done;
            circle h.x h.y 5.5 (rgb 0.5 0.28 1. (fade *. 0.5));
            circle h.x h.y 2.4 (rgb 0.95 0.9 1. (fade *. 0.95));
            (* the lash runs over a card and it cracks *)
            List.iter
              (fun (f, c) ->
                let ta = charge_t +. (sqrt f *. travel) in
                if age >= ta then (
                  let hit = clamp01 ((age -. ta) /. 0.32) in
                  let r = 10. in
                  circle c.x c.y (r +. (30. *. hit)) (rgb 0.5 0.28 1. (fade *. (1. -. hit) *. 0.42));
                  circle c.x c.y (r *. 0.4) (rgb 0.95 0.9 1. (fade *. (1. -. hit)));
                  for k = 0 to 7 do
                    let ang = (float k *. 0.785) +. ((noise k 3 9 -. 0.5) *. 0.9) in
                    let l0 = r *. 0.5 in
                    let l1 = l0 +. (16. *. hit) +. (noise k 5 4 *. 9.) in
                    line
                      (c.x +. (cos ang *. l0))
                      (c.y +. (sin ang *. l0))
                      (c.x +. (cos ang *. l1))
                      (c.y +. (sin ang *. l1))
                      1.6
                      (rgb 0.9 0.85 1. (fade *. (1. -. hit) *. 0.8))
                  done))
              hits)))

(* ------------------------------------------------------------ contextual mana *)

(* A creature that moves the mana of some houses but not all draws an arc from
   itself to each house it touches: a light green arc for a bonus, a dark red
   one for a malus. Non-blocking, so a repeating effect never holds the turn. *)
let mana_arc_duration = 0.7

let draw_mana_arc ~from_ ~to_ ~bonus ~i ~age =
  let duration = mana_arc_duration in
  let t = age -. (float i *. 0.04) in
  if t >= 0. && t < duration then (
    let fade = Float.min 1. ((duration -. t) /. 0.25) in
    let grow = clamp01 (t /. 0.2) in
    let bow = (if i mod 2 = 0 then 1. else -1.) *. (22. +. (float (i / 2) *. 10.)) in
    let dx = to_.x -. from_.x and dy = to_.y -. from_.y in
    let l = Float.max 1. (hypot dx dy) in
    let nx = -.dy /. l and ny = dx /. l in
    let ctrl =
      { x = ((from_.x +. to_.x) /. 2.) +. (nx *. bow); y = ((from_.y +. to_.y) /. 2.) +. (ny *. bow) }
    in
    (* a soft shimmer travelling along the arc, so it reads as a tether between
       planes rather than a solid damage beam *)
    let warp u = sin ((u *. 3.5) +. (t *. 2.2)) *. 3. *. sin (u *. Float.pi) in
    let point u =
      let q = 1. -. u in
      let x = (q *. q *. from_.x) +. (2. *. q *. u *. ctrl.x) +. (u *. u *. to_.x) in
      let y = (q *. q *. from_.y) +. (2. *. q *. u *. ctrl.y) +. (u *. u *. to_.y) in
      let w = warp u in
      { x = x +. (nx *. w); y = y +. (ny *. w) }
    in
    let n = 18 in
    let pts = List.init (n + 1) (fun k -> point (float k /. float n *. grow)) in
    let r, g, bl = if bonus then (0.42, 0.82, 0.46) else (0.62, 0.12, 0.12) in
    additive (fun () ->
        (* a translucent body with no hard core *)
        let rec segs = function
          | a :: (b :: _ as rest) ->
              line a.x a.y b.x b.y 2.4 (rgb r g bl (fade *. 0.1));
              line a.x a.y b.x b.y 0.9 (rgb r g bl (fade *. 0.42));
              segs rest
          | _ -> ()
        in
        segs pts;
        (* motes drifting along the tether *)
        for k = 0 to 6 do
          let u = mod_float ((float k /. 7.) +. (t *. 0.5)) 1. in
          if u <= grow then (
            let p = point u in
            let tw = 0.5 +. (0.5 *. sin ((t *. 5.) +. (float k *. 1.9))) in
            circle p.x p.y (1. +. (0.5 *. tw)) (rgb r g bl (fade *. 0.45 *. tw)))
        done;
        if grow > 0.9 then (
          let hit = (grow -. 0.9) /. 0.1 in
          circle to_.x to_.y (3. +. (4. *. hit)) (rgb r g bl (fade *. (1. -. hit) *. 0.35));
          circle to_.x to_.y 1.3 (rgb r g bl (fade *. (1. -. hit) *. 0.6)))))

(* Drawn in the underlay, so the tether sits behind the slot wells and the
   creature cards: it reads as reaching in from another plane. *)
let spawn_mana_arcs ~from_ ~links ~start =
  mk ~under:true ~start ~duration:mana_arc_duration (fun ~age ->
      List.iteri (fun i (to_, bonus) -> draw_mana_arc ~from_ ~to_ ~bonus ~i ~age) links)
