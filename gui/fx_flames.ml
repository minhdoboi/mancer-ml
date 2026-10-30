(* Spell and ability visuals: flames. *)

open Mancer
open Sp
module R = Raylib
open Fx_core
open Fx_lightning
open Fx_particles

(* ---------------------------------------------------------------- flames *)

let flame_column ~x ~y ~height ~fat ~hot ~alpha ~t ~id ~licks ~push =
  if alpha >= 0.03 && height >= 6. then (
    let bloom = Float.min 34. ((12. *. fat) +. 10.) in
    circle x (y -. 6.) bloom (rgb 1. 0.32 0.02 (alpha *. 0.2));
    for k = 0 to licks - 1 do
      let lean = (sin ((t *. 3.2) +. (float id *. 0.73) +. (float k *. 1.4)) *. 0.45) +. push in
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

(* A sheet of fire rolling sideways from [origin] across [left, left + span]. The
   crest races at [speed] px/s, leans forward and burns tallest; the body behind
   it stays alight, then dies back. [dirs] picks the directions it spreads in. *)
let fire_sheet ~left ~span ~y ~origin ~speed ~dirs ~t ~fade ~power ~seed =
  let cols = 34 in
  let reach = t *. speed in
  for i = 0 to cols do
    let x = left +. (float i /. float cols *. span) in
    let d = x -. origin in
    let dist = Float.abs d in
    let ok = (d >= 0. && dirs >= 0) || (d <= 0. && dirs <= 0) || dirs = 0 in
    let behind = reach -. dist in
    if ok && behind > -12. then
      let ignite = clamp01 ((behind +. 12.) /. 30.) in
      let body = exp (-.Float.max 0. (behind -. 40.) /. 230.) in
      let heat = ignite *. Float.max (ignite *. 0.35) body in
      if heat > 0.04 then
        let crest = exp (-.(behind -. 25.) *. (behind -. 25.) /. 2400.) in
        let flick = 0.88 +. (0.12 *. sin ((t *. 30.) +. float (i + seed))) in
        let ragged = 0.8 +. (0.4 *. noise i seed 7) in
        let h = (24. +. (56. *. heat)) *. (0.8 +. (0.9 *. crest)) *. flick *. ragged *. power in
        let side = if d >= 0. then 1. else -1. in
        flame_column ~x ~y ~height:h
          ~fat:((0.8 +. (0.3 *. heat)) *. (0.9 +. (0.3 *. power)))
          ~hot:(Float.min 1. (heat +. (crest *. 0.4)))
          ~alpha:(fade *. heat) ~t ~id:(i + seed) ~licks:3
          ~push:(side *. 0.95 *. crest)
  done;
  (* glowing bed of coals under the sheet and a white-hot leading edge *)
  let sweep a b =
    let lo = Float.max left (Float.min a b) and hi = Float.min (left +. span) (Float.max a b) in
    if hi -. lo > 6. then (
      let bed = fade *. Float.min 1. (t *. 4.) in
      line lo (y -. 4.) hi (y -. 4.) 22. (rgb 1. 0.3 0.03 (0.34 *. bed *. power));
      line lo (y -. 8.) hi (y -. 8.) 11. (rgb 1. 0.55 0.08 (0.5 *. bed *. power));
      line lo (y -. 10.) hi (y -. 10.) 4. (rgb 1. 0.9 0.4 (0.55 *. bed *. power)))
  in
  if dirs >= 0 then (
    let xf = Float.min (left +. span) (origin +. reach) in
    sweep origin xf;
    circle xf (y -. 20.) (16. +. (5. *. Float.abs (sin (t *. 26.)))) (rgb 1. 0.9 0.6 (0.45 *. fade *. power)));
  if dirs <= 0 then (
    let xf = Float.max left (origin -. reach) in
    sweep xf origin;
    circle xf (y -. 20.)
      (16. +. (5. *. Float.abs (sin ((t *. 26.) +. 1.))))
      (rgb 1. 0.9 0.6 (0.45 *. fade *. power)))

(* embers thrown up along the sheet *)
let sheet_embers ~left ~span ~y ~origin ~speed ~dirs ~t ~fade ~count =
  for e = 0 to count - 1 do
    let u = noise e 2 5 in
    let x = left +. (u *. span) in
    let d = x -. origin in
    if (d >= 0. && dirs >= 0) || (d <= 0. && dirs <= 0) || dirs = 0 then
      let born = Float.abs d /. speed in
      let life = 0.4 +. (0.35 *. noise e 3 1) in
      let a = t -. born in
      if a > 0. && a < life then
        let p = a /. life in
        let ex = x +. ((noise e 8 2 -. 0.5) *. 26.) +. ((if d >= 0. then 1. else -1.) *. 40. *. a) in
        let ey = y -. 14. -. ((70. +. (120. *. noise e 4 6)) *. p) in
        let col =
          if noise e 9 3 > 0.4 then rgb 1. 0.92 0.55 ((1. -. p) *. (1. -. p) *. fade)
          else rgb 1. 0.45 0.05 ((1. -. p) *. (1. -. p) *. fade *. 0.9)
        in
        circle ex ey (1.5 +. ((1. -. p) *. 2.3)) col
  done

let bounds bases =
  let xs = Array.map (fun p -> p.x) bases in
  if Array.length xs = 0 then (0., 0.)
  else (Array.fold_left min xs.(0) xs -. 44., Array.fold_left max xs.(0) xs +. 44.)

let spawn_flame_wave bases ~start =
  let left, right = bounds bases in
  let span = Float.max 1. (right -. left) in
  let y = if Array.length bases = 0 then 0. else bases.(0).y in
  let duration = 1.5 and speed = span /. 0.62 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.4) in
      if fade > 0.02 && Array.length bases > 0 then
        additive (fun () ->
            fire_sheet ~left ~span ~y ~origin:left ~speed ~dirs:1 ~t:age ~fade ~power:1. ~seed:0;
            sheet_embers ~left ~span ~y ~origin:left ~speed ~dirs:1 ~t:age ~fade ~count:70))

let envelope age rise hold fade =
  if age < 0. then 0.
  else if age < rise then age /. rise
  else if age < rise +. hold then 1.
  else Float.max 0. (1. -. ((age -. rise -. hold) /. fade))

let spawn_inferno ~target ~others ~start =
  let left, right = bounds (Array.append [| target |] others) in
  let span = Float.max 1. (right -. left) in
  let tx = target.x and ty = target.y in
  let duration = 1.7 and delay = 0.16 and speed = span /. 0.55 in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.4) in
      if fade > 0.02 then
        additive (fun () ->
            let t = age in
            (* the target is engulfed first, in a much bigger column *)
            let env = envelope t 0.08 0.7 0.4 in
            flame_column ~x:tx ~y:ty ~height:(150. *. env) ~fat:1.9 ~hot:1. ~alpha:(fade *. env) ~t ~id:1
              ~licks:5 ~push:0.;
            flame_column ~x:tx ~y:ty ~height:(80. *. env) ~fat:2.5 ~hot:1.
              ~alpha:(fade *. env *. 0.9)
              ~t ~id:2 ~licks:4 ~push:0.;
            circle tx (ty -. 24.) (34. *. env) (rgb 1. 0.95 0.75 (0.35 *. fade *. env));
            (* then it bursts outward as a wave over the other slots *)
            if t > delay then (
              let tt = t -. delay in
              fire_sheet ~left ~span ~y:ty ~origin:tx ~speed ~dirs:0 ~t:tt ~fade ~power:0.85 ~seed:40;
              sheet_embers ~left ~span ~y:ty ~origin:tx ~speed ~dirs:0 ~t:tt ~fade ~count:60);
            (if t < 0.45 then
               let p = t /. 0.45 in
               let ring = 16. +. (150. *. p) in
               let a = (1. -. p) *. fade in
               for k = 0 to 19 do
                 let ang = float k *. (Float.pi *. 2. /. 20.) in
                 circle
                   (tx +. (cos ang *. ring))
                   (ty -. 16. +. (sin ang *. ring *. 0.42))
                   (2.4 +. ((1. -. p) *. 2.6))
                   (rgb 1. 0.55 0.08 (a *. 0.7))
               done);
            for s = 0 to 29 do
              let life = 0.4 +. (0.25 *. noise s 1 4) in
              let a = t -. 0.03 in
              if a > 0. && a < life then
                let p = a /. life in
                let ang = -.(Float.pi /. 2.) +. ((noise s 2 8 -. 0.5) *. Float.pi *. 1.2) in
                let speed = 80. +. (170. *. noise s 6 2) in
                circle
                  (tx +. (cos ang *. speed *. a))
                  (ty -. 8. +. (sin ang *. speed *. a) -. (40. *. a))
                  ((2.6 *. (1. -. p)) +. 1.2)
                  (rgb 1. 0.78 0.25 ((1. -. p) *. fade))
            done))

(* Nuclear Missile: a missile streaks down onto every opponent slot, one after the other in a scattered
   order, and each one bursts into a fireball, a shock ring and a column of smoke. *)
let missile_fall = 0.4

let spawn_missiles ~pid ~bases ~start =
  let n = Array.length bases in
  let fall = missile_fall and gap = 0.11 and boom = 0.75 in
  let duration = (float (max 0 (n - 1)) *. gap) +. fall +. boom in
  let hit i = float (i * 5 mod max 1 n) *. gap in
  let reach = (70., 460.) in
  let dx, dy = reach in
  let len = hypot dx dy in
  let ux = dx /. len and uy = dy /. len in
  Array.iteri (fun i _ -> add_shake pid i ~start:(start +. hit i +. fall) ~duration:0.4 ~strength:0.8) bases;
  mk ~start ~duration (fun ~age ->
      Array.iteri
        (fun i base ->
          let t = age -. hit i in
          let cy = base.y -. 24. in
          (* target mark on the floor while the missile is on its way *)
          if t > -0.1 && t < fall then (
            let pulse = 0.6 +. (0.4 *. sin (t *. 30.)) in
            ellipse base.x base.y 30. 10. (rgb 1. 0.15 0.05 (0.22 *. pulse));
            ellipse base.x base.y 14. 5. (rgb 1. 0.4 0.15 (0.4 *. pulse)));
          if t >= 0. && t < fall then (
            let p = (t /. fall) ** 2. in
            let x = lerp (base.x -. dx) base.x p and y = lerp (cy -. dy) cy p in
            (* smoke trail, then the glowing exhaust *)
            for k = 1 to 10 do
              let back = float k *. 15. in
              let a = (1. -. (float k /. 11.)) *. 0.4 in
              circle (x -. (ux *. back)) (y -. (uy *. back)) (4. +. (float k *. 1.1)) (rgb 0.6 0.6 0.62 a)
            done;
            additive (fun () ->
                for k = 0 to 8 do
                  let back = float k *. 9. in
                  let a = 1. -. (float k /. 9.) in
                  circle
                    (x -. (ux *. back))
                    (y -. (uy *. back))
                    (7. -. (float k *. 0.6))
                    (rgb 1. 0.55 0.1 (a *. 0.8))
                done;
                circle x y 11. (rgb 1. 0.8 0.4 0.5));
            (* body, nose cone and fins *)
            let tx = x -. (ux *. 30.) and ty = y -. (uy *. 30.) in
            line tx ty x y 7. (rgb 0.78 0.8 0.84 1.);
            line tx ty x y 3. (rgb 0.95 0.96 1. 1.);
            tri
              (x -. (uy *. 3.5))
              (y +. (ux *. 3.5))
              (x +. (uy *. 3.5))
              (y -. (ux *. 3.5))
              (x +. (ux *. 11.))
              (y +. (uy *. 11.))
              (rgb 0.95 0.2 0.1 1.);
            tri tx ty
              (tx +. (uy *. 8.) -. (ux *. 7.))
              (ty -. (ux *. 8.) -. (uy *. 7.))
              (tx +. (ux *. 8.))
              (ty +. (uy *. 8.))
              (rgb 0.5 0.52 0.58 1.);
            tri tx ty
              (tx -. (uy *. 8.) -. (ux *. 7.))
              (ty +. (ux *. 8.) -. (uy *. 7.))
              (tx +. (ux *. 8.))
              (ty +. (uy *. 8.))
              (rgb 0.5 0.52 0.58 1.));
          let te = t -. fall in
          if te >= 0. && te < boom then (
            let p = te /. boom in
            let q = 1. -. p in
            (* smoke rises behind the fire *)
            if p > 0.12 then
              for k = 0 to 5 do
                let s = noise i k 11 in
                let ps = clamp01 ((p -. 0.12) /. 0.88) in
                circle
                  (base.x +. ((s -. 0.5) *. 54.) +. (sin ((te *. 3.) +. float k) *. 6.))
                  (cy -. (ps *. (30. +. (60. *. s))))
                  (12. +. (ps *. 22.))
                  (rgb 0.22 0.2 0.2 (q *. 0.5))
              done;
            additive (fun () ->
                if p < 0.4 then
                  circle base.x cy
                    ((70. *. (1. -. (p /. 0.4))) +. 12.)
                    (rgb 1. 0.97 0.8 ((1. -. (p /. 0.4)) ** 2.));
                let r = 16. +. (46. *. ease_out p) in
                circle base.x cy r (rgb 1. 0.4 0.05 (q *. 0.75));
                circle base.x cy (r *. 0.6) (rgb 1. 0.8 0.3 (q *. 0.8));
                flame_column ~x:base.x ~y:base.y
                  ~height:(110. *. envelope te 0.06 0.25 0.4)
                  ~fat:1.6 ~hot:1. ~alpha:q ~t:te ~id:(i + 7) ~licks:4 ~push:0.;
                (if p < 0.5 then
                   let pr = p /. 0.5 in
                   let ring = 14. +. (110. *. ease_out pr) in
                   for k = 0 to 17 do
                     let ang = float k *. (Float.pi *. 2. /. 18.) in
                     circle
                       (base.x +. (cos ang *. ring))
                       (cy +. 8. +. (sin ang *. ring *. 0.38))
                       ((2.6 *. (1. -. pr)) +. 1.)
                       (rgb 1. 0.6 0.15 ((1. -. pr) *. 0.8))
                   done);
                for s = 0 to 13 do
                  let life = 0.4 +. (0.3 *. noise i s 3) in
                  if te < life then
                    let ps = te /. life in
                    let ang = -.(Float.pi /. 2.) +. ((noise i s 5 -. 0.5) *. Float.pi *. 1.5) in
                    let speed = 110. +. (170. *. noise i s 9) in
                    circle
                      (base.x +. (cos ang *. speed *. te))
                      (cy +. (sin ang *. speed *. te) +. (260. *. te *. te))
                      ((2.4 *. (1. -. ps)) +. 1.)
                      (rgb 1. 0.8 0.3 (1. -. ps))
                done)))
        bases)

(* Scatterbomb: a clutch of fused bombs is lobbed from the caster's hand, spins over the board and lands on the
   opposing slots one after another. Each goes off in a fireball, a shock ring, sparks and a puff of smoke. *)
let spawn_scatterbomb ~pid ~src ~bases ~start =
  let n = Array.length bases in
  let fall = 0.45 and gap = 0.1 and boom = 0.7 in
  let duration = (float (max 0 (n - 1)) *. gap) +. fall +. boom in
  let hit i = float (i * 5 mod max 1 n) *. gap in
  Array.iteri
    (fun i (num, _) -> add_shake pid num ~start:(start +. hit i +. fall) ~duration:0.35 ~strength:0.7)
    bases;
  mk ~start ~duration (fun ~age ->
      Array.iteri
        (fun i (_, base) ->
          let t = age -. hit i in
          let cy = base.y -. 24. in
          if t >= 0. && t < fall then (
            let p = t /. fall in
            let arc = 110. +. (40. *. noise i 1 3) in
            let x = lerp src.x base.x p and y = lerp src.y cy p -. (arc *. 4. *. p *. (1. -. p)) in
            (* sparks shed by the burning fuse *)
            additive (fun () ->
                for k = 1 to 6 do
                  let pb = Float.max 0. (p -. (float k *. 0.03)) in
                  let tx = lerp src.x base.x pb
                  and ty = lerp src.y cy pb -. (arc *. 4. *. pb *. (1. -. pb)) in
                  circle tx (ty -. 8.) (3. -. (float k *. 0.4)) (rgb 1. 0.6 0.15 (0.7 -. (float k *. 0.1)))
                done);
            (* the bomb: round black body, a highlight, a fuse with a bright spark *)
            let sp = (t *. 9.) +. float i in
            circle x y 7. (rgb 0.08 0.08 0.1 1.);
            circle (x -. 2.2) (y -. 2.4) 2.2 (rgb 0.5 0.52 0.58 0.9);
            let fx = x +. (sin sp *. 3.) and fy = y -. 7. -. 5. in
            line (x +. 1.) (y -. 6.) fx fy 1.6 (rgb 0.75 0.6 0.3 1.);
            additive (fun () ->
                let fl = 0.7 +. (0.3 *. sin (t *. 60.)) in
                circle fx fy (4. *. fl) (rgb 1. 0.7 0.2 0.9);
                circle fx fy (1.8 *. fl) (rgb 1. 1. 0.8 1.)));
          let te = t -. fall in
          if te >= 0. && te < boom then (
            let p = te /. boom in
            let q = 1. -. p in
            if p > 0.12 then
              for k = 0 to 4 do
                let s = noise i k 11 in
                let ps = clamp01 ((p -. 0.12) /. 0.88) in
                circle
                  (base.x +. ((s -. 0.5) *. 46.) +. (sin ((te *. 3.) +. float k) *. 5.))
                  (cy -. (ps *. (24. +. (46. *. s))))
                  (10. +. (ps *. 17.))
                  (rgb 0.2 0.18 0.18 (q *. 0.5))
              done;
            additive (fun () ->
                if p < 0.35 then
                  circle base.x cy
                    ((52. *. (1. -. (p /. 0.35))) +. 10.)
                    (rgb 1. 0.96 0.78 ((1. -. (p /. 0.35)) ** 2.));
                let r = 14. +. (34. *. ease_out p) in
                circle base.x cy r (rgb 1. 0.42 0.06 (q *. 0.75));
                circle base.x cy (r *. 0.6) (rgb 1. 0.82 0.35 (q *. 0.8));
                (if p < 0.5 then
                   let pr = p /. 0.5 in
                   let ring = 12. +. (80. *. ease_out pr) in
                   for k = 0 to 15 do
                     let ang = float k *. (Float.pi *. 2. /. 16.) in
                     circle
                       (base.x +. (cos ang *. ring))
                       (cy +. 8. +. (sin ang *. ring *. 0.4))
                       ((2.2 *. (1. -. pr)) +. 1.)
                       (rgb 1. 0.6 0.15 ((1. -. pr) *. 0.8))
                   done);
                for s = 0 to 11 do
                  let life = 0.35 +. (0.3 *. noise i s 3) in
                  if te < life then
                    let ps = te /. life in
                    let ang = -.(Float.pi /. 2.) +. ((noise i s 5 -. 0.5) *. Float.pi *. 1.7) in
                    let speed = 90. +. (150. *. noise i s 9) in
                    circle
                      (base.x +. (cos ang *. speed *. te))
                      (cy +. (sin ang *. speed *. te) +. (240. *. te *. te))
                      ((2.2 *. (1. -. ps)) +. 1.)
                      (rgb 1. 0.8 0.3 (1. -. ps))
                done)))
        bases)

(* Astral Grenadier: a single astral grenade is lobbed from the card onto the
   creature the foe just summoned, then goes off in a violet blast. *)
let grenade_duration = 1.15

let spawn_grenade ~src ~target ~pid ~num ~start =
  let fall = 0.45 and boom = 0.7 in
  let cy = target.y -. 24. in
  add_shake pid num ~start:(start +. fall) ~duration:0.35 ~strength:0.7;
  mk ~start ~duration:grenade_duration (fun ~age ->
      if age >= 0. && age < fall then (
        let p = age /. fall in
        let arc = 120. in
        let x = lerp src.x target.x p and y = lerp src.y cy p -. (arc *. 4. *. p *. (1. -. p)) in
        (* the burning fuse sheds astral sparks *)
        additive (fun () ->
            for k = 1 to 6 do
              let pb = Float.max 0. (p -. (float k *. 0.03)) in
              let tx = lerp src.x target.x pb
              and ty = lerp src.y cy pb -. (arc *. 4. *. pb *. (1. -. pb)) in
              circle tx (ty -. 8.) (3. -. (float k *. 0.4)) (rgb 0.72 0.5 1. (0.7 -. (float k *. 0.1)))
            done);
        (* the grenade: dark body with a violet sheen, a fuse and a bright spark *)
        let sp = age *. 9. in
        circle x y 7. (rgb 0.12 0.1 0.18 1.);
        circle (x -. 2.2) (y -. 2.4) 2.2 (rgb 0.6 0.55 0.82 0.9);
        additive (fun () ->
            circle x y 10. (rgb 0.5 0.3 1. 0.16);
            let fx = x +. (sin sp *. 3.) and fy = y -. 7. -. 5. in
            line (x +. 1.) (y -. 6.) fx fy 1.6 (rgb 0.72 0.6 0.5 1.);
            let fl = 0.7 +. (0.3 *. sin (age *. 60.)) in
            circle fx fy (4. *. fl) (rgb 0.82 0.62 1. 0.9);
            circle fx fy (1.8 *. fl) (rgb 1. 1. 1. 1.)));
      let te = age -. fall in
      if te >= 0. && te < boom then (
        let p = te /. boom in
        let q = 1. -. p in
        (* a puff of violet smoke *)
        if p > 0.12 then
          for k = 0 to 4 do
            let s = noise k 3 11 in
            let ps = clamp01 ((p -. 0.12) /. 0.88) in
            circle
              (target.x +. ((s -. 0.5) *. 46.) +. (sin ((te *. 3.) +. float k) *. 5.))
              (cy -. (ps *. (24. +. (46. *. s))))
              (10. +. (ps *. 17.))
              (rgb 0.18 0.16 0.24 (q *. 0.5))
          done;
        additive (fun () ->
            if p < 0.35 then
              circle target.x cy
                ((52. *. (1. -. (p /. 0.35))) +. 10.)
                (rgb 0.93 0.9 1. ((1. -. (p /. 0.35)) ** 2.));
            let r = 14. +. (34. *. ease_out p) in
            circle target.x cy r (rgb 0.45 0.2 0.95 (q *. 0.75));
            circle target.x cy (r *. 0.6) (rgb 0.78 0.58 1. (q *. 0.8));
            (if p < 0.5 then
               let pr = p /. 0.5 in
               let ring = 12. +. (80. *. ease_out pr) in
               for k = 0 to 15 do
                 let ang = float k *. (Float.pi *. 2. /. 16.) in
                 circle
                   (target.x +. (cos ang *. ring))
                   (cy +. 8. +. (sin ang *. ring *. 0.4))
                   ((2.2 *. (1. -. pr)) +. 1.)
                   (rgb 0.72 0.5 1. ((1. -. pr) *. 0.8))
               done);
            for s = 0 to 11 do
              let life = 0.35 +. (0.3 *. noise s 3 3) in
              if te < life then
                let ps = te /. life in
                let ang = -.(Float.pi /. 2.) +. ((noise s 5 5 -. 0.5) *. Float.pi *. 1.7) in
                let speed = 90. +. (150. *. noise s 9 9) in
                circle
                  (target.x +. (cos ang *. speed *. te))
                  (cy +. (sin ang *. speed *. te) +. (240. *. te *. te))
                  ((2.2 *. (1. -. ps)) +. 1.)
                  (rgb 0.86 0.72 1. (1. -. ps))
            done)))

(* Unstable Cannon: a bomb is lobbed from the cannon onto a foe creature and
   goes off in a burst of fire and shrapnel.  The cannon fires a salvo, so
   the bombs follow one another, a little late each. *)
let cannon_bomb_duration = 1.35
let cannon_bomb_fall = 0.42
let cannon_bomb_boom = 0.8
let cannon_bomb_step = 0.16

(* One bomb, drawn at [age] seconds past its own start. *)
let draw_cannon_bomb ~src ~target ~age =
  let fall = cannon_bomb_fall and boom = cannon_bomb_boom in
  let cy = target.y -. 22. in
  if age >= 0. && age < fall then (
    let p = age /. fall in
    let arc = 130. in
    let x = lerp src.x target.x p and y = lerp src.y cy p -. (arc *. 4. *. p *. (1. -. p)) in
    (* the burning fuse sheds sparks *)
    additive (fun () ->
        for k = 1 to 6 do
          let pb = Float.max 0. (p -. (float k *. 0.035)) in
          let tx = lerp src.x target.x pb
          and ty = lerp src.y cy pb -. (arc *. 4. *. pb *. (1. -. pb)) in
          circle tx (ty -. 6.) (2.6 -. (float k *. 0.3)) (rgb 1. 0.55 0.12 (0.65 -. (float k *. 0.09)))
        done);
    (* the shell: dark iron, a hot rim and a burning fuse *)
    let sp = age *. 11. in
    circle x y 6.5 (rgb 0.14 0.12 0.13 1.);
    circle (x -. 2.) (y -. 2.) 2. (rgb 0.45 0.42 0.4 0.85);
    additive (fun () ->
        circle x y 11. (rgb 1. 0.45 0.08 0.14);
        let fx = x +. (sin sp *. 2.6) and fy = y -. 6. -. 5. in
        line (x +. 1.) (y -. 5.) fx fy 1.5 (rgb 0.5 0.4 0.32 1.);
        let fl = 0.7 +. (0.3 *. sin (age *. 55.)) in
        circle fx fy (4.5 *. fl) (rgb 1. 0.6 0.15 0.95);
        circle fx fy (1.9 *. fl) (rgb 1. 0.95 0.7 1.)))
  else
    let te = age -. fall in
    if te >= 0. && te < boom then (
      let p = te /. boom in
      let q = 1. -. p in
      (* a puff of smoke *)
      if p > 0.1 then
        for k = 0 to 4 do
          let s = noise k 7 13 in
          let ps = clamp01 ((p -. 0.1) /. 0.9) in
          circle
            (target.x +. ((s -. 0.5) *. 44.) +. (sin ((te *. 3.) +. float k) *. 5.))
            (cy -. (ps *. (26. +. (44. *. s))))
            (11. +. (ps *. 16.))
            (rgb 0.16 0.13 0.12 (q *. 0.55))
        done;
      additive (fun () ->
          (* the flash *)
          if p < 0.3 then
            circle target.x cy
              ((46. *. (1. -. (p /. 0.3))) +. 10.)
              (rgb 1. 0.92 0.72 ((1. -. (p /. 0.3)) ** 2.));
          (* the fireball *)
          let r = 15. +. (32. *. ease_out p) in
          circle target.x cy r (rgb 1. 0.45 0.06 (q *. 0.8));
          circle target.x cy (r *. 0.62) (rgb 1. 0.78 0.25 (q *. 0.85));
          circle target.x cy (r *. 0.3) (rgb 1. 0.97 0.85 (q *. 0.9));
          (* the shrapnel ring *)
          if p < 0.55 then (
            let pr = p /. 0.55 in
            let ring = 10. +. (74. *. ease_out pr) in
            for k = 0 to 13 do
              let ang = (float k *. (Float.pi *. 2. /. 14.)) +. 0.3 in
              circle
                (target.x +. (cos ang *. ring))
                (cy +. 8. +. (sin ang *. ring *. 0.45))
                ((2.4 *. (1. -. pr)) +. 0.8)
                (rgb 1. 0.6 0.15 ((1. -. pr) *. 0.85))
            done);
          (* the embers *)
          for s = 0 to 11 do
            let life = 0.35 +. (0.3 *. noise s 5 7) in
            if te < life then
              let ps = te /. life in
              let ang = -.(Float.pi /. 2.) +. ((noise s 3 9 -. 0.5) *. Float.pi *. 1.8) in
              let speed = 100. +. (160. *. noise s 7 4) in
              circle
                (target.x +. (cos ang *. speed *. te))
                (cy +. (sin ang *. speed *. te) +. (250. *. te *. te))
                ((2.4 *. (1. -. ps)) +. 0.9)
                (rgb 1. 0.72 0.22 (1. -. ps))
          done))

(* The Cannon's salvo: every bomb flies from the same muzzle and lands on its
   own creature, a moment after the one before it. *)
let cannon_bomb_span n = cannon_bomb_duration +. (float (max 0 (n - 1)) *. cannon_bomb_step)

let spawn_cannon_bombs ~src ~targets ~start =
  List.iteri
    (fun i (pid, num, _) ->
      add_shake pid num
        ~start:(start +. (float i *. cannon_bomb_step) +. cannon_bomb_fall)
        ~duration:0.35 ~strength:0.8)
    targets;
  mk ~start ~duration:(cannon_bomb_span (List.length targets)) (fun ~age ->
      List.iteri
        (fun i (_, _, at) ->
          let age = age -. (float i *. cannon_bomb_step) in
          if age >= 0. then draw_cannon_bomb ~src ~target:at ~age)
        targets)
