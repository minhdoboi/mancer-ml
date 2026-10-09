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
      let lean = sin ((t *. 3.2) +. (float id *. 0.73) +. (float k *. 1.4)) *. 0.45 +. push in
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
    if ok && behind > -12. then (
      let ignite = clamp01 ((behind +. 12.) /. 30.) in
      let body = exp (-.Float.max 0. (behind -. 40.) /. 230.) in
      let heat = ignite *. Float.max (ignite *. 0.35) body in
      if heat > 0.04 then (
        let crest = exp (-.(behind -. 25.) *. (behind -. 25.) /. 2400.) in
        let flick = 0.88 +. (0.12 *. sin ((t *. 30.) +. float (i + seed))) in
        let ragged = 0.8 +. (0.4 *. noise i seed 7) in
        let h = (24. +. (56. *. heat)) *. (0.8 +. (0.9 *. crest)) *. flick *. ragged *. power in
        let side = if d >= 0. then 1. else -1. in
        flame_column ~x ~y:y ~height:h
          ~fat:((0.8 +. (0.3 *. heat)) *. (0.9 +. (0.3 *. power)))
          ~hot:(Float.min 1. (heat +. (crest *. 0.4)))
          ~alpha:(fade *. heat) ~t ~id:(i + seed) ~licks:3 ~push:(side *. 0.95 *. crest)))
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
    circle xf (y -. 20.) (16. +. (5. *. Float.abs (sin ((t *. 26.) +. 1.)))) (rgb 1. 0.9 0.6 (0.45 *. fade *. power)))

(* embers thrown up along the sheet *)
let sheet_embers ~left ~span ~y ~origin ~speed ~dirs ~t ~fade ~count =
  for e = 0 to count - 1 do
    let u = noise e 2 5 in
    let x = left +. (u *. span) in
    let d = x -. origin in
    if (d >= 0. && dirs >= 0) || (d <= 0. && dirs <= 0) || dirs = 0 then (
      let born = Float.abs d /. speed in
      let life = 0.4 +. (0.35 *. noise e 3 1) in
      let a = t -. born in
      if a > 0. && a < life then
        let p = a /. life in
        let ex = x +. ((noise e 8 2 -. 0.5) *. 26.) +. (if d >= 0. then 1. else -1.) *. 40. *. a in
        let ey = y -. 14. -. ((70. +. (120. *. noise e 4 6)) *. p) in
        let col =
          if noise e 9 3 > 0.4 then rgb 1. 0.92 0.55 ((1. -. p) *. (1. -. p) *. fade)
          else rgb 1. 0.45 0.05 ((1. -. p) *. (1. -. p) *. fade *. 0.9)
        in
        circle ex ey (1.5 +. ((1. -. p) *. 2.3)) col)
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
              ~alpha:(fade *. env *. 0.9) ~t ~id:2 ~licks:4 ~push:0.;
            circle tx (ty -. 24.) (34. *. env) (rgb 1. 0.95 0.75 (0.35 *. fade *. env));
            (* then it bursts outward as a wave over the other slots *)
            if t > delay then (
              let tt = t -. delay in
              fire_sheet ~left ~span ~y:ty ~origin:tx ~speed ~dirs:0 ~t:tt ~fade ~power:0.85 ~seed:40;
              sheet_embers ~left ~span ~y:ty ~origin:tx ~speed ~dirs:0 ~t:tt ~fade ~count:60);
            if t < 0.45 then (
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
