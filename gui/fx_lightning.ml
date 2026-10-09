(* Spell and ability visuals: lightning bolts. *)

open Mancer
open Sp
module R = Raylib
open Fx_core

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
