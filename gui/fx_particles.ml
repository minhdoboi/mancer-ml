(* Spell and ability visuals: particle emitters and the hover smoke. *)

open Mancer
open Sp
module R = Raylib
open Fx_core
open Fx_lightning

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
      let paint () =
        List.iter
          (fun p ->
            let u = clamp01 ((age -. p.born) /. p.life) in
            let r, g, b = color_at e.cfg u in
            let a = alpha_at e.cfg u *. fade in
            if a > 0.02 then (
              let rad = p.size *. (0.7 +. ((1. -. u) *. 0.2)) in
              circle p.px p.py (rad *. 1.35) (rgb r g b (a *. 0.28));
              circle p.px p.py rad (rgb r g b (a *. 0.9))))
          e.puffs
      in
      if e.cfg.additive then additive paint else paint ()
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

(* A line spawn is centred on the emitter, so an effect meant to cover a whole
   slot row has to sit at the middle of the row and stretch to its width. *)
let spawn_row_particles cfg (spots : spot array) ~start =
  let n = Array.length spots in
  if n = 0 then mk ~start ~duration:0. (fun ~age:_ -> ())
  else
    let a = spots.(0) and b = spots.(n - 1) in
    let x = (a.x +. b.x) /. 2. and y = (a.y +. b.y) /. 2. in
    let cfg =
      match cfg.spawn with
      | Line (_, h) ->
          let pitch = if n > 1 then Float.abs (b.x -. a.x) /. float (n - 1) else 0. in
          { cfg with spawn = Line (pitch *. float n *. 1.1, h) }
      | _ -> cfg
    in
    spawn_particles cfg ~x ~y ~start

let nature_heal =
  {
    duration = 0.85;
    rate = 90.;
    life = (0.45, 0.8);
    max_particles = 200;
    spawn = Square (28., 22.);
    ox = 0.;
    oy = 0.;
    speed = (16., 34.);
    angle = (70., 110.);
    gravity = 0.;
    wind = (0., 0.);
    size = (1.6, 2.8);
    colors = [ { u = 0.; r = 0.21; g = 1.; b = 0.05 } ];
    alphas = [ { u = 0.; a = 0. }; { u = 0.2; a = 1. }; { u = 0.8; a = 0.75 }; { u = 1.; a = 0. } ];
    additive = true;
    texture = "";
  }

let spawn_heal_motes (spots : spot list) ~start =
  let emitters = List.map (fun p -> make_emitter nature_heal ~x:p.x ~y:p.y) spots in
  let duration = Float.min 3. (nature_heal.duration +. snd nature_heal.life) in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.2) in
      if fade > 0.02 then List.iter (fun e -> draw_emitter e ~age ~fade) emitters)

(* Plant therapy: pollen, a mix of green and yellow motes drifting higher *)
let pollen_green =
  {
    nature_heal with
    rate = 55.;
    life = (0.9, 1.5);
    max_particles = 160;
    spawn = Square (34., 24.);
    speed = (30., 58.);
    angle = (62., 118.);
    wind = (4., 0.);
    size = (1.8, 3.2);
    colors = [ { u = 0.; r = 0.3; g = 1.; b = 0.1 }; { u = 1.; r = 0.55; g = 0.95; b = 0.2 } ];
  }

let pollen_yellow =
  {
    pollen_green with
    rate = 45.;
    size = (2., 3.6);
    colors = [ { u = 0.; r = 1.; g = 0.9; b = 0.2 }; { u = 1.; r = 1.; g = 0.8; b = 0.3 } ];
  }

(* Sexual bond: the same rising pollen, blushed pink. *)
let pollen_pink =
  {
    pollen_green with
    colors = [ { u = 0.; r = 1.; g = 0.35; b = 0.7 }; { u = 1.; r = 1.; g = 0.68; b = 0.85 } ];
  }

let pollen_rose =
  {
    pollen_yellow with
    colors = [ { u = 0.; r = 1.; g = 0.25; b = 0.55 }; { u = 1.; r = 1.; g = 0.55; b = 0.78 } ];
  }

let pollen_duration = 0.85 +. 1.5

let spawn_pollen ?(pink = false) (spots : spot list) ~start =
  let first = if pink then pollen_pink else pollen_green in
  let second = if pink then pollen_rose else pollen_yellow in
  let emitters =
    List.concat_map (fun p -> [ make_emitter first ~x:p.x ~y:p.y; make_emitter second ~x:p.x ~y:p.y ]) spots
  in
  mk ~start ~duration:pollen_duration (fun ~age ->
      let fade = Float.min 1. ((pollen_duration -. age) /. 0.2) in
      if fade > 0.02 then List.iter (fun e -> draw_emitter e ~age ~fade) emitters)

(* Natures care: the healing motes plus small leaves tumbling up and away *)
let nature_care_duration = 2.6

let spawn_nature_care (spots : spot list) ~start =
  let motes = spawn_heal_motes spots ~start in
  mk ~start ~duration:nature_care_duration (fun ~age ->
      motes.draw ~age;
      let fade = Float.min 1. ((nature_care_duration -. age) /. 0.3) in
      if fade > 0.02 then
        List.iteri
          (fun j (p : spot) ->
            for i = 0 to 11 do
              let born = noise i j 1 *. 0.9 in
              let life = 1.1 +. (noise i j 2 *. 0.6) in
              let a = age -. born in
              if a > 0. && a < life then (
                let k = a /. life in
                let x0 = p.x +. ((noise i j 3 -. 0.5) *. 40.) in
                let sway =
                  sin ((a *. (3. +. (noise i j 4 *. 2.))) +. (noise i j 5 *. 6.28)) *. (10. +. (10. *. k))
                in
                let x = x0 +. sway +. ((noise i j 6 -. 0.5) *. 30. *. k) in
                let y = p.y -. 6. -. ((50. +. (60. *. noise i j 7)) *. (1. -. ((1. -. k) ** 1.6))) in
                let ang = (a *. (3. +. (noise i j 8 *. 4.))) +. (noise i j 9 *. 6.28) in
                let al = fade *. sin (Float.pi *. Float.min 1. (k *. 1.15)) in
                let len = 4.5 +. (noise i j 10 *. 4.) in
                let tone = noise i j 11 in
                let r, g, b =
                  if tone < 0.3 then (0.62, 0.8, 0.22)
                  else if tone < 0.65 then (0.25, 0.68, 0.2)
                  else (0.15, 0.5, 0.15)
                in
                leaf x y ang len al r g b;
                leaf x y (ang +. Float.pi) (len *. 0.7) al (r *. 0.8) (g *. 0.85) b)
            done)
          spots)

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

(* Acidic shower: slanted streaks of rain pour onto every slot's own column,
   splash on the slot floor and leave a low haze. [blue] keeps the same shape
   but rains clean water-blue instead of toxic green, for Rain of Tears. *)
let spawn_acid_rain ?(blue = false) (floors : spot array) ~start =
  let duration = 2.4 in
  let c (r, g, b) a = rgb r g b a in
  let haze_hi = if blue then (0.35, 0.7, 1.) else (0.5, 1., 0.35) in
  let haze_lo = if blue then (0.15, 0.45, 0.9) else (0.2, 0.8, 0.15) in
  let streak = if blue then (0.2, 0.55, 1.) else (0.25, 0.9, 0.18) in
  let seed = if blue then (0.55, 0.82, 1.) else (0.75, 1., 0.55) in
  let splash = if blue then (0.35, 0.7, 1.) else (0.45, 1., 0.3) in
  let mist_lo = if blue then (0.15, 0.22, 0.35) else (0.2, 0.28, 0.18) in
  let mist_hi = if blue then (0.3, 0.45, 0.65) else (0.42, 0.52, 0.34) in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.4) in
      let power = Float.min 1. (age /. 0.35) *. fade in
      if power > 0.02 then
        additive (fun () ->
            Array.iteri
              (fun j (f : spot) ->
                let haze = power *. (0.75 +. (0.25 *. sin ((age *. 6.) +. float j))) in
                ellipse f.x (f.y -. 4.) 46. 9. (c haze_lo (0.2 *. haze));
                ellipse f.x (f.y -. 4.) 28. 5. (c haze_hi (0.16 *. haze));
                for d = 0 to 43 do
                  (* only some drops are alive while the shower builds and thins out *)
                  if noise d j 9 < power then (
                    let cycle = 0.42 +. (0.14 *. noise d j 2) in
                    let p = Float.rem ((age /. cycle) +. noise d j 3) 1. in
                    let x = f.x +. ((noise d j 1 -. 0.5) *. 74.) in
                    let depth = noise d j 4 *. 7. in
                    let top = 175. +. (30. *. noise d j 5) in
                    let slant = 9. in
                    if p < 0.82 then (
                      let q = p /. 0.82 in
                      let y = f.y -. depth -. (top *. (1. -. q)) in
                      let hx = x -. (slant *. (1. -. q)) in
                      line (hx -. 3.) (y -. 16.) hx y 1.5 (c streak (0.55 *. power));
                      circle hx y 1.5 (c seed (0.8 *. power)))
                    else
                      let q = (p -. 0.82) /. 0.18 in
                      let w = 3. +. (9. *. q) in
                      ellipse x (f.y -. depth) w (w *. 0.35) (c splash ((1. -. q) *. 0.55 *. power));
                      if q < 0.7 then
                        circle
                          (x +. ((noise d j 6 -. 0.5) *. 16. *. q))
                          (f.y -. depth -. (7. *. sin (q *. 4.)))
                          1.2
                          (c seed ((1. -. q) *. 0.8 *. power)))
                done)
              floors);
      (* low mist hanging where the drops land: it shows the damage *)
      if fade > 0.02 then
        Array.iteri
          (fun j (f : spot) ->
            for k = 0 to 9 do
              let born = 0.15 +. (noise k j 11 *. 1.3) in
              let life = 0.9 +. (noise k j 12 *. 0.6) in
              let a = age -. born in
              if a > 0. && a < life then (
                let q = a /. life in
                let x = f.x +. ((noise k j 13 -. 0.5) *. 56.) +. (sin ((a *. 2.5) +. float k) *. 6. *. q) in
                let y = f.y -. 6. -. ((38. +. (36. *. noise k j 14)) *. ease_out q) in
                let r = 7. +. (15. *. q) in
                let al = sin (Float.pi *. Float.min 1. (q ** 0.7)) *. 0.42 *. fade in
                circle x y r (c mist_lo al);
                circle (x -. (r *. 0.25)) (y -. (r *. 0.2)) (r *. 0.6) (c mist_hi (al *. 0.7)))
            done)
          floors)

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

(* Call of the Fittest: a bluish haze rising off each card of the buffed house. *)
let label_haze =
  {
    smoke_cfg with
    duration = 0.9;
    rate = 40.;
    life = (0.8, 1.3);
    max_particles = 50;
    spawn = Square (60., 70.);
    speed = (8., 22.);
    angle = (60., 120.);
    gravity = 0.;
    size = (34., 46.);
    colors = [ { u = 0.; r = 0.45; g = 0.7; b = 1. }; { u = 1.; r = 0.2; g = 0.3; b = 0.7 } ];
    alphas = [ { u = 0.; a = 0. }; { u = 0.25; a = 0.85 }; { u = 1.; a = 0. } ];
    additive = true;
  }

let spawn_heal_haze (spots : spot array) ~start =
  let emitters = Array.map (fun p -> make_emitter label_haze ~x:p.x ~y:p.y) spots in
  let duration = Float.min 3. (label_haze.duration +. snd label_haze.life) in
  mk ~start ~duration (fun ~age ->
      let fade = Float.min 1. ((duration -. age) /. 0.2) in
      if fade > 0.02 then Array.iter (fun e -> draw_emitter e ~age ~fade) emitters)

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
