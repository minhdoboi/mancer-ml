(* Procedural beach waves for the lobby, ported from the beach-wave loop
   generator. Two value-noise tables are shaped by a slow swell envelope and a
   faster surge, plus a gusting wind layer, then low-passed with a cutoff that follows the swell. Every
   term is a smooth function of time, so the stream never needs to loop: it
   just keeps synthesising into a raylib AudioStream. *)

module R = Raylib

let sample_rate = 44100
let chunk = 8192 (* frames per update (~186 ms) = one stream sub-buffer, so a slow frame cannot starve it *)
let intensity = 0.8
let wind_level = 0.35
let speed = 1.0
let two_pi = 2. *. Float.pi

(* small deterministic value noise on a 4096-entry table, smoothstep-interpolated *)
let make_noise seed =
  let s = ref (seed land 0xFFFFFFFF) in
  let rnd () =
    s := ((!s * 1664525) + 1013904223) land 0xFFFFFFFF;
    float !s /. 4294967296.
  in
  let table = Array.init 4096 (fun _ -> (rnd () *. 2.) -. 1.) in
  fun t ->
    let x = t *. 4096. in
    let i = Float.to_int (Float.floor x) in
    let f = x -. Float.floor x in
    let a = table.(i land 4095) and b = table.((i + 1) land 4095) in
    let u = f *. f *. (3. -. (2. *. f)) in
    a +. ((b -. a) *. u)

(* sum of incommensurate slow sines: gentle lulls, strong surfs *)
let envelope t =
  let a = sin ((two_pi *. 0.071 *. speed *. t) +. 1.3)
  and b = sin ((two_pi *. 0.043 *. speed *. t) +. 4.1)
  and c = sin ((two_pi *. 0.117 *. speed *. t) +. 0.7) in
  let e = (((a +. b +. c) /. 3.) +. 1.) /. 2. in
  Float.pow e 1.6

(* wind: slowly varying gust envelope 0..1 *)
let gust t =
  let a = sin ((two_pi *. 0.052 *. t) +. 2.2)
  and b = sin ((two_pi *. 0.089 *. t) +. 5.6)
  and c = sin ((two_pi *. 0.023 *. t) +. 1.1) in
  let e = (((a *. 0.5) +. (b *. 0.3) +. (c *. 0.6)) /. 1.4 +. 1.) /. 2. in
  Float.pow e 1.3

(* deep gust body from [lo] plus a higher hiss from [mid] that swells with the gust *)
let wind_sample lo mid t =
  let g = gust t in
  let whoosh = lo ((t *. 0.55) +. (sin (t *. 0.013) *. 0.8)) in
  let whistle = mid ((t *. 4.7) +. (sin ((t *. 0.031) +. 1.) *. 0.9)) in
  ((whoosh *. 0.75) +. (whistle *. 0.25 *. g)) *. (0.35 +. (0.65 *. g)) *. 0.45

type t = {
  stream : R.AudioStream.t;
  buf : int Ctypes.CArray.t;
  n1 : float -> float;
  n2 : float -> float;
  w1 : float -> float;
  w2 : float -> float;
  w3 : float -> float;
  w4 : float -> float;
  mutable frame : int;
  mutable waves : float; (* 1 = beach (lobby), 0 = wind only (wind board); eases between *)
  mutable fade : float; (* 0 = silent, 1 = full; ramps so entering the lobby is not abrupt *)
  mutable lp_l : float * float; (* two cascaded one-pole low-pass states, ≈ the biquad *)
  mutable lp_r : float * float;
  mutable level : float;
  mutable level_age : int;
}

let state : t option ref = ref None

let create () =
  (* the default is the size of ONE sub-buffer; the stream has two, and each
     update must fill exactly one *)
  R.set_audio_stream_buffer_size_default chunk;
  let stream = R.load_audio_stream sample_rate 16 2 in
  R.play_audio_stream stream;
  Some
    {
      stream;
      buf = Ctypes.CArray.make Ctypes.int16_t (chunk * 2);
      n1 = make_noise 12345;
      n2 = make_noise 67890;
      w1 = make_noise 24680;
      w2 = make_noise 13579;
      w3 = make_noise 55555;
      w4 = make_noise 77777;
      frame = 0;
      waves = 1.;
      fade = 0.;
      lp_l = (0., 0.);
      lp_r = (0., 0.);
      level = 1.;
      level_age = 0;
    }

let pole cutoff = 1. -. exp (-.two_pi *. cutoff /. float sample_rate)
let step (s1, s2) k x =
  let s1 = s1 +. (k *. (x -. s1)) in
  let s2 = s2 +. (k *. (s1 -. s2)) in
  (s2, (s1, s2))

let fill o ~target ~waves_target ~volume =
  o.waves <-
    (if waves_target > o.waves then Float.min waves_target (o.waves +. 0.1)
     else Float.max waves_target (o.waves -. 0.1));
  let bare = 1. -. o.waves in
  let fade0 = o.fade in
  let fade1 = if target > fade0 then Float.min target (fade0 +. 0.3) else Float.max target (fade0 -. 0.5) in
  o.fade <- fade1;
  let gain = 0.9 *. intensity *. volume in
  for i = 0 to chunk - 1 do
    let t = float (o.frame + i) /. float sample_rate in
    let env = envelope t in
    let surge = 0.6 +. (0.4 *. sin (two_pi *. 0.35 *. speed *. t)) in
    (* on the wind board the gusts are soft and irregular: gentle swells between near-silent lulls *)
    let irr x =
      (* slow incommensurate swells (periods of roughly 5 to 25 s) so gusts rise and fall progressively *)
      let a = sin ((two_pi *. 0.043 *. t) +. x)
      and b = sin ((two_pi *. 0.071 *. t) +. (x *. 2.3))
      and c = sin ((two_pi *. 0.19 *. t) +. (x *. 4.1)) in
      let e = Float.max 0. ((((a *. 0.6) +. (b *. 0.4) +. (c *. 0.2)) /. 1.2 +. 0.15) /. 1.15) in
      e *. e in
    let wind_mod x = o.waves +. (bare *. (0.18 +. (1.6 *. irr x))) in
    let wl = wind_level +. (0.3 *. bare) in
    let l = o.n1 ((t *. 3.1) +. (sin (t *. 0.02) *. 0.5)) *. env *. surge *. 0.5 *. o.waves in
    let r = o.n2 ((t *. 2.9) +. (sin ((t *. 0.017) +. 2.) *. 0.5)) *. env *. surge *. 0.5 *. o.waves in
    let k = pole (500. +. (2500. *. env) +. (700. *. bare)) in
    let l = l +. (wind_sample o.w1 o.w3 t *. wl *. wind_mod 7.) in
    let r = r +. (wind_sample o.w2 o.w4 (t +. 0.37) *. wl *. wind_mod 13.) in
    let l, sl = step o.lp_l k l and r, sr = step o.lp_r k r in
    o.lp_l <- sl;
    o.lp_r <- sr;
    let f = fade0 +. ((fade1 -. fade0) *. float i /. float chunk) in
    let out x = max (-32768) (min 32767 (Float.to_int (x *. gain *. f *. 32767. *. 2.))) in
    Ctypes.CArray.set o.buf (2 * i) (out l);
    Ctypes.CArray.set o.buf ((2 * i) + 1) (out r)
  done;
  o.frame <- o.frame + chunk;
  R.update_audio_stream o.stream (Ctypes.to_voidp (Ctypes.CArray.start o.buf)) chunk

(* Call once per frame. [active] is true while the lobby (beach) or a wind board
   ([wind], wind only) is on screen;
   [volume] reads the persisted sound level (cached, the prefs file is re-read
   about once a second). *)
let update ~active ~wind ~volume =
  (match !state with None when active -> state := create () | _ -> ());
  Option.iter
    (fun o ->
      if R.is_audio_stream_processed o.stream && (active || o.fade > 0.) then (
        o.level_age <- o.level_age + 1;
        if o.level_age > 4 then (
          o.level <- volume ();
          o.level_age <- 0);
        fill o ~target:(if active then 1. else 0.) ~waves_target:(if wind then 0. else 1.) ~volume:o.level))
    !state

let unload () =
  Option.iter (fun o -> R.stop_audio_stream o.stream; R.unload_audio_stream o.stream) !state;
  state := None
