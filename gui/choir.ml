(* Distant wordless choir for the Limbo board. Voices are baked into wavetables
   once, then the stream only mixes and filters — cheap enough for the GUI
   thread. Same ambient toggle as the ocean and the earthquake rumble. *)

module R = Raylib

let sample_rate = 44100
let chunk = 8192
let intensity = 0.42
let two_pi = 2. *. Float.pi
let delay_len = 16384
let table_len = 2048
let table_f = float table_len
let formant f c bw = exp (-.(((f -. c) /. bw) ** 2.))

type voice = {
  f0 : float;
  pan_l : float;
  pan_r : float;
  vib_hz : float;
  vib_depth : float;
  swell_hz : float;
  swell_ph : float;
  gain : float;
  guest : bool;
  wave : float array;
  mutable phase : float;
  mutable sw0 : float;
  mutable sw1 : float;
  mutable step0 : float;
  mutable step1 : float;
}

type t = {
  stream : R.AudioStream.t;
  buf : int Ctypes.CArray.t;
  voices : voice array;
  delay : float array;
  mutable delay_pos : int;
  mutable frame : int;
  mutable fade : float;
  mutable lp_l : float * float;
  mutable lp_r : float * float;
  mutable level : float;
  mutable level_age : int;
}

let state : t option ref = ref None

let make_wave f0 =
  let tbl = Array.make table_len 0. in
  let peak = ref 1e-6 in
  for i = 0 to table_len - 1 do
    let ph = two_pi *. float i /. table_f in
    let acc = ref 0. in
    for k = 1 to 8 do
      let fk = f0 *. float k in
      if fk < 5500. then
        let g =
          (0.18
          +. formant fk 700. 140.
          +. (0.7 *. formant fk 1150. 180.)
          +. (0.28 *. formant fk 2450. 260.))
          /. float k ** 0.62
        in
        acc := !acc +. (g *. sin (float k *. ph))
    done;
    tbl.(i) <- !acc;
    if Float.abs !acc > !peak then peak := Float.abs !acc
  done;
  let n = 0.7 /. !peak in
  Array.map (fun x -> x *. n) tbl

let voice ~f0 ~pan ~vib_hz ~vib_depth ~swell_hz ~swell_ph ~gain ~guest ~phase =
  let pan_l = (1. -. pan) *. 0.5 and pan_r = (1. +. pan) *. 0.5 in
  {
    f0;
    pan_l;
    pan_r;
    vib_hz;
    vib_depth;
    swell_hz;
    swell_ph;
    gain;
    guest;
    wave = make_wave f0;
    phase = phase *. table_f /. two_pi;
    sw0 = 1.;
    sw1 = 1.;
    step0 = 0.;
    step1 = 0.;
  }

(* D-minor chapel, plus a G that joins as a second held note now and then. *)
let make_voices () =
  [|
    voice ~f0:73.4 ~pan:(-0.15) ~vib_hz:4.6 ~vib_depth:0.003 ~swell_hz:0.041 ~swell_ph:0.4 ~gain:0.55 ~guest:false ~phase:0.2;
    voice ~f0:146.8 ~pan:(-0.45) ~vib_hz:5.1 ~vib_depth:0.004 ~swell_hz:0.053 ~swell_ph:1.7 ~gain:0.85 ~guest:false ~phase:1.1;
    voice ~f0:174.6 ~pan:0.35 ~vib_hz:5.4 ~vib_depth:0.0045 ~swell_hz:0.037 ~swell_ph:3.9 ~gain:0.7 ~guest:false ~phase:2.4;
    voice ~f0:220.0 ~pan:(-0.25) ~vib_hz:5.7 ~vib_depth:0.005 ~swell_hz:0.061 ~swell_ph:5.2 ~gain:0.9 ~guest:false ~phase:0.6;
    voice ~f0:293.7 ~pan:0.2 ~vib_hz:5.9 ~vib_depth:0.0055 ~swell_hz:0.047 ~swell_ph:2.1 ~gain:1.0 ~guest:false ~phase:3.3;
    voice ~f0:349.2 ~pan:0.55 ~vib_hz:6.2 ~vib_depth:0.006 ~swell_hz:0.033 ~swell_ph:4.8 ~gain:0.8 ~guest:false ~phase:4.7;
    voice ~f0:440.0 ~pan:(-0.05) ~vib_hz:6.4 ~vib_depth:0.0065 ~swell_hz:0.055 ~swell_ph:0.9 ~gain:0.65 ~guest:false ~phase:5.5;
    voice ~f0:587.3 ~pan:0.4 ~vib_hz:6.8 ~vib_depth:0.007 ~swell_hz:0.029 ~swell_ph:6.1 ~gain:0.35 ~guest:false ~phase:1.8;
    voice ~f0:98.0 ~pan:0.15 ~vib_hz:4.8 ~vib_depth:0.0035 ~swell_hz:0.044 ~swell_ph:2.8 ~gain:0.5 ~guest:true ~phase:0.9;
    voice ~f0:196.0 ~pan:(-0.35) ~vib_hz:5.5 ~vib_depth:0.005 ~swell_hz:0.039 ~swell_ph:5.7 ~gain:0.95 ~guest:true ~phase:3.6;
    voice ~f0:392.0 ~pan:0.5 ~vib_hz:6.1 ~vib_depth:0.006 ~swell_hz:0.051 ~swell_ph:1.3 ~gain:0.85 ~guest:true ~phase:4.2;
  |]

let create () =
  R.set_audio_stream_buffer_size_default chunk;
  let stream = R.load_audio_stream sample_rate 16 2 in
  R.play_audio_stream stream;
  Some
    {
      stream;
      buf = Ctypes.CArray.make Ctypes.int16_t (chunk * 2);
      voices = make_voices ();
      delay = Array.make (delay_len * 2) 0.;
      delay_pos = 0;
      frame = 0;
      fade = 0.;
      lp_l = (0., 0.);
      lp_r = (0., 0.);
      level = 1.;
      level_age = 0;
    }

let pole cutoff = 1. -. exp (-.two_pi *. cutoff /. float sample_rate)
let lp_k = pole 1650.

let step (s1, s2) k x =
  let s1 = s1 +. (k *. (x -. s1)) in
  let s2 = s2 +. (k *. (s1 -. s2)) in
  (s2, (s1, s2))

let tap o ch back =
  let i = (o.delay_pos - back + delay_len) land (delay_len - 1) in
  o.delay.((i * 2) + ch)

(* Usually a distant hush; full volume is the rarer swell. Never silent. *)
let presence t =
  let a = sin ((two_pi *. 0.011 *. t) +. 0.8)
  and b = sin ((two_pi *. 0.019 *. t) +. 2.9)
  and c = sin ((two_pi *. 0.037 *. t) +. 4.6)
  and d = sin ((two_pi *. 0.007 *. t) +. 1.5) in
  let e = ((((a *. 0.4) +. (b *. 0.3) +. (c *. 0.2) +. (d *. 0.25)) /. 1.15) +. 1.) /. 2. in
  let rise = e ** 2.2 in
  let floor = 0.22 +. (0.08 *. (0.5 +. (0.5 *. sin ((two_pi *. 0.0053 *. t) +. 3.1)))) in
  floor +. ((1. -. floor) *. rise)

(* A G joins the D-minor hold when this rises; most of the time it stays out. *)
let second_note t =
  let a = sin ((two_pi *. 0.0083 *. t) +. 1.4)
  and b = sin ((two_pi *. 0.015 *. t) +. 4.2)
  and c = sin ((two_pi *. 0.027 *. t) +. 2.6) in
  let e = ((((a *. 0.45) +. (b *. 0.35) +. (c *. 0.25)) /. 1.05) +. 1.) /. 2. in
  e ** 2.8

let swell v t = 0.28 +. (0.72 *. (0.5 +. (0.5 *. sin ((two_pi *. v.swell_hz *. t) +. v.swell_ph))))

let phase_step v t =
  let f0 = v.f0 *. (1. +. (v.vib_depth *. sin (two_pi *. v.vib_hz *. t))) in
  f0 *. table_f /. float sample_rate

let lookup wave phase =
  let i = int_of_float phase in
  let f = phase -. float i in
  let a = wave.(i) in
  let b = wave.(if i + 1 < table_len then i + 1 else 0) in
  a +. ((b -. a) *. f)

let fill o ~target ~volume =
  let fade0 = o.fade in
  let fade1 = if target > fade0 then Float.min target (fade0 +. 0.22) else Float.max target (fade0 -. 0.35) in
  o.fade <- fade1;
  let gain = intensity *. volume in
  let t0 = float o.frame /. float sample_rate in
  let t1 = float (o.frame + chunk) /. float sample_rate in
  let p0 = presence t0 and p1 = presence t1 in
  let g0 = second_note t0 and g1 = second_note t1 in
  Array.iter
    (fun v ->
      v.sw0 <- swell v t0;
      v.sw1 <- swell v t1;
      v.step0 <- phase_step v t0;
      v.step1 <- phase_step v t1)
    o.voices;
  let inv = 1. /. float chunk in
  let n = Array.length o.voices in
  for i = 0 to chunk - 1 do
    let u = float i *. inv in
    let p = p0 +. ((p1 -. p0) *. u) in
    let guest = g0 +. ((g1 -. g0) *. u) in
    let dry_l = ref 0. and dry_r = ref 0. in
    for vi = 0 to n - 1 do
      let v = o.voices.(vi) in
      let step = v.step0 +. ((v.step1 -. v.step0) *. u) in
      let phase = v.phase +. step in
      let phase = if phase >= table_f then phase -. table_f else phase in
      v.phase <- phase;
      let s = lookup v.wave phase *. (v.sw0 +. ((v.sw1 -. v.sw0) *. u)) *. v.gain in
      let s = if v.guest then s *. guest else s in
      dry_l := !dry_l +. (s *. v.pan_l);
      dry_r := !dry_r +. (s *. v.pan_r)
    done;
    let dry_l = !dry_l *. p and dry_r = !dry_r *. p in
    let l =
      dry_l +. (tap o 0 3969 *. 0.42) +. (tap o 1 7497 *. 0.18) +. (tap o 0 11051 *. 0.16)
    in
    let r =
      dry_r +. (tap o 1 4410 *. 0.42) +. (tap o 0 8379 *. 0.18) +. (tap o 1 12007 *. 0.16)
    in
    o.delay.((o.delay_pos * 2) + 0) <- dry_l +. (l *. 0.18);
    o.delay.((o.delay_pos * 2) + 1) <- dry_r +. (r *. 0.18);
    o.delay_pos <- (o.delay_pos + 1) land (delay_len - 1);
    let l, sl = step o.lp_l lp_k l and r, sr = step o.lp_r lp_k r in
    o.lp_l <- sl;
    o.lp_r <- sr;
    let f = fade0 +. ((fade1 -. fade0) *. u) in
    let out x =
      let y = x *. 0.22 *. gain *. f *. 32767. in
      max (-32768) (min 32767 (Float.to_int y))
    in
    Ctypes.CArray.set o.buf (2 * i) (out l);
    Ctypes.CArray.set o.buf ((2 * i) + 1) (out r)
  done;
  o.frame <- o.frame + chunk;
  R.update_audio_stream o.stream (Ctypes.to_voidp (Ctypes.CArray.start o.buf)) chunk

let update ~active ~volume =
  (match !state with None when active -> state := create () | _ -> ());
  Option.iter
    (fun o ->
      if R.is_audio_stream_processed o.stream && (active || o.fade > 0.) then (
        o.level_age <- o.level_age + 1;
        if o.level_age > 4 then (
          o.level <- volume ();
          o.level_age <- 0);
        fill o ~target:(if active then 1. else 0.) ~volume:o.level;
        if (not active) && o.fade <= 0. then (
          R.stop_audio_stream o.stream;
          R.unload_audio_stream o.stream;
          state := None)))
    !state

let unload () =
  Option.iter
    (fun o ->
      R.stop_audio_stream o.stream;
      R.unload_audio_stream o.stream)
    !state;
  state := None
