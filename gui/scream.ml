(* A distant, strident scream for Pain of essence, synthesised once (no audio
   file): a harsh glottal tone whose pitch keeps climbing until it cuts off, shaped
   by two bright formants, roughened with a growl, then low-passed and echoed
   so it seems to come from far away. Written to a temp WAV and loaded as a
   raylib Sound. *)

module R = Raylib

let sample_rate = 44100
let duration = 1.2
let two_pi = 2. *. Float.pi
let formant f c bw = exp (-.(((f -. c) /. bw) ** 2.))

let synth () =
  let n = int_of_float (duration *. float sample_rate) in
  let dry = Array.make n 0. in
  let phase = ref 0. in
  let s = ref 987654321 in
  let rnd () =
    s := ((!s * 1664525) + 1013904223) land 0xFFFFFFFF;
    (float !s /. 4294967296.) -. 0.5
  in
  for i = 0 to n - 1 do
    let t = float i /. float sample_rate in
    (* pitch only climbs, with tiny irregular jitter instead of vibrato (which sounds like singing) *)
    let rise = t /. duration in
    let f0 = (560. +. (1500. *. (rise ** 1.5))) *. (1. +. (0.012 *. rnd ())) in
    phase := !phase +. (two_pi *. f0 /. float sample_rate);
    let v = ref 0. in
    for k = 1 to 16 do
      let fk = f0 *. float k in
      if fk < 9000. then
        let g = (0.15 +. formant fk 950. 450. +. (0.8 *. formant fk 2900. 700.) +. (0.4 *. formant fk 4200. 900.)) /. float k ** 0.7 in
        v := !v +. (g *. sin (float k *. !phase))
    done;
    (* growl: fast amplitude roughness, plus a little breath *)
    let rough = 1. +. (0.35 *. sin (two_pi *. 72. *. t)) in
    let attack = Float.min 1. (t /. 0.05) in
    (* it swells as it climbs, then cuts out into silence *)
    let swell = 0.55 +. (0.45 *. rise) in
    let cut = Float.min 1. ((duration -. t) /. 0.12) in
    dry.(i) <- ((!v *. rough) +. (rnd () *. 0.6)) *. attack *. swell *. cut
  done;
  (* distance: darken with a one-pole low-pass, then add washed-out echoes *)
  let lp = ref 0. in
  let k = 1. -. exp (-.two_pi *. 2200. /. float sample_rate) in
  let dark = Array.map (fun x -> lp := !lp +. (k *. (x -. !lp)); !lp) dry in
  let out = Array.copy dark in
  List.iter
    (fun (delay, gain) ->
      let d = int_of_float (delay *. float sample_rate) in
      for i = d to n - 1 do
        out.(i) <- out.(i) +. (dark.(i - d) *. gain)
      done)
    [ (0.12, 0.18); (0.26, 0.08) ];
  let peak = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 1e-6 out in
  Array.map (fun x -> x /. peak *. 0.7) out

let write_wav path samples =
  let n = Array.length samples in
  let oc = open_out_bin path in
  let le32 v = for b = 0 to 3 do output_byte oc ((v lsr (8 * b)) land 255) done in
  let le16 v = for b = 0 to 1 do output_byte oc ((v lsr (8 * b)) land 255) done in
  output_string oc "RIFF";
  le32 (36 + (n * 2));
  output_string oc "WAVEfmt ";
  le32 16;
  le16 1;
  le16 1;
  le32 sample_rate;
  le32 (sample_rate * 2);
  le16 2;
  le16 16;
  output_string oc "data";
  le32 (n * 2);
  Array.iter (fun x -> le16 (int_of_float (x *. 32767.) land 0xFFFF)) samples;
  close_out oc

let cached : R.Sound.t option option ref = ref None

let sound () =
  match !cached with
  | Some s -> s
  | None ->
      let s =
        try
          let path = Filename.temp_file "mancer_scream" ".wav" in
          write_wav path (synth ());
          let snd = R.load_sound path in
          (try Sys.remove path with _ -> ());
          if R.is_sound_valid snd then Some snd else None
        with _ -> None
      in
      cached := Some s;
      s

let unload () =
  Option.iter (Option.iter R.unload_sound) !cached;
  cached := None
