(* A distant earthquake under the elementalist board: a looping low rumble that
   never goes silent. It follows the same ambient setting as the ocean and wind. *)

open Mancer
module R = Raylib

let level = 0.6 (* the clip is already deep; keep it well under the effects *)
let fade_time = 1.5 (* seconds to ease in and out *)

type t = { music : R.Music.t; mutable fade : float }

let state : t option ref = ref None

let create () =
  match Assets.sound "earthquake-distant" with
  | None -> None
  | Some path ->
      let music = R.load_music_stream path in
      if R.is_music_valid music then (
        R.set_music_volume music 0.;
        R.play_music_stream music;
        Some { music; fade = 0. })
      else None

let update ~active ~volume =
  (match !state with None when active -> state := create () | _ -> ());
  Option.iter
    (fun r ->
      let dt = Float.min 0.1 (R.get_frame_time ()) in
      let target = if active then 1. else 0. in
      let step = dt /. fade_time in
      r.fade <- (if r.fade < target then Float.min target (r.fade +. step) else Float.max target (r.fade -. step));
      if r.fade <= 0. && not active then (
        R.stop_music_stream r.music;
        R.unload_music_stream r.music;
        state := None)
      else (
        R.update_music_stream r.music;
        R.set_music_volume r.music (r.fade *. level *. volume ())))
    !state

let unload () =
  Option.iter (fun r -> R.stop_music_stream r.music; R.unload_music_stream r.music) !state;
  state := None
