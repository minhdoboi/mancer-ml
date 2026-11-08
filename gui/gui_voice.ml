(* Campaign voice-over: a cache of raylib sounds keyed by the audio path, and
   the replay button the parchment dialogs draw for a line that has one.

   The clips are written by scripts/gen_vi_tts.py to
   [assets/audio/<lang>/<i18n key>.mp3] (a .wav or .ogg of the same name still
   loads), so a dialog only offers the button when the file exists for the
   language the game is running in. *)

module R = Raylib
open Mancer

let cache : (string, R.Sound.t option) Hashtbl.t = Hashtbl.create 16
let current : R.Sound.t option ref = ref None

let volume () = try Prefs.load () |> Prefs.sound_level with _ -> 1.

(* Silence the line already playing, so a new one does not layer on top. *)
let stop () =
  (match !current with Some s -> (try R.stop_sound s with _ -> ()) | None -> ());
  current := None

(* [key] is an i18n key; {!Assets.voice} maps it to the clip, if generated. *)
let path key = Assets.voice key

let available key = Option.is_some (path key)

let load p =
  match Hashtbl.find_opt cache p with
  | Some s -> s
  | None ->
      let s =
        try
          let snd = R.load_sound p in
          if R.is_sound_valid snd then Some snd
          else (
            (try R.unload_sound snd with _ -> ());
            None)
        with _ -> None
      in
      Hashtbl.replace cache p s;
      s

let play_key key =
  match path key with
  | None -> ()
  | Some p -> (
      stop ();
      match load p with
      | Some s ->
          R.set_sound_volume s (0.95 *. volume ());
          R.play_sound s;
          current := Some s
      | None -> ())

(* Auto-play: a parchment dialog calls {!autoplay} with the line it shows, so
   the clip starts by itself once, the first frame the line appears. The key is
   remembered so the per-frame redraw does not restart it; {!end_frame} ends a
   frame that showed no line, so reopening the same dialog plays it again. *)

let auto_last : string option ref = ref None
let auto_called = ref false

let autoplay key =
  auto_called := true;
  if !auto_last <> Some key then (
    auto_last := Some key;
    if volume () > 0. then play_key key)

let end_frame () =
  if not !auto_called then auto_last := None;
  auto_called := false

let unload () =
  stop ();
  Hashtbl.iter (fun _ s -> Option.iter R.unload_sound s) cache;
  Hashtbl.reset cache
