(* The media lives in the mancer-assets checkout exactly as the game reads it:
   cards/, backgrounds/, sounds/, i18n/, ui/, campaign/, particle/, combat/ and
   portraits/ are committed under the key the game looks up
   (cards/fire.goblin.jpg, backgrounds/fairyknight.jpg, portraits/ember.png).
   The one build step left is source/portraits/ — the big 1024² originals, cut
   here to the 256² the UI draws, since a face is shown at 84-120px. The run
   fails listing any house card the game finds no art for, and names the houses
   that fall back to the default board. manifest.tsv records what came from
   where. *)

open Mancer
open Sp
module R = Raylib

let out = ref "assets"

(* empty until --src is given; then <out>/source *)
let src = ref ""
let ( / ) = Filename.concat
let portrait_size = 256

(* what the game asks for, see Assets.images *)
let images = [ ".jpg"; ".png" ]

(* ------------------------------------------------------------ files *)

let is_dir p = Sys.file_exists p && Sys.is_directory p
let ls dir = if is_dir dir then Sys.readdir dir |> Array.to_list |> List.sort compare else []

let rec rm_rf p =
  if is_dir p then (
    List.iter (fun f -> rm_rf (p / f)) (ls p);
    Sys.rmdir p)
  else if Sys.file_exists p then Sys.remove p

let rec mkdir_p d =
  if not (Sys.file_exists d) then (
    mkdir_p (Filename.dirname d);
    Sys.mkdir d 0o755)

let ext p = String.lowercase_ascii (Filename.extension p)
let stem p = Filename.remove_extension (Filename.basename p)
let manifest = Buffer.create 4096

let relative p =
  let n = String.length !src in
  if String.length p > n && String.sub p 0 n = !src then String.sub p (n + 1) (String.length p - n - 1) else p

let record dest from = Buffer.add_string manifest (Printf.sprintf "%s\t%s\n" dest (relative from))

let copy from dest =
  mkdir_p (Filename.dirname (!out / dest));
  let data = In_channel.with_open_bin from In_channel.input_all in
  Out_channel.with_open_bin (!out / dest) (fun oc -> Out_channel.output_string oc data)

(* Assets.portrait only looks for .png, so anything else would be a silent miss *)
let cut from dest =
  let e = ext from in
  if e <> ".png" then failwith (from ^ ": a portrait source must be a .png");
  let dest = dest ^ e in
  let img = R.load_image from in
  if not (R.is_image_valid img) then failwith ("cannot read " ^ from);
  let big = R.Image.width img > portrait_size in
  if big then (
    R.image_resize (Ctypes.addr img) portrait_size portrait_size;
    mkdir_p (Filename.dirname (!out / dest));
    if not (R.export_image img (!out / dest)) then failwith ("cannot write " ^ dest))
  else copy from dest;
  R.unload_image img;
  record dest from

(* ------------------------------------------------------------ portraits *)

let sync_portraits () =
  let dir = !src / "portraits" in
  let files = ls dir in
  if files = [] then failwith (dir ^ " holds no pictures (see source/README.md)");
  List.iter (fun f -> cut (dir / f) ("portraits" / stem f)) files

(* ------------------------------------------------------------ coverage *)

(* Assets.card_art asks for the card key first and falls back to the English
   card name, so asking it the same question keeps this check honest about
   what the player will see. *)
let missing_art () =
  let missing = ref [] in
  List.iter
    (fun (h : house) ->
      List.iter
        (fun (c : card) ->
          if Assets.card_art c = None then
            missing := Printf.sprintf "%s (%s)" c.key (Assets.text c.name) :: !missing)
        (h.h_cards @ h.h_additional_cards))
    Catalog.all;
  List.rev !missing

let board_files n =
  List.filter_map
    (fun e ->
      let p = !out / "backgrounds" / (n ^ e) in
      if Sys.file_exists p then Some p else None)
    images

let own_board (h : house) =
  let defaults = board_files "default" in
  let mine = board_files h.h_key @ board_files (String.lowercase_ascii (Assets.text h.h_name)) in
  match defaults with [] -> mine <> [] | _ -> List.exists (fun p -> not (List.mem p defaults)) mine

(* ------------------------------------------------------------ main *)

let () =
  Arg.parse
    [
      ("--out", Arg.Set_string out, "DIR  the mancer-assets checkout (default assets)");
      ("--src", Arg.Set_string src, "DIR  the source media (default <out>/source)");
    ]
    (fun a -> raise (Arg.Bad a))
    "sync_assets [--src DIR] [--out DIR]";
  if !src = "" then src := !out / "source";
  if not (is_dir !out) then failwith (!out ^ " is not a directory");
  Unix.putenv "MANCER_ASSETS" !out;
  (* the art lookup falls back to the English card name, so the coverage check
     has to run in English whatever the player saved as their language *)
  Unix.putenv "MANCER_LANG" "en";
  Catalog.init ();
  R.set_trace_log_level R.TraceLogLevel.Warning;
  List.iter (fun d -> rm_rf (!out / d)) [ "manifest.tsv"; "portraits" ];
  sync_portraits ();
  Out_channel.with_open_text (!out / "manifest.tsv") (fun oc -> Buffer.output_buffer oc manifest);
  let written = List.length (String.split_on_char '\n' (Buffer.contents manifest)) - 1 in
  let fallback =
    List.filter_map (fun (h : house) -> if own_board h then None else Some h.h_key) Catalog.all
  in
  Printf.printf "wrote %s/portraits (%d of %d files)\n" !out written (List.length (ls (!out / "portraits")));
  if fallback <> [] then Printf.printf "default background for: %s\n" (String.concat ", " fallback);
  let missing = missing_art () in
  if missing <> [] then (
    Printf.printf "missing card art (%d):\n" (List.length missing);
    List.iter (Printf.printf "  %s\n") missing;
    exit 1)
