(* Play the same recorded game in OCaml and Scala; report the first differing turn. *)

open Mancer

let () = Printexc.record_backtrace true

let usage =
  "compare_engines.exe [--game FILE] [--seed N] [--special NAME] [--turns N] [--scala DIR] [--keep DIR]"

let rec find_up dir names n =
  if n = 0 then None
  else if List.exists (fun f -> Sys.file_exists (Filename.concat dir f)) names then Some dir
  else find_up (Filename.dirname dir) names (n - 1)

let mancer_root () =
  match find_up (Sys.getcwd ()) [ "dune-project" ] 8 with Some d -> d | None -> Sys.getcwd ()

let default_scala () =
  match Sys.getenv_opt "FREESPECTROGDX" with
  | Some p -> p
  | None -> Filename.concat (Filename.dirname (mancer_root ())) "freespectrogdx"

let value argv key =
  let rec go = function a :: v :: _ when a = key -> Some v | _ :: xs -> go xs | [] -> None in
  go (Array.to_list argv)

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let mkdir_p dir = if not (Sys.file_exists dir) then Unix.mkdir dir 0o755

let run_scala ~scala_dir ~in_path ~out_path =
  let gradlew = Filename.concat scala_dir "gradlew" in
  if not (Sys.file_exists gradlew) then failwith ("Scala repo not found (no gradlew) at " ^ scala_dir);
  let targs = Printf.sprintf "--in %s --out %s" in_path out_path in
  let cmd =
    Printf.sprintf "cd %s && ./gradlew --quiet :core:replay -Ptargs=%s" (Filename.quote scala_dir)
      (Filename.quote targs)
  in
  let code = Sys.command cmd in
  if code <> 0 then failwith (Printf.sprintf "Scala replay failed (exit %d): %s" code cmd)

let () =
  let argv = Sys.argv in
  let seed = Option.fold ~none:1 ~some:int_of_string (value argv "--seed") in
  let special = Option.value (value argv "--special") ~default:"Troopers" in
  let turns = Option.fold ~none:80 ~some:int_of_string (value argv "--turns") in
  let scala_dir = Option.value (value argv "--scala") ~default:(default_scala ()) in
  let keep = value argv "--keep" in
  Catalog.init ();
  let json, parsed =
    match value argv "--game" with
    | Some path ->
        let text = read_file path in
        (text, Game_record.of_json text)
    | None ->
        let rec_ = Replay.generate ~special ~max_turns:turns ~seed () in
        let text = Game_record.to_json rec_ in
        (text, Game_record.of_json text)
  in
  let dir =
    match keep with
    | Some d ->
        mkdir_p d;
        d
    | None ->
        let d =
          Filename.concat (Filename.get_temp_dir_name ())
            (Printf.sprintf "mancer-compare-%d" (Random.bits ()))
        in
        mkdir_p d;
        d
  in
  let game_path = Filename.concat dir "game.json" in
  let ml_path = Filename.concat dir "ocaml.json" in
  let sc_path = Filename.concat dir "scala.json" in
  write_file game_path json;
  let ml = Replay.run ~cat:(Catalog.default ()) parsed in
  write_file ml_path (Replay.snaps_to_json ml);
  Printf.printf "ocaml %d snapshots -> %s\n%!" (List.length ml) ml_path;
  run_scala ~scala_dir ~in_path:game_path ~out_path:sc_path;
  let sc = Replay.snaps_of_json (read_file sc_path) in
  Printf.printf "scala %d snapshots -> %s\n%!" (List.length sc) sc_path;
  match Replay.diff ml sc with
  | None ->
      Printf.printf "ok: boards match for %d turns (special %s seed %d)\n%!"
        (max 0 (List.length ml - 1))
        special
        (match value argv "--game" with Some _ -> parsed.rng_seed | None -> seed)
  | Some m ->
      Printf.printf "mismatch at t=%d: %s\n%!" m.t m.message;
      exit 1
