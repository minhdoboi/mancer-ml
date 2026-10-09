(* Dump board snapshots for a recorded game JSON. *)

open Mancer

let () = Printexc.record_backtrace true
let usage = "replay.exe --in GAME.json --out SNAPS.json"
let value = Util.flag_value
let read_file = Util.read_file
let write_file = Util.write_file

let () =
  let argv = Sys.argv in
  let inn = value argv "--in" and out = value argv "--out" in
  match (inn, out) with
  | Some inn, Some out ->
      Catalog.init ();
      let snaps = Replay.run ~cat:(Catalog.default ()) (Game_record.of_json (read_file inn)) in
      write_file out (Replay.snaps_to_json snaps)
  | _ ->
      prerr_endline usage;
      exit 2
