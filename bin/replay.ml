(* Dump board snapshots for a recorded game JSON. *)

open Mancer

let () = Printexc.record_backtrace true
let usage = "replay.exe --in GAME.json --out SNAPS.json"

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
