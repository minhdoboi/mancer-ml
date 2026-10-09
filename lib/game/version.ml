(* Which version this build is.

   A packaged game knows: the packager writes the release tag into the assets
   folder (version.txt) and the launcher can pass it in MANCER_VERSION. A build
   from a checkout has no version of its own and answers "dev". *)

let trim = String.trim

let from_env () =
  match Sys.getenv_opt "MANCER_VERSION" with Some v when trim v <> "" -> Some (trim v) | _ -> None

let from_file () =
  let path = Filename.concat (Assets.root ()) "version.txt" in
  try
    let ic = open_in_bin path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let s = trim s in
    if s = "" then None else Some s
  with _ -> None

(* The tag as the packager wrote it, e.g. "v0.9.5". The lobby asks for it on
   every frame, so the answer is remembered. *)
let resolved =
  lazy (match from_env () with Some v -> v | None -> Option.value (from_file ()) ~default:"dev")

let current () = Lazy.force resolved
