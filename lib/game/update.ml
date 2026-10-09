(* Is there a newer release on GitHub?

   The game has no HTTP client and no TLS, so the check shells out to curl:
   it is on every desktop we ship to. The call runs in a background domain, so
   the window keeps drawing while it waits. *)

let repo = "minhdoboi/mancer-ml"
let api_url = Printf.sprintf "https://api.github.com/repos/%s/releases/latest" repo
let page_url = Printf.sprintf "https://github.com/%s/releases/latest" repo

type outcome =
  | Up_to_date of string  (* the latest release *)
  | Newer of string       (* a newer release *)
  | Unknown of string     (* the latest release, but our own version is not a number *)
  | Failed of string      (* why the check did not work *)

(* "v0.9.5", "0.9.5-3-gabc" and "1.2" all begin with a dotted number; anything
   else ("dev", a commit hash) has no version to compare. A hash like "4ee54d2"
   starts with a digit, so a version needs at least two parts. *)
let parse_version (s : string) =
  let s = String.trim s in
  let s =
    if String.length s > 0 && (s.[0] = 'v' || s.[0] = 'V') then String.sub s 1 (String.length s - 1) else s
  in
  let num = Buffer.create 8 in
  let stop = ref false in
  String.iter
    (fun c ->
      if !stop then ()
      else if (c >= '0' && c <= '9') || c = '.' then Buffer.add_char num c
      else stop := true)
    s;
  let parts = String.split_on_char '.' (Buffer.contents num) |> List.filter (fun p -> p <> "") in
  let ints = List.map (fun p -> try Some (int_of_string p) with _ -> None) parts in
  if List.length parts < 2 || List.exists Option.is_none ints then None
  else Some (List.map Option.get ints)

(* Missing parts count as zero, so 0.9 equals 0.9.0. *)
let compare_versions a b =
  let rec go a b =
    match (a, b) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys -> if x <> y then compare x y else go xs ys
  in
  go a b

let read_all fd =
  let b = Buffer.create 4096 in
  let chunk = Bytes.create 4096 in
  let rec go () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> Buffer.contents b
    | n ->
        Buffer.add_subbytes b chunk 0 n;
        go ()
  in
  go ()

(* curl writes to a pipe, its errors to the null device: no second pipe to
   drain, so a chatty error can never block the child. *)
let curl url =
  let argv = [| "curl"; "-fsSL"; "--max-time"; "15"; "-H"; "Accept: application/vnd.github+json"; url |] in
  try
    let null = Unix.openfile Filename.null [ Unix.O_WRONLY ] 0o666 in
    let rd, wr = Unix.pipe () in
    let pid = Unix.create_process "curl" argv null wr null in
    Unix.close wr;
    Unix.close null;
    let out = read_all rd in
    Unix.close rd;
    let _, status = Unix.waitpid [] pid in
    (match status with
    | Unix.WEXITED 0 -> Ok out
    | Unix.WEXITED n -> Error (Printf.sprintf "curl exited with %d" n)
    | Unix.WSIGNALED n -> Error (Printf.sprintf "curl was killed by signal %d" n)
    | Unix.WSTOPPED n -> Error (Printf.sprintf "curl stopped by signal %d" n))
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Error "curl is not installed"
  | Unix.Unix_error (e, fn, _) -> Error (Printf.sprintf "%s: %s" fn (Unix.error_message e))

let latest_release () =
  match curl api_url with
  | Error e -> Error e
  | Ok body -> (
      try
        let open Yojson.Basic.Util in
        let j = Yojson.Basic.from_string body in
        let tag = j |> member "tag_name" |> to_string in
        let url = j |> member "html_url" |> to_string in
        Ok (tag, url)
      with
      | Yojson.Json_error e -> Error e
      | Yojson.Basic.Util.Type_error (msg, _) -> Error msg)

let check () =
  match latest_release () with
  | Error e -> Failed e
  | Ok (tag, _) -> (
      match (parse_version tag, parse_version (Version.current ())) with
      | Some latest, Some mine -> if compare_versions latest mine > 0 then Newer tag else Up_to_date tag
      | _ -> Unknown tag)

(* The check takes a moment, so it runs in the background and the lobby asks
   for the result once per frame. *)
type job = { mutable outcome : outcome option; mutable domain : unit Domain.t option }

let start () =
  let j = { outcome = None; domain = None } in
  j.domain <- Some (Domain.spawn (fun () -> j.outcome <- Some (check ())));
  j

let poll (j : job) =
  match j.outcome with
  | Some o ->
      (match j.domain with Some d -> ignore (Domain.join d) | None -> ());
      j.domain <- None;
      Some o
  | None -> None
