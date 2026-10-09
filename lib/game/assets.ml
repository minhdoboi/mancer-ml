(* Card text and art from the mancer-assets checkout (built by bin/sync_assets.ml). *)

open Sp

let root () = Option.value (Sys.getenv_opt "MANCER_ASSETS") ~default:"assets"
let path parts = List.fold_left Filename.concat (root ()) parts
let first_file paths = List.find_opt Sys.file_exists paths
let with_ext base exts = first_file (List.map (fun ext -> base ^ ext) exts)

(* Java properties: key=value, \uXXXX escapes *)
let unescape v =
  let b = Buffer.create (String.length v) in
  let n = String.length v in
  let rec go i =
    if i < n then
      if v.[i] = '\\' && i + 1 < n then (
        match v.[i + 1] with
        | 'u' when i + 5 < n -> (
            match int_of_string_opt ("0x" ^ String.sub v (i + 2) 4) with
            | Some cp when Uchar.is_valid cp ->
                Buffer.add_utf_8_uchar b (Uchar.of_int cp);
                go (i + 6)
            | _ -> go (i + 2))
        | 'n' | 't' ->
            Buffer.add_char b ' ';
            go (i + 2)
        | c ->
            Buffer.add_char b c;
            go (i + 2))
      else (
        Buffer.add_char b v.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

let load_properties file =
  let tbl = Hashtbl.create 1024 in
  (try
     In_channel.with_open_text file (fun ic ->
         In_channel.input_lines ic
         |> List.iter (fun line ->
             match String.index_opt line '=' with
             | Some i when line.[0] <> '#' ->
                 Hashtbl.replace tbl
                   (String.trim (String.sub line 0 i))
                   (unescape (String.sub line (i + 1) (String.length line - i - 1)))
             | _ -> ()))
   with Sys_error _ -> ());
  tbl

let lang () =
  match Sys.getenv_opt "MANCER_LANG" with
  | Some l when String.trim l <> "" -> String.trim l
  | _ -> ( match Prefs.saved_language () with Some l -> l | None -> "en")

let strings lang = load_properties (path [ "i18n"; lang ^ ".properties" ])
let english = lazy (strings "en")
let chosen = lazy (if lang () = "en" then Lazy.force english else strings (lang ()))
let clean s = String.map (fun c -> if Char.code c < 32 || c = '\127' then ' ' else c) s

let text key =
  let find tbl = Hashtbl.find_opt (Lazy.force tbl) key in
  clean (match find chosen with Some v -> v | None -> Option.value (find english) ~default:key)

let has_key key =
  Hashtbl.mem (Lazy.force chosen) key || Hashtbl.mem (Lazy.force english) key

let format tmpl args =
  let n = List.length args in
  let rec go i s =
    if i >= n then s
    else
      let pat = "{" ^ string_of_int i ^ "}" in
      let arg = List.nth args i in
      let rec replace s =
        match String.index_opt s '{' with
        | None -> s
        | Some j ->
            if j + String.length pat <= String.length s && String.sub s j (String.length pat) = pat then
              String.sub s 0 j ^ arg ^ replace (String.sub s (j + String.length pat) (String.length s - j - String.length pat))
            else String.sub s 0 (j + 1) ^ replace (String.sub s (j + 1) (String.length s - j - 1))
      in
      go (i + 1) (replace s)
  in
  go 0 tmpl

let card_name (c : card) = text c.name
let describe (c : card) = text c.description
let images = [ ".jpg"; ".png" ]

(* Keys are alphanumerics; vibe files keep spaces ("dark priest.jpg"). *)
let file_token s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' -> Buffer.add_char b (Char.lowercase_ascii c)
      | 'a' .. 'z' | '0' .. '9' -> Buffer.add_char b c
      | _ -> ())
    s;
  Buffer.contents b

let image_names primary translated =
  let display = text translated in
  [ primary; display; String.lowercase_ascii display; file_token display; file_token primary ]
  |> List.map String.trim
  |> List.filter (fun n -> n <> "")
  |> List.sort_uniq String.compare

let first_image dir names =
  first_file (List.concat_map (fun name -> List.map (fun ext -> path [ dir; name ^ ext ]) images) names)

let card_art (c : card) = first_image "cards" (image_names c.key c.name)

let background (house : house) =
  first_image "backgrounds" (image_names house.h_key house.h_name @ [ "default" ])

let png dir name = first_file [ path [ dir; name ^ ".png" ] ]
let creature_frame () = png "combat" "raka"
let spell_frame () = png "combat" "rakaSpell"
let slot_frame () = png "combat" "slot"
let ui name = png "ui" name
let portrait id = png "portraits" id
let campaign name = first_image "campaign" [ name ]
let particle name = png "particle" name
let sound name = with_ext (path [ "sounds"; name ]) [ ".mp3"; ".ogg"; ".wav" ]

let shader_pair name =
  let vs = path [ "shaders"; name ^ ".vs" ] and fs = path [ "shaders"; name ^ ".fs" ] in
  if Sys.file_exists vs && Sys.file_exists fs then Some (vs, fs) else None
