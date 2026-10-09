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

let lang () = Option.value (Sys.getenv_opt "MANCER_LANG") ~default:"en"
let strings lang = load_properties (path [ "i18n"; lang ^ ".properties" ])
let english = lazy (strings "en")
let chosen = lazy (if lang () = "en" then Lazy.force english else strings (lang ()))
let clean s = String.map (fun c -> if Char.code c < 32 || c = '\127' then ' ' else c) s

let text key =
  let find tbl = Hashtbl.find_opt (Lazy.force tbl) key in
  clean (match find chosen with Some v -> v | None -> Option.value (find english) ~default:key)

let card_name (c : card) = text c.name
let describe (c : card) = text c.description
let images = [ ".jpg"; ".png" ]
let card_art (c : card) = with_ext (path [ "cards"; c.key ]) images

let background (house : house) =
  first_file
    (List.concat_map
       (fun name -> List.map (fun ext -> path [ "backgrounds"; name ^ ext ]) images)
       [ house.h_key; "default" ])

let png dir name = first_file [ path [ dir; name ^ ".png" ] ]
let creature_frame () = png "combat" "raka"
let spell_frame () = png "combat" "rakaSpell"
let slot_frame () = png "combat" "slot"
let ui name = png "ui" name
let portrait id = png "portraits" id
let particle name = png "particle" name
let sound name = with_ext (path [ "sounds"; name ]) [ ".mp3"; ".ogg"; ".wav" ]
