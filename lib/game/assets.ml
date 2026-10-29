(* Card text and art from the mancer-assets checkout (built by bin/sync_assets.ml). *)

open Sp

let root () = Option.value (Sys.getenv_opt "MANCER_ASSETS") ~default:"assets"
let path parts = List.fold_left Filename.concat (root ()) parts
let first_file paths = List.find_opt Sys.file_exists paths
let with_ext base exts = first_file (List.map (fun ext -> base ^ ext) exts)
let load_properties = Properties.load

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

let has_key key = Hashtbl.mem (Lazy.force chosen) key || Hashtbl.mem (Lazy.force english) key

let format tmpl args =
  let args = Array.of_list args in
  let n = String.length tmpl in
  let b = Buffer.create (n + 16) in
  (* [{k}] with [k] a known argument index is replaced; anything else stays literal. *)
  let rec go i =
    if i < n then
      if tmpl.[i] = '{' then
        match String.index_from_opt tmpl i '}' with
        | Some j -> (
            match int_of_string_opt (String.sub tmpl (i + 1) (j - i - 1)) with
            | Some k
              when k >= 0
                   && k < Array.length args
                   && String.for_all (fun c -> c >= '0' && c <= '9') (String.sub tmpl (i + 1) (j - i - 1)) ->
                Buffer.add_string b args.(k);
                go (j + 1)
            | _ ->
                Buffer.add_char b '{';
                go (i + 1))
        | None -> Buffer.add_string b (String.sub tmpl i (n - i))
      else (
        Buffer.add_char b tmpl.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

let card_name (c : card) = text c.name

(* The English name, whatever the language: sound clips are keyed by it (Scala SpellCast.playSound). *)
let english_card_name (c : card) =
  clean (Option.value (Hashtbl.find_opt (Lazy.force english) c.name) ~default:c.name)

(* The basic houses of this player's deck: the classic four in classic mode,
   the standard four in standard mode. A card that reads a basic house's power
   names it in its text, so the text has to follow the mode the game is
   played in. Each house comes with the noun its power takes in this language
   ("anger" in English, "гнева" in Russian); a language that does not need a
   different form leaves the [.mana] key out and the house name is used. *)
let basic_houses (state : game_state) (pid : player_id) =
  let houses = state.players.(pid).ps_desc.init_desc.pd_houses in
  Array.init 4 (fun i ->
      if i < Array.length houses then
        let key = houses.(i).phd_house.h_key in
        let power = if has_key (key ^ ".mana") then text (key ^ ".mana") else text key in
        (text key, power)
      else ("", ""))

(* The names of the four basic houses, as this player's deck names them. *)
let basic_house_names state pid = Array.map fst (basic_houses state pid)

(* The card text marks a basic house with [@0] … [@3] (the house's name) or
   with [#0] … [#3] (the noun its power takes). The house files cannot ask for
   the text themselves — Assets sits above the houses in the build — so the
   front end fills the names in. *)
let fill_basic_houses tmpl houses =
  let n = String.length tmpl in
  let b = Buffer.create (n + 16) in
  let rec go i =
    if i < n then
      if tmpl.[i] = '[' && i + 1 < n && (tmpl.[i + 1] = '@' || tmpl.[i + 1] = '#') then
        let pick k = if tmpl.[i + 1] = '@' then fst houses.(k) else snd houses.(k) in
        match String.index_from_opt tmpl (i + 2) ']' with
        | Some j ->
            let digits = String.sub tmpl (i + 2) (j - i - 2) in
            (match int_of_string_opt digits with
            | Some k when k >= 0 && k < Array.length houses && String.for_all (fun c -> c >= '0' && c <= '9') digits ->
                Buffer.add_string b (pick k);
                go (j + 1)
            | _ ->
                Buffer.add_char b tmpl.[i];
                go (i + 1))
        | None ->
            Buffer.add_char b tmpl.[i];
            go (i + 1)
      else (
        Buffer.add_char b tmpl.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

let describe ?state ?player (c : card) =
  let tmpl = text c.description in
  let tmpl = match (c.desc_args, state, player) with Some f, Some st, Some pid -> format tmpl (f st pid) | _ -> tmpl in
  match (state, player) with
  | Some st, Some pid -> fill_basic_houses tmpl (basic_houses st pid)
  | _ -> tmpl

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

(* The card key first: that is the file the game ships. The English (or
   translated) name is only a fallback for the legacy art saved under it, and
   it must not win over a key, or a card would pick up the picture of another
   card that happens to share its name (both "Goliath"s). *)
let image_names primary translated =
  let display = text translated in
  [ primary; display; String.lowercase_ascii display; file_token display; file_token primary ]
  |> List.map String.trim
  |> List.filter (fun n -> n <> "")
  |> List.fold_left (fun seen n -> if List.mem n seen then seen else seen @ [ n ]) []

let first_image dir names =
  first_file (List.concat_map (fun name -> List.map (fun ext -> path [ dir; name ^ ext ]) images) names)

let card_art (c : card) = first_image "cards" (image_names c.key c.name)

let background (house : house) =
  first_image "backgrounds" (image_names house.h_key house.h_name @ [ "default" ])

(* Scala Board.Background reads "<house>-ground" and "<house>-sky" beside the
   flat picture: the ground painting with a transparent sky, and the sky
   itself, which the board scrolls sideways. *)
let background_layer (house : house) suffix =
  first_image "backgrounds" (List.map (fun name -> name ^ suffix) (image_names house.h_key house.h_name))

let png dir name = first_file [ path [ dir; name ^ ".png" ] ]
let creature_frame () = png "combat" "raka"
let spell_frame () = png "combat" "rakaSpell"
let slot_frame () = png "combat" "slot"
let combat name = with_ext (path [ "combat"; name ]) images
let ui name = png "ui" name
let portrait id = png "portraits" id
let campaign name = first_image "campaign" [ name ]
let particle name = png "particle" name
let sound name = with_ext (path [ "sounds"; name ]) [ ".mp3"; ".ogg"; ".wav" ]

let shader_pair name =
  let vs = path [ "shaders"; name ^ ".vs" ] and fs = path [ "shaders"; name ^ ".fs" ] in
  if Sys.file_exists vs && Sys.file_exists fs then Some (vs, fs) else None
