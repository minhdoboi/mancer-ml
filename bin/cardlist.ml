(* docs/cards.json + docs/cards.html: the card reference the GitHub Pages site serves.

   The site is uploaded as an artifact by .github/workflows/pages.yml, so
   nothing outside docs/ is published; that workflow checks out the assets
   submodule and copies assets/cards to cards/ next to these files. Regenerate
   them after touching a house (lib/houses/) or its text
   (assets/i18n/en.properties) and commit them — the workflow stays a plain
   file copy and needs no OCaml toolchain.

   The data lives in cards.json, one entry per card, and the page reads it and
   draws the list: one file to serve, one place to change, and a reader can
   search every house and card at once instead of page by page. Names,
   descriptions and costs come from the game itself (Catalog, Assets), so the
   list cannot drift from what the board shows.

   The list is published in every language the game ships: the English one is
   cards.json, the others cards.<lang>.json, and the page picks one from a
   language menu. The text of a list comes from the i18n bundle of that
   language (assets/i18n/<lang>.properties), the art path from the English
   one, because the art files are named after the card key.

   The page itself is a template, not generated code: templates/cards.html
   with templates/cards.css and templates/cards.js next to it. The tool
   writes them out with the {{...}} markers filled in, so the page can
   be edited as a page and the program only carries the data. *)

open Mancer
open Sp
open Yojson.Safe

let out = ref "docs"
let template = ref "templates"

(* The four classic basics (the elements of the original game, which this
   project is a tribute to) and Troopers stay out of the published list. The
   standard-mode basics — Anger, Sadness, Fear and Joy — are in it, on the
   page and in the game alike. *)
let hidden_houses = [ "air"; "fire"; "water"; "earth"; "troopers"; "Troopers" ]

(* ------------------------------------------------------------ languages *)

(* The name of a language in that language, for the menu on the page. *)
let language_names =
  [ ("en", "English"); ("fr", "Français"); ("es", "Español"); ("ru", "Русский"); ("vi", "Tiếng Việt") ]

let language_name code = match List.assoc_opt code language_names with Some n -> n | None -> code

(* One language's strings: its own bundle, and the English one it falls back
   to, the way the game resolves a key (Assets.text). *)
type bundle = { lang : string; own : (string, string) Hashtbl.t; en : (string, string) Hashtbl.t }

let i18n_file lang = Filename.concat (Assets.root ()) (Filename.concat "i18n" (lang ^ ".properties"))

let bundle lang =
  { lang; own = Assets.load_properties (i18n_file lang); en = Assets.load_properties (i18n_file "en") }

(* the game's key resolution, for a bundle of our own: this language, then
   English, then the key itself *)
let text b key =
  let find tbl = Hashtbl.find_opt tbl key in
  Assets.clean (match find b.own with Some v -> v | None -> Option.value (find b.en) ~default:key)

let has_key b key = Hashtbl.mem b.own key || Hashtbl.mem b.en key

(* the file the page reads for a language: the English list keeps the plain
   name, the others sit next to it *)
let json_file lang = if lang = "en" then "cards.json" else "cards." ^ lang ^ ".json"

(* the menu of the page: the languages, each in its own name, with the file
   the page reads for it *)
let langs_json langs =
  `List
    (List.map
       (fun code ->
         `Assoc
           [
             ("code", `String code); ("name", `String (language_name code)); ("file", `String (json_file code));
           ])
       langs)

(* ------------------------------------------------------------ helpers *)

(* a house's playable cards, plus the tokens it summons *)
let cards_of (h : house) = h.h_cards @ h.h_additional_cards

(* A card description may end in a value read off the live board (Apocalypse:
   "8 + fire mana [{0}]"). A static list has no board, so the value goes rather
   than printing a raw placeholder. *)
let drop_board_args s some =
  let n = String.length s in
  (* "[{12}]" from [i]: the end of the group, or None *)
  let slot_end i =
    if i + 2 >= n then None
    else
      match String.index_from_opt s (i + 2) ']' with
      | Some j when i + 4 < n && s.[i] = '[' && s.[i + 1] = '{' && s.[j - 1] = '}' ->
          let digits = String.sub s (i + 2) (j - i - 3) in
          if digits <> "" && String.for_all (fun c -> c >= '0' && c <= '9') digits then Some j else None
      | _ -> None
  in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else
      match slot_end i with
      | Some j -> go (j + 1)
      | None ->
          (* a bare "{0}" (the Prisoner's death damage) becomes "some" *)
          if i + 2 < n && s.[i] = '{' && s.[i + 1] = '0' && s.[i + 2] = '}' then (
            Buffer.add_string b some;
            go (i + 3))
          else (
            Buffer.add_char b s.[i];
            go (i + 1))
  in
  go 0;
  (* the group often follows a space: "... by 8 + fire mana [{0}]" *)
  String.trim (String.concat " " (String.split_on_char ' ' (Buffer.contents b)))

let body_text b (c : card) =
  let t = text b c.description in
  let t = if c.desc_args = None then t else drop_board_args t (text b "grimoire.some") in
  (* The card text marks the basic houses it names with [@0] … [@3] and their
     power with [#0] … [#3]. The site publishes the standard-mode houses, so
     those are the names it uses. *)
  let power key = if has_key b (key ^ ".mana") then text b (key ^ ".mana") else text b key in
  let houses =
    [ "anger"; "sadness"; "fear"; "joy" ]
    |> List.map (fun key -> (text b key, power key))
    |> Array.of_list
  in
  Assets.fill_basic_houses t houses

(* a file name with a space or an accent in it still has to make a valid URL:
   "Bill Bull.jpg" and "DwarvenCrossbowman" are both in assets/cards *)
let url_enc s =
  let b = Buffer.create (String.length s * 3) in
  String.iter
    (fun c ->
      if
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || String.contains "-_.~" c
      then Buffer.add_char b c
      else Printf.bprintf b "%%%02X" (Char.code c))
    s;
  Buffer.contents b

(* the art file the game would draw for this card, or None *)
let art_name (c : card) = match Assets.card_art c with None -> None | Some p -> Some (Filename.basename p)

(* The plain letter a code point sorts with: the accent of a letter does not
   decide the order, so "Éclair" sorts with the "e" names and not after the
   "z" ones, and "Kẻ Bên Kia" before "Kẻ Buộc Hồn". The letters of the
   languages we ship, with the plain letter they sort with. *)
let folded_letters =
  [
    ("a", "ÀÁÂÃÄÅĀĂĄǍǞǠǺȀȂȦḀẠẢẤẦẨẪẬẮẰẲẴẶàáâãäåāăąǎǟǡǻȁȃȧḁạảấầẩẫậắằẳẵặ");
    ("b", "ḂḄḆḃḅḇ");
    ("c", "ÇĆĈĊČḈçćĉċčḉ");
    ("d", "ÐĎĐḊḌḎḐḒðďđḋḍḏḑḓ");
    ("e", "ÈÉÊËĒĔĖĘĚȄȆȨḔḖḘḚḜẸẺẼẾỀỂỄỆèéêëēĕėęěȅȇȩḕḗḙḛḝẹẻẽếềểễệ");
    ("f", "Ḟƒḟ");
    ("g", "ĜĞĠĢǦǴḠĝğġģǧǵḡ");
    ("h", "ĤĦȞḢḤḦḨḪĥħȟḣḥḧḩḫẖ");
    ("i", "ÌÍÎÏĨĪĬĮİǏȈȊḬḮỈỊìíîïĩīĭįǐȉȋḭḯỉị");
    ("j", "Ĵĵǰ");
    ("k", "ĶǨḰḲḴķǩḱḳḵ");
    ("l", "ĹĻĽĿḶḸḺḼĺļľŀḷḹḻḽ");
    ("m", "ḾṀṂḿṁṃ");
    ("n", "ÑŃŅŇŊǸṄṆṈṊñńņňŉŋǹṅṇṉṋ");
    ("o", "ÒÓÔÕÖØŌŎŐƠǑǪǬȌȎȪȬȮȰṌṎṐṒỌỎỐỒỔỖỘỚỜỞỠỢòóôõöøōŏőơǒǫǭȍȏȫȭȯȱṍṏṑṓọỏốồổỗộớờởỡợ");
    ("p", "ṔṖṕṗ");
    ("r", "ŔŖŘȐȒṘṚṜṞŕŗřȑȓṙṛṝṟ");
    ("s", "ŚŜŞŠȘṠṢṤṦṨśŝşšſșṡṣṥṧṩ");
    ("t", "ŢŤŦȚṪṬṮṰţťŧțṫṭṯṱẗ");
    ("u", "ÙÚÛÜŨŪŬŮŰŲƯǓǕǗǙǛȔȖṲṴṶṸṺỤỦỨỪỬỮỰùúûüũūŭůűųưǔǖǘǚǜȕȗṳṵṷṹṻụủứừửữự");
    ("v", "ṼṾṽṿ");
    ("w", "ŴẀẂẄẆẈŵẁẃẅẇẉẘ");
    ("x", "ẊẌẋẍ");
    ("y", "ÝŶŸȲẎỲỴỶỸýÿŷȳẏẙỳỵỷỹ");
    ("z", "ŹŻŽẐẒẔźżžẑẓẕ");
  ]

let folded =
  let tbl = Hashtbl.create 512 in
  List.iter
    (fun (base, letters) ->
      let n = String.length letters in
      let rec go i =
        if i < n then (
          let d = String.get_utf_8_uchar letters i in
          let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
          Hashtbl.replace tbl cp base;
          go (i + Uchar.utf_decode_length d))
      in
      go 0)
    folded_letters;
  tbl

(* the case of a letter that has no accent to fold: the capitals of the
   languages we ship fold to their small form *)
let lowercase_cp cp =
  if cp >= 0x41 && cp <= 0x5A then cp + 0x20
  else if cp >= 0x400 && cp <= 0x40F then cp + 0x50
  else if cp >= 0x410 && cp <= 0x42F then cp + 0x20
  else cp

(* The sort key of a house name. The fold reads the string as UTF-8, one code
   point at a time; UTF-8 keeps the order of the code points, so comparing
   the folded bytes compares the letters. *)
let sort_key s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then (
      let d = String.get_utf_8_uchar s i in
      let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
      (match Hashtbl.find_opt folded cp with
      | Some base -> Buffer.add_string b base
      | None -> Buffer.add_utf_8_uchar b (Uchar.of_int (lowercase_cp cp)));
      go (i + Uchar.utf_decode_length d))
  in
  go 0;
  Buffer.contents b

(* ------------------------------------------------------------ data *)

(* one card as the page wants it *)
let card_json b (h : house) (c : card) =
  let fields =
    [
      ("key", `String c.key);
      ("name", `String (text b c.name));
      ("house", `String h.h_key);
      ("cost", `Int c.cost);
      ("spell", `Bool (c.is_spell || c.is_aura));
      ("text", `String (body_text b c));
      ("art", match art_name c with None -> `Null | Some f -> `String ("cards/" ^ url_enc f));
    ]
  in
  match c.creature_body with
  | None -> `Assoc fields
  | Some b2 ->
      let attack = match b2.b_attack.base with Some a -> `Int a | None -> `Null in
      `Assoc (fields @ [ ("attack", attack); ("life", `Int b2.b_life) ])

(* the blurb the game shows under a house name, when the i18n has one
   (gui_board.ml, house_blurb) *)
let house_blurb b (h : house) =
  let key = h.h_key ^ ".description" in
  if has_key b key then text b key else ""

let house_json b (h : house) index =
  (* The class author shown beside the house name on the page, from the game's
     settings groups (Table.house_groups): sinist, Wavelength, sfault, ... *)
  let author = match Table.group_of_house h.h_key with Some a -> `String a | None -> `Null in
  `Assoc
    [
      ("key", `String h.h_key);
      ("name", `String (text b h.h_name));
      ("blurb", `String (house_blurb b h));
      ("author", author);
      ("index", `Int index);
    ]

(* the strings of the page itself, from the same bundle as the card text *)
let ui_keys =
  [
    "title";
    "description";
    "back";
    "language";
    "spell";
    "creature";
    "token";
    "search";
    "noart";
    "loading";
    "none";
    "summary";
    "footer";
  ]

let ui_json b = `Assoc (List.map (fun k -> (k, `String (text b ("grimoire." ^ k)))) ui_keys)

(* ------------------------------------------------------------ template *)

(* The page is a static file: it draws the list from cards.json in the
   browser, so nothing in it is generated. It lives in templates/ as a
   skeleton plus its style sheet and its script, and the tool writes them out
   with the two {{...}} markers filled in. *)

(* the template, checked here so a missing file fails with a name and not with
   a "no such file" from halfway through writing the page. The art lookup
   wants ./assets, so the tool runs from the workspace root either way. *)
let template_dir () =
  if Sys.file_exists (Filename.concat !template "cards.html") then !template
  else failwith (Printf.sprintf "no page template in %s (cards.html, cards.css, cards.js)" !template)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic))

(* The {{name}} markers are filled in one pass, so a brace in the style sheet
   or the script cannot be mistaken for one; an unknown marker is left as it
   is, to show up in the page rather than vanish. *)
let fill template values =
  let n = String.length template in
  let buf = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else if i + 1 < n && template.[i] = '{' && template.[i + 1] = '{' then (
      match String.index_from_opt template (i + 2) '}' with
      | Some j when j + 1 < n && template.[j + 1] = '}' ->
          let name = String.sub template (i + 2) (j - i - 2) in
          let value =
            match List.assoc_opt name values with Some v -> v | None -> String.sub template i (j + 2 - i)
          in
          Buffer.add_string buf value;
          go (j + 2)
      | _ ->
          Buffer.add_char buf template.[i];
          go (i + 1))
    else (
      Buffer.add_char buf template.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents buf

(* the page, with the style sheet and the script inlined *)
let page_html dir langs =
  fill
    (read_file (Filename.concat dir "cards.html"))
    [
      ("css", read_file (Filename.concat dir "cards.css"));
      ("js", read_file (Filename.concat dir "cards.js"));
      ("langs", Yojson.Safe.to_string (langs_json langs));
    ]

(* ------------------------------------------------------------ main *)

let () =
  Arg.parse
    [
      ("--out", Arg.Set_string out, "DIR  where to write the files (default docs)");
      ("--template", Arg.Set_string template, "DIR  where the page template lives (default templates)");
    ]
    (fun a -> raise (Arg.Bad a))
    "cardlist [--out DIR] [--template DIR]";
  if not (Sys.file_exists !out) then failwith (!out ^ " is not a directory");
  (* the art lookup falls back to the English card name, so the list has to be
     built in English whatever language the player saved as theirs *)
  Unix.putenv "MANCER_ASSETS" "assets";
  Unix.putenv "MANCER_LANG" "en";
  Catalog.init ();
  let missing = ref [] in
  let langs = Table.languages in
  let dir = template_dir () in
  (* one list per language: the same cards, the text of that language *)
  List.iter
    (fun code ->
      let b = bundle code in
      (* the list on the left of the page is alphabetical, by the name the game
         shows in that language *)
      let house_name (h : house) = sort_key (text b h.h_name) in
      let houses =
        List.filter
          (fun (h : house) -> not (List.mem h.h_key hidden_houses))
          (List.sort (fun a b2 -> String.compare (house_name a) (house_name b2)) Catalog.all)
      in
      let cards = List.concat_map (fun (h : house) -> List.map (fun c -> (h, c)) (cards_of h)) houses in
      List.iter (fun (_, (c : card)) -> if art_name c = None then missing := c.key :: !missing) cards;
      let json =
        `Assoc
          [
            ("lang", `String code);
            ("langs", langs_json langs);
            ("ui", ui_json b);
            ("houses", `List (List.mapi (fun i h -> house_json b h i) houses));
            ("cards", `List (List.map (fun (h, c) -> card_json b h c) cards));
          ]
      in
      let file = Filename.concat !out (json_file code) in
      Out_channel.with_open_bin file (fun oc -> output_string oc (Yojson.Safe.to_string json));
      Printf.printf "wrote %s (%d cards, %d houses)\n" file (List.length cards) (List.length houses))
    langs;
  Out_channel.with_open_bin (Filename.concat !out "cards.html") (fun oc ->
      output_string oc (page_html dir langs));
  (* the list is one page now, so the pages of the old layout are stale *)
  let stale =
    Sys.readdir !out |> Array.to_list
    |> List.filter (fun f -> String.starts_with ~prefix:"cards" f && Filename.extension f = ".html")
  in
  List.iter
    (fun f ->
      if f <> "cards.html" then (
        Sys.remove (Filename.concat !out f);
        Printf.printf "removed stale %s\n" f))
    stale;
  Printf.printf "wrote %s/cards.html\n" !out;
  if !missing <> [] then (
    Printf.printf "no art for %d cards (shown as placeholders):\n" (List.length !missing);
    List.iter (Printf.printf "  %s\n") !missing)
