(* Build the mancer-assets checkout from the Scala repo, the job of its
   resources/pack_atlas.py minus the libGDX packing. Card art is keyed by card
   key (vibe theme first, then the original art), backgrounds by house key,
   sounds by sound key; TGAs become PNGs and portraits are cut to 256². Files
   under <out>/overrides are hand-made and copied over the result last.
   manifest.tsv records where each file came from. *)

open Mancer
open Sp
module R = Raylib

let src = ref (Option.value (Sys.getenv_opt "FREESPECTROGDX") ~default:"../freespectrogdx")
let out = ref "../mancer-assets"
let ( / ) = Filename.concat
let generated =
  [ "cards"; "backgrounds"; "combat"; "ui"; "portraits"; "particle"; "sounds"; "i18n"; "campaign" ]
let portrait_size = 256

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

(* raylib is built without TGA: truecolor, raw or RLE, 24/32 bpp *)
let load_tga file =
  let d = In_channel.with_open_bin file In_channel.input_all in
  let byte i = Char.code d.[i] and word i = Char.code d.[i] lor (Char.code d.[i + 1] lsl 8) in
  let kind = byte 2
  and w = word 12
  and h = word 14
  and bpp = byte 16 lsr 3
  and top = byte 17 land 0x20 <> 0 in
  if (kind <> 2 && kind <> 10) || (bpp <> 3 && bpp <> 4) then failwith (file ^ ": unsupported TGA");
  let img = R.gen_image_color w h (R.Color.create 0 0 0 0) in
  let pos = ref (18 + byte 0) and n = ref 0 in
  let pixel at =
    let a = if bpp = 4 then byte (at + 3) else 255 in
    let x = !n mod w and y = Int.div !n w in
    R.image_draw_pixel (Ctypes.addr img) x
      (if top then y else h - 1 - y)
      (R.Color.create (byte (at + 2)) (byte (at + 1)) (byte at) a);
    incr n
  in
  while !n < w * h do
    if kind = 2 then (
      pixel !pos;
      pos := !pos + bpp)
    else
      let head = byte !pos in
      let count = (head land 0x7f) + 1 in
      incr pos;
      if head land 0x80 <> 0 then (
        for _ = 1 to count do
          pixel !pos
        done;
        pos := !pos + bpp)
      else
        for _ = 1 to count do
          pixel !pos;
          pos := !pos + bpp
        done
  done;
  img

let load from = if ext from = ".tga" then load_tga from else R.load_image from

(* [dest] without extension; TGAs go through raylib to PNG, [~fit] downscales *)
let place ?fit from dest =
  let e = match ext from with ".tga" -> ".png" | ".jpeg" -> ".jpg" | e -> e in
  let dest = dest ^ e in
  let big =
    match fit with
    | None -> false
    | Some n ->
        let img = load from in
        let b = R.Image.width img > n in
        R.unload_image img;
        b
  in
  if ext from = ".tga" || big then (
    mkdir_p (Filename.dirname (!out / dest));
    let img = load from in
    if not (R.is_image_valid img) then failwith ("cannot read " ^ from);
    Option.iter (fun n -> if big then R.image_resize (Ctypes.addr img) n n) fit;
    if not (R.export_image img (!out / dest)) then failwith ("cannot write " ^ dest);
    R.unload_image img)
  else copy from dest;
  record dest from

(* ------------------------------------------------------------ card art *)

(* pack_atlas.py norm: case, apostrophes and spacing don't matter *)
let norm s =
  let s = String.lowercase_ascii s in
  let s = Str.global_replace (Str.regexp "'\\|\xe2\x80\x99") "" s in
  String.trim (Str.global_replace (Str.regexp "[ \t]+") " " s)

let aliases = [ ("mountain king", "moutain king") ]
let image_exts = [ ".jpg"; ".jpeg"; ".png"; ".tga" ]

let index dir =
  let tbl = Hashtbl.create 512 in
  List.iter
    (fun f ->
      if List.mem (ext f) image_exts && not (is_dir (dir / f)) then
        Hashtbl.replace tbl (norm (stem f)) (dir / f))
    (ls dir);
  tbl

let skip_suffixes = [ ".description"; ".choose"; ".phase"; ".noelement" ]
let prefix key = match String.index_opt key '.' with Some i -> String.sub key 0 i | None -> key

(* The house's cards, then every other i18n key under the same prefixes (summoned
   tokens, overlays): those are kept only when a picture exists. *)
let card_keys props (h : house) =
  let cards = h.h_cards @ h.h_additional_cards |> List.map (fun (c : card) -> c.key) in
  let prefixes = List.sort_uniq compare (h.h_key :: List.map prefix cards) in
  let extra =
    Hashtbl.fold
      (fun k v acc ->
        if
          List.mem (prefix k) prefixes
          && (not (List.mem k cards))
          && (not (List.exists (fun s -> String.ends_with ~suffix:s k) skip_suffixes))
          && not (Str.string_match (Str.regexp ".*{0}") v 0)
        then k :: acc
        else acc)
      props []
  in
  (List.sort_uniq compare cards, List.sort_uniq compare extra)

let sync_cards props =
  let vibe = !src / "resources/themes/vibe/Cards" and original = index (!src / "resources/Images/Cards") in
  let vibe_flat = index vibe in
  let missing = ref [] and from_original = ref [] and seen = Hashtbl.create 512 in
  List.iter
    (fun (h : house) ->
      let folder = Str.global_replace (Str.regexp " ") "" (norm (Assets.text h.h_name)) in
      let vibe_house = index (vibe / folder) in
      let cards, extra = card_keys props h in
      let find key =
        let name = norm (Assets.text key) in
        let alias = Option.value (List.assoc_opt name aliases) ~default:name in
        match
          List.find_map (fun (t, n) -> Hashtbl.find_opt t n) [ (vibe_house, name); (vibe_flat, name) ]
        with
        | Some f -> Some f
        | None ->
            List.find_map (fun n -> Hashtbl.find_opt original n) [ name; alias ]
            |> Option.map (fun f ->
                from_original := key :: !from_original;
                f)
      in
      let sync ~required key =
        if not (Hashtbl.mem seen key) then (
          Hashtbl.replace seen key ();
          match find key with
          | Some f -> place f ("cards" / key)
          | None -> if required then missing := Printf.sprintf "%s (%s)" key (Assets.text key) :: !missing)
      in
      List.iter (sync ~required:true) cards;
      List.iter (sync ~required:false) extra)
    Catalog.all;
  (List.rev !missing, List.rev !from_original)

(* ------------------------------------------------------------ the rest *)

(* Scala Board.Background.setBackground looks for "<house>-ground", "-sky" and
   "-smoke" next to the flat picture; the layers are named after the same stem,
   so "<name>-sky.jpg" pairs with "<name>-ground.png". *)
let layer_suffixes = [ "-ground"; "-sky"; "-smoke" ]

let sync_backgrounds () =
  let dir = !src / "resources/themes/vibe/Backgrounds" in
  let bgs = index dir in
  let names_of (h : house) =
    let display = Assets.text h.h_name in
    [ display; h.h_key; h.h_name ]
  in
  List.filter_map
    (fun (key, names) ->
      List.iter
        (fun suffix ->
          match List.find_map (fun n -> Hashtbl.find_opt bgs (norm (n ^ suffix))) names with
          | Some f -> place f ("backgrounds" / (key ^ suffix))
          | None -> ())
        layer_suffixes;
      match List.find_map (fun n -> Hashtbl.find_opt bgs (norm n)) names with
      | Some f ->
          place f ("backgrounds" / key);
          None
      | None -> Some key)
    (("default", [ "default" ]) :: List.map (fun (h : house) -> (h.h_key, names_of h)) Catalog.all)

let sync_dir ?fit from dest =
  List.iter (fun f -> if ext f = ".png" then place ?fit (from / f) (dest / stem f)) (ls from)

(* application.conf `sounds { key = file }`; fadePercent is not a clip *)
let sound_keys () =
  let lines =
    In_channel.with_open_text (!src / "core/src/main/resources/application.conf") In_channel.input_lines
  in
  let rec skip = function
    | [] -> []
    | l :: rest -> if String.trim l = "sounds {" then body rest else skip rest
  and body = function
    | [] -> []
    | l :: rest -> (
        let t = String.trim l in
        if t = "}" then []
        else
          match String.index_opt t '=' with
          | Some i ->
              let k = String.trim (String.sub t 0 i)
              and v = String.trim (String.sub t (i + 1) (String.length t - i - 1)) in
              if k <> "" && k <> "fadePercent" && v <> "" then (k, v) :: body rest else body rest
          | None -> body rest)
  in
  skip lines

let sync_sounds () =
  let dir = !src / "core/src/main/resources/sounds" in
  let keyed = sound_keys () in
  List.iter (fun (k, f) -> if Sys.file_exists (dir / f) then place (dir / f) ("sounds" / k)) keyed;
  List.iter
    (fun f ->
      if (not (is_dir (dir / f))) && not (List.mem_assoc (stem f) keyed) then
        place (dir / f) ("sounds" / stem f))
    (ls dir)

let rec sync_overrides rel =
  let dir = !out / "overrides" / rel in
  List.iter
    (fun f ->
      let r = if rel = "" then f else rel / f in
      if is_dir (dir / f) then sync_overrides r
      else (
        copy (dir / f) r;
        Buffer.add_string manifest (Printf.sprintf "%s\toverrides/%s\n" r r)))
    (ls dir)

let () =
  Arg.parse
    [
      ( "--src",
        Arg.Set_string src,
        "DIR  the freespectrogdx checkout (default $FREESPECTROGDX, else ../freespectrogdx)" );
      ("--out", Arg.Set_string out, "DIR  the mancer-assets checkout (default ../mancer-assets)");
    ]
    (fun a -> raise (Arg.Bad a))
    "sync_assets [--src DIR] [--out DIR]";
  if not (is_dir !out) then failwith (!out ^ " is not a directory");
  R.set_trace_log_level R.TraceLogLevel.Warning;
  List.iter (fun d -> rm_rf (!out / d)) ("manifest.tsv" :: generated);
  let resources = !src / "core/src/main/resources" in
  let i18n = resources / "i18n.properties" in
  List.iter
    (fun f ->
      if Str.string_match (Str.regexp "i18n\\(_\\([a-z]+\\)\\)?\\.properties$") f 0 then (
        let lang = try Str.matched_group 2 f with Not_found -> "en" in
        let dest = "i18n" / (lang ^ ".properties") in
        copy (resources / f) dest;
        record dest (resources / f)))
    (ls resources);
  Unix.putenv "MANCER_ASSETS" !out;
  let props = Assets.load_properties i18n in
  Catalog.init ();
  let missing, from_original = sync_cards props in
  let no_background = sync_backgrounds () in
  List.iter
    (fun n -> place (!src / "resources/Images/Combat" / (n ^ ".tga")) ("combat" / n))
    [ "raka"; "rakaSpell"; "slot" ];
  List.iter
    (fun n -> place (!src / "resources" / (n ^ ".png")) ("combat" / n))
    [ "crystalize"; "sand"; "shield"; "death"; "mortal"; "hird"; "limbo" ];
  (* Scala pack_atlas: the Cocoon card picture is also the empty-slot sprite. *)
  List.find_opt Sys.file_exists
    [ !src / "resources/themes/vibe/Cards/Cocoon.jpg"; !out / "cards" / "zen.cocoon.jpg" ]
  |> Option.iter (fun from -> place from ("combat" / "cocoon"));
  List.iter
    (fun n -> place (!src / "core/src/main/resources/src/images" / (n ^ ".png")) ("particle" / n))
    [ "particle"; "smoke"; "thin" ];
  sync_dir (!src / "core/src/main/resources/data/ui") "ui";
  sync_dir ~fit:portrait_size (!src / "core/src/main/resources/data/portraits") "portraits";
  List.iter
    (fun n ->
      let from = resources / "data" / "campaign" / (n ^ ".jpg") in
      if Sys.file_exists from then place from ("campaign" / n))
    [ "map"; "epilogue" ];
  sync_sounds ();
  sync_overrides "";
  Out_channel.with_open_text (!out / "manifest.tsv") (fun oc -> Buffer.output_buffer oc manifest);
  Printf.printf "wrote %s (%d files)\n" !out
    (List.length (String.split_on_char '\n' (Buffer.contents manifest)) - 1);
  if from_original <> [] then
    Printf.printf "original Spectromancer art (%d): %s\n" (List.length from_original)
      (String.concat ", " from_original);
  if no_background <> [] then Printf.printf "default background for: %s\n" (String.concat ", " no_background);
  if missing <> [] then (
    Printf.printf "missing card art (%d):\n" (List.length missing);
    List.iter (Printf.printf "  %s\n") missing;
    exit 1)
