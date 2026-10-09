(** User preferences in the [.mancer] folder, ported from Scala [Storage].

    The desktop Scala game keeps a Java properties file at [<home>/.mancer/user.prefs]
    ([Gdx.files external ".mancer/user.prefs]"). Every setting the lobby writes — language, sound level,
    portraits, house choices, campaign progress — goes through [Storage.persist], which updates the in-memory
    properties, stores the file, and re-reads the fields.

    This module is the same idea: key names and defaults match [Storage.scala] so the two front ends can share
    one file, and [persist] writes the file straight away. Keys the OCaml UIs need beyond the Scala set (the
    two seats, the two single specials, the bot budget and the two portraits) live under [setup.*]; the shared
    keys ([language], [sound.level], [player.portrait], …) keep their Scala names. *)

(* Key names mirror [Storage]'s constants. *)
let key_user_name = "user.name"
let key_card_theme = "card.theme"
let key_server = "server.host"
let key_server_port = "server.port"
let key_class_choice = [ "class.choice"; "ai.class.choice" ]
let key_ai_opponent = "ai.opponent"
let key_player_portrait = "player.portrait"
let key_sound_level = "sound.level"
let key_campaign_completed = "campaign.completed"
let key_campaign_position = "campaign.position"
let key_campaign_night_rest = "campaign.night.rest"
let key_campaign_wake_far = "campaign.wake.far"
let key_debug_mode = "debug.mode"
let key_language = "language"

(* Extra keys for the OCaml setup screens (seats, single specials, bot
   budget, both portraits). The Scala [class.choice] pools hold many houses,
   so a single picked special gets its own key instead of clobbering them. *)
let key_setup_seats = "setup.seats"
let key_setup_specials = "setup.specials"
let key_setup_budget = "setup.budget"
let key_setup_portraits = "setup.portraits"
let default_server = "172.99.78.51"
let default_server_port = "12345"
let default_card_theme = "vibe"
let default_ai_opponent = "summoner"
let default_player_portrait = "ember"
let default_sound_level = 1.0
let default_campaign_position = "day_start"
let default_language = "en"
let default_budget_ms = 1000
let default_seats = ("human", "oracle")
let default_specials = ([ "Troopers" ], [ "Troopers" ])
let default_portraits = ("ember", "oracle")

type t = { path : string; props : (string, string) Hashtbl.t }

let default_path () =
  match Sys.getenv_opt "MANCER_USER_PREFS" with
  | Some p when String.trim p <> "" -> p
  | _ ->
      let home = Option.value (Sys.getenv_opt "HOME") ~default:"." in
      Filename.concat (Filename.concat home ".mancer") "user.prefs"

let path t = t.path

(* --- Java properties encoding -------------------------------------- *)

(* [Properties.load]: [\uXXXX], [\n \r \t], and a backslash before anything
   else quotes that character. *)
let unescape s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '\\' && i + 1 < n then (
      match s.[i + 1] with
      | 'u' when i + 5 < n -> (
          match int_of_string_opt ("0x" ^ String.sub s (i + 2) 4) with
          | Some cp when Uchar.is_valid cp ->
              Buffer.add_utf_8_uchar b (Uchar.of_int cp);
              go (i + 6)
          | _ -> go (i + 2))
      | 'n' ->
          Buffer.add_char b '\n';
          go (i + 2)
      | 'r' ->
          Buffer.add_char b '\r';
          go (i + 2)
      | 't' ->
          Buffer.add_char b '\t';
          go (i + 2)
      | '\n' ->
          (* line continuation: skip the newline and leading whitespace *)
          let j = ref (i + 2) in
          while !j < n && (s.[!j] = ' ' || s.[!j] = '\t') do
            incr j
          done;
          go !j
      | c ->
          Buffer.add_char b c;
          go (i + 2))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* [Properties.store] escapes backslashes, control characters, non-Latin1 as
   [\uXXXX], and the [: = # !] syntax characters in keys. *)
let escape s ~is_key =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | ' ' when is_key -> Buffer.add_string b "\\ "
      | '=' when is_key -> Buffer.add_string b "\\="
      | ':' when is_key -> Buffer.add_string b "\\:"
      | '#' when is_key -> Buffer.add_string b "\\#"
      | '!' when is_key -> Buffer.add_string b "\\!"
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* First unescaped [= : <space>] ends the key, like [Properties.load]. *)
let split_line line =
  let n = String.length line in
  let rec find i =
    if i >= n then None
    else if line.[i] = '\\' then find (i + 2)
    else if line.[i] = '=' || line.[i] = ':' then Some (i, i + 1)
    else if line.[i] = ' ' || line.[i] = '\t' then (
      let j = ref (i + 1) in
      while !j < n && (line.[!j] = ' ' || line.[!j] = '\t') do
        incr j
      done;
      if !j < n && (line.[!j] = '=' || line.[!j] = ':') then (
        incr j;
        Some (i, !j))
      else Some (i, !j))
    else find (i + 1)
  in
  match find 0 with
  | None -> (unescape (String.trim line), "")
  | Some (ki, vi) ->
      (unescape (String.trim (String.sub line 0 ki)), unescape (String.trim (String.sub line vi (n - vi))))

let load_into tbl file =
  try
    In_channel.with_open_text file (fun ic ->
        let rec loop buf =
          match In_channel.input_line ic with
          | None ->
              if buf <> "" then
                let k, v = split_line buf in
                Hashtbl.replace tbl k v
          | Some raw ->
              (* a trailing odd run of backslashes continues the line *)
              let bs = ref 0 in
              for i = String.length raw - 1 downto 0 do
                if raw.[i] = '\\' then incr bs else ()
              done;
              (* count only the trailing run *)
              let trailing =
                let c = ref 0 in
                let i = ref (String.length raw - 1) in
                while !i >= 0 && raw.[!i] = '\\' do
                  incr c;
                  decr i
                done;
                !c
              in
              ignore bs;
              if trailing mod 2 = 1 then loop (buf ^ String.sub raw 0 (String.length raw - 1))
              else
                let k, v = split_line (buf ^ raw) in
                Hashtbl.replace tbl k v;
                loop ""
        in
        loop "")
  with Sys_error _ -> ()

let load ?path () =
  let path = Option.value path ~default:(default_path ()) in
  let props = Hashtbl.create 32 in
  (try if Sys.file_exists path then load_into props path with Sys_error _ -> ());
  { path; props }

let get t key = Hashtbl.find_opt t.props key

let save t =
  let dir = Filename.dirname t.path in
  let rec mkdir_p p =
    if p <> "." && p <> "/" && not (Sys.file_exists p) then (
      mkdir_p (Filename.dirname p);
      try Unix.mkdir p 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  (try if not (Sys.file_exists dir) then mkdir_p dir with Sys_error _ -> ());
  try
    let keys = Hashtbl.fold (fun k _ acc -> k :: acc) t.props [] |> List.sort String.compare in
    Out_channel.with_open_text t.path (fun oc ->
        let tm = Unix.localtime (Unix.gettimeofday ()) in
        Printf.fprintf oc "#user preferences\n#%04d-%02d-%02d %02d:%02d:%02d\n" (tm.tm_year + 1900)
          (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec;
        List.iter
          (fun k ->
            Printf.fprintf oc "%s=%s\n" (escape k ~is_key:true)
              (escape (Hashtbl.find t.props k) ~is_key:false))
          keys)
  with Sys_error _ -> ()

(* [Storage.persist]: set the properties, store the file, re-read the
   fields. The typed getters below always read [props], so they observe the
   write without a separate refresh step. *)
let persist t kvs =
  List.iter (fun (k, v) -> Hashtbl.replace t.props k v) kvs;
  save t

let set t key value = persist t [ (key, value) ]

(* --- typed fields (defaults match [Storage]) ------------------------- *)

let split_csv v = String.split_on_char ',' v |> List.map String.trim |> List.filter (( <> ) "")
let language t = match get t key_language with Some v when String.trim v <> "" -> v | _ -> default_language

let set_language t v =
  let v = String.trim v in
  persist t [ (key_language, if v = "" then default_language else v) ]

let saved_language () =
  try
    let t = load () in
    match get t key_language with Some v when String.trim v <> "" -> Some (String.trim v) | _ -> None
  with _ -> None

let sound_level t =
  match get t key_sound_level with
  | None -> default_sound_level
  | Some v -> (
      match float_of_string_opt (String.trim v) with
      | Some f -> Float.max 0. (Float.min 1. f)
      | None -> default_sound_level)

let set_sound_level t v =
  let v = Float.max 0. (Float.min 1. v) in
  persist t [ (key_sound_level, Printf.sprintf "%.2f" v) ]

let player_portrait t =
  match get t key_player_portrait with
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> default_player_portrait

let set_player_portrait t v = persist t [ (key_player_portrait, v) ]

let ai_opponent t =
  match get t key_ai_opponent with
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> default_ai_opponent

let set_ai_opponent t v = persist t [ (key_ai_opponent, v) ]
let debug_mode t = match get t key_debug_mode with Some v -> String.trim v = "true" | None -> false
let set_debug_mode t v = persist t [ (key_debug_mode, string_of_bool v) ]

let user_name t =
  match get t key_user_name with Some v when String.trim v <> "" -> Some (String.trim v) | _ -> None

let card_theme t =
  match get t key_card_theme with
  | Some "original" -> default_card_theme
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> default_card_theme

let class_choice t idx =
  let key = List.nth key_class_choice idx in
  match get t key with None -> [] | Some v -> split_csv v

let set_class_choice t idx names = persist t [ (List.nth key_class_choice idx, String.concat "," names) ]
let campaign_completed t = match get t key_campaign_completed with None -> [] | Some v -> split_csv v

let campaign_position t =
  match get t key_campaign_position with
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> default_campaign_position

(* --- OCaml setup screens ---------------------------------------------- *)

let pair_of_csv v ~default =
  match split_csv v with [ a; b ] -> (a, b) | [ a ] -> (a, snd default) | _ -> default

let setup_seats t =
  match get t key_setup_seats with None -> default_seats | Some v -> pair_of_csv v ~default:default_seats

let set_setup_seats t (a, b) = persist t [ (key_setup_seats, a ^ "," ^ b) ]
let split_list v = String.split_on_char ';' v |> List.map String.trim |> List.filter (( <> ) "")

let setup_specials t =
  (* Sides stay comma-separated; names within a side are semicolon-separated,
     so old "wind,Troopers" files load as singletons. Empty means all houses. *)
  match get t key_setup_specials with
  | None -> default_specials
  | Some v -> (
      match split_csv v with
      | [ a; b ] -> (split_list a, split_list b)
      | [ a ] -> (split_list a, [])
      | _ -> ([], []))

let set_setup_specials t (a, b) =
  persist t [ (key_setup_specials, String.concat ";" a ^ "," ^ String.concat ";" b) ]

let setup_budget t =
  match get t key_setup_budget with
  | None -> default_budget_ms
  | Some v -> (
      match int_of_string_opt (String.trim v) with Some n when n > 0 -> n | _ -> default_budget_ms)

let set_setup_budget t ms = persist t [ (key_setup_budget, string_of_int ms) ]

let setup_portraits t =
  match get t key_setup_portraits with
  | None -> (player_portrait t, default_portraits |> snd)
  | Some v -> pair_of_csv v ~default:(player_portrait t, snd default_portraits)

let set_setup_portraits t (p0, p1) =
  persist t [ (key_setup_portraits, p0 ^ "," ^ p1); (key_player_portrait, p0) ]

(** A persisted seat name back to [`Human]/[`Bot]; unknown bot ids fall back to [fallback]. *)
let parse_seat ~allowed ~fallback = function
  | "human" -> `Human
  | id when List.mem id allowed -> `Bot id
  | _ -> fallback
