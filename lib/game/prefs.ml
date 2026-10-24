(** User preferences in the [.mancer] folder, ported from Scala [Storage].

    The desktop Scala game keeps a Java properties file at [<home>/.mancer/user.prefs]
    ([Gdx.files external ".mancer/user.prefs]"). Every setting the lobby writes — language, sound level,
    portraits, house choices, campaign progress — goes through [Storage.persist], which updates the in-memory
    properties, stores the file, and re-reads the fields.

    This module is the same idea: key names and defaults match [Storage.scala] so the two front ends can share
    one file, and [persist] writes the file straight away. Keys the OCaml UIs need beyond the Scala set (the
    two seats, the two single specials, the bot budget and the two portraits) live under [setup.*]; the shared
    keys ([language], [sound.level], [player.portrait], …) keep their Scala names. *)

type side = Player | Ai

(* Key names mirror [Storage]'s constants. *)
let key_user_name = "user.name"
let key_card_theme = "card.theme"
let key_server = "server.host"
let key_server_port = "server.port"
let key_class_choice = function Player -> "class.choice" | Ai -> "ai.class.choice"
let key_ai_opponent = "ai.opponent"
let key_player_portrait = "player.portrait"
let key_sound_level = "sound.level"
let key_campaign_completed = "campaign.completed"
let key_campaign_position = "campaign.position"
let key_campaign_night_rest = "campaign.night.rest"
let key_campaign_wake_far = "campaign.wake.far"
let key_campaign_class = "campaign.class"
let key_campaign_mode = "campaign.mode"
let key_debug_mode = "debug.mode"
let key_ambient_sound = "sound.ambient"
let key_language = "language"

(* Extra keys for the OCaml setup screens (seats, single specials, bot
   budget, both portraits). The Scala [class.choice] pools hold many houses,
   so a single picked special gets its own key instead of clobbering them. *)
let key_setup_seats = "setup.seats"
let key_setup_specials = "setup.specials"
let key_setup_budget = "setup.budget"
let key_setup_portraits = "setup.portraits"
let key_setup_mode = "setup.mode"
let default_server = "172.99.78.51"
let default_server_port = "12345"
let default_card_theme = "vibe"
let default_ai_opponent = "summoner"
let default_player_portrait = "ember"
let default_sound_level = 1.0
let default_ocean_sound = true
let default_ambient_sound = default_ocean_sound
let default_campaign_position = "day_start"
(* The house the player plays with all through a campaign. *)
let default_campaign_class = "shaman"
(* A campaign keeps the classic deal unless the player picks otherwise. *)
let default_campaign_mode = "classic"
let default_language = "en"
let default_budget_ms = 1000
let default_mode = "standard"
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

let load ?path () =
  let path = Option.value path ~default:(default_path ()) in
  let props = Hashtbl.create 32 in
  if Sys.file_exists path then Properties.load_into props path;
  { path; props }

let get t key = Hashtbl.find_opt t.props key

let rec mkdir_p dir =
  if dir <> "." && dir <> "/" && not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

(* Prefs are best effort: a read-only home or full disk must not take the game down. The file is written to
   a temporary name and renamed, so a crash cannot leave a truncated [user.prefs]. *)
let save t =
  try
    mkdir_p (Filename.dirname t.path);
    let keys = Hashtbl.fold (fun k _ acc -> k :: acc) t.props [] |> List.sort String.compare in
    let tmp = t.path ^ ".tmp" in
    Out_channel.with_open_text tmp (fun oc ->
        let tm = Unix.localtime (Unix.gettimeofday ()) in
        Printf.fprintf oc "#user preferences\n#%04d-%02d-%02d %02d:%02d:%02d\n" (tm.tm_year + 1900)
          (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec;
        List.iter
          (fun k ->
            Printf.fprintf oc "%s=%s\n" (Properties.escape k ~is_key:true)
              (Properties.escape (Hashtbl.find t.props k) ~is_key:false))
          keys);
    Sys.rename tmp t.path
  with Sys_error _ | Unix.Unix_error _ -> ()

(* [Storage.persist]: set the properties, store the file, re-read the
   fields. The typed getters below always read [props], so they observe the
   write without a separate refresh step. *)
let persist t kvs =
  List.iter (fun (k, v) -> Hashtbl.replace t.props k v) kvs;
  save t

let set t key value = persist t [ (key, value) ]

(* --- typed fields (defaults match [Storage]) ------------------------- *)

let get_nonempty t key =
  match get t key with Some v when String.trim v <> "" -> Some (String.trim v) | _ -> None

let get_bool ?(default = false) t key =
  match get t key with Some v -> String.trim v = "true" | None -> default

let split_csv v = String.split_on_char ',' v |> List.map String.trim |> List.filter (( <> ) "")
let language t = Option.value (get_nonempty t key_language) ~default:default_language

let set_language t v =
  let v = String.trim v in
  persist t [ (key_language, if v = "" then default_language else v) ]

let saved_language () =
  match get (load ()) key_language with Some v when String.trim v <> "" -> Some (String.trim v) | _ -> None

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

(* lobby ambient sound, on by default unless the sound tab turns it off *)
let ambient_sound t = get_bool ~default:default_ambient_sound t key_ambient_sound
let set_ambient_sound t v = persist t [ (key_ambient_sound, string_of_bool v) ]
let player_portrait t = Option.value (get_nonempty t key_player_portrait) ~default:default_player_portrait
let set_player_portrait t v = persist t [ (key_player_portrait, v) ]
let ai_opponent t = Option.value (get_nonempty t key_ai_opponent) ~default:default_ai_opponent
let set_ai_opponent t v = persist t [ (key_ai_opponent, v) ]
let debug_mode t = get_bool t key_debug_mode
let set_debug_mode t v = persist t [ (key_debug_mode, string_of_bool v) ]
let user_name t = get_nonempty t key_user_name

let card_theme t =
  match get t key_card_theme with
  | Some "original" -> default_card_theme
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> default_card_theme

let class_choice t side = match get t (key_class_choice side) with None -> [] | Some v -> split_csv v
let set_class_choice t side names = persist t [ (key_class_choice side, String.concat "," names) ]
let campaign_completed t = match get t key_campaign_completed with None -> [] | Some v -> split_csv v

let campaign_position t =
  match get t key_campaign_position with
  | Some v when Option.is_some (Campaign.by_id (String.trim v)) -> String.trim v
  | _ -> default_campaign_position

let campaign_night_rest t = get_bool t key_campaign_night_rest
let campaign_wake_far t = get_bool t key_campaign_wake_far
let encode_completed completed = String.concat "," (List.sort String.compare completed)

let persist_campaign t completed position night_rest wake_far =
  persist t
    [
      (key_campaign_completed, encode_completed completed);
      (key_campaign_position, position);
      (key_campaign_night_rest, string_of_bool night_rest);
      (key_campaign_wake_far, string_of_bool wake_far);
    ]

let complete_campaign t ?(extras = []) id =
  let completed =
    let add acc x = if List.mem x acc then acc else x :: acc in
    List.fold_left add (campaign_completed t) (id :: extras)
  in
  persist t
    [
      (key_campaign_completed, encode_completed completed); (key_campaign_position, Campaign.next_position id);
    ]

let reset_campaign t = persist_campaign t [] Campaign.day_start_id false false

let reset_night_campaign t =
  persist_campaign t (Campaign.day_progress (campaign_completed t)) Campaign.night_start_id false false

let mark_night_rested t = persist t [ (key_campaign_night_rest, "true") ]
let mark_wake_far t = persist t [ (key_campaign_wake_far, "true") ]

(* The campaign class: the special house the player plays with from the first
   node to the last. The map's class selector writes it, and it outlives a
   campaign restart. *)
let campaign_class t = Option.value (get_nonempty t key_campaign_class) ~default:default_campaign_class
let set_campaign_class t v = persist t [ (key_campaign_class, v) ]

(* The campaign mode: which basic houses a campaign is dealt from. It is
   separate from [setup.mode], so a duel mode does not change the campaign,
   and it outlives a campaign restart. *)
let campaign_mode t =
  match get t key_campaign_mode with
  | Some v when String.trim v = "standard" -> "standard"
  | _ -> default_campaign_mode

let set_campaign_mode t v = persist t [ (key_campaign_mode, v) ]

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

(* Game mode: "standard" (anger) or "classic" (fire). Stored as text so a new
   mode can be added without touching the file format. *)
let setup_mode t =
  match get t key_setup_mode with
  | Some v when String.trim v = "classic" -> "classic"
  | _ -> default_mode

let set_setup_mode t v = persist t [ (key_setup_mode, v) ]

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
