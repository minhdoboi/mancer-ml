(* The single-player campaign map. Port of priv.sp.campaign.Campaign. *)

open Sp

type chapter = Day | Night
type unlock = Open | After of string list | After_any of string list
type starter = Opponent_starts | Player_starts | Random_starts
type fight = { house_key : string; bot_id : string; portrait_id : string; life : int option }
type choice = { id : string }
type meeting = { portrait_id : string; choices : choice list }

type node = {
  id : string;
  chapter : chapter;
  x : float;
  y : float;
  fight : fight option;
  unlock : unlock;
  meeting : meeting option;
}

type edge = { from : string; toward : string; via : string option }

let go_city = "go_city"
let avoid_city = "avoid_city"
let bring_city = "bring_city"
let stay_land = "stay_land"
let to_mountain = "to_mountain"
let to_valley = "to_valley"
let day_last = "wind"
let night_last = "highpriest"
let day_start_id = "day_start"
let night_start_id = "night_start"
let choice id = { id }

let starters =
  [
    ("dudes", Player_starts);
    ("disciples", Player_starts);
    ("seasons", Random_starts);
    ("junk", Random_starts);
    ("dreamweaver", Random_starts);
    ("colors", Random_starts);
    ("elementalist", Random_starts);
    ("zen", Random_starts);
    ("spiritshaman", Random_starts);
    ("faerie_druid", Random_starts);
    ("soulbinder", Random_starts);
    ("chronomaster", Random_starts);
  ]

let starter_of id = match List.assoc_opt id starters with Some s -> s | None -> Opponent_starts

let starter_player ?(rng = Random.State.make_self_init ()) id =
  match starter_of id with
  | Player_starts -> owner
  | Opponent_starts -> opponent
  | Random_starts -> if Random.State.bool rng then owner else opponent

(* Scala [GameBot.byId]: unknown and apprentice land on lecturer. Gambler is
   not ported, so it uses lecturer too. *)
let play_bot = function
  | "gambler" | "apprentice" -> "lecturer"
  | ("summoner" | "strategist" | "diviner" | "oracle" | "lecturer" | "greedy") as id -> id
  | _ -> "lecturer"

(* The map itself (nodes, positions, unlocks, edges) lives in [campaign_map.json]
   and is embedded at build time as [Campaign_map_data.json]. Dev mode can
   re-read the file from disk with [reload]. *)
let parse_node j =
  let open Yojson.Basic.Util in
  let strings k j = j |> member k |> to_list |> List.map to_string in
  let fight_of j =
    {
      house_key = j |> member "house_key" |> to_string;
      bot_id = j |> member "bot_id" |> to_string;
      portrait_id = j |> member "portrait_id" |> to_string;
      life = j |> member "life" |> to_int_option;
    }
  in
  let meeting_of j =
    {
      portrait_id = j |> member "portrait_id" |> to_string;
      choices = (match member "choices" j with `Null -> [] | _ -> List.map choice (strings "choices" j));
    }
  in
  let opt f k = match member k j with `Null -> None | v -> Some (f v) in
  {
    id = j |> member "id" |> to_string;
    chapter =
      (match j |> member "chapter" |> to_string with
      | "day" -> Day
      | "night" -> Night
      | c -> failwith ("campaign_map.json: unknown chapter " ^ c));
    x = j |> member "x" |> to_number;
    y = j |> member "y" |> to_number;
    fight = opt fight_of "fight";
    unlock =
      (match (member "after" j, member "after_any" j) with
      | `Null, `Null -> Open
      | _, `Null -> After (strings "after" j)
      | `Null, _ -> After_any (strings "after_any" j)
      | _ -> failwith "campaign_map.json: both after and after_any");
    meeting = opt meeting_of "meeting";
  }

let parse_map json =
  let open Yojson.Basic.Util in
  let nodes = json |> member "nodes" |> to_list |> List.map parse_node in
  let edges =
    json |> member "edges" |> to_list
    |> List.map (fun j ->
           {
             from = j |> member "from" |> to_string;
             toward = j |> member "to" |> to_string;
             via = j |> member "via" |> to_string_option;
           })
  in
  let tbl = Hashtbl.create 64 in
  List.iter (fun n -> Hashtbl.replace tbl n.id n) nodes;
  (nodes, edges, tbl)

let current = ref (parse_map (Yojson.Basic.from_string Campaign_map_data.json))
let nodes () = let n, _, _ = !current in n
let edges () = let _, e, _ = !current in e
let by_id id = let _, _, t = !current in Hashtbl.find_opt t id
let node id = match by_id id with Some n -> n | None -> failwith ("unknown campaign node " ^ id)

let map_file () =
  Option.value (Sys.getenv_opt "MANCER_CAMPAIGN_MAP") ~default:"lib/game/campaign_map.json"

let reload () =
  try
    current := parse_map (Yojson.Basic.from_file (map_file ()));
    Ok ()
  with e -> Error (Printexc.to_string e)

let is_fight n = Option.is_some n.fight
let is_playable n = Option.is_some n.fight || Option.is_some n.meeting
let mem_id ids id = List.mem id ids
let chapter_of completed = if mem_id completed day_last then Night else Day

let day_progress completed =
  List.filter (fun id -> match by_id id with Some n -> n.chapter = Day | None -> false) completed

let is_finished completed = mem_id completed night_last

let is_unlocked n completed =
  match n.unlock with
  | Open -> true
  | After ids -> List.for_all (mem_id completed) ids
  | After_any ids -> List.exists (mem_id completed) ids

let isolate_path =
  [ ("antimancer", [ "zen"; "fisherman"; "conjurer"; "chronomaster"; "limbo"; "highpriest" ]) ]

let available completed =
  let chapter = chapter_of completed in
  let open_ids =
    List.filter_map
      (fun n ->
        if n.chapter = chapter && is_playable n && (not (mem_id completed n.id)) && is_unlocked n completed
        then Some n.id
        else None)
      (nodes ())
  in
  match List.find_opt (fun (won, _) -> mem_id completed won) isolate_path with
  | Some (_, path) -> List.filter (mem_id path) open_ids
  | None -> open_ids

let next_position won_id =
  if won_id = day_last then night_start_id
  else match List.assoc_opt won_id isolate_path with Some (hd :: _) -> hd | _ -> won_id

let house_of specials key =
  let n = Catalog.token key in
  match List.find_opt (fun (h : house) -> Catalog.token h.h_key = n) specials with
  | Some h -> Some h
  | None -> List.find_opt (fun (h : house) -> Catalog.token h.h_name = n) specials

let end_message_key ~node_id ~bot_id ~won ~has_key =
  let suffix = if won then "win" else "lose" in
  let candidates =
    [
      "campaign." ^ node_id ^ "." ^ suffix; "campaign.end." ^ bot_id ^ "." ^ suffix; "campaign.end." ^ suffix;
    ]
  in
  match List.find_opt has_key candidates with Some k -> k | None -> List.hd (List.rev candidates)

let npcs =
  [
    "farmer";
    "duckwoman";
    "ram";
    "rabbit";
    "beaver";
    "otter";
    "elder";
    "fishman";
    "fisherman";
    "deer";
    "girl";
    "patou";
    "catboy";
    "panther";
    "crowd";
    "gardener";
    "night-host";
    "night-soup";
  ]

let campaign_portraits =
  [
    "disciples";
    "junk";
    "sower";
    "colors";
    "elementalist";
    "powermancer";
    "mountainking";
    "seasons";
    "emotion";
    "paganist";
    "quantumancer";
    "spiritshaman";
    "fairy_knight";
    "dreamweaver";
    "antimancer";
    "zen";
    "conjurer";
    "chronomaster";
    "shaman";
    "warp";
    "kinetician";
    "rogue";
    "tinker";
    "battlemage";
    "snowblood";
    "shadowmage";
    "shadowcaster";
    "darkpriest";
    "vampires";
    "soulreaper";
    "darksider";
    "limbo";
    "highpriest";
    "soulbinder";
  ]
