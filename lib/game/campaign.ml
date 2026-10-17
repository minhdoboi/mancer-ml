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

let fight house_key bot_id portrait_id ?life () = { house_key; bot_id; portrait_id; life }
let meeting portrait_id ?(choices = []) () = { portrait_id; choices }
let choice id = { id }

let node ?(fight = None) ?(meeting = None) id chapter x y unlock =
  { id; chapter; x; y; fight; unlock; meeting }

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

let mountain_approaches = [ "dreamweaver"; "powermancer"; "quantumancer" ]
let f = Option.some

let nodes =
  [
    node day_start_id Day 0.16 0.16 Open;
    node "entomologist" Day 0.22 0.28 Open
      ~fight:(f (fight "entomologist" "summoner" "summoner-f" ~life:30 ()));
    node "duckwoman" Day 0.33 0.43 (After [ "entomologist" ])
      ~meeting:(f (meeting "duckwoman" ~choices:[ choice go_city; choice avoid_city ] ()));
    node "disciples" Day 0.10 0.54 (After [ avoid_city ])
      ~fight:(f (fight "disciples" "strategist" "disciples" ~life:40 ()));
    node "farmer" Day 0.14 0.63 (After [ "disciples" ]) ~meeting:(f (meeting "farmer" ()));
    node "junk" Day 0.20 0.66 (After [ "farmer" ]) ~fight:(f (fight "junk" "strategist" "junk" ~life:40 ()));
    node "beaver" Day 0.35 0.70 (After [ "junk" ]) ~meeting:(f (meeting "beaver" ()));
    node "spiritshaman" Day 0.50 0.75 (After [ "beaver" ])
      ~fight:(f (fight "spiritshaman" "gambler" "spiritshaman" ~life:50 ()));
    node "quantumancer" Day 0.64 0.76 (After [ "spiritshaman" ])
      ~fight:(f (fight "quantumancer" "gambler" "quantumancer" ~life:50 ()));
    node "meteomancer" Day 0.37 0.15 Open
      ~fight:(f (fight "meteomancer" "summoner" "summoner" ~life:30 ()));
    node "seasons" Day 0.48 0.22 (After [ "meteomancer" ])
      ~fight:(f (fight "seasons" "strategist" "seasons" ~life:40 ()));
    node "sower" Day 0.70 0.42 (After [ "seasons" ]) ~fight:(f (fight "sower" "summoner" "sower" ~life:30 ()));
    node "otter" Day 0.82 0.46 (After [ "sower" ]) ~meeting:(f (meeting "otter" ()));
    node "fairy_knight" Day 0.94 0.55 (After [ "otter" ])
      ~fight:(f (fight "fairy" "strategist" "fairy_knight" ~life:40 ()));
    node "ram" Day 0.93 0.625 (After [ "fairy_knight" ]) ~meeting:(f (meeting "ram" ()));
    node "dreamweaver" Day 0.91 0.70 (After [ "ram" ])
      ~fight:(f (fight "dreamweaver" "strategist" "dreamweaver" ~life:40 ()));
    node "dudes" Day 0.45 0.50 (After_any [ go_city; "seasons" ])
      ~fight:(f (fight "Dudes" "strategist" "strategist-2" ~life:40 ()));
    node "rabbit" Day 0.525 0.55 (After [ "dudes" ]) ~meeting:(f (meeting "rabbit" ()));
    node "colors" Day 0.62 0.54 (After [ "rabbit" ])
      ~fight:(f (fight "colors" "strategist" "colors" ~life:40 ()));
    node "elementalist" Day 0.67 0.68 (After [ "colors" ])
      ~fight:(f (fight "elementalist" "gambler" "elementalist" ~life:50 ()));
    node "powermancer" Day 0.72 0.72 (After [ "elementalist" ])
      ~fight:(f (fight "powermancer" "gambler" "powermancer" ~life:50 ()));
    node "mountainking" Day 0.75 0.80 (After_any mountain_approaches)
      ~fight:(f (fight "mountainking" "gambler" "mountainking" ~life:50 ()));
    node "wind" Day 0.80 0.86 (After [ "mountainking" ]) ~fight:(f (fight "wind" "lecturer" "lecturer-f" ()));
    node night_start_id Night 0.44 0.45 (After [ day_last ]);
    node "crowd" Night 0.39 0.50 (After [ bring_city ]) ~meeting:(f (meeting "crowd" ()));
    node "antimancer" Night 0.37 0.54 (After [ "crowd" ])
      ~fight:(f (fight "antimancer" "lecturer" "antimancer" ()));
    node "zen" Night 0.10 0.76 (After [ "antimancer" ]) ~fight:(f (fight "zen" "lecturer" "zen" ()));
    node "fisherman" Night 0.32 0.80 (After [ "zen" ]) ~meeting:(f (meeting "fisherman" ()));
    node "conjurer" Night 0.55 0.78 (After [ "fisherman" ])
      ~fight:(f (fight "conjurer" "lecturer" "conjurer" ()));
    node "chronomaster" Night 0.62 0.81 (After [ "conjurer" ])
      ~fight:(f (fight "chronomaster" "gambler" "chronomaster" ()));
    node "shaman" Night 0.67 0.74 (After [ to_mountain ]) ~fight:(f (fight "shaman" "gambler" "shaman" ()));
    node "warp" Night 0.56 0.48 (After [ day_last ]) ~fight:(f (fight "warp" "lecturer" "warp" ()));
    node "elder" Night 0.71 0.56 (After [ "warp" ]) ~meeting:(f (meeting "elder" ()));
    node "kinetician" Night 0.81 0.65 (After [ "elder" ])
      ~fight:(f (fight "kinetician" "gambler" "kinetician" ~life:50 ()));
    node "girl" Night 0.76 0.66 (After [ "kinetician" ])
      ~meeting:(f (meeting "girl" ~choices:[ choice to_mountain; choice to_valley ] ()));
    node "rogue" Night 0.45 0.53 (After [ day_last ]) ~fight:(f (fight "rogue" "lecturer" "rogue" ()));
    node "fishman" Night 0.43 0.58 (After [ "rogue" ]) ~meeting:(f (meeting "fishman" ()));
    node "tinker" Night 0.50 0.57 (After [ "fishman" ]) ~fight:(f (fight "tinker" "lecturer" "tinker" ()));
    node "battlemage" Night 0.58 0.65 (After [ "tinker" ])
      ~fight:(f (fight "battlemage" "lecturer" "battlemage" ()));
    node "snowblood" Night 0.68 0.70 (After [ "battlemage" ])
      ~fight:(f (fight "snowblood" "gambler" "snowblood" ()));
    node "shadowmage" Night 0.49 0.24 (After [ stay_land ])
      ~fight:(f (fight "shadowmage" "oracle" "shadowmage" ()));
    node "panther" Night 0.62 0.36 (After [ "shadowmage" ]) ~meeting:(f (meeting "panther" ()));
    node "shadowcaster" Night 0.75 0.48 (After [ "panther" ])
      ~fight:(f (fight "shadowcaster" "oracle" "shadowcaster" ()));
    node "darkpriest" Night 0.87 0.65 (After [ "shadowcaster" ])
      ~fight:(f (fight "darkpriest" "oracle" "darkpriest" ()));
    node "vampires" Night 0.92 0.72 (After [ "darkpriest" ])
      ~fight:(f (fight "vampires" "oracle" "vampires" ()));
    node "deer" Night 0.87 0.78 (After [ "vampires" ]) ~meeting:(f (meeting "deer" ()));
    node "soulreaper" Night 0.97 0.86 (After [ "deer" ]) ~fight:(f (fight "reaper" "oracle" "soulreaper" ()));
    node "darksider" Night 0.70 0.63 (After_any [ "darkpriest"; to_valley ])
      ~fight:(f (fight "darksider" "oracle" "darksider" ()));
    node "patou" Night 0.67 0.67 (After [ "darksider" ]) ~meeting:(f (meeting "patou" ()));
    node "faerie_druid" Night 0.38 0.35 (After [ day_last ])
      ~fight:(f (fight "faerie" "oracle" "oracle-f" ()));
    node "soulbinder" Night 0.32 0.31 (After [ "faerie_druid" ])
      ~fight:(f (fight "soulbinder" "oracle" "soulbinder" ()));
    node "catboy" Night 0.25 0.28 (After [ "soulbinder" ])
      ~meeting:(f (meeting "catboy" ~choices:[ choice bring_city; choice stay_land ] ()));
    node "limbo" Night 0.82 0.83 (After_any [ "snowblood"; "soulreaper"; "shaman"; "patou"; "chronomaster" ])
      ~fight:(f (fight "limbo" "diviner" "limbo" ()));
    node "highpriest" Night 0.88 0.92 (After [ "limbo" ])
      ~fight:(f (fight "highpriest" "diviner" "highpriest" ()));
  ]

let by_id_tbl =
  let t = Hashtbl.create 64 in
  List.iter (fun n -> Hashtbl.replace t n.id n) nodes;
  t

let by_id id = Hashtbl.find_opt by_id_tbl id
let node id = match by_id id with Some n -> n | None -> failwith ("unknown campaign node " ^ id)
let is_fight n = Option.is_some n.fight
let is_playable n = Option.is_some n.fight || Option.is_some n.meeting

let edge ?(via = None) from toward = { from; toward; via }

let edges =
  [
    edge day_start_id "entomologist";
    edge "entomologist" "duckwoman";
    edge "duckwoman" "dudes" ~via:(Some go_city);
    edge "duckwoman" "disciples" ~via:(Some avoid_city);
    edge day_start_id "meteomancer";
    edge "meteomancer" "seasons";
    edge "seasons" "sower";
    edge "seasons" "dudes";
    edge "sower" "otter";
    edge "otter" "fairy_knight";
    edge "fairy_knight" "ram";
    edge "ram" "dreamweaver";
    edge "dreamweaver" "mountainking";
    edge "dudes" "rabbit";
    edge "rabbit" "colors";
    edge "colors" "elementalist";
    edge "elementalist" "powermancer";
    edge "powermancer" "mountainking";
    edge "disciples" "farmer";
    edge "farmer" "junk";
    edge "junk" "beaver";
    edge "beaver" "spiritshaman";
    edge "spiritshaman" "quantumancer";
    edge "quantumancer" "mountainking";
    edge "mountainking" "wind";
    edge "crowd" "antimancer";
    edge "zen" "fisherman";
    edge "fisherman" "conjurer";
    edge "conjurer" "chronomaster";
    edge "chronomaster" "limbo";
    edge "shaman" "limbo";
    edge night_start_id "warp";
    edge "warp" "elder";
    edge "elder" "kinetician";
    edge "kinetician" "girl";
    edge "girl" "shaman" ~via:(Some to_mountain);
    edge "girl" "darksider" ~via:(Some to_valley);
    edge night_start_id "faerie_druid";
    edge "faerie_druid" "soulbinder";
    edge "soulbinder" "catboy";
    edge "catboy" "crowd" ~via:(Some bring_city);
    edge "catboy" "shadowmage" ~via:(Some stay_land);
    edge night_start_id "rogue";
    edge "rogue" "fishman";
    edge "fishman" "tinker";
    edge "tinker" "battlemage";
    edge "battlemage" "snowblood";
    edge "snowblood" "limbo";
    edge "shadowmage" "panther";
    edge "panther" "shadowcaster";
    edge "shadowcaster" "darkpriest";
    edge "darkpriest" "vampires";
    edge "vampires" "deer";
    edge "deer" "soulreaper";
    edge "soulreaper" "limbo";
    edge "darkpriest" "darksider";
    edge "darksider" "patou";
    edge "patou" "limbo";
    edge "limbo" "highpriest";
  ]

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
      nodes
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
    [ "campaign." ^ node_id ^ "." ^ suffix; "campaign.end." ^ bot_id ^ "." ^ suffix; "campaign.end." ^ suffix ]
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
