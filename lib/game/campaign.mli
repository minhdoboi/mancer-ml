(** The single-player campaign map: nodes, unlocks and the lines spoken after a fight. Port of
    [priv.sp.campaign.Campaign]. *)

open Sp

type chapter = Day | Night

type unlock =
  | Open
  | After of string list  (** every listed id is completed *)
  | After_any of string list  (** any listed id is completed *)

(** Who takes the first turn of a campaign duel. Opponent starts unless listed in {!starters}. *)
type starter = Opponent_starts | Player_starts | Random_starts

type fight = { house_key : string; bot_id : string; portrait_id : string; life : int option }

type choice = { id : string }
(** Optional map-fork buttons on a meeting. [id] is stored in campaign progress. *)

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

val go_city : string
val avoid_city : string
val bring_city : string
val stay_land : string
val to_mountain : string
val to_valley : string
val day_last : string
val night_last : string
val day_start_id : string
val night_start_id : string
val nodes : unit -> node list
val edges : unit -> edge list

val reload : unit -> (unit, string) result
(** Re-reads the map from [MANCER_CAMPAIGN_MAP] (default [lib/game/campaign_map.json], relative to the working
    directory), replacing {!nodes}, {!edges} and {!by_id}. On error the old map is kept. *)
val by_id : string -> node option
val node : string -> node
val is_fight : node -> bool
val is_playable : node -> bool
val starter_of : string -> starter

val starter_player : ?rng:Random.State.t -> string -> player_id
(** Resolves {!starter_of} to a seat; [Random_starts] draws from [rng]. *)

val play_bot : string -> string
(** The Tournament bot that plays a campaign [bot_id]. Summoner, strategist and diviner play as themselves.
    Unported kinds fall back (gambler/apprentice and unknown → lecturer), matching Scala [GameBot.byId]. *)

val chapter_of : string list -> chapter

val day_progress : string list -> string list
(** Day nodes only, so a night restart keeps the day peak and drops night stops. *)

val is_finished : string list -> bool
val is_unlocked : node -> string list -> bool
val isolate_path : (string * string list) list

val available : string list -> string list
(** Playable, unlocked, incomplete nodes of the current chapter. Winning a key node in {!isolate_path} locks
    the rest of the map to that path. *)

val next_position : string -> string
val house_of : house list -> string -> house option

val end_message_key : node_id:string -> bot_id:string -> won:bool -> has_key:(string -> bool) -> string
(** Prefers [campaign.<id>.win|lose], then [campaign.end.<bot>.win|lose], then the generic
    [campaign.end.win|lose]. [has_key] lets tests resolve without the i18n bundle. *)

val npcs : string list
(** Non-playable campaign faces ([Portraits.npcs]). *)

val campaign_portraits : string list
(** Campaign-only fight faces, named after their node ([Portraits.campaign]). *)
