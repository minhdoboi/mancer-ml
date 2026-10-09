(** Headless tournament ([Tournament.scala]): JSON logs, Elo, card rates.

    Two tournaments share the driver.  [bots] ranks the bots (every seat plays
    [--special]); [houses] ranks the houses against each other, with the bots
    as the instrument that plays them. *)

open Sp

type args = {
  games : int;
  bots : string list;
  budget_ms : int;
  max_turns : int;
  out : string;
  mode : string;  (** [bots], [houses] or [summary]. *)
  dir : string;
  resume : string;
  include_ : string list;
  exclude : string list;
  min_plays : int;
  min_plays_set : bool;
  seed : int option;
  verbose : bool;
  special : string list;
  (** The houses of [--special].  The bot tournament plays one of them on both
      seats; the house tournament compares them. *)
  opponent_special : string list;
}

type standing_game = {
  num : int;
  first : string;
  second : string;
  winner : string option;
  specials : string * string;
  winner_player : int option;
}

type card_rate = { card : string; house : string; plays : int; wins : int; draws : int; rate : float }

type house_standing = {
  h_name : string;
  h_games : int;
  h_wins : int;
  h_losses : int;
  h_draws : int;
  h_elo : float;
}
(** A house's record in a run: how it did as a seat, and its Elo. *)

type matchup = { m_games : int; m_wins : int; m_losses : int; m_draws : int }

type house_matrix = {
  mx_houses : string list;  (** The houses, in the order of the run. *)
  mx_rows : (string * (string * matchup) list) list;  (** House -> its record against each other house. *)
}
(** The houses against each other: the score of every pair, both ways. *)

val bot_ids : string list
(** [greedy], [summoner], [strategist], [lecturer], [oracle], [diviner]. *)

val mover : ?stats_log:bool -> budget:int -> string -> Game.mover
(** The bot named by [id] ([apprentice] is greedy), searching [budget] ms per move. [stats_log] (default true)
    prints one stats line per search move to stdout. Raises [Failure] for an unknown id. *)

val default_args : args
val parse : string array -> args
val ordered_pair : string -> string -> int -> string * string

val update_stats :
  string ->
  string ->
  string option ->
  (string, float) Hashtbl.t ->
  (string, int * int * int) Hashtbl.t ->
  unit

val update_elo : string -> string -> string option -> (string, float) Hashtbl.t -> unit
(** The online rating update alone, so houses can be rated like bots. *)

val compute_standings :
  standing_game list ->
  include_:string list ->
  exclude:string list ->
  (string * (int * int * int)) list * (string * float) list

val house_standings :
  standing_game list ->
  include_:string list ->
  exclude:string list ->
  house_standing list
(** Per-house record of a run.  A game counts for the house of the seat that
    won it; a game where both seats share a house labels them [name] and
    [name2]. *)

val init_specials : unit -> unit
(** Initializes the cards of every special house of the catalog. *)

val house_set : args -> house list
(** The houses a house tournament compares: the ones in [--special] and the
    ones in [--opponent-special], each of them once.  With no
    [--opponent-special] the [--special] list battles among itself. *)

val house_matrix :
  standing_game list -> include_:string list -> exclude:string list -> house_matrix
(** The record of every house against every other house, both ways. *)

val house_pairs : house list -> (house * house) list
(** The round robin of a house tournament. *)

val crossing : string * string -> house * house -> int -> (string * string) * (house * house)
(** The bots and the houses of game [g] of a pairing: the house at seat 0
    alternates every game, the bots every other one. *)

val entry_key : string -> first:string -> second:string -> specials:string * string -> string

val game_line : string -> id:int -> bot_a:string -> bot_b:string -> result:Game_record.t -> string
(** The log line of a game.  It names the subject of the tournament: the bots
    in the bot tournament, the houses in the house tournament, where the
    winner is the house of the winning seat. *)
(** The key a pairing is counted under, so a resumed run knows what is left. *)

val specials_of : game_desc -> suffix:bool -> string * string
(** The class of each seat.  With [suffix] the second seat gets a "2"
    appended when both seats play the same house, so they stay apart. *)

val summary_text :
  ?houses:house_standing list ->
  ?matrix:house_matrix ->
  (string * (int * int * int)) list ->
  (string * float) list ->
  card_rate list ->
  card_rate list ->
  string

val load_dir : string -> standing_game list

val run : args -> unit
(** [bots] and [houses] write [game-NNN.json] plus [summary.json]; [summary]
    recomputes from [--dir]. *)
