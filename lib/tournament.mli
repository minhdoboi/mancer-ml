(** Headless bot-vs-bot tournament ([Tournament.scala]): JSON logs, Elo, card rates. *)

type args = {
  games : int;
  bots : string list;
  budget_ms : int;
  max_turns : int;
  out : string;
  mode : string;
  dir : string;
  resume : string;
  include_ : string list;
  exclude : string list;
  min_plays : int;
  min_plays_set : bool;
  seed : int option;
  verbose : bool;
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

val compute_standings :
  standing_game list ->
  include_:string list ->
  exclude:string list ->
  (string * (int * int * int)) list * (string * float) list

val summary_text :
  (string * (int * int * int)) list -> (string * float) list -> card_rate list -> card_rate list -> string

val load_dir : string -> standing_game list

val run : args -> unit
(** [play] writes [game-NNN.json] plus [summary.json]; [summary] recomputes from [--dir]. *)
