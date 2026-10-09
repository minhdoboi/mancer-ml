(** Tournament JSON schema ([GameRecord.scala]). *)

open Sp

type move = {
  t : int;
  player : player_id;
  card : string option;
  house : string option;
  cost : int option;
  slot : int option;
  side : string option;
  on : string option;
  vs : string option;
}

type t = {
  game : string;
  bot_names : string * string;
  starting_player : player_id;
  special_names : string * string;
  decks : player_desc array;
  winner_bot : string option;
  winner_player : player_id option;
  draw : bool;
  turns : int;
  reason : string;
  moves : move list;
}

val esc : string -> string
val special_name : player_desc -> string
val winner_class : t -> string option
val move_of : t:int -> player:player_id -> command option -> game_state -> game_desc -> move
val to_json : t -> string
