(** The opponent's deck as a bot may know it: cards seen played, nothing more. *)

open Sp

val seen_cards : updater -> pid:player_id -> card list
(** Opponent cards revealed so far (events seen since first call, plus their board). *)

val sample : desc:game_desc -> bot_id:player_id -> seen:card list -> n:int -> Random.State.t -> player_desc list
(** Up to [n] distinct legal opponent decks containing [seen]. Never the real deck. *)

val seen_key : card list -> string
(** Canonical key of a seen set, for caching beliefs until a new card shows up. *)

val superset_samples : int ref
(** Deals merged into {!guess}. *)

val superset : player_desc list -> player_desc option
(** Union of the decks' cards, house by house, in card order. [None] on an empty list. *)

val with_belief : game_state -> game_desc -> player_id -> player_desc -> game_state * game_desc
(** Replace the opponent of the given player by a guessed deck, in state and desc. *)

val guess : updater -> pid:player_id -> player_desc
(** Oversized guessed deck ({!superset} of {!superset_samples} deals), kept per
    seat until a new opponent card is seen. *)

val blind :
  (pid:player_id -> desc:game_desc -> game_state -> command option) -> Game.mover
(** [blind execute] is a mover that runs [execute] against {!guess}, not the real deck. *)
