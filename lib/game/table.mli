(** A game between two seats, each a human or a bot: dealing from a seed, bot dispatch, and the move choices a
    UI offers for each card. Built on {!Session}; the terminal UI uses it, and a graphical one can too.

    The human picks a card, then one of {!choices} for it; a bot seat plays with {!play_bot}. Both go through
    {!Session.submit}, so the event stream is the same whoever moves. *)

open Sp

type seat = Human | Bot of string  (** A bot id from {!Tournament.bot_ids}. *)

type setup = {
  seats : seat * seat;  (** Owner (moves first, bottom of the board), then opponent. *)
  specials : string * string;  (** Special houses, as {!Catalog.find_special} accepts them. *)
  budget_ms : int;  (** Search bots' time per move. *)
  seed : int option;  (** Deals and dice; [None] draws a fresh seed. *)
  max_turns : int;
}

val default_setup : setup
(** Human vs Oracle, Troopers vs Troopers, 1000 ms, random seed, 300 turns. *)

val seat_name : seat -> string
(** ["human"] or the bot id. *)

val player_portraits : string list
(** The faces a human seat can wear (Scala [Portraits.players]). *)

type opponent = { opp_bot : string; opp_kind : string; opp_portrait : string; opp_level : int }
(** An AI face and the bot behind it, like Scala's [AiFace] over [AiLevel]. [opp_kind] is the display family
    for [ai.<kind>] names ([greedy] wears [summoner] faces). *)

val duel_opponents : opponent list
(** The AI faces for the duel dialog, in level order. *)

val faces_for_bot : string -> string list
(** The portraits associated with a bot id; unknown ids get greedy's faces. *)

val bot_of_portrait : string -> string option
(** The bot behind an AI face, if it is one. *)

val house_groups : (string * house list) list
(** Ported special houses grouped by author, in the Scala settings order. Only groups with ported houses
    appear, so this grows as houses are ported. *)

val group_of_house : string -> string option
(** The author group of a house key, if it is ported. *)

val special_houses : unit -> house list
(** The ported special houses, initialised. *)

type t

val start : ?on_event:(game_event -> unit) -> setup -> t
(** Deals both decks and opens the session. [on_event] is as in {!Session.create}. Raises [Failure] for an
    unknown special or bot id. *)

val session : t -> Session.t
val setup : t -> setup

val seed : t -> int
(** The seed actually used, to replay the deal with [setup.seed = Some seed]. *)

val seat : t -> player_id -> seat

val to_move : t -> player_id option
(** [None] once the game is over. *)

val hand : t -> player_id -> player_house_desc list
(** {!Game.player_houses} on the live board. *)

val playable : t -> card_desc -> player_house_desc -> bool
(** The player to move can pay for and play this card now. *)

val choices : t -> card -> command list
(** The legal commands of the player to move that play [card], one per target. [[]] when it is not playable.
*)

val target_side : command -> player_id option
(** Whose slot [cmd_input] names, or [None] for a card without input. *)

val bot_command : t -> command option
(** Asks the bot to move for its seat, without submitting; a bot with no playable card passes. The search only
    reads the live game, so it may run on another domain while the UI draws, as long as nothing submits
    meanwhile. Raises [Invalid_argument] when the game is over or a human is to move. *)

val play_bot : t -> unit
(** [submit t (bot_command t)]. *)

val submit : t -> command option -> unit
(** {!Session.submit}. *)

val record : t -> Game_record.t
(** The game so far in the tournament JSON format, for [Replay] and [compare_engines]. *)

val save : t -> dir:string -> string
(** Writes {!record} to [dir/game-<seed>-<time>.json], creating [dir], and returns the path. *)

val event_line : ?name:(card -> string) -> game_event -> string option
(** A one-line description for a game log; [None] for events not worth a line ([Ev_refresh], [Ev_focus]).
    [name] shows a card (default its [name], which is an i18n key for most houses). *)
