(** A game between two seats, each a human or a bot: dealing from a seed, bot dispatch, and the move choices a
    UI offers for each card. Built on {!Session}; the terminal UI uses it, and a graphical one can too.

    The human picks a card, then one of {!choices} for it; a bot seat plays with {!play_bot}. Both go through
    {!Session.submit}, so the event stream is the same whoever moves. *)

open Sp

type seat = Human | Bot of string  (** A bot id from {!Tournament.bot_ids}. *)

type setup = {
  seats : seat * seat;  (** Owner (bottom of the board), then opponent. *)
  specials : string * string;  (** Special houses, as {!Catalog.find_special} accepts them. *)
  budget_ms : int;  (** Search bots' time per move. *)
  seed : int option;  (** Deals and dice; [None] draws a fresh seed. *)
  max_turns : int;
  starting : player_id;  (** Who takes the first turn; default [owner]. *)
  life : int option;  (** Both players' starting life; [None] is 60. *)
}

val default_setup : setup
(** Human vs Oracle, Troopers vs Troopers, 1000 ms, random seed, 300 turns, owner starts. *)

val seat_name : seat -> string
(** ["human"] or the bot id. *)

val random_starter : ?rng:Random.State.t -> unit -> player_id
(** A coin flip for who takes the first turn, like Scala's duel [Local]. Campaign fights keep their scripted
    starter instead. *)

val player_portraits : string list
(** The faces a human seat can wear (Scala [Portraits.players]). *)

type opponent = { opp_bot : string; opp_kind : string; opp_portrait : string; opp_level : int }
(** An AI face and the bot behind it, like Scala's [AiFace] over [AiLevel]. [opp_kind] is the display family
    for [ai.<kind>] names ([gambler] faces still play [lecturer]). *)

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

val resolve_special : string list -> string
(** A side's house for the fight: a random pick from its checked names, or from every ported special when the
    set is empty or unknown. *)

type t

val campaign_setup : ?budget_ms:int -> ?seed:int -> player_special:string -> Campaign.node -> setup
(** Human vs the node's bot, player's special vs the fight house, with the node's starter and life. *)

val campaign_class : unit -> string
(** The house key of the player's campaign class: the special house the map's class selector stores, or the
    default class when the stored name is not a ported house. *)

val campaign_class_of : string -> string
(** The house key of a campaign class name, or the default class when the name is not a ported house. *)

val start : ?on_event:(game_event -> unit) -> setup -> t
(** Deals both decks and opens the session. [on_event] is as in {!Session.create}. Raises [Failure] for an
    unknown special or bot id. *)

val start_swapped : ?on_event:(game_event -> unit) -> ?keep_starter:bool -> ?budget_ms:int -> t -> t
(** Same opening hands with the two sides swapped. Seats stay; [starting] flips unless [keep_starter] is set
    (campaign). *)

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

(** {1 Lobby}

    Logic the terminal and raylib lobbies share. *)

val budget_options : int list
(** Bot think times offered in the settings, in ms. *)

val languages : string list

val faces_for_seat : seat -> string list
(** The portraits a seat can wear: the player faces for a human, the bot's faces otherwise. *)

val clamp_portrait : seat -> string -> string
(** [id] if the seat can wear it, else the seat's first face. *)

val seat_of_string : fallback:[ `Human | `Bot of string ] -> string -> seat
(** A persisted seat name; unknown bot ids fall back. *)

val duel_of_prefs : ?bot_opponent:bool -> Prefs.t -> (seat * string) * (seat * string)
(** The saved duel as [(seat, portrait)] per side. The AI side follows its saved face and that face's bot.
    With [~bot_opponent:true] a human fallback on the AI side becomes the oracle bot. *)

val persist_duel : (seat * string) * (seat * string) -> unit
(** Writes seats and portraits (and the AI face, when the second seat is a bot) to the prefs file. *)

val human_to_move : t -> player_id option
(** The player to move, when a human holds that seat. *)

(** What a human selecting a card should see happen. *)
type pick =
  | Not_enough_mana
  | No_target
  | Play of command  (** exactly one legal command: play it, including a lone slot *)
  | Choose of command list  (** several commands (targets or inputs) to pick from *)

val pick_card : t -> card_desc -> player_house_desc -> pick
