(** Deck building, a greedy bot and the headless turn loop (CardShuffle.scala, HeadlessGame in
    Tournament.scala). *)

open Sp

(** {1 Setup} *)

val init_cards : ?cost_fn:(int -> int) -> house -> unit
(** Assigns cost, house index and card index to the house's cards, like Scala's House.initCards. Call once per
    house before building decks. Additional cards keep the cost they already have. *)

val empty_special : house
(** Placeholder special house with no cards. *)

val make_player_desc : ?special:house -> house -> player_desc
(** One basic house plus a special (defaults to {!empty_special}). *)

val initial_player_state : int list -> player_desc -> player_state
(** [initial_player_state mana desc]: four mana values get a special of 2 appended; five values are used
    as-is. Raises [Invalid_argument] for any other length. *)

val initial_game_state : ?mana0:int list -> ?mana1:int list -> game_desc -> game_state

(** {1 Bot} *)

val available_commands : updater -> player_update -> command list
(** Legal commands, cheapest first. *)

val bot_move : updater -> player_update -> command option

(** {1 Driver} *)

type result = { winner : player_id option; turns : int }

val step : updater -> (updater -> unit) -> unit
(** One engine step, published with {!Engine.Updater.lift}. *)

val play_game : ?mana0:int list -> ?mana1:int list -> game_desc -> max_turns:int -> verbose:bool -> result
