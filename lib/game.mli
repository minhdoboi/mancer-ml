(** Deck building, a greedy bot and the headless turn loop (CardShuffle.scala, HeadlessGame in
    Tournament.scala). *)

open Sp

(** {1 Setup} *)

val init_cards : house -> unit
(** Assigns cost, house index and card index to the house's cards, like Scala's House.initCards. Call once per
    house before building decks. *)

val make_player_desc : house -> player_desc

val initial_player_state : int list -> player_desc -> player_state
(** [initial_player_state mana desc]: [mana] for the four base houses; the special house gets 2. *)

val initial_game_state : game_desc -> game_state

(** {1 Bot} *)

val available_commands : updater -> player_update -> command list
(** Legal commands, cheapest first. *)

val bot_move : updater -> player_update -> command option

(** {1 Driver} *)

type result = { winner : player_id option; turns : int }

val step : updater -> (updater -> unit) -> unit
(** One engine step, published with {!Engine.Updater.lift}. *)

val play_game : game_desc -> max_turns:int -> verbose:bool -> result
