(** Deck building, bots and the headless turn loop (CardShuffle.scala, HeadlessGame in Tournament.scala). *)

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

val legal_commands : player_id -> game_desc -> game_state -> command list
(** Every legal command, interleaved one placement per card ([Choices.getNexts]). *)

val available_commands : updater -> player_update -> command list
(** Legal commands, cheapest first. The greedy bot's view. *)

val bot_move : updater -> player_update -> command option

type mover = updater -> player_id -> command option

val greedy_mover : mover
(** Cheapest legal card, frontmost open slot after [Choices] ordering. *)

type sim = { state : game_state; next : player_id; ended : player_id option }

val simulate : ?rng:Random.State.t -> game_desc -> game_state -> player_id -> command option -> sim
(** [BotSimulator.simulateCommand]: apply [cmd], then the end-turn chain unless a [Wait_player] transition
    hands over. The live game is not touched. Random effects draw from [rng], by default a self-seeded stream
    shared by all simulations and separate from any live game's. *)

(** {1 Driver} *)

type result = { winner : player_id option; turns : int; reason : string; moves : Game_record.move list }

val step : updater -> (updater -> unit) -> unit
(** One engine step, published with {!Engine.Updater.lift}. *)

val play_game :
  ?mana0:int list ->
  ?mana1:int list ->
  ?rng:Random.State.t ->
  ?bots:mover * mover ->
  game_desc ->
  max_turns:int ->
  verbose:bool ->
  result
(** HeadlessGame's turn loop. [bots] defaults to greedy vs greedy. [rng] feeds random card effects (see
    {!Engine.Updater.create}). *)
