(** A live game driven one move at a time (HeadlessGame's turn loop, Tournament.scala). This is the API a UI or
    a bot loop uses: create a session, read {!status}, {!submit} a move, and watch the event stream.

    Events: [on_event] receives every {!Sp.game_event}. The engine events mirror Scala's [UpdateListener];
    the session adds {!Sp.Ev_to_move}, {!Sp.Ev_submitted} and {!Sp.Ev_game_over}. Listeners run mid-step: read
    the board with {!peek}, not {!state}, and never call {!submit} from one. Bot simulations build their own
    updaters and emit nothing. *)

open Sp

type t
type status = To_move of player_id | Over of { winner : player_id option; reason : string }

val create :
  ?mana0:int list ->
  ?mana1:int list ->
  ?rng:Dice.t ->
  ?max_turns:int ->
  ?on_event:(game_event -> unit) ->
  game_desc ->
  t
(** Deals the opening state, runs [On_start] effects and announces the first player to move ([owner]).
    [max_turns] caps the number of moves (default unlimited); [rng] is as in {!Engine.Updater.create}. *)

val status : t -> status
(** [Over] once a player is dead ([reason = "ended"]) or [max_turns] moves were played ([reason = "maxTurns"]). *)

val legal_commands : t -> command list
(** The player to move's legal commands, cheapest first; [[]] when the game is over. Passing ([None]) is always
    legal. *)

val submit : t -> command option -> unit
(** Plays the move for the player to move, then resolves it: attacks and the end-of-turn chain, or a hand-over
    when a card queued a [Wait_player]. Raises [Invalid_argument] when the game is over or the command is for
    the other player. *)

val state : t -> game_state
(** The board between moves. Live, not a copy. *)

val peek : t -> game_state
(** A copy of the board as it stands, safe inside a listener ({!Engine.Updater.peek}). *)

val turns : t -> int
(** Moves played so far. *)

val moves : t -> Game_record.move list
(** Moves played so far, oldest first, in the tournament record format. *)

val updater : t -> updater
val desc : t -> game_desc

val trace_line : game_event -> string option
(** The [-v] trace line for an event, for the event kinds the trace has always shown (summon, spell, focus,
    refill). *)

(** {1 Lower level} *)

val start : updater -> unit
(** Runs both players' [On_start] effects. {!create} does this. *)

val drain : updater -> player_id -> player_id
(** Resolves what [pid]'s submit queued and returns who moves next. {!submit} does this; {!Replay} uses it to
    replay records that name the player of each move. *)

(** {1 Headless driver} *)

type result = { winner : player_id option; turns : int; reason : string; moves : Game_record.move list }

val play_game :
  ?mana0:int list ->
  ?mana1:int list ->
  ?rng:Dice.t ->
  ?bots:Game.mover * Game.mover ->
  game_desc ->
  max_turns:int ->
  verbose:bool ->
  result
(** Plays a whole game between two bots on a {!t}. [bots] defaults to greedy vs greedy; a player with no
    playable card passes without asking its bot. [verbose] prints {!trace_line}s. Shuffle deals stay on
    [Random.State]; only in-game dice use [Dice]. *)
