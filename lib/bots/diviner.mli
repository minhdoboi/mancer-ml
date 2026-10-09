(** Diviner bot: Oracle playouts inside several sampled opponent decks
    ([priv/sp/bot/diviner/DivinerBot.scala]). *)

open Sp

type settings = { mutable logging : bool; mutable stats_log : bool; mutable duration : int }

val default_settings : unit -> settings

module Params : sig
  val belief_count : int ref
  val risk_aversion : float ref
  val first_round_samples : int ref
end

module Eval : sig
  val world_mean : screen:float -> sum:float -> n:int -> float

  val score : ?aversion:float -> float list -> float
  (** Mean minus a penalty toward the worst world. *)
end

val execute : ?seen:card list -> settings -> pid:player_id -> desc:game_desc -> game_state -> command option
(** [seen] are opponent cards already revealed; every sampled belief contains them. [desc] is used as given
    for the bot's own deck; the opponent's is never searched. *)

val mover : settings -> Game.mover
