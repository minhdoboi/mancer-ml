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

val execute : settings -> pid:player_id -> desc:game_desc -> game_state -> command option
val mover : settings -> Game.mover
