(** Summoner bot: iterative-deepening best-first settle ([priv/sp/bot/summoner/SummonerBot.scala]). *)

open Sp

type settings = {
  mutable logging : bool;
  mutable stats_log : bool;
  mutable duration : int;
  mutable heuris : string;  (** ["race"] (default) or ["lifemana"]. *)
}

val default_settings : unit -> settings
val execute : settings -> pid:player_id -> desc:game_desc -> game_state -> command option
val mover : settings -> Game.mover
