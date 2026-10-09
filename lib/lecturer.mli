(** Lecturer bot: UCT Monte-Carlo tree search ([priv/sp/bot/lecturer/LecturerBot.scala]). *)

open Sp

type settings = {
  mutable logging : bool;
  mutable stats_log : bool;
  mutable duration : int;
  mutable rng : Random.State.t;
}

val default_settings : unit -> settings

module Params : sig
  val exploration : float ref
  val max_depth : int ref
  val prior_visits : float ref
  val rollout_plies : int ref
  val project_plies : int ref
  val pass_weight : float ref
end

type node
(** One tree node: the position after [move], valued from the side of the player who played it. *)

val search : settings -> pid:player_id -> desc:game_desc -> game_state -> node
(** Run UCT until the budget is spent and return the root. *)

val root_children : node -> (command option * int * float) list
(** [(move, visits, mean)] for each root move, mean from the bot's side. *)

val execute : settings -> pid:player_id -> desc:game_desc -> game_state -> command option

val mover : settings -> Game.mover
(** Snapshot [updater.state] and search. The live game is not mutated. *)
