(** Oracle bot: sequential-halving rollouts ([priv/sp/bot/oracle/OracleBot.scala]). *)

open Sp

type settings = { mutable logging : bool; mutable stats_log : bool; mutable duration : int }

val default_settings : unit -> settings

module Params : sig
  val root_cards : int ref
  val horizon : int ref
  val placements_per_card : int ref
  val reply_weights : float array ref
  val first_round_samples : int ref
  val project_plies : int ref
  val win_discount : float ref
  val screen_weight : float ref
end

module Eval : sig
  val score : projected:game_state -> before:game_state -> bot_id:player_id -> float
  (** Smooth leaf in (-1, 1), bot minus opponent. [projected] is [before] after both sides passed. *)
end

val base_moves : desc:game_desc -> player_id -> game_state -> command option list
(** Pass plus at most [Params.placements_per_card] placements per playable card. *)

val leaf_value : desc:game_desc -> ?bot_id:player_id -> game_state -> float
(** Engine projection of [projectPlies] passes, opponent to move, then {!Eval.score}. *)

val execute : settings -> pid:player_id -> desc:game_desc -> game_state -> command option

val mover : settings -> Game.mover
(** Snapshot [updater.state] and search. The live game is not mutated. *)
