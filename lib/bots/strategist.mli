(** Strategist bot: Summoner's search shaped by a tempo plan ([priv/sp/bot/strategist]). *)

open Sp

type settings = { mutable logging : bool; mutable stats_log : bool; mutable duration : int }

val default_settings : unit -> settings

type roles = { heal : float; damage : float; presence : float; mana : float }
type weights = roles

type profile = {
  is_spell : bool;
  attack : int;
  mana_attack : bool;
  life : int;
  cost : int;
  hasted : bool;
  multi : bool;
  invincible : bool;
  altar : bool;
}

val score_profile : profile -> string -> roles
val read_tempo : house:roles list -> known:roles list -> float * float

val for_tempo : float -> float -> float * int * int
(** [(gamma, max_depth, branch_top_k)] for tests. *)

val execute : settings -> pid:player_id -> desc:game_desc -> game_state -> command option
val mover : settings -> Game.mover
