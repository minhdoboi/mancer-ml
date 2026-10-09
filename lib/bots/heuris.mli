(** Who is ahead if the creatures already on the board keep attacking.
    Port of [RaceHeuris] in [priv/sp/bot/Heuristic.scala]. *)

open Sp

type fighter = {
  mutable life : int;
  attack : int;
  mutable active : bool;
  multi : bool;
  invincible : bool;
  wakes : bool;
}

val horizon : int

val score : bot_life:int -> human_life:int -> bot:fighter option array -> human:fighter option array -> bot_to_move:bool -> float
(** Forecast of a few combat swings in [-1, 1]. Immediate lethal is ±1. The
    arrays are mutated (lives and wake flags). *)

val long_term :
  bot:house_state array ->
  human:house_state array ->
  bot_attack:float ->
  human_attack:float ->
  bot_board:float ->
  human_board:float ->
  float

val mana_lead : bot:house_state array -> human:house_state array -> float
val blend : race:float -> bot:house_state array -> human:house_state array -> ?bot_attack:float -> ?human_attack:float -> ?bot_board:float -> ?human_board:float -> unit -> float

val of_state : bot_id:player_id -> to_move:player_id -> game_state -> float
(** [RaceHeuris.apply]: the short race, plus long-term terms when it is even. *)

val life_mana : bot_id:player_id -> start:game_state -> game_state -> float
(** [LifeManaHeuris]: life / mana / board mix in [0, 1], relative to [start]. *)

val synergy : weights:float * float * float * float -> bot_id:player_id -> game_state -> float
(** Role-weighted board + mana + sustain, bot minus opponent. Weights are
    [(heal, damage, presence, mana)]. *)
