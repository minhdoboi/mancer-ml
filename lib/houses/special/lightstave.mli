(** Special house Lightstaves (by Redrook): Auras around creatures. *)

val house : Sp.house

val auras : Sp.player_state -> Sp.aura_state list
(** The active Auras of a player state, oldest first. *)

val aura_color : string -> int * int * int * int
(** The outline/badge colour of an Aura, by card key. *)
