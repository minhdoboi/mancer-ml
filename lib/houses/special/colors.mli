(** colors special house. *)

val house : Sp.house

val cost_fn : int -> int
(** Colors costs are 0,1,2,2,3,4,4,4 (Scala [Map(0 -> 0, …)]). *)

val discolor_phase : string
