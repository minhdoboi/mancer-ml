(** antimancer special house. *)

val house : Sp.house

val cost_fn : int -> int
(** Antimancer costs run from 2 to 9 (Scala [initCards { i => i + 2 }]). *)
