(** warp special house. *)

val house : Sp.house

val bridle : Sp.slot_state -> Sp.slot_update -> unit
(** Silences the creature: [st] must already be off the slot (Scala [Warp.bridle]). *)

val unbridle : Sp.slot_update -> unit
