(** shadowmage special house. *)

val house : Sp.house

val is_hidden : Sp.slot_state -> bool
(** Scala [Shadowmage.isHidden]: Shade Stalker until it takes damage. *)

val visible_creatures : Sp.player_state -> int list
val is_hidden_from : Sp.slot_state -> owner:Sp.player_id -> viewer:Sp.player_id -> bool
