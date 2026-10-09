(** meteomancer special house. *)

val house : Sp.house
val fateweaver_phase : string
val is_meteo : Sp.card -> bool
val with_called_choices : Sp.command -> Sp.command list
