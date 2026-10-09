(** Special house Plaguebringers (by LordVanmaru): the Disease mechanic.

    A disease counter lives in the infecting player's data, keyed by the
    infected creature's [slot_id]. *)

val house : Sp.house

val counters_on : Sp.game_state -> int -> int
(** The disease counters on the board slot with this [slot_id], summed over
    the players (a creature's counters are its opponent's entries). *)
