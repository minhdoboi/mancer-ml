(** seasons special house. *)

val house : Sp.house
val phase1 : string
val phase2 : string

(** The four passives the Seasons cards grant.  Spring and Winter live as
    effects on the player, Summer in the Season_data, Autumn as a desc mod. *)
type season_passive = Spring | Summer | Autumn | Winter

(** The passives that are still active for [p]: one turns off the moment the
    matching card is played.  Empty when [p] does not own the Seasons house,
    so a Warp that proxied the listener shows nothing. *)
val active_passives : Sp.player_state -> season_passive list
