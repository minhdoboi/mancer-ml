(** Spell visual table, ported from Scala [SpellLooks].

    Styles and aims are data, not code. [looks] covers the shared procedural styles; [custom] is every spell
    with a dedicated effect in [Fx.Cast] (lightning, particles, flames, Madden, …). *)

type style =
  | Stones
  | Sprout
  | Eyes
  | Rise
  | Strike
  | Swirl
  | Ice
  | Shadow
  | Rays
  | Drops
  | Fireburst
  | Ring
  | Spikes

type aim = All_slots | Foe_slots | Own_slots | Own_life | Target | Own_target
type look = { style : style; aim : aim; r : float; g : float; b : float; power : float }

val looks : (string * look) list
val custom : string list
val get : string -> look option
val covered : string -> bool
