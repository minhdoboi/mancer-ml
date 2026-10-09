(* Shared spell looks, from SpellEffects.scala SpellLooks. Only keys that exist
   in the OCaml card list are kept, so the table and the houses stay in sync. *)

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

let look style aim r g b ?(power = 1.) () = { style; aim; r; g; b; power }

let looks =
  [
    ("earth.shower", look Stones All_slots 0.4 0.36 0.3 ());
    ("earth.therapy", look Sprout Own_life 0.2 0.75 0.2 ());
    ("earth.fury", look Eyes Own_life 1. 0.55 0.05 ());
    ("water.meditate", look Rise Own_life 0.35 0.7 1. ());
    ("Conscription", look Rise Own_life 0.95 0.8 0.35 ());
    ("air.twister", look Swirl Target 0.75 0.9 1. ());
    ("wind.Simoom.name", look Swirl Foe_slots 0.9 0.72 0.4 ());
    ("wind.Squall.name", look Swirl Foe_slots 0.7 0.88 1. ());
    ("wind.Whirlwind.name", look Swirl Foe_slots 0.8 0.92 1. ~power:1.2 ());
    ("wind.EnergeticVortex.name", look Swirl Foe_slots 0.55 0.75 1. ~power:1.25 ());
    ("wind.EternalStorm.name", look Swirl Foe_slots 0.65 0.8 1. ~power:1.35 ());
    ("disciples.wind", look Swirl Foe_slots 0.55 0.5 0.45 ());
    ("fire.apocalypse", look Fireburst All_slots 1. 0.4 0.05 ~power:1.35 ());
    ("NuclearMissile", look Fireburst Foe_slots 1. 0.55 0.15 ~power:1.5 ());
    ("faerie.cloud", look Rise Own_slots 0.7 0.85 1. ());
    ("zen.focus", look Rise Target 0.35 0.9 0.55 ());
    ("chronomaster.aging", look Shadow Target 0.28 0.26 0.24 ~power:1.15 ());
    ("chronomaster.freeze", look Ice Target 0.7 0.9 1. ~power:1.1 ());
    ("chronomaster.accelerate", look Rays Own_target 1. 0.78 0.28 ~power:1.15 ());
    ("chronomaster.groundhog", look Ring All_slots 0.45 0.85 0.9 ~power:1.2 ());
    ("disciples.drug", look Rise Own_slots 0.5 0.95 0.3 ());
  ]

let custom =
  [
    "air.call";
    "air.lightnin";
    "air.chain";
    "wind.BallLightning.name";
    "fire.wave";
    "fire.inferno";
    "disciples.madden";
    "earth.care";
    "faerie.fire";
    "water.shower";
  ]

let get key = List.assoc_opt key looks
let covered key = Option.is_some (get key) || List.mem key custom
