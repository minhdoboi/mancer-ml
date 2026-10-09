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
    ("earth.fury", look Eyes Own_life 1. 0.55 0.05 ());
    ("water.meditate", look Rise Own_life 0.35 0.7 1. ());
    ("Conscription", look Rise Own_life 0.95 0.8 0.35 ());
    ("wind.Simoom.name", look Swirl Foe_slots 0.9 0.72 0.4 ());
    ("wind.Squall.name", look Swirl Foe_slots 0.7 0.88 1. ());
    ("wind.Whirlwind.name", look Swirl Foe_slots 0.8 0.92 1. ~power:1.2 ());
    ("wind.EnergeticVortex.name", look Swirl Foe_slots 0.55 0.75 1. ~power:1.25 ());
    ("wind.EternalStorm.name", look Swirl Foe_slots 0.65 0.8 1. ~power:1.35 ());
    ("disciples.wind", look Swirl Foe_slots 0.55 0.5 0.45 ());
    ("NuclearMissile", look Fireburst Foe_slots 1. 0.55 0.15 ~power:1.5 ());
    ("faerie.cloud", look Rise Own_slots 0.7 0.85 1. ());
    ("zen.focus", look Rise Target 0.35 0.9 0.55 ());
    ("chronomaster.aging", look Shadow Target 0.28 0.26 0.24 ~power:1.15 ());
    ("chronomaster.freeze", look Ice Target 0.7 0.9 1. ~power:1.1 ());
    ("chronomaster.accelerate", look Rays Own_target 1. 0.78 0.28 ~power:1.15 ());
    ("chronomaster.groundhog", look Ring All_slots 0.45 0.85 0.9 ~power:1.2 ());
    ("disciples.drug", look Rise Own_slots 0.5 0.95 0.3 ());
    ("snowblood.echo", look Ring Own_slots 0.6 0.85 1. ());
    ("antimancer.3.name", look Ring Own_slots 0.4 0.85 1. ());
    ("antimancer.9.name", look Ring Foe_slots 0.8 0.45 1. ());
    ("powermancer.stasis", look Ice Foe_slots 0.65 0.8 1. ~power:1.1 ());
    ("powermancer.rainbow", look Rays Own_slots 0.95 0.55 1. ~power:1.15 ());
    ("battlemage.taunt", look Ring Target 0.95 0.45 0.15 ~power:1.1 ());
    ("battlemage.warcry", look Fireburst Foe_slots 0.95 0.65 0.2 ~power:1.2 ());
    ("colors.Discoloration.name", look Ring All_slots 0.9 0.9 0.95 ());
    ("rogue.sneak", look Shadow Target 0.25 0.35 0.2 ~power:1.1 ());
    ("rogue.trap", look Ice Foe_slots 0.45 0.55 0.85 ());
    ("entomologist.locust", look Drops Target 0.6 0.75 0.2 ~power:1.3 ());
    ("entomologist.hivemind", look Ring Target 0.8 0.9 0.3 ());
    ("soulbinder.summon", look Ring Target 0.7 0.85 1. ());
    ("soulbinder.release", look Rise Own_slots 0.4 0.95 0.7 ());
    ("tinker.plant", look Drops Target 0.75 0.5 0.18 ());
    ("tinker.scatter", look Fireburst Foe_slots 1. 0.5 0.08 ~power:1.15 ());
    ("shadowcaster.scythe", look Strike Foe_slots 0.55 0.08 0.12 ~power:1.2 ());
    ("spiritshaman.awakening", look Rise Own_slots 0.55 0.9 0.45 ~power:1.15 ());
    ("shadowmage.spike", look Spikes Foe_slots 0.92 0.08 0.1 ~power:1.2 ());
    ("conjurer.nectar", look Rise Target 0.72 0.5 0.95 ());
    ("conjurer.bravery", look Rise Own_slots 0.95 0.75 0.28 ());
    ("conjurer.nurture", look Rise Own_slots 0.35 0.85 0.4 ());
    ("conjurer.pain", look Shadow Target 0.55 0.12 0.35 ~power:1.1 ());
    ("quantumancer.surge", look Rays Foe_slots 0.55 0.4 1. ~power:1.2 ());
    ("seasons.SolarWave.name", look Rays All_slots 1. 0.8 0.3 ());
    ("seasons.EternalSummer.name", look Rays Own_slots 1. 0.7 0.25 ~power:1.2 ());
    ("seasons.TheGreatAurora.name", look Rays All_slots 0.4 1. 0.75 ~power:1.2 ());
    ("reaper.furious", look Strike Foe_slots 0.7 0.15 0.9 ());
    ("reaper.damnation", look Shadow Target 0.45 0.1 0.7 ());
    ("reaper.onslaught", look Shadow Foe_slots 0.4 0.1 0.65 ());
    ("reaper.sentence", look Shadow All_slots 0.35 0.08 0.55 ~power:1.2 ());
    ("reaper.rage", look Shadow Target 0.6 0.1 0.8 ());
    ("reaper.tribute", look Shadow Target 0.4 0.12 0.6 ());
    ("reaper.shackles", look Ring Target 0.45 0.2 0.75 ());
    ("reaper.passage", look Rise Target 0.35 0.9 0.55 ());
    ("sower.devouring", look Shadow Own_slots 0.3 0.55 0.2 ());
    ("sower.pollination", look Rise Target 0.85 0.9 0.35 ());
    ("shaman.hunger", look Rise Own_target 0.9 0.25 0.15 ());
    ("elementalist.freeze", look Ice All_slots 0.7 0.9 1. ());
    ("elementalist.incineration", look Fireburst All_slots 1. 0.28 0.02 ());
    ("highpriest.Simoom", look Swirl Foe_slots 0.9 0.7 0.35 ());
    ("highpriest.Curseofchaos", look Fireburst Target 0.95 0.25 0.1 ());
    ("highpriest.EyeofWajet", look Rays All_slots 1. 0.85 0.4 ());
    ("kinetician.force", look Ring Target 0.4 0.75 1. ());
    ("kinetician.warp", look Ring Target 0.6 0.4 1. ());
    ("kinetician.barrier", look Ring Target 0.35 0.7 1. ~power:1.15 ());
  ]

let custom =
  [
    "shadowmage.eclipse";
    "elementalist.avalanche";
    "kinetician.call";
    "shadowcaster.wave";
    "air.call";
    "air.lightnin";
    "air.chain";
    "air.twister";
    "wind.BallLightning.name";
    "fire.wave";
    "fire.inferno";
    "disciples.madden";
    "earth.care";
    "earth.therapy";
    "faerie.fire";
    "water.shower";
    "junk.poison";
    "limbo.nether";
    "limbo.redemption";
    "powermancer.equilibrium";
    "sower.tangling";
    "fire.apocalypse";
    "powermancer.burial";
    "powermancer.bargain";
    "dreamweaver.aurora";
    "shaman.fury";
    "shaman.moon";
    "vampires.flock";
    "vampires.ties";
    "warp.quake";
    "elementalist.frost";
    "darkpriest.BlackMass.name";
  ]

let get key = List.assoc_opt key looks
let covered key = Option.is_some (get key) || List.mem key custom
