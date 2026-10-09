(* Spell and ability visuals. The implementation lives in the [Fx_*] modules, one per section; this module
   exposes the interface in fx.mli. *)

type spot = Fx_core.spot = { x : float; y : float }
type t = Fx_core.t = { start : float; duration : float; draw : age:float -> unit }
type recipe = Fx_core.recipe = { duration : float; blocking : bool; make : start:float -> t }

type layout = Fx_core.layout = {
  slot_center : Mancer.Sp.player_id -> int -> spot;
  slot_floor : Mancer.Sp.player_id -> int -> spot;
  flame_base : Mancer.Sp.player_id -> int -> spot;
  life : Mancer.Sp.player_id -> spot;
  house : Mancer.Sp.player_id -> int -> spot;
  occupied : Mancer.Sp.player_id -> int list;
  scarecrow : Mancer.Sp.player_id -> int option;
  source_of : Mancer.Sp.command -> spot option;
  slot_house : Mancer.Sp.player_id -> int -> int option;
  slot_life : Mancer.Sp.player_id -> int -> int option;
  slot_card : Mancer.Sp.player_id -> int -> string option;
  house_cards : Mancer.Sp.player_id -> int -> spot list;
}

let set_transform = Fx_core.set_transform
let set_font = Fx_core.set_font
let load = Fx_core.load
let unload = Fx_core.unload
let spawn = Fx_core.spawn
let draw_all = Fx_core.draw_all
let set_scroll = Fx_core.set_scroll
let clear = Fx_core.clear
let slot_offset = Fx_core.slot_offset
let with_grey = Fx_core.with_grey
let with_night = Fx_core.with_night
let draw_sel = Fx_core.draw_sel

module Hover = Fx_particles.Hover

let victory = Fx_scenes.victory
let for_spell = Fx_stage.for_spell
let for_ability = Fx_stage.for_ability
let demo_names = Fx_stage.demo_names
let spawn_demo = Fx_stage.spawn_demo
