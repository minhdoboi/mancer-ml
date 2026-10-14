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
  extreme_house : Mancer.Sp.player_id -> [ `Highest | `Lowest ] -> int;
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
let sound_delay = Fx_stage.sound_delay
let for_ability = Fx_stage.for_ability
let demo_names = Fx_stage.demo_names
let spawn_demo = Fx_stage.spawn_demo

let set_vortex_spots spots = Fx_core.vortex_spots := spots

let vortex_badge (layout : layout) pid = Fx_dedicated.vortex_badge_of (layout.life pid)

let set_powder_spots ~occupied spots =
  Fx_core.powder_spots := spots;
  Fx_core.powder_occupied := occupied

let warp_amount = Fx_core.warp_amount
let warp_any = Fx_core.warp_any
let warp_shader name = Option.map (fun s -> s.Fx_core.sh) (Fx_core.load_named name)
let warp_uniforms ~amount =
  Option.iter
    (fun s ->
      Fx_core.set_float s "u_amount" amount;
      Fx_core.set_float s "u_time" (Raylib.get_time ()))
    (Fx_core.load_named "warp")

let set_trap_spots spots = Fx_core.trap_spots := spots

let trap_center = Fx_core.trap_center_of
