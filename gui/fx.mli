(** Spell and ability visuals for the raylib GUI, ported from the Scala effects. *)

open Mancer
open Sp

type spot = { x : float; y : float }
type t = { start : float; duration : float; under : bool; draw : age:float -> unit }
type recipe = { duration : float; blocking : bool; make : start:float -> t }

type layout = {
  slot_center : player_id -> int -> spot;
  slot_floor : player_id -> int -> spot;
  flame_base : player_id -> int -> spot;
  life : player_id -> spot;
  house : player_id -> int -> spot;
  occupied : player_id -> int list;
  scarecrow : player_id -> int option;
  source_of : command -> spot option;
  slot_house : Mancer.Sp.player_id -> int -> int option;
  slot_life : Mancer.Sp.player_id -> int -> int option;
  slot_card : Mancer.Sp.player_id -> int -> string option;
  house_cards : Mancer.Sp.player_id -> int -> spot list;
  extreme_house : Mancer.Sp.player_id -> [ `Highest | `Lowest ] -> int;
}

val set_transform : scale:float -> off_x:float -> off_y:float -> unit
val set_font : Raylib.Font.t -> unit
val load : unit -> unit
val unload : unit -> unit
val spawn : t -> unit

val draw_underlay : now:float -> unit
(** Draws the effects flagged [t.under], for the frame pass that runs before the
    cards, so they sit behind the board instead of over it. *)
val draw_all : now:float -> unit

val set_scroll : float -> unit
(** The board's current vertical scroll; effects follow it from where they spawned. *)
val clear : unit -> unit

(** Drops every overlay effect and shake, e.g. when a rematch starts. *)
val for_spell : layout -> command -> recipe option

val sound_delay : string -> float
(** Seconds after a spell's effect starts that its sound begins (0 for most spells). *)


val for_ability :
  layout -> kind:string -> player:player_id -> num:int -> target:(player_id * int) option -> recipe option

val slot_offset : player_id -> int -> float * float

val add_portrait_shake : player_id -> start:float -> duration:float -> strength:float -> unit
val portrait_offset : player_id -> float * float
(** A portrait's shake, as [slot_offset] gives it for a creature. *)
val with_grey : bool -> (unit -> unit) -> unit

val with_night : float -> (unit -> unit) -> unit
(** Draws [f] through the campaign night shader when [amount] is above a hair. *)

val draw_sel : x:float -> y:float -> w:float -> h:float -> unit
val victory : layout -> winner:player_id option -> you:bool -> start:float -> unit

module Hover : sig
  val show : x:float -> y:float -> unit
  val hide : unit -> unit
  val draw : unit -> unit
end

val demo_names : string list
val spawn_demo : layout -> string -> unit

val set_vortex_spots : (spot * float) list -> unit
(** Centres and radii of the Energetic Vortex icons to draw this frame (the board refreshes them every frame). *)

val vortex_badge : layout -> player_id -> spot * float
(** Where a player's lasting vortex hangs: the corner of their portrait. *)

val set_powder_spots : occupied:spot list -> spot list -> unit
(** Slot centres to cover in golden powder this frame (creatures hit by Solar Wave's triple damage).
    [occupied] lists every occupied slot, so the powder of a creature that died vanishes at once. *)

val warp_any : unit -> bool
(** Whether any creature has been under Warp reality recently (a cheap check before [warp_amount]). *)

val warp_amount : spot -> float
(** How strongly Warp reality bends the creature at this slot centre right now (0 when it does not). *)

val dissolve_state : spot -> (float * float) option
(** Obliterate: how far the creature's card at this slot centre has been eaten away (0..1) and its noise
    seed, while the effect is eating it; [None] when no effect is. *)

val dissolve_shader : unit -> Raylib.Shader.t option
val dissolve_uniforms : amount:float -> seed:float -> unit
(** The dissolve shader and its uniforms, as [warp_shader] and [warp_uniforms]. *)

val warp_shader : string -> Raylib.Shader.t option
(** The named effect shader, if it loaded. *)

val warp_uniforms : amount:float -> unit
(** Set the warp shader's strength and clock before drawing with it. *)

val set_trap_spots : (int * spot * float) list -> unit
(** Centres and radii of the Arcane Trap pentagrams to draw this frame, keyed by the trapped player. *)

val set_frost_spots : (int * spot * float) list -> unit
(** Centres and half-sizes of the portraits to coat in Deep Freeze frost this frame, keyed by player. *)

val trap_center : layout -> player_id -> spot * float
(** Where a player's lasting pentagram sits: the middle of their slot row. *)
