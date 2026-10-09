(** Spell and ability visuals for the raylib GUI, ported from the Scala effects. *)

open Mancer
open Sp

type spot = { x : float; y : float }
type t = { start : float; duration : float; draw : age:float -> unit }
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
}

val set_transform : scale:float -> off_x:float -> off_y:float -> unit
val load : unit -> unit
val unload : unit -> unit
val spawn : t -> unit
val draw_all : now:float -> unit
val for_spell : layout -> command -> recipe option

val for_ability :
  layout -> kind:string -> player:player_id -> num:int -> target:(player_id * int) option -> recipe option

val slot_offset : player_id -> int -> float * float
val with_grey : bool -> (unit -> unit) -> unit
val draw_sel : x:float -> y:float -> w:float -> h:float -> unit
val victory : layout -> winner:player_id option -> you:bool -> unit

module Hover : sig
  val show : x:float -> y:float -> unit
  val hide : unit -> unit
  val draw : unit -> unit
end

val demo_names : string list
val spawn_demo : layout -> string -> unit
