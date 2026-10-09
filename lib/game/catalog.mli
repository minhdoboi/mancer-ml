(** House and card lookup for recorded games. *)

open Sp

type t

val basics : house list
val specials : house list
val all : house list
val init : unit -> unit
val create : house list -> t
val default : unit -> t
val same_house : house -> string -> bool
val same_card : card -> string -> bool
val find_house : t -> string -> house
val find_card : t -> string -> card
val find_card_in_house : house -> string -> card
val find_special : string -> house
val token : string -> string
