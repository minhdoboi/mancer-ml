(** House and card lookup for recorded games. *)

open Sp

type t

type mode = Standard | Classic
(** Which set of basic houses a game is dealt from. Classic is fire/water/air/earth; standard swaps fire for anger. *)

val classic_basics : house list
val standard_basics : house list
val basics : house list
(** The classic set, kept for replaying recorded games. *)
val basics_of : mode -> house list
val string_of_mode : mode -> string
val mode_of_string : string -> mode
(** Unknown names read as [Standard]. *)
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
