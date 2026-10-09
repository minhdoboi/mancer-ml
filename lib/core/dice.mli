(** 48-bit LCG matching [java.util.Random] / [scala.util.Random]. Card effects on both engines draw from this
    so a seeded game and its Scala replay see the same dice. *)

type t

val make : int -> t
val make64 : int64 -> t
val copy : t -> t
val make_self : unit -> t

val next : t -> int -> int
(** [next r bits] is [java.util.Random.next(bits)]. *)

val int : t -> int -> int
(** [int r n] is [java.util.Random.nextInt(n)]. [n] must be positive. *)

val shuffle : t -> 'a list -> 'a list
(** [scala.util.Random.shuffle]. *)
