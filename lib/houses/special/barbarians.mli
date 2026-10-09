(** Special house Barbarians (by DonnieDarkcore): the lower the owner's life, the harder they hit. *)

val house : Sp.house

val cost_fn : int -> int
(** Deck costs, by card order. *)

val life_threshold : string -> int option
(** For a Barbarians card key, the owner's life total at or below which the
    card's second face opens; [None] for cards without a life condition. *)
