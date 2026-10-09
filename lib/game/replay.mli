(** Replay a recorded game and dump board snapshots for the Scala/OCaml compare harness. *)

open Sp

type occupant = { card_key : string; life : int; attack : int; status : int }
type side = { life : int; mana : int list; growth : int list; slots : occupant option array }
type snap = { t : int; player : int option; after : string; ended : int option; sides : side array }
type mismatch = { t : int; message : string }

val of_state : t:int -> ?player:int -> after:string -> ended:player_id option -> game_state -> snap
val desc_of : Catalog.t -> Game_record.parsed -> game_desc
val run : ?cat:Catalog.t -> Game_record.parsed -> snap list
val generate : ?special:string -> ?max_turns:int -> seed:int -> unit -> Game_record.t
val snaps_to_json : snap list -> string
val snaps_of_json : string -> snap list
val diff : snap list -> snap list -> mismatch option
