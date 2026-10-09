(** Card constructors and reaction helpers shared by houses (priv/sp/house/Common.scala,
    priv/sp/house/package.scala). *)

open Sp

(** {1 Cards} *)

val attack : int -> attack_sources

val mana_attack : int -> attack_sources
(** Attack equal to the owner's mana in the given house. *)

val add_source : attack_sources -> attack_source -> attack_sources
val normal_run_attack : run_attack
val multi_run_attack : run_attack
val never_run_attack : run_attack

val creature :
  key:string ->
  attack:attack_sources ->
  life:int ->
  description:string ->
  ?effects:phase_effects ->
  ?input_spec:input_spec ->
  ?mod_:mod_ ->
  ?new_reaction:(unit -> reaction) ->
  ?run_attack:run_attack ->
  ?is_altar:bool ->
  ?status:int ->
  unit ->
  card

val spell :
  key:string -> description:string -> ?input_spec:input_spec -> effects:phase_effects -> unit -> card

(** {1 Attack sources} *)

val same_source : attack_source -> attack_source -> bool
(** Scala [==] on attack sources: by key for {!Sp.Attack_bonus}, by value for the plain data cases, physical
    equality for closures. *)

val add_attack : slot_update -> attack_source -> unit
(** No-op on creatures whose printed attack is 0, as in Scala. *)

val remove_first_attack : slot_update -> attack_source -> unit

(** {1 Reactions} *)

val attack_bonus_reaction : cond:(int -> int -> bool) -> get_bonus:(int -> attack_source) -> unit -> reaction
(** [cond pos num]: does the creature at [pos] buff slot [num]? [get_bonus pos] should return an
    {!Sp.Attack_bonus} keyed by [pos]. *)

val mana_growth_reaction : owner:bool -> amount:int -> house_indexes:int list -> unit -> reaction
