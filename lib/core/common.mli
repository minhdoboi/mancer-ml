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
val custom_run_attack : (int list -> damage -> player_update -> unit) -> run_attack

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
  ?data:slot_data ->
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
val remove_any_attack : slot_update -> attack_source -> unit
val has_source : slot_update -> (attack_source -> bool) -> bool

val drain : slot_update -> damage -> unit
(** Scala [SlotUpdate.drain]: unblockable life loss, no protect/selfProtect. *)

val has_desc_mod : player_update -> desc_mod -> bool
(** Physical equality: houses keep one value per mod. *)

val remove_all_desc_mod : player_update -> desc_mod -> unit
val hide_spell_mod : desc_mod
val hide_creature_mod : desc_mod
val skip_turn_mod : desc_mod

val focus_slot : ?blocking:bool -> slot_update -> unit
(** Scala [SlotUpdate.focus]. *)

val remove_invincible : int -> card_effect
(** Scala [RemoveInvincible(slotId)]. *)

val remove_attack : attack_source -> card_effect
(** Scala [RemoveAttack]: removes the source from every creature of the owner. *)

val single_target_attack : int list -> damage -> player_update -> bool
(** Scala [SingleTargetAttack.attack]: the normal attack, true if it killed a creature. *)

val damage_creature_and_get : int -> damage -> player_update -> int
(** Scala [damageCreatureAndGet]: hits the opposite creature only, 0 if empty. *)

val damage_and_get : int -> damage -> player_update -> int
(** Scala [DamageAttack.damageAndGet]: hits the opposite slot (or the player), returns the damage done. *)

val init_owner_death : player_update -> unit
(** Scala [OwnerDeathEventListener.init]: when one of [p]'s creatures dies, its other special creatures get
    [on_death]. *)

val init_opp_death : player_update -> unit
(** Scala [OppDeathEventListener.init]: same, for deaths on the opponent's side. *)

val init_any_death : player_update -> unit
(** Scala [AnyDeathEventListener.init]: both sides. *)

val slot_interval : int -> int -> int list
val reduce_filled : slots_update -> (slot_update -> slot_update -> slot_update) -> slot_update option
val lowest_life : slot_update -> slot_update -> slot_update
val highest_life : slot_update -> slot_update -> slot_update
val strongest : slot_update -> slot_update -> slot_update
val nearest_empty : int -> player_update -> int option
val non_special : player_id -> game_state -> int list
val countdown : int -> card_effect -> card_effect
val ability : from_card:card -> to_card:card -> desc_mod
val incr_basic_cost_mod : desc_mod
val incr_fire_cost_mod : desc_mod
val hide_basic_mod : desc_mod
val hide_special_mod : desc_mod

(** {1 Reactions} *)

val attack_bonus_reaction : cond:(int -> int -> bool) -> get_bonus:(int -> attack_source) -> unit -> reaction
(** [cond pos num]: does the creature at [pos] buff slot [num]? [get_bonus pos] should return an
    {!Sp.Attack_bonus} keyed by [pos]. *)

val mana_growth_reaction : owner:bool -> amount:int -> house_indexes:int list -> unit -> reaction
