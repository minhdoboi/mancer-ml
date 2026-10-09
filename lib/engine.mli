(** The update layer: what house files and the game driver may use.

    Mirrors priv/sp/update/*.scala and priv/sp/GameCardEffect.scala. The update graph is mutated in place; see
    {!Updater.lift}. *)

open Sp

val events_enabled : bool ref
(** When set, engine events (summons, spells, focus) are printed. *)

val event : ('a, unit, string, unit) format4 -> 'a
val other_player : player_update -> player_update

val make_creature : card -> creature
(** Fails if the card is a spell. *)

(** {1 Effect environment} *)

val mk_env : player_id -> updater -> env
val ctx_of_env : env -> context
val player_of_env : env -> player_update
val other_of_env : env -> player_update
val get_mana : env -> int -> int

(** {1 Update graph} *)

(** HousesUpdate *)
module Houses : sig
  val incr_mana_all : houses_update -> unit
  (** Adds each house's growth: the start-of-turn mana gain. *)

  val incr_mana : houses_update -> int -> int list -> unit
  val incr_growth : houses_update -> int -> int list -> unit
end

(** SlotUpdate *)
module Slot : sig
  val get : slot_update -> slot_state
  (** Fails on an empty slot. *)

  val adjacent : slot_update -> slot_update list
  val filled_adjacent : slot_update -> slot_update list
  val opposite : slot_update -> slot_update
  val update : slot_update -> (slot_state -> slot_state) -> unit
  val toggle : slot_update -> int -> unit
  val toggle_off : slot_update -> int -> unit
  val toggle_run : slot_update -> unit
  val is_runnable : slot_state -> bool
  val overwrite_add : slot_update -> slot_state -> unit

  (** {2 Interception points}
      These dispatch to the occupant's reaction; use them from card code. *)

  val heal : slot_update -> int -> unit

  val inflict : slot_update -> damage -> unit
  (** Applies the player damage mod, then the reaction's [inflict]. *)

  val destroy : slot_update -> unit

  (** {2 Default actions}
      What {!Reaction.default} does. Calling these directly bypasses the reaction. *)

  val add : slot_update -> slot_state -> unit
  val remove : slot_update -> dead option -> slot_state
  val do_heal : slot_update -> int -> unit
  val do_stun : slot_update -> unit
  val do_priv_destroy : slot_update -> unit
  val delayed_destroy : slot_update -> damage -> unit
  val damage_slot : slot_update -> damage -> unit
end

(** SlotsUpdate *)
module Slots : sig
  val foreach : slots_update -> (slot_update -> unit) -> unit
  (** Iterates filled slots only. *)

  val foldl : slots_update -> 'a -> ('a -> slot_update -> 'a) -> 'a
  val get_open : slots_update -> int list
  val inflict_creatures : slots_update -> damage -> unit
  val current_attack : slot_update -> slot_state -> int

  val summon : slots_update -> int -> creature -> int
  (** Returns the new slot id. Summoning over an occupied slot calls the new reaction's [on_spawn_over]. *)

  val toggle_run : slots_update -> unit
  val heal_creatures : slots_update -> int -> unit
  val find_card : slots_update -> card -> slot_update option
  val find_slot_id : slots_update -> int -> slot_update option
  val finish_dead : slots_update -> dead -> unit
end

(** PlayerUpdate *)
module Player : sig
  val other : player_update -> player_update

  val mod_damage : player_update -> damage -> damage
  (** Applies the attacker's SpellMods to spell damage aimed at this player. *)

  val flush : player_update -> unit
  val refresh_slots : player_update -> unit
  val sync_houses : player_update -> unit
  val inflict : player_update -> damage -> unit
  val heal : player_update -> int -> unit
  val run_slots : player_update -> unit
  val apply_effects : player_update -> phase -> unit
  val add_effect : player_update -> phase * card_effect -> unit
  val remove_effect : player_update -> (card_effect -> bool) -> unit
  val map_effect : player_update -> (card_effect -> card_effect) -> unit
  val add_transition : player_update -> transition -> unit
  val pop_transition : player_update -> transition option
  val block_slot : player_update -> int -> unit
  val prepare_next_turn : player_update -> unit
  val submit_command : player_update -> command -> int option
  val submit : player_update -> command option -> unit
  val add_desc_mod : player_update -> desc_mod -> unit
  val remove_desc_mod : player_update -> desc_mod -> unit
end

module Reaction : sig
  val default : unit -> reaction
  (** Scala's [new Reaction]: every hook is a no-op, actions use the slot's default behaviour. Override fields
      with [{ (default ()) with ... }]. *)

  val bound_slot : reaction -> slot_update
  (** The slot the reaction is attached to (Scala's [selected]). Fails if the reaction has not been placed
      yet. *)
end

(** GameStateUpdater *)
module Updater : sig
  val create : game_state -> game_desc -> updater
  val player : updater -> player_id -> player_update
  val ended : updater -> player_id option
  val focus : updater -> int -> player_id -> bool -> unit

  val lift : updater -> (updater -> 'a) -> 'a * game_state
  (** Runs one engine step and publishes the state. Unlike Scala's lift, the returned state is the live one,
      not a snapshot. *)
end

(** {1 Card effects (GameCardEffect)} *)

val effects : (phase * card_effect) list -> phase_effects
(** Several effects for the same phase run in list order. *)

val no_effects : phase_effects
val focus : card_effect -> card_effect
val damage : ?is_ability:bool -> ?is_spell:bool -> int -> card_effect
val damage_creatures : ?is_ability:bool -> ?is_spell:bool -> int -> card_effect
val damage_creature : ?is_ability:bool -> ?is_spell:bool -> int -> card_effect
val mass_damage : ?is_ability:bool -> ?is_spell:bool -> ?immune_self:bool -> int -> card_effect
val heal : int -> card_effect
val heal_creature : int -> card_effect
val heal_creatures : int -> card_effect
val add_mana : int -> int list -> card_effect
val add_growth : int -> int list -> card_effect
val fill_empty_slots : creature -> card_effect
val add_desc_mod : desc_mod -> card_effect
val remove_desc_mod : desc_mod -> card_effect
val one_time_player_effect : card_effect -> card_effect
