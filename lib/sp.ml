(* Core state types.  Mirrors priv/sp/State.scala, priv/sp/CardSpec.scala and
   the update-graph types from priv/sp/update/*.scala.  Behaviour lives in
   engine.ml.

   Departures from the Scala original:

   - `Reaction` is a record of function fields, not a class hierarchy.  Houses
     override single fields with `{ r with ... }`.

   - The update graph (updater -> player -> slots -> slot -> slots) is cyclic,
     with parent pointers exactly as in SlotUpdate.

   Naming: the most central type of each kind keeps bare field names
   (`card`, `slot_state`, `reaction`, `damage`); the others carry a short
   prefix (`b_`, `cd_`, `h_`, `ps_`, `cmd_`, `env_`, ...) so that record
   literals and accesses never need a type annotation to disambiguate. *)

type player_id = int

let owner = 0
let opponent = 1
let other (id : player_id) = if id = owner then opponent else owner
let nb_slots = 6
let base_slot_list = List.init nb_slots Fun.id
let in_slot_range n = n >= 0 && n < nb_slots
let adjacents n = List.filter in_slot_range [ n - 1; n + 1 ]

(* status flags *)
let run_flag = 1
let stun_flag = 2
let invincible_flag = 4
let blocked_flag = 8
let paused_flag = 16
let cursed_flag = 32
let on_hold = stun_flag + blocked_flag + paused_flag

type phase = Direct | On_turn | On_end_turn | On_start

(* One mutually recursive group: input_spec mentions game_state,
   attack_source mentions player_update, creature_body mentions reaction, and
   so on. *)

type command_flag = Target_other
and transition = Wait_player of player_id * string option
and mod_ = Spell_mod of (int -> int)

(* Per-slot and per-player card state.  Scala stores `AnyRef` here
   (CardSpec.scala:98, State.scala:45). *)
and slot_data = No_data | Targeting of int option | Custom of string
and player_data = No_player_data | Player_targeting of int option

and input_spec =
  | No_input
  | Select_owner_slot
  | Select_owner_creature
  | Select_target_slot
  | Select_target_creature
  | Select_any_creature
  | Select_owner of (player_id -> game_state -> int list)
  | Select_target of (player_id -> game_state -> int list)

and attack_source =
  | Attack_func of (int -> int)
  (* Scala's `case class OrcAttackBonus(orcPos) extends AttackFunc`: a bonus
     identified by (tag, pos) so removeFirst can find it again.  Closures are
     not comparable in OCaml, hence the explicit key. *)
  | Attack_bonus of { tag : string; pos : int; bonus : int -> int }
  | Attack_state_func of (int -> player_update -> int)
  | Attack_slot_state_func of (int -> slot_update -> int)
  | Extra_attacks of int
  | Mana_attack of int

and attack_sources = { base : int option; sources : attack_source list }
and context = { ctx_player : player_id; ctx_card : card option; ctx_selected : int }
and damage = { amount : int; context : context; is_ability : bool; is_spell : bool }
and run_attack_kind = Single_target | Multi_target
and run_attack = { can_attack : bool; apply_ : run_attack_kind }

(* A card is the shared descriptor; a creature additionally carries the
   per-play body.  In Scala `Creature extends Card`; here the creature embeds
   its card, which removes the need for a downcast. *)
and creature_body = {
  b_attack : attack_sources;
  b_life : int;
  b_new_reaction : unit -> reaction;
  b_run_attack : run_attack;
  b_is_altar : bool;
  b_status : int;
  b_mod : mod_ option;
  b_data : slot_data;
}

and creature = { c_card : card; c_body : creature_body }

and slot_state = {
  card : creature;
  life : int;
  max_life : int;
  status : int;
  attack_sources : attack_sources;
  attack : int;
  target : int list;
  slot_id : int;
  reaction : reaction;
  data : slot_data;
}

and phase_effects = {
  direct : card_effect option;
  on_turn : card_effect option;
  on_end_turn : card_effect option;
  on_start : card_effect option;
}

and card = {
  key : string;
  name : string;
  (* set once at deck-build time, like Scala's `var cost` filled by
     House.initCards *)
  mutable cost : int;
  mutable house_index : int;
  mutable card_index : int;
  mutable house_id : int;
  id : int;
  effects : phase_effects;
  input_spec : input_spec;
  description : string;
  is_spell : bool;
  (* Some for creatures, None for spells *)
  creature_body : creature_body option;
}

and card_desc = { cd_card : card; cd_cost : int; cd_enabled : bool; cd_extra_mana : int }
and desc_mod = player_house_desc -> card_desc array -> card_desc array
and desc_reader = { init_desc : player_desc; mutable desc_mods : desc_mod list }

and house = {
  h_key : string;
  h_name : string;
  h_cards : card list;
  h_index : int;
  h_effects : (phase * card_effect) list;
  mutable h_house_id : int;
  mutable h_additional_cards : card list;
}

and player_house_desc = { phd_house : house; phd_cards : card_desc array }
and player_desc = { pd_houses : player_house_desc array }
and game_desc = { gd_players : player_desc array }
and house_state = { mana : int; growth : int }
and player_stats = { mutable nb_dead : int; mutable nb_summon : int }

and player_state = {
  (* mutable because Scala rebuilds them from the child FieldUpdates on every
     commit; here they are re-synced in place *)
  mutable ps_houses : house_state array;
  ps_desc : desc_reader;
  ps_slots : slot_state option array;
  mutable ps_slot_list : int list;
  mutable ps_life : int;
  mutable ps_effects : (phase * card_effect) list;
  mutable ps_data : player_data;
  mutable ps_transitions : transition list;
}

and game_state = { players : player_state array }

and command = {
  cmd_player : player_id;
  cmd_card : card;
  cmd_input : int option;
  cmd_cost : int;
  cmd_flag : command_flag option;
}

(* Board events — CardSpec.scala:166-179 *)
and dead = { d_num : int; d_slot : slot_state; d_player : player_update; d_damage : damage option }
and damage_event = { de_damage : damage; de_target : int option; de_player : player_update }
and summon_event = { s_num : int; s_card : creature; s_player : player_update }
and board_event = Board_dead of dead | Board_damage of damage_event | Board_summon of summon_event
and slot_mod = slot_state -> slot_state

and house_event_listener = {
  hl_on_death : unit -> bool;
  hl_mod : damage -> damage;
  hl_intercept_submit : command option -> bool * command option;
}

and card_effect = env -> unit

and env = {
  env_player : player_id;
  env_updater : updater;
  mutable env_selected : int;
  mutable env_card : card option;
  mutable env_paid_cost : int;
  mutable env_target_other : bool;
}

(* Reaction + Actions (CardSpec.scala:186-222).  Scala's implicit `selected`
   becomes an explicit slot_update argument on the action fields. *)
and reaction = {
  selected : slot_update option ref;
  heal : slot_update -> int -> unit;
  inflict : slot_update -> damage -> unit;
  destroy : slot_update -> unit;
  stun : slot_update -> unit;
  on_add : slot_update -> unit;
  on_remove : slot_update -> unit;
  self_protect : damage -> damage;
  on_my_damage : damage -> unit;
  on_my_remove : dead option -> unit;
  on_my_death : dead -> unit;
  on_summon : summon_event -> unit;
  (* called on the new creature's reaction when summoned over an occupied
     slot; the default destroys the occupant (CardSpec.scala:212) *)
  on_spawn_over : slot_update -> slot_mod option;
  on_overwrite : creature -> unit;
  clean_up : unit -> unit;
  on_protect : damage_event -> damage;
  on_death : dead -> unit;
}

(* The `*_hook` fields are Scala's `var` interception points: houses replace
   them at runtime. *)
and slot_update = {
  num : int;
  slots : slots_update;
  mutable value : slot_state option;
  mutable overridable_destroy : unit -> unit;
  mutable add_hook : slot_state -> unit;
  mutable on_damage_hook : slot_state -> damage -> unit;
  mutable remove_hook : dead option -> slot_state;
  mutable delayed_destroy_hook : damage -> unit;
}

and slots_update = {
  player : player_update;
  (* filled right after the record is built, to close the cycle *)
  mutable slot_updates : slot_update array;
  mutable logs : board_event list;
  mutable on_dead_hook : dead -> unit;
}

and houses_update = { hu_player : player_update; mutable hu_value : house_state array }

and player_update = {
  pid : player_id;
  updater : updater;
  pstate : player_state;
  slots_update : slots_update;
  houses_update : houses_update;
  house_event_listener : house_event_listener;
  stats : player_stats;
  mutable submit_command_hook : command -> int option;
  mutable on_player_damage_hook : damage -> unit;
  mutable guard_hook : damage -> damage;
  mutable add_mana_hook : house_state -> int -> house_state;
}

and updater = {
  mutable state : game_state;
  desc : game_desc;
  (* filled right after the record is built, to close the cycle *)
  mutable player_updates : player_update array;
  mutable ended : player_id option;
  mutable focus_hook : int -> player_id -> bool -> unit;
}

(* ---------------------------------------------------------------- helpers *)

let no_selection = -1

let effect_at (e : phase_effects) = function
  | Direct -> e.direct
  | On_turn -> e.on_turn
  | On_end_turn -> e.on_end_turn
  | On_start -> e.on_start

let mk_damage ?(is_ability = false) ?(is_spell = false) amount context =
  { amount; context; is_ability; is_spell }

(* Scala keeps separate counters for Card.currentId and SlotState.currentId *)
let card_counter = ref 0
let slot_counter = ref 0

let fresh_card_id () =
  incr card_counter;
  !card_counter

let fresh_slot_id () =
  incr slot_counter;
  !slot_counter

let has_flag (st : slot_state) flag = st.status land flag <> 0
let is_effect (d : damage) = d.is_ability || d.is_spell
let dead_is_destroy (d : dead) = Option.is_none d.d_damage
let card_index1 (c : card) = c.card_index + 1
let is_available cd (h : house_state) = cd.cd_cost <= h.mana + cd.cd_extra_mana && cd.cd_enabled

let state_to_string (s : game_state) =
  let player (p : player_state) =
    let mana = Array.to_list p.ps_houses |> List.map (fun h -> string_of_int h.mana) in
    let slots =
      Array.to_list p.ps_slots
      |> List.map (function Some sl -> Printf.sprintf "%s/%d" sl.card.c_card.name sl.life | None -> ".")
    in
    Printf.sprintf "houses %s, life %d, slots %s" (String.concat "," mana) p.ps_life (String.concat "," slots)
  in
  String.concat "\n" (Array.to_list s.players |> List.map player)
