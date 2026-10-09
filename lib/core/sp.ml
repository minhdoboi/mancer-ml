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

(* How many blows a custom attack may land in a row (the Shadow Dancer
   strikes twice).  The count is a safety net: a card that keeps asking for
   another blow cannot hang the game. *)
let max_strikes = 8

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

type command_flag = Target_other | Called_element of int
and transition = Wait_player of player_id * string option
and mod_ = Spell_mod of (int -> int)

(* Per-slot and per-player card state.  Scala stores `AnyRef` here
   (CardSpec.scala:98, State.scala:45).  The mod-carrying cases mirror
   Scala's `Destroyed` / `Destroyeds` / `LakeData`, which houses stash in
   the occupant's slot to undo on removal (darksider lake, fairy lesovik
   and alkonost).  They hold the exact `desc_mod` values added so
   `Player.remove_desc_mod` (physical equality) finds them again. *)
and slot_data =
  | No_data
  | Targeting of int option
  | Custom of string
  | Slot_destroyed of desc_mod
  | Slot_destroyeds of card list * desc_mod
  | Lake_mods of desc_mod list
  (* Warp: the creature under a Cloak, restored when the Cloak dies *)
  | Cloaked of slot_state
  (* Blacksmith Flying Chariot: the wielder riding the Pegasus, restored when
     the Pegasus dies *)
  | Ridden of slot_state
  (* Warp: id of the creature a Schizo silenced, released when the Schizo leaves *)
  | Schizo_target of int
  (* Trappers Mind Witch: id of the creature the Witch is silencing, released when the Witch leaves *)
  | Witch_bound of int
  (* Warp: set on the card of a silenced creature, the creature it stands for *)
  | Mortal_of of creature
  (* Shadowmage: Shade Stalker until it takes damage; Dark Warden after blocking once this turn *)
  | Stealthed
  | Absorbed
  (* Colors: X spent on this creature, and Emerald's special-cost mod if it added one *)
  | Color_x of int * desc_mod option
  (* Limbo: postponed death until end of turn, or a token that cannot enter limbo *)
  | Limbo_state
  | No_limbo
  (* Entomologist Mantis: house index whose growth is bridled *)
  | Mantis_house of int
  (* Spirit Shaman Panther Totem: owner turns survived *)
  | Count of int
  (* Quantumancer Spirit of Sealing: sealed (card, SealMod) pairs and the side that recorded them *)
  | Sealed of (card * desc_mod) list * player_id
  (* Soul Reaper Eternal Rage: lethal hits zero life and hang until a 2-turn countdown *)
  | Eternal
  (* Mountain King: this dwarf is in a hird with a neighbour *)
  | Hird
  (* High Priest Bennu: already made its dying attack *)
  | Bennu_dead
  (* High Priest Babi: last seen opponent mana total *)
  | Babi_mana of int

and slot_snap = {
  ss_num : int;
  ss_card : creature;
  ss_life : int;
  ss_max_life : int;
  ss_status : int;
  ss_attack_sources : attack_sources;
  ss_target : int list;
  ss_data : slot_data;
  ss_marks : blacksmith_mark list;
}

(* A lasting Blacksmith spell on a creature.  [bm_target] is the slot id of the
   linked creature for the spawned Astral Cloak/Dagger tokens (0 otherwise). *)
and blacksmith_mark = { bm_key : string; bm_used : bool; bm_target : int }

and board_snap = { bs_owner : slot_snap list; bs_opp : slot_snap list }
and wind_state = { winged : bool; vortex : bool }

(* Powermancer: Infernal Bargain bought, own turns finished, and the availability mod currently in the
   player's desc mods (with the special mana it was built for) *)
and power_data = { pw_bargain : bool; pw_turn : int; pw_mod : (int * desc_mod) option }
and chrono_data = { previous : board_snap option; current : board_snap option }
and shadowmage_data = { eclipse_shield : int }
and color_player = { cp_x : int; cp_hidden : (int * desc_mod) list }

(* Quantumancer: opponent spells seen, and the Cosmic Mage choice mod while it is up *)
and played_spell = { ps_card : card; ps_input : int option; ps_target_other : bool; ps_cost : int }
and quantum_data = { q_played : played_spell list; q_choice : desc_mod option }

(* Meteomancer: Called Element, Mistwalker hide flag, Fateweaver extra-play charges and HideNonCalled *)
and meteo_data = { mt_called : int option; mt_hidden : bool; mt_fate : int; mt_hide : desc_mod option }

(* Seasons: Solar Wave triple, Midnight Sun, Eternal Summer *)
and season_data = { se_triple : bool; se_midnight : bool; se_summer : bool }

(* Shaman ghost wolf: moon protection, fury, hunt heals, mate columns, shadows, rage enhancements *)
and wolf_state = {
  wo_protection : int;
  wo_fury : int option;
  wo_bonus : int;
  wo_hunting : int;
  wo_mates : int list;
  wo_shadows : int list;
  wo_enhanced : (int * int) list;
}

(* High Priest: unique opponent cards seen, and own turns counted *)
and priest_data = { hp_revealed : (int * int) list; hp_turn : int }

(* Beastlords: the distinct cards the player has revealed (played), as
   (house_index, card_index) identities, oldest first. *)
and beast_data = { bd_revealed : (int * int) list }

(* Lightstaves: an Aura around a creature, with the turns it has left. *)
and aura_state = { au_id : int; au_key : string; au_target : int; au_remaining : int }

(* Kinetician: ChangeTarget id plus Call of the Fittest cost mods *)
and kinetic_data = { ki_target : int option; ki_halves : (int * desc_mod) list }

and player_data =
  | No_player_data
  | Player_targeting of int option
  | Zen_cocoons of int list
  | Wind_state of wind_state
  | Chrono_data of chrono_data
  | Power_data of power_data
  | Shadowmage_data of shadowmage_data
  | Color_data of color_player
  | Limbo_player of [ `Limbo | `No_limbo ]
  (* Soulbinder: empty slots marked as Bound Souls *)
  | Soulbinder_data of int list
  (* Shadowcaster: Dark power, separate from special mana *)
  | Shadowcaster_data of int
  | Quantum_data of quantum_data
  | Meteo_data of meteo_data
  | Season_data of season_data
  (* Soul Reaper: souls gathered from deaths *)
  | Reaper_data of int
  | Wolf_data of wolf_state
  | Priest_data of priest_data
  | Kinetic_data of kinetic_data
  (* Beastlords: revealed cards *)
  | Beast_data of beast_data
  (* Lightstaves: active Auras *)
  | Aura_data of aura_state list

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
  | Aging_malus

and attack_sources = { base : int option; sources : attack_source list }
and context = { ctx_player : player_id; ctx_card : card option; ctx_selected : int }

(* [redirected]: set on a spell hit that a Battlemage field mage has taken in another creature's place, so it
   is not redirected twice (Scala's RedirectedContext) *)
and damage = {
  amount : int;
  context : context;
  is_ability : bool;
  is_spell : bool;
  redirected : bool;
  (* Bargulian Gauntlet: ignores protection and also lowers max life (so the
     damage cannot be healed back). *)
  mortal : bool;
}
and run_attack_kind =
  | Single_target
  | Multi_target
  | Custom of (int list -> damage -> player_update -> unit)
  (* An attack that strikes more than once.  The function lands one blow and
     returns true when another blow follows it; [blows] is the number of
     blows that already landed, so the first call gets 0.  The engine gives
     every blow its own animation and its own damage. *)
  | Custom_strikes of (int -> int list -> damage -> player_update -> bool)
and run_attack = { can_attack : bool; apply_ : run_attack_kind; is_multi_target : bool }

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
  marks : blacksmith_mark list;
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
  (* Scala [Description]: extra [{0}] args from the live board (Lightning
     damage, Fury total, …). [None] leaves the template as-is. *)
  desc_args : (game_state -> player_id -> string list) option;
  is_spell : bool;
  (* Auras are cast like spells but are deliberately not spells. *)
  is_aura : bool;
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
  h_make_listener : unit -> house_event_listener;
  h_data : player_data;
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
  mutable ps_maluses : player_malus list;
}

(* A malus the player carries: a named handicap with an amount, like the tax
   on the cost of their cards. *)
and player_malus = { pm_key : string; pm_amount : int }

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
  hl_init : player_update -> unit;
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
  (* Beastlords: a new card was revealed (played) by this creature's owner; the
     command's card is the freshly revealed one. Default is a no-op. *)
  on_reveal : card -> unit;
  (* called on the new creature's reaction when summoned over an occupied
     slot; the default destroys the occupant (CardSpec.scala:212) *)
  on_spawn_over : slot_update -> slot_mod option;
  on_overwrite : creature -> unit;
  clean_up : unit -> unit;
  on_protect : damage_event -> damage;
  on_death : dead -> unit;
  (* Scala LimboReaction.onLimbo: only limbo keepers implement this *)
  on_limbo : slot_update -> unit;
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
  mutable protect_hook : damage -> damage;
}

and slots_update = {
  player : player_update;
  (* filled right after the record is built, to close the cycle *)
  mutable slot_updates : slot_update array;
  mutable logs : board_event list;
  mutable on_dead_hook : dead -> unit;
  (* Scala slots.update.after: add, remove, and move *)
  mutable after_change : unit -> unit;
}

and houses_update = {
  hu_player : player_update;
  mutable hu_value : house_state array;
  (* Scala houses.update.after: fired after a full mana write *)
  mutable after_update : house_state array -> unit;
}

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
  mutable set_data_hook : player_data -> unit;
}

(* What a UI animates. The first ten mirror Scala's UpdateListener
   (GameUpdate.scala:56); the last three come from the turn loop. *)
and game_event =
  | Ev_focus of { num : int; player : player_id; blocking : bool }
  | Ev_move of { num : int; dest : int; player : player_id; dest_player : player_id }
  | Ev_swap of { num : int; dest : int; player : player_id; dest_player : player_id }
  | Ev_run_slot of { num : int; player : player_id }
  | Ev_summon of { num : int; player : player_id; creature : creature }
  | Ev_die of { num : int; player : player_id }
  | Ev_replaced of { num : int; player : player_id; creature : creature }
  | Ev_refresh of { silent : bool }
  | Ev_spell_played of command
  | Ev_ability of { kind : string; player : player_id; num : int; target : (player_id * int) option }
  (* A creature moved the mana of some (not all) houses: the UI draws an arc
     from the creature to each affected house label, light for a bonus and
     dark red for a malus. *)
  | Ev_mana of { player : player_id; num : int; target : player_id; deltas : (int * int) list }
  | Ev_to_move of { t : int; player : player_id }
  | Ev_submitted of { t : int; player : player_id; command : command option }
  | Ev_game_over of { winner : player_id option; reason : string }

and updater = {
  mutable state : game_state;
  desc : game_desc;
  (* filled right after the record is built, to close the cycle *)
  mutable player_updates : player_update array;
  mutable ended : player_id option;
  (* Scala's UpdateListener, as one typed stream; see game_event *)
  mutable listener : game_event -> unit;
  (* Scala's updater.randLogs: card effects draw from here, never from the global Random.
     This is a java.util.Random LCG so a seeded OCaml game and a Scala replay share dice. *)
  rng : Dice.t;
}

(* ---------------------------------------------------------------- helpers *)

let no_selection = -1

(* [h_index] of every special house; the four basic houses are 0..3. *)
let special_house_index = 4

let default_listener =
  {
    hl_on_death = (fun () -> true);
    hl_mod = Fun.id;
    hl_intercept_submit = (fun c -> (false, c));
    hl_init = ignore;
  }

let empty_board_snap = { bs_owner = []; bs_opp = [] }

let effect_at (e : phase_effects) = function
  | Direct -> e.direct
  | On_turn -> e.on_turn
  | On_end_turn -> e.on_end_turn
  | On_start -> e.on_start

let mk_damage ?(is_ability = false) ?(is_spell = false) ?(mortal = false) amount context =
  { amount; context; is_ability; is_spell; redirected = false; mortal }

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

(* every card of every house, after the player's description mods *)
let modded_cards (p : player_state) (h : player_house_desc) =
  List.fold_left (fun cards m -> m h cards) h.phd_cards p.ps_desc.desc_mods

(* how much of a malus the player carries *)
let malus_amount (p : player_state) key =
  List.fold_left (fun n m -> if m.pm_key = key then n + m.pm_amount else n) 0 p.ps_maluses

let is_disabled (p : player_state) =
  Array.for_all
    (fun h ->
      let hs = p.ps_houses.(h.phd_house.h_index) in
      Array.for_all (fun cd -> not (is_available cd hs)) (modded_cards p h))
    p.ps_desc.init_desc.pd_houses

(* a skipped turn (Deep freeze): the mods switch every card off, whatever the mana *)
let turn_skipped (p : player_state) =
  Array.for_all
    (fun h -> Array.for_all (fun cd -> not cd.cd_enabled) (modded_cards p h))
    p.ps_desc.init_desc.pd_houses

let command_to_string (c : command) =
  Printf.sprintf "%d> %s%s" c.cmd_player c.cmd_card.name
    (match c.cmd_input with None -> "" | Some n -> Printf.sprintf " at %d" n)

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
