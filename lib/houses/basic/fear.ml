(* Fear house — an air variant that takes air's basic slot in standard mode.
   Same card-rule shape as air, with its own keys and numbers. *)

open Sp
open Engine
open Common

(* Harpy: at 5 fear mana or more, 5 damage to the opponent when summoned. *)
let harpy : card_effect =
 fun env ->
  if get_mana env 2 + env.env_paid_cost >= 5 then
    Player.inflict (other_of_env env) (mk_damage 5 (ctx_of_env env))

(* Crow Sage: heals its owner by the owner's joy mana (house 3), capped at 10. *)
let crow_sage : card_effect =
 fun env ->
  Updater.focus env.env_updater env.env_selected env.env_player true;
  Player.heal (player_of_env env) (min (get_mana env 3) 10)

(* Dark Spirit Strike: 5 + fear power, counting the mana before the spell was paid. *)
let dark_spirit : card_effect =
 fun env -> Player.inflict (other_of_env env) (spell_damage env (5 + get_mana env 2 + env.env_paid_cost))

(* Fear of Fear: destroy the target. *)
let fear_of_fear : card_effect = fun env -> (opp_slot_of_env env).overridable_destroy ()

(* Malediction: 15 damage to the creature in the opposing slot. *)
let malediction : card_effect =
 fun env ->
  Updater.focus env.env_updater env.env_selected env.env_player true;
  Slot.inflict (opp_slot_of_env env) (ability_damage env 15)

(* Medusa Egregore: when its owner is at 10 life or less, the creature in the
   opposing slot is petrified (stunned) as she is summoned. *)
let medusa_gaze : card_effect =
 fun env -> if (player_of_env env).pstate.ps_life <= 10 then Slot.stun (opp_slot_of_env env)

(* Shapeless Mimic: rebirths itself on death while its owner's anger is 10+,
   unless the death was a "destroy". *)
let mimic_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        if (not (dead_is_destroy dead)) && dead.d_player.houses_update.hu_value.(0).mana > 9 then (
          let slot = dead.d_player.slots_update.slot_updates.(dead.d_num) in
          Slots.add_creature slot dead.d_slot.card;
          if has_flag dead.d_slot run_flag then Slot.toggle slot run_flag));
  }

let cards =
  [
    creature ~key:"fear.ghost" ~attack:(attack 4) ~life:12 ~description:"fear.ghost.description"
      ~mod_:(Spell_mod (fun x -> x + 1))
      ();
    creature ~key:"fear.harpy" ~attack:(attack 3) ~life:15 ~description:"fear.harpy.description"
      ~effects:(effects [ (Direct, harpy) ])
      ();
    spell ~key:"fear.strike" ~description:"fear.strike.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, damage_creature ~is_spell:true 6); (Direct, damage ~is_spell:true 6) ])
      ();
    creature ~key:"fear.sage" ~attack:(attack 4) ~life:19 ~description:"fear.sage.description"
      ~effects:(effects [ (Direct, crow_sage) ])
      ();
    creature ~key:"fear.fog" ~attack:(attack 0) ~life:28 ~description:"fear.fog.description"
      ~effects:(effects [ (On_turn, focus (damage ~is_ability:true 4)) ])
      ();
    spell ~key:"fear.spirit" ~description:"fear.spirit.description"
      ~desc_args:(fun state pid -> [ string_of_int (5 + state.players.(pid).ps_houses.(2).mana) ])
      ~effects:(effects [ (Direct, dark_spirit) ])
      ();
    creature ~key:"fear.mimic" ~attack:(attack 7) ~life:16 ~description:"fear.mimic.description"
      ~new_reaction:mimic_reaction ();
    spell ~key:"fear.invasion" ~description:"fear.invasion.description"
      ~effects:(effects [ (Direct, damage_creatures ~is_spell:true 9); (Direct, damage ~is_spell:true 9) ])
      ();
    creature ~key:"fear.clouds" ~attack:(attack 4) ~life:20 ~description:"fear.clouds.description"
      ~run_attack:multi_run_attack ();
    spell ~key:"fear.fear" ~description:"fear.fear.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, fear_of_fear) ])
      ();
    creature ~key:"fear.medusa" ~attack:(mana_attack 2) ~life:44 ~description:"fear.medusa.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:1 ~house_indexes:[ 2 ])
      ~effects:(effects [ (Direct, focus (damage ~is_ability:true 8)); (Direct, medusa_gaze) ])
      ();
    creature ~key:"fear.malediction" ~attack:(attack 9) ~life:40
      ~description:"fear.malediction.description"
      ~effects:(effects [ (Direct, malediction) ])
      ();
  ]

let house =
  {
    h_key = "fear";
    h_name = "fear";
    h_cards = cards;
    h_index = 2;
    h_effects = [];
    h_house_id = 3;
    h_additional_cards = [];
    h_make_listener = (fun () -> default_listener);
    h_data = No_player_data;
  }
