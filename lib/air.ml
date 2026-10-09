(* Air house — port of priv/sp/house/Air.scala. *)

open Sp
open Engine
open Common

let griffin : card_effect =
 fun env ->
  if get_mana env 2 + env.env_paid_cost >= 5 then
    Player.inflict (other_of_env env) (mk_damage 5 (ctx_of_env env))

let medic : card_effect =
 fun env ->
  Updater.focus env.env_updater env.env_selected env.env_player true;
  Player.heal (player_of_env env) (min (get_mana env 3) 10)

let lightning : card_effect =
 fun env ->
  Player.inflict (other_of_env env)
    (mk_damage ~is_spell:true (5 + get_mana env 2 + env.env_paid_cost) (ctx_of_env env))

let twister : card_effect =
 fun env -> (other_of_env env).slots_update.slot_updates.(env.env_selected).overridable_destroy ()

let titan : card_effect =
 fun env ->
  Updater.focus env.env_updater env.env_selected env.env_player true;
  Slot.inflict
    (other_of_env env).slots_update.slot_updates.(env.env_selected)
    (mk_damage ~is_ability:true 15 (ctx_of_env env))

let phoenix_reaction () =
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
    creature ~key:"air.fairy" ~attack:(attack 4) ~life:11 ~description:"air.fairy.description"
      ~mod_:(Spell_mod (fun x -> x + 1))
      ();
    creature ~key:"air.griffin" ~attack:(attack 3) ~life:15 ~description:"air.griffin.description"
      ~effects:(effects [ (Direct, griffin) ])
      ();
    spell ~key:"air.call" ~description:"air.call.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, damage_creature ~is_spell:true 6); (Direct, damage ~is_spell:true 6) ])
      ();
    creature ~key:"air.medic" ~attack:(attack 4) ~life:19 ~description:"air.medic.description"
      ~effects:(effects [ (Direct, medic) ])
      ();
    creature ~key:"air.fence" ~attack:(attack 0) ~life:28 ~description:"air.fence.description"
      ~effects:(effects [ (On_turn, focus (damage ~is_ability:true 4)) ])
      ();
    spell ~key:"air.lightnin" ~description:"air.lightnin.description"
      ~effects:(effects [ (Direct, lightning) ])
      ();
    creature ~key:"air.phoenix" ~attack:(attack 6) ~life:18 ~description:"air.phoenix.description"
      ~new_reaction:phoenix_reaction ();
    spell ~key:"air.chain" ~description:"air.chain.description"
      ~effects:(effects [ (Direct, damage_creatures ~is_spell:true 9); (Direct, damage ~is_spell:true 9) ])
      ();
    creature ~key:"air.cloud" ~attack:(attack 4) ~life:20 ~description:"air.cloud.description"
      ~run_attack:multi_run_attack ();
    spell ~key:"air.twister" ~description:"air.twister.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, twister) ])
      ();
    creature ~key:"air.elemental" ~attack:(mana_attack 2) ~life:44 ~description:"air.elemental.description"
      ~effects:(effects [ (Direct, focus (damage ~is_ability:true 8)); (On_turn, add_mana 1 [ 2 ]) ])
      ();
    creature ~key:"air.titan" ~attack:(attack 9) ~life:40 ~description:"air.titan.description"
      ~effects:(effects [ (Direct, titan) ])
      ();
  ]

let house =
  {
    h_key = "air";
    h_name = "air";
    h_cards = cards;
    h_index = 2;
    h_effects = [];
    h_house_id = 0;
    h_additional_cards = [];
  }
