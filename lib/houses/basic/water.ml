(* Water house — port of priv/sp/house/Water.scala. *)

open Sp
open Engine
open Common

let sprite : card_effect =
 fun env ->
  Updater.focus env.env_updater env.env_selected env.env_player true;
  Player.inflict (player_of_env env) (mk_damage ~is_ability:true 2 (ctx_of_env env))

let shower_drain : card_effect =
 fun env -> Houses.incr_mana (other_of_env env).houses_update (-1) [ 0; 1; 2; 3; 4 ]

let golem_reaction () =
  let r = Reaction.default () in
  { r with inflict = (fun s d -> if not (is_effect d) then Slot.damage_slot s d) }

let turtle_reaction () =
  let r = Reaction.default () in
  { r with self_protect = (fun d -> { d with amount = max 0 (d.amount - 5) }) }

let iceguard_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun e ->
        match e.de_target with
        | None -> { e.de_damage with amount = int_of_float (ceil (float_of_int e.de_damage.amount /. 2.0)) }
        | Some _ -> e.de_damage);
  }

let overlord_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if abs (me.num - slot.num) = 1 then Slot.toggle slot run_flag);
  }

let cards =
  [
    spell ~key:"water.meditate" ~description:"water.meditate.description"
      ~effects:(effects [ (Direct, add_mana 1 [ 0; 2; 3 ]) ])
      ();
    creature ~key:"water.sprite" ~attack:(attack 5) ~life:22 ~description:"water.sprite.description"
      ~effects:(effects [ (On_turn, sprite) ])
      ();
    creature ~key:"water.apostate" ~attack:(attack 3) ~life:10 ~description:"water.apostate.description"
      ~effects:(effects [ (Direct, focus (add_mana 2 [ 0 ])) ])
      ();
    creature ~key:"water.golem" ~attack:(attack 4) ~life:12 ~description:"water.golem.description"
      ~new_reaction:golem_reaction ();
    creature ~key:"water.elder" ~attack:(attack 3) ~life:16 ~description:"water.elder.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:1 ~house_indexes:[ 2 ])
      ();
    creature ~key:"water.guard" ~attack:(attack 3) ~life:20 ~description:"water.guard.description"
      ~new_reaction:iceguard_reaction ();
    creature ~key:"water.turtle" ~attack:(attack 5) ~life:17 ~description:"water.turtle.description"
      ~new_reaction:turtle_reaction ();
    spell ~key:"water.shower" ~description:"water.shower.description"
      ~effects:(effects [ (Direct, mass_damage ~is_spell:true 15); (Direct, shower_drain) ])
      ();
    creature ~key:"water.lord" ~attack:(attack 7) ~life:34 ~description:"water.lord.description"
      ~new_reaction:overlord_reaction ();
    creature ~key:"water.elemental" ~attack:(mana_attack 1) ~life:38
      ~description:"water.elemental.description"
      ~effects:(effects [ (Direct, focus (heal 10)); (On_turn, add_mana 1 [ 1 ]) ])
      ();
    creature ~key:"water.mind" ~attack:(attack 6) ~life:22 ~description:"water.mind.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:1 ~house_indexes:[ 0; 1; 2; 3; 4 ])
      ();
    creature ~key:"water.astral" ~attack:(attack 1) ~life:17 ~description:"water.astral.description"
      ~new_reaction:(mana_growth_reaction ~owner:false ~amount:(-1) ~house_indexes:[ 0; 1; 2; 3; 4 ])
      ();
  ]

let house =
  {
    h_key = "water";
    h_name = "water";
    h_cards = cards;
    h_index = 1;
    h_effects = [];
    h_house_id = 2;
    h_additional_cards = [];
    h_make_listener = (fun () -> default_listener);
    h_data = No_player_data;
  }
