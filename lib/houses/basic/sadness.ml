(* Sadness house — a water variant that takes water's basic slot in standard
   mode.  Same card-rule shape as water, with its own keys and numbers. *)

open Sp
open Engine
open Common

(* Living Shadow: every turn, 2 damage to its owner. *)
let living_shadow : card_effect =
 fun env ->
  Updater.focus env.env_updater env.env_selected env.env_player true;
  Player.inflict (player_of_env env) (ability_damage env 2)

(* Rain of Tears: drains a mana from every opponent house after the mass damage. *)
let tears_drain : card_effect =
 fun env -> Houses.incr_mana (other_of_env env).houses_update (-1) [ 0; 1; 2; 3; 4 ]

(* Blizzard Survivor: ignores spell and ability damage entirely. *)
let blizzard_survivor_reaction () =
  let r = Reaction.default () in
  { r with inflict = (fun s d -> if not (is_effect d) then Slot.damage_slot s d) }

(* Shellycoat: soaks 5 from every hit. *)
let shellycoat_reaction () =
  let r = Reaction.default () in
  { r with self_protect = (fun d -> { d with amount = max 0 (d.amount - 5) }) }

(* Ghostly Stag: halves (rounded up) every hit aimed at its owner. *)
let stag_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun e ->
        match e.de_target with
        | None -> { e.de_damage with amount = int_of_float (ceil (float_of_int e.de_damage.amount /. 2.0)) }
        | Some _ -> e.de_damage);
  }

(* Banshee: neighbours may attack the turn they enter play. *)
let banshee_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if abs (me.num - slot.num) = 1 then Slot.toggle slot run_flag);
  }

(* Pain Confessor: heals its owner by 10 when summoned, or 12 at 10 life or
   less. *)
let pain_confessor : card_effect =
 fun env ->
  let amount = if (player_of_env env).pstate.ps_life <= 10 then 12 else 10 in
  Updater.focus env.env_updater env.env_selected env.env_player true;
  Player.heal (player_of_env env) amount

let cards =
  [
    spell ~key:"sadness.retirement" ~description:"sadness.retirement.description"
      ~effects:(effects [ (Direct, add_mana 1 [ 0; 2; 3 ]) ])
      ();
    creature ~key:"sadness.shadow" ~attack:(attack 5) ~life:22 ~description:"sadness.shadow.description"
      ~effects:(effects [ (On_turn, living_shadow) ])
      ();
    creature ~key:"sadness.victim" ~attack:(attack 3) ~life:10 ~description:"sadness.victim.description"
      ~effects:(effects [ (Direct, focus (add_mana 2 [ 0 ])) ])
      ();
    creature ~key:"sadness.blizzard" ~attack:(attack 4) ~life:12
      ~description:"sadness.blizzard.description" ~new_reaction:blizzard_survivor_reaction ();
    creature ~key:"sadness.greywolf" ~attack:(attack 3) ~life:16
      ~description:"sadness.greywolf.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:1 ~house_indexes:[ 2 ])
      ();
    creature ~key:"sadness.stag" ~attack:(attack 3) ~life:20 ~description:"sadness.stag.description"
      ~new_reaction:stag_reaction ();
    creature ~key:"sadness.shellycoat" ~attack:(attack 5) ~life:16
      ~description:"sadness.shellycoat.description" ~new_reaction:shellycoat_reaction ();
    spell ~key:"sadness.tears" ~description:"sadness.tears.description"
      ~effects:(effects [ (Direct, mass_damage ~is_spell:true 15); (Direct, tears_drain) ])
      ();
    creature ~key:"sadness.banshee" ~attack:(attack 7) ~life:35 ~description:"sadness.banshee.description"
      ~new_reaction:banshee_reaction ();
    creature ~key:"sadness.paindruid" ~attack:(mana_attack 1) ~life:37
      ~description:"sadness.paindruid.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:1 ~house_indexes:[ 1 ])
      ~effects:(effects [ (Direct, pain_confessor) ])
      ();
    creature ~key:"sadness.fallenangel" ~attack:(attack 6) ~life:23
      ~description:"sadness.fallenangel.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:1 ~house_indexes:[ 0; 1; 2; 3; 4 ])
      ();
    creature ~key:"sadness.soulanchor" ~attack:(attack 1) ~life:18
      ~description:"sadness.soulanchor.description"
      ~new_reaction:(mana_growth_reaction ~owner:false ~amount:(-1) ~house_indexes:[ 0; 1; 2; 3; 4 ])
      ();
  ]

let house =
  {
    h_key = "sadness";
    h_name = "sadness";
    h_cards = cards;
    h_index = 1;
    h_effects = [];
    h_house_id = 2;
    h_additional_cards = [];
    h_make_listener = (fun () -> default_listener);
    h_data = No_player_data;
  }
