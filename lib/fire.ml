(* Fire house — port of priv/sp/house/Fire.scala. *)

open Sp
open Engine
open Common

let goblin_berserker : card_effect =
 fun env ->
  let d = mk_damage ~is_ability:true 2 (ctx_of_env env) in
  let selected = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
  match Slot.filled_adjacent selected with
  | [] -> ()
  | targets ->
      Updater.focus env.env_updater env.env_selected env.env_player true;
      List.iter (fun s -> Slot.inflict s d) targets

let inferno : card_effect =
 fun env ->
  let damage amount = mk_damage ~is_spell:true amount (ctx_of_env env) in
  Slots.foreach (other_of_env env).slots_update (fun s ->
      Slot.inflict s (damage (if s.num = env.env_selected then 18 else 10)))

let armageddon : card_effect =
 fun env ->
  (* counts fire mana from before the spell was paid, as the description shows *)
  let amount = get_mana env 0 + env.env_paid_cost + 8 in
  Player.inflict (other_of_env env) (mk_damage ~is_spell:true amount (ctx_of_env env));
  (* massDamage: every creature on both sides, the caster's included *)
  mass_damage ~is_spell:true amount env

(* OrcSlotReaction / BullSlotReaction *)
let bonus_reaction tag ~cond ~plus () =
  attack_bonus_reaction ~cond
    ~get_bonus:(fun pos -> Attack_bonus { tag; pos; bonus = (fun a -> a + plus) })
    ()

let cards =
  [
    creature ~key:"fire.goblin" ~attack:(attack 4) ~life:16 ~description:"fire.goblin.description"
      ~effects:(effects [ (On_turn, goblin_berserker) ])
      ();
    creature ~key:"fire.wall" ~attack:(attack 0) ~life:5 ~description:"fire.wall.description"
      ~effects:(effects [ (Direct, damage_creatures ~is_ability:true 5) ])
      ();
    creature ~key:"fire.monk" ~attack:(attack 3) ~life:13 ~description:"fire.monk.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:1 ~house_indexes:[ 0 ])
      ();
    creature ~key:"fire.drake" ~attack:(attack 4) ~life:18 ~description:"fire.drake.description"
      ~status:run_flag ();
    creature ~key:"fire.orc" ~attack:(attack 3) ~life:16 ~description:"fire.orc.description"
      ~new_reaction:(bonus_reaction "orc" ~cond:(fun pos num -> abs (pos - num) = 1) ~plus:2)
      ();
    spell ~key:"fire.wave" ~description:"fire.wave.description"
      ~effects:(effects [ (Direct, damage_creatures ~is_spell:true 9) ])
      ();
    creature ~key:"fire.bull" ~attack:(attack 6) ~life:20 ~description:"fire.bull.description"
      ~new_reaction:(bonus_reaction "bull" ~cond:( <> ) ~plus:1)
      ();
    creature ~key:"fire.blargl" ~attack:(attack 8) ~life:26 ~description:"fire.blargl.description"
      ~effects:(effects [ (Direct, mass_damage ~is_ability:true ~immune_self:true 4) ])
      ();
    spell ~key:"fire.inferno" ~description:"fire.inferno.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, inferno) ])
      ();
    creature ~key:"fire.elemental" ~attack:(mana_attack 0) ~life:36 ~description:"fire.elemental.description"
      ~effects:
        (effects
           [
             (Direct, damage_creatures ~is_ability:true 3);
             (Direct, focus (damage ~is_ability:true 3));
             (On_turn, add_mana 1 [ 0 ]);
           ])
      ();
    spell ~key:"fire.apocalypse" ~description:"fire.apocalypse.description"
      ~effects:(effects [ (Direct, armageddon) ])
      ();
    creature ~key:"fire.dragon" ~attack:(attack 9) ~life:41 ~description:"fire.dragon.description"
      ~mod_:(Spell_mod (fun x -> int_of_float (ceil (float_of_int x *. 1.5))))
      ();
  ]

let house =
  {
    h_key = "fire";
    h_name = "fire";
    h_cards = cards;
    h_index = 0;
    h_effects = [];
    h_house_id = 0;
    h_additional_cards = [];
  }
