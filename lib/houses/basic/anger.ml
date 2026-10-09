(* Anger house — a variant of fire (priv/sp/house/Fire.scala).  Same place
   (basic house index 0) and the same card-rule shape as fire, with its own
   keys and numbers so it can be customised without touching fire. *)

open Sp
open Engine
open Common

(* Troll: every turn, 2 damage to the owner's adjacent creatures. *)
let troll_rage : card_effect =
 fun env ->
  let d = ability_damage env 2 in
  let selected = own_slot_of_env env in
  match Slot.filled_adjacent selected with
  | [] -> ()
  | targets ->
      Updater.focus env.env_updater env.env_selected env.env_player true;
      List.iter (fun s -> Slot.inflict s d) targets

(* Scorching Voice: 18 to the target, 10 to the rest of the opponent's board. *)
let scorching_voice : card_effect =
 fun env ->
  let damage amount = spell_damage env amount in
  Slots.foreach (other_of_env env).slots_update (fun s ->
      Slot.inflict s (damage (if s.num = env.env_selected then 18 else 10)))

(* Werewolf: while its owner has 15 life or less, a spell that wounds it sends
   it at the opposite slot, out of the normal attack phase.  The creature has
   to survive the hit.  An out-of-turn blow must not wait for the attack phase
   to ready the creature, so the run flag is lent for this one swing and put
   back exactly as it was: with an occupied opposite slot it strikes that card,
   with an empty one it strikes the enemy player (Single_target's fallback,
   the same path Mountain King's swings take). *)
let strike_out_of_turn (me : slot_update) =
  let ready = match me.value with Some st -> has_flag st run_flag | None -> false in
  if not ready then Slot.toggle me run_flag;
  Option.iter (fun st -> Player.run_slot me.slots.player me.num st) me.value;
  if not ready then Slot.toggle_off me run_flag

let werewolf_retaliation () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun d ->
        let me = Reaction.bound_slot r in
        if d.is_spell && d.amount > 0 && me.slots.player.pstate.ps_life <= 15 then
          match me.value with
          | Some st when st.life - d.amount > 0 ->
              Slot.set_target me [ me.num ];
              strike_out_of_turn me
          | _ -> ());
  }

(* Outburst of Anger: counts anger mana from before the spell was paid, as the
   description shows. *)
let outburst : card_effect =
 fun env ->
  let amount = get_mana env 0 + env.env_paid_cost + 8 in
  Player.inflict (other_of_env env) (spell_damage env amount);
  mass_damage ~is_spell:true amount env

(* Nemesis: 3 damage to every opponent creature when summoned, rising to 5
   while its owner has 15 life or less. *)
let nemesis_sweep : card_effect =
 fun env ->
  let amount = if (player_of_env env).pstate.ps_life <= 15 then 5 else 3 in
  Slots.inflict_creatures (other_of_env env).slots_update (ability_damage env amount)

(* Anger Incarnation: a single blow of 9 or more makes its owner gain 1 anger
   mana.  The reaction is bound to the dragon's own slot, so the arc the UI
   draws starts from it. *)
let dragon_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun d ->
        let me = Reaction.bound_slot r in
        if d.amount >= 9 then incr_mana_from me me.slots.player 1 [ 0 ]);
  }

(* War Chief style lasting bonus: +1 attack to the owner's cards. *)
let bonus_reaction tag ~cond ~plus () =
  attack_bonus_reaction ~cond
    ~get_bonus:(fun pos -> Attack_bonus { tag; pos; bonus = (fun a -> a + plus) })
    ()

let cards =
  [
    creature ~key:"anger.werewolf" ~attack:(attack 4) ~life:16 ~description:"anger.werewolf.description"
      ~new_reaction:werewolf_retaliation
      ~effects:(effects [ (On_turn, troll_rage) ])
      ();
    creature ~key:"anger.hotspot" ~attack:(attack 0) ~life:5 ~description:"anger.hotspot.description"
      ~effects:(effects [ (Direct, damage_creatures ~is_ability:true 5) ])
      ();
    creature ~key:"anger.sentinel" ~attack:(attack 3) ~life:13 ~description:"anger.sentinel.description"
      ~new_reaction:(mana_growth_reaction ~owner:true ~amount:1 ~house_indexes:[ 0 ])
      ();
    creature ~key:"anger.oni" ~attack:(attack 4) ~life:19 ~description:"anger.oni.description"
      ~status:run_flag ();
    creature ~key:"anger.chainbreaker" ~attack:(attack 3) ~life:15
      ~description:"anger.chainbreaker.description"
      ~new_reaction:(bonus_reaction "chainbreaker" ~cond:(fun pos num -> abs (pos - num) = 1) ~plus:2)
      ();
    spell ~key:"anger.wave" ~description:"anger.wave.description"
      ~effects:(effects [ (Direct, damage_creatures ~is_spell:true 9) ])
      ();
    creature ~key:"anger.gorilla" ~attack:(attack 6) ~life:20 ~description:"anger.gorilla.description"
      ~new_reaction:(bonus_reaction "gorilla" ~cond:( <> ) ~plus:1)
      ();
    creature ~key:"anger.boar" ~attack:(attack 8) ~life:26 ~description:"anger.boar.description"
      ~effects:(effects [ (Direct, mass_damage ~is_ability:true ~immune_self:true 5) ])
      ();
    spell ~key:"anger.inferno" ~description:"anger.inferno.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, scorching_voice) ])
      ();
    creature ~key:"anger.nemesis" ~attack:(mana_attack 0) ~life:36
      ~description:"anger.nemesis.description"
      ~effects:
        (effects
           [
             (Direct, nemesis_sweep);
             (Direct, focus (damage ~is_ability:true 3));
             (On_turn, add_mana 1 [ 0 ]);
           ])
      ();
    creature ~key:"anger.dragon" ~attack:(attack 9) ~life:41 ~description:"anger.dragon.description"
      ~mod_:(Spell_mod (fun x -> int_of_float (ceil (float_of_int x *. 4.0 /. 3.0))))
      ~new_reaction:dragon_reaction ();
    spell ~key:"anger.apocalypse" ~description:"anger.apocalypse.description"
      ~desc_args:(fun state pid -> [ string_of_int (8 + state.players.(pid).ps_houses.(0).mana) ])
      ~effects:(effects [ (Direct, outburst) ])
      ();
  ]

let house =
  {
    h_key = "anger";
    h_name = "anger";
    h_cards = cards;
    h_index = 0;
    h_effects = [];
    h_house_id = 1;
    h_additional_cards = [];
    h_make_listener = (fun () -> default_listener);
    h_data = No_player_data;
  }
