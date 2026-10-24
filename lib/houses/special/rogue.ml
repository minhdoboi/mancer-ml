(* Special house Rogue — port of priv/sp/house/Rogue.scala. No listener; the extra attacks are custom
   [run_attack]s, same idea as Battlemage's charging knight. *)

open Sp
open Engine
open Common

(* Shadow Dancer: while a creature faces her she strikes twice.  The two blows
   are separate beats, so the board shows two lunges and two hits of 3
   instead of one lunge and a single hit of 6.  [blows] counts the blows that
   already landed, so the first one gets 0. *)
let dancer_strike blows target d player =
  let op = other_player player in
  let filled n = Option.is_some op.slots_update.slot_updates.(n).value in
  let blocked = List.exists filled target in
  ignore (single_target_attack target d player);
  (* the second blow follows only while the lane is still blocked *)
  blows = 0 && blocked && List.exists filled target

let dark_archer_attack target d player =
  if single_target_attack target d player then
    Player.inflict (other_player player) (mk_damage ~is_ability:true 3 d.context)

let sneak_attack : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = opp_slot_of_env env in
  let blocked = Option.is_some p.slots_update.slot_updates.(env.env_selected).value in
  let amount = if blocked then 8 else 4 in
  Slot.stun slot;
  Slot.inflict slot (spell_damage env amount)

let trickster_tick : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  if Option.is_some (Slot.opposite slot).value then (
    focus_slot slot;
    Player.inflict (other_of_env env) (ability_damage env 2))

let energy_steal : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  Option.iter
    (fun (opp : slot_state) ->
      focus_slot slot;
      Houses.incr_mana (other_of_env env).houses_update (-1) [ opp.card.c_card.house_index ])
    (Slot.opposite slot).value

let assassin_summon : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  let opp = Slot.opposite slot in
  if Option.is_some opp.value then (
    focus_slot slot;
    Slot.inflict opp (ability_damage env 8);
    if Option.is_none opp.value then
      Option.iter
        (fun (s : slot_state) ->
          let d =
            mk_damage s.attack
              { ctx_player = env.env_player; ctx_card = Some s.card.c_card; ctx_selected = slot.num }
          in
          ignore (single_target_attack s.target d (player_of_env env)))
        slot.value)

let arcane_trap : card_effect =
 fun env ->
  let op = other_of_env env in
  Houses.incr_mana op.houses_update (-2) [ 0; 1; 2; 3; 4 ];
  Slots.foreach op.slots_update Slot.stun;
  Player.add_desc_mod op skip_turn_mod;
  Player.add_effect op (On_end_turn, one_time_player_effect (remove_desc_mod skip_turn_mod))

let archer_summon : card_effect =
 fun env ->
  let op = other_of_env env in
  let killed_opposite = ref false in
  Slots.foreach op.slots_update (fun slot ->
      match slot.value with
      | Some s when s.life <= 6 ->
          if slot.num = env.env_selected then killed_opposite := true;
          Slot.destroy slot
      | _ -> ());
  if !killed_opposite then Player.inflict op (ability_damage env 3)

let fighter_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun damage ->
        let me = Reaction.bound_slot r in
        if Option.is_some me.value && damage.amount > 0 then
          let opp = Option.map (fun st -> st.card.c_card) (Slot.opposite me).value in
          match (damage.context.ctx_card, opp) with
          | Some c, Some o when c.id = o.id -> add_attack me (Attack_func (fun a -> a + 2))
          | _ -> ());
  }

let cards =
  [
    spell ~key:"rogue.sneak" ~description:"rogue.sneak.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, sneak_attack) ])
      ();
    creature ~key:"rogue.trickster" ~attack:(attack 4) ~life:20 ~description:"rogue.trickster.description"
      ~effects:(effects [ (On_turn, trickster_tick) ])
      ();
    creature ~key:"rogue.thief" ~attack:(attack 3) ~life:18 ~description:"rogue.thief.description"
      ~effects:(effects [ (On_turn, energy_steal) ])
      ();
    creature ~key:"rogue.dancer" ~attack:(attack 3) ~life:26 ~description:"rogue.dancer.description"
      ~run_attack:(custom_strikes_run_attack dancer_strike)
      ();
    creature ~key:"rogue.fighter" ~attack:(attack 3) ~life:31 ~description:"rogue.fighter.description"
      ~new_reaction:fighter_reaction ();
    creature ~key:"rogue.assassin" ~attack:(attack 9) ~life:35 ~description:"rogue.assassin.description"
      ~effects:(effects [ (Direct, assassin_summon) ])
      ();
    spell ~key:"rogue.trap" ~description:"rogue.trap.description"
      ~effects:(effects [ (Direct, arcane_trap) ])
      ();
    creature ~key:"rogue.archer" ~attack:(attack 6) ~life:42 ~description:"rogue.archer.description"
      ~effects:(effects [ (Direct, archer_summon) ])
      ~run_attack:(custom_run_attack dark_archer_attack)
      ();
  ]

let house = special_house ~key:"rogue" ~id:25 cards
