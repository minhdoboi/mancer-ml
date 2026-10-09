(* Special house Conjurer — port of priv/sp/house/Conjurer.scala. Spells trigger onConjure on the
   owner's War mage, Bone golem, Unholy guard and Dark mystic; Nurture heals after those resolve. *)

open Sp
open Engine
open Common

let is_wall (s : slot_update) =
  match s.value with Some st -> st.card.c_body.b_attack.base = Some 0 | None -> false

let filleds su = Array.to_list su.slot_updates |> List.filter (fun s -> Option.is_some s.value)

let until_eot player amount slots =
  let bonus = Attack_func (fun a -> max 0 (a + amount)) in
  List.iter (fun s -> if Option.is_some s.value && not (is_wall s) then add_attack s bonus) slots;
  Player.add_effect player (On_end_turn, one_time_player_effect (remove_attack bonus))

let nectar : card_effect =
 fun env ->
  let target = if env.env_target_other then other_of_env env else player_of_env env in
  let slot = target.slots_update.slot_updates.(env.env_selected) in
  match slot.value with
  | None -> ()
  | Some st ->
      Slot.heal slot 5;
      Houses.incr_mana (player_of_env env).houses_update 1 [ st.card.c_card.house_index ]

let bravery : card_effect =
 fun env ->
  let p = player_of_env env in
  until_eot p 1 (filleds p.slots_update);
  Houses.incr_mana p.houses_update 1 [ 4 ]

let war_mage_summon : card_effect =
 fun env -> until_eot (player_of_env env) 2 (filleds (player_of_env env).slots_update)

let nurture : card_effect = fun env -> Slots.foreach (player_of_env env).slots_update Slot.stun

let nurture_heal (p : player_update) =
  let total = ref 0 in
  Slots.foreach p.slots_update (fun slot ->
      match slot.value with
      | None -> ()
      | Some st ->
          let atk = Slots.current_attack slot st in
          total := !total + atk;
          Slot.heal slot (atk + 4));
  Player.heal p (!total + 4)

let pain : card_effect =
 fun env ->
  let slot = (other_of_env env).slots_update.slot_updates.(env.env_selected) in
  match slot.value with
  | None -> ()
  | Some st ->
      let extra = 2 * (other_of_env env).houses_update.hu_value.(st.card.c_card.house_index).mana in
      let d = mk_damage ~is_spell:true (3 + extra) (ctx_of_env env) in
      Slot.inflict slot d;
      Player.inflict (other_of_env env) d

let guard_summon : card_effect =
 fun env -> until_eot (other_of_env env) (-3) (filleds (other_of_env env).slots_update)

let mystic_summon : card_effect = fun env -> Houses.incr_mana (player_of_env env).houses_update 2 [ 0; 1; 2; 3 ]

let on_conjure (slot : slot_update) =
  match slot.value with
  | None -> ()
  | Some st -> (
      match st.card.c_card.key with
      | "conjurer.warmage" ->
          let others = filleds slot.slots |> List.filter (fun s -> s.num <> slot.num) in
          until_eot slot.slots.player 2 others
      | "conjurer.golem" ->
          add_attack slot (Attack_func (fun a -> a + 1));
          Slots.foreach slot.slots (fun s -> if s.num <> slot.num then Slot.heal s 3)
      | "conjurer.guard" ->
          let opp = other_player slot.slots.player in
          Slots.foreach opp.slots_update (fun s ->
              if Option.is_some s.value && not (is_wall s) then
                add_attack s (Attack_func (fun a -> max 0 (a - 1))))
      | "conjurer.mystic" -> Houses.incr_mana slot.slots.player.houses_update 2 [ 4 ]
      | _ -> ())

let bone_golem_reaction () =
  let r = Reaction.default () in
  { r with self_protect = (fun d -> if is_effect d then { d with amount = 0 } else d) }

let conjurer_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let old = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old c in
            if c.cmd_card.is_spell then (
              Slots.foreach p.slots_update on_conjure;
              if c.cmd_card.key = "conjurer.nurture" then nurture_heal p);
            r));
  }

let cards =
  [
    spell ~key:"conjurer.nectar" ~description:"conjurer.nectar.description" ~input_spec:Select_any_creature
      ~effects:(effects [ (Direct, nectar) ])
      ();
    spell ~key:"conjurer.bravery" ~description:"conjurer.bravery.description"
      ~effects:(effects [ (Direct, bravery) ])
      ();
    creature ~key:"conjurer.warmage" ~attack:(attack 1) ~life:26 ~description:"conjurer.warmage.description"
      ~effects:(effects [ (Direct, war_mage_summon) ])
      ();
    creature ~key:"conjurer.golem" ~attack:(attack 4) ~life:18 ~description:"conjurer.golem.description"
      ~new_reaction:bone_golem_reaction ();
    spell ~key:"conjurer.nurture" ~description:"conjurer.nurture.description"
      ~effects:(effects [ (Direct, nurture) ])
      ();
    spell ~key:"conjurer.pain" ~description:"conjurer.pain.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, pain) ])
      ();
    creature ~key:"conjurer.guard" ~attack:(attack 6) ~life:44 ~description:"conjurer.guard.description"
      ~effects:(effects [ (Direct, guard_summon) ])
      ();
    creature ~key:"conjurer.mystic" ~attack:(attack 5) ~life:58 ~description:"conjurer.mystic.description"
      ~effects:(effects [ (Direct, mystic_summon) ])
      ();
  ]

let house =
  {
    h_key = "conjurer";
    h_name = "conjurer";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 31;
    h_additional_cards = [];
    h_make_listener = conjurer_listener;
    h_data = No_player_data;
  }
