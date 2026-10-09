(* Special house Shadowcaster — port of priv/sp/house/Shadowcaster.scala. Dark power is a separate
   counter in player data, not the special mana that pays for cards. *)

open Sp
open Engine
open Common

let mage_ref : card option ref = ref None
let is_mage (c : card) = match !mage_ref with Some m -> c.id = m.id | None -> false
let get_power p = match p.pstate.ps_data with Shadowcaster_data x -> x | _ -> 0
let add_power p amount = Player.set_data p (Shadowcaster_data (max 0 (get_power p + amount)))

let count_kills opp f =
  let before =
    Array.to_list opp.slots_update.slot_updates
    |> List.filter_map (fun s -> if Option.is_some s.value then Some s.num else None)
  in
  f ();
  List.fold_left
    (fun n num -> if Option.is_none opp.slots_update.slot_updates.(num).value then n + 1 else n)
    0 before

let spell_tax : desc_mod =
 fun _ cards ->
  Array.map (fun cd -> if cd.cd_card.is_spell then { cd with cd_cost = cd.cd_cost + 1 } else cd) cards

let bat_attack target d player =
  let other = other_player player in
  let before =
    List.filter_map
      (fun n ->
        match other.slots_update.slot_updates.(n).value with Some st -> Some (n, st.life) | None -> None)
      target
  in
  ignore (single_target_attack target d player);
  let hits =
    List.fold_left
      (fun n (num, life) ->
        match other.slots_update.slot_updates.(num).value with
        | Some st when st.life < life -> n + 1
        | None -> n + 1
        | _ -> n)
      0 before
  in
  if hits > 0 && d.context.ctx_selected <> no_selection then
    for _ = 1 to hits do
      Slot.heal player.slots_update.slot_updates.(d.context.ctx_selected) 3
    done

let killer_attack target d player = if single_target_attack target d player then add_power player 1

let noble_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        let by_spell =
          match dead.d_damage with
          | Some d -> d.is_spell && d.context.ctx_player <> dead.d_player.pid
          | None -> false
        in
        if by_spell then add_power dead.d_player 2);
  }

let priestess_reaction () =
  let r = Reaction.default () in
  { r with on_my_death = (fun dead -> add_power dead.d_player 2) }

let mage_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num = me.num then Player.add_desc_mod me.slots.player spell_tax);
    on_my_remove = (fun _ -> Player.remove_desc_mod (Reaction.bound_slot r).slots.player spell_tax);
  }

let shadowcaster_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let old = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old c in
            (if c.cmd_card.is_spell then
               let mages =
                 Array.fold_left
                   (fun n s -> match s.value with Some st when is_mage st.card.c_card -> n + 1 | _ -> n)
                   0 p.slots_update.slot_updates
               in
               if mages > 0 then add_power p mages);
            r));
  }

let cards =
  [
    creature ~key:"shadowcaster.bat" ~attack:(attack 4) ~life:11 ~description:"shadowcaster.bat.description"
      ~run_attack:(custom_run_attack bat_attack) ();
    spell ~key:"shadowcaster.wave" ~description:"shadowcaster.wave.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 let x = get_power (player_of_env env) in
                 Slots.inflict_creatures (other_of_env env).slots_update (spell_damage env (x + 2)) );
           ])
      ();
    creature ~key:"shadowcaster.noble" ~attack:(attack 5) ~life:19
      ~description:"shadowcaster.noble.description" ~new_reaction:noble_reaction ();
    creature ~key:"shadowcaster.mage" ~attack:(attack 5) ~life:27 ~description:"shadowcaster.mage.description"
      ~mod_:(Spell_mod (fun x -> x + 2))
      ~new_reaction:mage_reaction ();
    spell ~key:"shadowcaster.scythe" ~description:"shadowcaster.scythe.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 let p = player_of_env env in
                 let op = other_of_env env in
                 let x = get_power p in
                 let kills =
                   count_kills op (fun () ->
                       Slots.inflict_creatures op.slots_update (spell_damage env (x + 4)))
                 in
                 for _ = 1 to kills do
                   Player.inflict op (spell_damage env 2)
                 done;
                 if kills > 0 then add_power p kills );
           ])
      ();
    creature ~key:"shadowcaster.assassin" ~attack:(attack 6) ~life:30
      ~description:"shadowcaster.assassin.description" ~status:run_flag
      ~run_attack:(custom_run_attack killer_attack) ();
    creature ~key:"shadowcaster.moonblade" ~attack:(attack 5) ~life:32
      ~description:"shadowcaster.moonblade.description"
      ~effects:
        (effects
           [
             ( On_turn,
               fun env ->
                 let p = player_of_env env in
                 let op = other_of_env env in
                 let x = get_power p in
                 if x > 0 && Slots.foldl op.slots_update false (fun acc _ -> acc || true) then (
                   focus_slot p.slots_update.slot_updates.(env.env_selected);
                   let kills =
                     count_kills op (fun () -> Slots.inflict_creatures op.slots_update (ability_damage env x))
                   in
                   if kills > 0 then add_power p kills) );
           ])
      ~run_attack:(custom_run_attack killer_attack) ();
    creature ~key:"shadowcaster.priestess" ~attack:(attack 10) ~life:31
      ~description:"shadowcaster.priestess.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 focus_slot (own_slot_of_env env);
                 Slots.foreach (other_of_env env).slots_update Slot.stun );
           ])
      ~new_reaction:priestess_reaction ();
  ]

let () = mage_ref := List.find_opt (fun (c : card) -> c.key = "shadowcaster.mage") cards

let house =
  special_house ~key:"shadowcaster" ~id:29 ~listener:shadowcaster_listener ~data:(Shadowcaster_data 0) cards
