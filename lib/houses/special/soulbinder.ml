(* Special house Soulbinder — port of priv/sp/house/Soulbinder.scala. Bound Souls are empty slots in
   player data; creatures must land on one, and summoning there consumes it. *)

open Sp
open Engine
open Common

let empty_souls = []
let souls_of = function Soulbinder_data xs -> xs | _ -> empty_souls
let get_souls p = souls_of p.pstate.ps_data

let hide_special_creature_mod : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> special_house_index then cards
  else
    Array.map
      (fun cd -> if cd.cd_card.is_spell || cd.cd_card.is_aura then cd else { cd with cd_enabled = false })
      cards

let set_souls p souls =
  match p.pstate.ps_data with
  | Soulbinder_data _ -> Player.set_data p (Soulbinder_data (List.sort_uniq compare souls))
  | _ -> ()

let add_soul p x = if List.mem x (get_souls p) then () else set_souls p (x :: get_souls p)

let add_soul_random (p : player_update) =
  let souls = get_souls p in
  let open_slots = List.filter (fun n -> not (List.mem n souls)) (Slots.get_open p.slots_update) in
  match open_slots with
  | [] -> false
  | ns ->
      let num = List.nth ns (Dice.int p.updater.rng (List.length ns)) in
      add_soul p num;
      true

let select_bound_soul pid (state : game_state) =
  let ps = state.players.(pid) in
  souls_of ps.ps_data |> List.filter (fun x -> Option.is_none ps.ps_slots.(x) && List.mem x ps.ps_slot_list)

let open_slot_no_bound_soul pid (state : game_state) =
  let ps = state.players.(pid) in
  let souls = souls_of ps.ps_data in
  List.filter (fun x -> Option.is_none ps.ps_slots.(x) && not (List.mem x souls)) ps.ps_slot_list

let total_souls env =
  List.length (get_souls (player_of_env env))
  + match (other_of_env env).pstate.ps_data with Soulbinder_data xs -> List.length xs | _ -> 0

let sheppard_source = Attack_state_func (fun a p -> a + List.length (get_souls p))

let tethered_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death = (fun _ -> add_soul (Reaction.bound_slot r).slots.player (Reaction.bound_slot r).num);
  }

let priestess_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        if add_soul_random me.slots.player then focus_slot me);
  }

let soul_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        init_owner_death p;
        let old_data = p.set_data_hook in
        p.set_data_hook <-
          (fun d ->
            old_data d;
            match d with
            | Soulbinder_data souls ->
                if souls = [] then (
                  if not (has_desc_mod p hide_special_creature_mod) then
                    Player.add_desc_mod p hide_special_creature_mod)
                else if has_desc_mod p hide_special_creature_mod then
                  Player.remove_desc_mod p hide_special_creature_mod
            | _ -> ());
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.add_hook in
            slot.add_hook <-
              (fun st ->
                old st;
                if List.mem slot.num (get_souls p) then
                  set_souls p (List.filter (( <> ) slot.num) (get_souls p))))
          p.slots_update.slot_updates);
  }

let cards =
  [
    creature ~key:"soulbinder.tethered" ~attack:(attack 4) ~life:17
      ~description:"soulbinder.tethered.description" ~input_spec:(Select_owner select_bound_soul)
      ~new_reaction:tethered_reaction ();
    spell ~key:"soulbinder.summon" ~description:"soulbinder.summon.description"
      ~input_spec:(Select_owner open_slot_no_bound_soul)
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 add_soul (player_of_env env) env.env_selected;
                 Houses.incr_mana (player_of_env env).houses_update 1 [ 0; 1; 2; 3 ] );
           ])
      ();
    creature ~key:"soulbinder.tree" ~attack:(attack 5) ~life:21 ~description:"soulbinder.tree.description"
      ~input_spec:(Select_owner select_bound_soul)
      ~effects:
        (effects
           [
             ( On_turn,
               fun env ->
                 let slot = own_slot_of_env env in
                 let souls = get_souls (player_of_env env) in
                 List.find_opt
                   (fun s -> Option.is_none s.value && not (List.mem s.num souls))
                   (Slot.open_adjacent slot)
                 |> Option.iter (fun s -> add_soul (player_of_env env) s.num) );
           ])
      ();
    creature ~key:"soulbinder.seeker" ~attack:(attack 6) ~life:26 ~description:"soulbinder.seeker.description"
      ~input_spec:(Select_owner select_bound_soul)
      ~effects:
        (effects
           [
             ( On_end_turn,
               fun env ->
                 let p = player_of_env env in
                 match Slots.get_open p.slots_update with
                 | [] -> ()
                 | ns ->
                     let num = List.nth ns (Dice.int env.env_updater.rng (List.length ns)) in
                     Slots.move p.slots_update env.env_selected num;
                     add_soul p env.env_selected );
           ])
      ();
    spell ~key:"soulbinder.release" ~description:"soulbinder.release.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 let p = player_of_env env in
                 let x = 3 * total_souls env in
                 Player.heal p x;
                 Slots.heal_creatures p.slots_update x;
                 set_souls p empty_souls;
                 (match (other_of_env env).pstate.ps_data with
                 | Soulbinder_data _ -> set_souls (other_of_env env) empty_souls
                 | _ -> ());
                 ignore (add_soul_random p);
                 ignore (add_soul_random p) );
           ])
      ();
    creature ~key:"soulbinder.sheppard"
      ~attack:{ base = Some 4; sources = [ sheppard_source ] }
      ~life:39 ~description:"soulbinder.sheppard.description" ~input_spec:(Select_owner select_bound_soul)
      ~effects:
        (effects
           [
             ( On_turn,
               fun env ->
                 let n = List.length (get_souls (player_of_env env)) in
                 (focus (fun e -> Player.heal (player_of_env e) (2 * n))) env );
           ])
      ();
    creature ~key:"soulbinder.priestess" ~attack:(attack 7) ~life:37
      ~description:"soulbinder.priestess.description" ~input_spec:(Select_owner select_bound_soul)
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 let total = total_souls env in
                 Player.heal (player_of_env env) (2 * total);
                 Player.inflict (other_of_env env) (ability_damage env (2 * total)) );
           ])
      ~new_reaction:priestess_reaction ();
    creature ~key:"soulbinder.dragon" ~attack:(attack 6) ~life:44 ~description:"soulbinder.dragon.description"
      ~input_spec:(Select_owner select_bound_soul)
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 focus_slot (own_slot_of_env env);
                 let d = ability_damage env 6 in
                 Player.inflict (other_of_env env) d;
                 Slots.inflict_creatures (other_of_env env).slots_update d );
             ( On_turn,
               fun env ->
                 let amount = 3 * List.length (get_souls (player_of_env env)) in
                 Slot.heal (own_slot_of_env env) amount;
                 Player.heal (player_of_env env) amount );
           ])
      ();
  ]

let house =
  special_house ~key:"soulbinder" ~id:28
    ~effects:[ (On_start, fun env -> ignore (add_soul_random (player_of_env env))) ]
    ~listener:soul_listener ~data:(Soulbinder_data empty_souls) cards
