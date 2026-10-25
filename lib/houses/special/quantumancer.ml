(* Special house Quantumancer — port of priv/sp/house/Quantumancer.scala. *)

open Sp
open Engine
open Common

let cosmic_phase = "cosmic replay phase"
let empty_data = { q_played = []; q_choice = None }
let get_data p = match p.pstate.ps_data with Quantum_data d -> d | _ -> empty_data
let set_data p d = Player.set_data p (Quantum_data d)

let hide_sealing : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> special_house_index then cards
  else
    Array.map
      (fun cd -> if cd.cd_card.key = "quantumancer.sealing" then { cd with cd_enabled = false } else cd)
      cards

let seal_mod (card : card) : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> card.house_index then cards
  else Array.map (fun cd -> if cd.cd_card.id = card.id then { cd with cd_enabled = false } else cd) cards

let sealed_cards (slot : slot_update) =
  match slot.value with
  | Some st -> (
      match st.data with Sealed (pairs, side) when side = slot.slots.player.pid -> pairs | _ -> [])
  | None -> []

let cosmic_choice (spells : card list) : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> special_house_index then
    Array.map (fun cd -> { cd with cd_enabled = false }) cards
  else
    (* Replace the column: the board only draws the first four rows of a house,
       so spells appended after the eight Quantumancer cards were never shown. *)
    List.map (fun s -> { cd_card = s; cd_cost = 0; cd_enabled = true; cd_extra_mana = 0 }) spells
    |> Array.of_list

let replayable p =
  let rec uniq acc seen = function
    | [] -> List.rev acc
    | c :: cs ->
        if List.exists (fun x -> x.id = c.id) seen then uniq acc seen cs else uniq (c :: acc) (c :: seen) cs
  in
  (get_data p).q_played |> List.rev
  |> List.filter_map (fun ps -> if ps.ps_card.is_spell && ps.ps_cost <= 10 then Some ps.ps_card else None)
  |> uniq [] []

let remove_choice p =
  match get_data p with
  | { q_choice = Some m; _ } as d ->
      Player.remove_desc_mod p m;
      set_data p { d with q_choice = None }
  | _ -> ()

let power_surge : card_effect =
 fun env ->
  let op = other_of_env env in
  Slots.foreach op.slots_update (fun slot ->
      match slot.value with
      | None -> ()
      | Some st ->
          let mana = op.houses_update.hu_value.(st.card.c_card.house_index).mana in
          Slot.inflict slot (spell_damage env (max mana 3)))

let fusion_turn : card_effect =
 fun env ->
  let op = other_of_env env in
  let idx =
    List.fold_left
      (fun best i ->
        if op.houses_update.hu_value.(i).mana < op.houses_update.hu_value.(best).mana then i else best)
      0 [ 1; 2; 3; 4 ]
  in
  incr_mana_from (own_slot_of_env env) op (-1) [ idx ];
  Updater.focus env.env_updater env.env_selected env.env_player true

(* Same input rules as [Game.available_commands]: a spell with no legal target
   would leave the phase with nothing to play. *)
let castable env p (c : card) =
  let u = env.env_updater in
  let op = other_player p in
  let filled (pl : player_update) =
    Array.exists (fun s -> Option.is_some s.value) pl.slots_update.slot_updates
  in
  let visible_foe =
    Array.exists
      (fun s -> match s.value with Some st -> st.data <> Stealthed | None -> false)
      op.slots_update.slot_updates
  in
  match c.input_spec with
  | No_input -> true
  | Select_owner_slot -> Slots.get_open p.slots_update <> []
  | Select_owner_creature -> filled p
  | Select_target_creature -> visible_foe
  | Select_target_slot -> Slots.get_open op.slots_update <> []
  | Select_any_creature -> filled p || visible_foe
  | Select_owner f -> f p.pid u.state <> []
  | Select_target f -> f op.pid u.state <> []

let cosmic_summon : card_effect =
 fun env ->
  let p = player_of_env env in
  (* the hand shows four rows per house: keep the most recent spells *)
  let rec last n l = if List.length l <= n then l else last n (List.tl l) in
  match last 4 (List.filter (castable env p) (replayable p)) with
  | [] -> ()
  | offers ->
      remove_choice p;
      let m = cosmic_choice offers in
      Player.add_desc_mod p m;
      set_data p { (get_data p) with q_choice = Some m };
      Player.add_transition p (Wait_player (env.env_player, Some cosmic_phase));
      Player.add_effect p (On_end_turn, one_time_player_effect (fun e -> remove_choice (player_of_env e)))

let record_opponent_spell p (c : command) =
  if c.cmd_card.is_spell then
    let d = get_data p in
    set_data p
      {
        d with
        q_played =
          d.q_played
          @ [
              {
                ps_card = c.cmd_card;
                ps_input = c.cmd_input;
                ps_target_other = c.cmd_flag = Some Target_other;
                ps_cost = c.cmd_card.cost;
              };
            ];
      }

let seal_opponent_card owner (card : card) =
  (* One SealMod per sealing slot, not one shared mod: concurrent copies each
     hold their own instance, so the first death cannot lift the other's seals
     (Scala shares a single SealMod across slots and leaks on first death). *)
  Slots.foreach owner.slots_update (fun slot ->
      match slot.value with
      | Some st when st.card.c_card.key = "quantumancer.sealing" ->
          let m = seal_mod card in
          Slot.set_data slot (Sealed (sealed_cards slot @ [ (card, m) ], owner.pid));
          Player.add_desc_mod (other_player owner) m
      | _ -> ())

let fusion_attack =
  Attack_state_func
    (fun a p -> a + Array.fold_left (fun m h -> max m h.mana) 0 (other_player p).houses_update.hu_value)

let dimension_attack target d player =
  let other = other_player player in
  let hit_creature = List.exists (fun n -> Option.is_some other.slots_update.slot_updates.(n).value) target in
  ignore (single_target_attack target d player);
  if hit_creature then Player.inflict other d

let kineticist_reaction () =
  let r = Reaction.default () in
  {
    r with
    inflict =
      (fun s d ->
        if is_effect d && d.context.ctx_player <> s.slots.player.pid && d.amount > 0 then (
          Slot.heal s d.amount;
          add_attack s (Attack_func (fun a -> a + 1));
          focus_slot ~blocking:false s)
        else Slot.damage_slot s d);
  }

let any_absorbing (p : player_update) =
  Array.exists
    (fun s -> match s.value with Some st -> st.data = Custom "martyr-absorb" | None -> false)
    p.slots_update.slot_updates

let martyr_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun de ->
        let me = Reaction.bound_slot r in
        let enemy_spell = de.de_damage.is_spell && de.de_damage.context.ctx_player <> me.slots.player.pid in
        let not_self = match de.de_target with Some n -> n <> me.num | None -> false in
        if enemy_spell && not_self && de.de_damage.amount > 0 && not (any_absorbing me.slots.player) then (
          let prev = match me.value with Some st -> st.data | None -> No_data in
          Slot.set_data me (Custom "martyr-absorb");
          Fun.protect ~finally:(fun () -> Slot.set_data me prev) (fun () -> Slot.damage_slot me de.de_damage);
          { de.de_damage with amount = 0 })
        else de.de_damage);
  }

let phosphor_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun d ->
        if not (is_effect d) then
          match d.context.ctx_card with
          | Some c when c.cost > 0 ->
              let me = Reaction.bound_slot r in
              let foe = other_player me.slots.player in
              let hits =
                Array.to_list foe.slots_update.slot_updates
                |> List.filter_map (fun s -> if Option.is_some s.value then Some s.num else None)
              in
              emit me.slots.player.updater
                (Ev_ability
                   {
                     kind = "quantumancer.phosphor";
                     player = me.slots.player.pid;
                     num = me.num;
                     target = (match hits with n :: _ -> Some (foe.pid, n) | [] -> None);
                   });
              let ctx =
                {
                  ctx_player = me.slots.player.pid;
                  ctx_card = Option.map (fun st -> st.card.c_card) me.value;
                  ctx_selected = me.num;
                }
              in
              Slots.foreach foe.slots_update (fun s -> Slot.inflict s (mk_damage ~is_ability:true c.cost ctx));
              focus_slot me
          | _ -> ());
  }

let sealing_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num = me.num then (
          let owner = me.slots.player in
          if not (has_desc_mod owner hide_sealing) then Player.add_desc_mod owner hide_sealing;
          let pairs = sealed_cards me in
          Slot.set_data me (Sealed (pairs, owner.pid));
          List.iter (fun (_, m) -> Player.add_desc_mod (other_player owner) m) pairs));
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        let owner = me.slots.player in
        let others =
          Array.exists
            (fun s ->
              s.num <> me.num
              && match s.value with Some st -> st.card.c_card.key = "quantumancer.sealing" | None -> false)
            me.slots.slot_updates
        in
        if not others then Player.remove_desc_mod owner hide_sealing;
        List.iter (fun (_, m) -> Player.remove_desc_mod (other_player owner) m) (sealed_cards me));
  }

let wrap_protect (p : player_update) =
  Array.iter
    (fun (slot : slot_update) ->
      let old = slot.protect_hook in
      slot.protect_hook <-
        (fun d ->
          let d = old d in
          Slots.foldl p.slots_update d (fun acc s ->
              match s.value with
              | Some st ->
                  st.reaction.on_protect { de_damage = acc; de_target = Some slot.num; de_player = p }
              | None -> acc)))
    p.slots_update.slot_updates

let quantum_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        wrap_protect p;
        let old = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old c in
            if c.cmd_card.key <> "quantumancer.cosmic" then remove_choice p;
            r);
        let op = other_player p in
        let old_opp = op.submit_command_hook in
        op.submit_command_hook <-
          (fun c ->
            let r = old_opp c in
            record_opponent_spell p c;
            if
              Array.exists
                (fun s ->
                  match s.value with Some st -> st.card.c_card.key = "quantumancer.sealing" | None -> false)
                p.slots_update.slot_updates
            then seal_opponent_card p c.cmd_card;
            r));
  }

let cards =
  [
    spell ~key:"quantumancer.surge" ~description:"quantumancer.surge.description"
      ~effects:(effects [ (Direct, power_surge) ])
      ();
    creature ~key:"quantumancer.kineticist" ~attack:(attack 2) ~life:17
      ~description:"quantumancer.kineticist.description" ~new_reaction:kineticist_reaction ();
    creature ~key:"quantumancer.warrior" ~attack:(attack 4) ~life:20
      ~description:"quantumancer.warrior.description" ~run_attack:(custom_run_attack dimension_attack) ();
    creature ~key:"quantumancer.martyr" ~attack:(attack 5) ~life:20
      ~description:"quantumancer.martyr.description" ~new_reaction:martyr_reaction ();
    creature ~key:"quantumancer.wall" ~attack:(attack 0) ~life:21 ~description:"quantumancer.wall.description"
      ~new_reaction:phosphor_reaction ();
    creature ~key:"quantumancer.sealing" ~attack:(attack 6) ~life:34
      ~description:"quantumancer.sealing.description" ~new_reaction:sealing_reaction ();
    creature ~key:"quantumancer.fusion"
      ~attack:{ base = None; sources = [ fusion_attack ] }
      ~life:36 ~description:"quantumancer.fusion.description"
      ~effects:(effects [ (On_turn, fusion_turn) ])
      ();
    creature ~key:"quantumancer.cosmic" ~attack:(attack 7) ~life:40
      ~description:"quantumancer.cosmic.description"
      ~effects:(effects [ (Direct, cosmic_summon) ])
      ();
  ]

let house =
  special_house ~key:"quantumancer" ~id:32 ~listener:quantum_listener ~data:(Quantum_data empty_data) cards
