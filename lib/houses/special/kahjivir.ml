(* Special house Kahjivir — cat-girl huntresses (from Valentyne).

   The house has no special mana or hidden state: every card reads the slot
   opposite it and either attacks, buffs, or punishes an empty lane.  The two
   spells that grant extra attacks remove their temporary sources at the end of
   the turn. *)

open Sp
open Engine
open Common

let house_id = 45

let is_huntress (c : card) = c.house_id = house_id

let filled_total (p : player_update) =
  Slots.foldl p.slots_update p.pstate.ps_life (fun total s -> total + (Slot.get s).life)

(* Same as Player.run_attack, but on an explicit old run-attack record.  The
   hunger wrapper needs to run the original attack without recursing through
   the wrapper that now sits in the slot's card body. *)
let apply_attack (run : run_attack) target d (p : player_update) =
  let op = other_player p in
  match run.apply_ with
  | Custom f -> f target d p
  | Multi_target ->
      Player.inflict op d;
      Slots.inflict_creatures op.slots_update d
  | Single_target ->
      if target = [] then Player.inflict op d
      else
        let hits =
          List.filter
            (fun num ->
              let slot = op.slots_update.slot_updates.(num) in
              let filled = Option.is_some slot.value in
              if filled then Slot.inflict slot d;
              filled)
            target
        in
        if List.length hits < List.length target then Player.inflict op d

let remove_extra_at_end (p : player_update) ids =
  Player.add_effect p
    ( On_end_turn,
      one_time_player_effect (fun env ->
          let p = player_of_env env in
          List.iter
            (fun id ->
              Option.iter
                (fun slot -> remove_first_attack slot (Extra_attacks 1))
                (Slots.find_slot_id p.slots_update id))
            ids) )

(* ----------------------------------------------------------- spells *)

let hunger : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = own_slot_of_env env in
  match slot.value with
  | Some st when is_huntress st.card.c_card ->
      let extra = Extra_attacks 1 in
      add_attack slot extra;
      let old_run = st.card.c_body.b_run_attack in
      let wrapper target d player =
        let op = other_player player in
        let before = filled_total op in
        apply_attack old_run target d player;
        let dealt = before - filled_total op in
        if dealt > 0 then
          Option.iter (fun me -> Slot.heal me dealt) (Slots.find_slot_id player.slots_update st.slot_id)
      in
      let wrapped = { old_run with apply_ = Custom wrapper } in
      Slot.update slot (fun cur ->
          { cur with card = { cur.card with c_body = { cur.card.c_body with b_run_attack = wrapped } } });
      Player.add_effect p
        ( On_end_turn,
          one_time_player_effect (fun e ->
              let player = player_of_env e in
              match Slots.find_slot_id player.slots_update st.slot_id with
              | None -> ()
              | Some me ->
                  remove_first_attack me extra;
                  Slot.update me (fun cur ->
                      if cur.card.c_body.b_run_attack == wrapped then
                        {
                          cur with
                          card = { cur.card with c_body = { cur.card.c_body with b_run_attack = old_run } };
                        }
                      else cur) ) )
  | _ -> ()

let ambush : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = own_slot_of_env env in
  match slot.value with
  | Some st when is_huntress st.card.c_card ->
      add_attack slot (Attack_func (fun a -> a + 2));
      let adjacent = Slot.filled_adjacent slot in
      List.iter (fun s -> add_attack s (Extra_attacks 1)) adjacent;
      remove_extra_at_end p (List.map (fun s -> (Slot.get s).slot_id) adjacent)
  | _ -> ()

(* -------------------------------------------------------- creatures *)

let stalker_summon : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match (Slot.opposite slot).value with
  | Some _ -> Slot.toggle_run slot
  | None -> Player.inflict (other_of_env env) (ability_damage env 3)

let javelin_attack =
  Attack_slot_state_func (fun a slot -> if Option.is_some (Slot.opposite slot).value then a + 3 else a)

let spirit_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        match (Slot.opposite me).value with
        | Some _ -> if not d.is_spell then { d with amount = max 0 (d.amount - 2) } else d
        | None -> if d.is_spell then { d with amount = 0 } else d);
  }

(* Each attack raises her attack permanently.  Against a creature she makes
   two attacks, and the second one uses the attack she has just gained. *)
let tribe_attack target d (p : player_update) =
  let op = other_player p in
  let occupied =
    List.exists (fun num -> Option.is_some op.slots_update.slot_updates.(num).value) target
  in
  let own_slot () = p.slots_update.slot_updates.(d.context.ctx_selected) in
  let hit_once () =
    let d = match (own_slot ()).value with Some st -> { d with amount = st.attack } | None -> d in
    ignore (single_target_attack target d p);
    match (own_slot ()).value with
    | Some _ -> add_attack (own_slot ()) (Attack_func (fun a -> a + 1))
    | None -> ()
  in
  hit_once ();
  if occupied then (
    Player.refresh_slots p;
    hit_once ())

let archer_attack =
  Attack_slot_state_func (fun a slot -> if Option.is_none (Slot.opposite slot).value then a + 6 else a)

let archer_turn : card_effect =
 fun env ->
  let me = own_slot_of_env env in
  if Option.is_some (Slot.opposite me).value then (
    focus_slot me;
    Slots.inflict_creatures (other_of_env env).slots_update (ability_damage env 3))

let head_summon : card_effect =
 fun env ->
  let me = own_slot_of_env env in
  match (Slot.opposite me).value with
  | Some st when st.life <= 25 ->
      focus_slot me;
      Slot.destroy (Slot.opposite me)
  | Some _ -> ()
  | None ->
      let op = other_of_env env in
      let d = ability_damage env 8 in
      Player.inflict op d;
      Slots.inflict_creatures op.slots_update d

let cards =
  [
    spell ~key:"kahjivir.hunger" ~description:"kahjivir.hunger.description"
      ~input_spec:Select_owner_creature ~effects:(effects [ (Direct, hunger) ])
      ();
    spell ~key:"kahjivir.ambush" ~description:"kahjivir.ambush.description"
      ~input_spec:Select_owner_creature ~effects:(effects [ (Direct, ambush) ])
      ();
    creature ~key:"kahjivir.stalker" ~attack:(attack 5) ~life:20
      ~description:"kahjivir.stalker.description" ~effects:(effects [ (Direct, stalker_summon) ])
      ();
    creature ~key:"kahjivir.javelin" ~attack:{ base = Some 4; sources = [ javelin_attack ] } ~life:29
      ~description:"kahjivir.javelin.description" ();
    creature ~key:"kahjivir.dancer" ~attack:(attack 6) ~life:27
      ~description:"kahjivir.dancer.description" ~new_reaction:spirit_reaction ();
    creature ~key:"kahjivir.shaman" ~attack:(attack 3) ~life:39
      ~description:"kahjivir.shaman.description" ~run_attack:(custom_run_attack tribe_attack)
      ();
    creature ~key:"kahjivir.archer" ~attack:{ base = Some 3; sources = [ archer_attack ] } ~life:40
      ~description:"kahjivir.archer.description" ~effects:(effects [ (On_turn, archer_turn) ])
      ();
    creature ~key:"kahjivir.headhuntress" ~attack:(attack 8) ~life:48
      ~description:"kahjivir.headhuntress.description" ~effects:(effects [ (Direct, head_summon) ])
      ();
  ]

let house = special_house ~key:"kahjivir" ~id:house_id cards
