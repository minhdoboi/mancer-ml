(* Special house Mountain King — port of priv/sp/house/MountainKing.scala. Adjacent dwarves form a
   hird. *)

open Sp
open Engine
open Common

let house_id = 38
let is_dwarf (c : card) = c.house_id = house_id
let soldier_ref : card option ref = ref None
let shieldman_ref : card option ref = ref None
let armour_ref : card option ref = ref None
let berserker_ref : card option ref = ref None
let ballista_ref : card option ref = ref None
let is_hird_of (st : slot_state) = st.data = Hird
let berserker_extras : (int, int ref) Hashtbl.t = Hashtbl.create 16

let lower_special_cost_mod : desc_mod =
 fun house cards ->
  if house.phd_house.h_index = special_house_index then
    Array.map (fun c -> { c with cd_cost = max 0 (c.cd_cost - 1) }) cards
  else cards

let soldier_lower =
  Attack_slot_state_func
    (fun atk slot ->
      let opp = Slot.opposite slot in
      match opp.value with
      | Some s when is_card !soldier_ref s.card.c_card && is_hird_of s ->
          let n =
            Array.fold_left
              (fun acc s ->
                match s.value with Some st when is_card !soldier_ref st.card.c_card -> acc + 1 | _ -> acc)
              0 opp.slots.slot_updates
          in
          max 0 (atk - n)
      | _ -> atk)

let rune_attack =
  Attack_slot_state_func
    (fun atk slot ->
      match slot.value with
      | Some st when is_hird_of st ->
          let n =
            Array.fold_left
              (fun acc s -> match s.value with Some v when is_dwarf v.card.c_card -> acc + 1 | _ -> acc)
              0 slot.slots.slot_updates
          in
          atk + n
      | _ -> atk)

let berserker_attack =
  Attack_slot_state_func
    (fun atk slot -> match slot.value with Some st when is_hird_of st -> atk + 3 | _ -> atk)

let soldier_effect : card_effect =
 fun env ->
  let bonus = Attack_func (fun a -> a + 3) in
  let selected = env.env_selected in
  List.iter
    (fun s -> add_attack s bonus)
    (Slot.filled_adjacent (player_of_env env).slots_update.slot_updates.(selected));
  Player.add_effect (player_of_env env)
    ( On_end_turn,
      countdown 1 (fun e ->
          List.iter
            (fun s -> remove_first_attack s bonus)
            (Slot.filled_adjacent (player_of_env e).slots_update.slot_updates.(selected))) )

let mountain_direct : card_effect =
 fun env ->
  match reduce_filled (other_of_env env).slots_update strongest with
  | None -> ()
  | Some s -> Slot.toggle s stun_flag

let set_soldier_hird (s : slot_update) b (other : player_update) =
  let opp = other.slots_update.slot_updates.(s.num) in
  if b && Option.is_some opp.value && not (has_source opp (fun x -> x == soldier_lower)) then
    add_attack opp soldier_lower

let soldier_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        let opp = Slot.opposite me in
        if has_source opp (fun x -> x == soldier_lower) then remove_first_attack opp soldier_lower);
  }

let rune_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        let from_opp_creature =
          d.context.ctx_selected = me.num
          && match d.context.ctx_card with Some c -> not c.is_spell | None -> false
        in
        if Option.is_some (Slot.opposite me).value && not from_opp_creature then { d with amount = 0 } else d);
  }

let shieldman_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun e ->
        let me = Reaction.bound_slot r in
        match e.de_target with
        | Some num
          when abs (num - me.num) = 1
               &&
               match e.de_damage.context.ctx_card with
               | Some c -> not (is_card !shieldman_ref c)
               | None -> true ->
            let amount = e.de_damage.amount in
            let samount = int_of_float (ceil (float_of_int amount /. 2.)) in
            let ctx =
              { ctx_player = me.slots.player.pid; ctx_card = !shieldman_ref; ctx_selected = me.num }
            in
            Slot.inflict me { e.de_damage with amount = samount; context = ctx };
            { e.de_damage with amount = amount - samount }
        | _ -> e.de_damage);
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some st when is_hird_of st && d.is_spell && d.amount >= st.life -> { d with amount = st.life - 1 }
        | _ -> d);
  }

let protect_from_opp d (slot : slot_update) =
  match slot.value with
  | None -> d
  | Some st ->
      let level_diff =
        match (Slot.opposite slot).value with
        | Some opp -> max 0 (opp.card.c_card.cost - st.card.c_card.cost)
        | None -> 0
      in
      if (not (is_effect d)) && level_diff <> 0 then { d with amount = max 0 (d.amount - level_diff) } else d

let armour_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect = (fun d -> protect_from_opp d (Reaction.bound_slot r));
    on_protect =
      (fun e ->
        let me = Reaction.bound_slot r in
        match e.de_target with
        | Some num
          when (match me.value with
                 | Some st -> is_hird_of st && not (is_card !armour_ref st.card.c_card)
                 | None -> false)
               && abs (me.num - num) = 1 ->
            protect_from_opp e.de_damage me.slots.slot_updates.(num)
        | _ -> e.de_damage);
  }

let ballista_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if me.slots.player.pid <> e.s_player.pid then (
          let summoned = e.s_player.slots_update.slot_updates.(e.s_num) in
          let ctx = { ctx_player = me.slots.player.pid; ctx_card = !ballista_ref; ctx_selected = me.num } in
          focus_slot me;
          let life = (Slot.get summoned).life in
          Slot.inflict summoned
            (mk_damage ~is_ability:true (int_of_float (ceil (float_of_int life /. 2.))) ctx);
          let self_amt = match me.value with Some st when is_hird_of st -> 7 | _ -> 10 in
          Slot.inflict me (mk_damage ~is_ability:true self_amt ctx)));
  }

let swing (slot : slot_update) =
  Option.iter (fun st -> Player.run_slot slot.slots.player slot.num st) slot.value

let on_berserker_player_damage amount (slot : slot_update) =
  match slot.value with
  | None -> ()
  | Some st ->
      let extra =
        match Hashtbl.find_opt berserker_extras st.slot_id with
        | Some e -> e
        | None ->
            let e = ref (-1) in
            Hashtbl.replace berserker_extras st.slot_id e;
            e
      in
      if amount > 4 then
        if !extra = -1 then (
          extra := 0;
          swing slot;
          while Option.is_some slot.value && !extra > 0 do
            extra := !extra - 1;
            swing slot
          done;
          extra := -1)
        else extra := !extra + 1

let berserker_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        match (Reaction.bound_slot r).value with
        | Some st when is_hird_of st -> { d with amount = d.amount + 3 }
        | _ -> d);
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num = me.num && slot.slots.player.pid = me.slots.player.pid then
          Option.iter (fun st -> Hashtbl.replace berserker_extras st.slot_id (ref (-1))) me.value);
    on_my_remove =
      (fun _ ->
        match (Reaction.bound_slot r).value with
        | Some st -> Hashtbl.remove berserker_extras st.slot_id
        | None -> ());
  }

let mountain_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some st when is_hird_of st -> Player.remove_desc_mod me.slots.player lower_special_cost_mod
        | _ -> ());
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if e.s_player.pid = me.slots.player.pid && is_dwarf e.s_card.c_card then (
          Slot.heal me 4;
          add_attack me (Attack_func (fun a -> a + 2))));
  }

let crossbow_attack target d player =
  match target with
  | [] -> Player.inflict (other_player player) d
  | num :: _ ->
      let op = other_player player in
      Player.inflict op d;
      Option.iter
        (fun _ -> Slot.inflict op.slots_update.slot_updates.(num) d)
        op.slots_update.slot_updates.(num).value

let crossbow_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some st when is_hird_of st ->
            Slot.set_data me No_data;
            swing me
        | _ -> ());
  }

let rec set_hird (owner : player_update) (s : slot_update) b =
  Slot.set_data s (if b then Hird else No_data);
  match s.value with
  | None -> ()
  | Some st ->
      if is_card !soldier_ref st.card.c_card then set_soldier_hird s b (other_player owner)
      else if st.card.c_card.key = "mountainking.MountainKingCard.name" then
        if b then Player.add_desc_mod owner lower_special_cost_mod
        else Player.remove_desc_mod owner lower_special_cost_mod

and on_add (owner : player_update) (slot : slot_update) =
  if slot.slots.player.pid = owner.pid then
    match slot.value with
    | Some st when is_dwarf st.card.c_card ->
        let neighbour =
          List.fold_left
            (fun acc s ->
              match s.value with
              | Some v when is_dwarf v.card.c_card ->
                  if v.data = No_data then set_hird owner s true;
                  true
              | _ -> acc)
            false (Slot.filled_adjacent slot)
        in
        if neighbour then set_hird owner slot true
    | _ -> ()
  else
    match owner.slots_update.slot_updates.(slot.num).value with
    | Some s when is_card !soldier_ref s.card.c_card && is_hird_of s ->
        if not (has_source slot (fun x -> x == soldier_lower)) then add_attack slot soldier_lower
    | _ -> ()

and on_death (owner : player_update) (dead : dead) =
  if dead.d_player.pid = owner.pid && is_dwarf dead.d_slot.card.c_card then
    Slots.foreach owner.slots_update (fun s ->
        if s.num <> dead.d_num then
          match s.value with
          | Some st when is_dwarf st.card.c_card ->
              (if abs (s.num - dead.d_num) = 1 then
                 let sym = s.num + (s.num - dead.d_num) in
                 if
                   (not (in_slot_range sym))
                   || not
                        (match owner.slots_update.slot_updates.(sym).value with
                        | Some v -> is_dwarf v.card.c_card
                        | None -> false)
                 then set_hird owner s false);
              st.reaction.on_death dead
          | _ -> ())

let protect (owner : player_update) (slot : slot_update) d =
  Slots.foldl owner.slots_update d (fun acc s ->
      match s.value with
      | Some st when st.card.c_card.house_index = special_house_index ->
          st.reaction.on_protect { de_damage = acc; de_target = Some slot.num; de_player = owner }
      | _ -> acc)

let mk_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let old_dead = p.slots_update.on_dead_hook in
        p.slots_update.on_dead_hook <-
          (fun dead ->
            old_dead dead;
            on_death p dead);
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.add_hook in
            slot.add_hook <-
              (fun st ->
                old st;
                on_add p slot);
            let old_p = slot.protect_hook in
            slot.protect_hook <- (fun d -> protect p slot (old_p d)))
          p.slots_update.slot_updates;
        let opp = other_player p in
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.add_hook in
            slot.add_hook <-
              (fun st ->
                old st;
                on_add p slot))
          opp.slots_update.slot_updates;
        let old_d = p.on_player_damage_hook in
        p.on_player_damage_hook <-
          (fun d ->
            old_d d;
            Slots.foreach p.slots_update (fun s ->
                match s.value with
                | Some st when is_card !berserker_ref st.card.c_card -> on_berserker_player_damage d.amount s
                | _ -> ())));
  }

let cards =
  [
    creature ~key:"mountainking.DwarvenSoldier.name" ~attack:(attack 3) ~life:12
      ~description:"mountainking.DwarvenSoldier.description" ~new_reaction:soldier_reaction
      ~effects:(effects [ (Direct, soldier_effect) ])
      ();
    creature ~key:"mountainking.DwarvenShieldman.name" ~attack:(attack 3) ~life:15
      ~description:"mountainking.DwarvenShieldman.description" ~new_reaction:shieldman_reaction ();
    creature ~key:"mountainking.DwarvenCrossbowman.name" ~attack:(attack 3) ~life:20
      ~description:"mountainking.DwarvenCrossbowman.description"
      ~run_attack:(custom_run_attack crossbow_attack) ~new_reaction:crossbow_reaction ();
    creature ~key:"mountainking.ArmourcladDwarf.name" ~attack:(attack 5) ~life:23
      ~description:"mountainking.ArmourcladDwarf.description" ~new_reaction:armour_reaction ();
    creature ~key:"mountainking.Runesmith.name"
      ~attack:{ base = Some 6; sources = [ rune_attack ] }
      ~life:24 ~description:"mountainking.Runesmith.description" ~new_reaction:rune_reaction ();
    creature ~key:"mountainking.Ballista.name" ~attack:(attack 6) ~life:40
      ~description:"mountainking.Ballista.description" ~new_reaction:ballista_reaction ();
    creature ~key:"mountainking.Berserker.name"
      ~attack:{ base = Some 6; sources = [ berserker_attack ] }
      ~life:40 ~description:"mountainking.Berserker.description" ~new_reaction:berserker_reaction ();
    creature ~key:"mountainking.MountainKingCard.name" ~attack:(attack 4) ~life:45
      ~description:"mountainking.MountainKingCard.description" ~new_reaction:mountain_reaction
      ~effects:(effects [ (Direct, mountain_direct) ])
      ();
  ]

let () =
  soldier_ref := Some (List.nth cards 0);
  shieldman_ref := Some (List.nth cards 1);
  armour_ref := Some (List.nth cards 3);
  ballista_ref := Some (List.nth cards 5);
  berserker_ref := Some (List.nth cards 6)

let house = special_house ~key:"mountainking" ~id:house_id ~listener:mk_listener cards
