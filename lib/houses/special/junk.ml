(* Special house Junk — port of priv/sp/house/JunkMage.scala.

   Scala's [attack.setDirty()] calls (Screamer) have no counterpart: attack sources are re-folded on every
   refresh. Fortune keeps its "used" flag in the slot data: No_data is "not yet", Custom "used" is "used". *)

open Sp
open Engine
open Common

let screamer_ref : card option ref = ref None
let fortune_ref : card option ref = ref None
let cyborg_ref : card option ref = ref None
let trash_ref : card option ref = ref None
let creature_of opt = match opt with Some c -> make_creature c | None -> failwith "junk not initialised"

let screamer_source =
  Attack_state_func
    (fun a p ->
      a
      + Array.fold_left
          (fun n s ->
            match s.value with Some st when is_card !screamer_ref st.card.c_card -> n + 1 | _ -> n)
          0 p.slots_update.slot_updates)

(* ------------------------------------------------------------------ spells *)

let poison_flower : card_effect =
 fun env ->
  let p = player_of_env env and op = other_of_env env in
  let damage = spell_damage env 5 in
  let slot = own_slot_of_env env in
  let h = (Slot.get slot).card.c_card.house_index in
  Slot.inflict slot damage;
  Houses.incr_mana p.houses_update (-1) [ h ];
  match (Slot.opposite slot).value with
  | None -> ()
  | Some opposite ->
      List.iter
        (fun num ->
          let s = op.slots_update.slot_updates.(num) in
          if Option.is_some s.value then Slot.inflict s damage)
        (slot_interval (env.env_selected - 1) (env.env_selected + 1));
      Houses.incr_mana op.houses_update (-1) [ opposite.card.c_card.house_index ]

(* ----------------------------------------------------------------- fortune *)

let fortune_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun de ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some st when st.data = No_data && de.de_target = None && de.de_damage.amount > 0 ->
            focus_slot ~blocking:false me;
            Slot.set_data me (Custom "used");
            { de.de_damage with amount = max 0 (de.de_damage.amount - 4) }
        | _ -> de.de_damage);
  }

let reset_protect : card_effect = fun env -> Slot.set_data (own_slot_of_env env) No_data

(* ---------------------------------------------------------------- assassin *)

(* nearestSlotOpposed: the empty own slot facing an enemy creature that is closest to [selected] *)
let nearest_slot_opposed selected (p : player_update) =
  let op = other_player p in
  p.pstate.ps_slot_list
  |> List.filter (fun n ->
      n <> selected
      && Option.is_none p.slots_update.slot_updates.(n).value
      && Option.is_some op.slots_update.slot_updates.(n).value)
  |> List.fold_left
       (fun best n ->
         match best with Some b when abs (b - selected) <= abs (n - selected) -> best | _ -> Some n)
       None

let roam : card_effect =
 fun env ->
  let p = player_of_env env and op = other_of_env env in
  if Option.is_none op.slots_update.slot_updates.(env.env_selected).value then
    Option.iter
      (fun n ->
        Slot.inflict op.slots_update.slot_updates.(n) (ability_damage env 5);
        Slots.move p.slots_update env.env_selected n)
      (nearest_slot_opposed env.env_selected p)

(* ----------------------------------------------------------------- cyborg *)

(* Up to two trash fuse into the first cyborg: their life and 2 attack each *)
let gather_trash : card_effect =
 fun env ->
  let p = player_of_env env in
  let trash =
    List.filter_map
      (fun n ->
        let slot = p.slots_update.slot_updates.(n) in
        match slot.value with Some st when is_card !trash_ref st.card.c_card -> Some (slot, st) | _ -> None)
      base_slot_list
  in
  let taken = List.filteri (fun i _ -> i < 2) trash in
  List.iter (fun (slot, _) -> Slot.destroy slot) taken;
  if taken <> [] then
    let life = List.fold_left (fun n (_, st) -> n + st.life) 0 taken in
    let attack = List.length taken * 2 in
    Option.iter
      (fun slot ->
        Updater.focus env.env_updater slot.num env.env_player true;
        Slot.update slot (fun s -> { s with life = s.life + life });
        add_attack slot (Attack_func (fun a -> a + attack)))
      (Slots.find p.slots_update (fun st -> is_card !cyborg_ref st.card.c_card))

let spawn_trash : card_effect = fun env -> fill_empty_slots (creature_of !trash_ref) env

(* ----------------------------------------------------------------- factory *)

(* Copies a cheap neighbour into the free slot on the other side of it *)
let factory_copy : card_effect =
 fun env ->
  let p = player_of_env env in
  let neighbour i = if List.mem i p.pstate.ps_slot_list then Some p.slots_update.slot_updates.(i) else None in
  let copy_into (source : slot_update) (dest : slot_update) =
    match (source.value, dest.value) with
    | Some s, None when s.card.c_card.cost < 6 ->
        focus_slot (own_slot_of_env env);
        Slots.add_creature dest s.card;
        Option.iter (fun copied -> Player.apply_effect p Direct dest.num copied) dest.value;
        true
    | _ -> false
  in
  match (neighbour (env.env_selected - 1), neighbour (env.env_selected + 1)) with
  | Some a, Some b -> if not (copy_into a b) then ignore (copy_into b a)
  | _ -> ()

(* MirrorSummon: a creature the owner summons next to this one is copied on its other side *)
let mirror_summon (r : reaction) ~max_cost ~apply_spawn (e : summon_event) =
  let me = Reaction.bound_slot r in
  let step = me.num - e.s_num in
  if e.s_player.pid = me.slots.player.pid && abs step = 1 && e.s_card.c_card.cost < max_cost + 1 then
    let pos = me.num + step in
    let p = me.slots.player in
    if List.mem pos p.pstate.ps_slot_list then
      let slot = p.slots_update.slot_updates.(pos) in
      if Option.is_none slot.value then (
        focus_slot me;
        Slots.add_creature slot e.s_card;
        if apply_spawn then
          Option.iter (fun copied -> Player.apply_effect p Direct slot.num copied) slot.value)

let factory_reaction () =
  let r = Reaction.default () in
  { r with on_summon = mirror_summon r ~max_cost:5 ~apply_spawn:true }

(* When a cheap neighbour dies, the controller closes the gap *)
let chain_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon = mirror_summon r ~max_cost:3 ~apply_spawn:false;
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        let p = dead.d_player in
        let step = dead.d_num - me.num in
        if dead.d_slot.card.c_card.cost < 6 && abs step = 1 then
          let get_at n =
            if List.mem n p.pstate.ps_slot_list then
              match p.slots_update.slot_updates.(n).value with
              | Some st when st.card.c_card.cost < 6 -> Some n
              | _ -> None
            else None
          in
          match get_at (dead.d_num + step) with
          | Some dest ->
              focus_slot me;
              Slots.move p.slots_update dest dead.d_num
          | None -> (
              match get_at (me.num - step) with
              | Some dest ->
                  focus_slot me;
                  Slots.move p.slots_update dest dead.d_num
              | None -> ()));
  }

let bot_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        if Option.is_some me.value then (
          focus_slot me;
          Player.heal dead.d_player 2;
          Slot.heal me 6));
  }

let junk_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        init_owner_death p;
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.protect_hook in
            slot.protect_hook <-
              (fun d ->
                let d = old d in
                Slots.foldl p.slots_update d (fun acc s ->
                    match s.value with
                    | Some st when is_card !fortune_ref st.card.c_card ->
                        st.reaction.on_protect { de_damage = acc; de_target = Some slot.num; de_player = p }
                    | _ -> acc)))
          p.slots_update.slot_updates);
  }

let trash = creature ~key:"junk.trash" ~attack:(attack 2) ~life:11 ~description:"" ()

let cyborg =
  creature ~key:"junk.cyborg" ~attack:(attack 3) ~life:30 ~description:"junk.cyborg.description"
    ~effects:(effects [ (Direct, spawn_trash); (On_turn, gather_trash) ])
    ()

let fortune =
  creature ~key:"junk.fortune" ~attack:(attack 3) ~life:15 ~description:"junk.fortune.description"
    ~new_reaction:fortune_reaction
    ~effects:(effects [ (On_end_turn, reset_protect) ])
    ()

let screamer =
  creature ~key:"junk.screamer"
    ~attack:{ base = Some 2; sources = [ screamer_source ] }
    ~life:14 ~description:"junk.screamer.description" ()

let () =
  trash.cost <- 1;
  screamer_ref := Some screamer;
  fortune_ref := Some fortune;
  cyborg_ref := Some cyborg;
  trash_ref := Some trash

let cards =
  [
    screamer;
    spell ~key:"junk.poison" ~description:"junk.poison.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, poison_flower) ])
      ();
    fortune;
    creature ~key:"junk.chain" ~attack:(attack 4) ~life:18 ~description:"junk.chain.description"
      ~new_reaction:chain_reaction ();
    creature ~key:"junk.assassin" ~attack:(attack 6) ~life:27 ~description:"junk.assassin.description"
      ~effects:(effects [ (On_end_turn, roam) ])
      ();
    creature ~key:"junk.factory" ~attack:(attack 4) ~life:29 ~description:"junk.factory.description"
      ~effects:(effects [ (Direct, factory_copy) ])
      ~new_reaction:factory_reaction ();
    creature ~key:"junk.bot" ~attack:(attack 6) ~life:26 ~description:"junk.bot.description"
      ~new_reaction:bot_reaction ();
    cyborg;
  ]

let house = special_house ~key:"junk" ~id:19 ~additional_cards:[ trash ] ~listener:junk_listener cards
