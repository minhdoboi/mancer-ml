(* Darksider special house — port of priv/sp/house/DarkSider.scala. *)

open Sp
open Engine
open Common

let wolf_ref : card option ref = ref None
let warlock_ref : card option ref = ref None
let knight_ref : card option ref = ref None
let lake_ref : card option ref = ref None

(* Same as zen's elementesist: hits every enemy creature sharing the
   opposite creature's element (house index), or the player if empty. *)
let elem_attack target d player =
  match target with
  | [] -> ()
  | num :: _ -> (
      let op = Player.other player in
      match op.slots_update.slot_updates.(num).value with
      | None -> Player.inflict op d
      | Some st ->
          let h = st.card.c_card.house_index in
          Slots.foreach op.slots_update (fun s ->
              if (Slot.get s).card.c_card.house_index = h then Slot.inflict s d))

(* Scala `Destroyed(card)`: hides that card from its own house. *)
let destroyed_mod (card : card) : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> card.house_index then cards
  else Array.of_list (List.filter (fun cd -> cd.cd_card.id <> card.id) (Array.to_list cards))

let wolf_spider_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if me.slots.player.pid <> e.s_player.pid then (
          let amount = e.s_card.c_card.cost in
          Updater.focus me.slots.player.updater me.num me.slots.player.pid true;
          let d =
            mk_damage ~is_ability:true amount
              { ctx_player = me.slots.player.pid; ctx_card = !wolf_ref; ctx_selected = me.num }
          in
          Slot.inflict e.s_player.slots_update.slot_updates.(me.num) d));
  }

let knight_cond me_num num = abs (me_num - num) = 1

let knight_apply me s =
  match s.value with
  | None -> ()
  | Some st -> (
      match !knight_ref with
      | Some k when st.card.c_card.id = k.id -> ()
      | _ ->
          let target = if st.target = [ s.num ] then [ me.num ] else me.num :: st.target in
          Slot.set_target s target)

let knight_undo me s =
  match s.value with
  | None -> ()
  | Some st -> (
      match !knight_ref with
      | Some k when st.card.c_card.id = k.id -> ()
      | _ ->
          let target = List.filter (( <> ) me.num) st.target in
          Slot.set_target s (if target = [] then [ s.num ] else target))

let black_knight_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num && me.slots.player.pid = slot.slots.player.pid then
          List.iter (knight_apply me) (Slot.filled_adjacent me)
        else if me.slots.player.pid = slot.slots.player.pid && knight_cond me.num slot.num then
          knight_apply me slot);
    on_remove =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.slots.player.pid = slot.slots.player.pid && knight_cond me.num slot.num then knight_undo me slot);
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        List.iter (knight_undo me) (Slot.filled_adjacent me));
  }

(* Broadcast via OwnerDeath in the house listener (see below). *)
let dark_mystic_reaction () =
  let r = Reaction.default () in
  { r with on_death = (fun _ -> Houses.incr_mana (Reaction.bound_slot r).slots.player.houses_update 1 [ 4 ]) }

let horror : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match (Slot.opposite slot).value with
  | None -> ()
  | Some st -> Player.add_desc_mod (other_of_env env) (destroyed_mod st.card.c_card)

let lake_add lake_slot cmd_card =
  let m = destroyed_mod cmd_card in
  let opp = Player.other lake_slot.slots.player in
  Player.add_desc_mod opp m;
  let cur =
    match lake_slot.value with Some st -> ( match st.data with Lake_mods xs -> xs | _ -> []) | None -> []
  in
  Slot.set_data lake_slot (Lake_mods (m :: cur))

let lake_reaction () =
  let r = Reaction.default () in
  {
    r with
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        let opp = Player.other me.slots.player in
        match me.value with
        | Some st -> (
            match st.data with
            | Lake_mods xs -> List.iter (fun m -> Player.remove_desc_mod opp m) xs
            | _ -> ())
        | None -> ());
  }

let dragon_stun : card_effect =
 fun env -> Slots.foreach (other_of_env env).slots_update (fun s -> Slot.stun s)

let dragon_reaction () =
  let r = Reaction.default () in
  {
    r with
    heal = (fun _ _ -> ());
    inflict = (fun s d -> if not (is_effect d) then Slot.damage_slot s d);
    destroy = (fun _ -> ());
    stun = (fun _ -> ());
  }

let darksider_listener () =
  let player = ref None in
  let get () = match !player with Some p -> p | None -> failwith "darksider listener not inited" in
  {
    hl_on_death = (fun () -> true);
    hl_mod = Fun.id;
    hl_intercept_submit =
      (fun c ->
        match c with
        | None -> (false, None)
        | Some cmd when cmd.cmd_player <> (get ()).pid -> (
            (* Warlock blocks opponent spells. *)
            let warlocks =
              Array.to_list (get ()).slots_update.slot_updates
              |> List.filter_map (fun s ->
                  match s.value with Some st when is_card !warlock_ref st.card.c_card -> Some s | _ -> None)
            in
            match (warlocks, cmd.cmd_card.is_spell) with
            | w :: _, true ->
                let amount = 3 * cmd.cmd_card.cost in
                Slot.inflict w
                  (mk_damage ~is_ability:true amount
                     { ctx_player = (get ()).pid; ctx_card = !warlock_ref; ctx_selected = w.num });
                let caster = (get ()).updater.player_updates.(cmd.cmd_player) in
                Houses.incr_mana caster.houses_update (-cmd.cmd_cost) [ cmd.cmd_card.house_index ];
                (true, None)
            | _ -> (false, Some cmd))
        | Some cmd -> (false, Some cmd));
    hl_init =
      (fun p ->
        player := Some p;
        let opp = other_player p in
        let old_sub = opp.submit_command_hook in
        opp.submit_command_hook <-
          (fun c ->
            let r = old_sub c in
            Array.iter
              (fun s ->
                match s.value with
                | Some st when is_card !lake_ref st.card.c_card -> lake_add s c.cmd_card
                | _ -> ())
              p.slots_update.slot_updates;
            r);
        let old = p.slots_update.on_dead_hook in
        p.slots_update.on_dead_hook <-
          (fun dead ->
            old dead;
            Array.iter
              (fun s ->
                match s.value with
                | Some st when s.num <> dead.d_num && st.card.c_card.house_index = special_house_index ->
                    st.reaction.on_death dead
                | _ -> ())
              p.slots_update.slot_updates));
  }

let fallen =
  creature ~key:"darksider.fallen.name" ~attack:(attack 4) ~life:12
    ~description:"darksider.fallen.description" ~run_attack:(custom_run_attack elem_attack) ()

let wolf_spider =
  creature ~key:"darksider.spider.name" ~attack:(attack 3) ~life:16
    ~description:"darksider.spider.description" ~new_reaction:wolf_spider_reaction ()

let black_knight =
  creature ~key:"darksider.knight.name" ~attack:(attack 4) ~life:18
    ~description:"darksider.knight.description" ~new_reaction:black_knight_reaction ()

let mystic =
  creature ~key:"darksider.mystic.name" ~attack:(mana_attack 4) ~life:21
    ~description:"darksider.mystic.description" ~new_reaction:dark_mystic_reaction ()

let faceless =
  creature ~key:"darksider.faceless.name" ~attack:(attack 7) ~life:34
    ~description:"darksider.faceless.description"
    ~effects:(effects [ (Direct, horror) ])
    ()

let lake =
  creature ~key:"darksider.lake.name" ~attack:(attack 0) ~life:41 ~description:"darksider.lake.description"
    ~new_reaction:lake_reaction ()

let warlock =
  creature ~key:"darksider.warlock.name" ~attack:(attack 5) ~life:30
    ~description:"darksider.warlock.description" ()

let dragon =
  creature ~key:"darksider.dragon.name" ~attack:(attack 8) ~life:39
    ~description:"darksider.dragon.description" ~new_reaction:dragon_reaction
    ~effects:(effects [ (Direct, dragon_stun) ])
    ()

let () =
  wolf_ref := Some wolf_spider;
  warlock_ref := Some warlock;
  knight_ref := Some black_knight;
  lake_ref := Some lake

let cards = [ fallen; wolf_spider; black_knight; mystic; faceless; lake; warlock; dragon ]
let house = special_house ~key:"darksider" ~id:10 ~listener:darksider_listener cards
