(* Special house Shaman — port of priv/sp/house/Shaman.scala. The ghost wolf starts in the last of
   the first five open slots; WolfMod replaces the first special while it is dead. *)

open Sp
open Engine
open Common

let house_id = 37

let empty_wolf =
  {
    wo_protection = 0;
    wo_fury = None;
    wo_bonus = 0;
    wo_hunting = 0;
    wo_mates = [];
    wo_shadows = [];
    wo_enhanced = [];
  }

let get_wolf p = match p.pstate.ps_data with Wolf_data d -> d | _ -> empty_wolf
let set_wolf p d = Player.set_data p (Wolf_data d)
let update_wolf p f = set_wolf p (f (get_wolf p))
let add_num xs n = if List.mem n xs then xs else n :: xs
let rem_num xs n = List.filter (( <> ) n) xs
let wolf_ref : card option ref = ref None
let protector_ref : card option ref = ref None
let is_wolf c = is_card !wolf_ref c
let is_protector c = is_card !protector_ref c
let wolf_attack_bonus = Attack_state_func (fun a p -> a + (get_wolf p).wo_bonus)
let hunt_bonus = Attack_func (fun a -> a + 2)
let one_attack_bonus = Attack_func (fun a -> a + 1)

let wolf_mod : desc_mod =
 fun house cards ->
  if house.phd_house.h_index < special_house_index then cards
  else
    match !wolf_ref with
    | None -> cards
    | Some w ->
        let desc = { cd_card = w; cd_cost = 2; cd_enabled = true; cd_extra_mana = 0 } in
        let rest = match Array.to_list cards with [] -> [] | _ :: xs -> xs in
        Array.of_list (desc :: rest)

let decr_special_cost_mod : desc_mod =
 fun house cards ->
  if house.phd_house.h_index < special_house_index then cards
  else Array.map (fun c -> { c with cd_cost = max 0 (c.cd_cost - 1) }) cards

let max_attack (p : player_update) default =
  Slots.foldl p.slots_update default (fun acc s -> max acc (Slot.get s).attack)

let shadow_slots (me : slot_update) =
  List.filter_map
    (fun n ->
      let s = me.slots.player.slots_update.slot_updates.(n) in
      if Option.is_some s.value then Some s else None)
    (get_wolf me.slots.player).wo_shadows

let wolf_attack target d player =
  match target with
  | [] -> ()
  | num :: _ ->
      let ws = get_wolf player in
      let heal_amount = ref (damage_and_get num d player) in
      List.iter (fun n -> heal_amount := !heal_amount + damage_creature_and_get n d player) ws.wo_mates;
      if ws.wo_hunting > 0 then Slot.heal player.slots_update.slot_updates.(num) !heal_amount

let common_wolf_protect d (p : player_update) =
  let prot = (get_wolf p).wo_protection in
  if prot <> 0 then { d with amount = max 0 (d.amount - prot) } else d

let wolf_reaction () =
  let r = Reaction.default () in
  {
    r with
    heal =
      (fun s amount ->
        Slot.do_heal s amount;
        List.iter (fun sh -> Slot.heal sh amount) (shadow_slots s));
    inflict =
      (fun s d ->
        Slot.damage_slot s d;
        List.iter (fun sh -> Slot.inflict sh d) (shadow_slots s));
    destroy =
      (fun s ->
        Slot.do_priv_destroy s;
        List.iter Slot.destroy (shadow_slots s));
    stun =
      (fun s ->
        Slot.do_stun s;
        List.iter Slot.stun (shadow_slots s));
    self_protect = (fun d -> common_wolf_protect d (Reaction.bound_slot r).slots.player);
    clean_up = (fun () -> Player.insert_desc_mod (Reaction.bound_slot r).slots.player wolf_mod);
  }

let shadow_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect = (fun d -> common_wolf_protect d (Reaction.bound_slot r).slots.player);
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        update_wolf me.slots.player (fun w -> { w with wo_shadows = rem_num w.wo_shadows me.num }));
  }

let hunter_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        match slot.value with Some st when is_wolf st.card.c_card -> add_attack slot hunt_bonus | _ -> ());
    on_remove =
      (fun slot ->
        match slot.value with
        | Some st when is_wolf st.card.c_card -> remove_first_attack slot hunt_bonus
        | _ -> ());
    on_my_remove =
      (fun dead ->
        let me = Reaction.bound_slot r in
        Option.iter
          (fun s -> remove_first_attack s hunt_bonus)
          (Slots.find_card me.slots (Option.get !wolf_ref));
        Option.iter (fun d -> update_wolf d.d_player (fun w -> { w with wo_hunting = w.wo_hunting - 1 })) dead);
  }

let protector_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun e ->
        match e.de_target with
        | None -> { e.de_damage with amount = max 0 (e.de_damage.amount - 2) }
        | Some num ->
            if e.de_damage.is_spell then
              let slot = (Reaction.bound_slot r).slots.slot_updates.(num) in
              match slot.value with
              | Some st when is_wolf st.card.c_card ->
                  focus_slot ~blocking:false (Reaction.bound_slot r);
                  Slot.heal slot e.de_damage.amount;
                  { e.de_damage with amount = 0 }
              | _ -> e.de_damage
            else e.de_damage);
  }

let mate_reaction () =
  let r = Reaction.default () in
  {
    r with
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        update_wolf me.slots.player (fun w -> { w with wo_mates = rem_num w.wo_mates me.num }));
  }

let wolf =
  creature ~key:"shaman.ghost"
    ~attack:{ base = Some 2; sources = [ wolf_attack_bonus ] }
    ~life:18 ~description:"shaman.ghost.description" ~new_reaction:wolf_reaction
    ~run_attack:(custom_run_attack wolf_attack)
    ~effects:
      (effects
         [
           ( Direct,
             fun env ->
               let p = player_of_env env in
               Player.remove_desc_mod p wolf_mod );
         ])
    ()

let () =
  wolf.cost <- 2;
  wolf_ref := Some wolf

let shadow_attack =
  Attack_state_func
    (fun a p ->
      match Slots.find_card p.slots_update wolf with Some s -> a + (Slot.get s).attack - 2 | None -> a)

let init_wolf : card_effect =
 fun env ->
  let p = player_of_env env in
  let first5 = List.filteri (fun i _ -> i < 5) (Slots.get_open p.slots_update) in
  match List.rev first5 with
  | [] -> ()
  | n :: _ ->
      let slot = p.slots_update.slot_updates.(n) in
      Slots.add_creature slot (make_creature wolf);
      focus_slot ~blocking:false slot

let hunger : card_effect =
 fun env ->
  let p = player_of_env env in
  let cap = max_attack (other_player p) (max_attack p 0) in
  Option.iter
    (fun s ->
      let bonus = Attack_func (fun a -> a + cap) in
      add_attack s bonus;
      Player.add_effect p (On_end_turn, one_time_player_effect (remove_attack bonus)))
    (Slots.find_card p.slots_update wolf)

let rage : card_effect =
 fun env ->
  List.iter
    (fun s ->
      let id = (Slot.get s).card.c_card.id in
      update_wolf (player_of_env env) (fun w ->
          let n = match List.assoc_opt id w.wo_enhanced with Some x -> x + 1 | None -> 1 in
          { w with wo_enhanced = (id, n) :: List.remove_assoc id w.wo_enhanced });
      add_attack s one_attack_bonus)
    (Slot.filled_adjacent (own_slot_of_env env))

let full_moon : card_effect =
 fun env ->
  let p = player_of_env env in
  update_wolf p (fun w -> { w with wo_protection = w.wo_protection + 1 });
  Option.iter
    (fun s -> List.iter (fun a -> Slot.heal a 8) (Slot.filled_adjacent s))
    (Slots.find_card p.slots_update wolf);
  Player.heal p 8

let remove_fury : card_effect =
 fun env -> update_wolf (player_of_env env) (fun w -> { w with wo_fury = None })

let phantom_fury : card_effect =
 fun env ->
  let p = player_of_env env in
  let ctx = { ctx_player = env.env_player; ctx_card = None; ctx_selected = env.env_selected } in
  Slots.inflict_creatures (other_of_env env).slots_update (mk_damage ~is_spell:true 7 ctx);
  update_wolf p (fun w ->
      { w with wo_fury = Option.map (fun s -> s.num) (Slots.find_card p.slots_update wolf) });
  Player.add_effect p (On_end_turn, one_time_player_effect remove_fury)

let hunt : card_effect =
 fun env ->
  let p = player_of_env env in
  update_wolf p (fun w -> { w with wo_hunting = w.wo_hunting + 1 });
  Option.iter (fun s -> add_attack s hunt_bonus) (Slots.find_card p.slots_update wolf)

let shade : card_effect =
 fun env ->
  let p = player_of_env env in
  update_wolf p (fun w -> { w with wo_shadows = add_num w.wo_shadows env.env_selected });
  List.iter
    (fun s -> Option.iter (fun st -> Player.run_slot p s.num st) s.value)
    (Slot.filled_adjacent p.slots_update.slot_updates.(env.env_selected))

let mate : card_effect =
 fun env ->
  let p = player_of_env env in
  Player.inflict (other_of_env env) (ability_damage env 8);
  update_wolf p (fun w -> { w with wo_mates = add_num w.wo_mates env.env_selected });
  Player.add_desc_mod p decr_special_cost_mod

let protect_slot (owner : player_update) (slot : slot_update) d =
  Slots.foldl owner.slots_update d (fun acc s ->
      match s.value with
      | Some st when is_protector st.card.c_card ->
          st.reaction.on_protect { de_damage = acc; de_target = Some slot.num; de_player = owner }
      | _ -> acc)

let shaman_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let opp = other_player p in
        let old_dead = opp.slots_update.on_dead_hook in
        opp.slots_update.on_dead_hook <-
          (fun dead ->
            old_dead dead;
            match (get_wolf p).wo_fury with
            | None -> ()
            | Some _ -> update_wolf p (fun w -> { w with wo_bonus = w.wo_bonus + 1 }));
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.protect_hook in
            slot.protect_hook <- (fun d -> protect_slot p slot (old d)))
          p.slots_update.slot_updates;
        let old = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old c in
            Option.iter
              (fun num ->
                match List.assoc_opt c.cmd_card.id (get_wolf p).wo_enhanced with
                | Some bonus ->
                    let slot = p.slots_update.slot_updates.(num) in
                    if Option.is_some slot.value then add_attack slot (Attack_func (fun a -> a + bonus))
                | None -> ())
              c.cmd_input;
            r));
  }

let cards =
  [
    spell ~key:"shaman.hunger" ~description:"shaman.hunger.description"
      ~effects:(effects [ (Direct, hunger) ])
      ();
    creature ~key:"shaman.rage" ~attack:(attack 2) ~life:10 ~description:"shaman.rage.description"
      ~effects:(effects [ (Direct, rage) ])
      ();
    spell ~key:"shaman.moon" ~description:"shaman.moon.description"
      ~effects:(effects [ (Direct, full_moon) ])
      ();
    spell ~key:"shaman.fury" ~description:"shaman.fury.description"
      ~effects:(effects [ (Direct, phantom_fury) ])
      ();
    creature ~key:"shaman.protector" ~attack:(attack 4) ~life:20 ~description:"shaman.protector.description"
      ~new_reaction:protector_reaction ();
    creature ~key:"shaman.hunter" ~attack:(attack 6) ~life:34 ~description:"shaman.hunter.description"
      ~new_reaction:hunter_reaction
      ~effects:(effects [ (Direct, hunt) ])
      ();
    creature ~key:"shaman.shadow"
      ~attack:{ base = Some 4; sources = [ shadow_attack ] }
      ~life:45 ~description:"shaman.shadow.description" ~new_reaction:shadow_reaction
      ~run_attack:(custom_run_attack wolf_attack)
      ~effects:(effects [ (Direct, shade) ])
      ();
    creature ~key:"shaman.mate" ~attack:(attack 5) ~life:29 ~description:"shaman.mate.description"
      ~new_reaction:mate_reaction
      ~effects:(effects [ (Direct, mate) ])
      ();
  ]

let () = protector_ref := Some (List.nth cards 4)

let house =
  special_house ~key:"shaman" ~id:house_id
    ~effects:[ (On_start, init_wolf) ]
    ~additional_cards:[ wolf ] ~listener:shaman_listener ~data:(Wolf_data empty_wolf) cards
