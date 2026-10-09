(* Special house Entomologist — port of priv/sp/house/Entomologist.scala. Hivemind is ChangeTarget:
   every owner creature aims at one enemy until end of turn (or that enemy dies). *)

open Sp
open Engine
open Common

let giant_ant_ref : card option ref = ref None
let targeting_of (p : player_update) = match p.pstate.ps_data with Player_targeting t -> t | _ -> None
let tracks_target p = match p.pstate.ps_data with Player_targeting _ -> true | _ -> false

let set_target p t =
  match p.pstate.ps_data with Player_targeting _ -> Player.set_data p (Player_targeting t) | _ -> ()

let recover_target (p : player_update) =
  Slots.foreach p.slots_update (fun s -> Slot.set_target s [ s.num ]);
  set_target p None

let change_target : card_effect =
 fun env ->
  let p = player_of_env env in
  let target = Slot.get (opp_slot_of_env env) in
  set_target p (Some target.slot_id);
  Slots.foreach p.slots_update (fun s -> Slot.set_target s [ env.env_selected ]);
  Player.add_effect p
    ( On_end_turn,
      one_time_player_effect (fun e ->
          let pl = player_of_env e in
          if (not (tracks_target pl)) || targeting_of pl <> None then recover_target pl) )

let drone =
  creature ~key:"entomologist.drone" ~attack:(attack 2) ~life:14 ~description:"entomologist.drone.description"
    ()

let insect =
  creature ~key:"entomologist.warrior" ~attack:(attack 4) ~life:10
    ~description:"entomologist.warrior.description"
    ~new_reaction:(fun () ->
      let r = Reaction.default () in
      {
        r with
        on_my_death = (fun _ -> Houses.incr_mana (Reaction.bound_slot r).slots.player.houses_update 1 [ 4 ]);
      })
    ()

let () = insect.cost <- 1

let assassin_source =
  Attack_state_func
    (fun a p ->
      let n =
        Array.fold_left
          (fun acc s -> match s.value with Some st when st.card.c_card.id = drone.id -> acc + 1 | _ -> acc)
          0 p.slots_update.slot_updates
      in
      a + (n * 2))

let lower_giant_cost : desc_mod =
 fun house cards ->
  if house.phd_house.h_index < special_house_index then cards
  else
    Array.map
      (fun cd ->
        if is_card !giant_ant_ref cd.cd_card then { cd with cd_cost = max 0 (cd.cd_cost - 2) } else cd)
      cards

let beetle_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        focus_slot me;
        Houses.incr_mana me.slots.player.houses_update 1 [ 0 ]);
  }

let moth_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        focus_slot me;
        Slots.inflict_creatures (other_player me.slots.player).slots_update
          (mk_damage ~is_ability:true 3
             {
               ctx_player = me.slots.player.pid;
               ctx_card = Some dead.d_slot.card.c_card;
               ctx_selected = dead.d_num;
             }));
  }

let giant_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect = (fun d -> { d with amount = max 0 (d.amount - 2) });
    clean_up = (fun () -> Player.remove_desc_mod (Reaction.bound_slot r).slots.player lower_giant_cost);
  }

let unbridle (me : slot_update) =
  match me.value with
  | Some st -> (
      match st.data with
      | Mantis_house h ->
          Houses.incr_growth (other_player me.slots.player).houses_update 1 [ h ];
          Slot.set_data me No_data
      | _ -> ())
  | None -> ()

let bridle (me : slot_update) house_index =
  Houses.incr_growth (other_player me.slots.player).houses_update (-1) [ house_index ];
  Slot.set_data me (Mantis_house house_index)

let mantis_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num = me.num then
          Option.iter (fun (st : slot_state) -> bridle me st.card.c_card.house_index) (Slot.opposite me).value);
    on_my_remove = (fun _ -> unbridle (Reaction.bound_slot r));
    on_my_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        Slot.inflict (Slot.opposite me)
          (mk_damage ~is_ability:true 5
             {
               ctx_player = me.slots.player.pid;
               ctx_card = Some dead.d_slot.card.c_card;
               ctx_selected = dead.d_num;
             });
        unbridle me);
  }

let locust : card_effect =
 fun env ->
  let s = opp_slot_of_env env in
  Slot.toggle s cursed_flag;
  let id = (Slot.get s).slot_id in
  let rec curse e =
    match Slots.find_slot_id (other_of_env e).slots_update id with
    | Some slot ->
        let ctx = { ctx_player = e.env_player; ctx_card = None; ctx_selected = no_selection } in
        Player.inflict (other_of_env e) (mk_damage ~is_spell:true 4 ctx);
        Slot.inflict slot (mk_damage ~is_spell:true 8 ctx)
    | None -> Player.remove_effect (player_of_env e) (fun f -> f == curse)
  in
  Player.add_effect (player_of_env env) (On_end_turn, curse)

let spawn_drone env num =
  let p = player_of_env env in
  if List.mem num p.pstate.ps_slot_list then
    let slot = p.slots_update.slot_updates.(num) in
    if Option.is_none slot.value then Slots.add_creature slot (make_creature drone)

let hive : card_effect =
 fun env ->
  let p = player_of_env env in
  match Slots.get_open p.slots_update with
  | [] -> ()
  | ns ->
      let n = List.nth ns (Dice.int env.env_updater.rng (List.length ns)) in
      let slot = p.slots_update.slot_updates.(n) in
      Slots.add_creature slot (make_creature insect);
      focus_slot ~blocking:false slot

let mantis_key = "entomologist.mantis"
let is_mantis (st : slot_state) = st.card.c_card.key = mantis_key

let react_opp_add (p : player_update) (_st : slot_state) num =
  Slots.foreach p.slots_update (fun s ->
      match s.value with
      | Some st when is_mantis st && num = s.num ->
          unbridle s;
          Option.iter (fun (opp : slot_state) -> bridle s opp.card.c_card.house_index) (Slot.opposite s).value
      | _ -> ())

let react_opp_remove (p : player_update) num =
  Slots.foreach p.slots_update (fun s ->
      match s.value with Some st when is_mantis st && num = s.num -> unbridle s | _ -> ())

let entomologist_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        init_opp_death p;
        let old_dead = (other_player p).slots_update.on_dead_hook in
        (other_player p).slots_update.on_dead_hook <-
          (fun dead ->
            old_dead dead;
            match targeting_of p with Some id when id = dead.d_slot.slot_id -> recover_target p | _ -> ());
        Array.iter
          (fun (slot : slot_update) ->
            let old_add = slot.add_hook in
            slot.add_hook <-
              (fun st ->
                old_add st;
                react_opp_add p st slot.num);
            let old_remove = slot.remove_hook in
            slot.remove_hook <-
              (fun dead ->
                let st = old_remove dead in
                react_opp_remove p slot.num;
                st))
          (other_player p).slots_update.slot_updates);
  }

let cards =
  [
    creature ~key:"entomologist.beetle" ~attack:(attack 3) ~life:12
      ~description:"entomologist.beetle.description"
      ~effects:
        (effects
           [ (Direct, fun env -> Slot.inflict (Slot.opposite (own_slot_of_env env)) (ability_damage env 4)) ])
      ~new_reaction:beetle_reaction ();
    creature ~key:"entomologist.moth" ~attack:(attack 3) ~life:14 ~description:"entomologist.moth.description"
      ~effects:
        (effects
           [ (On_turn, damage ~is_ability:true 1); (On_turn, focus (damage_creatures ~is_ability:true 1)) ])
      ~new_reaction:moth_reaction ();
    spell ~key:"entomologist.hivemind" ~description:"entomologist.hivemind.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, change_target) ])
      ();
    spell ~key:"entomologist.locust" ~description:"entomologist.locust.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, locust) ])
      ();
    creature ~key:"entomologist.ant" ~attack:(attack 5) ~life:23 ~description:"entomologist.ant.description"
      ~effects:(effects [ (Direct, fun env -> Player.add_desc_mod (player_of_env env) lower_giant_cost) ])
      ~new_reaction:giant_reaction ();
    creature ~key:"entomologist.assassin"
      ~attack:{ base = Some 5; sources = [ assassin_source ] }
      ~life:26 ~description:"entomologist.assassin.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 spawn_drone env (env.env_selected - 1);
                 spawn_drone env (env.env_selected + 1) );
           ])
      ();
    creature ~key:"entomologist.hive" ~attack:(attack 2) ~life:30 ~description:"entomologist.hive.description"
      ~effects:(effects [ (On_end_turn, hive) ])
      ();
    creature ~key:"entomologist.mantis" ~attack:(attack 7) ~life:47
      ~description:"entomologist.mantis.description"
      ~effects:
        (effects
           [ (Direct, fun env -> Houses.incr_mana (other_of_env env).houses_update (-1) [ 0; 1; 2; 3; 4 ]) ])
      ~new_reaction:mantis_reaction ();
  ]

let () = giant_ant_ref := List.find_opt (fun (c : card) -> c.key = "entomologist.ant") cards

let house =
  special_house ~key:"entomologist" ~id:27 ~additional_cards:[ drone; insect ] ~listener:entomologist_listener
    ~data:(Player_targeting None) cards
