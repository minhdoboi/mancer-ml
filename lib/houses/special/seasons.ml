(* Special house Seasons — port of priv/sp/house/Seasons.scala. Midnight Sun, Regrowth and Wintry
   start on; playing the matching pair of specials turns each passive off. *)

open Sp
open Engine
open Common

let phase1 = "summer phase 1"
let phase2 = "summer phase 2"
let init_data = { se_triple = false; se_midnight = true; se_summer = false }
let default_data = { se_triple = false; se_midnight = false; se_summer = false }
let get_data p = match p.pstate.ps_data with Season_data d -> d | _ -> default_data
let set_data p d = Player.set_data p (Season_data d)

let lower_basic : desc_mod =
 fun house cards ->
  if house.phd_house.h_index > 3 then cards
  else
    match cards with
    | [||] -> cards
    | _ ->
        let last = Array.length cards - 1 in
        let c = cards.(last) in
        let copy = Array.copy cards in
        copy.(last) <- { c with cd_cost = max 0 (c.cd_cost - 1) };
        copy

let destroyed_mod (card : card) : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> card.house_index then cards
  else Array.of_list (List.filter (fun cd -> cd.cd_card.id <> card.id) (Array.to_list cards))

let nectar_bloom =
  creature ~key:"seasons.Nectarbloom.name" ~attack:(attack 2) ~life:10
    ~description:"seasons.Nectarbloom.description"
    ~effects:(effects [ (On_turn, focus (heal_creature 2)) ])
    ()

let rec regrowth env =
  Array.iteri
    (fun idx h -> if h.mana < 2 then Houses.incr_mana (player_of_env env).houses_update 1 [ idx ])
    (player_of_env env).houses_update.hu_value

and wintry env =
  let p = player_of_env env in
  let op = other_of_env env in
  Slots.foreach op.slots_update (fun s ->
      if Option.is_none p.slots_update.slot_updates.(s.num).value then Slot.inflict s (ability_damage env 1))

let freeze : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  Slots.foreach op.slots_update (fun s ->
      if Option.is_some p.slots_update.slot_updates.(s.num).value then Slot.stun s)

let freeze_mana (h : house_state) incr =
  if h.mana > 9 then h else if h.mana + incr > 9 then { h with mana = 9 } else { h with mana = h.mana + incr }

let gaea_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  let nb_empty = List.length (Slots.get_open p.slots_update) + List.length (Slots.get_open op.slots_update) in
  Slot.inflict p.slots_update.slot_updates.(env.env_selected) (ability_damage env nb_empty)

let cornucopia_effect : card_effect =
 fun env ->
  Slots.foreach (player_of_env env).slots_update (fun s -> if s.num <> env.env_selected then Slot.heal s 9)

let cornucopia_reaction () =
  let r =
    attack_bonus_reaction ~cond:( <> )
      ~get_bonus:(fun pos -> Attack_bonus { tag = "cornucopia"; pos; bonus = (fun a -> a + 1) })
      ()
  in
  {
    r with
    on_protect =
      (fun d ->
        match d.de_target with
        | Some n when n <> (Reaction.bound_slot r).num ->
            { d.de_damage with amount = max 0 (d.de_damage.amount - 1) }
        | _ -> d.de_damage);
  }

let recover_triple : card_effect =
 fun env -> set_data (player_of_env env) { (get_data (player_of_env env)) with se_triple = false }

let solar_wave : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  Player.inflict op (spell_damage env (List.length (Slots.get_open op.slots_update)));
  set_data p { (get_data p) with se_triple = true };
  Player.add_effect p (On_end_turn, one_time_player_effect recover_triple)

let eternal_summer : card_effect =
 fun env ->
  let p = player_of_env env in
  set_data p { (get_data p) with se_summer = true };
  Player.add_transition p (Wait_player (env.env_player, Some phase2));
  Player.add_transition p (Wait_player (env.env_player, Some phase1));
  Player.add_effect p
    ( On_end_turn,
      one_time_player_effect (fun e ->
          let pl = player_of_env e in
          Player.remove_desc_mod pl hide_spell_mod;
          Player.remove_desc_mod pl hide_creature_mod;
          set_data pl { (get_data pl) with se_summer = false }) )

let great_aurora : card_effect =
 fun env ->
  let p = player_of_env env in
  Slots.foreach p.slots_update (fun slot ->
      Slot.heal slot 16;
      match slot.value with
      | Some st -> Houses.incr_mana p.houses_update 3 [ st.card.c_card.house_index ]
      | None -> ())

let equinox : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  let d = ability_damage env 4 in
  Player.inflict op d;
  Slots.inflict_creatures op.slots_update d;
  Player.heal p 4;
  Slots.heal_creatures p.slots_update 4

let spawn_nectar : card_effect =
 fun env ->
  let p = player_of_env env in
  let open_slots = Slots.get_open p.slots_update in
  match open_slots with
  | [] -> ()
  | ns -> (
      let n = List.nth ns (Dice.int env.env_updater.rng (List.length ns)) in
      Slots.add_creature p.slots_update.slot_updates.(n) (make_creature nectar_bloom);
      let remain = List.filter (( <> ) n) ns in
      match remain with
      | [] -> ()
      | rs ->
          let n2 = List.nth rs (Dice.int env.env_updater.rng (List.length rs)) in
          Slots.add_creature p.slots_update.slot_updates.(n2) (make_creature nectar_bloom))

let gaea_reaction () =
  let r = Reaction.default () in
  { r with inflict = (fun s d -> if not d.is_spell then Slot.damage_slot s d) }

let gaea_attack =
  Attack_state_func
    (fun a p ->
      let count su =
        Array.fold_left (fun n s -> if Option.is_some s.value then n + 1 else n) 0 su.slot_updates
      in
      a + count p.slots_update + count (other_player p).slots_update)

let wrap_protect p =
  Array.iter
    (fun (slot : slot_update) ->
      let old = slot.protect_hook in
      slot.protect_hook <-
        (fun d ->
          let d = old d in
          Slots.foldl p.slots_update d (fun acc s ->
              match s.value with
              | Some st when st.card.c_card.key = "seasons.Cornucopia.name" ->
                  st.reaction.on_protect { de_damage = acc; de_target = Some slot.num; de_player = p }
              | _ -> acc)))
    p.slots_update.slot_updates

let wrap_opp_triple p =
  let op = other_player p in
  Array.iter
    (fun (slot : slot_update) ->
      let old = slot.protect_hook in
      slot.protect_hook <-
        (fun d ->
          let d = old d in
          if (get_data p).se_triple then { d with amount = 3 * d.amount } else d))
    op.slots_update.slot_updates

let wrap_behemoth p =
  let op = other_player p in
  let old = op.add_mana_hook in
  op.add_mana_hook <-
    (fun h incr ->
      if incr > 0 then
        let freeze =
          Array.exists
            (fun s ->
              match s.value with
              | Some st -> st.card.c_card.key = "seasons.BehemothofFrozenTime.name"
              | None -> false)
            p.slots_update.slot_updates
        in
        if freeze then freeze_mana h incr else old h incr
      else old h incr)

let season_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        Player.add_desc_mod p lower_basic;
        wrap_protect p;
        wrap_opp_triple p;
        wrap_behemoth p;
        let old_guard = p.guard_hook in
        p.guard_hook <-
          (fun d ->
            let d = old_guard d in
            if (get_data p).se_midnight && d.amount > 4 then { d with amount = d.amount - 1 } else d);
        let old = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old c in
            if c.cmd_card.house_index = special_house_index then (
              Player.add_desc_mod p (destroyed_mod c.cmd_card);
              match c.cmd_card.key with
              | "seasons.AiluvBees.name" | "seasons.SpiritofEquinox.name" ->
                  Player.remove_effect p (fun e -> e == regrowth)
              | "seasons.SolarWave.name" | "seasons.EternalSummer.name" ->
                  set_data p { (get_data p) with se_midnight = false }
              | "seasons.Cornucopia.name" | "seasons.GaeasAvatar.name" -> Player.remove_desc_mod p lower_basic
              | "seasons.BehemothofFrozenTime.name" | "seasons.TheGreatAurora.name" ->
                  Player.remove_effect p (fun e -> e == wintry)
              | _ -> ());
            if (get_data p).se_summer && c.cmd_card.key <> "seasons.EternalSummer.name" then
              Player.add_desc_mod p (if c.cmd_card.is_spell then hide_spell_mod else hide_creature_mod);
            r));
  }

let cards =
  [
    creature ~key:"seasons.AiluvBees.name" ~attack:(attack 4) ~life:16
      ~description:"seasons.AiluvBees.description"
      ~effects:(effects [ (Direct, spawn_nectar) ])
      ();
    creature ~key:"seasons.SpiritofEquinox.name" ~attack:(attack 5) ~life:12
      ~description:"seasons.SpiritofEquinox.description"
      ~effects:(effects [ (Direct, equinox) ])
      ();
    spell ~key:"seasons.SolarWave.name" ~description:"seasons.SolarWave.description"
      ~effects:(effects [ (Direct, solar_wave) ])
      ();
    spell ~key:"seasons.EternalSummer.name" ~description:"seasons.EternalSummer.description"
      ~effects:(effects [ (Direct, eternal_summer) ])
      ();
    creature ~key:"seasons.Cornucopia.name" ~attack:(attack 0) ~life:26
      ~description:"seasons.Cornucopia.description"
      ~effects:(effects [ (Direct, cornucopia_effect) ])
      ~new_reaction:cornucopia_reaction ();
    creature ~key:"seasons.GaeasAvatar.name"
      ~attack:{ base = None; sources = [ gaea_attack ] }
      ~life:60 ~description:"seasons.GaeasAvatar.description"
      ~effects:(effects [ (On_turn, gaea_effect) ])
      ~new_reaction:gaea_reaction ();
    creature ~key:"seasons.BehemothofFrozenTime.name" ~attack:(attack 7) ~life:29
      ~description:"seasons.BehemothofFrozenTime.description"
      ~effects:(effects [ (Direct, freeze) ])
      ();
    spell ~key:"seasons.TheGreatAurora.name" ~description:"seasons.TheGreatAurora.description"
      ~effects:(effects [ (Direct, great_aurora) ])
      ();
  ]

let house =
  special_house ~key:"seasons" ~id:34
    ~effects:[ (On_turn, regrowth); (On_turn, wintry) ]
    ~additional_cards:[ nectar_bloom ] ~listener:season_listener ~data:(Season_data init_data) cards
