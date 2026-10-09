(* Special house Dark Priest — port of priv/sp/house/DarkPriest.scala. A Restless Soul is planted on
   the opponent at the start of the game and respawns while missing. *)

open Sp
open Engine
open Common

let ghost_ref : card option ref = ref None
let restless_ref : card option ref = ref None
let shadow_ref : card option ref = ref None
let heretic_ref : card option ref = ref None
let angel_ref : card option ref = ref None

let filled_count (p : player_update) =
  Array.fold_left (fun n s -> if Option.is_some s.value then n + 1 else n) 0 p.slots_update.slot_updates

let spawn_in_open (p : player_update) (card : card) =
  match Slots.get_open p.slots_update with
  | [] -> ()
  | ns ->
      let n = List.nth ns (Dice.int p.updater.rng (List.length ns)) in
      let slot = p.slots_update.slot_updates.(n) in
      Slots.add_creature slot (make_creature card);
      focus_slot ~blocking:false slot

let spawn_restless (p : player_update) = Option.iter (spawn_in_open p) !restless_ref

let has_restless (p : player_update) =
  Array.exists
    (fun s -> match s.value with Some st -> is_card !restless_ref st.card.c_card | None -> false)
    p.slots_update.slot_updates

let spawn_restless_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  if not (has_restless p) then spawn_restless p

let init_restless : card_effect =
 fun env ->
  let op = other_of_env env in
  Player.add_effect op (On_end_turn, spawn_restless_effect);
  spawn_restless op

let black_mass_x (su : slots_update) =
  let ids =
    Array.fold_left
      (fun acc s -> match s.value with Some st -> st.card.c_card.house_id :: acc | None -> acc)
      [] su.slot_updates
  in
  4 * List.length (List.sort_uniq compare ids)

let black_mass : card_effect =
 fun env ->
  let op = other_of_env env in
  let x = black_mass_x op.slots_update in
  Slots.inflict_creatures op.slots_update (spell_damage env x);
  Slot.destroy (own_slot_of_env env)

let occult : card_effect =
 fun env ->
  let slot = opp_slot_of_env env in
  if Option.is_none slot.value then
    Option.iter (fun c -> Slots.add_creature slot (make_creature c)) !shadow_ref

let shadow_heal : card_effect =
 fun env ->
  focus_slot (own_slot_of_env env);
  let op = other_of_env env in
  Player.heal op 1;
  Slots.heal_creatures op.slots_update 1

let ghost_heal : card_effect = fun env -> Player.heal (player_of_env env) (filled_count (player_of_env env))

let evampire : card_effect =
 fun env ->
  let houses =
    Slot.filled_adjacent (own_slot_of_env env) |> List.map (fun s -> (Slot.get s).card.c_card.house_index)
  in
  if houses <> [] then incr_mana_from (own_slot_of_env env) (player_of_env env) 1 houses

let betray : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = p.slots_update.slot_updates.(env.env_selected) in
  let d = ability_damage env 4 in
  focus_slot slot;
  Player.inflict p d;
  Slot.inflict slot d;
  List.iter (fun s -> Slot.inflict s d) (Slot.adjacent slot)

let missionar : card_effect =
 fun env ->
  let p = player_of_env env in
  let weakest =
    Slots.foldl p.slots_update None (fun acc s ->
        if s.num = env.env_selected then acc
        else
          match acc with
          | None -> Some s
          | Some a -> if (Slot.get a).attack > (Slot.get s).attack then Some s else acc)
  in
  Option.iter
    (fun w ->
      let house_index = (Slot.get w).card.c_card.house_index in
      Slot.inflict w (ability_damage env ((Slot.get w).life / 2));
      let weakest_other =
        Slots.foldl (other_of_env env).slots_update None (fun acc s ->
            if (Slot.get s).card.c_card.house_index <> house_index then acc
            else
              match acc with
              | None -> Some s
              | Some a -> if (Slot.get a).attack > (Slot.get s).attack then Some s else acc)
      in
      Option.iter (fun s -> Slot.inflict s (ability_damage env ((Slot.get s).life / 2))) weakest_other)
    weakest

let restless_reaction () =
  let r = Reaction.default () in
  { r with on_my_death = (fun dead -> Houses.incr_mana (other_player dead.d_player).houses_update 2 [ 4 ]) }

let ghost_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        let from_effect = match dead.d_damage with None -> true | Some d -> is_effect d in
        if from_effect then Option.iter (spawn_in_open (other_player dead.d_player)) !ghost_ref);
  }

let missionary_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if e.s_player.pid = me.slots.player.pid then (
          if e.s_card.c_card.house_index = special_house_index then (
            Slot.destroy me;
            Option.iter (fun c -> Slots.add_creature me (make_creature c)) !angel_ref)
          else
            let slot = e.s_player.slots_update.slot_updates.(e.s_num) in
            Slot.destroy slot;
            Option.iter (fun c -> Slots.add_creature slot (make_creature c)) !heretic_ref));
  }

let black_monk_reaction () =
  let r = Reaction.default () in
  { r with on_my_damage = (fun d -> Player.heal (Reaction.bound_slot r).slots.player d.amount) }

let one_attack_bonus = Attack_func (fun a -> a + 1)

let hydra_attack target d player =
  let op = other_player player in
  Player.inflict op d;
  Slots.inflict_creatures op.slots_update d;
  Player.heal player d.amount;
  match target with
  | num :: _ ->
      let slot = player.slots_update.slot_updates.(num) in
      if Option.is_some slot.value then add_attack slot one_attack_bonus
  | [] -> ()

let angel_attack target d player =
  match target with
  | [] -> Player.inflict (other_player player) d
  | num :: _ -> (
      let slot = (other_player player).slots_update.slot_updates.(num) in
      match slot.value with
      | None -> Player.inflict (other_player player) d
      | Some _ -> (
          Slot.inflict slot d;
          if Option.is_none slot.value then
            match !angel_ref with
            | Some c ->
                Slot.heal
                  player.slots_update.slot_updates.(num)
                  (match c.creature_body with Some b -> b.b_life | None -> 0)
            | None -> ()))

let restless =
  creature ~key:"darkpriest.RestlessSoul.name" ~attack:(attack 3) ~life:11
    ~description:"darkpriest.RestlessSoul.description" ~new_reaction:restless_reaction ()

let shadow_priest =
  creature ~key:"darkpriest.ShadowOfPriest.name" ~attack:(attack 3) ~life:13
    ~description:"darkpriest.ShadowOfPriest.description"
    ~effects:(effects [ (On_turn, shadow_heal) ])
    ()

let heretic = creature ~key:"darkpriest.Heretic.name" ~attack:(attack 6) ~life:20 ~description:"" ()

let black_angel =
  creature ~key:"darkpriest.BlackAngel.name" ~attack:(attack 8) ~life:25
    ~description:"darkpriest.BlackAngel.description" ~run_attack:(custom_run_attack angel_attack) ()

let () =
  restless_ref := Some restless;
  shadow_ref := Some shadow_priest;
  heretic_ref := Some heretic;
  angel_ref := Some black_angel

let ghost =
  creature ~key:"darkpriest.Ghost.name" ~attack:(attack 5) ~life:16
    ~description:"darkpriest.Ghost.description" ~new_reaction:ghost_reaction
    ~effects:(effects [ (Direct, ghost_heal) ])
    ()

let () = ghost_ref := Some ghost

let cards =
  [
    ghost;
    creature ~key:"darkpriest.Occultist.name" ~attack:(attack 4) ~life:20
      ~description:"darkpriest.Occultist.description"
      ~effects:(effects [ (Direct, occult) ])
      ();
    spell ~key:"darkpriest.BlackMass.name" ~description:"darkpriest.BlackMass.description"
      ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, black_mass) ])
      ();
    creature ~key:"darkpriest.EnergyVampire.name" ~attack:(attack 3) ~life:23
      ~description:"darkpriest.EnergyVampire.description"
      ~effects:(effects [ (On_turn, evampire) ])
      ();
    creature ~key:"darkpriest.BlackMonk.name" ~attack:(attack 4) ~life:25
      ~description:"darkpriest.BlackMonk.description" ~new_reaction:black_monk_reaction ();
    creature ~key:"darkpriest.Betrayer.name" ~attack:(attack 7) ~life:38
      ~description:"darkpriest.Betrayer.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (On_turn, betray) ])
      ();
    creature ~key:"darkpriest.DarkHydra.name" ~attack:(attack 1) ~life:32
      ~description:"darkpriest.DarkHydra.description"
      ~run_attack:(custom_run_attack ~multi:true hydra_attack)
      ();
    creature ~key:"darkpriest.Missionary.name" ~attack:(attack 3) ~life:36
      ~description:"darkpriest.Missionary.description" ~new_reaction:missionary_reaction
      ~effects:(effects [ (Direct, missionar) ])
      ();
  ]

let house =
  special_house ~key:"darkpriest" ~id:39
    ~effects:[ (On_start, init_restless) ]
    ~additional_cards:[ restless; shadow_priest; heretic; black_angel ]
    cards
