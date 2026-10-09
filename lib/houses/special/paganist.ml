(* Special house Paganist — original design (Zannoland).

   The class trades creatures (and its own life) for damage.  Two cards need a
   reading of the original text:

   - Sacrificial Lamb has no hand to summon from, so its death summons a random
     creature of the owner's special house into a random open slot, ignoring
     its cost.  The lamb itself is left out of the pool so the effect cannot
     loop.
   - Unholy Contract is played by choosing the victim: the caster points at an
     opponent creature, which is then sacrificed by its owner.  The engine has
     no way to ask the opponent for a sub-choice at spell time. *)

open Sp
open Engine
open Common

let lamb_ref : card option ref = ref None

let special_house_of (p : player_update) =
  Array.find_opt (fun h -> h.phd_house.h_index = special_house_index) p.pstate.ps_desc.init_desc.pd_houses

(* ------------------------------------------------------------- creatures *)

(* Killed by a spell: the owner gains the life it had when the spell landed.
   [dead.d_slot] is the pre-damage state, so this is the remaining life the
   card asks for.  A destroy effect carries no damage and heals nothing. *)
let virgin_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        match dead.d_damage with
        | Some d when d.is_spell ->
            emit dead.d_player.updater
              (Ev_ability
                 {
                   kind = "paganist.virgin";
                   player = dead.d_player.pid;
                   num = dead.d_num;
                   target = None;
                 });
            Player.heal dead.d_player dead.d_slot.life
        | _ -> ());
  }

let lamb_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        let p = dead.d_player in
        let pool =
          match special_house_of p with
          | None -> []
          | Some h ->
              (h.phd_house.h_cards @ h.phd_house.h_additional_cards)
              |> List.filter (fun (c : card) ->
                     Option.is_some c.creature_body && not (is_card !lamb_ref c))
        in
        match pool with
        | [] -> ()
        | _ -> (
            match Slots.get_open p.slots_update with
            | [] -> ()
            | opens ->
                let rng = p.updater.rng in
                let card = List.nth pool (Dice.int rng (List.length pool)) in
                let num = List.nth opens (Dice.int rng (List.length opens)) in
                ignore (Slots.summon p.slots_update num (make_creature card))));
  }

let moloch_heal : card_effect =
 fun env ->
  let p = player_of_env env and op = other_of_env env in
  Player.heal p 4;
  Player.heal op 4;
  Slots.heal_creatures p.slots_update 4;
  Slots.heal_creatures op.slots_update 4

(* All players and creatures, the owner and Moloch itself included. *)
let moloch_attack _target d player =
  let op = other_player player in
  Player.inflict op d;
  Slots.inflict_creatures op.slots_update d;
  Player.inflict player d;
  Slots.inflict_creatures player.slots_update d

(* ---------------------------------------------------------------- spells *)

let final_offering : card_effect =
 fun env ->
  let p = player_of_env env in
  let victims =
    Slots.foldl p.slots_update [] (fun acc s -> s :: acc)
    |> List.sort (fun a b -> compare (Slot.get b).card.c_card.cost (Slot.get a).card.c_card.cost)
    |> List.filteri (fun i _ -> i < 3)
  in
  let total = List.fold_left (fun n s -> n + (Slot.get s).card.c_card.cost) 0 victims in
  Player.inflict (other_of_env env) (spell_damage env total);
  List.iter Slot.destroy victims

let unholy_contract : card_effect =
 fun env ->
  let slot = opp_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some st ->
      let cost = st.card.c_card.cost in
      focus_slot ~blocking:true slot;
      Slot.destroy slot;
      Player.inflict (other_of_env env) (spell_damage env (max 0 (13 - cost)))

let terrorize : card_effect =
 fun env ->
  let slot = opp_slot_of_env env in
  if Option.is_some slot.value then Slot.destroy slot;
  let op = other_of_env env in
  Player.inflict op (spell_damage env 13);
  Player.add_desc_mod op incr_basic_cost_mod;
  Player.add_malus op cost_malus 1

let obliterate : card_effect =
 fun env ->
  let p = player_of_env env in
  Slots.foreach (other_of_env env).slots_update Slot.destroy;
  Houses.incr_mana p.houses_update (-3) [ 0; 1; 2; 3; 4 ];
  Player.add_desc_mod p skip_turn_mod;
  (* The current end of turn is the first tick; the mod goes away at the end of
     the caster's next turn, after it has locked that turn. *)
  Player.add_effect p (On_end_turn, countdown 2 (remove_desc_mod skip_turn_mod))

let river_of_blood : card_effect =
 fun env ->
  let p = player_of_env env in
  let d = spell_damage env 25 in
  Player.inflict p d;
  if p.pstate.ps_life > 0 && Option.is_none env.env_updater.ended then (
    (* the flood is announced only when it happens: a caster that bled out sends none *)
    emit p.updater (Ev_ability { kind = "paganist.flood"; player = p.pid; num = 0; target = None });
    let op = other_of_env env in
    Player.inflict op d;
    Slots.inflict_creatures op.slots_update d)

(* ----------------------------------------------------------------- cards *)

let virgin =
  creature ~key:"paganist.virgin" ~attack:(attack 3) ~life:15
    ~description:"paganist.virgin.description" ~new_reaction:virgin_reaction ()

let lamb =
  creature ~key:"paganist.lamb" ~attack:(attack 4) ~life:14 ~description:"paganist.lamb.description"
    ~new_reaction:lamb_reaction ()

let moloch =
  creature ~key:"paganist.moloch" ~attack:(attack 7) ~life:48 ~description:"paganist.moloch.description"
    ~input_spec:Select_owner_creature ~run_attack:(custom_run_attack ~multi:true moloch_attack)
    ~effects:(effects [ (On_turn, moloch_heal) ])
    ()

let () = lamb_ref := Some lamb

let cards =
  [
    virgin;
    lamb;
    spell ~key:"paganist.offering" ~description:"paganist.offering.description"
      ~effects:(effects [ (Direct, final_offering) ])
      ();
    spell ~key:"paganist.contract" ~description:"paganist.contract.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, unholy_contract) ])
      ();
    spell ~key:"paganist.terrorize" ~description:"paganist.terrorize.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, terrorize) ])
      ();
    moloch;
    spell ~key:"paganist.obliterate" ~description:"paganist.obliterate.description"
      ~effects:(effects [ (Direct, obliterate) ])
      ();
    spell ~key:"paganist.blood" ~description:"paganist.blood.description"
      ~effects:(effects [ (Direct, river_of_blood) ])
      ();
  ]

let house = special_house ~key:"paganist" ~id:44 cards
