(* Special house Trappers (zhupons). The Trappers have an incredible link with the
   dark forces of the earth: every card denies the opponent's creatures their
   attacks or their abilities, with the help of the doom roots and the dark
   forest. "Skips its next attack" is the stun flag, which blocks the creature's
   next run and is cleared by the owner's end-of-turn toggle_run; Poisonous Roots
   holds the opponent's creatures through their next turn with the same stun,
   released by its own end-of-turn tick. A card that holds creatures off for as
   long as it lives (Dark Pixie) uses that same stun, put back on every turn of
   its owner, so the held creature never gets a run. *)

open Sp
open Engine
open Common

let house_id = 47

(* The whip attributes its damage to its own card; refs are set after the card
   list below (shaman's wolf_ref pattern). *)
let whip_ref : card option ref = ref None
let medusa_ref : card option ref = ref None

(* ------------------------------------------------------------- creatures *)

(* Whip Master (3|11): while it is in play, every creature the opponent
   summons makes the opponent lose 5 life. The "turn after it is summoned"
   window is every opponent summon: turns alternate, so each opponent summon
   happens on a later turn than the Master's. *)
let whip_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        let p = me.slots.player in
        if e.s_player.pid <> p.pid then
          Player.inflict (other_player p)
            (mk_damage ~is_ability:true 5
               { ctx_player = p.pid; ctx_card = !whip_ref; ctx_selected = me.num }));
  }

(* Psychic Shaman (4|15): when the Shaman is summoned, the creature in the
   opposing slot skips its next attack. The Shaman itself starts runnable
   (fire.drake's run_flag), so it attacks the turn it is summoned. *)
let shaman_stun : card_effect = fun env -> Slot.stun (Slot.opposite (own_slot_of_env env))

(* Dark Pixie (5|12): the creature in the slot opposing the Pixie cannot attack
   for as long as the Pixie is in play. The stun flag is the "cannot attack"
   mark; the engine wipes it at the owner's end-of-turn toggle_run, so the Pixie
   puts it back on every turn of its owner, which is before the opponent's
   creatures run again. The reaction holds the creature as it arrives and
   releases it when the Pixie leaves; the turn effect re-holds the slot, which
   a creature can move into. The flag is set directly, not through the
   occupant's stun reaction, so a creature with a custom reaction (the
   Darksider dragon) is held as well. *)
let pixie_hold target = match target.value with Some _ -> Slot.toggle target stun_flag | None -> ()

let pixie_release target = Slot.toggle_off target stun_flag

let pixie_lock : card_effect = fun env -> pixie_hold (Slot.opposite (own_slot_of_env env))

let pixie_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num then pixie_hold (Slot.opposite me));
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if e.s_player.pid <> me.slots.player.pid && e.s_num = me.num then pixie_hold (Slot.opposite me));
    on_my_remove = (fun _ -> pixie_release (Slot.opposite (Reaction.bound_slot r)));
  }

(* Psychic Medusa (9|41): each turn, the opponent creature with the highest
   life skips its attack. *)
let medusa_guard : card_effect =
 fun env ->
  let rec guard e =
    let p = player_of_env e in
    match Slots.find_card p.slots_update (Option.get !medusa_ref) with
    | Some _ -> (
        match reduce_filled (other_player p).slots_update highest_life with
        | Some s -> Slot.stun s
        | None -> ())
    | None -> Player.remove_effect p (fun f -> f == guard)
  in
  Player.add_effect (player_of_env env) (On_end_turn, guard)

(* Mind Witch (8|38): the creature in the opposing slot loses its ability —
   the Warp silence ("bridle"), kept fresh for every creature that arrives in
   that slot and released when the Witch leaves. *)
let witch_silence me =
  let opp = Slot.opposite me in
  match opp.value with
  | Some st when not (is_bridled st) ->
      ignore (opp.remove_hook None);
      Slot.set_data me (Witch_bound st.slot_id);
      Warp.bridle st opp
  | _ -> ()

let witch_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num then witch_silence me);
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if e.s_player.pid <> me.slots.player.pid && e.s_num = me.num then witch_silence me);
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some { data = Witch_bound id; _ } ->
            (* the creature may have moved *)
            Option.iter Warp.unbridle (Slots.find_slot_id (other_player me.slots.player).slots_update id)
        | _ -> ());
  }

(* ---------------------------------------------------------------- spells *)

(* Underworld Rose: the target creature skips its next attack and takes damage
   equal to half its life, rounded up. *)
let rose : card_effect =
 fun env ->
  let slot = opp_slot_of_env env in
  match slot.value with
  | Some st ->
      let damage = (st.life + 1) / 2 in
      Slot.stun slot;
      Slot.inflict slot (spell_damage env damage)
  | None -> ()

(* Underground Poison: destroys the target opponent creature and gives the
   opponent mana equal to the destroyed creature's cost minus 3, in that
   creature's house. *)
let underground_poison : card_effect =
 fun env ->
  let op = other_of_env env in
  let slot = opp_slot_of_env env in
  match slot.value with
  | Some (s : slot_state) ->
      let cost = s.card.c_card.cost in
      Slot.destroy slot;
      Houses.incr_mana op.houses_update (max 0 (cost - 3)) [ s.card.c_card.house_index ]
  | None -> ()

(* Poisonous Roots: the opponent's creatures cannot attack on their next turn
   and all the opponent's powers are reduced by 1. The creatures are stunned like
   any other skipped attack. Like [countdown], the lock rebuilds itself on every
   tick instead of counting in a ref, because simulations share the effect
   closures of the state they copy; one tick holds the opponent through their
   next turn and releases at its end. *)
let roots_lock n =
  let rec make n =
    let rec tick env =
      let p = player_of_env env in
      let held = n > 1 in
      Slots.foreach p.slots_update (fun s ->
          if held then Slot.stun s else Slot.toggle_off s stun_flag);
      if held then Player.map_effect p (fun e -> if e == tick then make (n - 1) else e)
      else Player.remove_effect p (fun e -> e == tick)
    in
    tick
  in
  make n

let poisonous_roots : card_effect =
 fun env ->
  let op = other_of_env env in
  Slots.foreach op.slots_update Slot.stun;
  Houses.incr_mana op.houses_update (-1) [ 0; 1; 2; 3; 4 ];
  Player.add_effect op (On_end_turn, roots_lock 1)

(* ------------------------------------------------------------------ house *)

(* The list is in printed cost order, so the deck builder's default index+1
   gives the printed costs 1..8. *)
let cards =
  [
    creature ~key:"trappers.psychicshaman" ~attack:(attack 4) ~life:15
      ~description:"trappers.psychicshaman.description" ~status:run_flag
      ~effects:(effects [ (Direct, shaman_stun) ]) ();
    creature ~key:"trappers.whipmaster" ~attack:(attack 3) ~life:11
      ~description:"trappers.whipmaster.description" ~new_reaction:whip_reaction ();
    spell ~key:"trappers.rose" ~description:"trappers.rose.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, rose) ]) ();
    creature ~key:"trappers.darkpixie" ~attack:(attack 5) ~life:12
      ~description:"trappers.darkpixie.description" ~new_reaction:pixie_reaction
      ~effects:(effects [ (On_turn, pixie_lock) ]) ();
    spell ~key:"trappers.undergroundpoison" ~description:"trappers.undergroundpoison.description"
      ~input_spec:Select_target_creature ~effects:(effects [ (Direct, underground_poison) ]) ();
    spell ~key:"trappers.roots" ~description:"trappers.roots.description"
      ~effects:(effects [ (Direct, poisonous_roots) ]) ();
    creature ~key:"trappers.mindwitch" ~attack:(attack 8) ~life:38
      ~description:"trappers.mindwitch.description" ~new_reaction:witch_reaction ();
    creature ~key:"trappers.psychicmedusa" ~attack:(attack 9) ~life:41
      ~description:"trappers.psychicmedusa.description" ~effects:(effects [ (Direct, medusa_guard) ]) ();
  ]

(* Printed costs: Psychic Shaman 1, Whip Master 2, Underworld Rose 3, Dark
   Pixie 4, Underground Poison 5, Poisonous Roots 6, Mind Witch 7, Psychic
   Medusa 8. *)
let cost_fn i = i + 1

let () =
  whip_ref := Some (List.nth cards 1);
  medusa_ref := Some (List.nth cards 7)

let house = special_house ~key:"trappers" ~id:house_id cards
