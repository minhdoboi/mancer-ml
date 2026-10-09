(* Special house Antimancer — port of priv/sp/house/Antimancer.scala. Costs run from 2 to 9, see [cost_fn]. *)

open Sp
open Engine
open Common

let cost_fn i = i + 2
let retaliator_ref : card option ref = ref None
let bombardier_ref : card option ref = ref None
let mirror_priest_ref : card option ref = ref None
let angry_mob_ref : card option ref = ref None

(* Spell: an angry mob in every empty slot that faces an empty slot *)
let resist : card_effect =
 fun env ->
  let p = player_of_env env in
  List.iter
    (fun num ->
      let slot = p.slots_update.slot_updates.(num) in
      if Option.is_none slot.value && Option.is_none (Slot.opposite slot).value then
        Option.iter (fun m -> Slots.add_creature slot (make_creature m)) !angry_mob_ref)
    p.pstate.ps_slot_list

(* Spell: steal the target enemy creature into a random empty slot *)
let bribe : card_effect =
 fun env ->
  let p = player_of_env env in
  match Dice.shuffle env.env_updater.rng (Slots.get_open p.slots_update) with
  | [] -> ()
  | num :: _ ->
      let opp = opp_slot_of_env env in
      Option.iter
        (fun (st : slot_state) ->
          Slot.destroy opp;
          Slots.add_creature p.slots_update.slot_updates.(num) st.card)
        opp.value

let cost7_or_less pid (state : game_state) =
  List.init nb_slots Fun.id
  |> List.filter (fun n ->
      match state.players.(pid).ps_slots.(n) with Some st -> st.card.c_card.cost < 8 | None -> false)

let voodoo : card_effect =
 fun env ->
  let op = other_of_env env in
  let d = ability_damage env 1 in
  let filled = ref 0 in
  Slots.foreach op.slots_update (fun s ->
      incr filled;
      drain s d);
  Player.heal (player_of_env env) !filled

let retaliator_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun damage ->
        let me = Reaction.bound_slot r in
        Slots.inflict_creatures (other_player me.slots.player).slots_update
          (mk_damage ~is_ability:true damage.amount
             { ctx_player = me.slots.player.pid; ctx_card = !retaliator_ref; ctx_selected = me.num });
        focus_slot me);
  }

(* A dead creature of the owner that was killed by a spell or destroyed comes back *)
let martyr_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        let spell_or_destroy = match dead.d_damage with None -> true | Some d -> d.is_spell in
        if spell_or_destroy then (
          Slots.add_creature me.slots.slot_updates.(dead.d_num) dead.d_slot.card;
          focus_slot me));
  }

let harvester_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        incr_mana_from me me.slots.player 1 [ Dice.int me.slots.player.updater.rng 4 ];
        focus_slot me);
  }

let bombardier_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        Slot.inflict (Slot.opposite me)
          (mk_damage ~is_ability:true 28
             { ctx_player = me.slots.player.pid; ctx_card = !bombardier_ref; ctx_selected = me.num }));
  }

let antimancer_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        init_owner_death p;
        let op = other_player p in
        (* a Mirror priest gives its owner 1 mana of whatever the opponent plays *)
        let old_submit = op.submit_command_hook in
        op.submit_command_hook <-
          (fun c ->
            let r = old_submit c in
            Slots.foreach p.slots_update (fun s ->
                match s.value with
                | Some st when is_card !mirror_priest_ref st.card.c_card && not (is_bridled st) ->
                    incr_mana_from s p 1 [ c.cmd_card.house_index ];
                    focus_slot s
                | _ -> ());
            r);
        (* a Bombardier's player damage hits every enemy creature too *)
        let old_damage = op.on_player_damage_hook in
        op.on_player_damage_hook <-
          (fun d ->
            old_damage d;
            Slots.foreach p.slots_update (fun s ->
                match s.value with
                | Some st
                  when is_card !bombardier_ref st.card.c_card
                       && (not (is_bridled st))
                       && d.context.ctx_selected = s.num && d.context.ctx_player = p.pid ->
                    Slots.inflict_creatures op.slots_update d;
                    focus_slot s
                | _ -> ())));
  }

let angry_mob = creature ~key:"antimancer.0.name" ~attack:(attack 3) ~life:9 ~description:"" ()

let retaliator =
  creature ~key:"antimancer.4.name" ~attack:(attack 5) ~life:10 ~description:"antimancer.4.description"
    ~new_reaction:retaliator_reaction ()

let bombardier =
  creature ~key:"antimancer.8.name" ~attack:(attack 5) ~life:34 ~description:"antimancer.8.description"
    ~new_reaction:bombardier_reaction ()

let mirror_priest =
  creature ~key:"antimancer.2.name" ~attack:(attack 2) ~life:12 ~description:"antimancer.2.description" ()

let () =
  retaliator_ref := Some retaliator;
  bombardier_ref := Some bombardier;
  mirror_priest_ref := Some mirror_priest;
  angry_mob_ref := Some angry_mob

let cards =
  [
    mirror_priest;
    spell ~key:"antimancer.3.name" ~description:"antimancer.3.description"
      ~effects:(effects [ (Direct, resist) ])
      ();
    retaliator;
    creature ~key:"antimancer.5.name" ~attack:(attack 4) ~life:17 ~description:"antimancer.5.description"
      ~new_reaction:martyr_reaction ();
    creature ~key:"antimancer.6.name" ~attack:(attack 7) ~life:37 ~description:"antimancer.6.description"
      ~new_reaction:harvester_reaction ();
    creature ~key:"antimancer.7.name" ~attack:(attack 8) ~life:39 ~description:"antimancer.7.description"
      ~effects:(effects [ (On_turn, voodoo) ])
      ();
    bombardier;
    spell ~key:"antimancer.9.name" ~description:"antimancer.9.description"
      ~input_spec:(Select_target cost7_or_less)
      ~effects:(effects [ (Direct, bribe) ])
      ();
  ]

let house =
  special_house ~key:"antimancer" ~id:18 ~additional_cards:[ angry_mob ] ~listener:antimancer_listener cards
