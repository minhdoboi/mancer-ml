(* Elite Soldiers special house (by indio13): eight soldiers that can be
   permanently upgraded.  While a soldier is on the board its card is
   replaced by the matching cheap upgrade spell; playing that upgrade buffs
   the soldier and the buff is lost when the soldier dies. *)

open Sp
open Engine
open Common

let house_id = 52

let clubman_ref : card option ref = ref None
let spearman_ref : card option ref = ref None
let slinger_ref : card option ref = ref None
let crossbowman_ref : card option ref = ref None
let axeman_ref : card option ref = ref None
let swordmaster_ref : card option ref = ref None
let shotgunner_ref : card option ref = ref None
let commando_ref : card option ref = ref None

(* The [ability] replacement the soldier pushes while it is in play. *)
let clubman_mod : desc_mod ref = ref (fun _ cards -> cards)
let spearman_mod : desc_mod ref = ref (fun _ cards -> cards)
let slinger_mod : desc_mod ref = ref (fun _ cards -> cards)
let crossbowman_mod : desc_mod ref = ref (fun _ cards -> cards)
let axeman_mod : desc_mod ref = ref (fun _ cards -> cards)
let swordmaster_mod : desc_mod ref = ref (fun _ cards -> cards)
let shotgunner_mod : desc_mod ref = ref (fun _ cards -> cards)
let commando_mod : desc_mod ref = ref (fun _ cards -> cards)

let find_soldier (p : player_update) (r : card option ref) =
  match !r with Some c -> Slots.find_card p.slots_update c | None -> None

(* Permanent upgrade: raise the current (and maximum) life, and put a lasting
   attack bonus on the slot. *)
let empower ?(atk = 0) life (s : slot_update) =
  if life <> 0 then
    Slot.update s (fun st -> { st with life = st.life + life; max_life = st.max_life + life });
  if atk <> 0 then add_attack s (Attack_func (fun a -> max 0 (a + atk)))

(* While the soldier lives its deck card is the upgrade.  The mod is shared,
   so two soldiers of the same kind only ever add it once, and it comes off
   only when the last of them leaves.  [base] keeps the soldier's own
   reaction; the upgrade wiring wraps around it. *)
let with_upgrade (from_ref : card option ref) (mod_ref : desc_mod ref) (base : unit -> reaction) () =
  let r = base () in
  let old_add = r.on_add and old_remove = r.on_my_remove in
  {
    r with
    on_add =
      (fun slot ->
        old_add slot;
        let me = Reaction.bound_slot r in
        if me.num = slot.num && Option.is_some !from_ref then (
          let m = !mod_ref in
          if not (has_desc_mod me.slots.player m) then Player.add_desc_mod me.slots.player m));
    on_my_remove =
      (fun dead ->
        old_remove dead;
        let me = Reaction.bound_slot r in
        let still_here =
          Array.exists
            (fun s ->
              s != me && match s.value with Some st -> is_card !from_ref st.card.c_card | None -> false)
            me.slots.slot_updates
        in
        if not still_here then Player.remove_desc_mod me.slots.player !mod_ref);
  }

(* ------------------------------------------------------------- the soldiers *)

(* On death it deals 7 damage to the creature it faces. *)
let clubman_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        let ctx =
          {
            ctx_player = dead.d_player.pid;
            ctx_card = Some dead.d_slot.card.c_card;
            ctx_selected = dead.d_num;
          }
        in
        Slot.inflict (Slot.opposite me) (mk_damage ~is_ability:true 7 ctx));
  }

(* The creature in the opposing slot has 2 less attack while the Spearman
   holds the line. *)
let spearman_debuff pos =
  Attack_bonus { tag = "elite_spear"; pos; bonus = (fun a -> max 0 (a - 2)) }

let spearman_mark me =
  let opp = Slot.opposite me in
  let marked =
    has_source opp (function Attack_bonus b -> b.tag = "elite_spear" && b.pos = me.num | _ -> false)
  in
  if Option.is_some opp.value && not marked then add_attack opp (spearman_debuff me.num)

let spearman_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num then spearman_mark me);
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if e.s_player.pid <> me.slots.player.pid && e.s_num = me.num then spearman_mark me);
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        remove_first_attack (Slot.opposite me) (spearman_debuff me.num));
  }

(* A hit on a creature also wounds the opponent; a direct hit doubles on the
   enemy with the most life. *)
let slinger_attack target d p =
  let op = other_player p in
  match target with
  | [] -> Player.inflict op d
  | num :: _ ->
      let slot = op.slots_update.slot_updates.(num) in
      if Option.is_some slot.value then (
        Slot.inflict slot d;
        Player.inflict op d)
      else (
        Player.inflict op d;
        match reduce_filled op.slots_update highest_life with
        | Some s -> Slot.inflict s { d with amount = d.amount * 2 }
        | None -> ())

(* Sweeps every opponent creature; an empty opposing slot lets the shot
   through to the opponent.  Two kills in one volley pay one power of each
   element.  Deaths are logged, not applied, until the next flush, so the
   kills are counted from the life the volley will take off. *)
let crossbow_attack target d p =
  let op = other_player p in
  let blocked = List.exists (fun n -> Option.is_some op.slots_update.slot_updates.(n).value) target in
  let dying =
    Slots.foldl op.slots_update 0 (fun acc s ->
        match s.value with Some st when st.life - d.amount < 1 -> acc + 1 | _ -> acc)
  in
  Slots.inflict_creatures op.slots_update d;
  if dying >= 2 then Houses.incr_mana p.houses_update 1 [ 0; 1; 2; 3 ];
  if not blocked then Player.inflict op d

(* Every blow it takes from a creature is doubled and costs it 1 attack. *)
let axeman_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        let from_creature =
          d.context.ctx_player <> me.slots.player.pid
          && (not d.is_spell) && (not d.is_ability)
          && (match d.context.ctx_card with Some c -> Option.is_some c.creature_body | None -> false)
        in
        if from_creature then (
          add_attack me (Attack_func (fun a -> max 0 (a - 1)));
          { d with amount = d.amount * 2 })
        else d);
  }

(* Damage reaching it is cut to 2, and it lends 3 damage to its owner's
   spells. *)
let swordmaster_reaction () =
  let r = Reaction.default () in
  { r with self_protect = (fun d -> { d with amount = min d.amount 2 }) }

(* Every creature the opponent summons takes a volley of 5. *)
let shotgunner_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if e.s_player.pid <> me.slots.player.pid then
          let ctx =
            {
              ctx_player = me.slots.player.pid;
              ctx_card = Some (Slot.get me).card.c_card;
              ctx_selected = me.num;
            }
          in
          emit me.slots.player.updater
            (Ev_ability
               {
                 kind = "elitesoldiers.volley";
                 player = me.slots.player.pid;
                 num = me.num;
                 target = None;
               });
          Slots.inflict_creatures (other_player me.slots.player).slots_update
            (mk_damage ~is_ability:true 5 ctx));
  }

(* Steps into an empty neighbouring slot to strike from the flank, then comes
   back.  It prefers the gap that faces a creature so the second blow lands on
   one; with both neighbours filled there is no gap to step into. *)
let commando_attack target d p =
  let num = d.context.ctx_selected in
  if in_slot_range num then (
    ignore (single_target_attack target d p);
    let me = p.slots_update.slot_updates.(num) in
    match me.value with
    | None -> ()
    | Some _ -> (
        let step gap =
          Slots.move p.slots_update num gap.num;
          (* The second blow is a beat of its own, the way run_strikes gives the
             Shadow Dancer one: its own lunge at the gap, its own death flush
             and refresh, so the hit lands while the commando is there. *)
          emit p.updater (Ev_run_slot { num = gap.num; player = p.pid });
          let d2 = { d with context = { d.context with ctx_selected = gap.num } } in
          ignore (single_target_attack [ gap.num ] d2 p);
          Array.iter Player.flush p.updater.player_updates;
          emit p.updater (Ev_refresh { silent = true });
          Slots.move p.slots_update gap.num num
        in
        let gaps = Slot.open_adjacent me in
        let faces_creature s = Option.is_some (Slot.opposite s).value in
        match List.find_opt faces_creature gaps with
        | Some gap -> step gap
        | None -> ( match gaps with gap :: _ -> step gap | [] -> ())))

let clubman =
  creature ~key:"elitesoldiers.clubman" ~attack:(attack 3) ~life:17
    ~description:"elitesoldiers.clubman.description"
    ~new_reaction:(with_upgrade clubman_ref clubman_mod clubman_reaction) ()

let spearman =
  creature ~key:"elitesoldiers.spearman" ~attack:(attack 2) ~life:26
    ~description:"elitesoldiers.spearman.description"
    ~new_reaction:(with_upgrade spearman_ref spearman_mod spearman_reaction) ()

let slinger =
  creature ~key:"elitesoldiers.slinger" ~attack:(attack 3) ~life:19
    ~description:"elitesoldiers.slinger.description"
    ~new_reaction:(with_upgrade slinger_ref slinger_mod Reaction.default)
    ~run_attack:(custom_run_attack slinger_attack) ()

let crossbowman =
  creature ~key:"elitesoldiers.crossbowman" ~attack:(attack 2) ~life:21
    ~description:"elitesoldiers.crossbowman.description"
    ~new_reaction:(with_upgrade crossbowman_ref crossbowman_mod Reaction.default)
    ~run_attack:(custom_run_attack ~multi:true crossbow_attack) ()

let axeman =
  creature ~key:"elitesoldiers.axeman" ~attack:(attack 9) ~life:46
    ~description:"elitesoldiers.axeman.description"
    ~new_reaction:(with_upgrade axeman_ref axeman_mod axeman_reaction) ()

let swordmaster =
  creature ~key:"elitesoldiers.swordmaster" ~attack:(attack 6) ~life:14
    ~description:"elitesoldiers.swordmaster.description"
    ~new_reaction:(with_upgrade swordmaster_ref swordmaster_mod swordmaster_reaction)
    ~mod_:(Spell_mod (fun x -> x + 3)) ()

let shotgunner =
  creature ~key:"elitesoldiers.shotgunner" ~attack:(attack 5) ~life:32
    ~description:"elitesoldiers.shotgunner.description"
    ~new_reaction:(with_upgrade shotgunner_ref shotgunner_mod shotgunner_reaction) ()

let commando =
  creature ~key:"elitesoldiers.commando" ~attack:(attack 5) ~life:42
    ~description:"elitesoldiers.commando.description"
    ~new_reaction:(with_upgrade commando_ref commando_mod Reaction.default)
    ~run_attack:(custom_run_attack commando_attack) ()

(* ------------------------------------------------------------- the upgrades *)

let bolster_club : card_effect =
 fun env ->
  Option.iter (fun s -> empower ~atk:2 6 s) (find_soldier (player_of_env env) clubman_ref)

let wide_shield : card_effect =
 fun env ->
  Option.iter
    (fun s ->
      empower 14 s;
      List.iter (fun adj -> Slot.heal adj 4) (Slot.filled_adjacent s);
      Slot.toggle s stun_flag;
      Updater.focus env.env_updater s.num env.env_player true)
    (find_soldier (player_of_env env) spearman_ref)

let iron_sling : card_effect =
 fun env ->
  Option.iter (fun s -> empower ~atk:2 8 s) (find_soldier (player_of_env env) slinger_ref)

let ballistic : card_effect =
 fun env ->
  let p = player_of_env env in
  Option.iter
    (fun s ->
      empower ~atk:1 4 s;
      Updater.focus env.env_updater s.num p.pid false;
      Slots.move p.slots_update s.num env.env_selected)
    (find_soldier p crossbowman_ref)

let battle_shout : card_effect =
 fun env ->
  Option.iter
    (fun s ->
      empower ~atk:3 12 s;
      Updater.focus env.env_updater env.env_selected env.env_player true;
      Slot.stun (opp_slot_of_env env))
    (find_soldier (player_of_env env) axeman_ref)

let chain_mail : card_effect =
 fun env ->
  Option.iter (fun s -> empower ~atk:2 4 s) (find_soldier (player_of_env env) swordmaster_ref)

let sniper_bullets : card_effect =
 fun env ->
  Option.iter
    (fun s ->
      empower ~atk:3 10 s;
      Updater.focus env.env_updater env.env_selected env.env_player true;
      Slot.inflict (opp_slot_of_env env) (spell_damage env 13))
    (find_soldier (player_of_env env) shotgunner_ref)

let war_backpack : card_effect =
 fun env ->
  let p = player_of_env env in
  Option.iter
    (fun s ->
      empower ~atk:2 14 s;
      let op = other_player p in
      Player.add_desc_mod op hide_spell_mod;
      Player.add_effect op
        ( On_end_turn,
          one_time_player_effect (fun e ->
              Player.remove_desc_mod (player_of_env e) hide_spell_mod) ))
    (find_soldier p commando_ref)

let upgrade key description ?(input_spec = No_input) effect =
  spell ~key ~description ~input_spec ~effects:(effects [ (Direct, effect) ]) ()

let bolster = upgrade "elitesoldiers.bolster" "elitesoldiers.bolster.description" bolster_club
let wide_shield_card = upgrade "elitesoldiers.wideshield" "elitesoldiers.wideshield.description" wide_shield
let iron_sling_card = upgrade "elitesoldiers.ironsling" "elitesoldiers.ironsling.description" iron_sling

let ballistic_card =
  upgrade "elitesoldiers.ballistic" "elitesoldiers.ballistic.description" ~input_spec:Select_owner_slot
    ballistic

let battle_shout_card =
  upgrade "elitesoldiers.battleshout" "elitesoldiers.battleshout.description"
    ~input_spec:Select_target_creature battle_shout

let chain_mail_card = upgrade "elitesoldiers.chainmail" "elitesoldiers.chainmail.description" chain_mail

let sniper_bullets_card =
  upgrade "elitesoldiers.sniper" "elitesoldiers.sniper.description" ~input_spec:Select_target_creature
    sniper_bullets

let war_backpack_card = upgrade "elitesoldiers.warbackpack" "elitesoldiers.warbackpack.description" war_backpack

let () =
  clubman_ref := Some clubman;
  spearman_ref := Some spearman;
  slinger_ref := Some slinger;
  crossbowman_ref := Some crossbowman;
  axeman_ref := Some axeman;
  swordmaster_ref := Some swordmaster;
  shotgunner_ref := Some shotgunner;
  commando_ref := Some commando;
  clubman_mod := ability ~from_card:clubman ~to_card:bolster;
  spearman_mod := ability ~from_card:spearman ~to_card:wide_shield_card;
  slinger_mod := ability ~from_card:slinger ~to_card:iron_sling_card;
  crossbowman_mod := ability ~from_card:crossbowman ~to_card:ballistic_card;
  axeman_mod := ability ~from_card:axeman ~to_card:battle_shout_card;
  swordmaster_mod := ability ~from_card:swordmaster ~to_card:chain_mail_card;
  shotgunner_mod := ability ~from_card:shotgunner ~to_card:sniper_bullets_card;
  commando_mod := ability ~from_card:commando ~to_card:war_backpack_card;
  (* upgrades are cheap: they are the reward for keeping a soldier alive *)
  bolster.cost <- 1;
  wide_shield_card.cost <- 1;
  iron_sling_card.cost <- 1;
  ballistic_card.cost <- 2;
  battle_shout_card.cost <- 2;
  chain_mail_card.cost <- 2;
  sniper_bullets_card.cost <- 2;
  war_backpack_card.cost <- 3

let cards =
  [ clubman; spearman; slinger; crossbowman; axeman; swordmaster; shotgunner; commando ]

let house =
  special_house ~key:"elitesoldiers" ~id:house_id
    ~additional_cards:
      [
        bolster;
        wide_shield_card;
        iron_sling_card;
        ballistic_card;
        battle_shout_card;
        chain_mail_card;
        sniper_bullets_card;
        war_backpack_card;
      ]
    cards
