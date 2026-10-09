(* Special house Battlemage — port of priv/sp/house/Battlemage.scala.

   Scala's [attack.setDirty()] refreshes have no counterpart: attack sources are re-folded on every refresh.
   A spell hit that a field mage has taken over carries [redirected] (Scala's RedirectedContext). *)

open Sp
open Engine
open Common

let is_card opt (c : card) = match opt with Some x -> x.id = c.id | None -> false
let fieldmage_ref : card option ref = ref None
let cavalier_ref : card option ref = ref None
let retinue_ref : card option ref = ref None
let retinue_phase = "retinue"
let slot_of_env env = (player_of_env env).slots_update.slot_updates.(env.env_selected)

(* ---------------------------------------------------------------- desc mods *)

(* NextTacticsDiscount: the next special card costs 1 less *)
let next_tactics_discount : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> 4 then cards
  else Array.map (fun cd -> { cd with cd_cost = max 0 (cd.cd_cost - 1) }) cards

(* BanApocalypse: no Apocalypse while a Cavalier is in play *)
let ban_apocalypse : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> 0 then cards
  else Array.of_list (List.filter (fun cd -> cd.cd_card.key <> "fire.apocalypse") (Array.to_list cards))

(* ------------------------------------------------------------ attack sources *)

let sergeant_bonus =
  Attack_slot_state_func
    (fun a slot ->
      match slot.value with
      | Some st when st.card.c_body.b_attack.base = Some 0 -> a
      | _ -> if Option.is_none (Slot.opposite slot).value then a + 3 else a)

let cavalier_source = Attack_state_func (fun a p -> a + List.length (Slots.get_open p.slots_update))

let charge_attack target d player =
  let op = other_player player in
  let filled n = Option.is_some op.slots_update.slot_updates.(n).value in
  let blocked = List.exists filled target in
  ignore (single_target_attack target d player);
  if blocked && List.exists filled target then ignore (single_target_attack target d player)

(* ------------------------------------------------------------------- spells *)

let taunt : card_effect =
 fun env ->
  let p = player_of_env env and op = other_of_env env in
  let target_other = env.env_target_other in
  let slot = (if target_other then op else p).slots_update.slot_updates.(env.env_selected) in
  Option.iter
    (fun (s : slot_state) ->
      focus_slot slot;
      if target_other then
        match (slot_of_env env).value with Some mine when mine.attack > s.attack -> Slot.stun slot | _ -> ()
      else Player.inflict op (mk_damage ~is_ability:true s.attack (ctx_of_env env)))
    slot.value

let warcry : card_effect =
 fun env ->
  let op = other_of_env env in
  let d = mk_damage ~is_spell:true 9 (ctx_of_env env) in
  Player.inflict op d;
  Slots.inflict_creatures op.slots_update d;
  Slots.foreach op.slots_update (fun slot -> Warp.bridle (slot.remove_hook None) slot);
  Player.add_effect (player_of_env env)
    (On_end_turn, countdown 2 (fun e -> Slots.foreach (other_of_env e).slots_update Warp.unbridle))

(* Retinue: the next special card is cheaper, and the extra play it grants must be a creature *)
let retinue_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  remove_all_desc_mod p next_tactics_discount;
  Player.add_desc_mod p next_tactics_discount;
  Player.add_desc_mod p hide_spell_mod;
  Player.add_effect p (On_end_turn, one_time_player_effect (remove_desc_mod hide_spell_mod));
  Player.add_transition p (Wait_player (env.env_player, Some retinue_phase))

(* --------------------------------------------------------------- reactions *)

(* true if the field mage at [mage] takes this damage instead of its target *)
let redirects (mage : slot_update) (de : damage_event) =
  (match mage.value with Some st -> is_card !fieldmage_ref st.card.c_card && not (is_bridled st) | None -> false)
  && de.de_damage.is_spell
  && de.de_damage.context.ctx_player <> mage.slots.player.pid
  && (not de.de_damage.redirected)
  && (match de.de_target with Some t -> t <> mage.num | None -> true)
  && de.de_damage.amount > 0

let rune_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun de ->
        let me = Reaction.bound_slot r in
        (* if a field mage takes the hit, reduce the redirected hit instead, whatever the slot order *)
        let redirected =
          Array.exists (fun s -> Option.is_some s.value && redirects s de) me.slots.slot_updates
        in
        if de.de_damage.is_spell && de.de_damage.context.ctx_player <> me.slots.player.pid && not redirected
        then { de.de_damage with amount = max 0 (de.de_damage.amount - 2) }
        else de.de_damage);
  }

let sergeant_reaction () =
  attack_bonus_reaction ~cond:(fun sel num -> abs (sel - num) = 1) ~get_bonus:(fun _ -> sergeant_bonus) ()

let fieldmage_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun de ->
        let me = Reaction.bound_slot r in
        if redirects me de then (
          (* already modded by the caster side: hit the slot directly, without Player.mod_damage *)
          (Slot.get me).reaction.inflict me { de.de_damage with redirected = true };
          { de.de_damage with amount = 0 })
        else de.de_damage);
  }

let cavalier_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num = me.num && not (has_desc_mod me.slots.player ban_apocalypse) then
          Player.add_desc_mod me.slots.player ban_apocalypse);
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        let others =
          Array.exists
            (fun s ->
              s.num <> me.num
              && match s.value with Some st -> is_card !cavalier_ref st.card.c_card | None -> false)
            me.slots.slot_updates
        in
        if not others then remove_all_desc_mod me.slots.player ban_apocalypse);
  }

let retinue_reaction () =
  let r = Reaction.default () in
  {
    r with
    (* only on a real removal: a move (remove None then add) keeps the discount *)
    on_my_remove =
      (fun dead ->
        let me = Reaction.bound_slot r in
        let others =
          Array.exists
            (fun s ->
              s.num <> me.num
              && match s.value with Some st -> is_card !retinue_ref st.card.c_card | None -> false)
            me.slots.slot_updates
        in
        if Option.is_some dead && not others then remove_all_desc_mod me.slots.player next_tactics_discount);
  }

let battlemage_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.protect_hook in
            slot.protect_hook <-
              (fun d ->
                let d = old d in
                Slots.foldl p.slots_update d (fun acc s ->
                    match s.value with
                    | Some st ->
                        st.reaction.on_protect { de_damage = acc; de_target = Some slot.num; de_player = p }
                    | None -> acc)))
          p.slots_update.slot_updates;
        (* a special card other than the retinue uses up the discount *)
        let old_submit = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old_submit c in
            if c.cmd_card.house_index = 4 && not (is_card !retinue_ref c.cmd_card) then
              remove_all_desc_mod p next_tactics_discount;
            r));
  }

let fieldmage =
  creature ~key:"battlemage.fieldmage" ~attack:(attack 5) ~life:32
    ~description:"battlemage.fieldmage.description" ~new_reaction:fieldmage_reaction ()

let cavalier =
  creature ~key:"battlemage.cavalier"
    ~attack:{ base = Some 6; sources = [ cavalier_source ] }
    ~life:42 ~description:"battlemage.cavalier.description" ~new_reaction:cavalier_reaction ()

let retinue =
  creature ~key:"battlemage.retinue" ~attack:(attack 8) ~life:41 ~description:"battlemage.retinue.description"
    ~effects:(effects [ (Direct, retinue_effect) ])
    ~new_reaction:retinue_reaction ()

let () =
  fieldmage_ref := Some fieldmage;
  cavalier_ref := Some cavalier;
  retinue_ref := Some retinue

let cards =
  [
    spell ~key:"battlemage.taunt" ~description:"battlemage.taunt.description" ~input_spec:Select_any_creature
      ~effects:(effects [ (Direct, taunt) ])
      ();
    creature ~key:"battlemage.rune" ~attack:(attack 4) ~life:15 ~description:"battlemage.rune.description"
      ~new_reaction:rune_reaction ();
    creature ~key:"battlemage.sergeant" ~attack:(attack 4) ~life:23
      ~description:"battlemage.sergeant.description" ~new_reaction:sergeant_reaction ();
    creature ~key:"battlemage.knight" ~attack:(attack 4) ~life:28 ~description:"battlemage.knight.description"
      ~run_attack:(custom_run_attack charge_attack) ();
    fieldmage;
    cavalier;
    spell ~key:"battlemage.warcry" ~description:"battlemage.warcry.description"
      ~effects:(effects [ (Direct, warcry) ])
      ();
    retinue;
  ]

let house =
  {
    h_key = "battlemage";
    h_name = "battlemage";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 20;
    h_additional_cards = [];
    h_make_listener = battlemage_listener;
    h_data = No_player_data;
  }
