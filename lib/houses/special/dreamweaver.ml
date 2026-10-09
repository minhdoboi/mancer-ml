(* Special house Dreamweaver — port of priv/sp/house/Dreamweaver.scala.

   Scala's [attack.setDirty()] calls (Ethereal, Nightmare, Sword reactions and the Roc refresh) have no
   counterpart: attack sources are re-folded on every refresh, see Player.refresh_slots. *)

open Sp
open Engine
open Common

let castle_ref : card option ref = ref None

let nb_creatures (p : player_update) =
  Array.fold_left (fun n s -> if Option.is_some s.value then n + 1 else n) 0 p.slots_update.slot_updates

(* Ethereal wisp and Night mare: +1 for each other creature of the owner *)
let ethereal_source = Attack_state_func (fun a p -> a + nb_creatures p - 1)

(* Roc hatchling: halved (integer division, as in Scala) while the slot opposite is empty *)
let roc_source =
  Attack_slot_state_func (fun a slot -> if Option.is_some (Slot.opposite slot).value then a else a / 2)

let sword_source = Attack_slot_state_func (fun a slot -> a + List.length (Slot.filled_adjacent slot))

let aurora : card_effect =
 fun env ->
  let aur (p : player_update) =
    Slots.foreach p.slots_update (fun s ->
        let card = (Slot.get s).card.c_card in
        Slot.heal s 12;
        Houses.incr_mana p.houses_update 1 [ card.house_index ])
  in
  aur (player_of_env env);
  aur (other_of_env env)

let guide : card_effect =
 fun env ->
  let p = player_of_env env in
  let nb = nb_creatures p in
  let dp = get_mana env 4 in
  Slots.foreach p.slots_update (fun s -> if s.num <> env.env_selected then Slot.heal s dp);
  Player.heal p (nb * 3)

let mare : card_effect =
 fun env ->
  let op = other_of_env env in
  let empty = nb_slots - nb_creatures op in
  Slots.inflict_creatures op.slots_update (ability_damage env (6 * empty))

(* Wisp and Wisp-like: enemy spells and abilities do nothing *)
let ethereal_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        if is_effect d && d.context.ctx_player <> me.slots.player.pid then { d with amount = 0 } else d);
  }

let rainbow_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if me.slots.player.pid <> e.s_player.pid then
          Houses.incr_mana (other_player e.s_player).houses_update 3 [ e.s_card.c_card.house_index ]);
  }

(* Flying castle: every other creature of its owner takes 2 less. [protect] runs it over the castles. *)
let castle_protect (de : damage_event) (castle : slot_update) =
  match de.de_target with
  | Some t when t <> castle.num -> { de.de_damage with amount = max 0 (de.de_damage.amount - 2) }
  | _ -> de.de_damage

let castle_reaction () =
  let r = Reaction.default () in
  { r with on_protect = (fun de -> castle_protect de (Reaction.bound_slot r)) }

let sword_attack target d player =
  let num = List.hd target in
  let op = other_player player in
  let opp = op.slots_update.slot_updates.(num) in
  List.iter
    (fun (slot : slot_update) ->
      if slot.num = num && Option.is_none slot.value then Player.inflict op d;
      if Option.is_some slot.value then Slot.inflict slot d)
    (opp :: Slot.adjacent opp)

let dreamweaver_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        init_owner_death p;
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.protect_hook in
            slot.protect_hook <-
              (fun d ->
                let d = old d in
                Slots.foldl p.slots_update d (fun acc s ->
                    match s.value with
                    | Some st when is_card !castle_ref st.card.c_card ->
                        st.reaction.on_protect { de_damage = acc; de_target = Some slot.num; de_player = p }
                    | _ -> acc)))
          p.slots_update.slot_updates);
  }

let wisp =
  creature ~key:"dreamweaver.wisp"
    ~attack:{ base = Some 2; sources = [ ethereal_source ] }
    ~life:8 ~description:"dreamweaver.wisp.description" ~new_reaction:ethereal_reaction ()

let roc =
  creature ~key:"dreamweaver.roc"
    ~attack:{ base = Some 6; sources = [ roc_source ] }
    ~life:17 ~description:"dreamweaver.roc.description" ()

let castle =
  creature ~key:"dreamweaver.castle" ~attack:(attack 2) ~life:37 ~description:"dreamweaver.castle.description"
    ~run_attack:multi_run_attack ~new_reaction:castle_reaction ()

let () = castle_ref := Some castle

let cards =
  [
    wisp;
    roc;
    spell ~key:"dreamweaver.aurora" ~description:"dreamweaver.aurora.description"
      ~effects:(effects [ (Direct, aurora) ])
      ();
    creature ~key:"dreamweaver.guide" ~attack:(attack 4) ~life:19 ~description:"dreamweaver.guide.description"
      ~effects:(effects [ (Direct, guide) ])
      ();
    creature ~key:"dreamweaver.sword"
      ~attack:{ base = Some 4; sources = [ sword_source ] }
      ~life:28 ~description:"dreamweaver.sword.description"
      ~run_attack:(custom_run_attack ~multi:true sword_attack)
      ();
    creature ~key:"dreamweaver.butterfly" ~attack:(attack 4) ~life:31
      ~description:"dreamweaver.butterfly.description" ~new_reaction:rainbow_reaction ();
    castle;
    creature ~key:"dreamweaver.mare"
      ~attack:{ base = Some 5; sources = [ ethereal_source ] }
      ~life:44 ~description:"dreamweaver.mare.description"
      ~effects:(effects [ (Direct, mare) ])
      ();
  ]

let house = special_house ~key:"dreamweaver" ~id:17 ~listener:dreamweaver_listener cards
