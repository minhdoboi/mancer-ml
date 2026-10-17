(* Special house Troopers — port of priv/sp/house/Troopers.scala. *)

open Sp
open Engine
open Common

let conscription_cost i = if i = 0 then 0 else i + 1
let marine_card : card option ref = ref None

let marine_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if me.slots.player.pid <> e.s_player.pid then (
          Updater.focus me.slots.player.updater me.num me.slots.player.pid true;
          let d =
            mk_damage ~is_ability:true 4
              { ctx_player = me.slots.player.pid; ctx_card = !marine_card; ctx_selected = me.num }
          in
          Slot.inflict e.s_player.slots_update.slot_updates.(e.s_num) d));
  }

let goliath_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d -> if is_effect d then { d with amount = 0 } else { d with amount = max 0 (d.amount - 1) });
  }

let siege : card_effect =
 fun env ->
  let target =
    Slots.foldl (other_of_env env).slots_update None (fun acc s ->
        match acc with
        | None -> Some s
        | Some a -> if (Slot.get s).life >= (Slot.get a).life then Some s else acc)
  in
  Option.iter (fun s -> Slot.inflict s (ability_damage env 8)) target

let marine =
  creature ~key:"Marine" ~attack:(attack 4) ~life:17 ~description:"troopers.marine.description"
    ~new_reaction:marine_reaction ()

let () = marine_card := Some marine

let cards =
  [
    spell ~key:"Conscription" ~description:"troopers.conscription.description"
      ~effects:(effects [ (Direct, add_mana 1 [ 4 ]) ])
      ();
    marine;
    creature ~key:"Barracks" ~attack:(attack 2) ~life:17 ~description:"troopers.barracks.description"
      ~effects:(effects [ (On_turn, add_mana 1 [ 4 ]) ])
      ();
    creature ~key:"Wraith" ~attack:(attack 4) ~life:24 ~description:"troopers.wraith.description"
      ~effects:(effects [ (On_turn, focus (damage_creatures ~is_ability:true 2)) ])
      ();
    creature ~key:"Goliath" ~attack:(attack 6) ~life:20 ~description:"troopers.goliath.description"
      ~new_reaction:goliath_reaction ();
    creature ~key:"SiegeTank" ~attack:(attack 8) ~life:29 ~description:"troopers.siegetank.description"
      ~effects:(effects [ (On_turn, focus siege) ])
      ();
    spell ~key:"NuclearMissile" ~description:"troopers.nuclearmissile.description"
      ~effects:(effects [ (Direct, damage_creatures ~is_spell:true 19) ])
      ();
    creature ~key:"ScienceVessel" ~attack:(attack 6) ~life:60
      ~description:"troopers.sciencevessel.description"
      ~effects:(effects [ (Direct, damage_creatures ~is_ability:true 12) ])
      ();
  ]

let house = special_house ~key:"Troopers" ~id:5 cards
