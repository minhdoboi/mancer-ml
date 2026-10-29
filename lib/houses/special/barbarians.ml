(* Special house Barbarians (DonnieDarkcore). The Barbarians grow stronger as
their own life falls: every card reads the owner's life and opens a second
face below a threshold.  The thresholds in play are 8 (Axe Warrior, Ogre
Mage), 10 (Cerberus), 13 (Goliath), 15 (Steaming Bull) and 20 (Behemoth,
Eldritch Giant), so a bleeding player turns a modest deck into a wall of
finishers.

Dwarven Hammer is the house's one spell: it both hurts a creature and locks
the opponent out of their spells for their next turn (the common spell-hiding
"desc mod" the game folds over every card the opponent could play, dropped by
an end-of-turn effect on the opponent). *)

open Sp
open Engine
open Common

let house_id = 49

(* ---- thresholds ---- *)

(* All of these read the owner's life, so an attack that raises the attack while
the player is low uses Attack_state_func, exactly as the Emotion house's
Courage does. *)
let axe_attack = Attack_state_func (fun atk p -> if p.pstate.ps_life <= 8 then atk + 5 else atk)
let goliath_attack = Attack_state_func (fun atk p -> if p.pstate.ps_life <= 13 then atk + 4 else atk)

(* ---- Dwarven Hammer ---- *)

(* The lock: [hide_spell_mod] rides on the opponent for one of their turns and
   is dropped by that turn's end effect.  An aura is not a spell, so it stays
   castable just as [Assets] spells it out. *)
let dwarven_hammer : card_effect =
 fun env ->
  let op = other_of_env env in
  let target = op.slots_update.slot_updates.(env.env_selected) in
  if Option.is_some target.value then Slot.inflict target (ability_damage env 8);
  Player.add_desc_mod op hide_spell_mod;
  Player.add_effect op
    (On_end_turn, one_time_player_effect (fun e -> Player.remove_desc_mod (player_of_env e) hide_spell_mod))

(* ---- Goliath ---- *)

(* Double damage from an opponent creature or spell of level 4 or less.
   [self_protect] is the hook the engine runs on the victim before the life is
   cut, and the damage's context names the source card, so a hit from a
   level 5+ card (or from the creature itself) passes untouched. *)
let goliath_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        if d.context.ctx_player <> me.slots.player.pid && not d.redirected then
          match d.context.ctx_card with Some c when c.cost <= 4 -> { d with amount = d.amount * 2 } | _ -> d
        else d);
  }

(* ---- Cerberus ---- *)

(* Below 10 life the hound stops choosing: the attack sweeps every opponent
   creature (the Limbo wraith's Multi_target, minus the player). *)
let cerberus_attack target d p =
  if p.pstate.ps_life <= 10 then Slots.inflict_creatures (other_player p).slots_update d
  else ignore (single_target_attack target d p)

(* ---- Steaming Bull ---- *)

(* Below 15 life the bull runs twice.  The count is read from the owner's life
   at the moment of the attack, not baked in at summon time. *)
let bull_attack blows target d p =
  ignore (single_target_attack target d p);
  blows = 0 && p.pstate.ps_life <= 15

(* ---- Ogre Mage ---- *)

(* On every own turn while life is 8 or less: heal 8 and take a permanent +1
   attack.  A fresh closure joins the sources each turn, so the bonus really
   is permanent and keeps growing. *)
let ogre_tick : card_effect =
 fun env ->
  let p = player_of_env env in
  if p.pstate.ps_life <= 8 then (
    let me = own_slot_of_env env in
    Slot.heal me 8;
    add_attack me (Attack_func (fun a -> a + 1)))

(* ---- Behemoth ---- *)

(* Below 20 life the giant ignores the creature in front of it and swings at
   the player. *)
let behemoth_attack target d p =
  if p.pstate.ps_life <= 20 then Player.inflict (other_player p) d
  else ignore (single_target_attack target d p)

(* ---- Eldritch Giant ---- *)

(* Below 20 life, a random opponent creature of level 4 or more is destroyed
   at the owner's end of turn. *)
let eldritch_curse : card_effect =
 fun env ->
  let p = player_of_env env in
  if p.pstate.ps_life <= 20 then
    let op = other_player p in
    let big =
      Slots.foldl op.slots_update [] (fun acc s ->
          if (Slot.get s).card.c_card.cost >= 4 then s :: acc else acc)
    in
    match Dice.shuffle p.updater.rng big with victim :: _ -> Slot.destroy victim | [] -> ()

(* ---- the cards ---- *)

let axe_warrior =
  creature ~key:"barbarians.axewarrior"
    ~attack:{ base = Some 3; sources = [ axe_attack ] }
    ~life:16 ~description:"barbarians.axewarrior.description" ()

let dwarven_hammer =
  spell ~key:"barbarians.dwarvenhammer" ~description:"barbarians.dwarvenhammer.description"
    ~input_spec:Select_target_creature
    ~effects:(effects [ (Direct, dwarven_hammer) ])
    ()

let goliath =
  creature ~key:"barbarians.goliath"
    ~attack:{ base = Some 5; sources = [ goliath_attack ] }
    ~life:26 ~description:"barbarians.goliath.description" ~new_reaction:goliath_reaction ()

let cerberus =
  creature ~key:"barbarians.cerberus" ~attack:(attack 5) ~life:32
    ~description:"barbarians.cerberus.description" ~run_attack:(custom_run_attack cerberus_attack) ()

let ogre_mage =
  creature ~key:"barbarians.ogremage" ~attack:(attack 5) ~life:28
    ~description:"barbarians.ogremage.description"
    ~effects:(effects [ (On_turn, ogre_tick) ])
    ()

let steaming_bull =
  creature ~key:"barbarians.steamingbull" ~attack:(attack 6) ~life:30
    ~description:"barbarians.steamingbull.description"
    ~run_attack:(custom_strikes_run_attack bull_attack)
    ()

let eldritch_giant =
  creature ~key:"barbarians.eldritchgiant" ~attack:(attack 6) ~life:36
    ~description:"barbarians.eldritchgiant.description"
    ~effects:(effects [ (On_end_turn, eldritch_curse) ])
    ()

let behemoth =
  creature ~key:"barbarians.behemoth" ~attack:(attack 7) ~life:35
    ~description:"barbarians.behemoth.description" ~run_attack:(custom_run_attack behemoth_attack) ()

let cards =
  [ axe_warrior; dwarven_hammer; goliath; cerberus; steaming_bull; ogre_mage; behemoth; eldritch_giant ]

(* Deck costs, by card order: the list is ascending so the deck builder and the
   card list keep their order. *)
let cost_fn i = i + 1
let house = special_house ~key:"barbarians" ~id:house_id cards
