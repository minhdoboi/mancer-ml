(* Special house Technomage, by Sinist.  A defensive engineer: the Workpiece
   has no attack of its own and turns, the moment it is summoned, into the
   tower of the owner's highest element.  The towers then hold the board with
   walls and counter-fire while the opponent throws cards at them.

   The eight printed cards are (the Workpiece appears twice, so the opening
   quarter of the deck always deals a body and never a redundant turret):

     Workpiece    (2)  - 0|18 body that always turns into a tower on summon.
     Workpiece    (2)  - a second copy, for the deck's opening quarter.
     Wall of Daggers (3)  - 2|22, bites back at creatures and the opponent.
     Mass Repair  (4)  - heals the owner's creatures to full, +1 to the four
                         elemental powers.
     Wall of Distortion (5) - 0|30, shares the damage it takes with the whole
                         enemy board.
     Rocket Battery (6) - 9|36, loses 1 attack a turn and strafes whatever
                         strikes a neighbour.
     Fortification (7) - turns every elemental creature into the tower of its
                         own element and refunds one special mana each.
     Tower of Doom (8) - 0|60, grows on every death and spell, then empties its
                         charge into the opponent at the end of the turn.

   The four towers themselves (Raging Flame, Ice Shell, Eternal Storm, Green
   Forest) are additional cards, never dealt. *)

open Sp
open Engine
open Common

let house_id = 51

(* Self-referential cards: the refs are filled after the card list below. *)
let workpiece_ref : card option ref = ref None
let flame_ref : card option ref = ref None
let ice_ref : card option ref = ref None
let storm_ref : card option ref = ref None
let forest_ref : card option ref = ref None
let battery_ref : card option ref = ref None
let dagger_ref : card option ref = ref None
let distortion_ref : card option ref = ref None
let doom_ref : card option ref = ref None

(* Index 0..3 of the tower a given basic element becomes. *)
let tower_refs : card option array = Array.make 4 None

let tower_key = function
  | 0 -> "technomage.flame"
  | 1 -> "technomage.ice"
  | 2 -> "technomage.storm"
  | _ -> "technomage.forest"

(* The basic element with the most mana right now; ties go to the lowest. *)
let element_index (p : player_update) =
  let v = p.houses_update.hu_value in
  let best = ref 0 in
  for i = 1 to 3 do
    if v.(i).mana > v.(!best).mana then best := i
  done;
  !best

let tower_of_player p = match tower_refs.(element_index p) with Some c -> c | None -> failwith "technomage tower unset"

(* -- small helpers over the slot graph --------------------------------- *)

let count_key (p : player_update) key =
  Slots.foldl p.slots_update 0 (fun n s ->
      match s.value with Some st when st.card.c_card.key = key -> n + 1 | _ -> n)

let keyed_towers (p : player_update) key =
  Slots.foldl p.slots_update [] (fun acc s ->
      match s.value with Some st when st.card.c_card.key = key -> s :: acc | _ -> acc)

(* The damage a counter-firing tower attributes to itself. *)
let tower_damage (me : slot_update) amount =
  Player.inflict (other_player me.slots.player)
    (mk_damage ~is_ability:true amount
       {
         ctx_player = me.slots.player.pid;
         ctx_card = (match me.value with Some st -> Some st.card.c_card | None -> None);
         ctx_selected = me.num;
       })

(* Force an attack source onto a 0-attack tower (Common.add_attack refuses to
   grow a base of zero) and re-fold the cached value. *)
let grow_attack (slot : slot_update) amount =
  Slot.update slot (fun st ->
      let src = Attack_func (fun a -> max 0 (a + amount)) in
      let st =
        { st with attack_sources = { st.attack_sources with sources = st.attack_sources.sources @ [ src ] } }
      in
      { st with attack = Slots.current_attack slot st })

let zero_attack (slot : slot_update) =
  Slot.update slot (fun st -> { st with attack_sources = { base = Some 0; sources = [] }; attack = 0 })

(* The wall's attack even on the killing blow, when [me.value] is already
   empty but the committed [pstate] still holds the body. *)
let wall_attack (me : slot_update) =
  match me.value with
  | Some st -> st.attack
  | None -> ( match me.slots.player.pstate.ps_slots.(me.num) with Some st -> st.attack | None -> 0)

let lowest_attack a b = if (Slot.get b).attack < (Slot.get a).attack then b else a

(* ---------------------------------------------------------------- reactions *)

(* Workpiece: the passive.  Whatever summoned it, the body is swapped for the
   tower of the owner's highest element the moment it lands. *)
let workpiece_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num && Option.is_some me.value then
          Slots.add_creature me (make_creature (tower_of_player me.slots.player)));
  }

(* Ice Shell: every hit that lands on the body is cut by 3.  The owner's share
   is handled by the listener's guard hook. *)
let ice_reaction () =
  let r = Reaction.default () in
  { r with self_protect = (fun d -> { d with amount = max 0 (d.amount - 3) }) }

(* Green Forest: the wound heals back either the owner or the neighbouring
   bodies, whichever side holds less life. *)
let forest_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun d ->
        if d.amount > 0 then (
          let me = Reaction.bound_slot r in
          let p = me.slots.player in
          let neighbours = Slot.filled_adjacent me in
          let neighbour_life = List.fold_left (fun acc s -> acc + (Slot.get s).life) 0 neighbours in
          if neighbours = [] || p.pstate.ps_life < neighbour_life then Player.heal p d.amount
          else List.iter (fun s -> Slot.heal s d.amount) neighbours));
  }

(* Wall of Daggers: on any wound, 2 to every enemy creature and its own attack
   to the opponent.  Ability damage counts like any other: a summoned Boar or
   an enemy spell should make it bite.  The [busy] flag is what keeps a
   mirrored wall (or a reflected blow) from answering itself forever. *)
let dagger_reaction () =
  let r = Reaction.default () in
  let busy = ref false in
  {
    r with
    on_my_damage =
      (fun d ->
        if d.amount > 0 then
          once busy (fun () ->
              let me = Reaction.bound_slot r in
              let p = me.slots.player in
              let ctx = { ctx_player = p.pid; ctx_card = !dagger_ref; ctx_selected = me.num } in
              emit p.updater
                (Ev_ability { kind = "technomage.daggers"; player = p.pid; num = me.num; target = None });
              Slots.inflict_creatures (other_player p).slots_update (mk_damage ~is_ability:true 2 ctx);
              Player.inflict (other_player p) (mk_damage ~is_ability:true (wall_attack me) ctx)));
  }

(* Wall of Distortion: the wound is copied onto every enemy creature.  Same
   rule as the Wall of Daggers: an ability wounds it too, and the [busy] flag
   cuts the echo after one exchange. *)
let distortion_reaction () =
  let r = Reaction.default () in
  let busy = ref false in
  {
    r with
    on_my_damage =
      (fun d ->
        if d.amount > 0 then
          once busy (fun () ->
              let me = Reaction.bound_slot r in
              let p = me.slots.player in
              let ctx = { ctx_player = p.pid; ctx_card = !distortion_ref; ctx_selected = me.num } in
              emit p.updater
                (Ev_ability
                   { kind = "technomage.distortion"; player = p.pid; num = me.num; target = None });
              Slots.inflict_creatures (other_player p).slots_update (mk_damage ~is_ability:true d.amount ctx)));
  }

(* Tower of Doom: every death on the board is a charge. *)
let doom_reaction () =
  let r = Reaction.default () in
  { r with on_death = (fun _ -> grow_attack (Reaction.bound_slot r) 2) }

(* ------------------------------------------------------------------ effects *)

(* Tower of Doom: it arrives with a full workshop — every empty slot gets a
   workpiece, which immediately becomes a tower. *)
let fill_workpieces : card_effect =
 fun env ->
  let p = player_of_env env in
  match !workpiece_ref with
  | None -> ()
  | Some w ->
      List.iter (fun num -> ignore (Slots.summon p.slots_update num (make_creature w))) (Slots.get_open p.slots_update)

(* Raging Flame: at the owner's end of turn, 5 to the enemy creature with the
   lowest attack. *)
let flame_volley : card_effect =
 fun env ->
  match reduce_filled (other_of_env env).slots_update lowest_attack with
  | Some s ->
      emit env.env_updater
        (Ev_ability
           {
             kind = "technomage.flame";
             player = env.env_player;
             num = env.env_selected;
             target = Some (s.slots.player.pid, s.num);
           });
      Slot.inflict s (ability_damage env 5)
  | None -> ()

(* Rocket Battery: one attack off every turn. *)
let battery_aging : card_effect = fun env -> grow_attack (own_slot_of_env env) (-1)

(* Tower of Doom: with 8 or more attack banked, cash the whole charge in for
   triple into the opponent, then start over. *)
let doom_volley : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with
  | Some st when st.attack >= 8 ->
      Player.inflict (other_of_env env) (mk_damage ~is_ability:true (3 * st.attack) (ctx_of_env env));
      zero_attack slot
  | _ -> ()

(* Mass Repair: the owner's board back to full, and one more growth in each
   elemental power. *)
let repair_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  Slots.foreach p.slots_update (fun s -> Slot.update s (fun st -> { st with life = st.max_life }));
  Houses.incr_growth p.houses_update 1 [ 0; 1; 2; 3 ]

(* Fortification: every elemental creature becomes the tower of its element,
   refunding one special mana per body. *)
let fortification : card_effect =
 fun env ->
  let p = player_of_env env in
  let n = ref 0 in
  Array.iter
    (fun (slot : slot_update) ->
      match slot.value with
      | Some st when st.card.c_card.house_index < special_house_index -> (
          match tower_refs.(st.card.c_card.house_index) with
          | Some tower ->
              incr n;
              Slots.add_creature slot (make_creature tower)
          | None -> ())
      | _ -> ())
    p.slots_update.slot_updates;
  if !n > 0 then Houses.incr_mana p.houses_update !n [ special_house_index ]
(* ---------------------------------------------------------------- listener *)

let battery_retaliate (slot : slot_update) =
  let opp = Slot.opposite slot in
  if Option.is_some opp.value then
    List.iter
      (fun (n : slot_update) ->
        match n.value with
        | Some st when is_card !battery_ref st.card.c_card && st.attack > 0 ->
            emit n.slots.player.updater
              (Ev_ability
                 {
                   kind = "technomage.battery";
                   player = n.slots.player.pid;
                   num = n.num;
                   target = Some (opp.slots.player.pid, opp.num);
                 });
            Slot.inflict opp
              (mk_damage ~is_ability:true st.attack
                 { ctx_player = slot.slots.player.pid; ctx_card = Some st.card.c_card; ctx_selected = slot.num })
        | _ -> ())
      (Slot.filled_adjacent slot)

let technomage_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let op = other_player p in
        (* Ice Shell: the owner takes 3 less from every blow for each shell. *)
        let old_guard = p.guard_hook in
        p.guard_hook <-
          (fun d ->
            let d = old_guard d in
            let n = count_key p "technomage.ice" in
            if n = 0 || d.amount = 0 then d else { d with amount = max 0 (d.amount - (3 * n)) });
        (* Rocket Battery: strafe whoever strikes a neighbouring slot. *)
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.on_damage_hook in
            slot.on_damage_hook <-
              (fun st d ->
                old st d;
                if
                  d.amount > 0 && (not d.is_ability) && (not d.is_spell) && d.context.ctx_player <> p.pid
                then battery_retaliate slot))
          p.slots_update.slot_updates;
        (* Tower of Doom hears every death on either board. *)
        init_any_death p;
        (* Eternal Storm and Tower of Doom answer every card the opponent
           plays: the storm charges and fires for 2, the doom just charges. *)
        let old_sub = op.submit_command_hook in
        op.submit_command_hook <-
          (fun c ->
            List.iter
              (fun tower ->
                grow_attack tower 1;
                tower_damage tower 2)
              (keyed_towers p "technomage.storm");
            List.iter (fun tower -> grow_attack tower 2) (keyed_towers p "technomage.doom");
            old_sub c));
  }

(* --------------------------------------------------------------- the cards *)

(* The four towers are the material the passive summons; every one is a 0
   attack, runnable wall so it can hold a lane and still strike once it has
   been fed attack. *)
let flame =
  creature ~key:"technomage.flame" ~attack:(attack 0) ~life:15
    ~description:"technomage.flame.description" ~status:run_flag
    ~effects:(effects [ (On_end_turn, flame_volley) ]) ()

let ice =
  creature ~key:"technomage.ice" ~attack:(attack 0) ~life:20
    ~description:"technomage.ice.description" ~status:run_flag ~new_reaction:ice_reaction ()

let storm =
  creature ~key:"technomage.storm" ~attack:(attack 0) ~life:17
    ~description:"technomage.storm.description" ~status:run_flag ()

let forest =
  creature ~key:"technomage.forest" ~attack:(attack 0) ~life:19
    ~description:"technomage.forest.description" ~status:run_flag ~new_reaction:forest_reaction ()

(* The body the doom puts down.  0|18 and always transforms.  It is printed
   twice: the two copies fill the deck's opening quarter, so the class never
   opens on a card that only does what this one already does. *)
let workpiece =
  creature ~key:"technomage.workpiece" ~attack:(attack 0) ~life:18
    ~description:"technomage.workpiece.description" ~new_reaction:workpiece_reaction ()

let workpiece2 =
  creature ~key:"technomage.workpiece" ~attack:(attack 0) ~life:18
    ~description:"technomage.workpiece.description" ~new_reaction:workpiece_reaction ()

(* The Wall of Daggers and the Rocket Battery carry base attack, so they are
   deliberately NOT runnable: without the flag they wait out the turn they
   land and strike from the next one.  Only the four 0-attack towers carry
   [run_flag], so a tower that has been fed attack can still fire the turn it
   is built. *)
let daggers =
  creature ~key:"technomage.daggers" ~attack:(attack 2) ~life:22
    ~description:"technomage.daggers.description" ~new_reaction:dagger_reaction ()

let repair =
  spell ~key:"technomage.repair" ~description:"technomage.repair.description"
    ~effects:(effects [ (Direct, repair_effect) ]) ()

let distortion =
  creature ~key:"technomage.distortion" ~attack:(attack 0) ~life:30
    ~description:"technomage.distortion.description" ~status:run_flag ~new_reaction:distortion_reaction ()

let battery =
  creature ~key:"technomage.battery" ~attack:(attack 9) ~life:36
    ~description:"technomage.battery.description"
    ~effects:(effects [ (On_turn, battery_aging) ]) ()

let fortify_card =
  spell ~key:"technomage.fortification" ~description:"technomage.fortification.description"
    ~effects:(effects [ (Direct, fortification) ]) ()

let doom =
  creature ~key:"technomage.doom" ~attack:(attack 0) ~life:60
    ~description:"technomage.doom.description" ~status:run_flag ~new_reaction:doom_reaction
    ~effects:(effects [ (Direct, fill_workpieces); (On_end_turn, doom_volley) ]) ()

let () =
  (* The towers are never dealt or paid for, but they are the body a Workpiece
     becomes, so they carry its price tag: cost 2. *)
  flame.cost <- 2;
  ice.cost <- 2;
  storm.cost <- 2;
  forest.cost <- 2;
  workpiece_ref := Some workpiece;
  flame_ref := Some flame;
  ice_ref := Some ice;
  storm_ref := Some storm;
  forest_ref := Some forest;
  battery_ref := Some battery;
  dagger_ref := Some daggers;
  distortion_ref := Some distortion;
  doom_ref := Some doom;
  tower_refs.(0) <- Some flame;
  tower_refs.(1) <- Some ice;
  tower_refs.(2) <- Some storm;
  tower_refs.(3) <- Some forest

(* Printed cost order.  The class has no one-mana card: the two Workpieces
   cost two. *)
let cards = [ workpiece; workpiece2; daggers; repair; distortion; battery; fortify_card; doom ]

(* Workpiece 2, Workpiece 2, Wall of Daggers 3, Mass Repair 4, Wall of
   Distortion 5, Rocket Battery 6, Fortification 7, Tower of Doom 8.  The
   dealt special deck takes one card of each quarter: Workpiece, Wall of
   Daggers, Wall of Distortion, Fortification. *)
let cost_fn i = if i = 0 then 2 else i + 1

let house =
  special_house ~key:"technomage" ~id:house_id
    ~additional_cards:[ flame; ice; storm; forest ]
    ~listener:technomage_listener cards

(* Keep the leftover refs referenced so unused warnings stay quiet. *)
let () =
  ignore !flame_ref;
  ignore !storm_ref;
  ignore !forest_ref;
  ignore !dagger_ref;
  ignore !distortion_ref;
  ignore !doom_ref;
  ignore tower_key
