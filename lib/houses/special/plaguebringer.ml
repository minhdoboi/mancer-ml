(* Special house Plaguebringer, by LordVanmaru.  The Disease mechanic.

   A disease counter is not a status on the creature: it lives in the
   infecting player's data, keyed by the victim's [slot_id] (like the
   Shadowcaster's Dark power, but a map).  That way the count follows the
   creature across moves, survives simulations, and belongs to the player who
   applied it.

   The passive: every time a Plaguebringer's creature or spell damages an
   enemy creature, that creature gains one counter.  Zero damage adds none.
   The counter is added before the blow's death is flushed, so the consumers
   below (Soul Leech, Broodmother) read the fatal blow's counter too. *)

open Sp
open Engine
open Common

let house_id = 51

(* ---------------------------------------------------------------- state *)

let counters_of = function Plague_data xs -> xs | _ -> []
let counters (p : player_state) = counters_of p.ps_data
let get_counters (p : player_update) = counters p.pstate

let set_counters (p : player_update) xs = Player.set_data p (Plague_data xs)

let counter_of (p : player_update) id =
  match List.assoc_opt id (get_counters p) with Some n -> n | None -> 0

let set_counter (p : player_update) id n =
  let rest = List.filter (fun (i, _) -> i <> id) (get_counters p) in
  set_counters p (if n <= 0 then rest else (id, n) :: rest)

let add_counter (p : player_update) id n = set_counter p id (counter_of p id + n)
let clear_counter (p : player_update) id = set_counter p id 0

let counter_of_state (p : player_state) id =
  match List.assoc_opt id (counters p) with Some n -> n | None -> 0

(* The disease counters shown on a board slot: a creature's count is its
   opponent's entry, so every player's map is scanned for the slot_id. *)
let counters_on (state : game_state) id =
  Array.fold_left (fun acc p -> acc + counter_of_state p id) 0 state.players

(* The enemy creature slot_ids still on the board. *)
let enemy_ids (p : player_update) =
  Array.to_list (other_player p).slots_update.slot_updates
  |> List.filter_map (fun s -> match s.value with Some st -> Some st.slot_id | None -> None)

(* ------------------------------------------------------------ infection *)

(* The passive.  [st] is the state captured before the blow, so its slot_id is
   the victim's even when the blow just killed it.  Spells mark whatever their
   house (a basic Reinforce/Chain Lightning still spreads the plague), but a
   creature blow only marks when the creature is one of ours — the basic
   houses' beasts and brawlers carry no disease. *)
let infect_on_damage (p : player_update) st d =
  let creature_borne = match d.context.ctx_card with Some c -> c.house_id = house_id | None -> false in
  if d.amount > 0 && d.context.ctx_player = p.pid && (d.is_spell || creature_borne) then
    add_counter p st.slot_id 1

let install_infection (p : player_update) =
  let wrap s =
    let old = s.on_damage_hook in
    s.on_damage_hook <-
      (fun st d ->
        old st d;
        infect_on_damage p st d)
  in
  Array.iter wrap (other_player p).slots_update.slot_updates

(* Infect: when the victim dies, its counters are split as evenly as possible
   among the surviving enemy creatures, the remainder going to a random pick. *)
let spread_counters (p : player_update) id =
  let total = counter_of p id in
  clear_counter p id;
  if total > 0 then (
    let ids = enemy_ids p in
    match ids with
    | [] -> ()
    | _ ->
        let n = List.length ids in
        let base = total / n and rem = total mod n in
        List.iter (fun i -> add_counter p i base) ids;
        let shuffled = Dice.shuffle p.updater.rng ids in
        List.iteri (fun i id -> if i < rem then add_counter p id 1) shuffled)

(* --------------------------------------------------------------- the cards *)

let infect_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = opp_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some st ->
      let id = st.slot_id in
      let before = st.life in
      (* the passive adds the first counter; Infect adds the second.  A fatal
         hit leaves the slot empty already, and its counters are spread by the
         death hook (after the consumers have read them). *)
      Slot.inflict slot (spell_damage env 6);
      let dealt = match slot.value with Some now -> before - now.life | None -> before in
      if dealt > 0 then add_counter p id 1

(* Locust Carrier: summoned into a blocked lane, it strikes every enemy
   creature at once for its attack. *)
let locust_carrier_effect : card_effect =
 fun env ->
  let me = own_slot_of_env env in
  match me.value with
  | None -> ()
  | Some st ->
      if Option.is_some (Slot.opposite me).value then
        Slots.inflict_creatures (other_of_env env).slots_update (ability_damage env st.attack)

(* Infest: stun the target, then either set its counters to 3 (while dealing
   12) or consume them and burn for 5 each. *)
let infest_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = opp_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some st ->
      Slot.stun slot;
      let id = st.slot_id in
      let n = counter_of p id in
      if n <= 2 then (
        Slot.inflict slot (spell_damage env 12);
        set_counter p id 3)
      else (
        set_counter p id 0;
        Slot.inflict slot (spell_damage env (5 * n)))

(* Flying Injector: a plague cloud that chips every enemy creature each turn
   and then burns the ones that have built up a full infection. *)
let injector_turn : card_effect =
 fun env ->
  Slots.inflict_creatures (other_of_env env).slots_update (ability_damage env 1)

let injector_end : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  List.iter
    (fun id ->
      let n = counter_of p id in
      if n >= 4 then (
        set_counter p id 0;
        match Slots.find_slot_id op.slots_update id with
        | Some s -> Slot.inflict s (ability_damage env (4 * n))
        | None -> ()))
    (enemy_ids p)

(* Desecrator: on arrival it drains every enemy creature's infection and
   burns each for 5 a counter. *)
let desecrator_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  List.iter
    (fun id ->
      let n = counter_of p id in
      if n > 0 then (
        set_counter p id 0;
        match Slots.find_slot_id op.slots_update id with
        | Some s -> Slot.inflict s (ability_damage env (5 * n))
        | None -> ()))
    (enemy_ids p)

(* Titanclaw: on arrival it crushes every enemy creature carrying three or
   more counters.  No damage, so no counter is added by the sweep. *)
let titanclaw_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  let victims =
    Array.to_list op.slots_update.slot_updates
    |> List.filter (fun s ->
           match s.value with Some st -> counter_of p st.slot_id >= 3 | None -> false)
  in
  List.iter (fun s -> Slot.destroy s) victims

(* ------------------------------------------------------------- reactions *)

let soul_leech_ref : card option ref = ref None
let broodmother_ref : card option ref = ref None

(* Soul Leech: every enemy death bleeds the enemy player for the counters the
   creature carried when it died. *)
let soul_leech_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        let p = me.slots.player in
        let n = counter_of p dead.d_slot.slot_id in
        if n > 0 then
          Player.inflict (other_player p)
            (mk_damage ~is_ability:true n
               { ctx_player = p.pid; ctx_card = !soul_leech_ref; ctx_selected = me.num }));
  }

(* Broodmother: every enemy death turns its counters into Disease power (the
   Plaguebringer's special mana). *)
let broodmother_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        let p = me.slots.player in
        let n = counter_of p dead.d_slot.slot_id in
        if n > 0 then incr_mana_from me p n [ special_house_index ]);
  }

(* -------------------------------------------------------------- listener *)

(* The purge runs after the consumers (init_opp_death) have read the dead
   creature's counters.  A creature killed by Infect spreads its counters
   instead of losing them; the fatal blow's card is on the damage event. *)
let purge_on_death (p : player_update) (dead : dead) =
  let id = dead.d_slot.slot_id in
  let killed_by_infect =
    match dead.d_damage with
    | Some d -> ( match d.context.ctx_card with Some c -> c.key = "plaguebringer.infect" | None -> false)
    | None -> false
  in
  if killed_by_infect then spread_counters p id else clear_counter p id

let plaguebringer_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        install_infection p;
        (* the consumers run on the opponent's death, then the entry goes *)
        init_opp_death p;
        after_death (other_player p) (purge_on_death p));
  }

(* --------------------------------------------------------------- the deck *)

(* Ascending cost on purpose (the default cost_fn numbers the list 1..8) and
   the dealt special deck takes one card per quarter: Infect, Infest,
   Broodmother and Desecrator (indices 0, 2, 4, 6). *)

let infect =
  spell ~key:"plaguebringer.infect" ~description:"plaguebringer.infect.description"
    ~input_spec:Select_target_creature
    ~effects:(effects [ (Direct, infect_effect) ])
    ()

let locust_carrier =
  creature ~key:"plaguebringer.locustcarrier" ~attack:(attack 3) ~life:24
    ~description:"plaguebringer.locustcarrier.description"
    ~effects:(effects [ (Direct, locust_carrier_effect) ])
    ()

let infest =
  spell ~key:"plaguebringer.infest" ~description:"plaguebringer.infest.description"
    ~input_spec:Select_target_creature
    ~effects:(effects [ (Direct, infest_effect) ])
    ()

let flying_injector =
  creature ~key:"plaguebringer.flyinginjector" ~attack:(attack 4) ~life:26
    ~description:"plaguebringer.flyinginjector.description"
    ~effects:(effects [ (On_turn, injector_turn); (On_end_turn, injector_end) ])
    ()

let broodmother =
  let c =
    creature ~key:"plaguebringer.broodmother" ~attack:(attack 4) ~life:33
      ~description:"plaguebringer.broodmother.description"
      ~new_reaction:broodmother_reaction
      ()
  in
  broodmother_ref := Some c;
  c

let soul_leech =
  let c =
    creature ~key:"plaguebringer.soulleech" ~attack:(attack 4) ~life:27
      ~description:"plaguebringer.soulleech.description"
      ~new_reaction:soul_leech_reaction
      ()
  in
  soul_leech_ref := Some c;
  c

let desecrator =
  creature ~key:"plaguebringer.desecrator" ~attack:(attack 4) ~life:40
    ~description:"plaguebringer.desecrator.description"
    ~effects:(effects [ (Direct, desecrator_effect) ])
    ()

let titanclaw =
  creature ~key:"plaguebringer.titanclaw" ~attack:(attack 8) ~life:37
    ~description:"plaguebringer.titanclaw.description"
    ~effects:(effects [ (Direct, titanclaw_effect) ])
    ()

let cards =
  [
    infect;
    locust_carrier;
    infest;
    flying_injector;
    broodmother;
    soul_leech;
    desecrator;
    titanclaw;
  ]

let house = special_house ~key:"plaguebringer" ~id:house_id ~listener:plaguebringer_listener cards
