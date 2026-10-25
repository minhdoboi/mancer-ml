(* Special house Blacksmith — a defensive class built around persistent
   "wielder" spells.  Every Blacksmith spell must be cast on one of the
   caster's creatures of cost 8 or less; that creature is the wielder, and a
   small mark icon is shown on its card. *)

open Sp
open Engine
open Common

let house_id = 46

(* Mark keys shared with the GUI icon table. *)
let mark_spear = "spear"
let mark_bow = "bow"
let mark_shield = "shield"
let mark_hat = "hat"
let mark_chariot = "chariot"
let mark_cloak_dagger = "cloakdagger"
let mark_excalibur = "excalibur"
let mark_gauntlet = "gauntlet"
let mark_astral_cloak = "astral-cloak"
let mark_astral_dagger = "astral-dagger"

let pegasus_ref : card option ref = ref None
let astral_cloak_ref : card option ref = ref None
let astral_dagger_ref : card option ref = ref None
let max_wielder_cost = 8

(* --------------------------------------------------------------- marks *)

let has_mark key (st : slot_state) = List.exists (fun m -> m.bm_key = key) st.marks

let mark_used key (st : slot_state) =
  List.exists (fun m -> m.bm_key = key && m.bm_used) st.marks

let slot_has_mark slot key =
  match slot.value with Some st -> has_mark key st | None -> false

let put_mark slot ?(used = false) ?(target = 0) key =
  Slot.update slot (fun st ->
      let rest = List.filter (fun m -> m.bm_key <> key) st.marks in
      { st with marks = { bm_key = key; bm_used = used; bm_target = target } :: rest })

let use_mark slot key =
  Slot.update slot (fun st ->
      {
        st with
        marks = List.map (fun m -> if m.bm_key = key then { m with bm_used = true } else m) st.marks;
      })

let count_mark key (p : player_update) =
  Array.fold_left
    (fun n s -> match s.value with Some st when has_mark key st -> n + 1 | _ -> n)
    0 p.slots_update.slot_updates

(* The caster's creatures that may carry a Blacksmith spell.  Using
   [Select_owner] makes [Game.available_commands] offer only legal wielders. *)
let wielders pid (state : game_state) =
  List.init nb_slots Fun.id
  |> List.filter (fun n ->
      match state.players.(pid).ps_slots.(n) with
      | Some st -> st.card.c_card.cost <= max_wielder_cost
      | None -> false)

let wielder_input = Select_owner wielders

(* --------------------------------------------------------- card helpers *)

let set_run_attack slot ra =
  Slot.update slot (fun st ->
      let body = { st.card.c_body with b_run_attack = ra } in
      let card = { st.card.c_card with creature_body = Some body } in
      { st with card = { c_card = card; c_body = body } })

let add_on_turn slot f =
  Slot.update slot (fun st ->
      let cards = st.card.c_card in
      let old = cards.effects in
      let effect =
        match old.on_turn with None -> f | Some g -> fun env -> g env; f env
      in
      { st with card = { st.card with c_card = { cards with effects = { old with on_turn = Some effect } } } })

let apply_original ra target d (p : player_update) =
  match ra.apply_ with
  | Single_target -> ignore (single_target_attack target d p)
  | Multi_target ->
      Player.inflict (other_player p) d;
      Slots.inflict_creatures (other_player p).slots_update d
  | Custom f -> f target d p
  | Custom_strikes strike -> run_all_strikes strike target d p

(* --------------------------------------------------------------- spells *)

let mithril_spear : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some st ->
      put_mark slot mark_spear;
      if st.card.c_card.key = "earth.sprite" then
        add_on_turn slot (fun env ->
            let me = own_slot_of_env env in
            match (Slot.opposite me).value with
            | Some _ -> Slot.inflict (Slot.opposite me) (ability_damage env 2)
            | None -> ())
      else add_attack slot (Attack_func (fun a -> a + 2))

(* The bow's one stun.  Wrapping the wielder's own run attack keeps multi-hit
   wielders (Hydra, Forest Sprite) multi-hit. *)
let bow_run_attack (orig : run_attack) : run_attack =
  let stun d (player : player_update) first_hit =
    let op = other_player player in
    let attacker = player.slots_update.slot_updates.(d.context.ctx_selected) in
    match attacker.value with
    | Some st when has_mark mark_bow st && not (mark_used mark_bow st) -> (
        match first_hit with
        | Some n ->
            let hit = op.slots_update.slot_updates.(n) in
            if Option.is_some hit.value then Slot.stun hit;
            use_mark attacker mark_bow
        | None -> ())
    | _ -> ()
  in
  let first_hit target (player : player_update) =
    let op = other_player player in
    List.find_opt (fun n -> Option.is_some op.slots_update.slot_updates.(n).value) target
  in
  match orig.apply_ with
  | Custom_strikes strike ->
      (* a wielder that strikes twice keeps both blows *)
      let apply_ blows target d player =
        let hit = first_hit target player in
        let again = strike blows target d player in
        stun d player hit;
        again
      in
      { orig with apply_ = Custom_strikes apply_ }
  | _ ->
      let apply_ target d player =
        let hit = first_hit target player in
        apply_original orig target d player;
        stun d player hit
      in
      { orig with apply_ = Custom apply_ }

let mithril_bow : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some st ->
      put_mark slot mark_bow;
      Slot.toggle_run slot;
      if not (has_mark mark_bow st) then
        set_run_attack slot (bow_run_attack st.card.c_body.b_run_attack)

let mithril_shield : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some _ ->
      put_mark slot mark_shield;
      Slot.update slot (fun st -> { st with life = st.life + 10; max_life = st.max_life + 10 })

let wizard_hat : card_effect =
 fun env -> match (own_slot_of_env env).value with None -> () | Some _ -> put_mark (own_slot_of_env env) mark_hat

(* -------------------------------------------------------- flying chariot *)

(* The Pegasus triggers at the end of every turn, so the same effect is put on
   both players.  It looks the Pegasus up by slot id: once it dies the effect
   simply does nothing, and a later Pegasus gets its own id. *)
let pegasus_tick owner_pid slot_id env =
  let owner = env.env_updater.player_updates.(owner_pid) in
  match Slots.find_slot_id owner.slots_update slot_id with
  | None -> ()
  | Some slot -> (
      match slot.value with
      | Some { data = Ridden hidden; _ } ->
          let op = other_player owner in
          let amount = hidden.attack + 4 in
          let ctx = { ctx_player = owner_pid; ctx_card = !pegasus_ref; ctx_selected = slot.num } in
          let d = mk_damage ~is_ability:true amount ctx in
          let target = Slot.opposite slot in
          Updater.focus env.env_updater slot.num owner_pid true;
          (match target.value with
          | Some _ -> Slot.inflict target d
          (* No opposing creature: hit the opponent directly, never another
             slot. *)
          | None -> Player.inflict op d)
      | _ -> ())

let pegasus_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        match dead.d_slot.data with
        | Ridden hidden ->
            let slot = dead.d_player.slots_update.slot_updates.(dead.d_num) in
            if Option.is_none slot.value then (
              (* Keep the wielder exactly as it was: its marks, buffs and
                 slot data all survived under the Pegasus.  Only rebind its
                 reaction to the live update graph. *)
              let reaction = hidden.card.c_body.b_new_reaction () in
              reaction.selected := Some slot;
              let st = { hidden with target = [ slot.num ]; reaction } in
              (* Putting it back on the slot runs every neighbour's on_add, so an
                 Orc chieftain next door or a Bull Commander anywhere hands the
                 bonus over again; fold once more so the wielder's attack reads
                 right away instead of at the next refresh. *)
              Slot.overwrite_add slot st;
              Slot.update slot (fun s -> { s with attack = Slots.current_attack slot s }))
        | _ -> ());
  }

let flying_chariot : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some _ -> (
      match !pegasus_ref with
      | None -> ()
      | Some pegasus ->
          let hidden = slot.remove_hook None in
          (* Leaving the slot made every neighbour take its bonus back (an Orc
             chieftain next door, a Bull Commander anywhere): [Slot.remove]
             hands back the state with those sources gone, but its cached
             [attack] still counts them.  Fold once more, or the Pegasus would
             strike for X+4 with a bonus the creature no longer has. *)
          let hidden = { hidden with attack = Slots.current_attack slot hidden } in
          let owner = player_of_env env in
          ignore (Slots.summon owner.slots_update env.env_selected (make_creature pegasus));
          if Option.is_some slot.value then (
            Slot.set_data slot (Ridden hidden);
            put_mark slot mark_chariot;
            match slot.value with
            | Some st ->
                let tick env = pegasus_tick owner.pid st.slot_id env in
                Player.add_effect owner (On_end_turn, tick);
                Player.add_effect (other_player owner) (On_end_turn, tick)
            | None -> ()))

(* ------------------------------------------------------ cloak and dagger *)

let astral_cloak_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        incr_mana_from me me.slots.player 1 [ 1; 3 ]);
  }

let astral_dagger_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        incr_mana_from me me.slots.player 1 [ 0; 2 ]);
  }

let rec nearest_empty_side (p : player_update) selected step =
  let n = selected + step in
  if not (in_slot_range n) then None
  else if List.mem n p.pstate.ps_slot_list && Option.is_none p.slots_update.slot_updates.(n).value then Some n
  else nearest_empty_side p n step

let spawn_astral (p : player_update) (card : card) selected side_key wielder_id step =
  match nearest_empty_side p selected step with
  | None -> ()
  | Some n ->
      ignore (Slots.summon p.slots_update n (make_creature card));
      let slot = p.slots_update.slot_updates.(n) in
      if Option.is_some slot.value then put_mark slot ~target:wielder_id side_key

let cloak_and_dagger : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = own_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some st ->
      let wielder_id = st.slot_id in
      put_mark slot ~target:wielder_id mark_cloak_dagger;
      (match !astral_cloak_ref with
      | Some c -> spawn_astral p c env.env_selected mark_astral_cloak wielder_id (-1)
      | None -> ());
      match !astral_dagger_ref with
      | Some c -> spawn_astral p c env.env_selected mark_astral_dagger wielder_id 1
      | None -> ()

(* ------------------------------------------------------------ excalibur *)

let excalibur : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some _ ->
      let opp = Slot.opposite slot in
      (match opp.value with
      | Some _ -> Slot.destroy opp
      | None -> Player.inflict (other_of_env env) (spell_damage env 15));
      put_mark slot mark_excalibur;
      add_attack slot (Attack_func (fun a -> a / 2));
      set_run_attack slot multi_run_attack

(* --------------------------------------------------- bargulian gauntlet *)

let gauntlet_run_attack (orig : run_attack) : run_attack =
  let apply_ target d (player : player_update) =
    let d = { d with mortal = true } in
    apply_original orig target d player
  in
  { orig with apply_ = Custom apply_ }

let bargulian_gauntlet : card_effect =
 fun env ->
  let slot = own_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some st ->
      let amount = st.attack + 8 in
      let ctx = ctx_of_env env in
      let op = other_of_env env in
      put_mark slot mark_gauntlet;
      set_run_attack slot (gauntlet_run_attack st.card.c_body.b_run_attack);
      Slots.foreach op.slots_update (fun s ->
          Slot.inflict s (mk_damage ~is_ability:true ~mortal:true amount ctx))

(* -------------------------------------------------------------- listener *)

(* Cloak/Dagger are global auras, so they live in the protect/guard hooks and
   are recomputed from the live marks each hit.  This way a dead token stops
   working without having to unwrap a stack of closures. *)
let blacksmith_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let old_guard = p.guard_hook in
        p.guard_hook <-
          (fun d ->
            let d = old_guard d in
            let cloak = count_mark mark_astral_cloak p in
            let dagger = count_mark mark_astral_dagger (other_player p) in
            let amount = max 0 (d.amount - cloak) + dagger in
            { d with amount });
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.protect_hook in
            slot.protect_hook <-
              (fun d ->
                let d = old d in
                let owner = slot.slots.player in
                let target_id = match slot.value with Some st -> st.slot_id | None -> 0 in
                let cloaks =
                  Array.fold_left
                    (fun n s ->
                      match s.value with
                      | Some st ->
                          if
                            has_mark mark_astral_cloak st
                            && List.exists
                                 (fun m -> m.bm_key = mark_astral_cloak && m.bm_target = target_id)
                                 st.marks
                          then n + 1
                          else n
                      | None -> n)
                    0 owner.slots_update.slot_updates
                in
                let d = if cloaks = 0 then d else { d with amount = max 0 (d.amount - cloaks) } in
                let opp = other_player owner in
                let opposite_id =
                  match (Slot.opposite slot).value with Some st -> st.slot_id | None -> 0
                in
                let daggers =
                  Array.fold_left
                    (fun n s ->
                      match s.value with
                      | Some st ->
                          if
                            has_mark mark_astral_dagger st
                            && List.exists
                                 (fun m -> m.bm_key = mark_astral_dagger && m.bm_target = opposite_id)
                                 st.marks
                          then n + 1
                          else n
                      | None -> n)
                    0 opp.slots_update.slot_updates
                in
                let d = if daggers = 0 then d else { d with amount = d.amount + daggers } in
                match slot.value with
                | Some st when has_mark mark_hat st && is_effect d -> { d with amount = 0 }
                | _ -> d))
          p.slots_update.slot_updates);
  }

(* ---------------------------------------------------------------- cards *)

let pegasus =
  creature ~key:"blacksmith.pegasus" ~attack:(attack 0) ~life:18
    ~description:"blacksmith.pegasus.description" ~new_reaction:pegasus_reaction ()

let astral_cloak =
  creature ~key:"blacksmith.astralcloak" ~attack:(attack 3) ~life:15
    ~description:"blacksmith.astralcloak.description"
    ~new_reaction:astral_cloak_reaction ()

let astral_dagger =
  creature ~key:"blacksmith.astraldagger" ~attack:(attack 4) ~life:11
    ~description:"blacksmith.astraldagger.description"
    ~new_reaction:astral_dagger_reaction ()

let () =
  pegasus.cost <- 5;
  astral_cloak.cost <- 3;
  astral_dagger.cost <- 3;
  pegasus_ref := Some pegasus;
  astral_cloak_ref := Some astral_cloak;
  astral_dagger_ref := Some astral_dagger

let cards =
  [
    spell ~key:"blacksmith.mithrilspear" ~description:"blacksmith.mithrilspear.description"
      ~input_spec:wielder_input ~effects:(effects [ (Direct, mithril_spear) ])
      ();
    spell ~key:"blacksmith.mithrilbow" ~description:"blacksmith.mithrilbow.description"
      ~input_spec:wielder_input ~effects:(effects [ (Direct, mithril_bow) ])
      ();
    spell ~key:"blacksmith.mithrilshield" ~description:"blacksmith.mithrilshield.description"
      ~input_spec:wielder_input ~effects:(effects [ (Direct, mithril_shield) ])
      ();
    spell ~key:"blacksmith.wizardshat" ~description:"blacksmith.wizardshat.description"
      ~input_spec:wielder_input ~effects:(effects [ (Direct, wizard_hat) ])
      ();
    spell ~key:"blacksmith.flyingchariot" ~description:"blacksmith.flyingchariot.description"
      ~input_spec:wielder_input ~effects:(effects [ (Direct, flying_chariot) ])
      ();
    spell ~key:"blacksmith.cloakanddagger" ~description:"blacksmith.cloakanddagger.description"
      ~input_spec:wielder_input ~effects:(effects [ (Direct, cloak_and_dagger) ])
      ();
    spell ~key:"blacksmith.excalibur" ~description:"blacksmith.excalibur.description"
      ~input_spec:wielder_input ~effects:(effects [ (Direct, excalibur) ])
      ();
    spell ~key:"blacksmith.barguliangauntlet" ~description:"blacksmith.barguliangauntlet.description"
      ~input_spec:wielder_input ~effects:(effects [ (Direct, bargulian_gauntlet) ])
      ();
  ]

let house =
  special_house ~key:"blacksmith" ~id:house_id ~listener:blacksmith_listener
    ~additional_cards:[ pegasus; astral_cloak; astral_dagger ] cards
