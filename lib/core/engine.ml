(* The update layer, reaction defaults and effect combinators.

   Mirrors priv/sp/update/{SlotUpdate,SlotsUpdate,PlayerUpdate,GameUpdate}.scala
   and priv/sp/GameCardEffect.scala.

   - Scala's FieldUpdate keeps tid/dirty/valuedirty so `value` can be a lazily
     rebuilt cache of the parent.  Here slot and player records are mutated in
     place behind the same parent pointers, so a read always sees the latest
     child state, and attack sources are re-folded on every refresh instead of
     tracking `dirtyElementals`.

   - The modules are defined in dependency order, so no `let rec` spans them:
     Board (slot-group iteration and broadcasts) -> Slot -> Slots -> Player.
     Slots re-exports Board. *)

open Sp

let emit (u : updater) e = u.listener e

(* ---------------------------------------------------------------- helpers *)

let other_player (p : player_update) = p.updater.player_updates.(other p.pid)

let make_creature (c : card) =
  match c.creature_body with
  | Some b -> { c_card = c; c_body = b }
  | None -> failwith (c.name ^ " is not a creature")

let mk_env pid (u : updater) =
  {
    env_player = pid;
    env_updater = u;
    env_selected = no_selection;
    env_card = None;
    env_paid_cost = 0;
    env_target_other = false;
  }

let ctx_of_env env = { ctx_player = env.env_player; ctx_card = env.env_card; ctx_selected = env.env_selected }
let player_of_env env = env.env_updater.player_updates.(env.env_player)
let other_of_env env = env.env_updater.player_updates.(other env.env_player)
let spell_damage env amount = mk_damage ~is_spell:true amount (ctx_of_env env)
let ability_damage env amount = mk_damage ~is_ability:true amount (ctx_of_env env)
let own_slot_of_env env = (player_of_env env).slots_update.slot_updates.(env.env_selected)
let opp_slot_of_env env = (other_of_env env).slots_update.slot_updates.(env.env_selected)
let get_mana env house_index = (player_of_env env).pstate.ps_houses.(house_index).mana

(* PlayerUpdate.scala:253-266.  Called on the *victim*: if the damage came from
   the victim itself, hand it to the other side (this is how a creature's own
   ability gets the attacker's SpellMod).  Spell damage is then folded over the
   attacker's creatures, each SpellMod multiplying the running amount.  The
   fold starts from `d`, not from `other mod d`, so it terminates after one
   hop. *)
let rec mod_damage (p : player_update) (d : damage) =
  if d.context.ctx_player = p.pid then mod_damage (other_player p) d
  else if d.is_spell then
    let attacker = p.updater.player_updates.(d.context.ctx_player) in
    let d = attacker.house_event_listener.hl_mod d in
    Array.fold_left
      (fun acc s ->
        match s.value with
        | Some { card = { c_body = { b_mod = Some (Spell_mod f); _ }; _ }; _ } ->
            { acc with amount = f acc.amount }
        | _ -> acc)
      d (other_player p).slots_update.slot_updates
  else d

(* ---------------------------------------------------------------- houses *)

module Houses = struct
  let incr_mana_all (hu : houses_update) =
    hu.hu_value <- Array.map (fun h -> hu.hu_player.add_mana_hook h h.growth) hu.hu_value;
    hu.after_update hu.hu_value

  let incr_mana (hu : houses_update) amount idxs =
    List.iter (fun i -> hu.hu_value.(i) <- hu.hu_player.add_mana_hook hu.hu_value.(i) amount) idxs;
    hu.after_update hu.hu_value

  let incr_growth (hu : houses_update) amount idxs =
    List.iter
      (fun i ->
        let h = hu.hu_value.(i) in
        hu.hu_value.(i) <- { h with growth = max 0 (h.growth + amount) })
      idxs
end

(* ------------------------------------------------------------------ board *)

(* The part of SlotsUpdate that Slot itself depends on *)
module Board = struct
  let foreach (su : slots_update) f = Array.iter (fun s -> if Option.is_some s.value then f s) su.slot_updates

  let foldl (su : slots_update) z f =
    Array.fold_left (fun acc s -> if Option.is_some s.value then f acc s else acc) z su.slot_updates

  let iter_states (su : slots_update) f = Array.iter (fun s -> Option.iter (f s) s.value) su.slot_updates
  let react_add su slot = iter_states su (fun _ st -> st.reaction.on_add slot)
  let react_remove su slot = iter_states su (fun _ st -> st.reaction.on_remove slot)

  let react_summon (su : slots_update) e =
    iter_states su (fun s st ->
        if su.player.pid <> e.s_player.pid || s.num <> e.s_num then st.reaction.on_summon e)

  let finish_dead (su : slots_update) dead =
    su.on_dead_hook dead;
    Option.iter
      (fun st ->
        emit su.player.updater (Ev_replaced { num = dead.d_num; player = su.player.pid; creature = st.card }))
      su.slot_updates.(dead.d_num).value

  let log (su : slots_update) e = su.logs <- e :: su.logs
end

(* ------------------------------------------------------------------- slot *)

module Slot = struct
  let get s = match s.value with Some v -> v | None -> failwith (Printf.sprintf "empty slot %d" s.num)
  let adjacent s = List.map (fun n -> s.slots.slot_updates.(n)) (adjacents s.num)
  let filled_adjacent s = List.filter (fun s -> Option.is_some s.value) (adjacent s)

  let open_adjacent s =
    List.filter
      (fun adj -> Option.is_none adj.value && List.mem adj.num s.slots.player.pstate.ps_slot_list)
      (adjacent s)

  let opposite s = (other_player s.slots.player).slots_update.slot_updates.(s.num)

  (* Scala's `write(value map f)` *)
  let update s f = s.value <- Option.map f s.value
  let toggle s flag = update s (fun x -> { x with status = x.status lor flag })
  let toggle_off s flag = update s (fun x -> { x with status = x.status land lnot flag })
  let set_data s d = update s (fun x -> { x with data = d })
  let set_target s t = update s (fun x -> { x with target = t })

  let toggle_run s =
    match s.value with
    | Some x when (not (has_flag x run_flag)) || has_flag x stun_flag ->
        s.value <- Some { x with status = x.status lor run_flag land lnot stun_flag }
    | _ -> ()

  let is_runnable st = has_flag st run_flag && not (has_flag st on_hold)

  let add s st =
    s.value <- Some st;
    st.reaction.selected := Some s;
    Board.react_add s.slots s;
    s.slots.after_change ()

  let overwrite_add s st = s.add_hook st

  (* -- interception points: route through the occupant's reaction *)
  let heal s amount = match s.value with Some x when x.life > 0 -> x.reaction.heal s amount | _ -> ()
  let inflict s d = Option.iter (fun x -> x.reaction.inflict s (mod_damage s.slots.player d)) s.value
  let destroy s = Option.iter (fun x -> x.reaction.destroy s) s.value
  let stun s = Option.iter (fun x -> x.reaction.stun s) s.value

  (* -- default actions (CardSpec.scala:189-192) *)
  let do_heal s amount = update s (fun x -> { x with life = min x.max_life (x.life + amount) })
  let do_stun s = toggle s stun_flag

  (* SlotState.inflict *)
  let state_inflict st d =
    if has_flag st invincible_flag then Some st
    else
      let life = st.life - d.amount in
      if life < 1 then None else Some { st with life }

  let dead_event s st damage = { d_num = s.num; d_slot = st; d_player = s.slots.player; d_damage = damage }

  let remove s dead_opt =
    match s.value with
    | None -> failwith (Printf.sprintf "remove on empty slot %d" s.num)
    | Some st ->
        st.reaction.on_my_remove dead_opt;
        Board.react_remove s.slots s;
        (* the removal reactions may have edited this slot's attack sources
           (an Orc next to a moved creature takes its bonus back): return the
           post-reaction state, or a move re-applies the bonus on the new slot *)
        let st = Option.value s.value ~default:st in
        st.reaction.clean_up ();
        s.value <- None;
        s.slots.after_change ();
        st

  let do_priv_destroy s =
    Option.iter
      (fun st ->
        let ev = dead_event s st None in
        ignore (s.remove_hook (Some ev));
        Board.finish_dead s.slots ev)
      s.value

  let delayed_destroy s d =
    Option.iter
      (fun st ->
        let ev = dead_event s st (Some d) in
        ignore (s.remove_hook (Some ev));
        Board.log s.slots (Board_dead ev))
      s.value

  (* SlotUpdate.damageSlot (SlotUpdate.scala:70-85).  Does NOT re-apply the
     player mod: `inflict` already did. *)
  let damage_slot s d =
    Option.iter
      (fun st ->
        (* /!\ self_protect may have a side effect; protect_hook is Scala's
           SlotUpdate.protect decorator (LostChurch wraps it). *)
        let d = s.protect_hook (st.reaction.self_protect d) in
        let st = match s.value with Some x -> x | None -> st in
        let amount =
          match state_inflict st d with
          | None ->
              s.delayed_destroy_hook d;
              st.life
          | Some ns ->
              s.value <- Some ns;
              st.life - ns.life
        in
        s.on_damage_hook st { d with amount })
      s.value
end

(* ------------------------------------------------------------------ slots *)

module Slots = struct
  include Board

  let get_open (su : slots_update) =
    Array.to_list su.slot_updates
    |> List.filter_map (fun s ->
        if Option.is_none s.value && List.mem s.num su.player.pstate.ps_slot_list then Some s.num else None)

  let inflict_creatures su d = foreach su (fun s -> Slot.inflict s d)

  let fold_attack (su : slots_update) slot src =
    List.fold_left
      (fun acc -> function
        | Attack_func f -> f acc
        | Attack_bonus b -> b.bonus acc
        | Attack_state_func f -> f acc su.player
        | Attack_slot_state_func f -> f acc slot
        (* CardSpec.scala:297 — attack is a function of the owner's house mana.
           Read [houses_update], not [pstate]: mana from the current step
           (Meditate, payment) is not synced onto [pstate] until [result]. *)
        | Mana_attack idx -> acc + su.player.houses_update.hu_value.(idx).mana
        | Extra_attacks _ -> acc
        | Aging_malus -> acc - 3)
      (Option.value src.base ~default:0) src.sources

  let current_attack slot st = fold_attack slot.slots slot st.attack_sources

  let build_slot_state slot creature =
    let reaction = creature.c_body.b_new_reaction () in
    reaction.selected := Some slot;
    let b = creature.c_body in
    {
      card = creature;
      life = b.b_life;
      max_life = b.b_life;
      status = b.b_status;
      attack_sources = b.b_attack;
      attack = fold_attack slot.slots slot b.b_attack;
      target = [ slot.num ];
      slot_id = fresh_slot_id ();
      reaction;
      data = b.b_data;
    }

  let summon (su : slots_update) num creature =
    let slot = su.slot_updates.(num) in
    let base = build_slot_state slot creature in
    let st =
      match slot.value with
      | Some old -> (
          old.reaction.on_overwrite creature;
          match base.reaction.on_spawn_over slot with Some m -> m base | None -> base)
      | None -> base
    in
    (* re-read: the default on_spawn_over has just emptied the slot *)
    let summoned = Option.is_none slot.value in
    if summoned then Slot.overwrite_add slot st;
    emit su.player.updater (Ev_summon { num; player = su.player.pid; creature });
    if summoned then (
      let ev = { s_num = num; s_card = creature; s_player = su.player } in
      react_summon (other_player su.player).slots_update ev;
      react_summon su ev);
    st.slot_id

  let toggle_run su = foreach su Slot.toggle_run
  let heal_creatures su amount = foreach su (fun s -> Slot.heal s amount)

  let find (su : slots_update) pred =
    Array.find_opt (fun s -> match s.value with Some st -> pred st | None -> false) su.slot_updates

  let find_card su (card : card) = find su (fun st -> st.card.c_card.id = card.id)
  let find_slot_id su id = find su (fun st -> st.slot_id = id)

  (* SlotUpdate.add(card: Creature): place without going through summon (no onSummon). *)
  let add_creature slot creature = Slot.overwrite_add slot (build_slot_state slot creature)

  let priv_move st (dest : slot_update) =
    let st = { st with target = [ dest.num ]; attack = fold_attack dest.slots dest st.attack_sources } in
    st.reaction.selected := Some dest;
    Slot.overwrite_add dest st

  let swap slot dest_slot =
    emit slot.slots.player.updater
      (Ev_swap
         {
           num = slot.num;
           dest = dest_slot.num;
           player = slot.slots.player.pid;
           dest_player = dest_slot.slots.player.pid;
         });
    let removed = Slot.remove slot None in
    let removed2 = Slot.remove dest_slot None in
    priv_move removed dest_slot;
    priv_move removed2 slot

  let move ?dest_player (su : slots_update) num dest =
    let dest_p = Option.value dest_player ~default:su.player in
    if List.mem dest dest_p.pstate.ps_slot_list then
      match su.slot_updates.(num).value with
      | None -> ()
      | Some _ ->
          let dest_slot = dest_p.slots_update.slot_updates.(dest) in
          if Option.is_some dest_slot.value then swap su.slot_updates.(num) dest_slot
          else (
            emit su.player.updater (Ev_move { num; dest; player = su.player.pid; dest_player = dest_p.pid });
            let removed = Slot.remove su.slot_updates.(num) None in
            priv_move removed dest_slot)
end

(* ----------------------------------------------------------------- player *)

module Player = struct
  let other = other_player
  let mod_damage = mod_damage

  let flush p =
    let su = p.slots_update in
    if su.logs <> [] then (
      List.iter
        (function
          | Board_dead dead ->
              p.stats.nb_dead <- p.stats.nb_dead + 1;
              Slots.finish_dead su dead
          | Board_damage _ | Board_summon _ -> ())
        (List.rev su.logs);
      su.logs <- [])

  (* Re-folds attack sources on every call, which is what makes Scala's
     dirtyElementals()/AttackUpdate invalidation unnecessary. *)
  let refresh_slots p =
    Array.iter
      (fun s ->
        p.pstate.ps_slots.(s.num) <-
          Option.map
            (fun st ->
              let attack = Slots.current_attack s st in
              let st = if attack = st.attack then st else { st with attack } in
              s.value <- Some st;
              st)
            s.value)
      p.slots_update.slot_updates

  let sync_houses p = p.pstate.ps_houses <- p.houses_update.hu_value

  let result p =
    flush p;
    sync_houses p;
    refresh_slots p;
    p.pstate

  let inflict p d =
    if Option.is_none p.updater.ended then (
      let amount = (p.guard_hook (mod_damage p d)).amount in
      let life = p.pstate.ps_life - amount in
      if life <= 0 && p.house_event_listener.hl_on_death () then p.updater.ended <- Some (Sp.other p.pid)
      else p.on_player_damage_hook { d with amount };
      p.pstate.ps_life <- life)

  let heal p amount =
    if Option.is_none p.updater.ended && p.pstate.ps_life > 0 then
      p.pstate.ps_life <- p.pstate.ps_life + amount

  let add_desc_mod p m = p.pstate.ps_desc.desc_mods <- p.pstate.ps_desc.desc_mods @ [ m ]
  let insert_desc_mod p m = p.pstate.ps_desc.desc_mods <- m :: p.pstate.ps_desc.desc_mods

  (* PlayerState.remove (State.scala:112): only the first occurrence, so a mod
     added by two cards survives until both are gone *)
  let remove_desc_mod p m =
    let rec drop = function [] -> [] | x :: xs when x == m -> xs | x :: xs -> x :: drop xs in
    p.pstate.ps_desc.desc_mods <- drop p.pstate.ps_desc.desc_mods

  let set_data p d = p.set_data_hook d

  (* SingleTargetAttack / MultiTargetAttack (CardSpec.scala:230-264) *)
  let run_attack p st d =
    let op = other_player p in
    match st.card.c_body.b_run_attack.apply_ with
    | Custom f -> f st.target d p
    | Multi_target ->
        inflict op d;
        Slots.inflict_creatures op.slots_update d
    | Single_target ->
        if st.target = [] then inflict op d
        else
          let hits =
            List.filter
              (fun num ->
                let s = op.slots_update.slot_updates.(num) in
                let filled = Option.is_some s.value in
                if filled then Slot.inflict s d;
                filled)
              st.target
          in
          if List.length hits < List.length st.target then inflict op d

  let run_slot_once p num st =
    let ctx = { ctx_player = p.pid; ctx_card = Some st.card.c_card; ctx_selected = num } in
    emit p.updater (Ev_run_slot { num; player = p.pid });
    run_attack p st (mk_damage st.attack ctx);
    emit p.updater (Ev_refresh { silent = false })

  let locate_slot p num id =
    refresh_slots p;
    let arr = p.slots_update.slot_updates in
    let at_num = if num < Array.length arr then arr.(num).value else None in
    match at_num with
    | Some v when v.slot_id = id -> Some (num, v)
    | _ ->
        Array.find_map
          (fun s -> match s.value with Some v when v.slot_id = id -> Some (s.num, v) | _ -> None)
          arr

  let run_slot p num st =
    let extras =
      List.fold_left (fun a -> function Extra_attacks n -> a + n | _ -> a) 0 st.attack_sources.sources
    in
    let rec loop num i =
      if i <= extras && Option.is_none p.updater.ended then
        match locate_slot p num st.slot_id with
        | Some (n, s) when s.attack > 0 && Slot.is_runnable s && s.card.c_body.b_run_attack.can_attack ->
            run_slot_once p n s;
            loop n (i + 1)
        | _ -> ()
    in
    loop num 0

  (* Iterate a snapshot: a creature can move or die mid-sweep, so each one is
     re-located by id before acting. *)
  let iter_snapshot p f =
    refresh_slots p;
    Array.iter
      (Option.iter (fun st ->
           if Option.is_none p.updater.ended then
             Option.iter (fun (num, cur) -> f num cur) (locate_slot p 0 st.slot_id)))
      (Array.copy p.pstate.ps_slots)

  let run_slots p = iter_snapshot p (run_slot p)
  let add_effect p e = p.pstate.ps_effects <- e :: p.pstate.ps_effects

  let remove_effect p cond =
    p.pstate.ps_effects <- List.filter (fun (_, f) -> not (cond f)) p.pstate.ps_effects

  let map_effect p f = p.pstate.ps_effects <- List.map (fun (ph, e) -> (ph, f e)) p.pstate.ps_effects
  let add_transition p t = p.pstate.ps_transitions <- t :: p.pstate.ps_transitions

  let pop_transition p =
    match p.pstate.ps_transitions with
    | [] -> None
    | head :: tail ->
        p.pstate.ps_transitions <- tail;
        Some head

  let block_slot p n = p.pstate.ps_slot_list <- List.filter (( <> ) n) p.pstate.ps_slot_list
  let prepare_next_turn p = Houses.incr_mana_all p.houses_update

  let apply_effect p phase num st =
    if not (has_flag st blocked_flag) then
      Option.iter
        (fun f ->
          let env = mk_env p.pid p.updater in
          env.env_selected <- num;
          env.env_card <- Some st.card.c_card;
          f env;
          emit p.updater (Ev_refresh { silent = true }))
        (effect_at st.card.c_card.effects phase)

  (* slot effects first, then player effects (PlayerUpdate.applyEffects) *)
  let apply_effects p phase =
    iter_snapshot p (apply_effect p phase);
    List.iter
      (fun (ph, f) -> if ph = phase && Option.is_none p.updater.ended then f (mk_env p.pid p.updater))
      p.pstate.ps_effects

  let submit_command_default p (c : command) =
    if c.cmd_card.is_spell then emit p.updater (Ev_spell_played c);
    Houses.incr_mana p.houses_update (-c.cmd_cost) [ c.cmd_card.house_index ];
    emit p.updater (Ev_refresh { silent = true });
    let slot_id =
      match c.cmd_input with
      | Some num when not c.cmd_card.is_spell ->
          let target =
            match c.cmd_card.input_spec with
            | Select_target_creature -> (other_player p).slots_update
            | _ -> p.slots_update
          in
          p.stats.nb_summon <- p.stats.nb_summon + 1;
          Some (Slots.summon target num (make_creature c.cmd_card))
      | _ -> None
    in
    Option.iter
      (fun f ->
        let env = mk_env c.cmd_player p.updater in
        env.env_card <- Some c.cmd_card;
        env.env_paid_cost <- c.cmd_cost;
        env.env_target_other <- c.cmd_flag = Some Target_other;
        Option.iter (fun n -> env.env_selected <- n) c.cmd_input;
        f env)
      c.cmd_card.effects.direct;
    emit p.updater (Ev_refresh { silent = false });
    slot_id

  (* PlayerUpdate.submitCommand is a var that houses decorate *)
  let submit_command p c = p.submit_command_hook c

  let submit p c =
    let c =
      match p.house_event_listener.hl_intercept_submit c with
      | true, c' -> c'
      | false, _ -> (
          match (other_player p).house_event_listener.hl_intercept_submit c with
          | true, c' -> c'
          | false, _ -> c)
    in
    Option.iter (fun cmd -> ignore (submit_command p cmd)) c
end

(* --------------------------------------------------------------- reaction *)

module Reaction = struct
  (* Actions (CardSpec.scala:189-192) operate on the bound slot directly:
     inflict -> damageSlot, heal -> addLife, stun -> toggle, destroy ->
     privDestroy.  They must not route back through Slot.inflict, or the
     default reaction would call itself. *)
  let default () =
    {
      selected = ref None;
      heal = Slot.do_heal;
      inflict = Slot.damage_slot;
      destroy = Slot.do_priv_destroy;
      stun = Slot.do_stun;
      on_add = ignore;
      on_remove = ignore;
      self_protect = Fun.id;
      on_my_damage = ignore;
      on_my_remove = ignore;
      on_my_death = ignore;
      on_summon = ignore;
      on_spawn_over =
        (fun s ->
          Slot.destroy s;
          None);
      on_overwrite = ignore;
      clean_up = ignore;
      on_protect = (fun e -> e.de_damage);
      on_death = ignore;
      on_limbo = ignore;
    }

  let bound_slot r = match !(r.selected) with Some s -> s | None -> failwith "reaction not bound to a slot"
end

(* ---------------------------------------------------------------- updater *)

let copy_player_state (p : player_state) =
  {
    ps_houses = Array.copy p.ps_houses;
    ps_desc = { init_desc = p.ps_desc.init_desc; desc_mods = p.ps_desc.desc_mods };
    ps_slots = Array.copy p.ps_slots;
    ps_slot_list = p.ps_slot_list;
    ps_life = p.ps_life;
    ps_effects = p.ps_effects;
    ps_data = p.ps_data;
    ps_transitions = p.ps_transitions;
  }

let copy_game_state (s : game_state) = { players = Array.map copy_player_state s.players }

module Updater = struct
  (* Hook defaults are Scala's `var` initialisers (SlotUpdate.scala:63,
     PlayerUpdate.scala:283).  `let rec` closes each cycle: OCaml accepts a
     record literal whose fields are closures over the value being defined. *)
  let make_slot su num =
    let rec s =
      {
        num;
        slots = su;
        value = None;
        overridable_destroy = (fun () -> Slot.do_priv_destroy s);
        add_hook = (fun st -> Slot.add s st);
        on_damage_hook = (fun st d -> st.reaction.on_my_damage d);
        remove_hook = (fun d -> Slot.remove s d);
        delayed_destroy_hook = (fun d -> Slot.delayed_destroy s d);
        protect_hook = Fun.id;
      }
    in
    s

  let make_player u id =
    let pstate = u.state.players.(id) in
    let listener =
      match
        Array.find_opt (fun h -> h.phd_house.h_index = special_house_index) u.desc.gd_players.(id).pd_houses
      with
      | Some h -> h.phd_house.h_make_listener ()
      | None -> default_listener
    in
    let rec p =
      {
        pid = id;
        updater = u;
        pstate;
        slots_update =
          {
            player = p;
            slot_updates = [||];
            logs = [];
            (* SlotsUpdate.onDead: a house replacing it decides whether onMyDeath runs *)
            on_dead_hook =
              (fun dead ->
                emit u (Ev_die { num = dead.d_num; player = id });
                dead.d_slot.reaction.on_my_death dead);
            after_change = ignore;
          };
        houses_update = { hu_player = p; hu_value = pstate.ps_houses; after_update = ignore };
        house_event_listener = listener;
        stats = { nb_dead = 0; nb_summon = 0 };
        submit_command_hook = (fun c -> Player.submit_command_default p c);
        on_player_damage_hook = ignore;
        guard_hook =
          (fun d ->
            Slots.foldl p.slots_update d (fun acc s ->
                match s.value with
                | Some v -> v.reaction.on_protect { de_damage = acc; de_target = None; de_player = p }
                | None -> acc));
        add_mana_hook = (fun h amount -> { h with mana = max 0 (h.mana + amount) });
        set_data_hook = (fun d -> p.pstate.ps_data <- d);
      }
    in
    p.slots_update.slot_updates <- Array.init nb_slots (make_slot p.slots_update);
    p

  (* Fresh reaction bound to this slot, without firing on_add: attack bonuses and
     mana growth are already in the copied houses / attack_sources. *)
  let bind_slot (slot : slot_update) (st : slot_state) =
    let reaction = st.card.c_body.b_new_reaction () in
    reaction.selected := Some slot;
    { st with reaction }

  let load_player (p : player_update) =
    Array.iteri
      (fun i -> function
        | None -> p.slots_update.slot_updates.(i).value <- None
        | Some st ->
            p.slots_update.slot_updates.(i).value <- Some (bind_slot p.slots_update.slot_updates.(i) st))
      p.pstate.ps_slots

  let create ?(rng = Dice.make 0) state desc =
    let state = copy_game_state state in
    let u = { state; desc; player_updates = [||]; ended = None; listener = ignore; rng } in
    u.player_updates <- Array.init 2 (make_player u);
    Array.iter (fun p -> p.house_event_listener.hl_init p) u.player_updates;
    Array.iter load_player u.player_updates;
    u

  let player u id = u.player_updates.(id)
  let ended u = u.ended
  let focus u num pid blocking = emit u (Ev_focus { num; player = pid; blocking })
  let listen u f = u.listener <- f

  (* A copy of the board as it stands mid-step, for UI refreshes. Unlike
     export it does not flush pending deaths, so a listener cannot change the
     game. Attack is re-folded into the copy (live mana and bonuses) so the
     board can show a Meditate buff before the creature lunges. *)
  let peek u =
    {
      players =
        Array.map
          (fun p ->
            {
              (copy_player_state p.pstate) with
              ps_slots =
                Array.map
                  (fun s ->
                    match s.value with
                    | None -> None
                    | Some st ->
                        let attack = Slots.current_attack s st in
                        if attack = st.attack then Some st else Some { st with attack })
                  p.slots_update.slot_updates;
              ps_houses = Array.copy p.houses_update.hu_value;
            })
          u.player_updates;
    }

  let export u =
    copy_game_state { players = Array.map (fun p -> copy_player_state (Player.result p)) u.player_updates }

  (* Scala's lift rebuilds the update graph from a state, runs f, and reads
     the result back.  Here the graph is mutated in place, so lift only
     re-syncs the house arrays and publishes the live state: the returned
     game_state is NOT a snapshot. *)
  let lift u f =
    u.ended <- None;
    Array.iter Player.sync_houses u.player_updates;
    let r = f u in
    let state = { players = Array.map Player.result u.player_updates } in
    u.state <- state;
    (r, state)
end

(* ---------------------------------------------------------------- effects *)

let effects (l : (phase * card_effect) list) =
  let at phase =
    match List.filter_map (fun (p, f) -> if p = phase then Some f else None) l with
    | [] -> None
    | [ f ] -> Some f
    | fs -> Some (fun env -> List.iter (fun f -> f env) fs)
  in
  { direct = at Direct; on_turn = at On_turn; on_end_turn = at On_end_turn; on_start = at On_start }

let no_effects = effects []

let focus (f : card_effect) : card_effect =
 fun env ->
  Updater.focus env.env_updater env.env_selected env.env_player true;
  f env

let damage ?(is_ability = false) ?(is_spell = false) amount : card_effect =
 fun env -> Player.inflict (other_of_env env) (mk_damage ~is_ability ~is_spell amount (ctx_of_env env))

let damage_creatures ?(is_ability = false) ?(is_spell = false) amount : card_effect =
 fun env ->
  Slots.inflict_creatures (other_of_env env).slots_update
    (mk_damage ~is_ability ~is_spell amount (ctx_of_env env))

let damage_creature ?(is_ability = false) ?(is_spell = false) amount : card_effect =
 fun env ->
  Slot.inflict
    (other_of_env env).slots_update.slot_updates.(env.env_selected)
    (mk_damage ~is_ability ~is_spell amount (ctx_of_env env))

let mass_damage ?(is_ability = false) ?(is_spell = false) ?(immune_self = false) amount : card_effect =
 fun env ->
  let d = mk_damage ~is_ability ~is_spell amount (ctx_of_env env) in
  Slots.inflict_creatures (other_of_env env).slots_update d;
  Slots.foreach (player_of_env env).slots_update (fun s ->
      if not (immune_self && s.num = env.env_selected) then Slot.inflict s d)

let heal amount : card_effect = fun env -> Player.heal (player_of_env env) amount

let heal_creature amount : card_effect =
 fun env -> Slot.heal (player_of_env env).slots_update.slot_updates.(env.env_selected) amount

let heal_creatures amount : card_effect =
 fun env -> Slots.heal_creatures (player_of_env env).slots_update amount

let add_mana amount house_indexes : card_effect =
 fun env -> Houses.incr_mana (player_of_env env).houses_update amount house_indexes

let add_growth amount house_indexes : card_effect =
 fun env -> Houses.incr_growth (player_of_env env).houses_update amount house_indexes

let fill_empty_slots creature : card_effect =
 fun env ->
  let p = player_of_env env in
  List.iter
    (fun num ->
      if Option.is_none p.slots_update.slot_updates.(num).value then
        Slots.add_creature p.slots_update.slot_updates.(num) creature)
    p.pstate.ps_slot_list

let add_desc_mod m : card_effect = fun env -> Player.add_desc_mod (player_of_env env) m
let remove_desc_mod m : card_effect = fun env -> Player.remove_desc_mod (player_of_env env) m

(* GameCardEffect.oneTimePlayerEffect: runs once, then removes itself from
   the owning player's effect list *)
let one_time_player_effect (f : card_effect) : card_effect =
  let rec wrapper env =
    f env;
    Player.remove_effect (player_of_env env) (fun e -> e == wrapper)
  in
  wrapper
