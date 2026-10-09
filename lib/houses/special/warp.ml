(* Special house Warp — port of priv/sp/house/Warp.scala.

   A silenced creature ("bridled", Scala's MereMortal) is a copy of its card with no effects, no reaction and
   the plain attack; it keeps the card id and points back to the original through [Mortal_of], so
   [unbridle] can restore it. Scala caches one MereMortal per card; this port builds a new one each time,
   as nothing compares them. *)

open Sp
open Engine
open Common

let photographer_ref : card option ref = ref None
let slot_of_env env = (player_of_env env).slots_update.slot_updates.(env.env_selected)

(* --------------------------------------------------------- silenced cards *)

(* MereMortalReaction: only forwards the binding and the clean-up to the original's reaction *)
let mere_reaction (orig : creature) () =
  let inner = orig.c_body.b_new_reaction () in
  { (Reaction.default ()) with selected = inner.selected; clean_up = inner.clean_up }

let mere_mortal (orig : creature) : creature =
  let body =
    {
      orig.c_body with
      b_new_reaction = mere_reaction orig;
      b_run_attack = normal_run_attack;
      b_is_altar = false;
      b_mod = None;
      b_data = Mortal_of orig;
    }
  in
  let card =
    {
      orig.c_card with
      effects = no_effects;
      input_spec = Select_owner_slot;
      description = "";
      creature_body = Some body;
    }
  in
  { c_card = card; c_body = body }

(* [st] has already been taken off the slot *)
let bridle (st : slot_state) (slot : slot_update) =
  let m = mere_mortal st.card in
  let reaction = m.c_body.b_new_reaction () in
  reaction.selected := Some slot;
  slot.value <- Some { st with card = m; reaction; attack = Slots.current_attack slot st }

let unbridle (slot : slot_update) =
  match slot.value with
  | Some { card = { c_body = { b_data = Mortal_of orig; _ }; _ }; _ } ->
      let removed = slot.remove_hook None in
      let reaction = orig.c_body.b_new_reaction () in
      Slot.overwrite_add slot
        { removed with card = orig; reaction; attack = Slots.current_attack slot removed }
  | _ -> ()

(* ------------------------------------------------------------------ spells *)

let quake : card_effect =
 fun env ->
  let mine = (player_of_env env).houses_update.hu_value in
  let op = other_of_env env in
  Slots.foreach op.slots_update (fun slot ->
      let h = (Slot.get slot).card.c_card.house_index in
      let amount = abs (mine.(h).mana - op.houses_update.hu_value.(h).mana) in
      Slot.inflict slot (mk_damage ~is_spell:true amount (ctx_of_env env)))

let ram : card_effect =
 fun env ->
  let op = other_of_env env in
  let slot = op.slots_update.slot_updates.(env.env_selected) in
  Option.iter
    (fun (s : slot_state) ->
      Slot.destroy slot;
      Houses.incr_mana op.houses_update (max 0 (s.card.c_card.cost - 3)) [ s.card.c_card.house_index ])
    slot.value

(* The Queen silences every enemy creature, hurts them, and lets them go at the end of the next turn *)
let queen : card_effect =
 fun env ->
  let op = other_of_env env in
  Slots.foreach op.slots_update (fun slot -> bridle (slot.remove_hook None) slot);
  Slots.inflict_creatures op.slots_update (mk_damage ~is_ability:true 4 (ctx_of_env env));
  Player.add_effect (player_of_env env)
    (On_end_turn, countdown 2 (fun env -> Slots.foreach (other_of_env env).slots_update unbridle))

(* ---------------------------------------------------------------- stranger *)

let stranger_source =
  Attack_state_func
    (fun a p -> a + Array.fold_left (fun m h -> max m h.mana) 0 (other_player p).houses_update.hu_value)

(* MergeStranger: the Stranger's body with the opposite creature's effects, reaction and attack kind *)
let merge_stranger (s : creature) (c : creature) : creature =
  let sb = s.c_body and cb = c.c_body in
  let body =
    {
      cb with
      b_attack = sb.b_attack;
      b_life = sb.b_life;
      b_run_attack = (if is_multi_run cb.b_run_attack then sb.b_run_attack else cb.b_run_attack);
    }
  in
  let card =
    {
      s.c_card with
      id = c.c_card.id;
      effects = c.c_card.effects;
      description = c.c_card.description;
      creature_body = Some body;
    }
  in
  { c_card = card; c_body = body }

let merge : card_effect =
 fun env ->
  let slot = slot_of_env env in
  Option.iter
    (fun (opp : slot_state) ->
      let s = Slot.get slot in
      Slot.destroy slot;
      let merged = merge_stranger s.card opp.card in
      let b = s.card.c_body in
      let st =
        {
          s with
          card = merged;
          attack_sources = b.b_attack;
          reaction = merged.c_body.b_new_reaction ();
          data = merged.c_body.b_data;
        }
      in
      Slot.overwrite_add slot { st with attack = Slots.current_attack slot st })
    (Slot.opposite slot).value

(* ------------------------------------------------------------------- cloak *)

let cloak_source =
  Attack_slot_state_func
    (fun a slot -> match slot.value with Some { data = Cloaked c; _ } -> a + c.attack | _ -> a)

let cloak_mana : card_effect =
 fun env ->
  match (slot_of_env env).value with
  | Some { data = Cloaked c; _ } ->
      Houses.incr_mana (player_of_env env).houses_update 2 [ c.card.c_card.house_index ]
  | _ -> ()

let cloak_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_spawn_over =
      (fun _ ->
        let me = Reaction.bound_slot r in
        let cloaked = Slot.get me in
        ignore (me.remove_hook None);
        Some
          (fun st ->
            {
              st with
              data = Cloaked cloaked;
              life = st.life + cloaked.life;
              max_life = st.max_life + cloaked.life;
            }));
    on_my_death =
      (fun dead ->
        match dead.d_slot.data with
        | Cloaked c ->
            let slot = dead.d_player.slots_update.slot_updates.(dead.d_num) in
            let b = c.card.c_body in
            let st = { c with attack_sources = b.b_attack; target = [ slot.num ]; data = b.b_data } in
            Slot.overwrite_add slot { st with attack = Slots.current_attack slot st }
        | _ -> ());
  }

(* -------------------------------------------------------- errant and others *)

let errant_attack target d player =
  if single_target_attack target d player then
    Slot.toggle player.slots_update.slot_updates.(d.context.ctx_selected) paused_flag

let errant_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun _ ->
        let me = Reaction.bound_slot r in
        match me.value with Some st when has_flag st paused_flag -> Slot.toggle_off me paused_flag | _ -> ());
  }

(* An adjacent creature that dies is replaced by a Photographer with the same life (broadcast by the house
   listener) *)
let photo_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        match (!photographer_ref, me.value) with
        | Some photo, Some mine
          when dead.d_slot.card.c_card != photo
               && dead.d_player.pid = me.slots.player.pid
               && abs (dead.d_num - me.num) = 1 ->
            let slot = dead.d_player.slots_update.slot_updates.(dead.d_num) in
            Slots.add_creature slot (make_creature photo);
            Slot.update slot (fun st -> { st with life = mine.life })
        | _ -> ());
  }

(* The opposite creature loses its abilities until the Schizo leaves *)
let schizo_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num then
          let opp = Slot.opposite me in
          Option.iter
            (fun (st : slot_state) ->
              ignore (opp.remove_hook None);
              Slot.set_data me (Schizo_target st.slot_id);
              bridle st opp)
            opp.value);
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some { data = Schizo_target id; _ } ->
            (* the creature may have moved *)
            Option.iter unbridle (Slots.find_slot_id (other_player me.slots.player).slots_update id)
        | _ -> ());
  }

(* ---------------------------------------------------------------- listener *)

(* OpponentListener: Warp plays with the listener of the opponent's special house, on top of its own, and
   takes over the opponent's player data *)
let warp_listener () =
  let inner = ref default_listener in
  {
    hl_on_death = (fun () -> !inner.hl_on_death ());
    hl_mod = Fun.id;
    hl_intercept_submit = (fun c -> !inner.hl_intercept_submit c);
    hl_init =
      (fun p ->
        let op = other_player p in
        Array.iter
          (fun h ->
            if h.phd_house.h_index = 4 && h.phd_house.h_key <> "warp" then
              match h.phd_house.h_key with
              | "limbo" | "colors" | "soulbinder" | "reaper" -> ()
              | _ -> inner := h.phd_house.h_make_listener ())
          p.updater.desc.gd_players.(op.pid).pd_houses;
        !inner.hl_init p;
        init_owner_death p;
        match op.pstate.ps_data with
        | No_player_data | Reaper_data _ -> ()
        | data -> Player.set_data p data);
  }

let errant =
  creature ~key:"warp.errant" ~attack:(attack 4) ~life:19 ~description:"warp.errant.description"
    ~run_attack:(custom_run_attack errant_attack) ~new_reaction:errant_reaction ()

let photographer =
  creature ~key:"warp.photographer" ~attack:(attack 4) ~life:18 ~description:"warp.photographer.description"
    ~new_reaction:photo_reaction ()

let () = photographer_ref := Some photographer

let cards =
  [
    errant;
    spell ~key:"warp.quake" ~description:"warp.quake.description" ~effects:(effects [ (Direct, quake) ]) ();
    creature ~key:"warp.cloak"
      ~attack:{ base = Some 2; sources = [ cloak_source ] }
      ~life:15 ~description:"warp.cloak.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, cloak_mana) ])
      ~new_reaction:cloak_reaction ();
    photographer;
    creature ~key:"warp.schizo" ~attack:(attack 5) ~life:22 ~description:"warp.schizo.description"
      ~new_reaction:schizo_reaction ();
    creature ~key:"warp.ram" ~attack:(attack 6) ~life:26 ~description:"warp.ram.description"
      ~effects:(effects [ (Direct, ram) ])
      ();
    creature ~key:"warp.stranger"
      ~attack:{ base = None; sources = [ stranger_source ] }
      ~life:30 ~description:"warp.stranger.description"
      ~effects:(effects [ (Direct, merge) ])
      ();
    creature ~key:"warp.queen" ~attack:(attack 6) ~life:32 ~description:"warp.queen.description"
      ~effects:(effects [ (Direct, queen) ])
      ();
  ]

let house =
  {
    h_key = "warp";
    h_name = "warp";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 15;
    h_additional_cards = [];
    h_make_listener = warp_listener;
    h_data = No_player_data;
  }
