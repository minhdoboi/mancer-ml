(* Special house Meteomancer — port of priv/sp/house/Meteomancer.scala. Summoning a Meteo creature
   attunes a Called Element (Fire 0 .. Earth 3) carried on [Called_element] command flags. *)

open Sp
open Engine
open Common

let fateweaver_phase = "fateweaver phase"
let empty_data = { mt_called = None; mt_hidden = false; mt_fate = 0; mt_hide = None }
let get_data p = match p.pstate.ps_data with Meteo_data d -> d | _ -> empty_data
let set_data p d = Player.set_data p (Meteo_data d)
let get_called p = (get_data p).mt_called
let is_meteo (c : card) = c.house_id = 33

let with_called_choices (cmd : command) =
  if is_meteo cmd.cmd_card && cmd.cmd_flag = None then
    List.init 4 (fun i -> { cmd with cmd_flag = Some (Called_element i) })
  else [ cmd ]

let hide_non_called called : desc_mod =
 fun house cards ->
  if house.phd_house.h_index = called then cards
  else Array.map (fun cd -> { cd with cd_enabled = false }) cards

let count_meteo (p : player_update) =
  Array.fold_left
    (fun n s ->
      match s.value with Some st when is_meteo st.card.c_card -> n + 1 | _ -> n)
    0 p.slots_update.slot_updates

let mistwalker_attack = Attack_state_func (fun a p -> if count_meteo p = 1 then a + 5 else a)

let clairvoyant_summon : card_effect =
 fun env ->
  match get_called (player_of_env env) with
  | None -> ()
  | Some called ->
      let op = other_of_env env in
      Houses.incr_mana op.houses_update 3 [ called ];
      Houses.incr_mana op.houses_update (-1) (List.filter (( <> ) called) [ 0; 1; 2; 3; 4 ])

let prism_turn : card_effect =
 fun env ->
  match get_called (player_of_env env) with
  | None -> ()
  | Some called ->
      let mana = (other_of_env env).houses_update.hu_value.(called).mana in
      let dmg = mana / 2 in
      if dmg > 0 then (
        Updater.focus env.env_updater env.env_selected env.env_player true;
        Slots.inflict_creatures (other_of_env env).slots_update
          (mk_damage ~is_ability:true dmg (ctx_of_env env)))

let ctx_of owner source source_num =
  { ctx_player = owner.pid; ctx_card = Some source; ctx_selected = source_num }

let cast_call_to_thunder owner source source_num =
  let opp = other_player owner in
  let ctx = ctx_of owner source source_num in
  let target =
    Slots.foldl opp.slots_update None (fun acc s ->
        match acc with
        | None -> Some s
        | Some a -> if (Slot.get s).life > (Slot.get a).life then Some s else acc)
  in
  Option.iter (fun s -> Slot.inflict s (mk_damage ~is_spell:true 6 ctx)) target;
  Player.inflict opp (mk_damage ~is_spell:true 6 ctx);
  focus_slot owner.slots_update.slot_updates.(source_num)

let storm_strike owner source source_num =
  let opp = other_player owner in
  let ctx = ctx_of owner source source_num in
  let d = mk_damage ~is_ability:true 19 ctx in
  Slots.inflict_creatures opp.slots_update d;
  Player.inflict opp d;
  focus_slot owner.slots_update.slot_updates.(source_num)

let extremophile_ref : card option ref = ref None

let spawn_extremophile owner =
  match List.rev (Slots.get_open owner.slots_update) with
  | [] -> ()
  | n :: _ ->
      Option.iter
        (fun c -> Slots.add_creature owner.slots_update.slot_updates.(n) (make_creature c))
        !extremophile_ref

let on_opponent_plays_called owner (command : command) =
  match get_called owner with
  | Some called when command.cmd_card.house_index = called ->
      let is_spell_of_called = command.cmd_card.is_spell in
      let filled =
        Array.to_list owner.slots_update.slot_updates
        |> List.filter_map (fun (s : slot_update) ->
            match s.value with Some st when is_meteo st.card.c_card -> Some (s, st) | _ -> None)
      in
      List.iter
        (fun ((slot : slot_update), st) ->
          match st.card.c_card.key with
          | "meteomancer.thief" when is_spell_of_called ->
              cast_call_to_thunder owner st.card.c_card slot.num
          | "meteomancer.extremophile" -> spawn_extremophile owner
          | "meteomancer.prism" -> add_attack slot (Attack_func (fun a -> a + 1))
          | "meteomancer.storm" -> storm_strike owner st.card.c_card slot.num
          | "meteomancer.fateweaver" ->
              Houses.incr_mana owner.houses_update 4 [ called ];
              set_data owner { (get_data owner) with mt_fate = (get_data owner).mt_fate + 1 }
          | _ -> ())
        filled
  | _ -> ()

let set_called_on_summon p (c : command) =
  if is_meteo c.cmd_card then
    let flag_idx =
      match c.cmd_flag with Some (Called_element idx) when idx >= 0 && idx <= 3 -> Some idx | _ -> None
    in
    let prev = get_data p in
    let next = match flag_idx with Some i -> Some i | None -> ( match prev.mt_called with Some i -> Some i | None -> Some 0 ) in
    set_data p { prev with mt_called = next; mt_hidden = c.cmd_card.key = "meteomancer.mistwalker" }

let remove_hide p =
  match get_data p with
  | { mt_hide = Some m; _ } as d ->
      Player.remove_desc_mod p m;
      set_data p { d with mt_hide = None }
  | _ -> ()

let after_own_submit p =
  remove_hide p;
  let charges = (get_data p).mt_fate in
  if charges > 0 then (
    set_data p { (get_data p) with mt_fate = charges - 1 };
    match get_called p with
    | None -> ()
    | Some called ->
        let m = hide_non_called called in
        Player.add_desc_mod p m;
        set_data p { (get_data p) with mt_hide = Some m };
        Player.add_transition p (Wait_player (p.pid, Some fateweaver_phase));
        Player.add_effect p (On_end_turn, one_time_player_effect (fun e -> remove_hide (player_of_env e))))

let meteo_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let old = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            set_called_on_summon p c;
            let r = old c in
            after_own_submit p;
            r);
        let op = other_player p in
        let old_opp = op.submit_command_hook in
        op.submit_command_hook <-
          (fun c ->
            let r = old_opp c in
            on_opponent_plays_called p c;
            r));
  }

let extremophile_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        if is_effect d && d.context.ctx_player <> me.slots.player.pid then { d with amount = 0 } else d);
  }

let grenadier_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if e.s_player.pid <> me.slots.player.pid && e.s_card.c_card.house_index < 4 then
          match get_called me.slots.player with
          | None -> ()
          | Some called -> (
              let slot = e.s_player.slots_update.slot_updates.(e.s_num) in
              match slot.value with
              | None -> ()
              | Some state ->
                  let ctx = ctx_of me.slots.player (Option.get me.value).card.c_card me.num in
                  let amount =
                    if e.s_card.c_card.house_index = called then (state.life + 1) / 2 else 4
                  in
                  Slot.inflict slot (mk_damage ~is_ability:true amount ctx);
                  focus_slot me));
  }

let cards =
  [
    creature ~key:"meteomancer.thief" ~attack:(attack 4) ~life:15 ~description:"meteomancer.thief.description"
      ();
    creature ~key:"meteomancer.extremophile" ~attack:(attack 4) ~life:11
      ~description:"meteomancer.extremophile.description" ~new_reaction:extremophile_reaction ();
    creature ~key:"meteomancer.clairvoyant" ~attack:(attack 5) ~life:19
      ~description:"meteomancer.clairvoyant.description" ~effects:(effects [ (Direct, clairvoyant_summon) ])
      ();
    creature ~key:"meteomancer.mistwalker" ~attack:{ base = Some 2; sources = [ mistwalker_attack ] } ~life:28
      ~description:"meteomancer.mistwalker.description" ();
    creature ~key:"meteomancer.grenadier" ~attack:(attack 6) ~life:26
      ~description:"meteomancer.grenadier.description" ~new_reaction:grenadier_reaction ();
    creature ~key:"meteomancer.fateweaver" ~attack:(attack 5) ~life:24
      ~description:"meteomancer.fateweaver.description" ();
    creature ~key:"meteomancer.prism" ~attack:(attack 4) ~life:38 ~description:"meteomancer.prism.description"
      ~effects:(effects [ (On_turn, prism_turn) ])
      ();
    creature ~key:"meteomancer.storm" ~attack:(attack 8) ~life:45 ~description:"meteomancer.storm.description"
      ();
  ]

let () = extremophile_ref := Some (List.nth cards 1)

let house =
  {
    h_key = "meteomancer";
    h_name = "meteomancer";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 33;
    h_additional_cards = [];
    h_make_listener = meteo_listener;
    h_data = Meteo_data empty_data;
  }
