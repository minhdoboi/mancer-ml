(* Wind special house — port of priv/sp/house/MasterOfWind.scala. *)

open Sp
open Engine
open Common

let spirit_ref : card option ref = ref None
let get_wind p = match p.pstate.ps_data with Wind_state w -> w | _ -> { winged = false; vortex = false }
let set_wind p w = Player.set_data p (Wind_state w)

let winge : card_effect =
 fun env -> set_wind (player_of_env env) { (get_wind (player_of_env env)) with winged = true }

let simoom : card_effect =
 fun env ->
  let slot = (other_of_env env).slots_update.slot_updates.(env.env_selected) in
  match slot.value with
  | None -> ()
  | Some st ->
      if st.card.c_card.house_index <> 4 then (
        Slot.stun slot;
        Houses.incr_mana (player_of_env env).houses_update (st.card.c_card.cost / 2)
          [ st.card.c_card.house_index ])

let bl_phase = "Ball lightning phase"

let ball_lightning : card_effect =
 fun env ->
  let p = player_of_env env in
  Player.inflict (other_of_env env) (mk_damage ~is_spell:true 7 (ctx_of_env env));
  Player.add_transition p (Wait_player (p.pid, Some bl_phase))

let sq_phase1 = "Squall phase 1"
let sq_phase2 = "Squall phase 2"

let squall : card_effect =
 fun env ->
  let p = player_of_env env in
  let d = mk_damage ~is_spell:true 1 (ctx_of_env env) in
  Player.inflict (other_of_env env) d;
  Slots.inflict_creatures (other_of_env env).slots_update d;
  Player.add_transition p (Wait_player (p.pid, Some sq_phase2));
  Player.add_transition p (Wait_player (p.pid, Some sq_phase1))

let vortex : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  set_wind p { (get_wind p) with vortex = true };
  let air = op.houses_update.hu_value.(2).mana in
  Houses.incr_mana op.houses_update (-2) [ 2 ];
  Houses.incr_mana p.houses_update (min air 2) [ 2 ];
  Player.add_effect p
    ( On_end_turn,
      countdown 2 (fun env ->
          let p = player_of_env env in
          set_wind p { (get_wind p) with vortex = false }) )

let ww_phase = "whirlwind phase"

let whirlwind : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = p.slots_update.slot_updates.(env.env_selected) in
  Slot.destroy slot;
  (Slot.opposite slot).overridable_destroy ();
  Player.add_transition p (Wait_player (p.pid, Some ww_phase))

let storm : card_effect =
 fun env ->
  let p = player_of_env env in
  Slots.foreach p.slots_update (fun s -> Slot.set_target s []);
  Player.add_effect p
    ( On_end_turn,
      one_time_player_effect (fun env ->
          Slots.foreach (player_of_env env).slots_update (fun s -> Slot.set_target s [ s.num ])) )

let spirit_on_no_command (s : slot_update) =
  let d =
    mk_damage ~is_ability:true 3
      { ctx_player = s.slots.player.pid; ctx_card = !spirit_ref; ctx_selected = s.num }
  in
  let op = Player.other s.slots.player in
  Player.inflict op d;
  Slots.inflict_creatures op.slots_update d

let spirit_reaction () = Reaction.default ()

let spirit =
  creature ~key:"wind.TheSpiritOfThunderstorm.name" ~attack:(attack 2) ~life:28
    ~description:"wind.TheSpiritOfThunderstorm.description" ~new_reaction:spirit_reaction
    ~effects:(effects [ (On_turn, add_mana 1 [ 4 ]) ])
    ()

let () = spirit_ref := Some spirit

let wind_listener () =
  let player = ref None in
  let get () = match !player with Some p -> p | None -> failwith "wind listener not inited" in
  {
    hl_on_death = (fun () -> true);
    hl_mod =
      (fun d ->
        if (get_wind (get ())).vortex then
          { d with amount = int_of_float (ceil (float_of_int d.amount *. 1.5)) }
        else d);
    hl_intercept_submit =
      (fun c ->
        if Option.is_none c then
          Slots.foreach (get ()).slots_update (fun s ->
              match (!spirit_ref, s.value) with
              | Some sp, Some st when st.card.c_card.id = sp.id -> spirit_on_no_command s
              | _ -> ());
        (false, None));
    hl_init =
      (fun p ->
        player := Some p;
        Array.iter
          (fun slot ->
            let old = slot.add_hook in
            slot.add_hook <-
              (fun st ->
                old st;
                if slot.slots.player.pid = p.pid && (get_wind p).winged then (
                  Slot.toggle slot run_flag;
                  set_wind p { (get_wind p) with winged = false })))
          p.slots_update.slot_updates);
  }

let cards =
  [
    creature ~key:"wind.WingedWarrior.name" ~attack:(attack 4) ~life:9
      ~description:"wind.WingedWarrior.description"
      ~effects:(effects [ (Direct, winge) ])
      ();
    spell ~key:"wind.Simoom.name" ~description:"wind.Simoom.description"
      ~input_spec:(Select_target non_special)
      ~effects:(effects [ (Direct, simoom) ])
      ();
    spell ~key:"wind.BallLightning.name" ~description:"wind.BallLightning.description"
      ~effects:(effects [ (Direct, ball_lightning) ])
      ();
    spell ~key:"wind.Squall.name" ~description:"wind.Squall.description"
      ~effects:(effects [ (Direct, squall) ])
      ();
    spell ~key:"wind.EnergeticVortex.name" ~description:"wind.EnergeticVortex.description"
      ~effects:(effects [ (Direct, vortex) ])
      ();
    spirit;
    spell ~key:"wind.Whirlwind.name" ~description:"wind.Whirlwind.description"
      ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, whirlwind) ])
      ();
    spell ~key:"wind.EternalStorm.name" ~description:"wind.EternalStorm.description"
      ~effects:(effects [ (Direct, storm) ])
      ();
  ]

let house =
  {
    h_key = "wind";
    h_name = "wind";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 7;
    h_additional_cards = [];
    h_make_listener = wind_listener;
    h_data = Wind_state { winged = false; vortex = false };
  }
