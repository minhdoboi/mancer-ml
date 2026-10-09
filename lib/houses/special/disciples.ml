(* Disciples / Lost Church special house — port of priv/sp/house/LostChurch.scala. *)

open Sp
open Engine
open Common

let liberator_life = 15
let prisoner_ref : card option ref = ref None
let enraged_ref : card option ref = ref None
let preacher_ref : card option ref = ref None
let scarecrow_ref : card option ref = ref None
let dark_monk_ref : card option ref = ref None
let astral_ref : card option ref = ref None
let liberator_ref : card option ref = ref None
let preacher_bonus = Attack_bonus { tag = "preacher"; pos = 0; bonus = (fun a -> a + 1) }
let madden_bonus = Attack_bonus { tag = "madden"; pos = 0; bonus = (fun a -> a + 1) }
let lc_bonus pid = Attack_bonus { tag = "lc_bonus"; pos = pid; bonus = (fun a -> a + 1) }
let lc_weaken half = Attack_bonus { tag = "lc_weaken"; pos = 0; bonus = (fun a -> max 0 (a + half)) }
let is_prisoner_kind (c : card) = is_card !prisoner_ref c || is_card !enraged_ref c
let get_prisoner_slot su = Slots.find su (fun st -> is_prisoner_kind st.card.c_card)

let open_or_prisoner pid (state : game_state) =
  let p = state.players.(pid) in
  let filled = Array.fold_left (fun n -> function Some _ -> n + 1 | None -> n) 0 p.ps_slots in
  if filled = List.length p.ps_slot_list then []
  else
    p.ps_slot_list
    |> List.filter (fun i ->
        match p.ps_slots.(i) with None -> true | Some s -> is_card !prisoner_ref s.card.c_card)

let give_hope p =
  Option.iter
    (fun slot -> if not (has_source slot (same_source preacher_bonus)) then add_attack slot preacher_bonus)
    (get_prisoner_slot p.slots_update)

let spawn_prisoner : card_effect =
 fun env ->
  let p = player_of_env env in
  match (!prisoner_ref, get_prisoner_slot p.slots_update) with
  | Some prisoner, None -> (
      match Slots.get_open p.slots_update with
      | [] -> ()
      | opens ->
          let slot =
            p.slots_update.slot_updates.(List.nth opens (Dice.int env.env_updater.rng (List.length opens)))
          in
          Slots.add_creature slot (make_creature prisoner);
          (match !preacher_ref with
          | Some preacher when Option.is_some (Slots.find_card p.slots_update preacher) ->
              add_attack slot preacher_bonus
          | _ -> ());
          Updater.focus env.env_updater slot.num p.pid false)
  | _ -> ()

let weaken : card_effect =
 fun env ->
  let p = player_of_env env in
  let hid =
    p.pstate.ps_desc.init_desc.pd_houses |> Array.to_seq
    |> Seq.find_map (fun h ->
        if h.phd_house.h_index = special_house_index then Some h.phd_house.h_house_id else None)
    |> Option.value ~default:(-1)
  in
  Slots.foreach p.slots_update (fun s ->
      let st = Slot.get s in
      let body = st.card.c_body in
      if
        st.card.c_card.house_id = hid
        && st.life < body.b_life / 2
        && not (has_source s (function Attack_bonus b -> b.tag = "lc_weaken" | _ -> false))
      then
        match body.b_attack.base with
        | Some base -> add_attack s (lc_weaken (-int_of_float (ceil (float_of_int base /. 3.))))
        | None -> ())

let preacher_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num then (
          Player.add_desc_mod me.slots.player incr_basic_cost_mod;
          give_hope me.slots.player));
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        Option.iter
          (fun slot ->
            match !preacher_ref with
            | Some preacher when Option.is_none (Slots.find_card me.slots preacher) ->
                remove_any_attack slot preacher_bonus
            | _ -> ())
          (get_prisoner_slot me.slots);
        Player.remove_desc_mod me.slots.player incr_basic_cost_mod);
  }

(* filled in after dark_monk / false_prophet exist *)
let prophet_ability = ref (fun (_ : env) -> ())
let scarecrow_ability_mod : desc_mod ref = ref (fun _ cards -> cards)

let prophetize_full : card_effect =
 fun env ->
  let p = player_of_env env in
  Houses.incr_mana p.houses_update 2 [ 0; 1; 2; 3 ];
  Player.add_desc_mod p incr_basic_cost_mod;
  !prophet_ability env

let scare : card_effect =
 fun env ->
  let slot = opp_slot_of_env env in
  if Option.is_some slot.value then (
    Updater.focus env.env_updater env.env_selected env.env_player true;
    Slot.inflict slot (ability_damage env 4);
    Slot.stun slot);
  Player.add_desc_mod (player_of_env env) !scarecrow_ability_mod

let oppress : card_effect =
 fun env ->
  match !scarecrow_ref with
  | None -> ()
  | Some scarecrow ->
      Option.iter
        (fun slot ->
          List.iter
            (fun n ->
              let opp = (other_of_env env).slots_update.slot_updates.(n) in
              if Option.is_some opp.value then (
                Slot.inflict opp (ability_damage env 4);
                Slot.toggle opp stun_flag))
            (slot_interval (slot.num - 1) (slot.num + 1)))
        (Slots.find_card (player_of_env env).slots_update scarecrow)

let scarecrow_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_spawn_over =
      (fun s ->
        Option.iter
          (fun _ -> Option.iter (fun n -> Slots.move s.slots s.num n) (nearest_empty s.num s.slots.player))
          s.value;
        None);
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        Player.remove_desc_mod me.slots.player !scarecrow_ability_mod);
  }

let deliver_prisoner : card_effect =
 fun env ->
  let p = player_of_env env in
  match (!enraged_ref, get_prisoner_slot p.slots_update) with
  | Some enraged, Some slot ->
      Slot.destroy slot;
      Slots.add_creature slot (make_creature enraged)
  | _ -> ()

let speed_drug : card_effect =
 fun env ->
  let p = player_of_env env in
  let bonus = lc_bonus p.pid in
  Slots.foreach p.slots_update (fun s -> add_attack s bonus);
  Slots.inflict_creatures p.slots_update (spell_damage env 4)

let madden : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  Slots.foreach op.slots_update (fun slot ->
      Slot.inflict slot (spell_damage env 8);
      if Option.is_some slot.value then add_attack slot madden_bonus else Player.heal p 3);
  Slots.foreach p.slots_update (fun s -> add_attack s madden_bonus)

let falcon : card_effect =
 fun env ->
  Updater.focus env.env_updater env.env_selected env.env_player true;
  Slots.foreach (other_of_env env).slots_update (fun slot ->
      if slot.num <> env.env_selected then
        Slot.inflict slot (ability_damage env (abs (slot.num - env.env_selected))))

let liberator_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        match !enraged_ref with
        | None -> ()
        | Some enraged ->
            Option.iter
              (fun slot ->
                Slot.inflict slot
                  (mk_damage liberator_life
                     { ctx_player = dead.d_player.pid; ctx_card = None; ctx_selected = no_selection }))
              (Slots.find_card dead.d_player.slots_update enraged));
    on_protect =
      (fun e ->
        match e.de_target with
        | Some n when n <> (Reaction.bound_slot r).num && is_effect e.de_damage -> (
            match e.de_player.slots_update.slot_updates.(n).value with
            | Some st when is_card !enraged_ref st.card.c_card -> { e.de_damage with amount = 0 }
            | _ -> e.de_damage)
        | _ -> e.de_damage);
  }

let prisoner_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        let p = dead.d_player in
        (match !liberator_ref with
        | Some lib when Option.is_none (Slots.find_card p.slots_update lib) ->
            let mana i = p.houses_update.hu_value.(i).mana in
            let highs =
              [ 0; 1; 2; 3 ] |> List.sort (fun a b -> compare (mana a) (mana b)) |> fun xs ->
              match xs with _ :: _ :: rest -> rest | rest -> rest
            in
            Houses.incr_mana p.houses_update (-1) highs;
            Houses.incr_mana p.houses_update 1 [ 4 ]
        | _ -> ());
        let bonus = lc_bonus p.pid in
        Slots.foreach p.slots_update (fun s -> remove_any_attack s bonus));
  }

let prisoner_attack target d player =
  match target with
  | [] -> ()
  | num :: _ ->
      let op = Player.other player in
      let escaped =
        match !astral_ref with
        | Some a -> Option.is_some (Slots.find_card player.slots_update a)
        | None -> false
      in
      if escaped then Player.inflict op d
      else
        let slot = op.slots_update.slot_updates.(num) in
        if Option.is_some slot.value then Slot.inflict slot d else Player.inflict op d

let astral_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun e ->
        match e.de_target with
        | Some n -> (
            match e.de_player.slots_update.slot_updates.(n).value with
            | Some st when is_prisoner_kind st.card.c_card ->
                Slot.inflict (Reaction.bound_slot r) e.de_damage;
                { e.de_damage with amount = 0 }
            | _ -> e.de_damage)
        | None -> e.de_damage);
  }

let dark_monk_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num then Player.add_desc_mod (Player.other me.slots.player) incr_fire_cost_mod);
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        Player.remove_desc_mod (Player.other me.slots.player) incr_fire_cost_mod);
  }

let false_prophet_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death = (fun dead -> Houses.incr_mana dead.d_player.houses_update (-1) [ 0; 1; 2; 3 ]);
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        Player.remove_desc_mod me.slots.player incr_basic_cost_mod);
  }

let lc_listener () =
  let player = ref None in
  let get () = match !player with Some p -> p | None -> failwith "disciples listener not inited" in
  let protect slot d =
    match slot.value with
    | Some st when is_prisoner_kind st.card.c_card ->
        Slots.foldl (get ()).slots_update d (fun acc s ->
            let sc = (Slot.get s).card.c_card in
            if is_card !astral_ref sc || is_card !liberator_ref sc then
              s.value
              |> Option.map (fun v ->
                  v.reaction.on_protect { de_damage = acc; de_target = Some slot.num; de_player = get () })
              |> Option.value ~default:acc
            else acc)
    | _ -> d
  in
  {
    hl_on_death = (fun () -> true);
    hl_mod = Fun.id;
    hl_intercept_submit = (fun c -> (false, c));
    hl_init =
      (fun p ->
        player := Some p;
        Array.iter
          (fun slot ->
            let old = slot.protect_hook in
            slot.protect_hook <- (fun d -> protect slot (old d)))
          p.slots_update.slot_updates);
  }

let prisoner =
  creature ~key:"disciples.prisoner" ~attack:(attack 2) ~life:10 ~description:"disciples.prisoner.description"
    ~new_reaction:prisoner_reaction ~run_attack:(custom_run_attack prisoner_attack) ()

let enraged =
  creature ~key:"disciples.enraged" ~attack:(attack 6) ~life:35 ~description:"disciples.enraged.description"
    ~new_reaction:prisoner_reaction ~status:run_flag ~run_attack:(custom_run_attack prisoner_attack) ()

let wind_of_oppression =
  spell ~key:"disciples.wind" ~description:"disciples.wind.description"
    ~effects:(effects [ (Direct, oppress) ])
    ()

let guard_fire : card_effect = fun env -> Houses.incr_mana (other_of_env env).houses_update (-2) [ 0 ]

let dark_monk =
  creature ~key:"disciples.monk" ~attack:(attack 2) ~life:13 ~description:"disciples.monk.description"
    ~effects:(effects [ (Direct, guard_fire) ])
    ~new_reaction:dark_monk_reaction ()

let preacher =
  creature ~key:"disciples.preacher" ~attack:(attack 4) ~life:13 ~description:"disciples.preacher.description"
    ~effects:(effects [ (On_turn, add_mana 1 [ 4 ]) ])
    ~new_reaction:preacher_reaction ()

let false_prophet =
  creature ~key:"disciples.prophet" ~attack:(attack 4) ~life:14 ~description:"disciples.prophet.description"
    ~new_reaction:false_prophet_reaction
    ~effects:(effects [ (Direct, prophetize_full) ])
    ()

let astral_escape =
  creature ~key:"disciples.escape" ~attack:(attack 4) ~life:30 ~description:"disciples.escape.description"
    ~new_reaction:astral_reaction ()

let scarecrow =
  creature ~key:"disciples.scarecrow" ~attack:(attack 7) ~life:25
    ~description:"disciples.scarecrow.description"
    ~effects:(effects [ (Direct, scare) ])
    ~input_spec:(Select_owner open_or_prisoner) ~new_reaction:scarecrow_reaction ()

let liberator =
  creature ~key:"disciples.liberator" ~attack:(attack 3) ~life:liberator_life
    ~description:"disciples.liberator.description"
    ~desc_args:(fun _ _ -> [ string_of_int liberator_life ])
    ~new_reaction:liberator_reaction
    ~effects:(effects [ (Direct, focus deliver_prisoner) ])
    ()

let falconer =
  creature ~key:"disciples.falconer" ~attack:(attack 6) ~life:30 ~description:"disciples.falconer.description"
    ~effects:(effects [ (On_turn, focus falcon) ])
    ()

let () =
  prisoner_ref := Some prisoner;
  enraged_ref := Some enraged;
  preacher_ref := Some preacher;
  scarecrow_ref := Some scarecrow;
  dark_monk_ref := Some dark_monk;
  astral_ref := Some astral_escape;
  liberator_ref := Some liberator;
  prisoner.cost <- 1;
  enraged.cost <- 4;
  wind_of_oppression.cost <- 4;
  wind_of_oppression.card_index <- 4;
  dark_monk.cost <- 3;
  dark_monk.card_index <- 3;
  prophet_ability := add_desc_mod (ability ~from_card:false_prophet ~to_card:dark_monk);
  scarecrow_ability_mod := ability ~from_card:scarecrow ~to_card:wind_of_oppression

let cards =
  [
    spell ~key:"disciples.drug" ~description:"disciples.drug.description"
      ~effects:(effects [ (Direct, speed_drug) ])
      ();
    preacher;
    false_prophet;
    astral_escape;
    scarecrow;
    liberator;
    falconer;
    spell ~key:"disciples.madden" ~description:"disciples.madden.description"
      ~effects:(effects [ (Direct, madden) ])
      ();
  ]

let house =
  special_house ~key:"disciples" ~id:9
    ~effects:[ (On_end_turn, spawn_prisoner); (On_turn, weaken) ]
    ~additional_cards:[ prisoner; enraged; wind_of_oppression; dark_monk ]
    ~listener:lc_listener cards
