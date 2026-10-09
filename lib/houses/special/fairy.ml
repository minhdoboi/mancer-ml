(* Fairy Knight special house — port of priv/sp/house/FairyKnight.scala. *)

open Sp
open Engine
open Common

let vodyanoy_ref : card option ref = ref None
let rusalka_ref : card option ref = ref None
let likho_ref : card option ref = ref None
let gory_ref : card option ref = ref None
let lesovik_ref : card option ref = ref None
let sirin_ref : card option ref = ref None
let thgory_ref : card option ref = ref None
let ohgory_ref : card option ref = ref None

let destroyed_mod (card : card) : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> card.house_index then cards
  else Array.of_list (List.filter (fun cd -> cd.cd_card.id <> card.id) (Array.to_list cards))

let destroyeds_mod (excls : card list) : desc_mod =
 fun _house cards ->
  Array.of_list
    (List.filter (fun cd -> not (List.exists (fun c -> c.id = cd.cd_card.id) excls)) (Array.to_list cards))

let sirin_bonus id = Attack_bonus { tag = "sirin"; pos = id; bonus = (fun a -> max 0 ((a + 1) / 2)) }
let sirin_temper id s = add_attack s (sirin_bonus id)
let sirin_untemper id s = remove_first_attack s (sirin_bonus id)

(* Scala SlotUpdate.drain: unblockable life loss, no protect/selfProtect. *)
let drain s d =
  match s.value with
  | None -> ()
  | Some st ->
      let newlife = st.life - d.amount in
      if newlife < 1 then Slot.delayed_destroy s d else Slot.update s (fun x -> { x with life = newlife });
      s.on_damage_hook st { d with amount = st.life - max newlife 0 }

let add_lesovik_mod lesovik_slot opp_slot =
  match opp_slot.value with
  | None -> ()
  | Some st ->
      let m = destroyed_mod st.card.c_card in
      Player.add_desc_mod opp_slot.slots.player m;
      Slot.set_data lesovik_slot (Slot_destroyed m)

let remove_lesovik_mod lesovik_slot =
  match lesovik_slot.value with
  | Some st -> (
      match st.data with
      | Slot_destroyed m -> Player.remove_desc_mod (Player.other lesovik_slot.slots.player) m
      | _ -> ())
  | None -> ()

let lesovik_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = p.slots_update.slot_updates.(env.env_selected) in
  add_lesovik_mod slot (Slot.opposite slot)

let lesovik_reaction () =
  let r = Reaction.default () in
  {
    r with
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        remove_lesovik_mod me);
  }

let lesovik_on_opp_add me opp_slot = if opp_slot.num = me.num then add_lesovik_mod me opp_slot
let lesovik_on_opp_remove me opp_slot = if opp_slot.num = me.num then remove_lesovik_mod me

let kiki : card_effect =
 fun env ->
  let p = player_of_env env in
  let opens = Slots.get_open p.slots_update in
  match (opens, !vodyanoy_ref) with
  | _ :: _, Some vodyanoy ->
      let num = List.nth opens (Dice.int env.env_updater.rng (List.length opens)) in
      let slot = p.slots_update.slot_updates.(num) in
      Slots.add_creature slot (make_creature vodyanoy);
      Updater.focus env.env_updater num env.env_player false
  | _ -> ()

let rusalka_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        if dead.d_player.pid <> me.slots.player.pid && dead.d_num = me.num then
          match dead.d_damage with
          | Some d -> (
              match (d.context.ctx_card, !rusalka_ref) with
              | Some c, Some r when c.id = r.id ->
                  Player.add_desc_mod dead.d_player (destroyed_mod dead.d_slot.card.c_card)
              | _ -> ())
          | None -> ());
  }

let likho_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        let ctx_card = !likho_ref in
        let ctx = { ctx_player = me.slots.player.pid; ctx_card; ctx_selected = me.num } in
        match dead.d_damage with
        | Some d when d.context.ctx_selected <> no_selection && not (is_effect d) -> (
            let opp = (Player.other me.slots.player).slots_update.slot_updates.(d.context.ctx_selected) in
            match opp.value with
            | None -> ()
            | Some st ->
                let l = st.life in
                drain opp (mk_damage ~is_ability:true (if l < 11 then l else l - 10) ctx))
        | _ -> Houses.incr_mana me.slots.player.houses_update 2 [ Dice.int me.slots.player.updater.rng 5 ]);
  }

let sirin_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  let id = env.env_selected in
  let slot = p.slots_update.slot_updates.(id) in
  let opp = Slot.opposite slot in
  if Option.is_some opp.value then sirin_temper id opp;
  List.iter (sirin_temper id) (Slot.filled_adjacent opp)

let sirin_reaction () =
  let r = Reaction.default () in
  {
    r with
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        let opp = Slot.opposite me in
        if Option.is_some opp.value then sirin_untemper me.num opp;
        List.iter (sirin_untemper me.num) (Slot.filled_adjacent opp));
  }

let sirin_in_range me_num s_num = abs (s_num - me_num) < 2
let sirin_on_opp_add me opp_slot = if sirin_in_range me.num opp_slot.num then sirin_temper me.num opp_slot

let sirin_on_opp_remove me opp_slot =
  if sirin_in_range me.num opp_slot.num then sirin_untemper me.num opp_slot

let take n xs =
  let rec go acc i = function
    | [] -> List.rev acc
    | x :: rest -> if i >= n then List.rev acc else go (x :: acc) (i + 1) rest
  in
  go [] 0 xs

let alko_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  let slot = p.slots_update.slot_updates.(env.env_selected) in
  let all =
    Array.to_list op.pstate.ps_desc.init_desc.pd_houses
    |> List.concat_map (fun h -> Array.to_list h.phd_cards |> List.map (fun cd -> cd.cd_card))
  in
  let pool =
    match slot.value with
    | Some st -> (
        match st.data with
        | Slot_destroyeds (olds, m) ->
            Player.remove_desc_mod op m;
            List.filter (fun c -> not (List.exists (fun o -> o.id = c.id) olds)) all
        | _ -> all)
    | None -> all
  in
  let excls = take 6 (Dice.shuffle env.env_updater.rng pool) in
  let m = destroyeds_mod excls in
  Player.add_desc_mod op m;
  Slot.set_data slot (Slot_destroyeds (excls, m))

let alko_reaction () =
  let r = Reaction.default () in
  {
    r with
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some st -> (
            match st.data with
            | Slot_destroyeds (_, m) -> Player.remove_desc_mod (Player.other me.slots.player) m
            | _ -> ())
        | None -> ());
  }

let firebird_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        let me = Reaction.bound_slot r in
        let has_fire =
          List.exists (fun s -> (Slot.get s).card.c_card.house_index = 0) (Slot.filled_adjacent me)
        in
        if has_fire then { d with amount = 0 } else d);
  }

let gory_on_summon me e =
  if e.s_card.c_card.house_index = 0 then
    let targets =
      Array.to_list (Player.other e.s_player).slots_update.slot_updates
      |> List.filter (fun s -> Option.is_some s.value)
    in
    match targets with
    | [] -> ()
    | ss ->
        let target = List.nth ss (Dice.int me.slots.player.updater.rng (List.length ss)) in
        Slot.inflict target
          (mk_damage ~is_ability:true 6
             { ctx_player = me.slots.player.pid; ctx_card = !gory_ref; ctx_selected = me.num })

let gory_reaction () =
  let r = Reaction.default () in
  { r with on_summon = (fun e -> gory_on_summon (Reaction.bound_slot r) e) }

let zgory_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon = (fun e -> gory_on_summon (Reaction.bound_slot r) e);
    destroy =
      (fun s ->
        match !thgory_ref with
        | Some c -> Slots.add_creature s (make_creature c)
        | None -> Slot.do_priv_destroy s);
    on_my_death =
      (fun dead ->
        match !thgory_ref with
        | Some c -> (
            match dead.d_player.slots_update.slot_updates.(dead.d_num).value with
            | None ->
                Slots.add_creature dead.d_player.slots_update.slot_updates.(dead.d_num) (make_creature c)
            | Some _ -> ())
        | None -> ());
  }

let thgory_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon = (fun e -> gory_on_summon (Reaction.bound_slot r) e);
    destroy =
      (fun s ->
        match !ohgory_ref with
        | Some c -> Slots.add_creature s (make_creature c)
        | None -> Slot.do_priv_destroy s);
    on_my_death =
      (fun dead ->
        match !ohgory_ref with
        | Some c -> (
            match dead.d_player.slots_update.slot_updates.(dead.d_num).value with
            | None ->
                Slots.add_creature dead.d_player.slots_update.slot_updates.(dead.d_num) (make_creature c)
            | Some _ -> ())
        | None -> ());
  }

let is_lesovik st = match !lesovik_ref with Some c -> st.card.c_card.id = c.id | None -> false
let is_sirin st = match !sirin_ref with Some c -> st.card.c_card.id = c.id | None -> false

let fairy_listener () =
  let player = ref None in
  let get () = match !player with Some p -> p | None -> failwith "fairy listener not inited" in
  {
    hl_on_death = (fun () -> true);
    hl_mod = Fun.id;
    hl_intercept_submit = (fun c -> (false, c));
    hl_init =
      (fun p ->
        player := Some p;
        ignore (get ());
        let opp = p.updater.player_updates.(other p.pid) in
        (* OppDeath: rusalka watches opponent deaths. *)
        let old_dead = opp.slots_update.on_dead_hook in
        opp.slots_update.on_dead_hook <-
          (fun dead ->
            old_dead dead;
            Array.iter
              (fun s ->
                match s.value with
                | Some st when st.card.c_card.house_index = 4 -> st.reaction.on_death dead
                | _ -> ())
              p.slots_update.slot_updates);
        (* OnOppSlotUpdate: lesovik and sirin watch opponent adds/removes. *)
        Array.iter
          (fun slot ->
            let old_add = slot.add_hook in
            slot.add_hook <-
              (fun st ->
                old_add st;
                Array.iter
                  (fun s ->
                    match s.value with
                    | Some v when is_lesovik v -> lesovik_on_opp_add s slot
                    | Some v when is_sirin v -> sirin_on_opp_add s slot
                    | _ -> ())
                  p.slots_update.slot_updates);
            let old_remove = slot.remove_hook in
            slot.remove_hook <-
              (fun dead ->
                Array.iter
                  (fun s ->
                    match s.value with
                    | Some v when is_lesovik v -> lesovik_on_opp_remove s slot
                    | Some v when is_sirin v -> sirin_on_opp_remove s slot
                    | _ -> ())
                  p.slots_update.slot_updates;
                old_remove dead))
          opp.slots_update.slot_updates);
  }

let vodyanoy = creature ~key:"fairy.vodyanoy" ~attack:(attack 4) ~life:11 ~description:"" ()

let rusalka =
  creature ~key:"fairy.rusalka" ~attack:(attack 6) ~life:19 ~description:"fairy.rusalka.description"
    ~new_reaction:rusalka_reaction ()

let likho =
  creature ~key:"fairy.likho" ~attack:(attack 6) ~life:21 ~description:"fairy.likho.description"
    ~new_reaction:likho_reaction ()

let gory =
  creature ~key:"fairy.zmey" ~attack:(attack 9) ~life:35 ~description:"fairy.zmey.description"
    ~new_reaction:zgory_reaction ()

let thgorynych =
  creature ~key:"fairy.2head" ~attack:(attack 6) ~life:25 ~description:"" ~new_reaction:thgory_reaction ()

let ohgorynych =
  creature ~key:"fairy.1head" ~attack:(attack 3) ~life:15 ~description:"" ~new_reaction:gory_reaction ()

let lesovik =
  creature ~key:"fairy.lesovik" ~attack:(attack 4) ~life:14 ~description:"fairy.lesovik.description"
    ~effects:(effects [ (Direct, lesovik_effect) ])
    ~new_reaction:lesovik_reaction ()

let kikimora =
  creature ~key:"fairy.kikimora" ~attack:(attack 3) ~life:13 ~description:"fairy.kikimora.description"
    ~effects:(effects [ (Direct, kiki) ])
    ()

let sirin =
  creature ~key:"fairy.sirin" ~attack:(attack 4) ~life:23 ~description:"fairy.sirin.description"
    ~effects:(effects [ (Direct, sirin_effect) ])
    ~new_reaction:sirin_reaction ()

let alkonost =
  creature ~key:"fairy.alkonost" ~attack:(attack 6) ~life:31 ~description:"fairy.alkonost.description"
    ~effects:(effects [ (Direct, alko_effect); (On_turn, alko_effect) ])
    ~new_reaction:alko_reaction ()

let firebird =
  creature ~key:"fairy.firebird" ~attack:(attack 8) ~life:32 ~description:"fairy.firebird.description"
    ~new_reaction:firebird_reaction ()

let () =
  vodyanoy_ref := Some vodyanoy;
  rusalka_ref := Some rusalka;
  likho_ref := Some likho;
  gory_ref := Some gory;
  lesovik_ref := Some lesovik;
  sirin_ref := Some sirin;
  thgory_ref := Some thgorynych;
  ohgory_ref := Some ohgorynych;
  vodyanoy.cost <- 1;
  thgorynych.cost <- 8;
  ohgorynych.cost <- 8

let cards = [ lesovik; kikimora; rusalka; likho; sirin; alkonost; firebird; gory ]

let house =
  {
    h_key = "fairy";
    h_name = "fairy";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 12;
    h_additional_cards = [ vodyanoy; thgorynych; ohgorynych ];
    h_make_listener = fairy_listener;
    h_data = No_player_data;
  }
