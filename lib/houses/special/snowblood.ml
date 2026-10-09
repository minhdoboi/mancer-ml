(* Special house Snowblood — port of priv/sp/house/SB.scala. *)

open Sp
open Engine
open Common

let is_card opt (c : card) = match opt with Some x -> x.id = c.id | None -> false
let bounty_ref : card option ref = ref None
let maiko_ref : card option ref = ref None
let guide_ref : card option ref = ref None
let amaterasu_ref : card option ref = ref None
let slot_of_env env = (player_of_env env).slots_update.slot_updates.(env.env_selected)

(* Lower1Attack(id) *)
let lower1 pos = Attack_bonus { tag = "lower1"; pos; bonus = (fun a -> max 0 (a - 1)) }

(* Neighbours of a slot: the run of filled slots around [selected] (empty if [selected] is empty). Scala
   returns the slots from the far end back to the first, which this keeps. *)
let get_aligneds (su : slots_update) selected =
  if Option.is_none su.slot_updates.(selected).value then []
  else
    let filled n = in_slot_range n && Option.is_some su.slot_updates.(n).value in
    let rec first n = if filled (n - 1) then first (n - 1) else n in
    let rec last n = if filled (n + 1) then last (n + 1) else n in
    List.rev_map (fun n -> su.slot_updates.(n)) (slot_interval (first selected) (last selected))

(* ---------------------------------------------------------------- tracker *)

let tracker_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if e.s_player.pid = me.slots.player.pid && List.mem e.s_num (adjacents me.num) then
          let slot = me.slots.slot_updates.(e.s_num) in
          match slot.value with
          | Some st when not (has_flag st invincible_flag) ->
              Slot.toggle slot invincible_flag;
              Player.add_effect slot.slots.player
                (On_turn, one_time_player_effect (remove_invincible st.slot_id))
          | _ -> ());
  }

(* ----------------------------------------------------------------- hunter *)

let bounty_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        if dead.d_player.pid <> me.slots.player.pid then
          Option.iter
            (fun (d : damage) ->
              match d.context.ctx_card with
              | Some c when is_card !bounty_ref c && d.context.ctx_selected = me.num ->
                  let card = dead.d_slot.card.c_card in
                  Houses.incr_mana me.slots.player.houses_update ((card.cost + 2) / 3) [ card.house_index ]
              | _ -> ())
            dead.d_damage);
  }

let bounty_strike : card_effect =
 fun env ->
  Updater.focus env.env_updater env.env_selected env.env_player true;
  Slot.inflict
    (other_of_env env).slots_update.slot_updates.(env.env_selected)
    (mk_damage ~is_ability:true 2 (ctx_of_env env))

(* ------------------------------------------------------------------ maiko *)

(* Maiko weakens the opposite enemy and the two enemies beside it, as long as she lives. *)
let maiko_targets num = num :: adjacents num

let maiko_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.slots.player.pid <> me.slots.player.pid && List.mem slot.num (maiko_targets me.num) then
          add_attack slot (lower1 me.num));
    clean_up =
      (fun () ->
        let me = Reaction.bound_slot r in
        Slots.foreach (other_player me.slots.player).slots_update (fun s ->
            remove_first_attack s (lower1 me.num)));
  }

let letter =
  let reaction () =
    let r = Reaction.default () in
    {
      r with
      heal = (fun _ _ -> ());
      inflict = (fun s d -> Slot.damage_slot s { d with amount = 1 });
      on_my_death =
        (fun dead ->
          let me = Reaction.bound_slot r in
          Slot.inflict (Slot.opposite me)
            (mk_damage ~is_ability:true 15
               {
                 ctx_player = me.slots.player.pid;
                 ctx_card = Some dead.d_slot.card.c_card;
                 ctx_selected = me.num;
               }));
    }
  in
  creature ~key:"snowblood.letter" ~attack:(attack 0) ~life:3 ~description:"snowblood.letter.description"
    ~new_reaction:reaction ()

(* After her summon, Maiko's card is replaced by the Death letter. *)
let maiko_ability () =
  match !maiko_ref with
  | Some m -> ability ~from_card:m ~to_card:letter
  | None -> failwith "snowblood not initialised"

let maiko_effect : card_effect =
 fun env ->
  let op = other_of_env env in
  List.iter
    (fun n ->
      let s = op.slots_update.slot_updates.(n) in
      if Option.is_some s.value then add_attack s (lower1 env.env_selected))
    (maiko_targets env.env_selected);
  Player.add_desc_mod (player_of_env env) (maiko_ability ())

let maiko_heal : card_effect =
 fun env ->
  let slot = slot_of_env env in
  match Slot.filled_adjacent slot with
  | [] -> ()
  | adjs ->
      focus_slot slot;
      List.iter (fun s -> Slot.heal s 1) adjs

let maiko =
  creature ~key:"snowblood.maiko" ~attack:(attack 2) ~life:13 ~description:"snowblood.maiko.description"
    ~new_reaction:maiko_reaction
    ~effects:(effects [ (Direct, maiko_effect); (On_turn, maiko_heal) ])
    ()

(* ------------------------------------------------------------------- echo *)

let echo : card_effect =
 fun env ->
  let p = player_of_env env in
  Slots.foreach p.slots_update (fun s ->
      let c = (Slot.get s).card.c_card in
      List.iter
        (fun phase ->
          Option.iter
            (fun f ->
              let e = mk_env env.env_player env.env_updater in
              e.env_card <- Some c;
              e.env_selected <- s.num;
              f e)
            (effect_at c.effects phase))
        [ On_turn; Direct ]);
  (* then every Snowblood creature attacks right away, outside the attack phase *)
  Slots.foreach p.slots_update (fun s ->
      match s.value with Some st when st.card.c_card.house_index = 4 -> Player.run_slot p s.num st | _ -> ())

(* ----------------------------------------------------------------- kojiro *)

(* +4 attack on the turn Kojiro is summoned (5 in all), removed once that turn's attacks are done. *)
let kojiro_rush : card_effect =
 fun env ->
  let bonus = Attack_func (fun a -> a + 4) in
  add_attack (slot_of_env env) bonus;
  Player.add_effect (player_of_env env) (On_end_turn, one_time_player_effect (remove_attack bonus))

let kojiro_turn : card_effect =
 fun env ->
  match get_aligneds (other_of_env env).slots_update env.env_selected with
  | [] -> ()
  | aligneds ->
      focus_slot (slot_of_env env);
      let d = mk_damage ~is_ability:true 2 (ctx_of_env env) in
      List.iter (fun s -> Slot.inflict s d) aligneds

(* ------------------------------------------------------------------ guide *)

let guide_used st = match st.data with Custom "used" -> true | _ -> false

(* Scala keeps a Boolean in the slot data: No_data is "not yet", Custom "used" is "done". *)
let guide_summoned (me : slot_update) (slot : slot_update) =
  if me != slot then (
    (match me.value with
    | Some mst when not (guide_used mst) ->
        Option.iter
          (fun s ->
            let d =
              mk_damage ~is_ability:true s.attack
                { ctx_player = slot.slots.player.pid; ctx_card = Some s.card.c_card; ctx_selected = slot.num }
            in
            Slots.inflict_creatures (other_player me.slots.player).slots_update d;
            Slot.set_data me (Custom "used"))
          slot.value
    | _ -> ());
    let aligneds = get_aligneds me.slots me.num in
    if List.exists (fun s -> s == slot) aligneds then List.iter (fun s -> Slot.heal s 1) aligneds)

(* ------------------------------------------------------------------ janus *)

(* Slots 0-2 and 3-5 are the two sides of the board. *)
let janus_other_side num = if num > 2 then [ 0; 1; 2 ] else [ 3; 4; 5 ]
let janus_same_side_others num = List.filter (( <> ) num) (if num > 2 then [ 3; 4; 5 ] else [ 0; 1; 2 ])

let janus_on_summon : card_effect =
 fun env ->
  let p = player_of_env env in
  List.iter
    (fun n ->
      Option.iter
        (fun st -> Houses.incr_mana p.houses_update 1 [ st.card.c_card.house_index ])
        p.slots_update.slot_updates.(n).value)
    (janus_same_side_others env.env_selected);
  Player.heal p 2;
  focus_slot (slot_of_env env)

let janus_turn : card_effect =
 fun env ->
  let p = player_of_env env in
  let d = mk_damage ~is_ability:true 2 (ctx_of_env env) in
  let hit = ref false in
  List.iter
    (fun n ->
      let s = p.slots_update.slot_updates.(n) in
      if Option.is_some s.value then (
        Slot.inflict s d;
        hit := true))
    (janus_other_side env.env_selected);
  if !hit then focus_slot (slot_of_env env)

(* -------------------------------------------------------------- amaterasu *)

(* Amaterasu reacts to the element of what the owner summons; [slot] is the summoned creature's slot *)
let apply_amaterasu (me : slot_update) (slot : slot_update) house_index =
  let p = me.slots.player in
  let ctx =
    { ctx_player = p.pid; ctx_card = Option.map (fun st -> st.card.c_card) me.value; ctx_selected = me.num }
  in
  match house_index with
  | 0 -> Slot.inflict (Slot.opposite slot) (mk_damage ~is_ability:true 4 ctx)
  | 1 ->
      (* the lowest power, the first one on ties *)
      let houses = p.houses_update.hu_value in
      let lowest = ref 0 in
      Array.iteri (fun i h -> if h.mana < houses.(!lowest).mana then lowest := i) houses;
      Houses.incr_mana p.houses_update 1 [ !lowest ]
  | 2 -> Player.inflict (other_player p) (mk_damage ~is_ability:true 2 ctx)
  | 3 -> Player.heal p 2
  | _ -> ()

let apply_amaterasu_slot me (slot : slot_update) =
  (* the summon can already be gone (Ballista, etc.) *)
  Option.iter (fun st -> apply_amaterasu me slot st.card.c_card.house_index) slot.value

let amaterasu : card_effect =
 fun env ->
  let me = slot_of_env env in
  Slots.foreach (player_of_env env).slots_update (fun s ->
      match s.value with
      | Some st when st.card.c_card.house_index < 4 ->
          focus_slot s;
          apply_amaterasu_slot me s
      | _ -> ())

(* --------------------------------------------------------------- listener *)

let snowblood_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        init_opp_death p;
        let op = other_player p in
        (* a creature that lands on the enemy side: Maiko reacts to it *)
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.add_hook in
            slot.add_hook <-
              (fun st ->
                old st;
                Slots.foreach p.slots_update (fun s ->
                    match s.value with
                    | Some m when is_card !maiko_ref m.card.c_card -> m.reaction.on_add slot
                    | _ -> ())))
          op.slots_update.slot_updates;
        (* a creature the owner submits *)
        let old = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old c in
            (if not c.cmd_card.is_spell then
               match c.cmd_input with
               | Some num ->
                   let slot = p.slots_update.slot_updates.(num) in
                   Slots.foreach p.slots_update (fun s ->
                       match s.value with
                       | Some st when is_card !amaterasu_ref st.card.c_card ->
                           apply_amaterasu s slot c.cmd_card.house_index
                       | Some st when is_card !guide_ref st.card.c_card -> guide_summoned s slot
                       | _ -> ())
               | None -> ());
            r));
  }

let cards =
  let tracker =
    creature ~key:"snowblood.tracker" ~attack:(attack 3) ~life:14 ~description:"snowblood.tracker.description"
      ~new_reaction:tracker_reaction ()
  in
  let bounty =
    creature ~key:"snowblood.hunter" ~attack:(attack 4) ~life:16 ~description:"snowblood.hunter.description"
      ~new_reaction:bounty_reaction
      ~effects:(effects [ (Direct, bounty_strike) ])
      ()
  in
  let guide =
    creature ~key:"snowblood.guide" ~attack:(attack 5) ~life:26 ~description:"snowblood.guide.description" ()
  in
  let amat =
    creature ~key:"snowblood.amaterasu" ~attack:(attack 7) ~life:30
      ~description:"snowblood.amaterasu.description"
      ~effects:(effects [ (Direct, amaterasu) ])
      ()
  in
  bounty_ref := Some bounty;
  maiko_ref := Some maiko;
  guide_ref := Some guide;
  amaterasu_ref := Some amat;
  [
    tracker;
    bounty;
    maiko;
    spell ~key:"snowblood.echo" ~description:"snowblood.echo.description"
      ~effects:(effects [ (Direct, echo) ])
      ();
    creature ~key:"snowblood.kojiro" ~attack:(attack 5) ~life:27 ~description:"snowblood.kojiro.description"
      ~status:run_flag
      ~input_spec:(Select_owner (fun pid st -> st.players.(pid).ps_slot_list))
      ~effects:(effects [ (Direct, kojiro_rush); (On_turn, kojiro_turn) ])
      ();
    guide;
    creature ~key:"snowblood.janus" ~attack:(attack 6) ~life:25 ~description:"snowblood.janus.description"
      ~effects:(effects [ (Direct, janus_on_summon); (On_turn, janus_turn) ])
      ();
    amat;
  ]

let () = letter.cost <- 3

let house =
  {
    h_key = "snowblood";
    h_name = "snowblood";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 16;
    h_additional_cards = [ letter ];
    h_make_listener = snowblood_listener;
    h_data = No_player_data;
  }
