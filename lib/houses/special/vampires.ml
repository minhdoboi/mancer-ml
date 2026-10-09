(* Special house Vampires — port of priv/sp/house/Vampire.scala. *)

open Sp
open Engine
open Common

let is_card opt (c : card) = match opt with Some x -> x.id = c.id | None -> false
let acolyte_ref : card option ref = ref None
let ghoul_ref : card option ref = ref None
let aristocrat_ref : card option ref = ref None
let neophyte_ref : card option ref = ref None
let creature_of opt = match opt with Some c -> make_creature c | None -> failwith "vampires not initialised"

(* Spell: the owner's weakest creature flies to the target slot (and is invulnerable for a turn if cheap) *)
let dark_flock : card_effect =
 fun env ->
  let p = player_of_env env in
  match reduce_filled p.slots_update lowest_life with
  | None -> ()
  | Some s ->
      let card = (Slot.get s).card.c_card in
      if s.num <> env.env_selected then
        emit env.env_updater
          (Ev_ability
             {
               kind = "DarkFlockBats";
               player = env.env_player;
               num = s.num;
               target = Some (env.env_player, env.env_selected);
             });
      Slots.move p.slots_update s.num env.env_selected;
      if card.cost < 10 then (
        let slot = p.slots_update.slot_updates.(env.env_selected) in
        Slot.toggle slot invincible_flag;
        Player.add_effect p (On_turn, one_time_player_effect (remove_invincible (Slot.get slot).slot_id)))

let blood_ties : card_effect =
 fun env ->
  let slot = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
  let attack = (Slot.get slot).attack in
  let adjacent = Slot.filled_adjacent slot in
  Slot.destroy slot;
  List.iter
    (fun s ->
      match s.value with
      | Some st
        when st.card.c_card.cost < 9
             && match st.card.c_body.b_run_attack.apply_ with Multi_target -> false | _ -> true ->
          add_attack s (Attack_func (fun a -> a + attack))
      | _ -> ())
    adjacent

(* Starts of turn: jump in front of the enemy with the least life *)
let aristo : card_effect =
 fun env ->
  let p = player_of_env env in
  match reduce_filled (other_of_env env).slots_update lowest_life with
  | Some s when s.num <> env.env_selected ->
      let occupied_by_aristocrat =
        match p.slots_update.slot_updates.(s.num).value with
        | Some st -> is_card !aristocrat_ref st.card.c_card
        | None -> false
      in
      if not occupied_by_aristocrat then Slots.move p.slots_update env.env_selected s.num
  | _ -> ()

let ghoulify : card_effect =
 fun env ->
  let slot = (player_of_env env).slots_update.slot_updates.(env.env_selected) in
  List.iter
    (fun s ->
      if Option.is_some s.value then (
        Slot.destroy s;
        Slots.add_creature s (creature_of !ghoul_ref)))
    (Slot.adjacent slot)

let mansion_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        if me.slots.player.pid = dead.d_player.pid && dead.d_slot.card.c_card.house_index < 4 then
          Slots.add_creature dead.d_player.slots_update.slot_updates.(dead.d_num) (creature_of !neophyte_ref));
  }

(* Acolyte.onDamaged: an enemy creature that took more than 5 gets 4 more *)
let acolyte_hit (acolyte_slot : slot_update) (victim : slot_update) (d : damage) =
  if Option.is_some victim.value && d.amount > 5 then (
    Slot.inflict victim
      (mk_damage ~is_ability:true 4
         {
           ctx_player = acolyte_slot.slots.player.pid;
           ctx_card = !acolyte_ref;
           ctx_selected = acolyte_slot.num;
         });
    focus_slot ~blocking:false acolyte_slot)

let vampire_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        init_any_death p;
        Array.iter
          (fun (victim : slot_update) ->
            let old = victim.on_damage_hook in
            victim.on_damage_hook <-
              (fun st d ->
                old st d;
                Array.iter
                  (fun s ->
                    match s.value with
                    | Some a when is_card !acolyte_ref a.card.c_card && not (is_bridled a) ->
                        acolyte_hit s victim d
                    | _ -> ())
                  p.slots_update.slot_updates))
          (other_player p).slots_update.slot_updates);
  }

let noctule_attack target d player =
  let num = List.hd target in
  let op = other_player player in
  let slot = op.slots_update.slot_updates.(num) in
  match slot.value with
  | None ->
      let before = op.pstate.ps_life in
      Player.inflict op d;
      Player.heal player (before - op.pstate.ps_life)
  | Some _ -> Slot.inflict slot d

let ghoul_attack target d player =
  let num = List.hd target in
  let op = other_player player in
  let slot = op.slots_update.slot_updates.(num) in
  match slot.value with
  | None -> Player.inflict op d
  | Some st ->
      Slot.inflict slot
        (if st.life < 11 then { d with amount = 1000; is_ability = false; is_spell = false } else d)

let aristo_attack target d player =
  let num = List.hd target in
  let op = other_player player in
  let slot = op.slots_update.slot_updates.(num) in
  match slot.value with
  | None -> Player.inflict op d
  | Some st ->
      Slot.inflict slot d;
      if Option.is_none slot.value then Player.inflict op (mk_damage ~is_ability:true st.attack d.context)

let neophyte_attack target d player =
  let num = List.hd target in
  let healed = damage_and_get num d player in
  Slot.heal player.slots_update.slot_updates.(num) ((healed + 1) / 2)

let nosferatu_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        Slot.heal me 2;
        Player.heal me.slots.player 3);
  }

let acolyte =
  creature ~key:"vampires.acolyte" ~attack:(attack 3) ~life:21 ~description:"vampires.acolyte.description" ()

let ghoul =
  creature ~key:"vampires.ghoul" ~attack:(attack 4) ~life:22 ~description:"vampires.ghoul.description"
    ~run_attack:(custom_run_attack ghoul_attack) ()

let aristocrat =
  creature ~key:"vampires.aristocrat" ~attack:(attack 7) ~life:36
    ~description:"vampires.aristocrat.description" ~run_attack:(custom_run_attack aristo_attack)
    ~effects:(effects [ (On_turn, aristo) ])
    ()

let neophyte =
  creature ~key:"vampires.neophyte" ~attack:(attack 5) ~life:14 ~description:"vampires.neophyte.description"
    ~run_attack:(custom_run_attack neophyte_attack) ()

let () =
  acolyte_ref := Some acolyte;
  ghoul_ref := Some ghoul;
  aristocrat_ref := Some aristocrat;
  neophyte_ref := Some neophyte

let cards =
  [
    spell ~key:"vampires.flock" ~description:"vampires.flock.description" ~input_spec:Select_owner_slot
      ~effects:(effects [ (Direct, dark_flock) ])
      ();
    creature ~key:"vampires.noctule" ~attack:(attack 5) ~life:17 ~description:"vampires.noctule.description"
      ~run_attack:(custom_run_attack noctule_attack) ();
    ghoul;
    acolyte;
    spell ~key:"vampires.ties" ~description:"vampires.ties.description" ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, blood_ties) ])
      ();
    creature ~key:"vampires.nosferatu" ~attack:(attack 5) ~life:34
      ~description:"vampires.nosferatu.description" ~new_reaction:nosferatu_reaction ();
    aristocrat;
    creature ~key:"vampires.mansion" ~attack:(attack 0) ~life:40 ~description:"vampires.mansion.description"
      ~new_reaction:mansion_reaction
      ~effects:(effects [ (Direct, ghoulify) ])
      ();
  ]

let house =
  {
    h_key = "vampires";
    h_name = "vampires";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 14;
    h_additional_cards = [ neophyte ];
    h_make_listener = vampire_listener;
    h_data = No_player_data;
  }
