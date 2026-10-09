(* Special house Lightstaves, by Redrook.  The Aura mechanic.

   An Aura is a spell-like card that is cast on a creature and stays in play
   until its duration runs out or its target leaves the board.  It is marked
   [is_aura] but deliberately not [is_spell]: it pays a card and a target, not
   a spell slot, and effects that count or forbid spells do not see it.  The
   active auras live in [Aura_data] so they travel with the player state and
   with simulations. *)

open Sp
open Engine
open Common

(* --------------------------------------------------------------- the keys *)

let life_key = "lightstave.auraoflife"
let protection_key = "lightstave.auraofprotection"
let illumination_key = "lightstave.auraofillumination"
let aura_duration = 4

(* ------------------------------------------------------------------ state *)

let auras_of = function Aura_data xs -> xs | _ -> []
let auras (p : player_state) = auras_of p.ps_data
let get_auras p = auras p.pstate

let set_auras (p : player_update) xs = Player.set_data p (Aura_data xs)

let aura_counter = ref 0

let fresh_aura_id () =
  incr aura_counter;
  !aura_counter

let find_aura p id = List.find_opt (fun a -> a.au_id = id) (get_auras p)

let aura_color key =
  match key with
  | k when k = life_key -> (86, 214, 132, 255)
  | k when k = protection_key -> (104, 172, 255, 255)
  | k when k = illumination_key -> (255, 216, 104, 255)
  | _ -> (220, 220, 220, 255)

(* --------------------------------------------------------------- desc mods *)

(* A second aura of the same name may not be played while the first is up. *)
let hide_key_mod key : desc_mod =
 fun _ cards -> Array.map (fun cd -> if cd.cd_card.key = key then { cd with cd_enabled = false } else cd) cards

let hide_life = hide_key_mod life_key
let hide_protection = hide_key_mod protection_key
let hide_illumination = hide_key_mod illumination_key

let hide_mod_for key =
  if key = life_key then hide_life
  else if key = protection_key then hide_protection
  else hide_illumination

(* --------------------------------------------------------------- targeting *)

let target_slot p id =
  match Slots.find_slot_id p.slots_update id with
  | Some s -> Some s
  | None -> Slots.find_slot_id (other_player p).slots_update id

let target_slot_of_env env =
  let side = if env.env_target_other then other_of_env env else player_of_env env in
  side.slots_update.slot_updates.(env.env_selected)

(* --------------------------------------------------------------- the auras *)

(* Ends one aura.  Aura of Life heals its caster as it dissipates; the other
   auras simply leave.  The desc mod is removed only when the last copy of
   that name is gone, so a card cannot be played again too early. *)
let aura_is_active p key =
  List.exists
    (fun a -> a.au_key = key)
    (get_auras p @ get_auras (other_player p))

let dissipate (p : player_update) id =
  match find_aura p id with
  | None -> ()
  | Some a ->
      set_auras p (List.filter (fun x -> x.au_id <> id) (get_auras p));
      if a.au_key = life_key then Player.heal p 5;
      if not (aura_is_active p a.au_key) then begin
        Player.remove_desc_mod p (hide_mod_for a.au_key);
        Player.remove_desc_mod (other_player p) (hide_mod_for a.au_key)
      end

(* Runs at the start of the caster's turn, before the remaining turns are
   reduced: an Aura cast on one turn first acts on the next one, then gets one
   effect on each of its turns.  The last turn still gets its effect and the
   Aura dissipates afterwards. *)
let apply_aura_tick (p : player_update) a =
  match target_slot p a.au_target with
  | None -> dissipate p a.au_id
  | Some target -> (
      (match a.au_key with
      | k when k = life_key ->
          Slot.heal target 4;
          List.iter (fun s -> Slot.heal s 4) (Slot.filled_adjacent target)
      | k when k = illumination_key ->
          Slot.heal target 3;
          Slots.foreach p.slots_update (fun s ->
              if s.num <> target.num || s.slots.player.pid <> p.pid then Slot.heal s 3);
          let d =
            mk_damage ~is_ability:true 3
              { ctx_player = p.pid; ctx_card = None; ctx_selected = a.au_target }
          in
          Slots.inflict_creatures (other_player p).slots_update d
      | _ -> ());
      match find_aura p a.au_id with
      | None -> ()
      | Some now ->
          let remaining = now.au_remaining - 1 in
          if remaining <= 0 then dissipate p now.au_id
          else
            set_auras p
              (List.map (fun x -> if x.au_id = now.au_id then { x with au_remaining = remaining } else x)
                 (get_auras p)))

let tick : card_effect =
 fun env ->
  let p = player_of_env env in
  let ids = List.map (fun a -> a.au_id) (get_auras p) in
  List.iter (fun id -> Option.iter (apply_aura_tick p) (find_aura p id)) ids

let ensure_tick p =
  if not (List.exists (fun (_, e) -> e == tick) p.pstate.ps_effects) then
    Player.add_effect p (On_turn, tick)

let add_aura p key target_id =
  if aura_is_active p key then ()
  else (
    set_auras p
      (get_auras p
      @ [
          {
            au_id = fresh_aura_id ();
            au_key = key;
            au_target = target_id;
            au_remaining = aura_duration;
          };
        ]);
    (* The name is exclusive across the table, not just for its caster. *)
    Player.add_desc_mod p (hide_mod_for key);
    Player.add_desc_mod (other_player p) (hide_mod_for key);
    ensure_tick p)

let cast_aura key : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = target_slot_of_env env in
  match slot.value with
  | None -> ()
  | Some st -> add_aura p key st.slot_id

let cast_life : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = target_slot_of_env env in
  if slot.value <> None && not (aura_is_active p life_key) then begin
    cast_aura life_key env;
    Player.heal p 5
  end

(* Prism Lantern: every active aura of the owner lasts one turn longer. *)
let extend_auras amount : card_effect =
 fun env ->
  let p = player_of_env env in
  if get_auras p <> [] then
    set_auras p (List.map (fun a -> { a with au_remaining = a.au_remaining + amount }) (get_auras p))

(* --------------------------------------------------------- Priestess of Light *)

let end_all_auras : card_effect =
 fun env ->
  let p = player_of_env env in
  let op = other_of_env env in
  let end_player owner =
    let ids = List.map (fun a -> a.au_id) (get_auras owner) in
    List.iter (dissipate owner) ids;
    List.length ids
  in
  let count = end_player p + end_player op in
  if count > 0 then (
    let d =
      mk_damage ~is_ability:true (6 * count)
        { ctx_player = p.pid; ctx_card = env.env_card; ctx_selected = env.env_selected }
    in
    Player.inflict op d;
    Slots.inflict_creatures op.slots_update d)

let priestess_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        incr_mana_from me me.slots.player 1 [ special_house_index ]);
  }

(* ---------------------------------------------------------- damage reactions *)

let aurora_ref : card option ref = ref None

let aurora_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun damage ->
        let me = Reaction.bound_slot r in
        if damage.amount > 0 then
          let d =
            mk_damage ~is_ability:true 2
              { ctx_player = me.slots.player.pid; ctx_card = !aurora_ref; ctx_selected = me.num }
          in
          Player.inflict (other_player me.slots.player) d);
  }

let palace_ref : card option ref = ref None

let palace_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_damage =
      (fun damage ->
        let me = Reaction.bound_slot r in
        if damage.amount > 0 && damage.is_spell then
          let op = other_player me.slots.player in
          let d =
            mk_damage ~is_ability:true 3
              { ctx_player = me.slots.player.pid; ctx_card = !palace_ref; ctx_selected = me.num }
          in
          Player.inflict op d;
          Slots.inflict_creatures op.slots_update d);
  }

(* --------------------------------------------------------------- protection *)

(* Damage aimed at a neighbour of a protected creature is handed to the
   protected creature instead; damage aimed at the creature itself loses 5.
   [redirected] stops a redirected hit from being bounced on again. *)
let protect_damage p s d =
  let auras = List.filter (fun a -> a.au_key = protection_key) (get_auras p) in
  let rec go d = function
    | [] -> d
    | a :: rest -> (
        match target_slot p a.au_target with
        | None -> go d rest
        | Some target ->
            let same_side = s.slots.player.pid = target.slots.player.pid in
            if same_side && s.num = target.num then
              go { d with amount = max 0 (d.amount - 5) } rest
            else if same_side && (not d.redirected) && d.amount > 0
                    && List.mem s.num (adjacents target.num) then (
              (match target.value with
              | Some st -> st.reaction.inflict target { d with redirected = true }
              | None -> ());
              { d with amount = 0 })
            else go d rest)
  in
  go d auras

let install_protection p =
  let wrap s =
    let old = s.protect_hook in
    s.protect_hook <- (fun d -> protect_damage p s (old d))
  in
  Array.iter wrap p.slots_update.slot_updates;
  Array.iter wrap (other_player p).slots_update.slot_updates

(* --------------------------------------------------------------- listener *)

let lightstave_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        install_protection p;
        let on_death dead =
          List.iter
            (fun a -> if a.au_target = dead.d_slot.slot_id then dissipate p a.au_id)
            (get_auras p)
        in
        after_death p on_death;
        after_death (other_player p) on_death;
        init_opp_death p);
  }

(* --------------------------------------------------------------- the cards *)

let prism_lantern =
  creature ~key:"lightstave.prismlantern" ~attack:(attack 3) ~life:17
    ~description:"lightstave.prismlantern.description"
    ~effects:(effects [ (Direct, extend_auras 1) ])
    ()

let aurora =
  let c =
    creature ~key:"lightstave.aurora" ~attack:(attack 3) ~life:21
      ~description:"lightstave.aurora.description" ~new_reaction:aurora_reaction ()
  in
  aurora_ref := Some c;
  c

let aura_of_life =
  spell ~is_aura:true ~key:life_key ~description:"lightstave.auraoflife.description"
    ~input_spec:Select_owner_creature
    ~effects:(effects [ (Direct, cast_life) ])
    ()

let aura_of_protection =
  spell ~is_aura:true ~key:protection_key ~description:"lightstave.auraofprotection.description"
    ~input_spec:Select_owner_creature
    ~effects:(effects [ (Direct, cast_aura protection_key) ])
    ()

let aura_of_illumination =
  spell ~is_aura:true ~key:illumination_key ~description:"lightstave.auraofillumination.description"
    ~input_spec:Select_owner_creature
    ~effects:(effects [ (Direct, cast_aura illumination_key) ])
    ()

let crystalline_palace =
  let c =
    creature ~key:"lightstave.crystallinepalace" ~attack:(attack 3) ~life:27
      ~description:"lightstave.crystallinepalace.description" ~run_attack:multi_run_attack
      ~new_reaction:palace_reaction ()
  in
  palace_ref := Some c;
  c

let priestess_of_light =
  creature ~key:"lightstave.priestessoflight" ~attack:(attack 6) ~life:33
    ~description:"lightstave.priestessoflight.description" ~new_reaction:priestess_reaction
    ~effects:(effects [ (Direct, end_all_auras) ])
    ()

let rainbow_dragon : card_effect =
 fun env ->
  let p = player_of_env env in
  let me = own_slot_of_env env in
  List.iter
    (fun i ->
      let n =
        Array.fold_left
          (fun n s ->
            match s.value with Some st when st.card.c_card.house_index = i -> n + 1 | _ -> n)
          0 p.slots_update.slot_updates
      in
      if n > 0 then incr_mana_from me p n [ i ])
    [ 0; 1; 2; 3 ]

let rainbow_dragon =
  creature ~key:"lightstave.rainbowdragon" ~attack:(attack 8) ~life:47
    ~description:"lightstave.rainbowdragon.description"
    ~effects:(effects [ (Direct, rainbow_dragon) ])
    ()

let cards =
  [
    prism_lantern;
    aurora;
    aura_of_life;
    aura_of_protection;
    aura_of_illumination;
    crystalline_palace;
    priestess_of_light;
    rainbow_dragon;
  ]

let house = special_house ~key:"lightstave" ~id:48 ~listener:lightstave_listener cards
