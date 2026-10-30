(* Special house Chaos Priest (Sinist). A mad healer whose every card rolls
   the dice: powers get blocked and buffed at random, wounds heal whichever
   creature the dice pick, the opponent's creatures get stunned all but one,
   summons and spells are flung to random slots and random slots are sealed.
   The cards that listen to the whole board (Self-taught Healer, Witcher,
   Distorter) do it through the house listener, the way the Beastlord's
   listener does: hooks installed in [hl_init] survive the re-creation of the
   update graph in bot simulations, which the per-card hooks of a reaction
   would not. *)

open Sp
open Engine
open Common

let house_id = 50

let healer_ref : card option ref = ref None
let witcher_ref : card option ref = ref None
let distorter_ref : card option ref = ref None

(* A random creature on the board, or None when it is empty. *)
let random_filled rng (su : slots_update) =
  let filled =
    Array.to_list su.slot_updates |> List.filter (fun s -> Option.is_some s.value)
  in
  match filled with [] -> None | _ -> Some (List.nth filled (Dice.int rng (List.length filled)))

(* The number of powers a player owns: the four basic houses plus the special. *)
let nb_powers (p : player_update) = Array.length p.houses_update.hu_value

(* ---------------------------------------------------------------- spells *)

(* Deep Meditation: the growth of one random power stops for one turn of its
   owner's, while every other power gains 1 mana right away. The chosen power
   skips the next growth tick — the one that lands just before the owner's next
   turn — and the spell expires when that turn begins. *)
let deep_meditation : card_effect =
 fun env ->
  let p = player_of_env env in
  let hu = p.houses_update in
  let idx = Dice.int env.env_updater.rng (nb_powers p) in
  let saved = hu.hu_value.(idx).growth in
  hu.hu_value.(idx) <- { hu.hu_value.(idx) with growth = 0 };
  Array.iteri (fun j _ -> if j <> idx then Houses.incr_mana hu 1 [ j ]) hu.hu_value;
  let rec tick env =
    let p = player_of_env env in
    let h = p.houses_update.hu_value.(idx) in
    p.houses_update.hu_value.(idx) <- { h with growth = saved };
    Player.remove_effect p (fun e -> e == tick)
  in
  Player.add_effect p (On_turn, tick)

(* Unstable Healing: a random creature of the owner is healed completely. *)
let unstable_healing : card_effect =
 fun env ->
  let p = player_of_env env in
  match random_filled env.env_updater.rng p.slots_update with
  | Some s -> (
      match s.value with
      | Some st when st.life > 0 && st.life < st.max_life ->
          focus_slot s;
          Slot.heal s (st.max_life - st.life)
      | _ -> ())
  | None -> ()

(* Shockwave: every opponent creature is stunned except one, chosen at
   random. The stun is the usual skip-next-attack flag, released by the
   opponent's own end-of-turn toggle_run. The dice-spared slot is sent to the
   front end so the wave can be drawn on the stunned slots around it while the
   spared one stays clean -- the hit that landed there is invisible. *)
let shockwave : card_effect =
 fun env ->
  let op = other_of_env env in
  match random_filled env.env_updater.rng op.slots_update with
  | None -> ()
  | Some spared ->
      emit env.env_updater
        (Ev_ability
           {
             kind = "chaospriest.shockwave";
             player = env.env_player;
             num = spared.num;
             target = Some (op.pid, spared.num);
           });
      Slots.foreach op.slots_update (fun s -> if s.num <> spared.num then Slot.stun s)

(* ------------------------------------------------------------- creatures *)

(* Self-taught Healer: every turn of its owner it rolls how much damage the
   owner shrugs off this turn (1-5) and stores it in its slot data; the house
   listener's guard reads it while the Healer lives. *)
let healer_turn : card_effect =
 fun env ->
  let me = own_slot_of_env env in
  let r = 1 + Dice.int env.env_updater.rng 5 in
  Slot.set_data me (Custom (string_of_int r))

let healer_reduction (p : player_update) =
  match Slots.find_card p.slots_update (Option.get !healer_ref) with
  | None -> 0
  | Some s -> (
      match s.value with
      | Some st -> (
          match st.data with Custom v -> Option.value (int_of_string_opt v) ~default:0 | _ -> 0)
      | None -> 0)

(* Glider: every turn one of its neighbours is carried to a random unblocked
   empty slot of the owner's board. *)
let glider_turn : card_effect =
 fun env ->
  let me = own_slot_of_env env in
  let rng = env.env_updater.rng in
  let neighbours = Slot.filled_adjacent me in
  let empties =
    List.filter (fun n -> Option.is_none me.slots.slot_updates.(n).value)
      me.slots.player.pstate.ps_slot_list
  in
  match (neighbours, empties) with
  | [], _ | _, [] -> ()
  | _ ->
      let mover = List.nth neighbours (Dice.int rng (List.length neighbours)) in
      let dest = List.nth empties (Dice.int rng (List.length empties)) in
      Slots.move me.slots mover.num dest

(* ---------------------------------------------------------------- listener *)

(* The board a command's input points at: a creature summons into the
   caster's board (into the opponent's for the rare Select_target_creature
   creature), a spell reads its input_spec. Spells that pick their own
   targets (Select_owner / Select_target with a predicate) are left alone. *)
let input_board (p : player_update) (c : command) =
  let caster = p.updater.player_updates.(c.cmd_player) in
  if c.cmd_card.is_spell || c.cmd_card.is_aura then
    match c.cmd_card.input_spec with
    | Select_target_creature | Select_target_slot -> Some p.slots_update
    | Select_owner_slot | Select_owner_creature -> Some caster.slots_update
    | _ -> None
  else if c.cmd_card.input_spec = Select_target_creature then Some p.slots_update
  else Some caster.slots_update

(* The slots a command may land on: creatures summon into empty unblocked
   slots, spells aim at filled slots when they need a creature and at any
   unblocked slot otherwise. *)
let redirect_targets (c : command) (su : slots_update) =
  let unblocked = List.filter (fun n -> List.mem n su.player.pstate.ps_slot_list) base_slot_list in
  let at n = su.slot_updates.(n) in
  match (c.cmd_card.creature_body, c.cmd_card.input_spec) with
  | Some _, _ ->
      (* a creature summon: the empty unblocked slots of its board *)
      List.filter (fun n -> Option.is_none (at n).value) unblocked |> List.map at
  | None, (Select_owner_creature | Select_target_creature) ->
      (* the spell needs a creature: the filled unblocked slots *)
      List.filter (fun n -> Option.is_some (at n).value) unblocked |> List.map at
  | None, (Select_owner_slot | Select_target_slot) -> List.map at unblocked
  | None, _ -> []

(* Distorter: a command of the opponent is re-aimed at a random slot of the
   board it was headed for. *)
let distort (p : player_update) (c : command) =
  match (c.cmd_input, input_board p c) with
  | None, _ | _, None -> c
  | Some _, Some su -> (
      let targets = redirect_targets c su in
      match targets with
      | [] -> c
      | _ ->
          let dest = List.nth targets (Dice.int p.updater.rng (List.length targets)) in
          Option.iter focus_slot (Slots.find_card p.slots_update (Option.get !distorter_ref));
          { c with cmd_input = Some dest.num })

let chaospriest_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let op = other_player p in
        (* Self-taught Healer: the owner shrugs off the Healer's rolled
           amount while it lives *)
        let old_guard = p.guard_hook in
        p.guard_hook <-
          (fun d ->
            let d = old_guard d in
            let r = healer_reduction p in
            if r = 0 || d.amount = 0 then d else { d with amount = max 0 (d.amount - r) });
        (* Witcher: every wound the opponent's creatures take costs the
           opponent one point of a random power while a Witcher faces them *)
        Array.iter
          (fun (opp : slot_update) ->
            let old = opp.on_damage_hook in
            opp.on_damage_hook <-
              (fun st d ->
                old st d;
                if d.amount > 0 then (
                  match Slots.find_card p.slots_update (Option.get !witcher_ref) with
                  | Some w when w.num = opp.num ->
                      Houses.incr_mana op.houses_update (-1) [ Dice.int p.updater.rng (nb_powers p) ]
                  | _ -> ())))
          op.slots_update.slot_updates;
        (* Distorter: the opponent's summons and spells land where the dice
           say while it lives *)
        let old_submit = op.submit_command_hook in
        op.submit_command_hook <-
          (fun c ->
            let c =
              match Slots.find_card p.slots_update (Option.get !distorter_ref) with
              | Some _ -> distort p c
              | None -> c
            in
            old_submit c));
  }

(* Unstable Cannon: every turn of its owner it seals a random empty opponent
   slot for exactly the opponent's next turn (attacks still cross it, summons
   and moves do not), remembering the slot in its data; the seal is lifted
   when the next one is placed, or when the Cannon leaves. A full opponent
   board leaves it with nowhere to seal. *)
let blocked_of_cannon (me : slot_update) =
  match me.value with
  | Some st -> ( match st.data with Custom s -> int_of_string_opt s | _ -> None)
  | None -> None

let unblock_slot (p : player_update) n =
  if not (List.mem n p.pstate.ps_slot_list) then
    p.pstate.ps_slot_list <- List.sort compare (n :: p.pstate.ps_slot_list)

let cannon_block : card_effect =
 fun env ->
  let op = other_of_env env in
  let me = own_slot_of_env env in
  Option.iter (unblock_slot op) (blocked_of_cannon me);
  match Slots.get_open op.slots_update with
  | [] -> ()
  | opens ->
      let n = List.nth opens (Dice.int env.env_updater.rng (List.length opens)) in
      Player.block_slot op n;
      Slot.set_data me (Custom (string_of_int n));
      Updater.focus env.env_updater n op.pid true

let cannon_unblock_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        Option.iter (unblock_slot (other_player me.slots.player)) (blocked_of_cannon me));
  }

(* When the Cannon enters the game it deals 10 damage to 3 random opponent
   creatures. *)
let cannon_entry : card_effect =
  fun env ->
    let op = other_of_env env in
    match random_filled env.env_updater.rng op.slots_update with
    | None -> ()
    | Some _ ->
        let victims =
          op.slots_update.slot_updates
          |> Array.to_list |> List.filter (fun s -> Option.is_some s.value)
          |> Dice.shuffle env.env_updater.rng
        in
        let hit = List.filteri (fun i _ -> i < 3) victims in
        List.iter (fun s -> Slot.inflict s (ability_damage env 10)) hit;
        (* the cannon is the muzzle: it lobs a bomb at each of the creatures
           it hit, and the front end draws them *)
        focus_slot ~blocking:false (own_slot_of_env env);
        let kind =
          "chaospriest.cannon.bomb"
          ^ String.concat "" (List.map (fun s -> "." ^ string_of_int s.num) hit)
        in
        emit env.env_updater
          (Ev_ability
             { kind; player = env.env_player; num = env.env_selected; target = None })

(* ------------------------------------------------------------------ house *)

(* The list is in printed cost order, so the deck builder's default index+1
   gives the printed costs 1..8. *)
let cards =
  [
    spell ~key:"chaospriest.deepmeditation" ~description:"chaospriest.deepmeditation.description"
      ~effects:(effects [ (Direct, deep_meditation) ]) ();
    spell ~key:"chaospriest.unstablehealing" ~description:"chaospriest.unstablehealing.description"
      ~effects:(effects [ (Direct, unstable_healing) ]) ();
    spell ~key:"chaospriest.shockwave" ~description:"chaospriest.shockwave.description"
      ~effects:(effects [ (Direct, shockwave) ]) ();
    creature ~key:"chaospriest.selftaughthealer" ~attack:(attack 3) ~life:20
      ~description:"chaospriest.selftaughthealer.description"
      ~effects:(effects [ (On_turn, healer_turn) ]) ();
    creature ~key:"chaospriest.glider" ~attack:(attack 5) ~life:23
      ~description:"chaospriest.glider.description" ~effects:(effects [ (On_turn, glider_turn) ]) ();
    creature ~key:"chaospriest.witcher" ~attack:(attack 5) ~life:38
      ~description:"chaospriest.witcher.description" ();
    creature ~key:"chaospriest.distorter" ~attack:(attack 8) ~life:40
      ~description:"chaospriest.distorter.description" ();
    creature ~key:"chaospriest.unstablecannon" ~attack:(attack 8) ~life:44
      ~description:"chaospriest.unstablecannon.description" ~new_reaction:cannon_unblock_reaction
      ~effects:(effects [ (Direct, cannon_entry); (On_turn, cannon_block) ]) ();
  ]

(* Printed costs: Deep Meditation 1, Unstable Healing 2, Shockwave 3,
   Self-taught Healer 4, Glider 5, Witcher 6, Distorter 7, Unstable Cannon 8.
   The dealt special deck takes one card of every quarter: Deep Meditation,
   Shockwave, Glider, Distorter. *)
let cost_fn i = i + 1

let () =
  healer_ref := Some (List.nth cards 3);
  witcher_ref := Some (List.nth cards 5);
  distorter_ref := Some (List.nth cards 6)

let house =
  special_house ~key:"chaospriest" ~id:house_id ~listener:chaospriest_listener cards
