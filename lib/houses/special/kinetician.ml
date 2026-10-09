(* Special house Kinetician — port of priv/sp/house/Kinetician.scala. Force of Will is ChangeTarget;
   Call of the Fittest halves every card of the chosen house until one is played. *)

open Sp
open Engine
open Common

let empty_data = { ki_target = None; ki_halves = [] }
let get_data p = match p.pstate.ps_data with Kinetic_data d -> d | _ -> empty_data
let set_data p d = Player.set_data p (Kinetic_data d)
let manipulator_ref : card option ref = ref None

let nearest_slot_opposed ?(opposed = true) selected (p : player_update) =
  let op = other_player p in
  p.pstate.ps_slot_list
  |> List.filter (fun n ->
      n <> selected
      && Option.is_none p.slots_update.slot_updates.(n).value
      && Option.is_some op.slots_update.slot_updates.(n).value = opposed)
  |> List.fold_left
       (fun best n ->
         match best with Some b when abs (b - selected) <= abs (n - selected) -> best | _ -> Some n)
       None

let recover_target (p : player_update) =
  Slots.foreach p.slots_update (fun s -> Slot.set_target s [ s.num ]);
  set_data p { (get_data p) with ki_target = None }

let change_target : card_effect =
 fun env ->
  let p = player_of_env env in
  let target = Slot.get (opp_slot_of_env env) in
  set_data p { (get_data p) with ki_target = Some target.slot_id };
  Slots.foreach p.slots_update (fun s -> Slot.set_target s [ env.env_selected ]);
  Player.add_effect p (On_end_turn, one_time_player_effect (fun e -> recover_target (player_of_env e)))

let house_cards (p : player_update) idx =
  match Array.find_opt (fun h -> h.phd_house.h_index = idx) p.pstate.ps_desc.init_desc.pd_houses with
  | None -> [||]
  | Some h -> List.fold_left (fun cs m -> m h cs) h.phd_cards p.pstate.ps_desc.desc_mods

let halve_cost (card : card) : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> card.house_index then cards
  else Array.map (fun c -> if c.cd_card.id = card.id then { c with cd_cost = c.cd_cost / 2 } else c) cards

let mage : card_effect =
 fun env ->
  let p = player_of_env env in
  let strongest =
    Slots.foldl p.slots_update None (fun acc s ->
        if s.num = env.env_selected then acc
        else
          match acc with
          | None -> Some s
          | Some a -> if (Slot.get s).attack > (Slot.get a).attack then Some s else acc)
  in
  Option.iter (fun s -> Slots.move p.slots_update env.env_selected s.num) strongest

let trick : card_effect =
 fun env ->
  Option.iter
    (fun n -> Slots.move (player_of_env env).slots_update env.env_selected n)
    (nearest_slot_opposed ~opposed:false env.env_selected (player_of_env env))

let lone : card_effect =
 fun env ->
  let op = other_of_env env in
  let nb =
    Array.fold_left (fun n s -> if Option.is_none s.value then n + 1 else n) 0 op.slots_update.slot_updates
  in
  Player.inflict op (ability_damage env nb);
  focus_slot (own_slot_of_env env)

let fittest : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = p.slots_update.slot_updates.(env.env_selected) in
  let idx = (Slot.get slot).card.c_card.house_index in
  let halves =
    Array.fold_left
      (fun acc cd ->
        let m = halve_cost cd.cd_card in
        Player.add_desc_mod p m;
        (cd.cd_card.id, m) :: acc)
      (get_data p).ki_halves (house_cards p idx)
  in
  set_data p { (get_data p) with ki_halves = halves }

let force_barrier : card_effect =
 fun env ->
  Slot.update (own_slot_of_env env) (fun st ->
      { st with life = st.life * 2; max_life = st.max_life * 2; status = st.status land lnot run_flag })

let warp : card_effect =
 fun env ->
  let slot = opp_slot_of_env env in
  Slot.update slot (fun st ->
      if st.life > st.card.c_card.cost then { st with life = st.card.c_card.cost } else st)

let focus_bonus = Attack_func (fun a -> a + 1)

let force : card_effect =
 fun env ->
  let p = player_of_env env in
  Slots.foreach p.slots_update (fun s -> add_attack s focus_bonus);
  change_target env;
  Player.add_effect p
    ( On_end_turn,
      one_time_player_effect (fun e ->
          Slots.foreach (player_of_env e).slots_update (fun s -> remove_first_attack s focus_bonus)) )

let attack3 = Attack_func (fun _ -> 3)

let manipulate : card_effect =
 fun env ->
  let op = other_of_env env in
  Slots.foreach op.slots_update (fun s -> add_attack s attack3);
  Player.add_effect op
    ( On_end_turn,
      fun e -> Slots.foreach (player_of_env e).slots_update (fun s -> remove_first_attack s attack3) )

let after_submit (me : slot_update) slot_id =
  let op = other_player me.slots.player in
  match Slots.find_slot_id op.slots_update slot_id with
  | None -> ()
  | Some slot -> (
      match slot.value with
      | None -> ()
      | Some st ->
          let open_before = Slots.get_open me.slots in
          if open_before <> [] || st.card.c_card.cost * 4 >= (Slot.get me).life then (
            let ctx =
              { ctx_player = me.slots.player.pid; ctx_card = !manipulator_ref; ctx_selected = me.num }
            in
            Slot.inflict me (mk_damage ~is_ability:true (4 * st.card.c_card.cost) ctx);
            match Slots.get_open me.slots with
            | [] -> ()
            | ns ->
                let dest = if List.mem slot.num ns then slot.num else List.hd ns in
                Slots.move ~dest_player:me.slots.player op.slots_update slot.num dest))

let manipulator_reaction () =
  let r = Reaction.default () in
  r

let kinetic_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let old_dead = (other_player p).slots_update.on_dead_hook in
        (other_player p).slots_update.on_dead_hook <-
          (fun dead ->
            old_dead dead;
            match (get_data p).ki_target with
            | Some id when id = dead.d_slot.slot_id -> recover_target p
            | _ -> ());
        let op = other_player p in
        let old_opp = op.submit_command_hook in
        op.submit_command_hook <-
          (fun c ->
            let r = old_opp c in
            Option.iter
              (fun id ->
                Slots.foreach p.slots_update (fun s ->
                    match s.value with
                    | Some st when is_card !manipulator_ref st.card.c_card -> after_submit s id
                    | _ -> ()))
              r;
            r);
        let old = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old c in
            let d = get_data p in
            (match List.assoc_opt c.cmd_card.id d.ki_halves with
            | Some m ->
                Player.remove_desc_mod p m;
                set_data p { d with ki_halves = List.remove_assoc c.cmd_card.id d.ki_halves }
            | None -> ());
            r));
  }

let manipulator =
  creature ~key:"kinetician.manipulator" ~attack:(attack 3) ~life:28
    ~description:"kinetician.manipulator.description"
    ~effects:(effects [ (Direct, manipulate) ])
    ~new_reaction:manipulator_reaction ()

let () = manipulator_ref := Some manipulator

let cards =
  [
    creature ~key:"kinetician.magerunner" ~attack:(attack 4) ~life:10
      ~description:"kinetician.magerunner.description"
      ~effects:(effects [ (Direct, mage) ])
      ();
    creature ~key:"kinetician.tricker" ~attack:(attack 4) ~life:10
      ~description:"kinetician.tricker.description"
      ~effects:(effects [ (On_turn, trick) ])
      ();
    spell ~key:"kinetician.force" ~description:"kinetician.force.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, force) ])
      ();
    spell ~key:"kinetician.warp" ~description:"kinetician.warp.description" ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, warp) ])
      ();
    creature ~key:"kinetician.mage" ~attack:(attack 7) ~life:22 ~description:"kinetician.mage.description"
      ~effects:(effects [ (On_turn, lone) ])
      ();
    spell ~key:"kinetician.call" ~description:"kinetician.call.description"
      ~input_spec:(Select_owner non_special)
      ~effects:(effects [ (Direct, fittest) ])
      ();
    spell ~key:"kinetician.barrier" ~description:"kinetician.barrier.description"
      ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, force_barrier) ])
      ();
    manipulator;
  ]

let house =
  special_house ~key:"kinetician" ~id:42 ~listener:kinetic_listener ~data:(Kinetic_data empty_data) cards
