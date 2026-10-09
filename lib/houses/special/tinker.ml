(* Special house Tinker — port of priv/sp/house/Tinker.scala. Bombs sit on the opponent's board and tick on
   that player's turn; the queen, if present, raises the blast. *)

open Sp
open Engine
open Common

let queen_ref : card option ref = ref None
let soldier_ref : card option ref = ref None
let dragon_ref : card option ref = ref None
let is_card opt (st : slot_state) = match opt with Some c -> st.card.c_card.id = c.id | None -> false
let has_card su opt = Option.is_some (Slots.find su (is_card opt))
let slot_of_env env = (player_of_env env).slots_update.slot_updates.(env.env_selected)

let bomb_explode (owner : player_update) (card : card) =
  let ctx = { ctx_player = owner.pid; ctx_card = Some card; ctx_selected = no_selection } in
  let to_owner = if has_card (other_player owner).slots_update !queen_ref then 6 else 3 in
  Player.inflict owner (mk_damage ~is_ability:true to_owner ctx);
  if has_card owner.slots_update !queen_ref then
    Player.inflict (other_player owner) (mk_damage ~is_ability:true 3 ctx)

let bomb_reaction () =
  let r = Reaction.default () in
  { r with on_my_death = (fun dead -> bomb_explode dead.d_player dead.d_slot.card.c_card) }

let tinkerbomb =
  creature ~key:"tinker.bomb" ~attack:(attack 0) ~life:5 ~description:"tinker.bomb.description"
    ~effects:
      (effects
         [
           ( On_turn,
             fun env ->
               let slot = slot_of_env env in
               focus_slot ~blocking:false slot;
               drain slot (mk_damage ~is_ability:true 1 (ctx_of_env env)) );
         ])
    ~new_reaction:bomb_reaction ()

let plant_bomb_at (player : player_update) num =
  if in_slot_range num && List.mem num player.pstate.ps_slot_list then
    let slot = player.slots_update.slot_updates.(num) in
    if Option.is_none slot.value then Slots.add_creature slot (make_creature tinkerbomb)

let rec take_open player n slots =
  if n <= 0 || slots = [] then ()
  else
    let i = Dice.int player.updater.rng (List.length slots) in
    let rec split j = function
      | [] -> (None, [])
      | x :: xs when j = 0 -> (Some x, xs)
      | x :: xs ->
          let p, r = split (j - 1) xs in
          (p, x :: r)
    in
    match split i slots with
    | Some num, rest ->
        plant_bomb_at player num;
        take_open player (n - 1) rest
    | None, _ -> ()

let plant_random player n = take_open player n (Slots.get_open player.slots_update)

let plant_rightmost player =
  match Slots.get_open player.slots_update with
  | [] -> ()
  | ns -> plant_bomb_at player (List.fold_left max (List.hd ns) ns)

let plant_leftmost player =
  match Slots.get_open player.slots_update with
  | [] -> ()
  | ns -> plant_bomb_at player (List.fold_left min (List.hd ns) ns)

let spider_attack target d player =
  if single_target_attack target d player then ignore (single_target_attack target d player)

let falcon_reaction () =
  let r = Reaction.default () in
  { r with on_my_death = (fun dead -> plant_leftmost (other_player dead.d_player)) }

let tinker_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let opp = other_player p in
        let old = opp.slots_update.on_dead_hook in
        opp.slots_update.on_dead_hook <-
          (fun dead ->
            old dead;
            (match !soldier_ref with
            | Some soldier when dead.d_slot.card.c_card.id = tinkerbomb.id ->
                Slots.foreach p.slots_update (fun s ->
                    match s.value with
                    | Some st when st.card.c_card.id = soldier.id ->
                        add_attack s (Attack_func (fun a -> a + 1))
                    | _ -> ())
            | _ -> ());
            match (dead.d_damage, !dragon_ref) with
            | Some d, Some dragon -> (
                match d.context.ctx_card with
                | Some c when c.id = dragon.id -> Houses.incr_mana p.houses_update 1 [ 4 ]
                | _ -> ())
            | _ -> ()));
  }

let cards =
  [
    spell ~key:"tinker.plant" ~description:"tinker.plant.description" ~input_spec:Select_target_slot
      ~effects:(effects [ (Direct, fun env -> plant_bomb_at (other_of_env env) env.env_selected) ])
      ();
    creature ~key:"tinker.spider" ~attack:(attack 4) ~life:13 ~description:"tinker.spider.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 let opp = Slot.opposite (slot_of_env env) in
                 if Option.is_none opp.value then Slots.add_creature opp (make_creature tinkerbomb) );
           ])
      ~run_attack:(custom_run_attack spider_attack) ();
    creature ~key:"tinker.falcon" ~attack:(attack 5) ~life:17 ~description:"tinker.falcon.description"
      ~effects:(effects [ (Direct, fun env -> plant_rightmost (other_of_env env)) ])
      ~new_reaction:falcon_reaction ();
    spell ~key:"tinker.scatter" ~description:"tinker.scatter.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 let op = other_of_env env in
                 let occupied =
                   Array.to_list op.slots_update.slot_updates
                   |> List.filter_map (fun s -> if Option.is_some s.value then Some s.num else None)
                 in
                 Slots.inflict_creatures op.slots_update (mk_damage ~is_spell:true 8 (ctx_of_env env));
                 List.iter
                   (fun n -> if Option.is_none op.slots_update.slot_updates.(n).value then plant_bomb_at op n)
                   occupied );
           ])
      ();
    creature ~key:"tinker.soldier" ~attack:(attack 3) ~life:44 ~description:"tinker.soldier.description" ();
    creature ~key:"tinker.automaton" ~attack:(attack 5) ~life:31 ~description:"tinker.automaton.description"
      ~effects:(effects [ (On_end_turn, fun env -> plant_random (other_of_env env) 1) ])
      ();
    creature ~key:"tinker.queen" ~attack:(attack 7) ~life:32 ~description:"tinker.queen.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 Slots.foreach (other_of_env env).slots_update (fun s ->
                     match s.value with
                     | Some st when st.card.c_card.id = tinkerbomb.id -> Slot.destroy s
                     | _ -> ()) );
           ])
      ();
    creature ~key:"tinker.dragon" ~attack:(attack 7) ~life:40 ~description:"tinker.dragon.description"
      ~effects:
        (effects
           [
             ( Direct,
               fun env ->
                 let slot = slot_of_env env in
                 Option.iter
                   (fun (s : slot_state) ->
                     let d =
                       mk_damage s.attack
                         {
                           ctx_player = env.env_player;
                           ctx_card = Some s.card.c_card;
                           ctx_selected = slot.num;
                         }
                     in
                     emit env.env_updater (Ev_run_slot { num = slot.num; player = env.env_player });
                     ignore (single_target_attack s.target d (player_of_env env));
                     emit env.env_updater (Ev_refresh { silent = false }))
                   slot.value;
                 plant_random (other_of_env env) 3 );
           ])
      ();
  ]

let () =
  soldier_ref := List.find_opt (fun (c : card) -> c.key = "tinker.soldier") cards;
  queen_ref := List.find_opt (fun (c : card) -> c.key = "tinker.queen") cards;
  dragon_ref := List.find_opt (fun (c : card) -> c.key = "tinker.dragon") cards

let house =
  {
    h_key = "tinker";
    h_name = "tinker";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 26;
    h_additional_cards = [ tinkerbomb ];
    h_make_listener = tinker_listener;
    h_data = No_player_data;
  }
