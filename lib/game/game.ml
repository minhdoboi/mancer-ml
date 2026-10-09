(* Deck building, a simple bot, and the headless turn loop.  Mirrors
   priv/sp/CardShuffle.scala and HeadlessGame in priv/sp/bot/Tournament.scala.

   Legal four-house decks live in shuffle.ml.  [make_player_desc] is the
   fixed cost-slice used by tests that need a reproducible hand. *)

open Sp
open Engine

(* ------------------------------------------------------------ deck building *)

(* House.initCards: default cost = index + 1. Additional cards keep their own cost. *)
let init_cards ?(cost_fn = fun i -> i + 1) (h : house) =
  List.iteri
    (fun i (c : card) ->
      c.cost <- cost_fn i;
      c.house_index <- h.h_index;
      c.house_id <- h.h_house_id;
      c.card_index <- i)
    h.h_cards;
  List.iteri
    (fun i (c : card) ->
      c.house_index <- h.h_index;
      c.house_id <- h.h_house_id;
      c.card_index <- i)
    h.h_additional_cards

let deck_costs = [ 1; 2; 3; 4; 5; 6 ]
let is_special (h : house) = h.h_index = special_house_index

(* CardShuffle's HModel picks one special card from each quarter of the
   house; we take the first of each quarter until the deck builder is ported *)
let special_pick (h : house) =
  let quarter = max 1 (List.length h.h_cards / 4) in
  fun i _ -> i mod quarter = 0 && i / quarter < 4

let deck_of_house (h : house) =
  h.h_cards
  |> List.filteri (if is_special h then special_pick h else fun i _ -> List.mem (i + 1) deck_costs)
  |> List.map (fun c -> { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 })
  |> Array.of_list

let empty_special =
  {
    h_key = "none";
    h_name = "none";
    h_cards = [];
    h_index = special_house_index;
    h_effects = [];
    h_house_id = 0;
    h_additional_cards = [];
    h_make_listener = (fun () -> default_listener);
    h_data = No_player_data;
  }

let make_player_desc ?(special = empty_special) (h : house) =
  {
    pd_houses =
      [|
        { phd_house = h; phd_cards = deck_of_house h };
        { phd_house = special; phd_cards = deck_of_house special };
      |];
  }

let special_effects (d : player_desc) =
  match Array.find_opt (fun h -> h.phd_house.h_index = special_house_index) d.pd_houses with
  | Some h -> h.phd_house.h_effects
  | None -> []

let default_life = 60

let initial_player_state ?(life = default_life) (mana : int list) (d : player_desc) =
  let mana =
    match mana with
    | [ _; _; _; _ ] -> mana @ [ 2 ]
    | [ _; _; _; _; _ ] -> mana
    | _ -> invalid_arg "Game.initial_player_state: expected 4 or 5 mana values"
  in
  {
    ps_houses = Array.of_list (List.map (fun mana -> { mana; growth = 1 }) mana);
    ps_desc = { init_desc = d; desc_mods = [] };
    ps_slots = Array.make nb_slots None;
    ps_slot_list = base_slot_list;
    ps_life = life;
    ps_effects = special_effects d;
    ps_data =
      (match Array.find_opt (fun h -> h.phd_house.h_index = special_house_index) d.pd_houses with
      | Some h -> h.phd_house.h_data
      | None -> No_player_data);
    ps_transitions = [];
    ps_maluses = [];
  }

(* [life] is the life both sides start with; [owner_life] and [opponent_life]
   give one side its own life, as the easy campaign does for the player. *)
let initial_game_state ?(mana0 = [ 2; 2; 2; 2 ]) ?(mana1 = [ 1; 1; 1; 1 ]) ?life ?owner_life ?opponent_life
    (desc : game_desc) =
  let life0 = match owner_life with Some l -> l | None -> Option.value life ~default:default_life in
  let life1 = match opponent_life with Some l -> l | None -> Option.value life ~default:default_life in
  {
    players =
      [|
        initial_player_state ~life:life0 mana0 desc.gd_players.(0);
        initial_player_state ~life:life1 mana1 desc.gd_players.(1);
      |];
  }

(* ------------------------------------------------------------------ the bot *)

(* Choices.getNexts: every legal command, interleaved one placement per card
   so the search meets every card before it retries slots.  Greedy still sorts
   cheapest-first on top of this list. *)

let filled_nums (p : player_state) =
  List.init nb_slots Fun.id |> List.filter (fun n -> Option.is_some p.ps_slots.(n))

let open_nums (p : player_state) =
  List.init nb_slots Fun.id
  |> List.filter (fun n -> Option.is_none p.ps_slots.(n) && List.mem n p.ps_slot_list)

let commands_for pid (state : game_state) (cd : card_desc) =
  let p = state.players.(pid) in
  let op = state.players.(other pid) in
  let command ?(flag = None) input =
    { cmd_player = pid; cmd_card = cd.cd_card; cmd_input = input; cmd_cost = cd.cd_cost; cmd_flag = flag }
  in
  match cd.cd_card.input_spec with
  | No_input -> [ command None ]
  | Select_owner f -> List.map (fun n -> command (Some n)) (f pid state)
  | Select_owner_slot -> List.map (fun n -> command (Some n)) (open_nums p)
  | Select_owner_creature -> List.map (fun n -> command (Some n)) (filled_nums p)
  | Select_target f -> List.map (fun n -> command (Some n)) (f (other pid) state)
  | Select_target_slot -> List.map (fun n -> command (Some n)) (open_nums op)
  (* Scala Board/Bot: a hidden Shade Stalker cannot be targeted. *)
  | Select_target_creature -> List.map (fun n -> command (Some n)) (Shadowmage.visible_creatures op)
  | Select_any_creature ->
      List.map (fun n -> command (Some n)) (filled_nums p)
      @ List.map (fun n -> command ~flag:(Some Target_other) (Some n)) (Shadowmage.visible_creatures op)

let place_rank ~enemy (cmd : command) =
  match cmd.cmd_input with
  | None -> (0, 0, 0)
  | Some num -> (
      match cmd.cmd_card.input_spec with
      | Select_owner_slot ->
          let blocked = if in_slot_range num && Option.is_some enemy.ps_slots.(num) then 1 else 0 in
          (blocked, num, 0)
      | Select_target_creature | Select_target _ ->
          let life = match enemy.ps_slots.(num) with Some st -> st.life | None -> 0 in
          (0, life, num)
      | _ -> (0, num, 0))

let group_by_card commands =
  let tbl = Hashtbl.create 16 in
  let ids = ref [] in
  List.iter
    (fun c ->
      let id = c.cmd_card.id in
      match Hashtbl.find_opt tbl id with
      | Some xs -> Hashtbl.replace tbl id (c :: xs)
      | None ->
          Hashtbl.add tbl id [ c ];
          ids := id :: !ids)
    commands;
  List.rev_map (fun id -> List.rev (Hashtbl.find tbl id)) !ids

let interleave groups =
  let rec go acc gs =
    match List.filter_map (function x :: xs -> Some (x, xs) | [] -> None) gs with
    | [] -> List.rev acc
    | heads -> go (List.rev_append (List.map fst heads) acc) (List.map snd heads)
  in
  go [] groups

let order_moves ~enemy commands =
  group_by_card commands
  |> List.map (List.stable_sort (fun a b -> compare (place_rank ~enemy a) (place_rank ~enemy b)))
  |> List.sort (fun a b ->
      let ca = (List.hd a).cmd_card and cb = (List.hd b).cmd_card in
      let c = compare cb.cost ca.cost in
      if c <> 0 then c else compare ca.id cb.id)
  |> interleave

let player_houses pid (desc : game_desc) (state : game_state) =
  let d = desc.gd_players.(pid) in
  match state.players.(pid).ps_desc.desc_mods with
  | [] -> Array.to_list d.pd_houses
  | mods ->
      Array.to_list d.pd_houses
      |> List.map (fun h -> { h with phd_cards = List.fold_left (fun cards m -> m h cards) h.phd_cards mods })

let expand_flags (cmd : command) =
  if Meteomancer.is_meteo cmd.cmd_card && cmd.cmd_flag = None then
    List.init 4 (fun i -> { cmd with cmd_flag = Some (Called_element i) })
  else [ cmd ]

let legal_commands pid (desc : game_desc) (state : game_state) =
  let p = state.players.(pid) in
  let houses = player_houses pid desc state in
  let raw =
    List.concat_map
      (fun h ->
        Array.to_list h.phd_cards
        |> List.filter (fun cd -> is_available cd p.ps_houses.(h.phd_house.h_index))
        |> List.concat_map (commands_for pid state)
        |> List.concat_map expand_flags)
      houses
  in
  order_moves ~enemy:state.players.(other pid) raw

let available_commands (u : updater) (p : player_update) =
  (* Cheapest card, frontmost open slot — not Choices interleaving. Live desc
     mods (Cosmic Mage, Fateweaver, Sealing) are applied so extra-phase offers
     show up the same way they do in [legal_commands]. *)
  let pstate = p.pstate in
  let open_slots = Slots.get_open p.slots_update in
  let houses =
    match pstate.ps_desc.desc_mods with
    | [] -> Array.to_list u.desc.gd_players.(p.pid).pd_houses
    | mods ->
        Array.to_list u.desc.gd_players.(p.pid).pd_houses
        |> List.map (fun h ->
            { h with phd_cards = List.fold_left (fun cards m -> m h cards) h.phd_cards mods })
  in
  let command ?(flag = None) cd input =
    { cmd_player = p.pid; cmd_card = cd.cd_card; cmd_input = input; cmd_cost = cd.cd_cost; cmd_flag = flag }
  in
  houses
  |> List.concat_map (fun h ->
      Array.to_list h.phd_cards
      |> List.filter (fun cd -> is_available cd pstate.ps_houses.(h.phd_house.h_index))
      |> List.concat_map (fun cd ->
          let filled (pl : player_update) =
            Array.to_list pl.slots_update.slot_updates
            |> List.filter_map (fun s -> if Option.is_some s.value then Some s.num else None)
          in
          (* Scala Board/Bot: a hidden Shade Stalker cannot be targeted. *)
          let visible_foes =
            Array.to_list (Player.other p).slots_update.slot_updates
            |> List.filter_map (fun s ->
                match s.value with Some st when st.data <> Stealthed -> Some s.num | _ -> None)
          in
          match cd.cd_card.input_spec with
          | Select_owner_slot -> List.map (fun n -> command cd (Some n)) open_slots
          | Select_owner_creature -> List.map (fun n -> command cd (Some n)) (filled p)
          | Select_target_creature -> List.map (fun n -> command cd (Some n)) visible_foes
          | Select_target_slot ->
              (* Bot.scala:229: the opponent's open slots *)
              List.map (fun n -> command cd (Some n)) (Slots.get_open (Player.other p).slots_update)
          | Select_any_creature ->
              List.map (fun n -> command cd (Some n)) (filled p)
              @ List.map (fun n -> command ~flag:(Some Target_other) cd (Some n)) visible_foes
          | Select_owner f -> List.map (fun n -> command cd (Some n)) (f p.pid u.state)
          | Select_target f -> List.map (fun n -> command cd (Some n)) (f (other p.pid) u.state)
          | No_input -> [ command cd None ])
      |> List.concat_map expand_flags)
  |> List.stable_sort (fun a b -> compare a.cmd_cost b.cmd_cost)

let bot_move u p = List.nth_opt (available_commands u p) 0

type mover = updater -> player_id -> command option

let greedy_mover u pid = bot_move u (Updater.player u pid)

(* BotSimulator.simulateCommand: one command, then the end-turn chain unless a
   WaitPlayer transition hands over. *)
type sim = { state : game_state; next : player_id; ended : player_id option }

(* Simulations draw their own dice so that bot search never consumes the live
   game's rng: a seeded game replays the same whatever the search budget. *)
let sim_rng = lazy (Dice.make_self ())

let simulate ?rng (desc : game_desc) (state : game_state) pid cmd =
  let rng = match rng with Some r -> r | None -> Lazy.force sim_rng in
  let u = Updater.create ~rng state desc in
  let next = ref (other pid) in
  let _, state =
    Updater.lift u (fun u ->
        let p = Updater.player u pid in
        Player.submit p cmd;
        Array.iter Player.flush u.player_updates;
        match Player.pop_transition p with
        | Some (Wait_player (n, _)) -> next := n
        | None ->
            Player.run_slots p;
            if Option.is_none (Updater.ended u) then (
              Player.apply_effects p On_end_turn;
              Slots.toggle_run p.slots_update;
              let op = Player.other p in
              Player.prepare_next_turn op;
              if Option.is_none (Updater.ended u) then Player.apply_effects op On_turn);
            next := other pid)
  in
  { state; next = !next; ended = Updater.ended u }

(* ------------------------------------------------------------------- driver *)

(* HeadlessGame's `sync`: run one engine step, then publish the state *)
let step u f = ignore (Updater.lift u f)
