(* Deck building, a simple bot, and the headless turn loop.  Mirrors
   priv/sp/CardShuffle.scala and HeadlessGame in priv/sp/bot/Tournament.scala.

   Scala's deck builder uses ChocoSolver to satisfy deck constraints; here
   decks are a fixed slice per house so runs are reproducible. *)

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
  List.iter
    (fun (c : card) ->
      c.house_index <- h.h_index;
      c.house_id <- h.h_house_id)
    h.h_additional_cards

let deck_costs = [ 1; 2; 3; 4; 5; 6 ]
let is_special (h : house) = h.h_index = 4

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
    h_index = 4;
    h_effects = [];
    h_house_id = 0;
    h_additional_cards = [];
  }

let make_player_desc ?(special = empty_special) (h : house) =
  {
    pd_houses =
      [|
        { phd_house = h; phd_cards = deck_of_house h };
        { phd_house = special; phd_cards = deck_of_house special };
      |];
  }

let initial_player_state (mana : int list) (d : player_desc) =
  {
    ps_houses = Array.of_list (List.map (fun mana -> { mana; growth = 1 }) (mana @ [ 2 ]));
    ps_desc = { init_desc = d; desc_mods = [] };
    ps_slots = Array.make nb_slots None;
    ps_slot_list = base_slot_list;
    ps_life = 60;
    ps_effects = [];
    ps_data = No_player_data;
    ps_transitions = [];
  }

let initial_game_state (desc : game_desc) =
  {
    players =
      [|
        initial_player_state [ 2; 2; 2; 2 ] desc.gd_players.(0);
        initial_player_state [ 1; 1; 1; 1 ] desc.gd_players.(1);
      |];
  }

(* ------------------------------------------------------------------ the bot *)

(* Deterministic greedy bot: cheapest legal card, frontmost open slot.  Good
   enough to exercise the engine; not a port of the Scala heuristics. *)

let available_commands (u : updater) (p : player_update) =
  let pstate = u.state.players.(p.pid) in
  let open_slots = Slots.get_open p.slots_update in
  let command cd input =
    { cmd_player = p.pid; cmd_card = cd.cd_card; cmd_input = input; cmd_cost = cd.cd_cost; cmd_flag = None }
  in
  Array.to_list u.desc.gd_players.(p.pid).pd_houses
  |> List.concat_map (fun h ->
      Array.to_list h.phd_cards
      |> List.filter (fun cd -> is_available cd pstate.ps_houses.(h.phd_house.h_index))
      |> List.concat_map (fun cd ->
          let filled p =
            Array.to_list p.slots_update.slot_updates
            |> List.filter_map (fun s -> if Option.is_some s.value then Some s.num else None)
          in
          match cd.cd_card.input_spec with
          | Select_owner_slot -> List.map (fun n -> command cd (Some n)) open_slots
          | Select_owner_creature -> List.map (fun n -> command cd (Some n)) (filled p)
          | Select_target_creature | Select_target_slot ->
              List.map (fun n -> command cd (Some n)) (filled (Player.other p))
          | Select_any_creature -> List.map (fun n -> command cd (Some n)) (filled p @ filled (Player.other p))
          | _ -> [ command cd None ]))
  |> List.stable_sort (fun a b -> compare a.cmd_cost b.cmd_cost)

let bot_move u p = List.nth_opt (available_commands u p) 0

(* ------------------------------------------------------------------- driver *)

type result = { winner : player_id option; turns : int }

(* HeadlessGame's `sync`: run one engine step, then publish the state *)
let step u f = ignore (Updater.lift u f)

let play_game (desc : game_desc) ~max_turns ~verbose =
  events_enabled := verbose;
  let u = Updater.create (initial_game_state desc) desc in
  let running () = Option.is_none (Updater.ended u) in
  step u (fun u -> Array.iter (fun p -> Player.apply_effects p On_start) u.player_updates);
  (* t = turns played so far, as in HeadlessGame: at most max_turns moves *)
  let rec loop t pid =
    if (not (running ())) || t >= max_turns then t
    else
      let p = Updater.player u pid in
      let cmd = bot_move u p in
      step u (fun _ -> Player.submit p cmd);
      (* Drain transitions: None runs the turn out *)
      let rec drain () =
        match Player.pop_transition p with
        | Some _ -> drain ()
        | None ->
            let op = Player.other p in
            List.iter
              (fun f -> if running () then step u (fun _ -> f ()))
              [
                (fun () -> Player.run_slots p);
                (fun () -> Player.apply_effects p On_end_turn);
                (fun () -> Slots.toggle_run p.slots_update);
                (fun () -> Player.prepare_next_turn op);
                (fun () -> Player.apply_effects op On_turn);
              ]
      in
      drain ();
      loop (t + 1) (other pid)
  in
  let turns = loop 0 owner in
  events_enabled := false;
  { winner = Updater.ended u; turns }
