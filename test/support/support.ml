(* Fixtures shared by the Alcotest suites and the expect tests. *)

open Mancer
open Sp
open Engine

let all_houses = [ Fire.house; Water.house; Air.house; Earth.house; Troopers.house ]

let init_all_houses () =
  Game.init_cards Fire.house;
  Game.init_cards Water.house;
  Game.init_cards Air.house;
  Game.init_cards Earth.house;
  Game.init_cards ~cost_fn:Troopers.conscription_cost Troopers.house

let card_by_key key =
  all_houses
  |> List.find_map (fun (h : house) ->
      List.find_opt (fun (c : card) -> c.key = key) (h.h_cards @ h.h_additional_cards))
  |> function
  | Some c -> c
  | None -> failwith ("unknown card " ^ key)

let new_game ?(mana0 = 6) ?mana ?(house = Fire.house) () =
  init_all_houses ();
  let d = Game.make_player_desc house in
  let m = match mana with Some xs -> xs | None -> [ mana0; 4; 4; 4 ] in
  let mk () = Game.initial_player_state m d in
  Updater.create { players = [| mk (); mk () |] } { gd_players = [| d; d |] }

let step u f = Game.step u (fun _ -> f ())
let player = Updater.player
let slot u pid num = (player u pid).slots_update.slot_updates.(num)
let occupant u pid num = Option.map (fun st -> st.card.c_card.key) (slot u pid num).value
let is_empty u pid num = Option.is_none (slot u pid num).value
let life_at u pid num = match (slot u pid num).value with Some st -> st.life | None -> -1
let attack_at u pid num = match (slot u pid num).value with Some st -> st.attack | None -> -1
let player_life u pid = (player u pid).pstate.ps_life

(* Places a creature directly on the board, bypassing the bot.  It gets the
   run flag, which the real game grants at end of turn via toggle_run. *)
let place ?(run = true) u pid key num =
  let c = make_creature (card_by_key key) in
  let c = if run then { c with c_body = { c.c_body with b_status = run_flag } } else c in
  ignore (Slots.summon (player u pid).slots_update num c)

(* Plays a card through the real command path, Direct effects included *)
let play u pid key ~input ~cost =
  step u (fun () ->
      ignore
        (Player.submit_command (player u pid)
           {
             cmd_player = pid;
             cmd_card = card_by_key key;
             cmd_input = input;
             cmd_cost = cost;
             cmd_flag = None;
           }))

let print_board u = print_endline (state_to_string u.state)
