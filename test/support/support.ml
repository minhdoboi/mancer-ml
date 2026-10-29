(* Fixtures shared by the Alcotest suites and the expect tests. *)

open Mancer
open Sp
open Engine

let all_houses =
  [
    Fire.house;
    Anger.house;
    Water.house;
    Sadness.house;
    Air.house;
    Fear.house;
    Earth.house;
    Joy.house;
    Troopers.house;
    Zen.house;
    Wind.house;
    Chronomaster.house;
    Disciples.house;
    Darksider.house;
    Faerie.house;
    Fairy.house;
    Dudes.house;
    Vampires.house;
    Warp.house;
    Snowblood.house;
    Dreamweaver.house;
    Junk.house;
    Battlemage.house;
    Antimancer.house;
    Powermancer.house;
    Shadowmage.house;
    Colors.house;
    Limbo.house;
    Rogue.house;
    Tinker.house;
    Entomologist.house;
    Soulbinder.house;
    Shadowcaster.house;
    Spiritshaman.house;
    Conjurer.house;
    Quantumancer.house;
    Meteomancer.house;
    Seasons.house;
    Soulreaper.house;
    Sower.house;
    Shaman.house;
    Mountainking.house;
    Darkpriest.house;
    Elementalist.house;
    Highpriest.house;
    Kinetician.house;
    Emotion.house;
    Paganist.house;
    Kahjivir.house;
    Blacksmith.house;
    Trappers.house;
    Beastlord.house;
    Lightstave.house;
  ]

let init_all_houses () =
  Game.init_cards Fire.house;
  Game.init_cards Anger.house;
  Game.init_cards Water.house;
  Game.init_cards Sadness.house;
  Game.init_cards Air.house;
  Game.init_cards Fear.house;
  Game.init_cards Earth.house;
  Game.init_cards Joy.house;
  Game.init_cards ~cost_fn:Troopers.conscription_cost Troopers.house;
  Game.init_cards Zen.house;
  Game.init_cards Wind.house;
  Game.init_cards Chronomaster.house;
  Game.init_cards Disciples.house;
  Game.init_cards Darksider.house;
  Game.init_cards Faerie.house;
  Game.init_cards Fairy.house;
  Game.init_cards Dudes.house;
  Game.init_cards Vampires.house;
  Game.init_cards Warp.house;
  Game.init_cards Snowblood.house;
  Game.init_cards Dreamweaver.house;
  Game.init_cards Junk.house;
  Game.init_cards Battlemage.house;
  Game.init_cards ~cost_fn:Antimancer.cost_fn Antimancer.house;
  Game.init_cards Powermancer.house;
  Game.init_cards Shadowmage.house;
  Game.init_cards ~cost_fn:Colors.cost_fn Colors.house;
  Game.init_cards Limbo.house;
  Game.init_cards Rogue.house;
  Game.init_cards Tinker.house;
  Game.init_cards Entomologist.house;
  Game.init_cards Soulbinder.house;
  Game.init_cards Shadowcaster.house;
  Game.init_cards Spiritshaman.house;
  Game.init_cards Conjurer.house;
  Game.init_cards Quantumancer.house;
  Game.init_cards Meteomancer.house;
  Game.init_cards Seasons.house;
  Game.init_cards Soulreaper.house;
  Game.init_cards Sower.house;
  Game.init_cards Shaman.house;
  Game.init_cards Mountainking.house;
  Game.init_cards Darkpriest.house;
  Game.init_cards Elementalist.house;
  Game.init_cards Highpriest.house;
  Game.init_cards Kinetician.house;
  Game.init_cards Emotion.house;
  Game.init_cards Paganist.house;
  Game.init_cards Kahjivir.house;
  Game.init_cards Blacksmith.house;
  Game.init_cards ~cost_fn:Trappers.cost_fn Trappers.house;
  Game.init_cards ~cost_fn:Beastlord.cost_fn Beastlord.house;
  Game.init_cards Lightstave.house

let card_by_key key =
  all_houses
  |> List.find_map (fun (h : house) ->
      List.find_opt (fun (c : card) -> c.key = key) (h.h_cards @ h.h_additional_cards))
  |> function
  | Some c -> c
  | None -> failwith ("unknown card " ^ key)

let new_game ?(mana0 = 6) ?mana ?(house = Fire.house) ?(special = Game.empty_special) () =
  init_all_houses ();
  let d = Game.make_player_desc ~special house in
  let m = match mana with Some xs -> xs | None -> [ mana0; 4; 4; 4 ] in
  let mk () = Game.initial_player_state m d in
  Updater.create { players = [| mk (); mk () |] } { gd_players = [| d; d |] }

let new_duel ?(mana = [ 6; 4; 4; 4; 6 ]) ~special0 ~special1 () =
  init_all_houses ();
  let d0 = Game.make_player_desc ~special:special0 Fire.house in
  let d1 = Game.make_player_desc ~special:special1 Fire.house in
  let mk d = Game.initial_player_state mana d in
  Updater.create { players = [| mk d0; mk d1 |] } { gd_players = [| d0; d1 |] }

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
let play ?(flag = None) u pid key ~input ~cost =
  step u (fun () ->
      ignore
        (Player.submit_command (player u pid)
           {
             cmd_player = pid;
             cmd_card = card_by_key key;
             cmd_input = input;
             cmd_cost = cost;
             cmd_flag = flag;
           }))

let print_board u = print_endline (state_to_string u.state)
