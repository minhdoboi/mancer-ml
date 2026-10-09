(* What a bot may know about the opponent's deck: only the cards it has seen
   played, like Scala's [BotKnowledge]/[CardGuess]. The engine hands every mover
   the real [game_desc]; these helpers swap the opponent's deck for a guess so a
   bot never searches on cards it could not have seen.

   Scala solves a constraint model for an oversized deck holding every still
   possible card. Here the single-deck guess is the union of a few legal
   four-cards-per-house deals from {!Shuffle}, each containing every seen card;
   Diviner keeps the deals apart as separate worlds. *)

open Sp

(* Opponent cards seen so far, per updater and seat: the listener is wrapped
   once per seat so summons and spells that have since died or resolved are
   still remembered. Two bots sharing a game each keep their own record; a new
   updater (a new game) drops the old ones. *)
let tracked : (updater * player_id * (int, card) Hashtbl.t) list ref = ref []

let track_opponent (u : updater) ~pid =
  match List.find_opt (fun (u', p, _) -> u' == u && p = pid) !tracked with
  | Some (_, _, seen) -> seen
  | None ->
      let seen = Hashtbl.create 16 in
      let note (c : card) = Hashtbl.replace seen c.id c in
      let prev = u.listener in
      u.listener <-
        (fun e ->
          (match e with
          | Ev_summon { player; creature; _ } when player <> pid -> note creature.c_card
          | Ev_spell_played c when c.cmd_player <> pid -> note c.cmd_card
          | Ev_submitted { player; command = Some c; _ } when player <> pid -> note c.cmd_card
          | _ -> ());
          prev e);
      tracked := (u, pid, seen) :: List.filter (fun (u', _, _) -> u' == u) !tracked;
      seen

(* Every opponent card seen: remembered events plus what is on their board now. *)
let seen_cards (u : updater) ~pid =
  let seen = track_opponent u ~pid in
  Array.iter
    (function
    | Some (s : slot_state) when s.data <> Stealthed -> Hashtbl.replace seen s.card.c_card.id s.card.c_card
    | _ -> ())
    u.state.players.(other pid).ps_slots;
  Hashtbl.fold (fun _ c acc -> c :: acc) seen []

(* Per-house 1-based card indexes of [seen] cards that belong to [opp]'s deck
   (summoned tokens and other non-deck cards drop out). *)
let required_indexes (opp : player_desc) seen =
  let req = Array.make 5 [] in
  List.iter
    (fun (c : card) ->
      Array.iteri
        (fun i ph ->
          if List.exists (fun (x : card) -> x.id = c.id) ph.phd_house.h_cards then
            req.(i) <- List.sort_uniq compare (card_index1 c :: req.(i)))
        opp.pd_houses)
    seen;
  req

(* Legal opponent decks consistent with what was seen: [seen] cards are forced
   into every sample. The real deck is never among them. Empty when no legal
   deck can be dealt. *)
let sample ~desc ~bot_id ~seen ~n rng =
  let bot = desc.gd_players.(bot_id) in
  let opp = desc.gd_players.(other bot_id) in
  let special = opp.pd_houses.(4).phd_house in
  let require = required_indexes opp seen in
  let deal require =
    (* [Shuffle] fails with [Failure] when it cannot build a legal deck; anything else is a bug. *)
    try Some (Shuffle.create_player ~rng ~exclude:bot ~require ~first:false ~special Catalog.basics)
    with Failure _ -> None
  in
  let key (d : Shuffle.deal) =
    Array.to_list d.desc.pd_houses
    |> List.concat_map (fun h -> Array.to_list h.phd_cards |> List.map (fun cd -> cd.cd_card.id))
    |> List.sort compare |> List.map string_of_int |> String.concat ","
  in
  let seen_keys = Hashtbl.create 8 in
  let rec go require acc found i =
    if found >= n || i > n * 4 then List.rev acc
    else
      match deal require with
      | None -> go require acc found (i + 1)
      | Some d ->
          let k = key d in
          if Hashtbl.mem seen_keys k then go require acc found (i + 1)
          else (
            Hashtbl.add seen_keys k ();
            go require (d.desc :: acc) (found + 1) (i + 1))
  in
  match go require [] 0 0 with
  | [] -> go (Array.make 5 []) [] 0 0 (* seen cards cannot form a legal deck: sample unconstrained *)
  | l -> l

let seen_key seen =
  seen
  |> List.map (fun (c : card) -> c.id)
  |> List.sort_uniq compare |> List.map string_of_int |> String.concat ","

(* Several legal decks merged house by house, like Scala's oversized
   [CardGuess.createAIPlayer]: the bot can answer with any card one of them
   holds, so a real card missed by a single deal is less likely to be ignored.
   Seen cards are in every sample, so they are in the union. *)
let superset_samples = ref 4

(* BELIEF_SAMPLES=n overrides the deal count for tournaments; 0 makes {!guess}
   return the real deck, only to measure what guessing costs. *)
let () = Option.iter (fun v -> superset_samples := int_of_string v) (Sys.getenv_opt "BELIEF_SAMPLES")

let superset = function
  | [] -> None
  | first :: _ as decks ->
      let house i =
        let tbl = Hashtbl.create 8 in
        List.iter
          (fun d -> Array.iter (fun cd -> Hashtbl.replace tbl cd.cd_card.id cd) d.pd_houses.(i).phd_cards)
          decks;
        Hashtbl.fold (fun _ cd acc -> cd :: acc) tbl []
        |> List.sort (fun a b -> compare (card_index1 a.cd_card) (card_index1 b.cd_card))
        |> Array.of_list
      in
      Some { pd_houses = Array.mapi (fun i ph -> { ph with phd_cards = house i }) first.pd_houses }

(* [visible] and [desc] with the opponent of [bot_id] replaced by [belief]. *)
let with_belief visible desc bot_id belief =
  let opp = other bot_id in
  let p = visible.players.(opp) in
  let players = Array.copy visible.players in
  (* A stealthed Shade Stalker is not on the bot's picture of the board: its
     slot reads as open, so neither targeting nor slot choice can betray it. *)
  let ps_slots =
    Array.map (function Some st when st.data = Stealthed -> None | s -> s) p.ps_slots
  in
  players.(opp) <- { p with ps_slots; ps_desc = { init_desc = belief; desc_mods = p.ps_desc.desc_mods } };
  let gd = { gd_players = Array.copy desc.gd_players } in
  gd.gd_players.(opp) <- belief;
  ({ players }, gd)

(* One guessed deck per seat and set of seen cards, so the bot plays against a
   stable opponent until a new card is revealed (Scala regenerates [k] the
   same way). *)
let cache : (updater * player_id * string * player_desc) list ref = ref []

let guess (u : updater) ~pid =
  let seen = seen_cards u ~pid in
  let key = seen_key seen in
  if !superset_samples <= 0 then u.desc.gd_players.(other pid)
  else
    match List.find_opt (fun (u', p, k, _) -> u' == u && p = pid && k = key) !cache with
    | Some (_, _, _, belief) -> belief
    | None -> (
        let rng = Random.State.make_self_init () in
        match superset (sample ~desc:u.desc ~bot_id:pid ~seen ~n:!superset_samples rng) with
        | Some belief ->
            cache := (u, pid, key, belief) :: List.filter (fun (u', p, _, _) -> u' == u && p <> pid) !cache;
            belief
        | None -> u.desc.gd_players.(other pid)
        (* no legal deck at all: nothing better to guess *))

(* Run a perfect-information [execute] on the state as the bot can know it. *)
let blind execute (u : updater) pid =
  let state, desc = with_belief u.state u.desc pid (guess u ~pid) in
  execute ~pid ~desc state
