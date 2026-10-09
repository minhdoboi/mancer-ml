(* Replay a recorded game and dump board snapshots. Used by the Scala/OCaml
   compare harness: both engines play the same decks and moves, then the first
   differing turn is the report. *)

open Sp
open Engine

type occupant = { card_key : string; life : int; attack : int; status : int }
type side = { life : int; mana : int list; growth : int list; slots : occupant option array }
type snap = { t : int; player : int option; after : string; ended : int option; sides : side array }
type mismatch = { t : int; message : string }

let occupant_of = function
  | None -> None
  | Some (st : slot_state) ->
      Some { card_key = st.card.c_card.key; life = st.life; attack = st.attack; status = st.status }

let side_of (p : player_state) =
  {
    life = p.ps_life;
    mana = Array.to_list p.ps_houses |> List.map (fun (h : house_state) -> h.mana);
    growth = Array.to_list p.ps_houses |> List.map (fun (h : house_state) -> h.growth);
    slots = Array.map occupant_of p.ps_slots;
  }

let of_state ~t ?player ~after ~ended (state : game_state) =
  { t; player; after; ended; sides = Array.map side_of state.players }

let player_desc_of cat (raw : Game_record.raw_deck) =
  let house (hname, cards) =
    let h = Catalog.find_house cat hname in
    let card n =
      let c = Catalog.find_card_in_house h n in
      { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 }
    in
    { phd_house = h; phd_cards = Array.of_list (List.map card cards) }
  in
  let by_index a b = compare a.phd_house.h_index b.phd_house.h_index in
  { pd_houses = Array.of_list (List.sort by_index (List.map house raw)) }

let desc_of cat (p : Game_record.parsed) =
  if Array.length p.decks < 2 then failwith "Replay: game record needs two decks";
  { gd_players = [| player_desc_of cat p.decks.(0); player_desc_of cat p.decks.(1) |] }

let card_in_desc (pd : player_desc) name =
  pd.pd_houses
  |> Array.find_map (fun h ->
      Array.find_map
        (fun cd -> if Catalog.same_card cd.cd_card name then Some cd.cd_card else None)
        h.phd_cards)

let command_of (desc : game_desc) (m : Game_record.move) =
  match m.card with
  | None -> None
  | Some name ->
      let card =
        match card_in_desc desc.gd_players.(m.player) name with
        | Some c -> c
        | None -> failwith (Printf.sprintf "Replay: unknown card %s for player %d" name m.player)
      in
      let flag = match m.side with Some "opp" -> Some Target_other | _ -> None in
      Some
        {
          cmd_player = m.player;
          cmd_card = card;
          cmd_input = m.slot;
          cmd_cost = Option.value m.cost ~default:card.cost;
          cmd_flag = flag;
        }

let run ?(cat = Catalog.default ()) (p : Game_record.parsed) =
  Catalog.init ();
  let desc = desc_of cat p in
  let mana i default = if Array.length p.mana > i && p.mana.(i) <> [] then p.mana.(i) else default in
  let mana0 = mana 0 [ 2; 2; 2; 2 ] and mana1 = mana 1 [ 1; 1; 1; 1 ] in
  let u = Updater.create ~rng:(Dice.make p.rng_seed) (Game.initial_game_state ~mana0 ~mana1 desc) desc in
  let snaps = ref [] in
  let push t player after = snaps := of_state ~t ?player ~after ~ended:(Updater.ended u) u.state :: !snaps in
  Session.start u;
  push 0 None "start";
  List.iter
    (fun (m : Game_record.move) ->
      if Option.is_none (Updater.ended u) then (
        let pl = Updater.player u m.player in
        let cmd = command_of desc m in
        Game.step u (fun _ -> Player.submit pl cmd);
        ignore (Session.drain u m.player);
        push m.t (Some m.player) (Option.value m.card ~default:"pass")))
    p.moves;
  List.rev !snaps

let generate ?(special = "Troopers") ?(max_turns = 80) ~seed () =
  Catalog.init ();
  let spec = Catalog.find_special special in
  let rng = Random.State.make [| seed |] in
  let p1, p2 = Shuffle.two_players ~rng ~specials:(spec, spec) Catalog.basics in
  let desc = { gd_players = [| p1.desc; p2.desc |] } in
  let r =
    Session.play_game desc ~mana0:(Array.to_list p1.mana) ~mana1:(Array.to_list p2.mana) ~rng:(Dice.make seed)
      ~max_turns ~verbose:false
  in
  ({
     Game_record.game = string_of_int seed;
     bot_names = ("greedy", "greedy");
     starting_player = owner;
     special_names = (Game_record.special_name p1.desc, Game_record.special_name p2.desc);
     decks = desc.gd_players;
     mana = [| Array.to_list p1.mana; Array.to_list p2.mana |];
     rng_seed = seed;
     winner_bot = (match r.winner with Some _ -> Some "greedy" | None -> None);
     winner_player = r.winner;
     draw = Option.is_none r.winner;
     turns = r.turns;
     reason = r.reason;
     moves = r.moves;
   }
    : Game_record.t)

(* ---------------------------------------------------------------- json *)

let ints xs = `List (List.map (fun i -> `Int i) xs)

let occ_to_json : occupant option -> Yojson.Basic.t = function
  | None -> `Null
  | Some o ->
      `Assoc
        [
          ("card", `String o.card_key);
          ("life", `Int o.life);
          ("attack", `Int o.attack);
          ("status", `Int o.status);
        ]

let side_to_json (s : side) : Yojson.Basic.t =
  `Assoc
    [
      ("life", `Int s.life);
      ("mana", ints s.mana);
      ("growth", ints s.growth);
      ("slots", `List (Array.to_list s.slots |> List.map occ_to_json));
    ]

let snap_to_json (s : snap) : Yojson.Basic.t =
  let player = match s.player with None -> [] | Some p -> [ ("player", `Int p) ] in
  `Assoc
    (player
    @ [
        ("t", `Int s.t);
        ("after", `String s.after);
        ("ended", match s.ended with None -> `Null | Some p -> `Int p);
        ("sides", `List (Array.to_list s.sides |> List.map side_to_json));
      ])

(* One snapshot per line so the Scala and OCaml dumps diff cleanly. *)
let snaps_to_json snaps =
  "[\n" ^ String.concat ",\n" (List.map (fun s -> Yojson.Basic.to_string (snap_to_json s)) snaps) ^ "\n]\n"

open Yojson.Basic.Util

let occ_of_json = function
  | `Null -> None
  | j ->
      let int k = member k j |> to_int in
      Some
        {
          card_key = member "card" j |> to_string;
          life = int "life";
          attack = int "attack";
          status = int "status";
        }

let side_of_json j : side =
  {
    life = member "life" j |> to_int;
    mana = member "mana" j |> to_list |> filter_int;
    growth = member "growth" j |> to_list |> filter_int;
    slots = member "slots" j |> to_list |> List.map occ_of_json |> Array.of_list;
  }

let snap_of_json j : snap =
  {
    t = member "t" j |> to_int;
    player = member "player" j |> to_int_option;
    after = member "after" j |> to_string_option |> Option.value ~default:"";
    ended = member "ended" j |> to_int_option;
    sides = member "sides" j |> to_list |> List.map side_of_json |> Array.of_list;
  }

let snaps_of_json text =
  match Yojson_five.Basic.from_string text with
  | Ok j -> to_list j |> List.map snap_of_json
  | Error e -> failwith ("Replay: " ^ e)

(* ---------------------------------------------------------------- diff *)

let ints_str xs = String.concat "," (List.map string_of_int xs)
let opt_str = function None -> "none" | Some p -> string_of_int p
let first n f = Seq.find_map f (Seq.init n Fun.id)

let fmt_occ = function
  | None -> "empty"
  | Some o -> Printf.sprintf "%s life=%d attack=%d status=%d" o.card_key o.life o.attack o.status

let describe_side i (x : side) (y : side) =
  if x.life <> y.life then Some (Printf.sprintf "p%d life %d vs %d" i x.life y.life)
  else if x.mana <> y.mana then
    Some (Printf.sprintf "p%d mana %s vs %s" i (ints_str x.mana) (ints_str y.mana))
  else if x.growth <> y.growth then
    Some (Printf.sprintf "p%d growth %s vs %s" i (ints_str x.growth) (ints_str y.growth))
  else if Array.length x.slots <> Array.length y.slots then Some (Printf.sprintf "p%d slot count" i)
  else
    first (Array.length x.slots) (fun n ->
        if x.slots.(n) = y.slots.(n) then None
        else
          Some
            (Printf.sprintf "p%d slot %d: ocaml %s / scala %s" i n
               (fmt_occ x.slots.(n))
               (fmt_occ y.slots.(n))))

let describe (a : snap) (b : snap) =
  if a.ended <> b.ended then Printf.sprintf "ended %s vs %s" (opt_str a.ended) (opt_str b.ended)
  else if Array.length a.sides <> Array.length b.sides then
    Printf.sprintf "side count %d vs %d" (Array.length a.sides) (Array.length b.sides)
  else
    first (Array.length a.sides) (fun i -> describe_side i a.sides.(i) b.sides.(i))
    |> Option.value ~default:(Printf.sprintf "t %d vs %d" a.t b.t)

(* [player] and [after] are labels, not engine state: only t, ended and the board are compared. *)
let same (a : snap) (b : snap) = a.t = b.t && a.ended = b.ended && a.sides = b.sides

let diff ml sc =
  let rec go last_t = function
    | [], [] -> None
    | (a : snap) :: xs, b :: ys ->
        if same a b then go a.t (xs, ys)
        else Some { t = a.t; message = Printf.sprintf "after %s: %s" a.after (describe a b) }
    | _ ->
        Some
          {
            t = last_t;
            message = Printf.sprintf "snapshot count: ocaml %d scala %d" (List.length ml) (List.length sc);
          }
  in
  go 0 (ml, sc)
