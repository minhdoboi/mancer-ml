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
  let phds =
    List.map
      (fun (hname, cards) ->
        let h = Catalog.find_house cat hname in
        let phd_cards =
          cards
          |> List.map (fun n ->
              let c = Catalog.find_card_in_house h n in
              { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 })
          |> Array.of_list
        in
        { phd_house = h; phd_cards })
      raw
  in
  let by_index = Array.make 5 None in
  List.iter
    (fun phd ->
      let i = phd.phd_house.h_index in
      if i >= 0 && i < 5 then by_index.(i) <- Some phd)
    phds;
  let ordered = Array.to_list by_index |> List.filter_map Fun.id in
  { pd_houses = Array.of_list (if ordered = [] then phds else ordered) }

let desc_of cat (p : Game_record.parsed) =
  if Array.length p.decks < 2 then failwith "Replay: game record needs two decks";
  { gd_players = [| player_desc_of cat p.decks.(0); player_desc_of cat p.decks.(1) |] }

let card_in_desc (pd : player_desc) name =
  let rec houses i =
    if i >= Array.length pd.pd_houses then None
    else
      let h = pd.pd_houses.(i) in
      let rec cards j =
        if j >= Array.length h.phd_cards then houses (i + 1)
        else
          let c = h.phd_cards.(j).cd_card in
          if Catalog.same_card c name then Some c else cards (j + 1)
      in
      cards 0
  in
  houses 0

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

let drain u pid =
  let rec loop pid =
    if Option.is_some (Updater.ended u) then pid
    else
      let popped = ref None in
      Game.step u (fun _ -> popped := Player.pop_transition (Updater.player u pid));
      match !popped with
      | Some (Wait_player (next, _)) -> next
      | None ->
          let p = Updater.player u pid in
          let op = Player.other p in
          let running () = Option.is_none (Updater.ended u) in
          List.iter
            (fun f -> if running () then Game.step u (fun _ -> f ()))
            [
              (fun () -> Player.run_slots p);
              (fun () -> Player.apply_effects p On_end_turn);
              (fun () -> Slots.toggle_run p.slots_update);
              (fun () -> Player.prepare_next_turn op);
              (fun () -> Player.apply_effects op On_turn);
            ];
          other pid
  in
  loop pid

let run ?(cat = Catalog.default ()) (p : Game_record.parsed) =
  Catalog.init ();
  let desc = desc_of cat p in
  let mana0 = if Array.length p.mana < 1 || p.mana.(0) = [] then [ 2; 2; 2; 2 ] else p.mana.(0) in
  let mana1 = if Array.length p.mana < 2 || p.mana.(1) = [] then [ 1; 1; 1; 1 ] else p.mana.(1) in
  let u = Updater.create ~rng:(Dice.make p.rng_seed) (Game.initial_game_state ~mana0 ~mana1 desc) desc in
  let snaps = ref [] in
  let push t player after = snaps := of_state ~t ?player ~after ~ended:(Updater.ended u) u.state :: !snaps in
  Game.step u (fun u -> Array.iter (fun pl -> Player.apply_effects pl On_start) u.player_updates);
  push 0 None "start";
  List.iter
    (fun (m : Game_record.move) ->
      if Option.is_none (Updater.ended u) then (
        let pl = Updater.player u m.player in
        let cmd = command_of desc m in
        Game.step u (fun _ -> Player.submit pl cmd);
        ignore (drain u m.player);
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
    Game.play_game desc ~mana0:(Array.to_list p1.mana) ~mana1:(Array.to_list p2.mana) ~rng:(Dice.make seed)
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

let skip_ws s i =
  let n = String.length s in
  let rec go i = if i >= n then i else match s.[i] with ' ' | '\n' | '\r' | '\t' -> go (i + 1) | _ -> i in
  go i

let matching_brace s start =
  let n = String.length s in
  let rec go i depth in_str escape =
    if i >= n then failwith "Replay: unterminated object";
    let c = s.[i] in
    if in_str then
      if escape then go (i + 1) depth true false
      else if c = '\\' then go (i + 1) depth true true
      else if c = '"' then go (i + 1) depth false false
      else go (i + 1) depth true false
    else
      match c with
      | '"' -> go (i + 1) depth true false
      | '{' -> go (i + 1) (depth + 1) false false
      | '}' -> if depth = 1 then i + 1 else go (i + 1) (depth - 1) false false
      | _ -> go (i + 1) depth false false
  in
  go (start + 1) 1 false false

let matching_bracket s start =
  let n = String.length s in
  let rec go i depth in_str escape =
    if i >= n then failwith "Replay: unterminated array";
    let c = s.[i] in
    if in_str then
      if escape then go (i + 1) depth true false
      else if c = '\\' then go (i + 1) depth true true
      else if c = '"' then go (i + 1) depth false false
      else go (i + 1) depth true false
    else
      match c with
      | '"' -> go (i + 1) depth true false
      | '[' -> go (i + 1) (depth + 1) false false
      | ']' -> if depth = 1 then i + 1 else go (i + 1) (depth - 1) false false
      | '{' -> go (matching_brace s i) depth false false
      | _ -> go (i + 1) depth false false
  in
  go (start + 1) 1 false false

let find_key s key =
  let tok = "\"" ^ key ^ "\"" in
  let rec go from =
    match String.index_from_opt s from '"' with
    | None -> None
    | Some i ->
        if i + String.length tok <= String.length s && String.sub s i (String.length tok) = tok then Some i
        else go (i + 1)
  in
  go 0

let after_colon s key =
  match find_key s key with
  | None -> None
  | Some i ->
      let j = skip_ws s (i + String.length key + 2) in
      if j < String.length s && s.[j] = ':' then Some (skip_ws s (j + 1)) else None

let parse_string_at s i =
  let i = skip_ws s i in
  if i >= String.length s || s.[i] <> '"' then failwith "Replay: expected string";
  let b = Buffer.create 16 in
  let rec go i escape =
    if i >= String.length s then failwith "Replay: unterminated string";
    let c = s.[i] in
    if escape then (
      Buffer.add_char b c;
      go (i + 1) false)
    else if c = '\\' then go (i + 1) true
    else if c = '"' then (Buffer.contents b, i + 1)
    else (
      Buffer.add_char b c;
      go (i + 1) false)
  in
  go (i + 1) false

let parse_int_at s i =
  let i = skip_ws s i in
  let n = String.length s in
  let neg = i < n && s.[i] = '-' in
  let i0 = if neg then i + 1 else i in
  let rec go i = if i < n && s.[i] >= '0' && s.[i] <= '9' then go (i + 1) else i in
  let i1 = go i0 in
  if i1 = i0 then failwith "Replay: expected int";
  let v = int_of_string (String.sub s i0 (i1 - i0)) in
  ((if neg then -v else v), i1)

let field_string obj key =
  match after_colon obj key with None -> None | Some i -> Some (fst (parse_string_at obj i))

let field_int obj key =
  match after_colon obj key with
  | None -> None
  | Some i ->
      let i = skip_ws obj i in
      if i + 4 <= String.length obj && String.sub obj i 4 = "null" then None
      else Some (fst (parse_int_at obj i))

let parse_int_list s =
  let i = skip_ws s 0 in
  if i >= String.length s || s.[i] <> '[' then failwith "Replay: expected int list";
  let rec go i acc =
    let i = skip_ws s i in
    if i >= String.length s then failwith "Replay: unterminated list";
    match s.[i] with
    | ']' -> List.rev acc
    | ',' -> go (i + 1) acc
    | '-' | '0' .. '9' ->
        let v, j = parse_int_at s i in
        go j (v :: acc)
    | _ -> failwith "Replay: expected int in list"
  in
  go (i + 1) []

let parse_slots s =
  let i = skip_ws s 0 in
  if i >= String.length s || s.[i] <> '[' then failwith "Replay: expected slots";
  let rec go i acc =
    let i = skip_ws s i in
    if i >= String.length s then failwith "Replay: unterminated slots";
    match s.[i] with
    | ']' -> Array.of_list (List.rev acc)
    | ',' -> go (i + 1) acc
    | 'n' -> go (i + 4) (None :: acc)
    | '{' ->
        let j = matching_brace s i in
        let obj = String.sub s i (j - i) in
        let occ =
          {
            card_key = Option.value (field_string obj "card") ~default:"";
            life = Option.value (field_int obj "life") ~default:0;
            attack = Option.value (field_int obj "attack") ~default:0;
            status = Option.value (field_int obj "status") ~default:0;
          }
        in
        go j (Some occ :: acc)
    | _ -> failwith "Replay: expected slot"
  in
  go (i + 1) []

let parse_side obj =
  let mana =
    match after_colon obj "mana" with
    | None -> []
    | Some i ->
        let i = skip_ws obj i in
        let k = matching_bracket obj i in
        parse_int_list (String.sub obj i (k - i))
  in
  let growth =
    match after_colon obj "growth" with
    | None -> []
    | Some i ->
        let i = skip_ws obj i in
        let k = matching_bracket obj i in
        parse_int_list (String.sub obj i (k - i))
  in
  let slots =
    match after_colon obj "slots" with
    | None -> [||]
    | Some i ->
        let i = skip_ws obj i in
        let k = matching_bracket obj i in
        parse_slots (String.sub obj i (k - i))
  in
  { life = Option.value (field_int obj "life") ~default:0; mana; growth; slots }

let parse_sides s =
  let i = skip_ws s 0 in
  if i >= String.length s || s.[i] <> '[' then failwith "Replay: expected sides";
  let rec go i acc =
    let i = skip_ws s i in
    if i >= String.length s then failwith "Replay: unterminated sides";
    match s.[i] with
    | ']' -> Array.of_list (List.rev acc)
    | ',' -> go (i + 1) acc
    | '{' ->
        let j = matching_brace s i in
        go j (parse_side (String.sub s i (j - i)) :: acc)
    | _ -> failwith "Replay: expected side"
  in
  go (i + 1) []

let parse_snap obj =
  let sides =
    match after_colon obj "sides" with
    | None -> [||]
    | Some i ->
        let i = skip_ws obj i in
        let k = matching_bracket obj i in
        parse_sides (String.sub obj i (k - i))
  in
  {
    t = Option.value (field_int obj "t") ~default:0;
    player = field_int obj "player";
    after = Option.value (field_string obj "after") ~default:"";
    ended = field_int obj "ended";
    sides;
  }

let snaps_of_json text =
  let i = skip_ws text 0 in
  if i >= String.length text || text.[i] <> '[' then failwith "Replay: expected snapshot array";
  let rec go i acc =
    let i = skip_ws text i in
    if i >= String.length text then failwith "Replay: unterminated snapshots";
    match text.[i] with
    | ']' -> List.rev acc
    | ',' -> go (i + 1) acc
    | '{' ->
        let j = matching_brace text i in
        go j (parse_snap (String.sub text i (j - i)) :: acc)
    | _ -> failwith "Replay: expected snapshot"
  in
  go (i + 1) []

let occ_json = function
  | None -> "null"
  | Some o ->
      Printf.sprintf {|{"card":"%s","life":%d,"attack":%d,"status":%d}|} (Game_record.esc o.card_key) o.life
        o.attack o.status

let side_json (s : side) =
  let ints xs = "[" ^ String.concat "," (List.map string_of_int xs) ^ "]" in
  let slots = Array.to_list s.slots |> List.map occ_json |> String.concat "," in
  Printf.sprintf {|{"life":%d,"mana":%s,"growth":%s,"slots":[%s]}|} s.life (ints s.mana) (ints s.growth) slots

let snap_json s =
  let player = match s.player with None -> "" | Some p -> Printf.sprintf {|"player":%d,|} p in
  let ended = match s.ended with None -> "null" | Some p -> string_of_int p in
  let sides = Array.to_list s.sides |> List.map side_json |> String.concat "," in
  Printf.sprintf {|{%s"t":%d,"after":"%s","ended":%s,"sides":[%s]}|} player s.t (Game_record.esc s.after)
    ended sides

let snaps_to_json snaps = "[\n" ^ String.concat ",\n" (List.map snap_json snaps) ^ "\n]\n"

let occ_eq a b =
  match (a, b) with
  | None, None -> true
  | Some x, Some y -> x.card_key = y.card_key && x.life = y.life && x.attack = y.attack && x.status = y.status
  | _ -> false

let fmt_occ = function
  | None -> "empty"
  | Some o -> Printf.sprintf "%s life=%d attack=%d status=%d" o.card_key o.life o.attack o.status

let describe (a : snap) (b : snap) =
  let rec side i =
    if i >= Array.length a.sides || i >= Array.length b.sides then
      Printf.sprintf "side count %d vs %d" (Array.length a.sides) (Array.length b.sides)
    else
      let x = a.sides.(i) and y = b.sides.(i) in
      if x.life <> y.life then Printf.sprintf "p%d life %d vs %d" i x.life y.life
      else if x.mana <> y.mana then
        Printf.sprintf "p%d mana %s vs %s" i
          (String.concat "," (List.map string_of_int x.mana))
          (String.concat "," (List.map string_of_int y.mana))
      else if x.growth <> y.growth then
        Printf.sprintf "p%d growth %s vs %s" i
          (String.concat "," (List.map string_of_int x.growth))
          (String.concat "," (List.map string_of_int y.growth))
      else
        let rec slot n =
          if n >= Array.length x.slots || n >= Array.length y.slots then
            if Array.length x.slots <> Array.length y.slots then Printf.sprintf "p%d slot count" i
            else side (i + 1)
          else if not (occ_eq x.slots.(n) y.slots.(n)) then
            Printf.sprintf "p%d slot %d: ocaml %s / scala %s" i n (fmt_occ x.slots.(n)) (fmt_occ y.slots.(n))
          else slot (n + 1)
        in
        slot 0
  in
  if a.ended <> b.ended then
    Printf.sprintf "ended %s vs %s"
      (match a.ended with None -> "none" | Some p -> string_of_int p)
      (match b.ended with None -> "none" | Some p -> string_of_int p)
  else side 0

let side_eq (a : side) (b : side) =
  a.life = b.life && a.mana = b.mana && a.growth = b.growth
  && Array.length a.slots = Array.length b.slots
  && Array.for_all2 occ_eq a.slots b.slots

let snap_eq (a : snap) (b : snap) =
  a.t = b.t && a.ended = b.ended
  && Array.length a.sides = Array.length b.sides
  && Array.for_all2 side_eq a.sides b.sides

let diff ml sc =
  let a : snap array = Array.of_list ml in
  let b : snap array = Array.of_list sc in
  let n = min (Array.length a) (Array.length b) in
  let rec go i =
    if i = n then
      if Array.length a = Array.length b then None
      else
        Some
          {
            t = (if n = 0 then 0 else a.(n - 1).t);
            message = Printf.sprintf "snapshot count: ocaml %d scala %d" (Array.length a) (Array.length b);
          }
    else if not (snap_eq a.(i) b.(i)) then
      Some { t = a.(i).t; message = Printf.sprintf "after %s: %s" a.(i).after (describe a.(i) b.(i)) }
    else go (i + 1)
  in
  go 0
