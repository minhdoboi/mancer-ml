(* Deck and opening-mana builder.  Port of priv/sp/CardShuffle.scala.

   Scala posts this as a Choco model (IntVars + allDifferent + a handful of
   reified memberships).  The domains are tiny — four 1-based card indexes per
   house — so the same constraints are solved here by enumerating legal
   per-house 4-tuples and searching across houses, first fit over shuffled
   candidates. *)

open Sp

type deal = { desc : player_desc; mana : int array }

let mana_gens = [ (0, 3); (1, 5); (3, 5) ]
let card_desc (c : card) = { cd_card = c; cd_cost = c.cost; cd_enabled = true; cd_extra_mana = 0 }
let indexes1 (h : house) = List.map card_index1 h.h_cards
let sum = List.fold_left ( + ) 0
let has xs n = List.mem n xs
let count xs = List.fold_left (fun n b -> if b then n + 1 else n) 0 xs
let maximum = function [] -> 0 | x :: xs -> List.fold_left max x xs

let shuffle_list rng xs =
  let a = Array.of_list xs in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done;
  Array.to_list a

(* All-different tuples with one value from each domain.  [ok k prev cur]
   constrains the k-th consecutive pair, like Scala's `cards.sliding(2)`. *)
let tuples ?(ok = fun _ _ _ -> true) domains =
  let rec go k prev acc = function
    | [] -> [ List.rev acc ]
    | d :: ds ->
        List.concat_map
          (fun x ->
            match prev with
            | _ when List.mem x acc -> []
            | Some p when not (ok (k - 1) p x) -> []
            | _ -> go (k + 1) (Some x) (x :: acc) ds)
          d
  in
  go 0 None [] domains

let split_at n xs = (List.filteri (fun i _ -> i < n) xs, List.filteri (fun i _ -> i >= n) xs)

(* HModel for a special house: the (possibly excluded) range in four
   consecutive chunks of size/4, the last one taking the remainder *)
let quarters xs =
  let n = List.length xs / 4 in
  let c0, r0 = split_at n xs in
  let c1, r1 = split_at n r0 in
  let c2, c3 = split_at n r1 in
  [ c0; c1; c2; c3 ]

(* HModel cardRanges: houseIndex 0 = fire, 3 = earth, else water/air *)
let basic_ranges idx =
  [
    (1, if idx = 0 || idx = 3 then 4 else 3);
    (2, if idx = 0 then 9 else 8);
    ((if idx = 0 then 5 else if idx = 3 then 6 else 4), 11);
    ((if idx = 0 || idx = 3 then 10 else 9), 12);
  ]

(* Legal 4-card picks for one basic house: sum in (20, 30), and outside fire
   each consecutive pair satisfies y - x > gap, with gap 1 (or 0 with 5%
   chance) drawn per pair.  Fire has no ordering rule, so its tuples are
   deduplicated by card set: otherwise a set with several valid orderings
   would be drawn more often. *)
let basic_candidates rng idx allowed =
  let domains =
    List.map (fun (lo, hi) -> List.filter (fun i -> i >= lo && i <= hi) allowed) (basic_ranges idx)
  in
  let in_sum t = sum t > 20 && sum t < 30 in
  if idx = 0 then
    tuples domains |> List.filter in_sum |> List.map (List.sort compare) |> List.sort_uniq compare
  else
    let gaps = Array.init 3 (fun _ -> if Random.State.float rng 1. > 0.05 then 1 else 0) in
    tuples ~ok:(fun k prev cur -> cur - prev > gaps.(k)) domains |> List.filter in_sum

(* CardShuffler.postConstraints, evaluated on the four basic 4-tuples *)
let basics_ok fire water air earth =
  let mana_gen = count [ has fire 3; has water 5; has earth 5 ] in
  let wipe = count [ has fire 6; has fire 9; has air 8 ] in
  let finish = count [ has fire 11; has air 6; has air 8; has earth 6 ] in
  let defense =
    count [ has water 6; has water 10; has air 4; has earth 1; has earth 2; has earth 4; has earth 11 ]
  in
  let mass = count [ has fire 11; has earth 9; has water 8 ] in
  let air7 = has air 7 in
  mana_gen = 1 && wipe = 1 && finish > 0 && finish < 3 && defense > 1 && defense < 4
  && maximum fire > 9
  && maximum earth > 9
  && maximum water > 8
  && maximum air > 8
  && (not (has fire 9 && has fire 11))
  && (not (has fire 5 && has earth 3))
  && (not (has water 1 && has earth 9))
  && (not (has earth 5 && has earth 6))
  && ((not air7) || mass = 1)
  && ((not air7) || has water 5 || has water 3 || has fire 11)

(* Prunes before the earth loop: only rules earth cannot rescue.  Must never
   reject a combination basics_ok would accept. *)
let fire_water_air_ok fire water air =
  let air7 = has air 7 in
  count [ has fire 6; has fire 9; has air 8 ] = 1
  && maximum fire > 9
  && maximum water > 8
  && maximum air > 8
  && (not (has fire 9 && has fire 11))
  && count [ has fire 3; has water 5 ] <= 1
  && count [ has fire 11; has air 6; has air 8 ] <= 2
  && count [ has water 6; has water 10; has air 4 ] <= 3
  && ((not air7) || count [ has fire 11; has water 8 ] <= 1)
  && ((not air7) || has water 5 || has water 3 || has fire 11)

let cards_of (h : house) indexes =
  h.h_cards |> List.filter (fun c -> List.mem (card_index1 c) indexes) |> List.map card_desc |> Array.of_list

let to_player_desc houses picks =
  {
    pd_houses = Array.init 5 (fun i -> { phd_house = houses.(i); phd_cards = cards_of houses.(i) picks.(i) });
  }

(* CardModel.ExcludePlayerCards: a house the opponent also plays may not reuse
   their card indexes *)
let allowed_indexes (exclude : player_desc option) (h : house) =
  let taken =
    match
      Option.bind exclude (fun p -> Array.find_opt (fun ph -> ph.phd_house.h_name = h.h_name) p.pd_houses)
    with
    | None -> []
    | Some ph -> Array.to_list ph.phd_cards |> List.map (fun cd -> card_index1 cd.cd_card)
  in
  List.filter (fun i -> not (List.mem i taken)) (indexes1 h)

(* Exhaustive first-fit search, so None means no deck exists for these gaps *)
let pick_basics rng ~require allowed =
  let cands idx =
    basic_candidates rng idx allowed.(idx)
    |> List.filter (fun t -> List.for_all (has t) require.(idx))
    |> shuffle_list rng
  in
  let fire = cands 0 and water = cands 1 and air = cands 2 and earth = cands 3 in
  List.find_map
    (fun f ->
      List.find_map
        (fun w ->
          List.find_map
            (fun a ->
              if not (fire_water_air_ok f w a) then None
              else List.find_map (fun e -> if basics_ok f w a e then Some [| f; w; a; e |] else None) earth)
            air)
        water)
    fire

let pick_special rng ~require allowed =
  let ok t = List.for_all (has t) require in
  match shuffle_list rng (List.filter ok (tuples (quarters allowed))) with
  | [] -> failwith "Shuffle: special house has no legal pick"
  | s :: _ -> s

(* ManaShuffler: the mana gen whose card this deck holds, as (house, cost) *)
let find_mana_gen (desc : player_desc) =
  List.find_map
    (fun (idx, cost) ->
      if Array.exists (fun cd -> cd.cd_cost = cost) desc.pd_houses.(idx).phd_cards then Some (idx, cost)
      else None)
    mana_gens

let pick_mana rng ~first (desc : player_desc) =
  let total = if first then 19 else 18 in
  let gen = find_mana_gen desc in
  let has_cost i c = Array.exists (fun cd -> cd.cd_cost = c) desc.pd_houses.(i).phd_cards in
  let fire10_water3 = has_cost 0 10 && has_cost 1 3 in
  let valid a =
    sum a = total
    && (match gen with
      | None -> true
      | Some (idx, cost) ->
          let m = List.nth a idx in
          m >= if first then cost else cost - 1)
    && ((not fire10_water3) || List.hd a < 6)
  in
  (* each pool in 2..6, values tried in random order per house *)
  let rec vals acc =
    if List.length acc = 4 then
      let a = List.rev acc in
      if valid a then Some a else None
    else List.find_map (fun m -> vals (m :: acc)) (shuffle_list rng [ 2; 3; 4; 5; 6 ])
  in
  match vals [] with
  | Some a -> Array.of_list (a @ [ 2 ])
  | None -> failwith "Shuffle.pick_mana: no opening mana fits this deck"

let check_basics basics =
  let houses = Array.of_list basics in
  if Array.length houses <> 4 then invalid_arg "Shuffle: expected [fire; water; air; earth]";
  Array.iteri
    (fun i (h : house) ->
      if h.h_index <> i then
        invalid_arg
          (Printf.sprintf "Shuffle: %s has house index %d but is in position %d" h.h_name h.h_index i))
    houses

let deal_deck rng ?exclude ?(require = Array.make 5 []) ~special basics =
  check_basics basics;
  let houses = Array.of_list (basics @ [ special ]) in
  let allowed = Array.map (allowed_indexes exclude) houses in
  (* pick_basics is exhaustive, so a retry can only help because the per-pair
     gaps are re-drawn *)
  let rec basics_attempt n =
    match pick_basics rng ~require allowed with
    | Some four -> four
    | None when n > 0 -> basics_attempt (n - 1)
    | None -> failwith "Shuffle.create_player: no legal deck"
  in
  let four = basics_attempt 8 in
  to_player_desc houses (Array.append four [| pick_special rng ~require:require.(4) allowed.(4) |])

let create_player ?(rng = Random.State.make_self_init ()) ?exclude ?require ~first ~special basics =
  let desc = deal_deck rng ?exclude ?require ~special basics in
  { desc; mana = pick_mana rng ~first desc }

(* CardShuffle.get: owner first, then the opponent excluding the owner's
   cards.  ManaShuffler gives the starting player the 19-mana opening.  Both
   decks are dealt before any mana so that, for a given seed, who starts only
   changes the opening mana, never the decks. *)
let two_players ?(rng = Random.State.make_self_init ()) ?(starting = owner) ~specials:(s0, s1) basics =
  let d1 = deal_deck rng ~special:s0 basics in
  let d2 = deal_deck rng ~exclude:d1 ~special:s1 basics in
  let deal desc first = { desc; mana = pick_mana rng ~first desc } in
  (deal d1 (starting = owner), deal d2 (starting = opponent))
