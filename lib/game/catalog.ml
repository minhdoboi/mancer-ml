(* House and card lookup for recorded games. Accepts a key (`fire.goblin`), a
   house key (`fire` / `zen`), or the Scala I18n name (`Fire` / `Goblin`). *)

open Sp

type t = { houses : house list }

let token s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' -> Buffer.add_char b (Char.lowercase_ascii c)
      | 'a' .. 'z' | '0' .. '9' -> Buffer.add_char b c
      | _ -> ())
    s;
  Buffer.contents b

let basics = [ Fire.house; Water.house; Air.house; Earth.house ]
let specials = [ Troopers.house; Zen.house; Wind.house; Chronomaster.house; Disciples.house ]
let all = basics @ specials

let init () =
  List.iter Game.init_cards basics;
  Game.init_cards ~cost_fn:Troopers.conscription_cost Troopers.house;
  List.iter Game.init_cards [ Zen.house; Wind.house; Chronomaster.house; Disciples.house ]

let create houses = { houses }
let default () = create all

let same_house (h : house) name =
  let n = token name in
  token h.h_key = n || token h.h_name = n

let same_card (c : card) name =
  let n = token name in
  let n_raw = String.lowercase_ascii name in
  String.lowercase_ascii c.key = n_raw
  || String.lowercase_ascii c.name = n_raw
  || token c.key = n
  || token c.name = n

let find_house cat name =
  match List.find_opt (fun h -> same_house h name) cat.houses with
  | Some h -> h
  | None -> failwith ("unknown house " ^ name)

let cards_of (h : house) = h.h_cards @ h.h_additional_cards

let find_card_in_house h name =
  match List.find_opt (fun c -> same_card c name) (cards_of h) with
  | Some c -> c
  | None -> failwith (Printf.sprintf "unknown card %s in house %s" name h.h_key)

let find_card cat name =
  match List.find_map (fun h -> List.find_opt (fun c -> same_card c name) (cards_of h)) cat.houses with
  | Some c -> c
  | None -> failwith ("unknown card " ^ name)

let find_special name =
  let n = token name in
  match List.find_opt (fun (h : house) -> token h.h_name = n || token h.h_key = n) specials with
  | Some h -> h
  | None ->
      failwith
        (Printf.sprintf "unknown special house %s (known: %s)" name
           (String.concat ", " (List.map (fun (h : house) -> h.h_key) specials)))
