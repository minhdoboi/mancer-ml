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

(* Classic keeps the original four basic houses; standard replaces fire with
   its Anger variant (same basic slot, index 0). *)
type mode = Standard | Classic

let classic_basics = [ Fire.house; Water.house; Air.house; Earth.house ]
let standard_basics = [ Anger.house; Sadness.house; Fear.house; Joy.house ]

(* [basics] stays the classic set: recorded games and replays were dealt with
   it, and {!Replay} still reads them by card key. *)
let basics = classic_basics
let basics_of = function Standard -> standard_basics | Classic -> classic_basics

let string_of_mode = function Standard -> "standard" | Classic -> "classic"

let mode_of_string s =
  match String.lowercase_ascii (String.trim s) with "classic" -> Classic | _ -> Standard

let specials =
  [
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
    Barbarians.house;
    Chaospriest.house;
  ]

let all = standard_basics @ [ Fire.house; Water.house; Air.house; Earth.house ] @ specials

let init () =
  List.iter Game.init_cards classic_basics;
  Game.init_cards Anger.house;
  Game.init_cards Sadness.house;
  Game.init_cards Fear.house;
  Game.init_cards Joy.house;
  Game.init_cards ~cost_fn:Troopers.conscription_cost Troopers.house;
  List.iter Game.init_cards
    [
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
      Powermancer.house;
      Shadowmage.house;
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
      Chaospriest.house;
    ];
  Game.init_cards ~cost_fn:Antimancer.cost_fn Antimancer.house;
  Game.init_cards ~cost_fn:Colors.cost_fn Colors.house;
  Game.init_cards ~cost_fn:Trappers.cost_fn Trappers.house;
  Game.init_cards ~cost_fn:Beastlord.cost_fn Beastlord.house;
  Game.init_cards Lightstave.house;
  Game.init_cards ~cost_fn:Barbarians.cost_fn Barbarians.house

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

(* A house whose key differs from the name the game shows: Soul Reaper's key
   is "reaper" (its cards are "reaper.*"), so accept the module name too. *)
let special_aliases = [ ("soulreaper", Soulreaper.house) ]

let find_special name =
  let n = token name in
  match List.find_opt (fun (h : house) -> token h.h_name = n || token h.h_key = n) specials with
  | Some h -> h
  | None -> (
      match List.find_opt (fun (alias, (h : house)) -> token alias = n) special_aliases with
      | Some (_, h) -> h
      | None ->
          failwith
            (Printf.sprintf "unknown special house %s (known: %s)" name
               (String.concat ", " (List.map (fun (h : house) -> h.h_key) specials))))
