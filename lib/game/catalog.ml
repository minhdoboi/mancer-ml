(* House and card lookup for recorded games. Accepts a key (`anger.werewolf`), a
   house key (`anger` / `zen`), or the Scala I18n name (`Anger` / `Werewolf`). *)

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

let standard_basics = [ Anger.house; Sadness.house; Fear.house; Joy.house ]

let basics = standard_basics

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
    Technomage.house;
  ]

let all = standard_basics @ specials

let init () =
  List.iter Game.init_cards standard_basics;
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
  Game.init_cards ~cost_fn:Barbarians.cost_fn Barbarians.house;
  Game.init_cards ~cost_fn:Technomage.cost_fn Technomage.house

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
