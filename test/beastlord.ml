(* Mechanics for the Beastlords: the reveal counter and the pack. The dealt
   special deck holds one card of every quarter: Blood Hound, Wyvern,
   Poison Dart, Minotaur. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let on_end u pid = step u (fun () -> Player.apply_effects (player u pid) On_end_turn)
let on_turn u pid = step u (fun () -> Player.apply_effects (player u pid) On_turn)
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let data u pid = match (player u pid).pstate.ps_data with Beast_data d -> d | _ -> assert false
let hurt u pid n =
  step u (fun () -> Player.inflict (player u pid) (mk_damage n { ctx_player = 1 - pid; ctx_card = None; ctx_selected = -1 }))

(* Is the card's desc entry gated off by the player's desc mods? A fresh
   probe is folded through the mods, so spent cards don't matter — and the
   house is found in the full card list, so undealt cards work too. *)
let banned u pid key =
  let pd = (player u pid).pstate.ps_desc in
  let h =
    Option.get
      (Array.find_opt
         (fun h -> List.exists (fun c -> c.Sp.key = key) h.phd_house.Sp.h_cards)
         pd.init_desc.pd_houses)
  in
  let probe = [| { cd_card = card_by_key key; cd_cost = 0; cd_enabled = true; cd_extra_mana = 0 } |] in
  let out = List.fold_left (fun cs m -> m h cs) probe pd.desc_mods in
  not out.(0).cd_enabled

(* ------------------------------------------------------------- the counter *)

let playing_counts_once () =
  let u = new_game ~special:Beastlord.house () in
  check_int "nothing revealed" 0 (List.length (data u 0).bd_revealed);
  place u 0 "fire.goblin" 3;
  play u 0 "fire.goblin" ~input:(Some 3) ~cost:0;
  check_int "one revealed" 1 (List.length (data u 0).bd_revealed);
  place u 0 "fire.monk" 4;
  play u 0 "fire.monk" ~input:(Some 4) ~cost:0;
  check_int "two revealed" 2 (List.length (data u 0).bd_revealed);
  place u 0 "fire.goblin" 5;
  play u 0 "fire.goblin" ~input:(Some 5) ~cost:0;
  check_int "a repeat is not a new reveal" 2 (List.length (data u 0).bd_revealed);
  play u 0 "beastlord.wolverine" ~input:(Some 2) ~cost:0;
  check_int "three revealed" 3 (List.length (data u 0).bd_revealed)

let opponent_cards_are_their_own () =
  let u = new_game ~special:Beastlord.house () in
  place u 1 "fire.goblin" 2;
  play u 1 "fire.goblin" ~input:(Some 2) ~cost:0;
  check_int "player 0 saw nothing" 0 (List.length (data u 0).bd_revealed);
  check_int "player 1 saw one" 1 (List.length (data u 1).bd_revealed)

(* ------------------------------------------------------------- wolverine *)

let wolverine_bans_itself_this_turn () =
  let u = new_game ~special:Beastlord.house () in
  play u 0 "beastlord.wolverine" ~input:(Some 2) ~cost:0;
  check_bool "wolverine bans itself" true (banned u 0 "beastlord.wolverine");
  check_bool "another card is fine" false (banned u 0 "beastlord.minotaur")

let wolverine_ban_lifts_next_turn () =
  let u = new_game ~special:Beastlord.house () in
  play u 0 "beastlord.wolverine" ~input:(Some 2) ~cost:0;
  on_end u 0;
  on_turn u 1;
  on_end u 1;
  on_turn u 0;
  check_bool "ban lifted" false (banned u 0 "beastlord.wolverine")

(* ---------------------------------------------------------------- minotaur *)

let minotaur_grows_on_reveal () =
  let u = new_game ~special:Beastlord.house () in
  play u 0 "beastlord.minotaur" ~input:(Some 2) ~cost:0;
  check_int "minotaur starts at 40" 40 (life_at u 0 2);
  place u 0 "fire.goblin" 3;
  play u 0 "fire.goblin" ~input:(Some 3) ~cost:0;
  check_int "minotaur grew by 5" 45 (life_at u 0 2);
  place u 0 "fire.goblin" 4;
  play u 0 "fire.goblin" ~input:(Some 4) ~cost:0;
  check_int "a repeat is not a new reveal" 45 (life_at u 0 2)

let minotaur_hears_the_opponent () =
  let u = new_game ~special:Beastlord.house () in
  play u 1 "beastlord.minotaur" ~input:(Some 2) ~cost:0;
  play u 0 "beastlord.minotaur" ~input:(Some 2) ~cost:0;
  check_int "the other side's reveal is not mine" 40 (life_at u 1 2);
  place u 1 "fire.goblin" 3;
  play u 1 "fire.goblin" ~input:(Some 3) ~cost:0;
  check_int "my own next reveal grows the beast" 45 (life_at u 1 2)

(* ---------------------------------------------------------------- elephant *)

let elephant_pays_the_revealed_house () =
  let u = new_game ~special:Beastlord.house () in
  play u 0 "beastlord.elephant" ~input:(Some 2) ~cost:0;
  place u 0 "fire.goblin" 3;
  play u 0 "fire.goblin" ~input:(Some 3) ~cost:0;
  check_int "elephant paid fire" 8 (mana u 0 0);
  place u 0 "water.sprite" 4;
  play u 0 "water.sprite" ~input:(Some 4) ~cost:0;
  check_int "elephant paid water" 6 (mana u 0 1)

(* -------------------------------------------------------------- sabretooth *)

let sabretooth_hits_on_reveal () =
  let u = new_game ~special:Beastlord.house () in
  play u 0 "beastlord.sabretooth" ~input:(Some 2) ~cost:0;
  place u 1 "fire.dragon" 3;
  place u 1 "water.golem" 4;
  place u 0 "fire.goblin" 3;
  play u 0 "fire.goblin" ~input:(Some 3) ~cost:0;
  check_int "dragon took 5" (41 - 5) (life_at u 1 3);
  check_int "turtle took 5" (17 - 5) (life_at u 1 4);
  check_int "my own creature is spared" 16 (life_at u 0 3)

let sabretooth_spares_its_owner () =
  let u = new_game ~special:Beastlord.house () in
  play u 0 "beastlord.sabretooth" ~input:(Some 2) ~cost:0;
  play u 0 "beastlord.minotaur" ~input:(Some 3) ~cost:0;
  check_int "own minotaur unharmed" 40 (life_at u 0 3)

let () =
  Alcotest.run "beastlord"
    [
      ( "beastlord",
        [
          Alcotest.test_case "counter" `Quick playing_counts_once;
          Alcotest.test_case "opponent counter" `Quick opponent_cards_are_their_own;
          Alcotest.test_case "wolverine" `Quick wolverine_bans_itself_this_turn;
          Alcotest.test_case "wolverine ban lifts" `Quick wolverine_ban_lifts_next_turn;
          Alcotest.test_case "minotaur" `Quick minotaur_grows_on_reveal;
          Alcotest.test_case "minotaur hears all" `Quick minotaur_hears_the_opponent;
          Alcotest.test_case "elephant" `Quick elephant_pays_the_revealed_house;
          Alcotest.test_case "sabretooth" `Quick sabretooth_hits_on_reveal;
          Alcotest.test_case "sabretooth spares owner" `Quick sabretooth_spares_its_owner;
        ] );
    ]
