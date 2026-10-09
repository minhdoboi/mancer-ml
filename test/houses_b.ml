(* Mechanics for Antimancer, Junk, Battlemage and Powermancer, same style as test/more_houses.ml. *)

open Mancer
open Sp
open Engine
open Support

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let ctx pid = { ctx_player = pid; ctx_card = None; ctx_selected = 0 }

let hit ?(is_ability = false) ?(is_spell = false) u pid num amount =
  step u (fun () -> Slot.inflict (slot u pid num) (mk_damage ~is_ability ~is_spell amount (ctx (1 - pid))))

let kill u pid num = hit u pid num 1000
let mana u pid i = (player u pid).houses_update.hu_value.(i).mana
let settle u = step u (fun () -> ())
let all_mana = [ 6; 4; 4; 4; 10 ]
let set_life u pid num life = step u (fun () -> Slot.update (slot u pid num) (fun st -> { st with life }))
let flagged u pid num flag = has_flag (Option.get (slot u pid num).value) flag

(* -------------------------------------------------------------- antimancer *)

let antimancer_costs () =
  init_all_houses ();
  check_int "first" 2 (card_by_key "antimancer.2.name").cost;
  check_int "last" 9 (card_by_key "antimancer.9.name").cost;
  check_int "mob is a token" 0 (card_by_key "antimancer.0.name").cost

let resist_fills_quiet_slots () =
  let u = new_game ~mana:all_mana ~special:Antimancer.house () in
  place u 1 "fire.goblin" 0;
  play u 0 "antimancer.3.name" ~input:None ~cost:3;
  check_bool "faces a creature" true (is_empty u 0 0);
  List.iter (fun n -> check_string "mob" "antimancer.0.name" (Option.get (occupant u 0 n))) [ 1; 2; 3; 4; 5 ]

let retaliator_hits_back () =
  let u = new_game ~special:Antimancer.house () in
  place u 0 "antimancer.4.name" 0;
  place u 1 "fire.dragon" 2;
  let a = life_at u 1 2 in
  hit u 0 0 6;
  check_int "6 back to every enemy" (a - 6) (life_at u 1 2)

let martyr_returns_after_spell () =
  let u = new_game ~special:Antimancer.house () in
  place u 0 "antimancer.5.name" 0;
  place u 0 "fire.goblin" 1;
  hit ~is_spell:true u 0 1 100;
  check_string "back" "fire.goblin" (Option.get (occupant u 0 1));
  hit u 0 1 100;
  check_bool "not after an attack" true (is_empty u 0 1)

let harvester_gives_mana () =
  let u = new_game ~special:Antimancer.house () in
  place u 0 "antimancer.6.name" 0;
  place u 0 "fire.goblin" 1;
  let total () = List.fold_left (fun n i -> n + mana u 0 i) 0 [ 0; 1; 2; 3 ] in
  let before = total () in
  kill u 0 1;
  check_int "one mana somewhere" (before + 1) (total ())

let voodoo_drains_and_heals () =
  let u = new_game ~special:Antimancer.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "antimancer.7.name" 0;
  place u 1 "fire.dragon" 1;
  place u 1 "fire.dragon" 4;
  let a = life_at u 1 1 in
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "1 each" (a - 1) (life_at u 1 1);
  check_int "and the other" (a - 1) (life_at u 1 4);
  check_int "2 life" 42 (player_life u 0)

let bombardier_blast () =
  let u = new_game ~special:Antimancer.house () in
  place u 0 "antimancer.8.name" 0;
  place u 1 "fire.dragon" 0;
  let a = life_at u 1 0 in
  kill u 0 0;
  check_int "28 to the opposite" (a - 28) (life_at u 1 0)

let bombardier_splash () =
  let u = new_game ~special:Antimancer.house () in
  place u 0 "antimancer.8.name" 0;
  place u 1 "fire.dragon" 3;
  let a = life_at u 1 3 in
  step u (fun () -> Player.run_slots (player u 0));
  check_int "player" 55 (player_life u 1);
  check_int "and every creature" (a - 5) (life_at u 1 3)

let mirror_priest_copies_mana () =
  let u = new_game ~special:Antimancer.house () in
  place u 0 "antimancer.2.name" 0;
  play u 1 "fire.goblin" ~input:(Some 2) ~cost:1;
  check_int "fire +1" 7 (mana u 0 0)

let bribe_steals () =
  let u = new_game ~mana:all_mana ~special:Antimancer.house () in
  place u 1 "fire.goblin" 2;
  play u 0 "antimancer.9.name" ~input:(Some 2) ~cost:9;
  check_bool "gone from them" true (is_empty u 1 2);
  let mine = List.filter (fun n -> occupant u 0 n = Some "fire.goblin") base_slot_list in
  check_int "one of mine now" 1 (List.length mine)

(* -------------------------------------------------------------------- junk *)

let screamers_scream () =
  let u = new_game ~special:Junk.house () in
  place u 0 "junk.screamer" 0;
  place u 0 "junk.screamer" 1;
  settle u;
  check_int "2 + 2 screamers" 4 (attack_at u 0 0)

let poison_flower_spreads () =
  let u = new_game ~mana:all_mana ~special:Junk.house () in
  place u 0 "earth.hydra" 1;
  place u 1 "earth.hydra" 0;
  place u 1 "earth.hydra" 1;
  place u 1 "earth.hydra" 2;
  place u 1 "earth.hydra" 3;
  let a = life_at u 1 0 in
  let mine = life_at u 0 1 in
  play u 0 "junk.poison" ~input:(Some 1) ~cost:2;
  check_int "own creature" (mine - 5) (life_at u 0 1);
  List.iter (fun n -> check_int "enemy in range" (a - 5) (life_at u 1 n)) [ 0; 1; 2 ];
  check_int "out of range" a (life_at u 1 3);
  check_int "own creature's pool -1" 3 (mana u 0 3);
  check_int "their opposite creature's pool -1" 3 (mana u 1 3)

let fortune_absorbs_once () =
  let u = new_game ~special:Junk.house () in
  place u 0 "junk.fortune" 0;
  step u (fun () -> Player.inflict (player u 0) (mk_damage 10 (ctx 1)));
  check_int "6 of 10" (60 - 6) (player_life u 0);
  step u (fun () -> Player.inflict (player u 0) (mk_damage 10 (ctx 1)));
  check_int "then nothing" (60 - 16) (player_life u 0);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  step u (fun () -> Player.inflict (player u 0) (mk_damage 10 (ctx 1)));
  check_int "again next turn" (60 - 22) (player_life u 0)

let assassin_roams () =
  let u = new_game ~special:Junk.house () in
  place u 0 "junk.assassin" 0;
  place u 1 "fire.dragon" 3;
  let a = life_at u 1 3 in
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_string "moved" "junk.assassin" (Option.get (occupant u 0 3));
  check_int "5 damage" (a - 5) (life_at u 1 3)

let assassin_stays_when_opposed () =
  let u = new_game ~special:Junk.house () in
  place u 0 "junk.assassin" 0;
  place u 1 "fire.dragon" 0;
  place u 1 "fire.dragon" 3;
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_string "stays" "junk.assassin" (Option.get (occupant u 0 0))

let cyborg_fills_and_eats () =
  let u = new_game ~mana:all_mana ~special:Junk.house () in
  play u 0 "junk.cyborg" ~input:(Some 0) ~cost:8;
  check_int "trash everywhere" 5
    (List.length (List.filter (fun n -> occupant u 0 n = Some "junk.trash") base_slot_list));
  let life = life_at u 0 0 and a = attack_at u 0 0 in
  step u (fun () -> Player.apply_effects (player u 0) On_turn);
  check_int "two trash eaten" 3
    (List.length (List.filter (fun n -> occupant u 0 n = Some "junk.trash") base_slot_list));
  check_int "life" (life + 22) (life_at u 0 0);
  check_int "attack" (a + 4) (attack_at u 0 0)

let factory_copies_cheap_neighbour () =
  let u = new_game ~mana:all_mana ~special:Junk.house () in
  place u 0 "fire.goblin" 1;
  play u 0 "junk.factory" ~input:(Some 2) ~cost:6;
  check_string "copied across" "fire.goblin" (Option.get (occupant u 0 3))

let chain_controller_mirrors () =
  let u = new_game ~special:Junk.house () in
  place u 0 "junk.chain" 2;
  play u 0 "fire.goblin" ~input:(Some 1) ~cost:1;
  check_string "mirrored" "fire.goblin" (Option.get (occupant u 0 3))

let chain_controller_closes_gap () =
  let u = new_game ~special:Junk.house () in
  place u 0 "fire.goblin" 3;
  place u 0 "fire.goblin" 1;
  place u 0 "junk.chain" 2;
  kill u 0 3;
  check_string "the other neighbour steps in" "fire.goblin" (Option.get (occupant u 0 3));
  check_bool "left empty" true (is_empty u 0 1)

let recycling_bot_heals () =
  let u = new_game ~special:Junk.house () in
  (player u 0).pstate.ps_life <- 40;
  place u 0 "junk.bot" 0;
  place u 0 "fire.goblin" 1;
  set_life u 0 0 10;
  kill u 0 1;
  check_int "heals itself 6" 16 (life_at u 0 0);
  check_int "heals owner 2" 42 (player_life u 0)

(* -------------------------------------------------------------- battlemage *)

let rune_warrior_blunts_spells () =
  let u = new_game ~special:Battlemage.house () in
  place u 0 "battlemage.rune" 0;
  place u 0 "fire.dragon" 1;
  let a = life_at u 0 1 in
  hit ~is_spell:true u 0 1 10;
  check_int "2 less" (a - 8) (life_at u 0 1);
  hit u 0 1 10;
  check_int "not against attacks" (a - 18) (life_at u 0 1)

let field_mage_takes_spells () =
  let u = new_game ~special:Battlemage.house () in
  place u 0 "battlemage.fieldmage" 0;
  place u 0 "fire.dragon" 1;
  let mage = life_at u 0 0 and a = life_at u 0 1 in
  hit ~is_spell:true u 0 1 10;
  check_int "target untouched" a (life_at u 0 1);
  check_int "mage hurt instead" (mage - 10) (life_at u 0 0)

let field_mage_not_twice () =
  let u = new_game ~special:Battlemage.house () in
  place u 0 "battlemage.fieldmage" 0;
  place u 0 "battlemage.rune" 1;
  let mage = life_at u 0 0 and rune = life_at u 0 1 in
  hit ~is_spell:true u 0 1 10;
  check_int "rune untouched" rune (life_at u 0 1);
  check_int "the redirected hit is blunted by the rune" (mage - 8) (life_at u 0 0)

let sergeant_charges_neighbours () =
  let u = new_game ~special:Battlemage.house () in
  place u 0 "battlemage.sergeant" 1;
  place u 0 "fire.goblin" 0;
  place u 0 "fire.goblin" 3;
  settle u;
  check_int "neighbour unopposed +3" (4 + 3) (attack_at u 0 0);
  check_int "far one untouched" 4 (attack_at u 0 3);
  place u 1 "fire.dragon" 0;
  settle u;
  check_int "opposed: nothing" 4 (attack_at u 0 0)

let knight_charges_twice () =
  let u = new_game ~special:Battlemage.house () in
  place u 0 "battlemage.knight" 0;
  place u 1 "fire.dragon" 0;
  let a = life_at u 1 0 in
  step u (fun () -> Player.run_slots (player u 0));
  check_int "two hits" (a - 8) (life_at u 1 0)

let knight_one_hit_on_empty () =
  let u = new_game ~special:Battlemage.house () in
  place u 0 "battlemage.knight" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "one hit" (60 - 4) (player_life u 1)

let cavalier_counts_gaps () =
  let u = new_game ~special:Battlemage.house () in
  place u 0 "battlemage.cavalier" 0;
  settle u;
  check_int "6 + 5 open" 11 (attack_at u 0 0);
  place u 0 "fire.goblin" 1;
  settle u;
  check_int "6 + 4 open" 10 (attack_at u 0 0)

let cavalier_bans_apocalypse () =
  let u = new_game ~special:Battlemage.house () in
  let mods () = List.length (player u 0).pstate.ps_desc.desc_mods in
  place u 0 "battlemage.cavalier" 0;
  place u 0 "battlemage.cavalier" 1;
  check_int "one ban, however many cavaliers" 1 (mods ());
  kill u 0 0;
  check_int "still banned" 1 (mods ());
  kill u 0 1;
  check_int "lifted" 0 (mods ())

let taunt_own_creature () =
  let u = new_game ~special:Battlemage.house () in
  place u 0 "fire.dragon" 1;
  play u 0 "battlemage.taunt" ~input:(Some 1) ~cost:1;
  check_int "its attack to the opponent" (60 - 9) (player_life u 1)

let warcry_silences () =
  let u = new_game ~mana:all_mana ~special:Battlemage.house () in
  place u 1 "fire.dragon" 1;
  let a = life_at u 1 1 in
  play u 0 "battlemage.warcry" ~input:None ~cost:7;
  check_int "player" (60 - 9) (player_life u 1);
  check_int "creature" (a - 9) (life_at u 1 1);
  check_bool "silenced" true
    (match (slot u 1 1).value with
    | Some { card = { c_body = { b_data = Mortal_of _; _ }; _ }; _ } -> true
    | _ -> false)

let retinue_discounts_next () =
  let u = new_game ~mana:all_mana ~special:Battlemage.house () in
  play u 0 "battlemage.retinue" ~input:(Some 0) ~cost:8;
  let special = (player u 0).pstate.ps_desc.init_desc.pd_houses.(1) in
  let costs () =
    let pd = (player u 0).pstate.ps_desc in
    List.fold_left (fun cs m -> m special cs) special.phd_cards pd.desc_mods
    |> Array.to_list
    |> List.map (fun cd -> (cd.cd_card.key, cd.cd_cost, cd.cd_enabled))
  in
  let _, cost, _ = List.find (fun (k, _, _) -> k = "battlemage.sergeant") (costs ()) in
  check_int "sergeant costs 1 less" 2 cost;
  check_bool "spells are hidden for the extra play" true
    (List.exists (fun (k, _, enabled) -> k = "battlemage.taunt" && not enabled) (costs ()));
  check_bool "a transition hands the turn back" true
    (match Player.pop_transition (player u 0) with Some (Wait_player (0, _)) -> true | _ -> false)

(* -------------------------------------------------------------- powermancer *)

let power_data u pid =
  match (player u pid).pstate.ps_data with Power_data d -> d | _ -> failwith "no power data"

let bargain_is_locked_early () =
  let u = new_game ~mana:all_mana ~special:Powermancer.house () in
  let buy () =
    step u (fun () ->
        Player.submit (player u 0)
          (Some
             {
               cmd_player = 0;
               cmd_card = card_by_key "powermancer.bargain";
               cmd_input = None;
               cmd_cost = 1;
               cmd_flag = None;
             }))
  in
  buy ();
  check_bool "refused on turn one" false (power_data u 0).pw_bargain;
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  step u (fun () -> Player.apply_effects (player u 0) On_end_turn);
  check_int "two turns counted" 2 (power_data u 0).pw_turn;
  buy ();
  check_bool "bought" true (power_data u 0).pw_bargain

let split_payment () =
  let u = new_game ~mana:[ 2; 4; 4; 4; 10 ] ~special:Powermancer.house () in
  step u (fun () -> Player.set_data (player u 0) (Power_data { (power_data u 0) with pw_bargain = true }));
  let cost = (card_by_key "fire.dragon").cost in
  step u (fun () ->
      Player.submit (player u 0)
        (Some
           {
             cmd_player = 0;
             cmd_card = card_by_key "fire.dragon";
             cmd_input = Some 0;
             cmd_cost = cost;
             cmd_flag = None;
           }));
  check_string "played" "fire.dragon" (Option.get (occupant u 0 0));
  check_int "fire pool spent" 0 (mana u 0 0);
  check_int "special pays the rest" (10 - (cost - 2)) (mana u 0 4)

let burial_sacrifices () =
  let u = new_game ~mana:all_mana ~special:Powermancer.house () in
  place u 0 "fire.dragon" 0;
  let cost = (card_by_key "fire.dragon").cost in
  play u 0 "powermancer.burial" ~input:(Some 0) ~cost:2;
  check_bool "gone" true (is_empty u 0 0);
  check_int "fire +3" 9 (mana u 0 0);
  check_int "water +1" 5 (mana u 0 1);
  check_int "life" (60 + min (2 * cost) 10) (player_life u 0)

(* cost of a basic card as the player sees it, after the desc mods *)
let cost_of u pid house key =
  let pd = (player u pid).pstate.ps_desc in
  let h = pd.init_desc.pd_houses.(house) in
  let cards = List.fold_left (fun cs m -> m h cs) h.phd_cards pd.desc_mods in
  (List.find (fun cd -> cd.cd_card.key = key) (Array.to_list cards)).cd_cost

let tower_discounts () =
  let u = new_game ~mana:all_mana ~special:Powermancer.house () in
  let creature = cost_of u 0 0 "fire.drake" and spell = cost_of u 0 0 "fire.wave" in
  place u 0 "powermancer.tower" 0;
  place u 0 "powermancer.tower" 1;
  check_int "creatures 1 less, once" (creature - 1) (cost_of u 0 0 "fire.drake");
  check_int "spells 2 less, once" (spell - 2) (cost_of u 0 0 "fire.wave");
  kill u 0 0;
  check_int "still one tower" (creature - 1) (cost_of u 0 0 "fire.drake");
  kill u 0 1;
  check_int "none left" creature (cost_of u 0 0 "fire.drake")

let tower_pings () =
  let u = new_game ~mana:all_mana ~special:Powermancer.house () in
  place u 0 "powermancer.tower" 0;
  play u 0 "fire.goblin" ~input:(Some 2) ~cost:1;
  check_int "creature play pings 3" (60 - 3) (player_life u 1);
  (player u 0).pstate.ps_life <- 40;
  play u 0 "fire.wave" ~input:None ~cost:1;
  check_bool "spell play heals 3" true (player_life u 0 >= 43)

let hunter_feeds_special () =
  let u = new_game ~mana:all_mana ~special:Powermancer.house () in
  place u 0 "powermancer.hunter" 0;
  place u 1 "fire.dragon" 0;
  step u (fun () -> Player.run_slots (player u 0));
  check_int "special +1" 11 (mana u 0 4)

let hunter_drains_on_player () =
  let u = new_game ~mana:all_mana ~special:Powermancer.house () in
  place u 0 "powermancer.hunter" 0;
  let total () = List.fold_left (fun n i -> n + mana u 1 i) 0 [ 0; 1; 2; 3; 4 ] in
  let before = total () in
  step u (fun () -> Player.run_slots (player u 0));
  check_int "a random pool loses one" (before - 1) (total ())

let equilibrium_taxes () =
  let u = new_game ~mana:[ 3; 4; 5; 7; 2 ] ~special:Powermancer.house () in
  (player u 0).pstate.ps_life <- 40;
  play u 0 "powermancer.equilibrium" ~input:None ~cost:1;
  check_int "hits for their best pool" (60 - 7) (player_life u 1);
  check_int "drains it" 5 (mana u 1 3);
  check_int "heals for my worst pool (special, after paying 1)" 41 (player_life u 0);
  check_int "and fills it" 3 (mana u 0 4)

let hound_shifts_growth () =
  let u = new_game ~mana:all_mana ~special:Powermancer.house () in
  let g pid = (player u pid).houses_update.hu_value.(4).growth in
  let a = g 0 and b = g 1 in
  play u 0 "powermancer.hound" ~input:(Some 0) ~cost:6;
  check_int "mine +1" (a + 1) (g 0);
  check_int "theirs -1" (max 0 (b - 1)) (g 1)

let stasis_freezes () =
  let u = new_game ~mana:all_mana ~special:Powermancer.house () in
  place u 1 "fire.dragon" 0;
  play u 0 "powermancer.stasis" ~input:None ~cost:7;
  check_int "their pools -1" 5 (mana u 1 0);
  check_bool "stunned" true (flagged u 1 0 stun_flag);
  step u (fun () -> Player.apply_effects (player u 1) On_end_turn);
  let special = (player u 1).pstate.ps_desc.init_desc.pd_houses.(0) in
  let cards =
    List.fold_left (fun cs m -> m special cs) special.phd_cards (player u 1).pstate.ps_desc.desc_mods
  in
  check_bool "nothing disabled afterwards" true (Array.for_all (fun cd -> cd.cd_enabled) cards)

let rainbow_summons_four () =
  let u = new_game ~mana:all_mana ~special:Powermancer.house () in
  play u 0 "powermancer.rainbow" ~input:None ~cost:8;
  let avatars =
    List.filter
      (fun n ->
        match occupant u 0 n with
        | Some k -> String.length k > 18 && String.sub k 0 18 = "powermancer.avatar"
        | None -> false)
      base_slot_list
  in
  check_int "four" 4 (List.length avatars);
  check_int "fire growth +1" 2 (player u 0).houses_update.hu_value.(0).growth

let () =
  let open Alcotest in
  run "houses_b"
    [
      ( "antimancer",
        [
          test_case "costs" `Quick antimancer_costs;
          test_case "resist" `Quick resist_fills_quiet_slots;
          test_case "retaliator" `Quick retaliator_hits_back;
          test_case "martyr" `Quick martyr_returns_after_spell;
          test_case "harvester" `Quick harvester_gives_mana;
          test_case "voodoo" `Quick voodoo_drains_and_heals;
          test_case "bombardier death" `Quick bombardier_blast;
          test_case "bombardier splash" `Quick bombardier_splash;
          test_case "mirror priest" `Quick mirror_priest_copies_mana;
          test_case "bribe" `Quick bribe_steals;
        ] );
      ( "junk",
        [
          test_case "screamers" `Quick screamers_scream;
          test_case "poison" `Quick poison_flower_spreads;
          test_case "fortune" `Quick fortune_absorbs_once;
          test_case "assassin roams" `Quick assassin_roams;
          test_case "assassin stays" `Quick assassin_stays_when_opposed;
          test_case "cyborg" `Quick cyborg_fills_and_eats;
          test_case "factory" `Quick factory_copies_cheap_neighbour;
          test_case "chain mirror" `Quick chain_controller_mirrors;
          test_case "chain gap" `Quick chain_controller_closes_gap;
          test_case "bot" `Quick recycling_bot_heals;
        ] );
      ( "battlemage",
        [
          test_case "rune" `Quick rune_warrior_blunts_spells;
          test_case "field mage" `Quick field_mage_takes_spells;
          test_case "field mage once" `Quick field_mage_not_twice;
          test_case "sergeant" `Quick sergeant_charges_neighbours;
          test_case "knight" `Quick knight_charges_twice;
          test_case "knight empty" `Quick knight_one_hit_on_empty;
          test_case "cavalier" `Quick cavalier_counts_gaps;
          test_case "cavalier ban" `Quick cavalier_bans_apocalypse;
          test_case "taunt" `Quick taunt_own_creature;
          test_case "warcry" `Quick warcry_silences;
          test_case "retinue" `Quick retinue_discounts_next;
        ] );
      ( "powermancer",
        [
          test_case "bargain lock" `Quick bargain_is_locked_early;
          test_case "split payment" `Quick split_payment;
          test_case "burial" `Quick burial_sacrifices;
          test_case "tower cost" `Quick tower_discounts;
          test_case "tower ping" `Quick tower_pings;
          test_case "hunter special" `Quick hunter_feeds_special;
          test_case "hunter drain" `Quick hunter_drains_on_player;
          test_case "equilibrium" `Quick equilibrium_taxes;
          test_case "hound" `Quick hound_shifts_growth;
          test_case "stasis" `Quick stasis_freezes;
          test_case "rainbow" `Quick rainbow_summons_four;
        ] );
    ]
