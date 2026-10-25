(* Special house Powermancer — port of priv/sp/house/Powermancer.scala.

   Everything converts into everything else, taxed through the special pool. The special pool pays for basic
   cards (Infernal Bargain), the player keeps one availability mod per special mana value in [power_data]. *)

open Sp
open Engine
open Common

let hunter_ref : card option ref = ref None
let tower_ref : card option ref = ref None
let avatars_ref : card list ref = ref []
let bargain_phase = "Infernal bargain phase"
let initial_data = { pw_bargain = false; pw_turn = 0; pw_mod = None }
let get_data (p : player_update) = match p.pstate.ps_data with Power_data d -> d | _ -> initial_data
let set_data p d = Player.set_data p (Power_data d)
let special_mana (p : player_update) = p.houses_update.hu_value.(4).mana

(* ---------------------------------------------------------------- desc mods *)

(* Bargained basic cards keep their real cost, but special mana counts toward their availability *)
let bargain_mod special : desc_mod =
 fun house cards ->
  if house.phd_house.h_index >= special_house_index then cards
  else Array.map (fun cd -> { cd with cd_extra_mana = special }) cards

(* One discount, however many Towers: spells cost 2 less, other cards 1 less *)
let tower_cost_mod : desc_mod =
 fun _ cards ->
  Array.map (fun cd -> { cd with cd_cost = max 0 (cd.cd_cost - if cd.cd_card.is_spell then 2 else 1) }) cards

(* Keeps the bargain availability mod in sync with the special pool *)
let refresh_bargain p =
  let d = get_data p in
  if d.pw_bargain then
    let mana = special_mana p in
    match d.pw_mod with
    | Some (m, _) when m = mana -> ()
    | old ->
        Option.iter (fun (_, md) -> Player.remove_desc_mod p md) old;
        let md = bargain_mod mana in
        Player.add_desc_mod p md;
        set_data p { d with pw_mod = Some (mana, md) }

(* ------------------------------------------------------------------- spells *)

let infernal_bargain : card_effect =
 fun env ->
  let p = player_of_env env in
  set_data p { (get_data p) with pw_bargain = true };
  refresh_bargain p;
  Player.add_transition p (Wait_player (env.env_player, Some bargain_phase))

let bargain =
  spell ~key:"powermancer.bargain" ~description:"powermancer.bargain.description"
    ~effects:(effects [ (Direct, infernal_bargain) ])
    ()

(* The bargain is locked for the first two turns: it reads as disabled until the end of the second *)
let hide_bargain : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> special_house_index then cards
  else Array.map (fun cd -> if cd.cd_card.id = bargain.id then { cd with cd_enabled = false } else cd) cards

(* Sacrifice an own creature for +3 of its type, +1 of every other, and life (twice its cost, at most 10) *)
let eternal_burial : card_effect =
 fun env ->
  let p = player_of_env env in
  let slot = own_slot_of_env env in
  Option.iter
    (fun (st : slot_state) ->
      let card = st.card.c_card in
      let idx = card.house_index in
      Slot.destroy slot;
      Houses.incr_mana p.houses_update 3 [ idx ];
      Houses.incr_mana p.houses_update 1 (List.filter (( <> ) idx) [ 0; 1; 2; 3; 4 ]);
      Player.heal p (min (2 * card.cost) 10))
    slot.value

let extreme better (houses : house_state array) =
  let best = ref 0 in
  Array.iteri (fun i h -> if better h.mana houses.(!best).mana then best := i) houses;
  !best

(* Tax the opponent's best pool, top up your worst *)
let equilibrium : card_effect =
 fun env ->
  let p = player_of_env env and op = other_of_env env in
  let opp = op.houses_update.hu_value in
  let hi = extreme ( > ) opp in
  Player.inflict op (mk_damage ~is_spell:true opp.(hi).mana (ctx_of_env env));
  Houses.incr_mana op.houses_update (-2) [ hi ];
  let own = p.houses_update.hu_value in
  let lo = extreme ( < ) own in
  Player.heal p own.(lo).mana;
  Houses.incr_mana p.houses_update 2 [ lo ]

(* Astral Hound: shift special growth both ways *)
let hound_summon : card_effect =
 fun env ->
  Houses.incr_growth (player_of_env env).houses_update 1 [ 4 ];
  Houses.incr_growth (other_of_env env).houses_update (-1) [ 4 ]

(* Stasis: drain, silence and stun the opponent for a turn *)
let stasis : card_effect =
 fun env ->
  let op = other_of_env env in
  Houses.incr_mana op.houses_update (-1) [ 0; 1; 2; 3; 4 ];
  Player.add_desc_mod op skip_turn_mod;
  Player.add_effect op (On_end_turn, one_time_player_effect (remove_desc_mod skip_turn_mod));
  Slots.foreach op.slots_update Slot.stun

(* Rainbow Avatars: four 2/16 generators in random empty slots *)
let rainbow : card_effect =
 fun env ->
  let p = player_of_env env in
  let rng = env.env_updater.rng in
  let picks = List.filteri (fun i _ -> i < 4) (Dice.shuffle rng (Slots.get_open p.slots_update)) in
  let tokens = Dice.shuffle rng !avatars_ref in
  List.iteri
    (fun i num ->
      match List.nth_opt tokens i with
      | Some token -> Slots.add_creature p.slots_update.slot_updates.(num) (make_creature token)
      | None -> ())
    picks

(* ----------------------------------------------------------------- the tower *)

let tower_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num = me.num && not (has_desc_mod me.slots.player tower_cost_mod) then
          Player.add_desc_mod me.slots.player tower_cost_mod);
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        let others =
          Array.exists
            (fun s ->
              s.num <> me.num
              && match s.value with Some st -> is_card !tower_ref st.card.c_card | None -> false)
            me.slots.slot_updates
        in
        if not others then Player.remove_desc_mod me.slots.player tower_cost_mod);
  }

(* Every play of the owner pings the opponent for 3, or heals 3 if it was a spell *)
let tower_ping (owner : player_update) (c : command) =
  match
    Array.find_opt
      (fun s -> match s.value with Some st -> is_card !tower_ref st.card.c_card | None -> false)
      owner.slots_update.slot_updates
  with
  | None -> ()
  | Some slot ->
      if c.cmd_card.is_spell then Player.heal owner 3
      else
        Player.inflict (other_player owner)
          (mk_damage ~is_ability:true 3
             { ctx_player = owner.pid; ctx_card = !tower_ref; ctx_selected = slot.num })

(* ----------------------------------------------------------------- turn gate *)

(* Infernal Bargain opens on your third turn. Own turns are counted when they end: On_turn is skipped for the
   starting player's first turn, which would put the second player one turn ahead. *)
let veil_bargain : card_effect =
 fun env ->
  let p = player_of_env env in
  if not (has_desc_mod p hide_bargain) then Player.add_desc_mod p hide_bargain

let count_turns : card_effect =
 fun env ->
  let p = player_of_env env in
  let d = get_data p in
  set_data p { d with pw_turn = d.pw_turn + 1 };
  if d.pw_turn + 1 >= 2 then Player.remove_desc_mod p hide_bargain

(* Special grows at turn start: keep bargained availability in sync *)
let refresh_turn : card_effect = fun env -> refresh_bargain (player_of_env env)

(* ------------------------------------------------------------ split payment *)

(* Spend the whole type pool and cover the rest from special. Only the special shortfall is debited here: the
   command goes on with cost = type pool, so the normal submit path drains the type pool and the paid cost stays
   honest. *)
let can_split_play p (c : command) =
  (get_data p).pw_bargain
  && c.cmd_card.house_index < special_house_index
  &&
  let pool = p.houses_update.hu_value.(c.cmd_card.house_index).mana in
  pool < c.cmd_cost && pool + special_mana p >= c.cmd_cost

let apply_split p (c : command) =
  let pool = p.houses_update.hu_value.(c.cmd_card.house_index).mana in
  Houses.incr_mana p.houses_update (-(c.cmd_cost - pool)) [ 4 ];
  { c with cmd_cost = pool }

let hunter_dealt (owner : player_update) (d : damage) =
  (match d.context.ctx_card with Some c -> is_card !hunter_ref c | None -> false)
  && d.context.ctx_player = owner.pid && d.amount > 0

let power_listener () =
  let player = ref None in
  let get () = match !player with Some p -> p | None -> failwith "powermancer listener not inited" in
  {
    default_listener with
    hl_intercept_submit =
      (fun c ->
        let p = get () in
        match c with
        | Some cmd
          when cmd.cmd_player = p.pid && is_card (Some bargain) cmd.cmd_card && (get_data p).pw_turn < 2 ->
            (* the bargain itself is locked for the first two turns, engine-side too *)
            (true, None)
        | Some cmd
          when cmd.cmd_player = p.pid && (not (is_card (Some bargain) cmd.cmd_card)) && can_split_play p cmd
          ->
            let paid = apply_split p cmd in
            refresh_bargain p;
            (* claiming the submit hides it from the opponent's listener: forward the rewritten command so
               that e.g. a Warlock can still block it *)
            let blocked, alt = (other_player p).house_event_listener.hl_intercept_submit (Some paid) in
            (true, if blocked then alt else Some paid)
        | _ -> (false, None));
    hl_init =
      (fun p ->
        player := Some p;
        let op = other_player p in
        (* Power Hunter: reward actual damage dealt, post-mitigation. The mana
           is attributed to the hunter so the UI can draw its arcs. *)
        let hunter_mana target amount idxs =
          match !hunter_ref with
          | Some c -> Option.iter (fun hs -> incr_mana_from hs target amount idxs) (Slots.find_card p.slots_update c)
          | None -> ()
        in
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.on_damage_hook in
            slot.on_damage_hook <-
              (fun st d ->
                old st d;
                if hunter_dealt p d then hunter_mana p 1 [ 4 ]))
          op.slots_update.slot_updates;
        let old_damage = op.on_player_damage_hook in
        op.on_player_damage_hook <-
          (fun d ->
            old_damage d;
            if hunter_dealt p d then hunter_mana op (-1) [ Dice.int p.updater.rng 5 ]);
        let old_submit = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let r = old_submit c in
            tower_ping p c;
            refresh_bargain p;
            r));
  }

let hunter =
  creature ~key:"powermancer.hunter" ~attack:(attack 5) ~life:14 ~description:"powermancer.hunter.description"
    ()

let tower =
  creature ~key:"powermancer.tower" ~attack:(attack 0) ~life:25 ~description:"powermancer.tower.description"
    ~new_reaction:tower_reaction ()

let avatar element idx =
  creature ~key:("powermancer.avatar." ^ element) ~attack:(attack 2) ~life:16
    ~description:("powermancer.avatar." ^ element ^ ".description")
    ~new_reaction:(mana_growth_reaction ~owner:true ~amount:1 ~house_indexes:[ idx ])
    ()

let avatars = [ avatar "fire" 0; avatar "water" 1; avatar "air" 2; avatar "earth" 3 ]

let () =
  hunter_ref := Some hunter;
  tower_ref := Some tower;
  avatars_ref := avatars

let cards =
  [
    bargain;
    spell ~key:"powermancer.burial" ~description:"powermancer.burial.description"
      ~input_spec:Select_owner_creature
      ~effects:(effects [ (Direct, eternal_burial) ])
      ();
    hunter;
    tower;
    spell ~key:"powermancer.equilibrium" ~description:"powermancer.equilibrium.description"
      ~effects:(effects [ (Direct, equilibrium) ])
      ();
    creature ~key:"powermancer.hound" ~attack:(attack 1) ~life:26 ~description:"powermancer.hound.description"
      ~effects:(effects [ (Direct, hound_summon) ])
      ();
    spell ~key:"powermancer.stasis" ~description:"powermancer.stasis.description"
      ~effects:(effects [ (Direct, stasis) ])
      ();
    spell ~key:"powermancer.rainbow" ~description:"powermancer.rainbow.description"
      ~effects:(effects [ (Direct, rainbow) ])
      ();
  ]

let house =
  special_house ~key:"powermancer" ~id:21
    ~effects:[ (On_start, veil_bargain); (On_turn, refresh_turn); (On_end_turn, count_turns) ]
    ~additional_cards:avatars ~listener:power_listener ~data:(Power_data initial_data) cards
