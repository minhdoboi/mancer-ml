(* Special house Beastlords, by Fail. The reveal mechanic.

   Every card a player plays is "revealed" for good: its identity
   (house_index, card_index) joins the player's revealed set, carried on
   [Beast_data] so the board can show the count beside the portrait. The
   moment a new card is revealed, every beast on the board hears it through
   its reaction's [on_reveal] field.

   Wolverine Pack buys one more play the way the Wind's multi-phase cards do:
   a [Wait_player] transition hands the turn back to the same player, and a
   desc mod bans a second copy until the turn ends. *)

open Sp
open Engine
open Common

(* ------------------------------------------------------------------ state *)

let empty_data = { bd_revealed = [] }
let get_data p = match p.pstate.ps_data with Beast_data d -> d | _ -> empty_data
let set_data p d = Player.set_data p (Beast_data d)

let identity (c : card) = (c.house_index, c.card_index)

(* The distinct cards this player has revealed so far, oldest first. *)
let revealed p = (get_data p).bd_revealed
let revealed_count p = List.length (revealed p)
let revealed_of_house p house_index = List.length (List.filter (fun (h, _) -> h = house_index) (revealed p))

(* ------------------------------------------------------------ desc mods *)

(* Poison Dart waits until five cards are revealed. The count never goes
   down, so the gate opens once, for good. *)
let gate_mod : desc_mod =
  fun _ cards ->
    Array.map
      (fun cd -> if cd.cd_card.key = "beastlord.poison" then { cd with cd_enabled = false } else cd)
      cards

(* The card Wolverine Pack bans by name: a second copy this turn. *)
let wolverine_ban : desc_mod =
  fun _ cards ->
    Array.map
      (fun cd -> if cd.cd_card.key = "beastlord.wolverine" then { cd with cd_enabled = false } else cd)
      cards

let wolverine_phase = "wolverine phase"

(* One more play this turn: the transition hands the turn back to the same
   player, and the ban lifts when the turn ends. *)
let extra_play ban phase : card_effect =
  fun env ->
    let p = player_of_env env in
    Player.add_desc_mod p ban;
    Player.add_transition p (Wait_player (p.pid, Some phase));
    Player.add_effect
      p
      ( On_end_turn,
        one_time_player_effect (fun e -> Player.remove_desc_mod (player_of_env e) ban))

(* ------------------------------------------------------------- the reveal *)

(* The fresh reveal reaches every beast on the board through its reaction. *)
let notify_reveal p (c : card) =
  Array.iter
    (fun s -> match s.value with Some st -> st.reaction.on_reveal c | None -> ())
    p.slots_update.slot_updates

(* A submitted command reveals its card if it is a new one. *)
let reveal p (c : card) =
  let d = get_data p in
  let id = identity c in
  if not (List.mem id d.bd_revealed) then (
    set_data p { bd_revealed = d.bd_revealed @ [ id ] };
    notify_reveal p c;
    if revealed_count p >= 5 then Player.remove_desc_mod p gate_mod)

(* ------------------------------------------------------------- Blood Hound *)

let hound_ref : card option ref = ref None
let hound_bonus : attack_source = Attack_func (fun a -> a + 2)

(* --------------------------------------------------------------- the cards *)

let poison =
  spell ~key:"beastlord.poison" ~description:"beastlord.poison.description"
    ~input_spec:Select_target_creature
    ~effects:(effects [ (Direct, damage_creature 11) ])
    ()

let hound = creature ~key:"beastlord.hound" ~attack:(attack 3) ~life:16 ~description:"beastlord.hound.description" ()

let wolverine =
  creature ~key:"beastlord.wolverine" ~attack:(attack 4) ~life:16
    ~description:"beastlord.wolverine.description"
    ~effects:(effects [ (Direct, extra_play wolverine_ban wolverine_phase) ])
    ()

let minotaur =
  let r () =
    let r = Reaction.default () in
    { r with
        on_reveal =
          (fun _ ->
            let me = Reaction.bound_slot r in
            Option.iter
              (fun st ->
                (* grows itself: life and max life together *)
                Slot.update me (fun st -> { st with life = st.life + 5; max_life = st.max_life + 5 }))
              me.value)
    }
  in
  creature ~key:"beastlord.minotaur" ~attack:(attack 7) ~life:40
    ~description:"beastlord.minotaur.description" ~new_reaction:r ()

let elephant =
  let r () =
    let r = Reaction.default () in
    { r with
        on_reveal =
          (fun c ->
            let me = Reaction.bound_slot r in
            incr_mana_from me me.slots.player 2 [ c.house_index ])
    }
  in
  creature ~key:"beastlord.elephant" ~attack:(attack 6) ~life:39
    ~description:"beastlord.elephant.description" ~new_reaction:r ()

let ritual_effect : card_effect =
  fun env ->
    let p = player_of_env env in
    let amount = 2 * revealed_count p in
    Player.heal p amount;
    Slots.heal_creatures p.slots_update amount

let ritual_card =
  spell ~key:"beastlord.ritual" ~description:"beastlord.ritual.description"
    ~effects:(effects [ (Direct, ritual_effect) ])
    ()

let sabretooth =
  let r () =
    let r = Reaction.default () in
    { r with
        on_reveal =
          (fun _ ->
            let me = Reaction.bound_slot r in
            match me.value with
            | None -> ()
            | Some st ->
                let ctx =
                  { ctx_player = me.slots.player.pid; ctx_card = Some st.card.c_card; ctx_selected = me.num }
                in
                Slots.inflict_creatures (Player.other me.slots.player).slots_update
                  (mk_damage ~is_ability:true 5 ctx))
    }
  in
  creature ~key:"beastlord.sabretooth" ~attack:(attack 10) ~life:42
    ~description:"beastlord.sabretooth.description" ~new_reaction:r ()

(* Fire is the first basic house whatever the mode calls it. *)
let fire_house_index = 0
let wyvern =
  creature ~key:"beastlord.wyvern"
    ~attack:{ base = Some 4; sources = [ Attack_state_func (fun atk p -> atk + revealed_of_house p fire_house_index) ] }
    ~life:29 ~description:"beastlord.wyvern.description" ()

let () = hound_ref := Some hound

(* ---------------------------------------------------------------- listener *)

let beast_listener () =
  let player = ref None in
  {
    default_listener with
    hl_init =
      (fun p ->
        player := Some p;
        (* the dart gate: shut while fewer than five cards are revealed *)
        if revealed_count p < 5 then Player.add_desc_mod p gate_mod;
        (* Blood Hound: every wound the opposite lane takes feeds the pack *)
        Array.iter
          (fun (opp : slot_update) ->
            let old = opp.on_damage_hook in
            opp.on_damage_hook <-
              (fun st d ->
                old st d;
                if d.amount > 0 then
                  let mine = p.slots_update.slot_updates.(opp.num) in
                  match mine.value with
                  | Some hound when is_card !hound_ref hound.card.c_card -> add_attack mine hound_bonus
                  | _ -> ()))
          (other_player p).slots_update.slot_updates;
        (* every card the player plays is a reveal, announced before the card
           resolves: a spell counts itself, a creature arrives too late to
           hear its own reveal *)
        let old_submit = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            reveal p c.cmd_card;
            old_submit c));
  }

(* The list is in printed cost order, so the deck builder's default index+1
   gives the printed costs. *)
let cards = [ poison; hound; wolverine; wyvern; ritual_card; minotaur; elephant; sabretooth ]

let house =
  special_house ~key:"beastlord" ~id:45 ~listener:beast_listener ~data:(Beast_data empty_data) cards

(* Printed costs: Poison Dart 1, Blood Hound 2, Wolverine Pack 3, Wyvern 4,
   Ancient Ritual 5, Minotaur 6, Slumbering Elephant 7, Sabretooth Pack 8.
   The dealt special deck takes one card of every quarter: Poison Dart,
   Wolverine Pack, Ancient Ritual, Slumbering Elephant. *)
let cost_fn i = i + 1