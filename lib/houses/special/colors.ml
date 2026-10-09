(* Special house Colors — port of priv/sp/house/Colors.scala.

   Costs are 0,1,2,2,3,4,4,4. Playing a special card snapshots the special pool into player data (X), then
   the after-submit hook spends the rest of the pool on the creature. Direct effects read player data because
   slot data is not set until after the summon. Desc mods are kept by physical equality, so HideCardMod and
   Emerald's cost bump are stored on the player / in [Color_x]. *)

open Sp
open Engine
open Common

let is_card opt (c : card) = match opt with Some x -> x.id = c.id | None -> false
let discolor_phase = "discolor phase"
let violet_ref : card option ref = ref None
let azure_ref : card option ref = ref None
let empty_attack = { base = None; sources = [] }
let gold_bonus = Attack_func (fun a -> a + 1)
let slot_of_env env = (player_of_env env).slots_update.slot_updates.(env.env_selected)
let cost_fn i = match i with 0 -> 0 | 1 -> 1 | 2 | 3 -> 2 | 4 -> 3 | 5 | 6 | 7 -> 4 | n -> n
let get_slot_x = function Color_x (x, _) -> x | _ -> 0
let slot_x (s : slot_update) = match s.value with Some st -> get_slot_x st.data | None -> 0
let color_player_of = function Color_data d -> d | _ -> { cp_x = 0; cp_hidden = [] }
let get_player_x p = (color_player_of p.pstate.ps_data).cp_x
let set_player p d = Player.set_data p (Color_data d)

let incr_special_cost_mod x : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> 4 then cards
  else Array.map (fun cd -> { cd with cd_cost = cd.cd_cost + x }) cards

let lower_2x_cost_mod x : desc_mod =
 fun _house cards ->
  Array.map
    (fun cd -> if cd.cd_card.cost <= 2 * x then { cd with cd_cost = max 0 (cd.cd_cost - 1) } else cd)
    cards

let hide_card_mod (card : card) : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> card.house_index then cards
  else Array.map (fun cd -> if cd.cd_card.id = card.id then { cd with cd_enabled = false } else cd) cards

let select_colour pid (state : game_state) =
  List.init nb_slots Fun.id
  |> List.filter (fun n ->
      match state.players.(pid).ps_slots.(n) with Some st -> st.card.c_card.house_index = 4 | None -> false)

let is_multi (c : card) =
  match c.creature_body with
  | Some b -> ( match b.b_run_attack.apply_ with Multi_target -> true | _ -> false)
  | None -> false

let discolor : card_effect =
 fun env ->
  Slot.destroy (slot_of_env env);
  Player.add_transition (player_of_env env) (Wait_player (env.env_player, Some discolor_phase))

let violet_effect : card_effect =
 fun env -> Player.add_desc_mod (player_of_env env) (lower_2x_cost_mod (get_player_x (player_of_env env)))

let azure_effect : card_effect =
 fun env ->
  let x = slot_x (slot_of_env env) in
  let d = mk_damage ~is_ability:true 4 (ctx_of_env env) in
  Slots.foreach (player_of_env env).slots_update (fun slot ->
      match slot.value with
      | Some st when st.card.c_card.cost > x - 2 && st.card.c_card.cost < x + 2 -> Slot.inflict slot d
      | _ -> ())

let silver : card_effect =
 fun env ->
  let x = slot_x (slot_of_env env) in
  Slots.heal_creatures (player_of_env env).slots_update (x - 1);
  if x > 5 then Houses.incr_mana (player_of_env env).houses_update (-1) [ 4 ]

let crimson_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun de ->
        if
          Option.is_none de.de_target
          && (not (is_effect de.de_damage))
          && not (match de.de_damage.context.ctx_card with Some c -> is_multi c | None -> false)
        then { de.de_damage with amount = de.de_damage.amount + slot_x (Reaction.bound_slot r) }
        else de.de_damage);
  }

let emerald_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num = slot.num then (
          let m = incr_special_cost_mod (get_player_x slot.slots.player) in
          Player.add_desc_mod slot.slots.player m;
          Slot.set_data me (Color_x (0, Some m))));
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        match me.value with
        | Some { data = Color_x (_, Some m); _ } -> Player.remove_desc_mod me.slots.player m
        | _ -> ());
    on_protect =
      (fun de ->
        if Option.is_none de.de_target then
          let x = slot_x (Reaction.bound_slot r) in
          { de.de_damage with amount = max 0 (de.de_damage.amount - max 0 (x - 3)) }
        else de.de_damage);
  }

let gold_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        if dead.d_num = me.num - 1 then (
          let x = slot_x me in
          let slot = me.slots.slot_updates.(dead.d_num) in
          Slots.add_creature slot dead.d_slot.card;
          Option.iter (fun st -> Slot.update slot (fun s -> { s with life = min (4 * x) st.life })) slot.value;
          Slot.destroy me;
          Slots.foreach (other_player me.slots.player).slots_update (fun s -> add_attack s gold_bonus)));
  }

let azure_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun de ->
        if
          is_effect de.de_damage
          && not (match de.de_damage.context.ctx_card with Some c -> is_card !azure_ref c | None -> false)
        then
          match de.de_target with
          | Some i -> (
              let slot = (Reaction.bound_slot r).slots.slot_updates.(i) in
              let x = slot_x (Reaction.bound_slot r) in
              match slot.value with
              | Some s when s.card.c_card.cost > x - 2 && s.card.c_card.cost < x + 2 ->
                  { de.de_damage with amount = 0 }
              | _ -> de.de_damage)
          | None -> de.de_damage
        else de.de_damage);
  }

let violet_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_protect =
      (fun de ->
        let me = Reaction.bound_slot r in
        if de.de_damage.is_spell && de.de_damage.context.ctx_player <> me.slots.player.pid then
          { de.de_damage with amount = max 0 (de.de_damage.amount - 3) }
        else de.de_damage);
  }

let amber_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        let x = get_slot_x dead.d_slot.data in
        focus_slot me;
        let houses = me.slots.player.houses_update.hu_value in
        Array.to_list houses
        |> List.mapi (fun i h -> (i, h.mana))
        |> List.sort (fun (_, a) (_, b) -> compare a b)
        |> List.rev
        |> fun ranked ->
        List.iteri
          (fun i (idx, _) -> if i < 2 then Houses.incr_mana me.slots.player.houses_update x [ idx ])
          ranked);
  }

let color_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        init_owner_death p;
        Array.iter
          (fun (slot : slot_update) ->
            let old = slot.protect_hook in
            slot.protect_hook <-
              (fun d ->
                let d = old d in
                Slots.foldl p.slots_update d (fun acc s ->
                    match s.value with
                    | Some st when is_card !violet_ref st.card.c_card || is_card !azure_ref st.card.c_card ->
                        st.reaction.on_protect { de_damage = acc; de_target = Some slot.num; de_player = p }
                    | _ -> acc)))
          p.slots_update.slot_updates;
        let old_submit = p.submit_command_hook in
        p.submit_command_hook <-
          (fun c ->
            let prev = color_player_of p.pstate.ps_data in
            set_player p { prev with cp_x = p.houses_update.hu_value.(4).mana };
            let r = old_submit c in
            if c.cmd_card.house_index = 4 then (
              let hide = hide_card_mod c.cmd_card in
              Player.add_desc_mod p hide;
              let cur = color_player_of p.pstate.ps_data in
              set_player p { cur with cp_hidden = (c.cmd_card.id, hide) :: cur.cp_hidden };
              match c.cmd_card.creature_body with
              | Some body when body.b_attack.base <> Some 0 ->
                  Option.iter
                    (fun n ->
                      let slot = p.slots_update.slot_updates.(n) in
                      if Option.is_some slot.value then (
                        let x = get_player_x p in
                        Houses.incr_mana p.houses_update (-x) [ 4 ];
                        add_attack slot (Attack_func (fun a -> a + x));
                        let old_mod =
                          match slot.value with
                          | Some st -> ( match st.data with Color_x (_, m) -> m | _ -> None)
                          | None -> None
                        in
                        Slot.set_data slot (Color_x (x, old_mod))))
                    c.cmd_input
              | _ -> ());
            r);
        let old_dead = p.slots_update.on_dead_hook in
        p.slots_update.on_dead_hook <-
          (fun dead ->
            old_dead dead;
            if dead.d_slot.card.c_card.house_index = 4 then
              let cur = color_player_of p.pstate.ps_data in
              match List.partition (fun (id, _) -> id = dead.d_slot.card.c_card.id) cur.cp_hidden with
              | (_, m) :: rest, keep ->
                  Player.remove_desc_mod p m;
                  set_player p { cur with cp_hidden = rest @ keep }
              | [], _ -> ());
        (* ice guard should be applied after crimson *)
        let op = other_player p in
        let old_guard = op.guard_hook in
        op.guard_hook <-
          (fun damage ->
            old_guard
              (Slots.foldl p.slots_update damage (fun acc s ->
                   match s.value with
                   | Some st when st.card.c_card.key = "colors.Crimson.name" ->
                       st.reaction.on_protect { de_damage = acc; de_target = None; de_player = p }
                   | _ -> acc))));
  }

let violet =
  creature ~key:"colors.Violet.name" ~attack:empty_attack ~life:29 ~description:"colors.Violet.description"
    ~effects:(effects [ (Direct, violet_effect) ])
    ~new_reaction:violet_reaction ()

let azure =
  creature ~key:"colors.Azure.name" ~attack:empty_attack ~life:24 ~description:"colors.Azure.description"
    ~effects:(effects [ (On_turn, azure_effect) ])
    ~new_reaction:azure_reaction ()

let () =
  violet_ref := Some violet;
  azure_ref := Some azure

let cards =
  [
    spell ~key:"colors.Discoloration.name" ~description:"colors.Discoloration.description"
      ~input_spec:(Select_owner select_colour)
      ~effects:(effects [ (Direct, discolor) ])
      ();
    creature ~key:"colors.Amber.name" ~attack:empty_attack ~life:21 ~description:"colors.Amber.description"
      ~new_reaction:amber_reaction ();
    violet;
    azure;
    creature ~key:"colors.Silver.name" ~attack:empty_attack ~life:33 ~description:"colors.Silver.description"
      ~effects:(effects [ (On_turn, silver) ])
      ();
    creature ~key:"colors.Gold.name" ~attack:(attack 0) ~life:35 ~description:"colors.Gold.description"
      ~new_reaction:gold_reaction ();
    creature ~key:"colors.Emerald.name" ~attack:empty_attack ~life:16
      ~description:"colors.Emerald.description" ~new_reaction:emerald_reaction ();
    creature ~key:"colors.Crimson.name" ~attack:empty_attack ~life:34
      ~description:"colors.Crimson.description" ~new_reaction:crimson_reaction ();
  ]

let house =
  {
    h_key = "colors";
    h_name = "colors";
    h_cards = cards;
    h_index = 4;
    h_effects = [];
    h_house_id = 23;
    h_additional_cards = [];
    h_make_listener = color_listener;
    h_data = Color_data { cp_x = 0; cp_hidden = [] };
  }
