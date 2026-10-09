(* Card constructors and reaction helpers shared by houses.  Mirrors
   priv/sp/house/Common.scala and priv/sp/house/package.scala. *)

open Sp
open Engine

(* ------------------------------------------------------------ constructors *)

let attack base = { base = Some base; sources = [] }
let mana_attack idx = { base = None; sources = [ Mana_attack idx ] }
let add_source s src = { s with sources = s.sources @ [ src ] }
let normal_run_attack = { can_attack = true; apply_ = Single_target }
let multi_run_attack = { can_attack = true; apply_ = Multi_target }
let never_run_attack = { can_attack = false; apply_ = Single_target }
let custom_run_attack apply = { can_attack = true; apply_ = Custom apply }

let new_card ~key ~effects ~input_spec ~description ~is_spell ~body =
  {
    key;
    name = key;
    cost = 0;
    house_index = 0;
    card_index = 0;
    house_id = 0;
    id = fresh_card_id ();
    effects;
    input_spec;
    description;
    is_spell;
    creature_body = body;
  }

let creature ~key ~attack ~life ~description ?(effects = no_effects) ?(input_spec = Select_owner_slot) ?mod_
    ?(new_reaction = Reaction.default) ?(run_attack = normal_run_attack) ?(is_altar = false) ?(status = 0) ()
    =
  let body =
    {
      b_attack = attack;
      b_life = life;
      b_new_reaction = new_reaction;
      b_run_attack = run_attack;
      b_is_altar = is_altar;
      b_status = status;
      b_mod = mod_;
      b_data = No_data;
    }
  in
  new_card ~key ~effects ~input_spec ~description ~is_spell:false ~body:(Some body)

let spell ~key ~description ?(input_spec = No_input) ~effects () =
  new_card ~key ~effects ~input_spec ~description ~is_spell:true ~body:None

(* ----------------------------------------------------------- attack bonus *)

(* AttackSources.removeFirst uses Scala `==`: case-class equality for keyed
   sources, reference equality for anonymous closures.  Never use OCaml's `=`
   here, it raises on functional values. *)
let same_source a b =
  match (a, b) with
  | Attack_bonus x, Attack_bonus y -> x.tag = y.tag && x.pos = y.pos
  | Mana_attack x, Mana_attack y -> x = y
  | Extra_attacks x, Extra_attacks y -> x = y
  | Aging_malus, Aging_malus -> true
  | _ -> a == b

let rec remove_first src = function
  | [] -> []
  | x :: xs -> if same_source x src then xs else x :: remove_first src xs

(* AttackUpdate.add: creatures with a printed attack of 0 take no bonus *)
let add_attack s src =
  Slot.update s (fun st ->
      if st.attack_sources.base = Some 0 then st
      else { st with attack_sources = { st.attack_sources with sources = src :: st.attack_sources.sources } })

let remove_first_attack s src =
  Slot.update s (fun st ->
      {
        st with
        attack_sources = { st.attack_sources with sources = remove_first src st.attack_sources.sources };
      })

let remove_any_attack s src =
  Slot.update s (fun st ->
      {
        st with
        attack_sources =
          {
            st.attack_sources with
            sources = List.filter (fun x -> not (same_source x src)) st.attack_sources.sources;
          };
      })

let has_source s pred =
  match s.value with Some st -> List.exists pred st.attack_sources.sources | None -> false

(* ------------------------------------------------------------ board helpers *)

let slot_interval n m =
  let lo = max 0 n and hi = min 5 m in
  if hi < lo then [] else List.init (hi - lo + 1) (fun i -> lo + i)

let reduce_filled su cmp =
  Slots.foldl su None (fun acc s -> match acc with None -> Some s | Some a -> Some (cmp a s))

let lowest_life a b = if (Slot.get b).life < (Slot.get a).life then b else a
let strongest a b = if (Slot.get b).attack > (Slot.get a).attack then b else a

let nearest_empty selected (p : player_update) =
  p.pstate.ps_slot_list
  |> List.filter_map (fun n ->
      if Option.is_none p.slots_update.slot_updates.(n).value then Some (n, abs (n - selected)) else None)
  |> function
  | [] -> None
  | xs ->
      Some (List.fold_left (fun (n, d) (n', d') -> if d' < d then (n', d') else (n, d)) (List.hd xs) xs |> fst)

let non_special pid (state : game_state) =
  List.init nb_slots Fun.id
  |> List.filter (fun n ->
      match state.players.(pid).ps_slots.(n) with Some st -> st.card.c_card.house_index < 4 | None -> false)

let countdown n f =
  let remaining = ref n in
  let rec tick env =
    decr remaining;
    if !remaining <= 0 then (
      f env;
      Player.remove_effect (player_of_env env) (fun e -> e == tick))
  in
  tick

let ability ~from_card ~to_card : desc_mod =
 fun house cards ->
  if house.phd_house.h_index = 4 then
    Array.map
      (fun c ->
        if c.cd_card.id = from_card.id then
          { cd_card = to_card; cd_cost = to_card.cost; cd_enabled = true; cd_extra_mana = 0 }
        else c)
      cards
  else cards

let incr_basic_cost_mod : desc_mod =
 fun house cards ->
  if house.phd_house.h_index = 4 then cards else Array.map (fun c -> { c with cd_cost = c.cd_cost + 1 }) cards

let incr_fire_cost_mod : desc_mod =
 fun house cards ->
  if house.phd_house.h_index = 0 then Array.map (fun c -> { c with cd_cost = c.cd_cost + 1 }) cards else cards

let hide_basic_mod : desc_mod =
 fun house cards ->
  if house.phd_house.h_index = 4 then cards else Array.map (fun c -> { c with cd_enabled = false }) cards

(* AttackBonusReaction (Common.scala:91): grants `get_bonus pos` to every slot
   `num` with `cond pos num`, where pos is the giver's slot. *)
let attack_bonus_reaction ~cond ~get_bonus () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        let bonus = get_bonus me.num in
        if me.num = slot.num then
          Slots.foreach slot.slots (fun s -> if cond s.num slot.num then add_attack s bonus)
        else if cond me.num slot.num then add_attack slot bonus);
    on_remove =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if me.num <> slot.num && cond me.num slot.num then remove_first_attack slot (get_bonus me.num));
    on_my_remove =
      (fun _ ->
        let me = Reaction.bound_slot r in
        let bonus = get_bonus me.num in
        Slots.foreach me.slots (fun s -> if cond s.num me.num then remove_first_attack s bonus));
  }

(* ------------------------------------------------------------ mana growth *)

(* ManaGrowthReaction (house/package.scala:45): raises the owner's (or the
   opponent's) growth while the creature is on the board *)
let mana_growth_reaction ~owner ~amount ~house_indexes () =
  let r = Reaction.default () in
  let houses () =
    let p = (Reaction.bound_slot r).slots.player in
    (if owner then p else other_player p).houses_update
  in
  {
    r with
    on_add =
      (fun slot ->
        if slot.num = (Reaction.bound_slot r).num then Houses.incr_growth (houses ()) amount house_indexes);
    on_my_remove = (fun _ -> Houses.incr_growth (houses ()) (-amount) house_indexes);
  }
