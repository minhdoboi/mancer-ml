(* Special house High Priest — port of priv/sp/house/HighPriest.scala. PathSet swaps the specials for
   the water/air set when those houses outweigh fire/earth on the board. *)

open Sp
open Engine
open Common

let empty_data = { hp_revealed = []; hp_turn = 0 }
let get_data p = match p.pstate.ps_data with Priest_data d -> d | _ -> empty_data
let set_data p d = Player.set_data p (Priest_data d)
let apis_ref : card option ref = ref None
let sphinx_ref : card option ref = ref None
let puzzle_ref : card option ref = ref None
let ouro_ref : card option ref = ref None
let serpent_ref : card option ref = ref None
let sunstone_ref : card option ref = ref None
let mummy_ref : card option ref = ref None
let ra_ref : card option ref = ref None
let babi_ref : card option ref = ref None
let amit_ref : card option ref = ref None
let hp_set_ref : card list ref = ref []
let serpo_bonus = Attack_func (fun a -> int_of_float (ceil (float_of_int a *. 2.)))

let nearest_slot_opposed ?(opposed = true) selected (p : player_update) =
  let op = other_player p in
  p.pstate.ps_slot_list
  |> List.filter (fun n ->
      n <> selected
      && Option.is_none p.slots_update.slot_updates.(n).value
      && Option.is_some op.slots_update.slot_updates.(n).value = opposed)
  |> List.fold_left
       (fun best n ->
         match best with Some b when abs (b - selected) <= abs (n - selected) -> best | _ -> Some n)
       None

let path_set : desc_mod =
 fun house cards ->
  if house.phd_house.h_index <> special_house_index then cards
  else
    Array.map
      (fun c ->
        match List.nth_opt !hp_set_ref (c.cd_card.cost - 1) with
        | Some card -> { cd_card = card; cd_cost = card.cost; cd_enabled = true; cd_extra_mana = 0 }
        | None -> c)
      cards

let choose_path (p : player_update) =
  let fireearth, waterair =
    Array.fold_left
      (fun (f, w) s ->
        match s.value with
        | None -> (f, w)
        | Some st ->
            let h = st.card.c_card.house_index and cost = st.card.c_card.cost in
            if h = 0 || h = 3 then (f + cost, w) else if h < 4 then (f, w + cost) else (f, w))
      (0, 0) p.slots_update.slot_updates
  in
  let has = has_desc_mod p path_set in
  if fireearth >= waterair then (if has then Player.remove_desc_mod p path_set)
  else if not has then Player.add_desc_mod p path_set

let hp_turn : card_effect =
 fun env ->
  let p = player_of_env env in
  set_data p { (get_data p) with hp_turn = (get_data p).hp_turn + 1 }

let init_path : card_effect = fun env -> choose_path (player_of_env env)

let wajet : card_effect =
 fun env ->
  let p = player_of_env env in
  let n = List.length (get_data p).hp_revealed in
  Player.heal p n;
  Slots.heal_creatures p.slots_update n;
  Slots.inflict_creatures (other_of_env env).slots_update (spell_damage env n)

let curse : card_effect =
 fun env ->
  let slot = opp_slot_of_env env in
  let slots = slot :: Slot.filled_adjacent slot in
  let amount = List.fold_left (fun a s -> a + (Slot.get s).attack) 0 slots in
  let d = spell_damage env amount in
  List.iter (fun s -> Slot.inflict s d) slots

let apis_effect : card_effect =
 fun env ->
  let p = player_of_env env in
  Houses.incr_mana p.houses_update 1 [ 4 ];
  let nb =
    Array.fold_left
      (fun acc s -> match s.value with Some st when is_card !apis_ref st.card.c_card -> acc + 1 | _ -> acc)
      0 p.slots_update.slot_updates
  in
  if nb > 1 then (
    Player.heal p (3 * (nb - 1));
    focus_slot p.slots_update.slot_updates.(env.env_selected))

let ouro : card_effect =
 fun env ->
  let p = player_of_env env in
  let ouro_num = env.env_selected in
  Option.iter
    (fun pos ->
      Option.iter
        (fun c -> Slots.add_creature p.slots_update.slot_updates.(pos) (make_creature c))
        !serpent_ref)
    (nearest_empty ouro_num p);
  let rec die env =
    let hp = other_of_env env in
    Array.iter
      (fun (s : slot_update) ->
        match s.value with
        | Some st when is_card !serpent_ref st.card.c_card ->
            let amount = st.life in
            Player.heal hp amount;
            (match hp.slots_update.slot_updates.(ouro_num).value with
            | Some o when is_card !ouro_ref o.card.c_card ->
                Slot.heal hp.slots_update.slot_updates.(ouro_num) amount
            | _ -> Player.remove_effect (player_of_env env) (fun e -> e == die));
            focus_slot s;
            Slot.destroy s
        | _ -> ())
      hp.slots_update.slot_updates
  in
  Player.add_effect (other_of_env env) (On_end_turn, die)

let ra : card_effect =
 fun env ->
  Option.iter
    (fun pos ->
      Option.iter
        (fun c -> Slots.add_creature (player_of_env env).slots_update.slot_updates.(pos) (make_creature c))
        !sunstone_ref)
    (nearest_empty env.env_selected (player_of_env env))

let simoom : card_effect =
 fun env ->
  let p = player_of_env env in
  let nturn = (get_data p).hp_turn in
  let bonus =
    Attack_state_func
      (fun atk owner ->
        let curr = (get_data (other_player owner)).hp_turn in
        2 + min ((curr - nturn) * 3) (atk - 2))
  in
  let max_attack = ref 0 in
  Slots.foreach (other_of_env env).slots_update (fun slot ->
      let att = (Slot.get slot).attack in
      if att > 2 then (
        if att > !max_attack then max_attack := att;
        add_attack slot bonus));
  let turn_count = int_of_float (ceil (float_of_int !max_attack /. 3.)) in
  let rec tick env =
    let p = player_of_env env in
    if (get_data p).hp_turn > nturn + turn_count then (
      Slots.foreach (other_player p).slots_update (fun s -> remove_any_attack s bonus);
      Player.remove_effect p (fun e -> e == tick))
  in
  Player.add_effect p (On_end_turn, tick)

let incr_ra : card_effect =
 fun env ->
  match !ra_ref with
  | None -> ()
  | Some c ->
      Option.iter
        (fun s -> add_attack s (Attack_func (fun a -> a + 1)))
        (Slots.find_card (player_of_env env).slots_update c)

let apep : card_effect = fun env -> Houses.incr_mana (player_of_env env).houses_update (-1) [ 0; 1; 2; 3 ]
let apep_opp : card_effect = fun env -> Houses.incr_mana (other_of_env env).houses_update (-1) [ 0; 1; 2; 3 ]

let sphinx_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        Option.iter
          (fun c -> Slots.add_creature dead.d_player.slots_update.slot_updates.(dead.d_num) (make_creature c))
          !puzzle_ref);
  }

let puzzle_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun dead ->
        match dead.d_damage with
        | Some d when (not (is_effect d)) && d.context.ctx_player <> dead.d_player.pid ->
            Option.iter
              (fun c ->
                let slot = dead.d_player.slots_update.slot_updates.(dead.d_num) in
                Slots.add_creature slot (make_creature c);
                Slot.update slot (fun st -> { st with life = st.life / 2 }))
              !sphinx_ref
        | _ ->
            let op = other_player dead.d_player in
            let idx, _, _ =
              Array.fold_left
                (fun (best, m, i) h -> if h.mana > m then (i, h.mana, i + 1) else (best, m, i + 1))
                (0, 0, 0) op.houses_update.hu_value
            in
            Houses.incr_mana op.houses_update (-3) [ idx ]);
  }

let anubite_attack target d player =
  if single_target_attack target d player then
    Option.iter
      (fun pos ->
        Updater.focus player.updater d.context.ctx_selected player.pid true;
        Option.iter
          (fun c -> Slots.add_creature player.slots_update.slot_updates.(pos) (make_creature c))
          !mummy_ref)
      (nearest_empty d.context.ctx_selected player)

let babi_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_add =
      (fun slot ->
        let me = Reaction.bound_slot r in
        if slot.num = me.num then
          let mana =
            Array.fold_left (fun a h -> a + h.mana) 0 (other_player me.slots.player).houses_update.hu_value
          in
          Slot.set_data me (Babi_mana mana));
  }

let react_babi_mana (houses : house_state array) (s : slot_update) =
  match s.value with
  | Some { data = Babi_mana old; _ } ->
      let mana = Array.fold_left (fun a h -> a + h.mana) 0 houses in
      Slot.set_data s (Babi_mana mana);
      if mana > old && Option.is_some (Slot.opposite s).value then (
        let ctx = { ctx_player = s.slots.player.pid; ctx_card = !babi_ref; ctx_selected = s.num } in
        Slot.inflict (Slot.opposite s) (mk_damage ~is_ability:true (mana - old) ctx);
        focus_slot s)
  | _ -> ()

let amit_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_death =
      (fun dead ->
        let me = Reaction.bound_slot r in
        let power = dead.d_player.houses_update.hu_value.(dead.d_slot.card.c_card.house_index).mana in
        let ctx = { ctx_player = me.slots.player.pid; ctx_card = !amit_ref; ctx_selected = me.num } in
        Player.inflict dead.d_player (mk_damage ~is_ability:true power ctx);
        Houses.incr_mana me.slots.player.houses_update 1 [ 4 ];
        focus_slot me);
  }

let scarab_reaction () =
  let r = Reaction.default () in
  {
    r with
    self_protect =
      (fun d ->
        if d.is_spell then d
        else
          let n = List.length (Slot.filled_adjacent (Reaction.bound_slot r)) in
          if n <> 0 then { d with amount = max 0 (d.amount - (2 * n)) } else d);
  }

let sun_priest_attack target d player =
  ignore (single_target_attack target d player);
  let min_mana = Array.fold_left (fun m h -> min m h.mana) max_int player.houses_update.hu_value in
  Slots.inflict_creatures (other_player player).slots_update { d with amount = min_mana }

let crocodile_attack target d player =
  ignore (single_target_attack target d player);
  let num = d.context.ctx_selected in
  Slot.toggle player.slots_update.slot_updates.(num) paused_flag;
  Player.add_effect player
    ( On_turn,
      countdown 2 (fun env -> Slot.toggle_off (player_of_env env).slots_update.slot_updates.(num) paused_flag)
    )

let bennu_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_remove =
      (fun dead ->
        let me = Reaction.bound_slot r in
        (* Only a real death (Some dead). Warp Schizo / Queen silence is
           [remove None] and must not fire the dying attack. *)
        match (me.value, dead) with
        | Some st, Some d -> (
            match st.data with
            | Bennu_dead -> ()
            | _ ->
                let from_owner =
                  match d.d_damage with
                  | Some dm -> dm.context.ctx_player = me.slots.player.pid
                  | None -> false
                in
                if not from_owner then (
                  add_attack me (Attack_func (fun a -> int_of_float (ceil (float_of_int a *. 3.))));
                  Slot.set_data me Bennu_dead;
                  Option.iter (fun s -> Player.run_slot me.slots.player me.num s) me.value))
        | _ -> ());
  }

let serpo_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_summon =
      (fun e ->
        let me = Reaction.bound_slot r in
        if
          e.s_num <> me.num
          && e.s_card.c_card.house_index = special_house_index
          && e.s_player.pid = me.slots.player.pid
        then
          Option.iter
            (fun n ->
              Slots.move me.slots me.num n;
              let dest = me.slots.slot_updates.(n) in
              if Option.is_some dest.value && not (has_source dest (fun x -> x == serpo_bonus)) then (
                add_attack dest serpo_bonus;
                Player.add_effect me.slots.player
                  ( On_end_turn,
                    fun env ->
                      let s = (player_of_env env).slots_update.slot_updates.(n) in
                      if Option.is_some s.value then remove_first_attack s serpo_bonus )))
            (nearest_slot_opposed ~opposed:false me.num me.slots.player));
  }

let hp_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        init_any_death p;
        (if p.pstate.ps_desc.init_desc.pd_houses |> Array.exists (fun h -> h.phd_house.h_key = "highpriest")
         then
           let old_slots = p.slots_update.after_change in
           p.slots_update.after_change <-
             (fun () ->
               old_slots ();
               choose_path p));
        let op = other_player p in
        let old_houses = op.houses_update.after_update in
        op.houses_update.after_update <-
          (fun houses ->
            old_houses houses;
            Slots.foreach p.slots_update (fun s ->
                match s.value with
                | Some st when is_card !babi_ref st.card.c_card -> react_babi_mana houses s
                | _ -> ()));
        let old_sub = op.submit_command_hook in
        op.submit_command_hook <-
          (fun c ->
            let r = old_sub c in
            let ident = (c.cmd_card.house_index, c.cmd_card.card_index) in
            let d = get_data p in
            if not (List.mem ident d.hp_revealed) then
              set_data p { d with hp_revealed = ident :: d.hp_revealed };
            r));
  }

let guardian_mummy = creature ~key:"highpriest.guardianmummy" ~attack:(attack 4) ~life:20 ~description:"" ()

let serpent =
  creature ~key:"highpriest.serpent" ~attack:(attack 2) ~life:8 ~description:"highpriest.serpent.description"
    ()

let sun_stone =
  creature ~key:"highpriest.sunstone" ~attack:(attack 0) ~life:22
    ~description:"highpriest.sunstone.description"
    ~mod_:(Spell_mod (fun x -> x + 2))
    ~effects:(effects [ (On_turn, incr_ra) ])
    ()

let puzzle =
  creature ~key:"highpriest.puzzle" ~attack:(attack 0) ~life:6 ~description:"highpriest.puzzle.description"
    ~new_reaction:puzzle_reaction ()

let apis =
  creature ~key:"highpriest.Apis" ~attack:(attack 4) ~life:25 ~description:"highpriest.Apis.description"
    ~effects:(effects [ (On_turn, apis_effect) ])
    ()

let sphinx =
  creature ~key:"highpriest.sphinx" ~attack:(attack 7) ~life:24 ~description:"highpriest.sphinx.description"
    ~new_reaction:sphinx_reaction ()

let ouroboros =
  creature ~key:"highpriest.Ouroboros" ~attack:(attack 6) ~life:38
    ~description:"highpriest.Ouroboros.description"
    ~effects:(effects [ (On_turn, ouro) ])
    ()

let dragon_of_ra =
  creature ~key:"highpriest.Wingeddragon" ~attack:(attack 6) ~life:45
    ~description:"highpriest.Wingeddragon.description"
    ~effects:(effects [ (Direct, ra) ])
    ()

let babi =
  creature ~key:"highpriest.Babi" ~attack:(attack 5) ~life:21 ~description:"highpriest.Babi.description"
    ~new_reaction:babi_reaction ()

let amit =
  creature ~key:"highpriest.Ammit" ~attack:(attack 9) ~life:39 ~description:"highpriest.Ammit.description"
    ~new_reaction:amit_reaction ()

let hp_set =
  [
    creature ~key:"highpriest.Ancientcrocodile" ~attack:(attack 8) ~life:15
      ~description:"highpriest.Ancientcrocodile.description" ~run_attack:(custom_run_attack crocodile_attack)
      ();
    creature ~key:"highpriest.Serpopard" ~attack:(attack 4) ~life:18
      ~description:"highpriest.Serpopard.description" ~new_reaction:serpo_reaction ();
    creature ~key:"highpriest.Anubite" ~attack:(attack 5) ~life:20
      ~description:"highpriest.Anubite.description" ~run_attack:(custom_run_attack anubite_attack) ();
    babi;
    spell ~key:"highpriest.Curseofchaos" ~description:"highpriest.Curseofchaos.description"
      ~input_spec:Select_target_creature
      ~effects:(effects [ (Direct, curse) ])
      ();
    spell ~key:"highpriest.Simoom" ~description:"highpriest.Simoom.description"
      ~effects:(effects [ (Direct, simoom) ])
      ();
    amit;
    creature ~key:"highpriest.Apep" ~attack:(attack 5) ~life:42 ~description:"highpriest.Apep.description"
      ~effects:(effects [ (On_turn, apep); (On_end_turn, apep_opp) ])
      ~run_attack:multi_run_attack ();
  ]

let () =
  List.iteri (fun i (c : card) -> c.cost <- i + 1) hp_set;
  hp_set_ref := hp_set;
  apis_ref := Some apis;
  sphinx_ref := Some sphinx;
  puzzle_ref := Some puzzle;
  ouro_ref := Some ouroboros;
  serpent_ref := Some serpent;
  sunstone_ref := Some sun_stone;
  mummy_ref := Some guardian_mummy;
  ra_ref := Some dragon_of_ra;
  babi_ref := Some babi;
  amit_ref := Some amit

let cards =
  [
    creature ~key:"highpriest.Sacredscarab" ~attack:(attack 3) ~life:11
      ~description:"highpriest.Sacredscarab.description" ~new_reaction:scarab_reaction ();
    creature ~key:"highpriest.sunpriest" ~attack:(attack 2) ~life:16
      ~description:"highpriest.sunpriest.description"
      ~run_attack:(custom_run_attack ~multi:true sun_priest_attack)
      ();
    apis;
    creature ~key:"highpriest.Bennu" ~attack:(attack 5) ~life:21 ~description:"highpriest.Bennu.description"
      ~new_reaction:bennu_reaction ();
    spell ~key:"highpriest.EyeofWajet" ~description:"highpriest.EyeofWajet.description"
      ~effects:(effects [ (Direct, wajet) ])
      ();
    sphinx;
    ouroboros;
    dragon_of_ra;
  ]

let house =
  special_house ~key:"highpriest" ~id:41
    ~effects:[ (On_turn, hp_turn); (On_start, init_path) ]
    ~additional_cards:(hp_set @ [ guardian_mummy; serpent; sun_stone; puzzle ])
    ~listener:hp_listener ~data:(Priest_data empty_data) cards
