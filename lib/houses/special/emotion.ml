(* Special house Emotion — original design. The eight cards follow the eight
   emotions: cowardice, revolt, loneliness, courage, love, hope, teamwork and
   legend. *)

open Sp
open Engine
open Common

let coward_ref : card option ref = ref None
let hope_ref : card option ref = ref None

let filled su = Slots.foldl su 0 (fun n _ -> n + 1)

(* Coward dies the moment it is the owner's only creature.  [on_dead] covers
   every way another creature can leave the board; the Direct effect covers
   entering an empty board in the first place. *)
let check_coward (p : player_update) =
  match !coward_ref with
  | None -> ()
  | Some coward ->
      let filled_slots =
        Slots.foldl p.slots_update [] (fun acc s -> s :: acc)
      in
      (match filled_slots with
      | [ s ] when is_card (Some coward) (Slot.get s).card.c_card ->
          emit p.updater
            (Ev_ability { kind = "emotion.coward"; player = p.pid; num = s.num; target = None });
          Slot.destroy s
      | _ -> ())

let coward_direct : card_effect = fun env -> check_coward (player_of_env env)

let coward_listener () =
  {
    default_listener with
    hl_init =
      (fun p ->
        let old = p.slots_update.on_dead_hook in
        p.slots_update.on_dead_hook <-
          (fun dead ->
            old dead;
            if dead.d_player.pid = p.pid then check_coward p));
  }

let revolt : card_effect =
 fun env ->
  let op = other_of_env env in
  let amount = 2 + (2 * filled op.slots_update) in
  Player.inflict op (spell_damage env amount)

let loneliness : card_effect =
 fun env ->
  let damage = spell_damage env 16 in
  let hit_lonely (p : player_update) =
    Slots.foreach p.slots_update (fun s ->
        if Option.is_none (Slot.opposite s).value then Slot.inflict s damage)
  in
  hit_lonely (player_of_env env);
  hit_lonely (other_of_env env)

let courage_attack =
  Attack_state_func (fun atk p -> if p.pstate.ps_life <= 12 then atk * 3 else atk)

let love_heal : card_effect =
 fun env ->
  let p = player_of_env env in
  let me = own_slot_of_env env in
  let st = Slot.get me in
  if st.life < st.max_life then
    emit env.env_updater
      (Ev_ability { kind = "emotion.love"; player = env.env_player; num = me.num; target = None });
  Slot.heal me (filled p.slots_update)

let hope_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        let p = me.slots.player in
        emit p.updater (Ev_ability { kind = "emotion.hope"; player = p.pid; num = me.num; target = None });
        Slots.heal_creatures p.slots_update 7;
        Slots.inflict_creatures (other_player p).slots_update
          (mk_damage ~is_ability:true 7
             { ctx_player = p.pid; ctx_card = !hope_ref; ctx_selected = me.num }));
  }

let teamwork_attack =
  Attack_slot_state_func (fun atk slot ->
      if List.length (Slot.filled_adjacent slot) = 2 then atk * 2 else atk)

let legend_reaction () =
  let r = Reaction.default () in
  {
    r with
    on_my_death =
      (fun _ ->
        let me = Reaction.bound_slot r in
        Slots.foreach me.slots.player.slots_update (fun s ->
            Slot.update s (fun st -> { st with life = (st.life + 1) / 2 })));
  }

let coward =
  creature ~key:"emotion.coward" ~attack:(attack 4) ~life:24 ~description:"emotion.coward.description"
    ~effects:(effects [ (Direct, coward_direct) ])
    ()

let () = coward_ref := Some coward

let hope =
  creature ~key:"emotion.hope" ~attack:(attack 7) ~life:34 ~description:"emotion.hope.description"
    ~new_reaction:hope_reaction ()

let () = hope_ref := Some hope

let cards =
  [
    coward;
    spell ~key:"emotion.revolt" ~description:"emotion.revolt.description"
      ~effects:(effects [ (Direct, revolt) ])
      ();
    spell ~key:"emotion.loneliness" ~description:"emotion.loneliness.description"
      ~effects:(effects [ (Direct, loneliness) ])
      ();
    creature ~key:"emotion.courage" ~attack:{ base = Some 5; sources = [ courage_attack ] } ~life:32
      ~description:"emotion.courage.description" ();
    creature ~key:"emotion.love" ~attack:(attack 7) ~life:25 ~description:"emotion.love.description"
      ~effects:(effects [ (On_turn, love_heal) ])
      ();
    hope;
    creature ~key:"emotion.teamwork"
      ~attack:{ base = Some 6; sources = [ teamwork_attack ] }
      ~life:42 ~description:"emotion.teamwork.description" ();
    creature ~key:"emotion.legend" ~attack:(attack 10) ~life:63 ~description:"emotion.legend.description"
      ~new_reaction:legend_reaction ();
  ]

let house = special_house ~key:"emotion" ~id:43 ~listener:coward_listener cards
