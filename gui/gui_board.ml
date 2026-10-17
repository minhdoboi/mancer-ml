(* Raylib UI: the game screen: game state, board, hands and the card preview. *)

open Mancer
open Sp
module R = Raylib
open Gui_draw
open Gui_lobby
open Gui_campaign

(* --------------------------------------------------------------- game ui *)

type thinking = { domain : unit Domain.t; result : (command option, exn) result option Atomic.t }
type mode = Browse | Target of command list
type last_played = { player : player_id; card : card option }

type hover =
  | Hover_card of card * player_id * int option (* the creature's current max life, if it is on the board *)
  | Hover_house of house
  | Hover_note of { title : string; body : string }

type house_dlg = {
  house : house;
  player : player_id;
  mutable scroll : float;
  mutable drag : bool;
  mutable content_h : float;
}

type called_dlg = { cd : card_desc; hd : player_house_desc; current : int option }

type slide = {
  card : card;
  cost : int option;
  attack : string option;
  life : int option;
  max_life : int option;
  sx : float;
  sy : float;
  tx : float;
  ty : float;
  start : float;
  duration : float;
  hides : (player_id * int) list;
  sound : string option;
  mutable sounded : bool;
}

(* A creature's attack (Scala card.run): straight toward the opposite row
   and back, linear both ways.  [a_dir] is the vertical direction: normal
   attacks cross the gap, Revolt turns the foe's creatures on their owner. *)
type attack = { a_pid : player_id; a_num : int; a_start : float; a_dir : float }

(* Scala Focus: a 0.5 s scale pulse on the card whose effect fires *)
type focus = { f_pid : player_id; f_num : int; f_start : float }

(* Scala SlotComponent.fade: the dying card fades out instead of popping off. *)
type death = { d_pid : player_id; d_num : int; d_id : int; d_start : float }

(* Scala DamagableInt: a value that changed between two refreshes *)
type pop_at = Slot_life of player_id * int | Player_life of player_id | Mana of player_id * int
type pop = { at : pop_at; amount : int; p_start : float; mutable p_sounded : bool }

(* Scala SpellPlayed: the circular spell art sits for 1 s after the cast. *)
type spell_show = { ss_card : card; ss_player : player_id; ss_start : float }

type game_ui = {
  table : Table.t;
  portraits : string * string;
  last_played : last_played option ref;
  discovered : (player_id * string, unit) Hashtbl.t;
  mutable mode : mode;
  mutable paused : bool;
  mutable settings : game_settings option;
  mutable budget_ms : int;
  mutable message : string;
  mutable thinking : thinking option;
  mutable last_move : float;
  mutable hover : hover option;
  mutable hover_opp : bool;
  mutable house_dlg : house_dlg option;
  mutable called_dlg : called_dlg option;
  mutable scroll_y : float;
  mutable bg_at : float;
  mutable slides : slide list;
  mutable late_sounds : (float * string) list;
  mutable anim_at : float;
  (* Arcane Trap per victim: 0 none, 1 cast but skip not on the board yet, 2 skip showing *)
  trap : int array;
  mutable freeze : int;  (* Deep Freeze: 0 none, 1 cast, 2 the frozen turns are running *)
  (* The engine resolves a move at once; the board is replayed from the
     snapshot taken at each refresh, on the same timeline as the slides. *)
  mutable frames : (float * game_state) list; (* oldest first, not shown yet *)
  mutable shown : game_state;
  mutable last_frame : game_state;
  mutable attacks : attack list;
  mutable focuses : focus list;
  mutable deaths : death list;
  mutable pops : pop list;
  mutable spell_shows : spell_show list;
  mutable campaign : Campaign.node option;
  mutable campaign_recorded : bool;
  mutable ended_at : float option;
  (* Wall time when YOU WON / YOU LOSE may appear: after attacks and life pops. *)
  mutable over_at : float option;
  (* Some won: end parchment is up on the board, like Scala CampaignEndDialog. *)
  mutable campaign_end : bool option;
}

(* Filled once the board layout helpers exist, so submit can queue slides. *)
let board_event : (game_ui -> game_event -> unit) ref = ref (fun _ _ -> ())

let make_game ?(campaign = None) (setup : Table.setup) portraits table_of =
  (* Rematch / new fight: the victory banner and portrait halo never expire,
     so a fresh board must drop the previous game's overlay effects. *)
  Fx.clear ();
  let last_played = ref None in
  let discovered = Hashtbl.create 16 in
  let ui_holder = ref None in
  let on_event e =
    (match e with
    | Ev_submitted { player; command; _ } ->
        last_played := Some { player; card = Option.map (fun c -> c.cmd_card) command };
        Option.iter (fun c -> Hashtbl.replace discovered (player, c.cmd_card.key) ()) command
    | Ev_spell_played c ->
        (* a spell with a delayed impact plays its sound from [queue_slides], once its effect starts *)
        if Fx.sound_delay c.cmd_card.key = 0. then play_card_sound c.cmd_card;
        (* Pain of essence: a distant, not too loud scream *)
        if c.cmd_card.key = "conjurer.pain" then
          Option.iter
            (fun s ->
              R.set_sound_volume s (0.3 *. sound_level ());
              R.play_sound s)
            (Scream.sound ())
        else if c.cmd_card.key = "battlemage.warcry" then
          Option.iter
            (fun s ->
              R.set_sound_volume s (0.85 *. sound_level ());
              R.play_sound s)
            (Warcry.sound ())
    | _ -> ());
    Option.iter (fun ui -> !board_event ui e) !ui_holder
  in
  let table = table_of on_event in
  let ui =
    {
      table;
      portraits;
      last_played;
      discovered;
      mode = Browse;
      paused = false;
      settings = None;
      budget_ms = setup.budget_ms;
      message = "";
      thinking = None;
      last_move = R.get_time ();
      hover = None;
      hover_opp = false;
      house_dlg = None;
      called_dlg = None;
      scroll_y = 0.;
      bg_at = R.get_time ();
      slides = [];
      late_sounds = [];
      anim_at = 0.;
      trap = [| 0; 0 |];
      freeze = 0;
      frames = [];
      shown = Session.peek (Table.session table);
      last_frame = Session.peek (Table.session table);
      attacks = [];
      focuses = [];
      deaths = [];
      pops = [];
      spell_shows = [];
      campaign;
      campaign_recorded = false;
      ended_at = None;
      over_at = None;
      campaign_end = None;
    }
  in
  ui_holder := Some ui;
  ui

let new_game ?(campaign = None) setup portraits =
  make_game ~campaign setup portraits (fun on_event -> Table.start ~on_event setup)

let new_swapped_game ?(campaign = None) src setup portraits =
  make_game ~campaign setup portraits (fun on_event ->
      Table.start_swapped ~on_event ~keep_starter:(Option.is_some campaign) ~budget_ms:setup.budget_ms src)

let session ui = Table.session ui.table
let state ui = Session.state (session ui)
let desc ui = Session.desc (session ui)

(* The board as the animation has reached it *)
let view ui = ui.shown

(* The live session already has next-turn mana when Ev_to_move fires; the
   header still shows [view] until that silent frame is applied. Do not light
   cards from the live table during the swing — wait until input is open. *)
let shown_available ui pid cd hd =
  let houses = (view ui).players.(pid).ps_houses in
  let i = hd.phd_house.h_index in
  i >= 0 && i < Array.length houses && is_available cd houses.(i)

(* Scala application.conf card.run is 1 s; its 5 px peak is hard to see at
   this scale, so the lunge goes further *)
let attack_duration = 1.
let attack_reach = 18.
let focus_duration = 0.5

(* Scala AlphaAction is 0.1 s; a touch longer so the fade reads as a fade. *)
let death_duration = 0.2
let pop_duration = 1.
let pop_rise = 20.

let slides_busy ui =
  let now = R.get_time () in
  List.exists (fun s -> now < s.start +. s.duration) ui.slides
  || List.exists (fun a -> now < a.a_start +. attack_duration) ui.attacks
  || List.exists (fun f -> now < f.f_start +. focus_duration) ui.focuses
  || List.exists (fun d -> now < d.d_start +. death_duration) ui.deaths
  || ui.frames <> []

let input_ok ui = not (slides_busy ui)
let card_playable ui pid cd hd = input_ok ui && Table.playable ui.table cd hd && shown_available ui pid cd hd
let human_to_move ui = Table.human_to_move ui.table
let both_bots ui = Table.seat ui.table owner <> Human && Table.seat ui.table opponent <> Human

(* Scala GameSession.myPlayerId: the human side hidden stalkers stay invisible to.
   The owner still sees (tinted) its own stalker; the opponent's stays an empty well. *)
let viewer ui =
  if Table.seat ui.table owner = Human then owner
  else if Table.seat ui.table opponent = Human then opponent
  else owner

let concealed ui pid (st : slot_state) = Shadowmage.is_hidden_from st ~owner:pid ~viewer:(viewer ui)

(* Blocks until a running search ends: nothing may submit while it reads the game *)
let settle ui =
  Option.iter (fun th -> Domain.join th.domain) ui.thinking;
  ui.thinking <- None

let guarded ui f =
  try
    ui.message <- "";
    f ()
  with e ->
    ui.paused <- true;
    ui.mode <- Browse;
    ui.called_dlg <- None;
    ui.message <- "error: " ^ Printexc.to_string e

let play ui cmd =
  if input_ok ui then
    guarded ui (fun () ->
        ui.mode <- Browse;
        ui.called_dlg <- None;
        Table.submit ui.table cmd;
        ui.last_move <- R.get_time ())

(* Scala CalledElementDialog: element picker shown when a Meteomancer card
   is clicked. Fire 0 .. Earth 3, index-aligned with the basic house index. *)
let element_keys = [| "fire"; "water"; "air"; "earth" |]

let current_called ui pid =
  match (state ui).players.(pid).ps_data with Meteo_data d -> d.mt_called | _ -> None

let called_title ui pid =
  let cur =
    match current_called ui pid with
    | Some i when i >= 0 && i < Array.length element_keys -> Assets.text element_keys.(i)
    | _ -> Assets.text "meteomancer.noelement"
  in
  Assets.format (Assets.text "meteomancer.choose") [ cur ]

let close_called_dlg ui = ui.called_dlg <- None

let commit_called ui idx =
  match ui.called_dlg with
  | None -> ()
  | Some dlg -> (
      ui.called_dlg <- None;
      if not (input_ok ui) then ()
      else
        let cmds =
          List.filter
            (fun c -> c.cmd_flag = Some (Called_element idx))
            (Table.choices ui.table dlg.cd.cd_card)
        in
        match cmds with
        | [] -> ui.message <- "no legal target"
        | [ c ] -> play ui (Some c)
        | filtered ->
            ui.message <- "";
            ui.mode <- Target filtered)

let pick_card ui cd hd =
  if not (input_ok ui) then ()
  else if Meteomancer.is_meteo cd.cd_card then (
    match Table.pick_card ui.table cd hd with
    | Not_enough_mana -> ui.message <- "not enough mana"
    | No_target -> ui.message <- "no legal target"
    | Play _ | Choose _ ->
        let pid = Option.value (human_to_move ui) ~default:owner in
        ui.message <- "";
        ui.mode <- Browse;
        ui.called_dlg <- Some { cd; hd; current = current_called ui pid })
  else
    match Table.pick_card ui.table cd hd with
    | Not_enough_mana -> ui.message <- "not enough mana"
    | No_target -> ui.message <- "no legal target"
    | Play c -> play ui (Some c)
    | Choose cmds ->
        ui.message <- "";
        ui.mode <- Target cmds

(* Starts or collects the bot's search *)
let drive_bot ui =
  match ui.thinking with
  | Some th -> (
      match Atomic.get th.result with
      | None -> ()
      | Some r -> (
          Domain.join th.domain;
          ui.thinking <- None;
          match r with
          | Ok cmd -> if Option.is_some (Table.to_move ui.table) then play ui cmd
          | Error e ->
              ui.paused <- true;
              ui.message <- "bot error: " ^ Printexc.to_string e))
  | None -> (
      match Table.to_move ui.table with
      | Some pid when Table.seat ui.table pid <> Human && (not ui.paused) && input_ok ui ->
          let delay = if both_bots ui then 0.6 else 0.35 in
          if R.get_time () -. ui.last_move >= delay then
            let result = Atomic.make None in
            let table = ui.table in
            let domain =
              Domain.spawn (fun () ->
                  Atomic.set result (Some (try Ok (Table.bot_command table) with e -> Error e)))
            in
            ui.thinking <- Some { domain; result }
      | Some pid
        when Table.seat ui.table pid = Human
             && (not ui.paused) && input_ok ui
             && turn_skipped (state ui).players.(pid)
             && R.get_time () -. ui.last_move >= 0.8 ->
          (* Deep freeze: nothing can be played, so the turn passes by itself *)
          play ui None
      | _ -> ())

(* ---------------------------------------------------------------- board *)

(* Scala raka is 90×102. Empty slots share that hit rect; the gold well is
   drawn at its native 81×95 so it sits inside the card, not around it. *)
let card_w = 90.
let card_h = 102.
let portrait_size = 84.
let card_gap = 0.
let house_col = 92.
let slot_w = card_w
let slot_h = card_h
let slot_gap = 10.
let pane_margin = 6.
let row_y pid = if pid = opponent then 66. else 224.
let hand_header_y = 334.
let hand_y = 356.
let n_hand_rows = 4
let house_header_h = 22.
let cards_block_h = (float n_hand_rows *. (card_h +. card_gap)) -. card_gap
let opp_header_y = 42.
let opp_hand_y0 = opp_header_y -. 4. -. cards_block_h
let max_scroll = -.opp_hand_y0 +. 40.
let last_played_h = 48.
let spell_show_size = 86.
let spell_show_duration = 1.
let oy ui y = y +. ui.scroll_y

type layout = {
  pane_x : float;
  pane_w : float;
  slot_x0 : float;
  hand_x0 : float;
  preview_x : float;
  preview_w : float;
}

let lay =
  ref { pane_x = 800.; pane_w = 620.; slot_x0 = 800.; hand_x0 = 800.; preview_x = 420.; preview_w = 360. }

let make_layout table =
  let n = max (List.length (Table.hand table owner)) (List.length (Table.hand table opponent)) in
  let hand_w = float n *. house_col in
  let slots_w = (float nb_slots *. slot_w) +. (float (nb_slots - 1) *. slot_gap) in
  let pane_w = Float.max hand_w slots_w in
  let pane_x = vw -. pane_margin -. pane_w in
  let slot_x0 = pane_x +. pane_w -. slots_w in
  let hand_x0 = pane_x +. pane_w -. hand_w in
  let preview_x = 8. in
  let preview_w = Float.min 340. (Float.max 120. (hand_x0 -. 16.)) in
  { pane_x; pane_w; slot_x0; hand_x0; preview_x; preview_w }

let slot_x n = !lay.slot_x0 +. (float n *. (slot_w +. slot_gap))
let slot_rect ui pid num = rect (slot_x num) (oy ui (row_y pid)) slot_w slot_h

let hand_card_rect ui pid col k =
  let x = !lay.hand_x0 +. (float col *. house_col) in
  let cx = x +. ((house_col -. card_w) /. 2.) in
  let cards_y = if pid = opponent then opp_hand_y0 else hand_y in
  rect cx (oy ui (cards_y +. (float k *. (card_h +. card_gap)))) card_w card_h

let find_hand_rect ui pid (card : card) =
  let rec cols i = function
    | [] -> None
    | hd :: rest ->
        let rec rows k =
          if k >= Array.length hd.phd_cards || k >= n_hand_rows then cols (i + 1) rest
          else if hd.phd_cards.(k).cd_card.key = card.key then Some (hand_card_rect ui pid i k)
          else rows (k + 1)
        in
        rows 0
  in
  cols 0 (Table.hand ui.table pid)

let slot_hidden ui pid num =
  let now = R.get_time () in
  List.exists
    (fun s ->
      now >= s.start && now < s.start +. s.duration && List.exists (fun (p, n) -> p = pid && n = num) s.hides)
    ui.slides

let schedule ui duration =
  let now = R.get_time () in
  if ui.anim_at < now then ui.anim_at <- now;
  let start = ui.anim_at in
  ui.anim_at <- start +. duration;
  start

let push_slide ui ~start ~duration ~hides ~(from_r : rect) ~(to_r : rect) ~sound card cost attack life
    max_life =
  ui.slides <-
    {
      card;
      cost;
      attack;
      life;
      max_life;
      sx = from_r.x;
      sy = from_r.y;
      tx = to_r.x;
      ty = to_r.y;
      start;
      duration;
      hides;
      sound;
      sounded = false;
    }
    :: ui.slides

let face_of_slot ui pid num =
  match (state ui).players.(pid).ps_slots.(num) with
  | None -> None
  | Some st ->
      Some
        ( st.card.c_card,
          Some st.card.c_card.cost,
          Some (string_of_int st.attack),
          Some st.life,
          Some st.max_life )

(* Life always pops; mana only on loud refreshes, so paying for a card is
   quiet (Scala CardPanel.refresh(silent)). *)
let diff_pops ~silent ~start (a : game_state) (b : game_state) =
  let pops = ref [] in
  let add at amount =
    if amount <> 0 then pops := { at; amount; p_start = start; p_sounded = false } :: !pops
  in
  Array.iteri
    (fun pid (pb : player_state) ->
      let pa = a.players.(pid) in
      add (Player_life pid) (pb.ps_life - pa.ps_life);
      if not silent then
        Array.iteri
          (fun i (h : house_state) -> add (Mana (pid, i)) (h.mana - pa.ps_houses.(i).mana))
          pb.ps_houses;
      Array.iteri
        (fun n sb ->
          match (pa.ps_slots.(n), sb) with
          | Some x, Some y when x.slot_id = y.slot_id -> add (Slot_life (pid, n)) (y.life - x.life)
          | _ -> ())
        pb.ps_slots)
    b.players;
  !pops

let push_frame ui ~silent =
  let start = Float.max (R.get_time ()) ui.anim_at in
  let st = Session.peek (session ui) in
  ui.pops <- diff_pops ~silent ~start ui.last_frame st @ ui.pops;
  ui.frames <- ui.frames @ [ (start, st) ];
  ui.last_frame <- st

let fx_layout ui : Fx.layout =
  let occupied pid =
    let p = (view ui).players.(pid) in
    List.filter (fun n -> Option.is_some p.ps_slots.(n)) p.ps_slot_list
  in
  let scarecrow pid =
    let p = (view ui).players.(pid) in
    List.find_opt
      (fun n ->
        match p.ps_slots.(n) with Some st -> st.card.c_card.key = "disciples.scarecrow" | None -> false)
      p.ps_slot_list
  in
  let house pid i =
    let header_y = if pid = opponent then opp_header_y else hand_header_y in
    match List.find_index (fun hd -> hd.phd_house.h_index = i) (Table.hand ui.table pid) with
    | None -> { Fx.x = !lay.hand_x0; y = oy ui header_y }
    | Some col ->
        {
          Fx.x = !lay.hand_x0 +. (float col *. house_col) +. (house_col /. 2.);
          y = oy ui header_y +. (house_header_h /. 2.);
        }
  in
  {
    slot_center =
      (fun pid n ->
        let r = slot_rect ui pid n in
        { x = r.x +. (r.w /. 2.); y = r.y +. (r.h /. 2.) });
    slot_floor =
      (fun pid n ->
        let well = slot_well_rect (slot_rect ui pid n) in
        { x = well.x +. (well.w /. 2.); y = well.y +. well.h });
    flame_base =
      (fun pid n ->
        let r = slot_rect ui pid n in
        { x = r.x +. (r.w /. 2.); y = r.y +. r.h -. 38. });
    life = (fun pid -> { x = 8. +. (portrait_size /. 2.); y = oy ui (row_y pid) +. (portrait_size /. 2.) });
    house;
    slot_house =
      (fun pid n ->
        match (view ui).players.(pid).ps_slots.(n) with
        | Some st -> Some st.card.c_card.house_index
        | None -> None);
    slot_life =
      (fun pid n -> Option.map (fun (st : slot_state) -> st.life) (view ui).players.(pid).ps_slots.(n));
    slot_card =
      (fun pid n ->
        Option.map (fun (st : slot_state) -> st.card.c_card.key) (view ui).players.(pid).ps_slots.(n));
    house_cards =
      (fun pid idx ->
        let hand = Table.hand ui.table pid in
        match List.find_index (fun hd -> hd.phd_house.h_index = idx) hand with
        | None -> []
        | Some col ->
            let hd = List.nth hand col in
            List.init
              (min n_hand_rows (Array.length hd.phd_cards))
              (fun k ->
                let r = hand_card_rect ui pid col k in
                { Fx.x = r.x +. (r.w /. 2.); y = r.y +. (r.h /. 2.) }));
    occupied;
    scarecrow;
    source_of =
      (fun c ->
        find_hand_rect ui c.cmd_player c.cmd_card
        |> Option.map (fun r -> { Fx.x = r.x +. (r.w /. 2.); y = r.y +. (r.h /. 2.) }));
    extreme_house =
      (fun pid kind ->
        let hs = (view ui).players.(pid).ps_houses in
        let better = match kind with `Highest -> ( > ) | `Lowest -> ( < ) in
        let best = ref 0 in
        Array.iteri (fun i (h : house_state) -> if better h.mana hs.(!best).mana then best := i) hs;
        !best);
  }

let run_fx ui recipe =
  let start =
    if recipe.Fx.blocking then schedule ui recipe.duration else Float.max (R.get_time ()) ui.anim_at
  in
  Fx.spawn (recipe.make ~start);
  start

let queue_slides ui e =
  lay := make_layout ui.table;
  match e with
  | Ev_summon { num; player; creature } ->
      (* Scala GameInit.summon: a stalker hidden from me animates nothing. *)
      if creature.c_body.b_data = Stealthed && player <> viewer ui then push_frame ui ~silent:true
      else
        let card = creature.c_card in
        let from_r =
          Option.value (find_hand_rect ui player card) ~default:(rect 0. (vh -. 600.) card_w card_h)
        in
        let to_r = slot_rect ui player num in
        let cost, attack, life, max_life =
          match face_of_slot ui player num with
          | Some (_, cost, attack, life, max_life) -> (cost, attack, life, max_life)
          | None -> (Some card.cost, printed_attack card, printed_life card, printed_life card)
        in
        let start = schedule ui 0.5 in
        push_slide ui ~start ~duration:0.5
          ~hides:[ (player, num) ]
          ~from_r ~to_r ~sound:(Some "move") card cost attack life max_life;
        (* Keep the landed creature on the board if a later blocking focus
           (Wolf Spider, Marine, …) delays the submit refresh. *)
        push_frame ui ~silent:true
  | Ev_move { num; dest; player; dest_player } ->
      Option.iter
        (fun (card, cost, attack, life, max_life) ->
          let start = schedule ui 1. in
          push_slide ui ~start ~duration:1.
            ~hides:[ (player, num); (dest_player, dest) ]
            ~from_r:(slot_rect ui player num) ~to_r:(slot_rect ui dest_player dest) ~sound:(Some "move") card
            cost attack life max_life;
          push_frame ui ~silent:true)
        (face_of_slot ui player num)
  | Ev_swap { num; dest; player; dest_player } -> (
      match (face_of_slot ui player num, face_of_slot ui dest_player dest) with
      | Some (a, ac, aa, al, am), Some (b, bc, ba, bl, bm) ->
          let start = schedule ui 1. in
          let src = slot_rect ui player num and dst = slot_rect ui dest_player dest in
          let hides = [ (player, num); (dest_player, dest) ] in
          push_slide ui ~start ~duration:1. ~hides ~from_r:src ~to_r:dst ~sound:(Some "move") a ac aa al am;
          push_slide ui ~start ~duration:1. ~hides ~from_r:dst ~to_r:src ~sound:None b bc ba bl bm;
          push_frame ui ~silent:true
      | _ -> ())
  | Ev_die { num; player } -> (
      match
        match ui.shown.players.(player).ps_slots.(num) with
        | Some st -> Some st
        | None -> ui.last_frame.players.(player).ps_slots.(num)
      with
      | None -> ()
      | Some st ->
          let now = R.get_time () in
          let d_start = Float.max now ui.anim_at in
          if ui.anim_at < d_start +. death_duration then ui.anim_at <- d_start +. death_duration;
          ui.deaths <- { d_pid = player; d_num = num; d_id = st.slot_id; d_start } :: ui.deaths)
  | Ev_run_slot { num; player } ->
      let a_start = schedule ui attack_duration in
      let a_dir = if player = owner then -1. else 1. in
      ui.attacks <- { a_pid = player; a_num = num; a_start; a_dir } :: ui.attacks
  | Ev_focus { num; player; blocking } ->
      (* a blocking focus holds the timeline, as Scala's waitAction does *)
      let f_start = if blocking then schedule ui focus_duration else Float.max (R.get_time ()) ui.anim_at in
      ui.focuses <- { f_pid = player; f_num = num; f_start } :: ui.focuses
  | Ev_refresh { silent } -> push_frame ui ~silent
  (* the turn's mana income lands here: silent, as in Scala; card effects
     already popped on their own loud refresh *)
  | Ev_to_move _ -> push_frame ui ~silent:true
  | Ev_spell_played c ->
      let start = Float.max (R.get_time ()) ui.anim_at in
      (* Revolt: the foe's creatures lunge away from the opposing row, into
         their own owner, so the card motion mirrors a normal attack. *)
      if c.cmd_card.key = "emotion.revolt" then (
        let foe = other c.cmd_player in
        let a_start = start +. 0.12 in
        let a_dir = if foe = owner then 1. else -1. in
        List.iter
          (fun num ->
            match (view ui).players.(foe).ps_slots.(num) with
            | Some _ ->
                ui.attacks <- { a_pid = foe; a_num = num; a_start; a_dir } :: ui.attacks
            | None -> ())
          base_slot_list);
      ui.spell_shows <- { ss_card = c.cmd_card; ss_player = c.cmd_player; ss_start = start } :: ui.spell_shows;
      if c.cmd_card.key = "rogue.trap" then ui.trap.(1 - c.cmd_player) <- 1;
      if c.cmd_card.key = "elementalist.freeze" then ui.freeze <- 1;
      let delay = Fx.sound_delay c.cmd_card.key in
      let fx_start = Option.map (run_fx ui) (Fx.for_spell (fx_layout ui) c) in
      if delay > 0. then
        let at = Option.value fx_start ~default:start +. delay in
        ui.late_sounds <- (at, card_sound_name c.cmd_card) :: ui.late_sounds
  | Ev_ability { kind; player; num; target } ->
      ignore (Option.map (run_fx ui) (Fx.for_ability (fx_layout ui) ~kind ~player ~num ~target))
  | Ev_game_over { winner; _ } ->
      push_frame ui ~silent:true;
      (* Engine already set Over; Scala waitAction holds endGame until the
         attack finishes. Hold the banner until that timeline and the life
         pops have played. *)
      let after_anims = schedule ui 0. in
      let after_pops =
        List.fold_left
          (fun acc p ->
            match p.at with
            | Player_life _ | Slot_life _ -> Float.max acc (p.p_start +. pop_duration)
            | Mana _ -> acc)
          after_anims ui.pops
      in
      if ui.anim_at < after_pops then ui.anim_at <- after_pops;
      ui.over_at <- Some after_pops;
      Fx.victory (fx_layout ui) ~winner ~you:(Table.seat ui.table owner = Human) ~start:after_pops
  | _ -> ()

let () = board_event := queue_slides

let tick_slides ui =
  let now = R.get_time () in
  List.iter
    (fun s ->
      if (not s.sounded) && now >= s.start then (
        s.sounded <- true;
        Option.iter (fun n -> play_sound n) s.sound))
    ui.slides;
  ui.late_sounds <-
    List.filter
      (fun (at, name) ->
        if now >= at then (
          play_sound ~volume:0.9 name;
          false)
        else true)
      ui.late_sounds;
  ui.slides <- List.filter (fun s -> now < s.start +. s.duration) ui.slides;
  let rec advance = function
    | (t, st) :: rest when t <= now ->
        ui.shown <- st;
        advance rest
    | rest -> rest
  in
  ui.frames <- advance ui.frames;
  ui.attacks <- List.filter (fun a -> now < a.a_start +. attack_duration) ui.attacks;
  ui.spell_shows <- List.filter (fun s -> now < s.ss_start +. spell_show_duration) ui.spell_shows;
  ui.focuses <- List.filter (fun f -> now < f.f_start +. focus_duration) ui.focuses;
  ui.deaths <-
    List.filter
      (fun d ->
        now < d.d_start +. death_duration
        ||
        match ui.shown.players.(d.d_pid).ps_slots.(d.d_num) with
        | Some st -> st.slot_id = d.d_id
        | None -> false)
      ui.deaths;
  List.iter
    (fun p ->
      if (not p.p_sounded) && now >= p.p_start then (
        p.p_sounded <- true;
        match p.at with
        | Mana _ -> ()
        | Player_life _ -> play_sound (if p.amount < 0 then "damage" else "heal")
        | Slot_life _ -> play_sound (if p.amount < 0 then "damage" else "creatureheal")))
    ui.pops;
  ui.pops <- List.filter (fun p -> now < p.p_start +. pop_duration) ui.pops

let draw_slides ui =
  let now = R.get_time () in
  List.iter
    (fun s ->
      if now >= s.start then
        let t = Float.min 1. ((now -. s.start) /. s.duration) in
        let x = s.sx +. ((s.tx -. s.sx) *. t) and y = s.sy +. ((s.ty -. s.sy) *. t) in
        let life_color = match (s.life, s.max_life) with Some l, Some m when l < m -> gold | _ -> life_ok in
        draw_card_face (desc ui) (rect x y card_w card_h) s.card ~cost:s.cost ~attack:s.attack ~life:s.life
          ~life_color ~tint:R.Color.white ~decor:(fun () -> ()))
    ui.slides

let peek_smoke_card ui =
  if human_to_move ui <> Some owner then None
  else
    let selected = match ui.mode with Target (c :: _) -> Some c.cmd_card.key | _ -> None in
    let found = ref None in
    List.iteri
      (fun col hd ->
        Array.iteri
          (fun k cd ->
            if k < n_hand_rows && Option.is_none !found then
              let r = hand_card_rect ui owner col k in
              if inside r && card_playable ui owner cd hd && selected <> Some cd.cd_card.key && input_ok ui
              then found := Some r)
          hd.phd_cards)
      (Table.hand ui.table owner);
    !found

let is_silenced (st : slot_state) = Common.is_bridled st

(* Scala SlotCardActors.decorateStatus: one combat overlay, blocked first. *)
let status_overlay (st : slot_state) =
  if has_flag st blocked_flag then Some ("crystalize", false)
  else if has_flag st stun_flag then Some ("sand", false)
  else if is_silenced st then Some ("mortal", false)
  else if has_flag st invincible_flag then Some ("shield", true)
  else if has_flag st cursed_flag then Some ("death", true)
  else
    match st.data with
    | Limbo_state -> Some ("limbo", false)
    | Eternal -> Some ("eternalrage", true)
    | Hird -> Some ("hird", true)
    | _ -> None

let draw_status_overlay r st =
  match status_overlay st with
  | None -> false
  | Some (name, corner) -> (
      match texture (Assets.combat name) with
      | None -> false
      | Some t ->
          let tw = float (R.Texture.width t) and th = float (R.Texture.height t) in
          let nw, nh = creature_native in
          let fr, s = fit_in r nw nh in
          let bl_x, bl_y = if corner then (50., 60.) else (2., 15.) in
          image_at t (box fr (bl_x, nh -. bl_y -. th, tw, th) s);
          true)

let flag_names st =
  let flags =
    [
      (stun_flag, "stunned");
      (blocked_flag, "blocked");
      (paused_flag, "paused");
      (invincible_flag, "invincible");
      (cursed_flag, "cursed");
    ]
    |> List.filter_map (fun (f, n) -> if has_flag st f then Some n else None)
  in
  let extra =
    (match st.data with Limbo_state -> [ "limbo" ] | _ -> [])
    @ if is_silenced st then [ "silenced" ] else []
  in
  String.concat " · " (flags @ extra)

(* Scala SlotCardActors: paused gray+half-alpha, limbo pale blue, hidden Shade
   Stalker a violet wash (stealthTint), MereMortal (Schizo / Queen /
   Battlemage) a dusty wash plus the mortal overlay. *)
let stealth_tint = rgba 133 115 184 255

let slot_face_look st =
  if has_flag st paused_flag then (rgba 102 102 102 255, 0.5)
  else
    match st.data with
    | Limbo_state -> (rgba 158 199 255 255, 1.)
    | Stealthed -> (stealth_tint, 1.)
    | _ when is_silenced st -> (rgba 168 148 112 255, 1.)
    | _ -> (R.Color.white, 1.)

(* 0 → reach → 0, toward the other row: P1 is at the bottom and goes up *)
let attack_offset ui pid num =
  let now = R.get_time () in
  match List.find_opt (fun a -> a.a_pid = pid && a.a_num = num && now >= a.a_start) ui.attacks with
  | None -> 0.
  | Some a ->
      let t = (now -. a.a_start) /. attack_duration in
      let d = attack_reach *. (1. -. Float.abs (1. -. (2. *. t))) in
      a.a_dir *. d

(* Scala CardActors Focus: 1 + 0.05 sin(pi t) *)
let focus_scale ui pid num =
  let now = R.get_time () in
  match List.find_opt (fun f -> f.f_pid = pid && f.f_num = num && now >= f.f_start) ui.focuses with
  | None -> 1.
  | Some f -> 1. +. (0.05 *. sin (Float.pi *. Float.min 1. ((now -. f.f_start) /. focus_duration)))

let death_alpha ui pid num slot_id =
  match List.find_opt (fun d -> d.d_pid = pid && d.d_num = num && d.d_id = slot_id) ui.deaths with
  | None -> None
  | Some d ->
      let now = R.get_time () in
      if now < d.d_start then Some 1.
      else Some (Float.max 0. (1. -. fade_smooth (Float.min 1. ((now -. d.d_start) /. death_duration))))

let soul_slot_tint = rgba 158 178 235 255
let cocoon_slot_tint = rgba 224 204 140 255

let has_bound_soul (p : player_state) num =
  match p.ps_data with
  | Soulbinder_data souls -> List.mem num souls && Option.is_none p.ps_slots.(num)
  | _ -> false

let has_cocoon (p : player_state) num =
  match p.ps_data with Zen_cocoons xs -> List.mem num xs && Option.is_none p.ps_slots.(num) | _ -> false

let rgb01 r g b a =
  let b8 x = int_of_float (Float.max 0. (Float.min 255. (x *. 255.))) in
  rgba (b8 r) (b8 g) (b8 b) (b8 a)

(* Scala SlotButton.BoundSoulActor: additive spirit in the well. *)
let draw_bound_soul well =
  let t = R.get_time () in
  let pulse = 0.72 +. (0.28 *. sin (t *. 2.6)) in
  let cx = well.x +. (well.w /. 2.) in
  let cy = well.y +. (well.h /. 2.) in
  let ellipse rx ry r g b a =
    if rx *. !scale > 0.4 && ry *. !scale > 0.4 then
      R.draw_ellipse_v (pos cx cy) (rx *. !scale) (ry *. !scale) (alpha_color (rgb01 r g b a))
  in
  let circle x y rad r g b a =
    if rad *. !scale > 0.4 then R.draw_circle_v (pos x y) (rad *. !scale) (alpha_color (rgb01 r g b a))
  in
  R.begin_blend_mode R.BlendMode.Additive;
  R.draw_rectangle 0 0 1 1 (alpha_color (rgb01 1. 1. 1. 0.));
  ellipse 32. 40. 0.35 0.7 1. (0.22 *. pulse);
  ellipse 18. 29. 0.55 0.85 1. (0.38 *. pulse);
  circle cx (cy -. 16.) 13. 0.85 0.95 1. (0.55 *. pulse);
  circle cx (cy -. 18.) 5.5 1. 1. 1. 0.8;
  let rise = mod_float (t *. 18.) 36. in
  circle (cx -. 10.) (cy +. 8. -. (rise *. 0.4)) 3.2 0.75 0.92 1. 0.28;
  circle (cx +. 12.) (cy +. 2. -. (mod_float (rise +. 12.) 36. *. 0.35)) 2.6 0.75 0.92 1. 0.28;
  R.end_blend_mode ()

(* Scala SlotButton.CocoonActor: gold glow in the well, then the Cocoon sprite. *)
let draw_cocoon well =
  let t = R.get_time () in
  let pulse = 0.72 +. (0.28 *. sin (t *. 2.2)) in
  let cx = well.x +. (well.w /. 2.) in
  let cy = well.y +. (well.h /. 2.) in
  let ellipse rx ry r g b a =
    if rx *. !scale > 0.4 && ry *. !scale > 0.4 then
      R.draw_ellipse_v (pos cx cy) (rx *. !scale) (ry *. !scale) (alpha_color (rgb01 r g b a))
  in
  R.begin_blend_mode R.BlendMode.Additive;
  R.draw_rectangle 0 0 1 1 (alpha_color (rgb01 1. 1. 1. 0.));
  ellipse 30. 42. 0.95 0.78 0.35 (0.2 *. pulse);
  ellipse 16. 30. 0.98 0.9 0.65 (0.32 *. pulse);
  ellipse 8. 16. 1. 0.97 0.88 (0.45 *. pulse);
  R.end_blend_mode ();
  match texture (Assets.combat "cocoon") with
  | None -> ()
  | Some tex ->
      let a = 0.62 +. (0.2 *. sin (t *. 2.2)) in
      with_alpha a (fun () -> image_at tex (rect (cx -. 35.) (cy -. 44.) 70. 88.))

let draw_slot_well ?(tint = R.Color.white) r =
  match texture (Assets.slot_frame ()) with
  | Some t -> image_slot ~tint t (slot_well_rect r)
  | None ->
      fill ~round:0.08 (slot_well_rect r) panel;
      outline ~round:0.08 (slot_well_rect r) line

let draw_empty_slot ui pid num r (p : player_state) =
  let hidden = slot_hidden ui pid num in
  if (not hidden) && has_cocoon p num then (
    draw_slot_well ~tint:cocoon_slot_tint r;
    draw_cocoon (slot_well_rect r);
    if inside r then
      ui.hover <-
        Some (Hover_note { title = Assets.text "zen.cocoon"; body = Assets.text "zen.cocoon.description" }))
  else if (not hidden) && has_bound_soul p num then (
    draw_slot_well ~tint:soul_slot_tint r;
    draw_bound_soul (slot_well_rect r);
    if inside r then
      ui.hover <-
        Some
          (Hover_note
             {
               title = Assets.text "soulbinder.boundsoul";
               body = Assets.text "soulbinder.boundsoul.description";
             }))
  else draw_slot_well r

(* Warp reality: the card is painted into an off-screen texture, then drawn through the warp shader. *)
let warp_rt : (int * int * R.RenderTexture.t) option ref = ref None

let with_warp r amount draw =
  match Fx.warp_shader "warp" with
  | None -> draw r
  | Some shader ->
      let w = max 1 (int_of_float (Float.ceil (r.w *. !scale)))
      and h = max 1 (int_of_float (Float.ceil (r.h *. !scale))) in
      let rt =
        match !warp_rt with
        | Some (rw, rh, t) when rw = w && rh = h -> t
        | old ->
            Option.iter (fun (_, _, t) -> R.unload_render_texture t) old;
            let t = R.load_render_texture w h in
            warp_rt := Some (w, h, t);
            t
      in
      let ox = !off_x and oy = !off_y and a = !draw_alpha in
      off_x := 0.;
      off_y := 0.;
      draw_alpha := 1.;
      R.begin_texture_mode rt;
      R.clear_background (R.Color.create 0 0 0 0);
      draw (rect 0. 0. r.w r.h);
      R.end_texture_mode ();
      off_x := ox;
      off_y := oy;
      draw_alpha := a;
      Fx.warp_uniforms ~amount;
      R.begin_shader_mode shader;
      R.draw_texture_pro (R.RenderTexture.texture rt)
        (R.Rectangle.create 0. 0. (float w) (-.float h))
        (screen r) (R.Vector2.create 0. 0.) 0. (alpha_color R.Color.white);
      R.end_shader_mode ()

let draw_slot ui pid num =
  let r = rect (slot_x num) (oy ui (row_y pid)) slot_w slot_h in
  let p = (view ui).players.(pid) in
  (* A Shade Stalker hidden from the viewer is an empty well: no highlight,
     no hover card, no click target (Scala SlotPanel / GameInit). *)
  let concealed = match p.ps_slots.(num) with Some st -> concealed ui pid st | None -> false in
  let targets =
    if concealed then []
    else
      match ui.mode with
      | Target cmds -> List.filter (fun c -> c.cmd_input = Some num && Table.target_side c = Some pid) cmds
      | Browse -> []
  in
  (match targets with _ :: _ when inside r -> fill ~round:0.08 r (rgba 246 196 82 70) | _ -> ());
  (if not (List.mem num p.ps_slot_list) then text_c ~size:28. ~color:faint r (r.y +. (r.h /. 2.) -. 16.) "×"
   else
     match p.ps_slots.(num) with
     | None -> draw_empty_slot ui pid num r p
     | Some st when concealed -> draw_empty_slot ui pid num r p
     | Some _ when slot_hidden ui pid num -> draw_empty_slot ui pid num r p
     | Some st ->
         let fade = match death_alpha ui pid num st.slot_id with None -> 1. | Some a -> a in
         if fade <= 0.02 then draw_slot_well r
         else (
           if fade < 0.999 then draw_slot_well r;
           let c = st.card.c_card in
           if fade >= 0.999 && inside r then ui.hover <- Some (Hover_card (c, pid, Some st.max_life));
           let life_color = if st.life < st.max_life then gold else life_ok in
           let shake_x, shake_y = Fx.slot_offset pid num in
           let r = { r with x = r.x +. shake_x; y = r.y +. attack_offset ui pid num +. shake_y } in
           let k = focus_scale ui pid num in
           let r =
             {
               x = r.x -. (r.w *. (k -. 1.) /. 2.);
               y = r.y -. (r.h *. (k -. 1.) /. 2.);
               w = r.w *. k;
               h = r.h *. k;
             }
           in
           let tint, look_a = slot_face_look st in
           let decorated = ref false in
           with_alpha (fade *. look_a) (fun () ->
               let warp =
                 if Fx.warp_any () then Fx.warp_amount ((fx_layout ui).slot_center pid num) else 0.
               in
               let face r =
                 draw_card_face (desc ui) r c ~cost:(Some c.cost)
                   ~attack:(Some (string_of_int st.attack))
                   ~life:(Some st.life) ~life_color ~tint
                   ~decor:(fun () -> decorated := draw_status_overlay r st)
               in
               if warp > 0.02 then with_warp r warp face else face r;
               if not !decorated then
                 let flags = flag_names st in
                 if flags <> "" then
                   text_c ~size:11. ~color:gold r (r.y +. r.h -. 2.) (fit 11. (r.w +. 8.) flags))));
  match targets with
  | [] -> ()
  | c :: _ ->
      let hot = inside r in
      outline ~round:0.08 ~thick:(if hot then 3.5 else 2.5) r gold;
      if input_ok ui then on_click r (fun () -> play ui (Some c))

(* Scala PortraitFlash: red or green, easing back to white over 0.5 s *)
let portrait_flash ui pid =
  let now = R.get_time () in
  List.find_map
    (fun p ->
      match p.at with
      | Player_life q when q = pid && now >= p.p_start && now -. p.p_start < 0.5 ->
          let u = (now -. p.p_start) /. 0.5 in
          let k = 1. -. ((1. -. u) *. (1. -. u)) in
          let r, g, b = if p.amount < 0 then (1., 0.28, 0.28) else (0.4, 1., 0.48) in
          let ch c = int_of_float (255. *. (c +. ((1. -. c) *. k))) in
          Some (rgba (ch r) (ch g) (ch b) 255)
      | _ -> None)
    ui.pops

(* Scala ReadPlate: dark wash so life / extra labels read over board art. *)
let plate_ink = rgba 10 8 5 148

let draw_label_plate ?font ~size ~color x y s =
  let tw = text_w ?font size s in
  let pad_x = 5. and pad_y = 2. in
  fill ~round:0.16 (rect (x -. pad_x) (y -. pad_y) (tw +. (pad_x *. 2.)) (size +. (pad_y *. 2.))) plate_ink;
  text ?font ~size ~color x y s

let draw_player_bar ui pid =
  let x = 8. in
  let y = oy ui (row_y pid) in
  let face_id = (if pid = owner then fst else snd) ui.portraits in
  let face = rect x y portrait_size portrait_size in
  draw_face ?tint:(portrait_flash ui pid) face_id face;
  outline ~round:0.02 ~thick:1. face line;
  let seat = Table.seat ui.table pid in
  (match seat with
  | Table.Bot id ->
      (* In a campaign fight the opponent is the campaign character, not the bot kind. *)
      let label =
        match (pid, ui.campaign) with
        | p, Some n when p = opponent -> Assets.text ("campaign." ^ n.id ^ ".name")
        | _ -> cap id
      in
      text_c ~font:bold ~size:12. ~color:fg face (y +. portrait_size +. 2.) (fit ~font:bold 12. face.w label)
  | Table.Human -> ());
  let tx = x +. portrait_size +. 8. in
  let p = (view ui).players.(pid) in
  draw_label_plate ~font:bold ~size:22. ~color:fg tx (y +. 8.) (string_of_int p.ps_life);
  (* Scala DataPanel beside the life: souls, Dark power, eclipse shield, shaman buffs. *)
  let line_y = ref (y +. 36.) in
  let badge s =
    draw_label_plate ~size:13. ~color:accent tx !line_y s;
    line_y := !line_y +. 19.
  in
  (match p.ps_data with
  | Soulbinder_data xs ->
      let n = List.length xs in
      if n > 0 then badge (Printf.sprintf "%d soul%s" n (if n = 1 then "" else "s"))
  | Reaper_data n -> badge (Printf.sprintf "%d soul%s" n (if n = 1 then "" else "s"))
  | Shadowcaster_data n -> badge (Printf.sprintf "Dark %d" n)
  | Shadowmage_data d when d.eclipse_shield > 0 -> badge (Printf.sprintf "eclipse %d" d.eclipse_shield)
  | Wolf_data d when d.wo_protection > 0 -> badge (Printf.sprintf "wolf armor %d" d.wo_protection)
  | Wind_state w when w.vortex -> badge "vortex x1.5"
  | _ -> ());
  if
    Array.exists
      (function Some (st : slot_state) -> st.card.c_card.key = "shaman.protector" | None -> false)
      p.ps_slots
  then badge "protector -2"

let draw_board ui =
  (* Solar Wave: the opponent's creatures wear golden powder while they take triple damage. *)
  (* A creature that is sliding or attacking has left its slot: no powder on it. *)
  let settled pid n =
    Option.is_some (view ui).players.(pid).ps_slots.(n)
    && (not (slot_hidden ui pid n))
    && attack_offset ui pid n = 0.
  in
  let settled_spots pid =
    List.filter_map
      (fun n -> if settled pid n then Some ((fx_layout ui).slot_center pid n) else None)
      base_slot_list
  in
  Fx.set_powder_spots
    ~occupied:(settled_spots opponent @ settled_spots owner)
    (List.concat_map
       (fun pid ->
         match (view ui).players.(pid).ps_data with
         | Season_data d when d.se_triple -> settled_spots (1 - pid)
         | _ -> [])
       [ opponent; owner ]);
  (* Energetic Vortex lingers beside the portrait of the player who cast it. *)
  Fx.set_vortex_spots
    (List.filter_map
       (fun pid ->
         match (view ui).players.(pid).ps_data with
         | Wind_state w when w.vortex -> Some (Fx.vortex_badge (fx_layout ui) pid)
         | _ -> None)
       [ opponent; owner ]);
  (* Arcane Trap: a pentagram over the victim's slots until the skipped turn is over. *)
  Fx.set_trap_spots
    (List.filter_map
       (fun pid ->
         let skipped = turn_skipped (view ui).players.(pid) in
         (match (ui.trap.(pid), skipped) with
         | 1, true -> ui.trap.(pid) <- 2
         | 2, false -> ui.trap.(pid) <- 0
         | _ -> ());
         if ui.trap.(pid) = 2 then
           let c, r = Fx.trap_center (fx_layout ui) pid in
           Some (pid, c, r)
         else None)
       [ opponent; owner ]);
  (* Deep Freeze: both portraits stay frosted from the cast until the skipped turns and the special-card ban are
     over. *)
  let frozen_now =
    Array.exists
      (fun p -> turn_skipped p || List.exists (fun m -> m == Common.hide_special_mod) p.ps_desc.desc_mods)
      (view ui).players
  in
  (match (ui.freeze, frozen_now) with
  | 1, true -> ui.freeze <- 2
  | 2, false -> ui.freeze <- 0
  | _ -> ());
  Fx.set_frost_spots
    (if ui.freeze = 0 then []
     else
       List.map
         (fun pid -> (pid, (fx_layout ui).life pid, portrait_size /. 2.))
         [ opponent; owner ]);
  draw_player_bar ui opponent;
  List.iter (fun n -> draw_slot ui opponent n) base_slot_list;
  List.iter (fun n -> draw_slot ui owner n) base_slot_list;
  draw_player_bar ui owner

(* ----------------------------------------------------------------- hands *)

let reveal_hand ui pid = Table.seat ui.table pid = Human || (both_bots ui && pid = owner)
let house_title (h : house) = if Assets.has_key h.h_key then Assets.text h.h_key else cap h.h_name

let house_blurb (h : house) =
  let key = h.h_key ^ ".description" in
  if Assets.has_key key then Assets.text key else ""

let open_house_dlg ui pid (h : house) =
  ui.house_dlg <- Some { house = h; player = pid; scroll = 0.; drag = false; content_h = 0. }

let card_preview_bits ?state ?player ?max_life (c : card) =
  let title = Printf.sprintf "%s (%d)" (Assets.card_name c) c.cost in
  let subtitle =
    match c.creature_body with
    | Some b ->
        let life =
          match max_life with
          | Some m when m <> b.b_life -> Printf.sprintf "%d (%d)" m b.b_life
          | _ -> string_of_int b.b_life
        in
        Printf.sprintf "Life : %s  Attack : %s" life
          (match b.b_attack.base with Some a -> string_of_int a | None -> "X")
    | None -> "Spell"
  in
  let desc_text =
    let d = Assets.describe ?state ?player c in
    if d = "" || d = c.description then "" else d
  in
  (title, subtitle, desc_text)

let draw_house_header ui pid hd col header_y =
  let x = !lay.hand_x0 +. (float col *. house_col) in
  let y = oy ui header_y in
  let p = (view ui).players.(pid) in
  let i = hd.phd_house.h_index in
  let name = house_title hd.phd_house in
  let mana = string_of_int p.ps_houses.(i).mana in
  let pad_x = 3. in
  let max_w = house_col -. (pad_x *. 2.) in
  let rec fit size =
    if size <= 11. then size
    else
      let tw = text_w size (name ^ " ") +. text_w ~font:bold size mana in
      if tw <= max_w then size else fit (size -. 1.)
  in
  let size = fit 16. in
  let plate = rect x (y +. 1.) house_col (house_header_h -. 2.) in
  let hot = inside plate in
  fill ~round:0.04 plate (rgba 10 8 5 148);
  let name_s = name ^ " " in
  let total = text_w size name_s +. text_w ~font:bold size mana in
  let tx = x +. ((house_col -. total) /. 2.) in
  let ty = y +. ((house_header_h -. size) /. 2.) -. 1. in
  let name_color = if hot then accent else fg in
  text ~size ~color:name_color tx ty name_s;
  text ~font:bold ~size (tx +. text_w size name_s) ty mana;
  if hot then (
    ui.hover <- Some (Hover_house hd.phd_house);
    if pid = opponent then ui.hover_opp <- true);
  on_click plate (fun () -> open_house_dlg ui pid hd.phd_house)

let draw_hand_at ui pid ~cards_y ~header_y =
  let reveal_all = reveal_hand ui pid in
  let can_choose = human_to_move ui = Some pid in
  (* In a game with a human, only the side to move looks active; a bot-only
     table keeps the informative tint on both sides. *)
  let human_game = Table.seat ui.table owner = Human || Table.seat ui.table opponent = Human in
  let turn_on = (not human_game) || (Table.to_move ui.table = Some pid && input_ok ui) in
  let selected = match ui.mode with Target (c :: _) -> Some c.cmd_card.key | _ -> None in
  List.iteri
    (fun col hd ->
      draw_house_header ui pid hd col header_y;
      let x = !lay.hand_x0 +. (float col *. house_col) in
      let cx = x +. ((house_col -. card_w) /. 2.) in
      Array.iteri
        (fun k cd ->
          if k < n_hand_rows then
            let r = rect cx (oy ui (cards_y +. (float k *. (card_h +. card_gap)))) card_w card_h in
            let c = cd.cd_card in
            let known = reveal_all || Hashtbl.mem ui.discovered (pid, c.key) in
            if known then (
              let playable = card_playable ui pid cd hd in
              let active = can_choose && playable && input_ok ui in
              let is_selected = selected = Some c.key in
              let grey = reveal_all && not playable in
              if (not is_selected) && active && inside r then fill ~round:0.08 r (rgba 255 255 255 28);
              Fx.with_grey grey (fun () ->
                  draw_card_face (desc ui) r c ~cost:(Some cd.cd_cost) ~attack:(printed_attack c)
                    ~life:(printed_life c) ~life_color:life_ok ~tint:R.Color.white ~decor:(fun () -> ()));
              (* Gray-out veil while another side is to move, over art and text alike. *)
              if not turn_on then fill ~round:0.08 r (rgba 168 168 178 125);
              if is_selected then Fx.draw_sel ~x:r.x ~y:r.y ~w:r.w ~h:r.h;
              if inside r then (
                ui.hover <- Some (Hover_card (c, pid, None));
                if pid = opponent then ui.hover_opp <- true);
              if active then on_click r (fun () -> pick_card ui cd hd)))
        hd.phd_cards)
    (Table.hand ui.table pid)

(* Scala DamageComponent: green "+X" or red "-X" rising 20 px in 1 s *)
let draw_pops ui =
  let now = R.get_time () in
  List.iter
    (fun p ->
      if now >= p.p_start then
        let anchor =
          match p.at with
          | Slot_life (pid, n) ->
              let r = slot_rect ui pid n in
              Some (r.x +. r.w -. 30., r.y +. r.h -. 30.)
          | Player_life pid -> Some (8. +. portrait_size +. 48., oy ui (row_y pid) +. 8.)
          | Mana (pid, i) ->
              let header_y = if pid = opponent then opp_header_y else hand_header_y in
              List.find_index (fun hd -> hd.phd_house.h_index = i) (Table.hand ui.table pid)
              |> Option.map (fun col ->
                  (!lay.hand_x0 +. (float (col + 1) *. house_col) -. 24., oy ui header_y -. 2.))
        in
        Option.iter
          (fun (x, y) ->
            let y = y -. (pop_rise *. Float.min 1. ((now -. p.p_start) /. pop_duration)) in
            let s = if p.amount > 0 then "+" ^ string_of_int p.amount else string_of_int p.amount in
            text ~font:bold ~size:18. ~color:(rgba 0 0 0 200) (x +. 1.) (y +. 1.) s;
            text ~font:bold ~size:18. ~color:(if p.amount > 0 then green else red) x y s)
          anchor)
    ui.pops

let draw_hands ui =
  (* Scala: opponent cards, then house labels toward the slots; player the reverse. *)
  draw_hand_at ui opponent ~cards_y:opp_hand_y0 ~header_y:opp_header_y;
  draw_hand_at ui owner ~cards_y:hand_y ~header_y:hand_header_y

(* ---------------------------------------------------------- left preview *)

let preview_top ui = oy ui (if ui.hover_opp then opp_hand_y0 else hand_y)
let last_played_rect _ui = rect 8. (vh -. last_played_h -. 8.) 180. last_played_h

(* Between the portrait column and slot 0, midway between the two players. *)
let spell_show_rect ui =
  let gap_l = 8. +. portrait_size and gap_r = !lay.slot_x0 in
  let avail = Float.max 40. (gap_r -. gap_l -. 8.) in
  let size = Float.min spell_show_size avail in
  let x = gap_l +. ((gap_r -. gap_l -. size) /. 2.) in
  let y0 = oy ui (row_y opponent +. (slot_h /. 2.)) in
  let y1 = oy ui (row_y owner +. (slot_h /. 2.)) in
  let y = ((y0 +. y1) /. 2.) -. (size /. 2.) in
  rect x y size size

let draw_card_art r (c : card) =
  match card_texture c with Some t -> image t r | None -> fill ~round:0.04 r panel_hi

let draw_note_preview ~x ~w ~y ~pad ~title ~body =
  let title_size = 20. and desc_size = 18. in
  let title_lh = 24. and desc_lh = 22. in
  let title_lines = List.filteri (fun i _ -> i < 2) (wrap ~font:bold title_size (w -. (pad *. 2.)) title) in
  let desc_lines =
    if body = "" then [] else List.filteri (fun i _ -> i < 8) (wrap desc_size (w -. (pad *. 2.)) body)
  in
  let desc_h = if desc_lines = [] then 0. else 8. +. (float (List.length desc_lines) *. desc_lh) in
  let box_h = pad +. (float (List.length title_lines) *. title_lh) +. desc_h +. pad in
  let box = rect x y w box_h in
  fill ~round:0.04 box (rgba 0 0 0 90);
  List.iteri
    (fun i l -> text ~font:bold ~size:title_size (x +. pad) (y +. pad +. (float i *. title_lh)) l)
    title_lines;
  List.iteri
    (fun i l ->
      text ~size:desc_size (x +. pad)
        (y +. pad +. (float (List.length title_lines) *. title_lh) +. 8. +. (float i *. desc_lh))
        l)
    desc_lines

let draw_preview ui =
  let x = !lay.preview_x and w = !lay.preview_w in
  let y = preview_top ui in
  let pad = 8. in
  match ui.hover with
  | None -> ()
  | Some (Hover_house h) -> draw_note_preview ~x ~w ~y ~pad ~title:(house_title h) ~body:(house_blurb h)
  | Some (Hover_note { title; body }) -> draw_note_preview ~x ~w ~y ~pad ~title ~body
  | Some (Hover_card (c, pid, max_life)) ->
      let art_s = if c.is_spell then 130. else 120. in
      let title_size = 20. and sub_size = 16. and desc_size = 18. in
      let title_lh = 24. and desc_lh = 22. in
      let title, subtitle, desc_text = card_preview_bits ~state:(view ui) ~player:pid ?max_life c in
      let info_w = Float.max 80. (w -. art_s -. (pad *. 3.)) in
      let title_lines = List.filteri (fun i _ -> i < 2) (wrap ~font:bold title_size info_w title) in
      let desc_lines =
        if desc_text = "" then []
        else List.filteri (fun i _ -> i < 8) (wrap desc_size (w -. (pad *. 2.)) desc_text)
      in
      let info_h = (float (List.length title_lines) *. title_lh) +. 6. +. sub_size in
      let header_h = Float.max art_s info_h in
      let desc_h = if desc_lines = [] then 0. else 8. +. (float (List.length desc_lines) *. desc_lh) in
      let box_h = pad +. header_h +. desc_h +. pad in
      let box = rect x y w box_h in
      fill ~round:0.04 box (rgba 0 0 0 90);
      let art = rect (x +. pad) (y +. pad) art_s art_s in
      draw_card_art art c;
      let tx = x +. pad +. art_s +. pad in
      List.iteri
        (fun i l -> text ~font:bold ~size:title_size tx (y +. pad +. (float i *. title_lh)) l)
        title_lines;
      let sub_y = y +. pad +. (float (List.length title_lines) *. title_lh) +. 4. in
      text ~size:sub_size ~color:muted tx sub_y subtitle;
      List.iteri
        (fun i l -> text ~size:desc_size (x +. pad) (y +. pad +. header_h +. 8. +. (float i *. desc_lh)) l)
        desc_lines

let draw_last_played ui =
  match !(ui.last_played) with
  | None -> ()
  | Some lp -> (
      let r = last_played_rect ui in
      fill ~round:0.06 r (rgba 10 8 5 148);
      if inside r then hovering := true;
      match lp.card with
      | Some c ->
          if inside r then ui.hover <- Some (Hover_card (c, lp.player, None));
          let thumb = 40. in
          draw_card_art (rect (r.x +. 6.) (r.y +. ((r.h -. thumb) /. 2.)) thumb thumb) c;
          text ~size:14. ~color:fg (r.x +. 52.) (r.y +. ((r.h -. 14.) /. 2.)) "Last played"
      | None ->
          text ~size:14. ~color:muted (r.x +. 12.)
            (r.y +. ((r.h -. 14.) /. 2.))
            (Printf.sprintf "P%d passed" (lp.player + 1)))

let draw_spell_shows ui =
  let now = R.get_time () in
  List.iter
    (fun s ->
      if now >= s.ss_start && now < s.ss_start +. spell_show_duration then (
        let r = spell_show_rect ui in
        if inside r then (
          hovering := true;
          ui.hover <- Some (Hover_card (s.ss_card, s.ss_player, None)));
        match card_texture s.ss_card with Some t -> image_circle t r | None -> fill ~round:0.5 r panel_hi))
    ui.spell_shows

(* Scala HouseDescription: parchment list of every card in the house. *)
let house_dlg_box = rect 56. 36. 720. 696.

let house_dlg_view =
  rect (house_dlg_box.x +. 20.) (house_dlg_box.y +. 56.) (house_dlg_box.w -. 52.) (house_dlg_box.h -. 116.)

let house_dlg_bar = rect (house_dlg_view.x +. house_dlg_view.w +. 4.) house_dlg_view.y 12. house_dlg_view.h

let house_scroll_by dlg wheel =
  let max = Float.max 0. (dlg.content_h -. house_dlg_view.h) in
  dlg.scroll <- Float.min max (Float.max 0. (dlg.scroll -. (wheel *. 48.)))

let house_card_art_size (c : card) = if c.is_spell then (100., 108.) else (96., 96.)
let house_card_title_size = 19.
let house_card_sub_size = 16.
let house_card_desc_size = 17.
let house_card_title_lh = 23.
let house_card_desc_lh = 21.

let measure_house_card ?state ?player col_w (c : card) =
  let title, subtitle, desc_text = card_preview_bits ?state ?player c in
  let art_w, art_h = house_card_art_size c in
  let pad = 10. in
  let info_w = Float.max 60. (col_w -. art_w -. (pad *. 3.)) in
  let title_lines = wrap ~font:paper_font house_card_title_size info_w title in
  let desc_lines =
    if desc_text = "" then [] else wrap ~font:paper_font house_card_desc_size (col_w -. (pad *. 2.)) desc_text
  in
  let info_h = (float (List.length title_lines) *. house_card_title_lh) +. 4. +. house_card_sub_size in
  let header_h = Float.max art_h info_h in
  let desc_h = if desc_lines = [] then 0. else 6. +. (float (List.length desc_lines) *. house_card_desc_lh) in
  (title_lines, subtitle, desc_lines, art_w, art_h, pad +. header_h +. desc_h +. pad)

let draw_house_card r (c : card) title_lines subtitle desc_lines art_w art_h =
  let pad = 10. in
  fill ~round:0.04 r (rgba 140 107 41 70);
  outline ~round:0.04 ~thick:1. r (rgba 140 107 41 90);
  let art = rect (r.x +. pad) (r.y +. pad) art_w art_h in
  draw_card_art art c;
  let tx = r.x +. pad +. art_w +. 8. in
  List.iteri
    (fun i l ->
      text ~font:paper_font ~size:house_card_title_size ~color:ink tx
        (r.y +. pad +. (float i *. house_card_title_lh))
        l)
    title_lines;
  let sub_y = r.y +. pad +. (float (List.length title_lines) *. house_card_title_lh) +. 2. in
  text ~font:paper_font ~size:house_card_sub_size ~color:ink_soft tx sub_y subtitle;
  let header_h =
    Float.max art_h ((float (List.length title_lines) *. house_card_title_lh) +. 4. +. house_card_sub_size)
  in
  List.iteri
    (fun i l ->
      text ~font:paper_font ~size:house_card_desc_size ~color:ink (r.x +. pad)
        (r.y +. pad +. header_h +. 6. +. (float i *. house_card_desc_lh))
        l)
    desc_lines

let rec house_card_pairs = function
  | a :: b :: rest -> (Some a, Some b) :: house_card_pairs rest
  | [ a ] -> [ (Some a, None) ]
  | [] -> []

let close_house_dlg ui = ui.house_dlg <- None

let draw_house_details ui dlg =
  click_full (fun () -> close_house_dlg ui);
  let box = house_dlg_box in
  paper box;
  on_click box (fun () -> ());
  text_c ~font:paper_font ~size:30. ~color:ink box (box.y +. 16.) (house_title dlg.house);
  let view = house_dlg_view in
  let col_gap = 12. in
  let col_w = (view.w -. col_gap) /. 2. in
  let blurb = house_blurb dlg.house in
  let blurb_size = 18. and blurb_lh = 22. in
  let blurb_lines = if blurb = "" then [] else wrap ~font:paper_font blurb_size view.w blurb in
  let blurb_h = if blurb_lines = [] then 0. else (float (List.length blurb_lines) *. blurb_lh) +. 10. in
  let cards = dlg.house.h_cards @ dlg.house.h_additional_cards in
  let measured =
    List.map (fun c -> (c, measure_house_card ~state:ui.shown ~player:dlg.player col_w c)) cards
  in
  let rows = house_card_pairs measured in
  let row_gap = 12. in
  let content_h =
    List.fold_left
      (fun acc -> function
        | Some (_, (_, _, _, _, _, h0)), Some (_, (_, _, _, _, _, h1)) -> acc +. Float.max h0 h1 +. row_gap
        | Some (_, (_, _, _, _, _, h0)), None -> acc +. h0 +. row_gap
        | None, _ -> acc)
      blurb_h rows
  in
  dlg.content_h <- content_h;
  let max = Float.max 0. (content_h -. view.h) in
  dlg.scroll <- Float.min max (Float.max 0. dlg.scroll);
  begin_clip view;
  let y = ref (view.y -. dlg.scroll) in
  if blurb_lines <> [] then (
    List.iteri
      (fun i l -> text ~font:paper_font ~size:blurb_size ~color:ink view.x (!y +. (float i *. blurb_lh)) l)
      blurb_lines;
    y := !y +. blurb_h);
  let seen (r : rect) = r.y +. r.h > view.y && r.y < view.y +. view.h in
  let draw_one x (c, (title_lines, subtitle, desc_lines, art_w, art_h, h)) =
    let r = rect x !y col_w h in
    if seen r then draw_house_card r c title_lines subtitle desc_lines art_w art_h;
    h
  in
  List.iter
    (fun pair ->
      let h =
        match pair with
        | Some left, Some right ->
            Float.max (draw_one view.x left) (draw_one (view.x +. col_w +. col_gap) right)
        | Some left, None -> draw_one view.x left
        | None, _ -> 0.
      in
      y := !y +. h +. row_gap)
    rows;
  end_clip ();
  if max > 0. then (
    fill house_dlg_bar (rgba 140 107 41 90);
    let th = Float.max 24. (view.h *. view.h /. content_h) in
    let ty = view.y +. (dlg.scroll /. max *. (view.h -. th)) in
    let thumb = rect house_dlg_bar.x ty house_dlg_bar.w th in
    fill ~round:0.06 thumb (rgba 122 90 40 255);
    if R.is_mouse_button_down R.MouseButton.Left then (
      if dlg.drag || inside thumb then (
        dlg.drag <- true;
        hovering := true;
        let _, my = mouse () in
        let f = (my -. view.y -. (th /. 2.)) /. (view.h -. th) in
        dlg.scroll <- Float.min max (Float.max 0. (f *. max))))
    else dlg.drag <- false)
  else dlg.drag <- false;
  paper_btn_row ~size:20.
    ~cx:(box.x +. (box.w /. 2.))
    ~y:(box.y +. box.h -. 48.)
    [ ("ok", fun () -> close_house_dlg ui) ]

(* Scala CalledElementDialog: 2x2 element grid plus cancel. Cancelling calls
   nothing and leaves any previously recorded command untouched. *)
let called_dlg_box = rect 216. 254. 400. 260.

let draw_called_dlg ui dlg =
  click_full (fun () -> close_called_dlg ui);
  let box = called_dlg_box in
  paper box;
  on_click box (fun () -> ());
  let cur_label =
    match dlg.current with
    | Some i when i >= 0 && i < Array.length element_keys -> Assets.text element_keys.(i)
    | _ -> Assets.text "meteomancer.noelement"
  in
  let title = Assets.format (Assets.text "meteomancer.choose") [ cur_label ] in
  let title_lines = wrap ~font:paper_font 22. (box.w -. 40.) title in
  List.iteri
    (fun i l ->
      text ~font:paper_font ~size:22. ~color:ink (box.x +. 20.) (box.y +. 14. +. (float i *. 26.)) l)
    title_lines;
  let grid_y = box.y +. 14. +. (float (List.length title_lines) *. 26.) +. 10. in
  let bw, bh, gap = (170., 36., 12.) in
  let gx = box.x +. ((box.w -. ((2. *. bw) +. gap)) /. 2.) in
  Array.iteri
    (fun i key ->
      let col = i mod 2 and row = i / 2 in
      let r = rect (gx +. (float col *. (bw +. gap))) (grid_y +. (float row *. (bh +. gap))) bw bh in
      pbutton ~size:20. r (Assets.text key) (fun () -> commit_called ui i))
    element_keys;
  paper_btn_row ~size:20.
    ~cx:(box.x +. (box.w /. 2.))
    ~y:(box.y +. box.h -. 48.)
    [ (Assets.text "button.cancel", fun () -> close_called_dlg ui) ]

let over_ready ui = match ui.over_at with Some t -> R.get_time () >= t | None -> false

let icon_button r name f =
  let hot = inside r in
  if hot then hovering := true;
  (match texture (Assets.ui name) with
  | Some t ->
      let tint = if hot then gold else R.Color.white in
      image ~tint t r
  | None -> button ~size:11. r (cap name) f);
  if Option.is_some (texture (Assets.ui name)) then on_click r f;
  hot

let draw_menu ui ~quit ~settings ~switch ~replay ~start_new =
  let icon, gap, pad = (40., 4., 4.) in
  let over = match Session.status (session ui) with Session.Over _ -> true | Session.To_move _ -> false in
  let last = if over then ("finished", start_new, "button.new") else ("new", start_new, "button.surrender") in
  let items =
    [
      ("quit", quit, "button.quit");
      ("settings", settings, "button.settings");
      ("switch", switch, "button.switch");
      ("restart", replay, "button.restart");
      last;
    ]
  in
  let n = float (List.length items) in
  (* Scala actionBar sits on the board group, so the camera scroll moves it. *)
  let bar = rect 8. (oy ui 4.) ((n *. icon) +. ((n -. 1.) *. gap) +. (pad *. 2.)) (icon +. (pad *. 2.)) in
  (* FantasyFrame: gold rim then a lighter wash than the old 0.70 fill. *)
  fill ~round:0.08 bar (rgba 107 82 31 140);
  fill ~round:0.08 (rect (bar.x +. 2.) (bar.y +. 2.) (bar.w -. 4.) (bar.h -. 4.)) (rgba 18 13 5 112);
  let tip = ref None in
  List.iteri
    (fun i (name, f, tip_key) ->
      let r = rect (bar.x +. pad +. (float i *. (icon +. gap))) (bar.y +. pad) icon icon in
      if icon_button r name f then tip := Some (r, Assets.text tip_key))
    items;
  Option.iter (fun (anchor, label) -> tooltip_below anchor label) !tip;
  if ui.message <> "" then text ~size:12. ~color:red 8. (bar.y +. bar.h +. 4.) (fit 12. 400. ui.message)

let draw_pass ui =
  let bw, bh = (70., 28.) in
  let x = 8. +. ((portrait_size -. bw) /. 2.) in
  let y0 = oy ui (row_y opponent +. slot_h) in
  let y1 = oy ui (row_y owner) in
  let y = ((y0 +. y1) /. 2.) -. (bh /. 2.) in
  let r = rect x y bw bh in
  let enabled = Option.is_some (human_to_move ui) && input_ok ui in
  let icon = if enabled then "play-turn" else "play" in
  match texture (Assets.ui icon) with
  | Some t ->
      let ir = rect (x +. ((bw -. 26.) /. 2.)) (y +. ((bh -. 26.) /. 2.)) 26. 26. in
      if enabled && inside r then hovering := true;
      image ~tint:(if enabled then R.Color.white else rgba 160 160 160 180) t ir;
      if enabled then on_click r (fun () -> play ui None)
  | None -> button ~enabled ~size:14. r "Pass" (fun () -> play ui None)

(* Shadowmage eclipse: while a shield is up the board goes dark, except around
   the two portraits where the sun hangs. The dimming eases in and out. *)
let eclipse_level = ref 0.

let eclipse_active ui =
  Array.exists
    (fun p -> match p.ps_data with Shadowmage_data d -> d.eclipse_shield > 0 | _ -> false)
    (view ui).players

let draw_eclipse ui =
  let target = if eclipse_active ui then 1. else 0. in
  let step = R.get_frame_time () *. 1.6 in
  eclipse_level :=
    if target > !eclipse_level then Float.min target (!eclipse_level +. step)
    else Float.max target (!eclipse_level -. step);
  if !eclipse_level > 0.01 then
    let sw = float (R.get_screen_width ()) and sh = float (R.get_screen_height ()) in
    let l = fx_layout ui in
    let suns =
      List.map
        (fun pid ->
          let p = l.Fx.life pid in
          pos p.x p.y)
        [ owner; opponent ]
    in
    let reach = 190. *. !scale in
    let cell = 16. in
    let ny = int_of_float (ceil (sh /. cell)) and nx = int_of_float (ceil (sw /. cell)) in
    for j = 0 to ny - 1 do
      for i = 0 to nx - 1 do
        let cx = (float i +. 0.5) *. cell and cy = (float j +. 0.5) *. cell in
        let d =
          List.fold_left
            (fun m s -> Float.min m (hypot (cx -. R.Vector2.x s) (cy -. R.Vector2.y s)))
            infinity suns
        in
        let k = Float.max 0. (Float.min 1. ((d -. (reach *. 0.35)) /. (reach *. 0.65))) in
        let a = 0.62 *. !eclipse_level *. k *. k *. (3. -. (2. *. k)) in
        if a > 0.01 then
          R.draw_rectangle
            (int_of_float (float i *. cell))
            (int_of_float (float j *. cell))
            (int_of_float cell + 1)
            (int_of_float cell + 1)
            (R.Color.create 6 3 14 (int_of_float (a *. 255.)))
      done
    done
