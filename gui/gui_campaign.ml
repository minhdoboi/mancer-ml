(* Raylib UI: the campaign map and its intro and epilogue. *)

open Mancer
open Sp
module R = Raylib
open Gui_draw
open Gui_lobby

(* ----------------------------------------------------------- campaign *)

type campaign_dlg =
  | Camp_none
  | Camp_fight of Campaign.node
  | Camp_meeting of Campaign.node
  | Camp_restart
  | Camp_note of {
      title : string;
      body : string;
      portrait : string option;
      button : string;
      ack : unit -> unit;
    }
  | Camp_end of { node : Campaign.node; won : bool }

type campaign_ui = {
  mutable dlg : campaign_dlg;
  mutable night : float;
  mutable night_target : float;
  mutable pending_fight : Campaign.node option;
  mutable pending_epilogue : bool;
  mutable pending_lobby : bool;
  mutable show_settings : bool;
  gs : game_settings;
}

let map_x x = x *. vw
let map_y y = (1. -. y) *. vh
let hit_r = 17.

let circle_at x y radius color =
  let p = pos x y in
  R.draw_circle
    (int_of_float (R.Vector2.x p +. 0.5))
    (int_of_float (R.Vector2.y p +. 0.5))
    (radius *. !scale) color

let bez a c b t =
  let u = 1. -. t in
  (u *. u *. a) +. (2. *. u *. t *. c) +. (t *. t *. b)

let draw_curve (from : Campaign.node) (toward : Campaign.node) width color =
  let x0 = map_x from.x and y0 = map_y from.y in
  let x1 = map_x toward.x and y1 = map_y toward.y in
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let len = Float.max 16. (sqrt ((dx *. dx) +. (dy *. dy))) in
  let sign = if (Hashtbl.hash from.id + Hashtbl.hash toward.id) mod 2 = 0 then 1. else -1. in
  let bulge = Float.min 58. (len *. 0.24) *. sign in
  let cx = ((x0 +. x1) *. 0.5) -. (dy /. len *. bulge) in
  let cy = ((y0 +. y1) *. 0.5) +. (dx /. len *. bulge) in
  let steps = 22 in
  for i = 0 to steps - 1 do
    let t0 = float i /. float steps and t1 = float (i + 1) /. float steps in
    R.draw_line_ex
      (pos (bez x0 cx x1 t0) (bez y0 cy y1 t0))
      (pos (bez x0 cx x1 t1) (bez y0 cy y1 t1))
      (width *. !scale) color
  done

let campaign_progress () =
  let p = Prefs.load () in
  (Prefs.campaign_completed p, Prefs.campaign_position p)

(* The campaign class and difficulty now live in the game settings dialog's
   Campaign tab ({!Gui_lobby.campaign_classes}). *)

let mage_line (n : Campaign.node) =
  match n.fight with
  | None -> Assets.text ("campaign." ^ n.id ^ ".name")
  | Some f ->
      let mage =
        Assets.format (Assets.text "campaign.mage")
          [ Assets.text f.house_key; Assets.text ("campaign.role." ^ f.bot_id) ]
      in
      let life = match f.life with Some n -> n | None -> 60 in
      Assets.text ("campaign." ^ n.id ^ ".name")
      ^ " (" ^ mage ^ ")   "
      ^ Assets.format (Assets.text "campaign.life") [ string_of_int life ]

let end_line ~node_id ~bot_id ~won =
  Assets.text (Campaign.end_message_key ~node_id ~bot_id ~won ~has_key:Assets.has_key)

let parchment_card ~title ~portrait ~subtitle ~message buttons =
  click_full (fun () -> ());
  let msg_w = 400. in
  let face = 112. in
  let face_h = if Option.is_some portrait then face +. 12. else 0. in
  let type_size = 20. in
  let line_h = 26. in
  let lines = wrap ~font:paper_font type_size msg_w message in
  let nlines = List.length lines in
  let edge = 36. in
  let title_h = 32. in
  let sub_h = if subtitle = "" then 0. else 28. in
  let msg_h = float (max 1 nlines) *. line_h in
  let box_w = 520. in
  let box_h =
    Float.min (vh -. 16.)
      (edge +. title_h +. 8. +. face_h +. sub_h +. 8. +. msg_h +. 16. +. btn_h +. edge)
  in
  let box = rect ((vw -. box_w) /. 2.) ((vh -. box_h) /. 2.) box_w box_h in
  paper box;
  on_click box (fun () -> ());
  text ~font:paper_font ~size:22. ~color:ink (box.x +. 40.) (box.y +. 22.) title;
  (match portrait with
  | Some id ->
      draw_face id (rect (box.x +. ((box.w -. face) /. 2.)) (box.y +. edge +. title_h) face face)
  | None -> ());
  let y = ref (box.y +. edge +. title_h +. face_h) in
  if subtitle <> "" then (
    text_c ~font:paper_font ~size:type_size ~color:ink box !y subtitle;
    y := !y +. sub_h);
  y := !y +. 8.;
  List.iteri
    (fun i l ->
      text_c ~font:paper_font ~size:type_size ~color:ink (rect box.x !y box.w 0.)
        (!y +. (float i *. line_h))
        l)
    lines;
  paper_btn_row ~size:16. ~cx:(box.x +. (box.w /. 2.)) ~y:(box.y +. box.h -. edge -. btn_h) buttons

(* [final] is the last duel of the campaign: the button leads on to the
   epilogue instead of back to the map. *)
let draw_campaign_end ~final ~node ~won ~replay ~next =
  match node.Campaign.fight with
  | None -> ()
  | Some f ->
      let buttons =
        (if won then [] else [ (Assets.text "campaign.end.replay", replay) ])
        @ [ (Assets.text (if final then "campaign.continue" else "campaign.end.map"), next) ]
      in
      parchment_card
        ~title:(Assets.text (if won then "campaign.end.win.title" else "campaign.end.lose.title"))
        ~portrait:(Some f.portrait_id) ~subtitle:(mage_line node)
        ~message:(end_line ~node_id:node.id ~bot_id:f.bot_id ~won)
        buttons

let dlg_open = function Camp_none -> false | _ -> true

let maybe_notes (camp : campaign_ui) =
  if dlg_open camp.dlg then ()
  else
    try
      let p = Prefs.load () in
      let completed = Prefs.campaign_completed p in
      let chapter = Campaign.chapter_of completed in
      if
        chapter = Campaign.Night
        && Prefs.campaign_position p = Campaign.night_start_id
        && not (Prefs.campaign_night_rest p)
      then
        camp.dlg <-
          Camp_note
            {
              title = Assets.text "campaign.night.rest.title";
              body = Assets.text "campaign.night.rest";
              portrait = None;
              button = Assets.text "campaign.next";
              ack =
                (fun () ->
                  camp.dlg <-
                    Camp_note
                      {
                        title = Assets.text "campaign.night.visit.title";
                        body = Assets.text "campaign.night.visit";
                        portrait = Some "night-host";
                        button = Assets.text "campaign.next";
                        ack =
                          (fun () ->
                            camp.dlg <-
                              Camp_note
                                {
                                  title = Assets.text "campaign.night.meal.title";
                                  body = Assets.text "campaign.night.meal";
                                  portrait = Some "night-soup";
                                  button = Assets.text "campaign.next";
                                  ack =
                                    (fun () ->
                                      camp.dlg <-
                                        Camp_note
                                          {
                                            title = Assets.text "campaign.night.out.title";
                                            body = Assets.text "campaign.night.out";
                                            portrait = None;
                                            button = Assets.text "campaign.continue";
                                            ack = (fun () -> Prefs.mark_night_rested (Prefs.load ()));
                                          });
                                });
                      });
            }
      else
        match List.assoc_opt "antimancer" Campaign.isolate_path with
        | Some (only :: _)
          when List.mem "antimancer" completed
               && (not (List.mem only completed))
               && not (Prefs.campaign_wake_far p) ->
            camp.dlg <-
              Camp_note
                {
                  title = Assets.text "campaign.antimancer.wake.title";
                  body = Assets.text "campaign.antimancer.wake";
                  portrait = None;
                  button = Assets.text "campaign.continue";
                  ack = (fun () -> Prefs.mark_wake_far (Prefs.load ()));
                }
        | _ -> ()
    with _ -> ()

let new_campaign ?(from_game = false) () =
  let completed, _ = campaign_progress () in
  let night = if Campaign.chapter_of completed = Campaign.Night then 1. else 0. in
  let camp =
    {
      dlg = Camp_none;
      night = (if from_game then night else if night = 1. then 0. else 0.);
      night_target = night;
      pending_fight = None;
      pending_epilogue = false;
      pending_lobby = false;
      show_settings = false;
      gs = game_settings_from_prefs ();
    }
  in
  maybe_notes camp;
  camp

let draw_campaign_intro lobby ~start =
  click_full (fun () -> close_dialog "campaign-intro" (fun () -> lobby.show_campaign_intro <- false));
  let box = rect 156. 80. 520. 560. in
  paper box;
  on_click box (fun () -> ());
  text_c ~font:paper_font ~size:22. ~color:ink box (box.y +. 16.) (Assets.text "campaign.title");
  let p = Prefs.load () in
  let id = Prefs.player_portrait p in
  draw_portrait_chooser ~x:box.x ~y:(box.y +. 56.) ~size:96. ~col_w:box.w id Table.player_portraits (fun v ->
      Prefs.set_player_portrait (Prefs.load ()) v);
  (* The class and difficulty, under the portrait: the house the player keeps
     from the first node to the last, and the bot time a fight uses. The
     campaign deal comes from the Game tab's mode. *)
  let sel_y = box.y +. 192. in
  let sel_w = 176. in
  (* Tag and box, centered as a pair. Two rows, class then difficulty. *)
  let sel_row y tag state items selected on_pick =
    let tag_w = text_w 14. tag in
    let gx = box.x +. ((box.w -. (tag_w +. 10. +. sel_w)) /. 2.) in
    text ~size:14. ~color:ink_soft gx (y +. 6.) tag;
    pselect ~size:16. ~width:sel_w ~state ~x:(gx +. tag_w +. 10.) ~y ~items ~selected ~on_pick
  in
  (* The difficulty is pills, like the bot time on the Game tab. *)
  let pill_row y tag items selected on_pick =
    let widths = List.map (fun (_, l) -> paper_btn_w ~size:16. l) items in
    let pills_w = List.fold_left (fun acc w -> acc +. w +. 6.) 0. widths -. 6. in
    let tag_w = text_w 14. tag in
    let gx = box.x +. ((box.w -. (tag_w +. 10. +. pills_w)) /. 2.) in
    text ~size:14. ~color:ink_soft gx (y +. 6.) tag;
    let bx = ref (gx +. tag_w +. 10.) in
    List.iter
      (fun ((v, label), w) ->
        pbutton ~selected:(v = selected) ~size:16. (rect !bx y w 28.) label (fun () -> on_pick v);
        bx := !bx +. w +. 6.)
      (List.combine items widths)
  in
  let lines = wrap ~font:paper_font 20. 440. (Assets.text "campaign.intro") in
  List.iteri
    (fun i l -> text_c ~font:paper_font ~size:20. ~color:ink box (box.y +. 270. +. (float i *. 26.)) l)
    lines;
  paper_btn_row ~size:16.
    ~cx:(box.x +. (box.w /. 2.))
    ~y:(box.y +. box.h -. 52.)
    [
      ( Assets.text "button.startcampaign",
        fun () ->
          drop_dialog ();
          lobby.show_campaign_intro <- false;
          start () );
      ( Assets.text "button.cancel",
        fun () -> close_dialog "campaign-intro" (fun () -> lobby.show_campaign_intro <- false) );
    ];
  (* The selector is drawn last: its list opens over the dialog and covers the
     difficulty pills above it. *)
  pill_row (sel_y +. 30.) "Difficulty" (campaign_difficulties ())
    (Prefs.campaign_difficulty p)
    (fun d -> Prefs.set_campaign_difficulty (Prefs.load ()) d);
  sel_row sel_y (Assets.text "campaign.class") lobby.class_select (campaign_classes ())
    (Table.campaign_class_of (Prefs.campaign_class p))
    (fun key -> Prefs.set_campaign_class (Prefs.load ()) key)

let draw_campaign (camp : campaign_ui) =
  camp.night <- camp.night +. ((camp.night_target -. camp.night) *. Float.min 1. (R.get_frame_time () *. 1.6));
  if abs_float (camp.night -. camp.night_target) < 0.002 then camp.night <- camp.night_target;
  let completed, here = campaign_progress () in
  let prefs = Prefs.load () in
  let chapter = Campaign.chapter_of completed in
  let open_ids = Campaign.available completed in
  let pulse = 8.4 +. (sin (R.get_time () *. 4.) *. 1.6) in
  (match texture (Assets.campaign "map") with
  | Some t -> Fx.with_night camp.night (fun () -> cover t)
  | None -> fill (rect 0. 0. vw vh) bg);
  List.iter
    (fun (e : Campaign.edge) ->
      match (Campaign.by_id e.from, Campaign.by_id e.toward) with
      | Some a, Some b when a.chapter = chapter && b.chapter = chapter ->
          let via_ok = match e.via with None -> true | Some v -> List.mem v completed in
          let lit = ((not (Campaign.is_playable a)) || List.mem a.id completed) && via_ok in
          draw_curve a b (if lit then 3.1 else 2.2) (if lit then rgba 237 199 82 217 else rgba 46 41 31 115)
      | _ -> ())
    (Campaign.edges ());
  List.iter
    (fun (n : Campaign.node) ->
      if n.chapter = chapter then (
        let x = map_x n.x and y = map_y n.y in
        if not (Campaign.is_playable n) then circle_at x y 5. (rgba 242 230 178 140)
        else if List.mem n.id completed then (
          circle_at x y 7.5 (rgba 107 82 36 230);
          circle_at x y 4.2 (rgba 209 173 77 242))
        else if List.mem n.id open_ids then (
          circle_at x y (pulse +. 6.) (rgba 242 209 71 71);
          circle_at x y pulse (rgba 250 219 87 242);
          circle_at x y 3.4 (rgba 51 31 10 230))
        else circle_at x y 5.5 (rgba 31 28 23 140);
        if n.id = here then (
          circle_at x y 12.5 (rgba 242 242 255 217);
          circle_at x y 9.5 (rgba 38 46 71 89))))
    (Campaign.nodes ());
  (match Campaign.by_id here with
  | None -> ()
  | Some n ->
      let face = rect (map_x n.x -. 14.) (map_y n.y -. 24.) 28. 28. in
      draw_face (Prefs.player_portrait prefs) face);
  List.iter
    (fun (n : Campaign.node) ->
      if n.chapter = chapter && Campaign.is_playable n && List.mem n.id open_ids then
        let r = rect (map_x n.x -. hit_r) (map_y n.y -. hit_r) (hit_r *. 2.) (hit_r *. 2.) in
        on_click r (fun () -> camp.dlg <- (if Campaign.is_fight n then Camp_fight n else Camp_meeting n)))
    (Campaign.nodes ());
  let finished = Campaign.is_finished completed in
  let back = Assets.text "button.back" in
  pbutton ~size:16.
    (rect 16. 12. (paper_btn_w ~size:16. back) 26.)
    back
    (fun () -> camp.pending_lobby <- true);
  let restart = Assets.text "campaign.restart" in
  let rw = paper_btn_w ~size:16. restart in
  pbutton ~size:16. (rect (vw -. 16. -. rw) 12. rw 26.) restart (fun () -> camp.dlg <- Camp_restart);
  (* The campaign class and difficulty moved into the game settings dialog's
     Campaign tab; this button opens it there. *)
  let settings = Assets.text "button.settings" in
  let sw = paper_btn_w ~size:16. settings in
  pbutton ~size:16.
    (rect (vw -. 16. -. rw -. 10. -. sw) 12. sw 26.)
    settings
    (fun () ->
      camp.gs.tab <- 2;
      camp.show_settings <- true);
  (if debug_mode () then
     let label = "Reload map" in
     let w = paper_btn_w ~size:16. label in
     pbutton ~size:16. (rect (vw -. 16. -. w) 44. w 26.) label (fun () ->
         match Campaign.reload () with
         | Ok () -> ()
         | Error e -> prerr_endline ("campaign reload failed: " ^ e)));
  text_c ~font:bold ~size:22. ~color:fg (rect 0. 0. vw 0.) 16.
    (Assets.text (if finished then "campaign.complete" else "campaign.title"));
  text_c ~size:14. ~color:muted (rect 0. 0. vw 0.) 44.
    (Assets.text (if chapter = Campaign.Night then "campaign.night" else "campaign.day"));
  (if finished then
     let epi = Assets.text "campaign.epilogue" in
     let ew = paper_btn_w ~size:16. epi in
     pbutton ~size:16.
       (rect ((vw -. ew) /. 2.) (vh -. 48.) ew 28.)
       epi
       (fun () -> camp.pending_epilogue <- true));
  run_dialog "campaign" (camp.dlg <> Camp_none) (fun () ->
      match camp.dlg with
      | Camp_none -> ()
      | Camp_note n ->
          parchment_card ~title:n.title ~portrait:n.portrait ~subtitle:"" ~message:n.body
            [
              ( n.button,
                fun () ->
                  camp.dlg <- Camp_none;
                  n.ack () );
            ]
      | Camp_restart ->
          let night = chapter = Campaign.Night in
          let buttons =
            (if night then
               [
                 ( Assets.text "campaign.restart.all",
                   fun () ->
                     Prefs.reset_campaign (Prefs.load ());
                     camp.dlg <- Camp_none;
                     camp.night_target <- 0. );
                 ( Assets.text "campaign.restart.night",
                   fun () ->
                     Prefs.reset_night_campaign (Prefs.load ());
                     camp.dlg <- Camp_none;
                     camp.night_target <- 1. );
               ]
             else
               [
                 ( Assets.text "button.yes",
                   fun () ->
                     Prefs.reset_campaign (Prefs.load ());
                     camp.dlg <- Camp_none;
                     camp.night_target <- 0. );
               ])
            @ [ (Assets.text "button.cancel", fun () -> camp.dlg <- Camp_none) ]
          in
          parchment_card ~title:(Assets.text "campaign.restart")
            ~portrait:None ~subtitle:""
            ~message:(Assets.text (if night then "campaign.restart.choice" else "campaign.restart.confirm"))
            buttons
      | Camp_fight n -> (
          match n.fight with
          | None -> camp.dlg <- Camp_none
          | Some f ->
              parchment_card
                ~title:(Assets.text ("campaign." ^ n.id ^ ".name"))
                ~portrait:(Some f.portrait_id) ~subtitle:(mage_line n)
                ~message:(Assets.text ("campaign." ^ n.id ^ ".message"))
                (( Assets.text "button.fight",
                   fun () ->
                     camp.dlg <- Camp_none;
                     camp.pending_fight <- Some n )
                 ::
                 (if debug_mode () then
                    [
                      ( Assets.text "button.debugwin",
                        fun () ->
                          Prefs.complete_campaign (Prefs.load ()) n.id;
                          camp.dlg <- Camp_none;
                          let completed = Prefs.campaign_completed (Prefs.load ()) in
                          if Campaign.is_finished completed then camp.pending_epilogue <- true
                          else if Campaign.chapter_of completed = Campaign.Night then camp.night_target <- 1.
                      );
                    ]
                  else [])
                @ [ (Assets.text "button.cancel", fun () -> camp.dlg <- Camp_none) ]))
      | Camp_meeting n -> (
          match n.meeting with
          | None -> camp.dlg <- Camp_none
          | Some m ->
              let choices =
                match m.choices with
                | [] ->
                    [
                      ( Assets.text "campaign.continue",
                        fun () ->
                          Prefs.complete_campaign (Prefs.load ()) n.id;
                          camp.dlg <- Camp_none );
                    ]
                | cs ->
                    List.map
                      (fun (c : Campaign.choice) ->
                        ( Assets.text ("campaign." ^ n.id ^ ".choice." ^ c.id),
                          fun () ->
                            Prefs.complete_campaign (Prefs.load ()) ~extras:[ c.id ] n.id;
                            camp.dlg <- Camp_none ))
                      cs
              in
              parchment_card
                ~title:(Assets.text ("campaign." ^ n.id ^ ".name"))
                ~portrait:(Some m.portrait_id)
                ~subtitle:(Assets.text ("campaign." ^ n.id ^ ".name"))
                ~message:(Assets.text ("campaign." ^ n.id ^ ".message"))
                choices)
      | Camp_end { node; won } ->
          draw_campaign_end ~final:false ~node ~won
            ~replay:(fun () ->
              camp.dlg <- Camp_none;
              camp.pending_fight <- Some node)
            ~next:(fun () -> camp.dlg <- Camp_none));
  run_dialog "settings" camp.show_settings (fun () ->
      draw_game_settings camp.gs ~show_seed:false ~on_budget:ignore ~close:(fun () ->
          camp.show_settings <- false;
          camp.gs.seed_focus <- false))

let draw_epilogue ~menu =
  (match texture (Assets.campaign "epilogue") with Some t -> cover t | None -> fill (rect 0. 0. vw vh) bg);
  let line_h = 26. in
  let lines = wrap ~font:paper_font 20. 520. (Assets.text "campaign.epilogue") in
  let nlines = max 1 (List.length lines) in
  let box_w = 592. in
  let box_h = Float.min (vh -. 32.) (24. +. (float nlines *. line_h) +. 16. +. 48.) in
  let box = rect ((vw -. box_w) /. 2.) (vh -. box_h -. 32.) box_w box_h in
  paper box;
  List.iteri
    (fun i l -> text_c ~font:paper_font ~size:20. ~color:ink box (box.y +. 24. +. (float i *. line_h)) l)
    lines;
  paper_btn_row ~size:16.
    ~cx:(box.x +. (box.w /. 2.))
    ~y:(box.y +. box.h -. 48.)
    [ (Assets.text "campaign.epilogue.menu", menu) ]
