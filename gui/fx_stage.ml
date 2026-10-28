(* Spell and ability visuals: stage helpers and the spell/ability tables. *)

open Mancer
open Sp
module R = Raylib
open Fx_core
open Fx_lightning
open Fx_particles
open Fx_flames
open Fx_dedicated
open Fx_scenes

(* ---------------------------------------------------------------- stage helpers *)

let own_foe layout player = (slots_of player layout.slot_center, slots_of (other player) layout.slot_center)

let oppress_foe layout player =
  let foe = other player in
  match layout.scarecrow player with
  | None -> slots_of foe layout.slot_center
  | Some n ->
      List.filter in_slot_range [ n - 1; n; n + 1 ] |> Array.of_list |> Array.map (layout.slot_center foe)

let source_or_life layout (c : command) =
  Option.value (layout.source_of c) ~default:(layout.life c.cmd_player)

let target_spot layout (c : command) =
  match (c.cmd_input, Table.target_side c) with
  | Some n, Some pid -> Some (layout.slot_center pid n)
  | _ -> None

(* The creature standing opposite a lane: that lane on the other side, or the
   nearest one with a creature in it, so a shot aimed at a lane reads as a shot
   at something.  An empty board still takes the lane itself. *)
let opposite_spot layout (c : command) lane =
  let foe = other (Option.value (Table.target_side c) ~default:c.cmd_player) in
  if Option.is_some (layout.slot_card foe lane) then layout.slot_center foe lane
  else
    match List.sort (fun a b -> compare (abs (a - lane)) (abs (b - lane))) (layout.occupied foe) with
    | [] -> layout.slot_center foe lane
    | n :: _ -> layout.slot_center foe n

let look_recipe layout (c : command) look =
  let own, foe0 = own_foe layout c.cmd_player in
  let foe = if c.cmd_card.key = "disciples.wind" then oppress_foe layout c.cmd_player else foe0 in
  let foe =
    if look.Spell_looks.style = Spell_looks.Spikes then slots_of (other c.cmd_player) layout.slot_floor
    else foe
  in
  let own, foe =
    if look.Spell_looks.style = Spell_looks.Stones then
      (slots_of c.cmd_player layout.slot_floor, slots_of (other c.cmd_player) layout.slot_floor)
    else (own, foe)
  in
  let own_life = layout.life c.cmd_player and foe_life = layout.life (other c.cmd_player) in
  let target = target_spot layout c in
  let duration = style_duration look in
  recipe ~duration ~blocking:true (fun ~start ->
      spawn_style look ~own ~foe ~own_life ~foe_life ~target ~start)

(* Seconds after a spell's effect starts that its sound should begin, for spells whose impact comes late. *)
let sound_delay key = match key with "NuclearMissile" -> missile_fall | _ -> 0.

let for_spell layout (c : command) =
  let player = c.cmd_player in
  let other = other player in
  let source = source_or_life layout c in
  let target = target_spot layout c in
  let foe_life = layout.life other in
  match c.cmd_card.key with
  | "colors.Discoloration.name" ->
      let own =
        layout.occupied player
        |> List.filter (fun n ->
            match layout.slot_card player n with
            | Some k -> String.starts_with ~prefix:"colors." k
            | None -> false)
        |> List.map (layout.slot_center player)
        |> Array.of_list
      in
      let look =
        {
          Spell_looks.style = Spell_looks.Ring;
          aim = Spell_looks.Own_slots;
          r = 0.9;
          g = 0.9;
          b = 0.95;
          power = 1.;
        }
      in
      let own_life = layout.life player and foe_life = layout.life other in
      Some
        (recipe ~duration:(style_duration look) ~blocking:true (fun ~start ->
             spawn_style look ~own ~foe:[||] ~own_life ~foe_life ~target:None ~start))
  | "air.call" ->
      Option.map
        (fun t ->
          let pts = [ source; t; foe_life ] in
          recipe ~duration:(lightning_duration 3) ~blocking:true (fun ~start -> spawn_lightning pts ~start))
        target
  | "air.lightnin" ->
      Some
        (recipe ~duration:(lightning_duration 2) ~blocking:true (fun ~start ->
             spawn_lightning [ source; foe_life ] ~start))
  | "air.twister" ->
      Option.bind c.cmd_input (fun n ->
          Option.map
            (fun pid ->
              let floor = layout.slot_floor pid n in
              recipe ~duration:1.7 ~blocking:true (fun ~start -> spawn_twister ~floor ~start))
            (Table.target_side c))
  | "air.chain" ->
      let pts =
        List.rev
          (foe_life
          :: List.init nb_slots (fun i ->
              let p = layout.slot_center other i in
              { p with y = p.y +. (20. *. float (i mod 2)) }))
      in
      Some
        (recipe
           ~duration:(lightning_duration (List.length pts))
           ~blocking:true
           (fun ~start -> spawn_lightning pts ~start))
  | "fear.strike" ->
      Option.map
        (fun t ->
          let pts = [ source; t; foe_life ] in
          recipe ~duration:(lightning_duration 3) ~blocking:true (fun ~start ->
              spawn_lightning ~style:fear_bolt pts ~start))
        target
  | "fear.spirit" ->
      Some
        (recipe ~duration:(lightning_duration 2) ~blocking:true (fun ~start ->
             spawn_lightning [ source; foe_life ] ~start))
  | "fear.fear" ->
      Option.bind c.cmd_input (fun n ->
          Option.map
            (fun pid ->
              let floor = layout.slot_floor pid n in
              recipe ~duration:1.7 ~blocking:true (fun ~start -> spawn_twister ~floor ~start))
            (Table.target_side c))
  | "fear.invasion" ->
      (* ghosts pour in from the right and sweep left onto every creature the
         opponent owns, and onto the opponent itself *)
      let row = slots_of other layout.slot_center in
      let targets =
        Array.of_list (List.map (layout.slot_center other) (layout.occupied other) @ [ foe_life ])
      in
      Some
        (recipe ~duration:ghost_invasion_duration ~blocking:true (fun ~start ->
             spawn_ghost_invasion ~row ~targets ~start))
  | "wind.BallLightning.name" ->
      Some
        (recipe ~duration:ball_duration ~blocking:true (fun ~start ->
             spawn_ball_lightning ~from_p:source ~to_p:foe_life ~start))
  | "fire.wave" ->
      Some
        (recipe ~duration:1.5 ~blocking:true (fun ~start ->
             spawn_flame_wave (slots_of other layout.slot_floor) ~start))
  | "anger.wave" ->
      Some
        (recipe ~duration:1.5 ~blocking:true (fun ~start ->
             spawn_flame_wave (slots_of other layout.slot_floor) ~start))
  | "NuclearMissile" ->
      let bases = slots_of other layout.slot_floor in
      Some (recipe ~duration:1.7 ~blocking:true (fun ~start -> spawn_missiles ~pid:other ~bases ~start))
  | "fire.inferno" ->
      Option.map
        (fun n ->
          let bases = slots_of other layout.slot_floor in
          let others = Array.init (nb_slots - 1) (fun i -> bases.(if i < n then i else i + 1)) in
          recipe ~duration:1.7 ~blocking:true (fun ~start -> spawn_inferno ~target:bases.(n) ~others ~start))
        c.cmd_input
  | "anger.inferno" ->
      Option.map
        (fun n ->
          let bases = slots_of other layout.slot_floor in
          let others = Array.init (nb_slots - 1) (fun i -> bases.(if i < n then i else i + 1)) in
          recipe ~duration:1.7 ~blocking:true (fun ~start -> spawn_inferno ~target:bases.(n) ~others ~start))
        c.cmd_input
  | "disciples.madden" ->
      let foe_occ = layout.occupied other in
      let foe_row = slots_of other layout.slot_center in
      let own_row = slots_of player layout.slot_center in
      let eye =
        {
          x = Array.fold_left (fun s p -> s +. p.x) 0. foe_row /. float nb_slots;
          y = (foe_row.(0).y +. own_row.(0).y) *. 0.5;
        }
      in
      let nums = if foe_occ = [] then List.init nb_slots Fun.id else foe_occ in
      let foe_ids = List.map (fun n -> (other, n, layout.slot_center other n)) nums in
      let ally_ids = List.map (fun n -> (player, n, layout.slot_center player n)) (layout.occupied player) in
      Some
        (recipe ~duration:madden_duration ~blocking:true (fun ~start ->
             spawn_madden ~foes:foe_ids ~allies:ally_ids ~eye ~start))
  | "earth.care" ->
      let life = layout.life player in
      let spots = match target with Some t -> [ t; life ] | None -> [ life ] in
      Some
        (recipe ~duration:nature_care_duration ~blocking:true (fun ~start -> spawn_nature_care spots ~start))
  | "earth.therapy" ->
      let spots = [ layout.life player ] in
      Some (recipe ~duration:pollen_duration ~blocking:true (fun ~start -> spawn_pollen spots ~start))
  | "joy.care" ->
      let life = layout.life player in
      let spots = match target with Some t -> [ t; life ] | None -> [ life ] in
      Some
        (recipe ~duration:nature_care_duration ~blocking:true (fun ~start -> spawn_nature_care spots ~start))
  | "joy.bond" ->
      let spots = [ layout.life player ] in
      (* Sexual Bond: the same therapy motes, blushed pink *)
      Some (recipe ~duration:pollen_duration ~blocking:true (fun ~start -> spawn_pollen ~pink:true spots ~start))
  | "joy.burst" ->
      (* Solar Burst: a curtain of solar light rolls across the board and every
         slot it touches is left burning *)
      let floors = Array.append (slots_of player layout.slot_floor) (slots_of other layout.slot_floor) in
      Some (recipe ~duration:solar_burst_duration ~blocking:true (fun ~start -> spawn_solar_burst ~floors ~start))
  | "joy.fusion" ->
      (* Fusion: the caster's two strongest creatures pour their joy into one
         strike at the opponent -- an arc of pink, yellow and red from each. *)
      let strongest =
        layout.occupied player
        |> List.filter_map (fun n ->
               match layout.slot_attack player n with
               | Some a -> Some (a, layout.slot_center player n)
               | None -> None)
        |> List.sort (fun (a, _) (b, _) -> compare b a)
        |> List.map snd
      in
      let froms = Array.of_list (List.filteri (fun i _ -> i < 2) strongest) in
      if Array.length froms = 0 then None
      else
        Some
          (recipe ~duration:fusion_duration ~blocking:true (fun ~start ->
               spawn_fusion ~froms ~to_:foe_life ~start))
  | "faerie.fire" ->
      let floors = slots_of other layout.slot_floor in
      Some (recipe ~duration:1.9 ~blocking:true (fun ~start -> spawn_faerie_fire ~floors ~start))
  | "water.shower" ->
      let floors = Array.append (slots_of player layout.slot_floor) (slots_of other layout.slot_floor) in
      Some (recipe ~duration:2.4 ~blocking:true (fun ~start -> spawn_acid_rain floors ~start))
  | "sadness.tears" ->
      let floors = Array.append (slots_of player layout.slot_floor) (slots_of other layout.slot_floor) in
      (* the same acidic downpour, but clean water-blue instead of toxic green *)
      Some (recipe ~duration:2.4 ~blocking:true (fun ~start -> spawn_acid_rain ~blue:true floors ~start))
  | "elementalist.frost" ->
      let mid = match target_spot layout c with Some t -> t | None -> layout.slot_center other 2 in
      Some
        (recipe ~duration:(lightning_duration 3) ~blocking:true (fun ~start ->
             spawn_lightning ~style:frost [ source; mid; foe_life ] ~start))
  | "darkpriest.BlackMass.name" ->
      let origin = match target_spot layout c with Some t -> t | None -> source in
      let corner = layout.slot_center other 0 in
      Some
        (recipe ~duration:1. ~blocking:true (fun ~start ->
             spawn_black_mass ~source:origin ~target:corner ~w:500. ~h:100. ~start))
  | "kinetician.call" ->
      Option.bind c.cmd_input (layout.slot_house player)
      |> Option.map (fun idx ->
          let cards = Array.of_list (layout.house_cards player idx) in
          recipe ~duration:1.5 ~blocking:true (fun ~start -> spawn_heal_haze cards ~start))
  | "elementalist.incineration" ->
      (* only the weakest friendly and the strongest enemy creature burn; ties keep the first slot *)
      let pick pid better =
        List.fold_left
          (fun acc n ->
            match (layout.slot_life pid n, acc) with
            | Some l, None -> Some (n, l)
            | Some l, Some (_, bl) when better l bl -> Some (n, l)
            | _ -> acc)
          None (layout.occupied pid)
        |> Option.map (fun (n, _) -> layout.slot_center pid n)
      in
      let spots pid better = Option.to_list (pick pid better) |> Array.of_list in
      let own = spots player ( < ) and foe = spots other ( > ) in
      let look =
        {
          Spell_looks.style = Spell_looks.Fireburst;
          aim = Spell_looks.All_slots;
          r = 1.;
          g = 0.28;
          b = 0.02;
          power = 1.;
        }
      in
      let own_life = layout.life player and foe_life = layout.life other in
      Some
        (recipe ~duration:(style_duration look) ~blocking:true (fun ~start ->
             spawn_style look ~own ~foe ~own_life ~foe_life ~target:None ~start))
  | "elementalist.avalanche" ->
      let floors = slots_of other layout.slot_floor in
      let rock_look =
        {
          Spell_looks.style = Spell_looks.Stones;
          aim = Spell_looks.Foe_slots;
          r = 0.4;
          g = 0.36;
          b = 0.3;
          power = 1.15;
        }
      in
      let base = look_recipe layout c rock_look in
      Some
        (recipe ~duration:base.duration ~blocking:true (fun ~start ->
             let rocks = base.make ~start in
             mk ~start ~duration:base.duration (fun ~age ->
                 rocks.draw ~age;
                 let fade = Float.min 1. ((base.duration -. age) /. 0.2) in
                 if fade > 0.02 then snow_fall ~floors ~age ~fade)))
  | "sower.tangling" ->
      Option.map
        (fun n ->
          let from_ = layout.slot_center other n and to_ = layout.slot_center player n in
          recipe ~duration:1.0 ~blocking:true (fun ~start -> spawn_tangling ~from_ ~to_ ~start))
        c.cmd_input
  | "vampires.ties" ->
      Option.map
        (fun n ->
          let targets =
            List.filter (fun i -> in_slot_range i && List.mem i (layout.occupied player)) [ n - 1; n + 1 ]
            |> List.map (layout.slot_center player)
            |> Array.of_list
          in
          let from_ = layout.slot_center player n in
          recipe ~duration:1.1 ~blocking:true (fun ~start -> spawn_blood_ties ~from_ ~targets ~start))
        c.cmd_input
  | "dreamweaver.aurora" ->
      let occ pid = layout.occupied pid |> List.map (layout.slot_center pid) |> Array.of_list in
      let foe = occ other and own = occ player in
      let bounds = Array.append (slots_of other layout.slot_center) (slots_of player layout.slot_center) in
      Some (recipe ~duration:2.4 ~blocking:true (fun ~start -> spawn_aurora ~bounds ~foe ~own ~start))
  | "powermancer.bargain" ->
      let cards =
        List.concat_map (fun idx -> layout.house_cards player idx) [ 0; 1; 2; 3; 4; 5 ] |> Array.of_list
      in
      Some (recipe ~duration:1.75 ~blocking:true (fun ~start -> spawn_page_fire ~cards ~start))
  | "powermancer.burial" ->
      Option.map
        (fun at -> recipe ~duration:2.0 ~blocking:true (fun ~start -> spawn_burial ~at ~start))
        target
  | "powermancer.equilibrium" ->
      let opp_hi = layout.extreme_house other `Highest in
      let own_lo = layout.extreme_house player `Lowest in
      let opp_house = layout.house other opp_hi in
      let own_house = layout.house player own_lo in
      let opp_life = layout.life other in
      let own_life = layout.life player in
      Some
        (recipe ~duration:1.4 ~blocking:true (fun ~start ->
             spawn_equilibrium ~opp_house ~opp_life ~own_house ~own_life ~start))
  | "fire.apocalypse" ->
      let spots pid = layout.occupied pid |> List.map (layout.slot_center pid) in
      let everything =
        slots_of player layout.slot_center |> Array.to_list |> fun l ->
        l @ Array.to_list (slots_of other layout.slot_center)
      in
      let n = float (List.length everything) in
      let sum f = List.fold_left (fun a (p : spot) -> a +. f p) 0. everything in
      let center = { x = sum (fun p -> p.x) /. n; y = sum (fun p -> p.y) /. n } in
      let targets = Array.of_list (spots player @ spots other @ [ layout.life other ]) in
      Some (recipe ~duration:2.6 ~blocking:true (fun ~start -> spawn_apocalypse ~center ~targets ~start))
  | "anger.apocalypse" ->
      let spots pid = layout.occupied pid |> List.map (layout.slot_center pid) in
      let everything =
        slots_of player layout.slot_center |> Array.to_list |> fun l ->
        l @ Array.to_list (slots_of other layout.slot_center)
      in
      let n = float (List.length everything) in
      let sum f = List.fold_left (fun a (p : spot) -> a +. f p) 0. everything in
      let center = { x = sum (fun p -> p.x) /. n; y = sum (fun p -> p.y) /. n } in
      let targets = Array.of_list (spots player @ spots other @ [ layout.life other ]) in
      Some (recipe ~duration:2.6 ~blocking:true (fun ~start -> spawn_apocalypse ~center ~targets ~start))
  | "shaman.moon" ->
      (* only the ghost wolf and its two neighbours glow *)
      let wolf =
        List.find_opt (fun n -> layout.slot_card player n = Some "shaman.ghost") (layout.occupied player)
      in
      let own =
        match wolf with
        | None -> slots_of player layout.slot_center
        | Some w ->
            List.filter (fun i -> in_slot_range i && List.mem i (layout.occupied player)) [ w - 1; w; w + 1 ]
            |> List.map (layout.slot_center player)
            |> Array.of_list
      in
      let look =
        {
          Spell_looks.style = Spell_looks.Rays;
          aim = Spell_looks.Own_slots;
          r = 0.75;
          g = 0.8;
          b = 1.;
          power = 1.;
        }
      in
      let own_life = layout.life player and foe_life = layout.life other in
      Some
        (recipe ~duration:(style_duration look) ~blocking:true (fun ~start ->
             spawn_style look ~own ~foe:[||] ~own_life ~foe_life ~target:None ~start))
  | "shaman.fury" ->
      let foe = slots_of other layout.slot_center in
      Some (recipe ~duration:1.2 ~blocking:true (fun ~start -> spawn_claws ~foe ~start))
  | "shadowcaster.wave" ->
      let floors = slots_of other layout.slot_floor in
      Some (recipe ~duration:1.2 ~blocking:true (fun ~start -> spawn_shadow_wave ~floors ~start))
  | "shadowcaster.scythe" ->
      let foe = slots_of other layout.slot_center in
      Some (recipe ~duration:1.15 ~blocking:true (fun ~start -> spawn_scythe ~foe ~foe_life ~start))
  | "junk.poison" ->
      (* Scala SpellCast: bloom in the lane, smoke on the owner and occupied opposite neighbours. *)
      Option.map
        (fun n ->
          let own = layout.slot_center player n in
          let facing = layout.slot_center other n in
          let extras =
            if List.mem n (layout.occupied other) then
              List.filter in_slot_range [ n - 1; n; n + 1 ]
              |> List.filter (fun i -> List.mem i (layout.occupied other))
              |> List.map (layout.slot_center other)
            else []
          in
          recipe ~duration:1.2 ~blocking:true (fun ~start ->
              spawn_poison ~own ~facing ~smoke:(Array.of_list (own :: extras)) ~start))
        c.cmd_input
  | "tinker.scatter" ->
      let bases =
        let nums = match layout.occupied other with [] -> base_slot_list | ns -> ns in
        Array.of_list (List.map (fun n -> (n, layout.slot_floor other n)) nums)
      in
      Some
        (recipe ~duration:1.75 ~blocking:true (fun ~start ->
             spawn_scatterbomb ~pid:other ~src:source ~bases ~start))
  | "rogue.trap" ->
      let houses = Array.init 5 (layout.house other) in
      let center = fst (Fx_core.trap_center_of layout other) in
      Some (recipe ~duration:2.6 ~blocking:true (fun ~start -> spawn_arcane_trap ~houses ~center ~start))
  | "wind.Simoom.name" ->
      Option.map
        (fun t -> recipe ~duration:1.5 ~blocking:true (fun ~start -> spawn_simoom ~target:t ~start))
        target
  | "wind.Whirlwind.name" ->
      (* Whirlwind destroys the chosen creature and the one facing it: one gale joining the two slots. *)
      Option.map
        (fun n ->
          let a = layout.slot_center player n and b = layout.slot_center other n in
          recipe ~duration:1.4 ~blocking:true (fun ~start -> spawn_whirlwind ~a ~b ~start))
        c.cmd_input
  | "seasons.SolarWave.name" ->
      let own = slots_of player layout.slot_center and foe = slots_of other layout.slot_center in
      Some (recipe ~duration:1.8 ~blocking:true (fun ~start -> spawn_solar_wave ~own ~foe ~foe_life ~start))
  | "seasons.EternalSummer.name" ->
      let own = slots_of player layout.slot_center and foe = slots_of other layout.slot_center in
      let hand = source in
      Some (recipe ~duration:2.2 ~blocking:true (fun ~start -> spawn_eternal_summer ~own ~foe ~hand ~start))
  | "kinetician.warp" ->
      Option.map
        (fun t -> recipe ~duration:1.6 ~blocking:true (fun ~start -> spawn_warp_reality ~target:t ~start))
        target
  | "kinetician.barrier" ->
      Option.map
        (fun t -> recipe ~duration:2.0 ~blocking:true (fun ~start -> spawn_force_barrier ~target:t ~start))
        target
  | "conjurer.pain" ->
      Option.map
        (fun t ->
          recipe ~duration:2.0 ~blocking:true (fun ~start -> spawn_pain_of_essence ~target:t ~foe_life ~start))
        target
  | "conjurer.bravery" ->
      let spots = Array.of_list (List.map (layout.slot_center player) (layout.occupied player)) in
      Some (recipe ~duration:1.5 ~blocking:true (fun ~start -> spawn_bravery ~spots ~start))
  | "emotion.revolt" ->
      let from_points =
        layout.occupied other |> List.map (layout.slot_center other) |> Array.of_list
      in
      Some
        (recipe ~duration:1.15 ~blocking:true (fun ~start ->
             spawn_revolt ~from_points ~to_:foe_life ~start))
  | "emotion.loneliness" ->
      let lonely pid =
        layout.occupied pid
        |> List.filter (fun n -> layout.slot_card (Sp.other pid) n = None)
        |> List.map (layout.slot_center pid)
      in
      let spots = Array.of_list (lonely player @ lonely other) in
      Some
        (recipe ~duration:1.7 ~blocking:true (fun ~start -> spawn_loneliness ~spots ~start))
  | "conjurer.nectar" ->
      Option.map
        (fun t ->
          let house =
            match (c.cmd_input, Table.target_side c) with
            | Some n, Some tp -> Option.map (layout.house player) (layout.slot_house tp n)
            | _ -> None
          in
          recipe ~duration:1.8 ~blocking:true (fun ~start -> spawn_lavender_nectar ~target:t ~house ~start))
        target
  | "beastlord.poison" ->
      (* the dart flies from the card to the creature it targets, not to the portrait *)
      Option.map
        (fun t ->
          recipe ~duration:poison_dart_duration ~blocking:true (fun ~start ->
              spawn_poison_dart ~from_:source ~target:t ~start))
        target
  | "trappers.rose" ->
      (* the stems need both ends of the creature: the card they drink from and
         the ground they come up through *)
      Option.bind c.cmd_input (fun n ->
          Option.map
            (fun tp ->
              recipe ~duration:underworld_rose_duration ~blocking:true (fun ~start ->
                  spawn_underworld_rose ~source ~target:(layout.slot_center tp n) ~start))
            (Table.target_side c))
  | "trappers.undergroundpoison" ->
      (* the poison comes up under the creature it kills and runs out along the roots
         to every other creature on that side, bursting through each lane's ground *)
      Option.bind c.cmd_input (fun n ->
          Option.map
            (fun pid ->
              let foes =
                List.filter (fun m -> m <> n) (layout.occupied pid)
                |> List.map (fun m -> (layout.slot_floor pid m, layout.slot_center pid m))
                |> Array.of_list
              in
              recipe ~duration:underground_poison_duration ~blocking:true (fun ~start ->
                  spawn_underground_poison ~target:(layout.slot_center pid n) ~floor:(layout.slot_floor pid n)
                    ~foes ~start))
            (Table.target_side c))
  | "trappers.roots" ->
      (* the creatures on that side are bound, and green smoke puffs up from each one's floor *)
      let floors = Array.of_list (List.map (layout.slot_floor other) (layout.occupied other)) in
      if floors = [||] then None
      else Some (recipe ~duration:roots_smoke_duration ~blocking:true (fun ~start -> spawn_roots_smoke ~floors ~start))
  | "wind.EnergeticVortex.name" ->
      let foe_air = layout.house other 2 and own_air = layout.house player 2 in
      let life = layout.life player in
      Some
        (recipe ~duration:2.2 ~blocking:true (fun ~start ->
             spawn_energetic_vortex ~foe_air ~own_air ~life ~start))
  | "wind.EternalStorm.name" ->
      let slots = Array.append (slots_of player layout.slot_center) (slots_of other layout.slot_center) in
      Some (recipe ~duration:1.3 ~blocking:true (fun ~start -> spawn_eternal_storm ~slots ~start))
  | "battlemage.warcry" ->
      let foe = slots_of other layout.slot_center in
      Some
        (recipe ~duration:1.25 ~blocking:true (fun ~start ->
             spawn_warcry ~source ~foe ~foe_life ~foe_pid:other ~start))
  | "elementalist.freeze" ->
      (* Deep Freeze touches the players, not the slots: icicles fall on both portraits. *)
      let look =
        {
          Spell_looks.style = Spell_looks.Ice;
          aim = Spell_looks.Own_life;
          r = 0.7;
          g = 0.9;
          b = 1.;
          power = 1.;
        }
      in
      let duration = style_duration look in
      let lives = [| layout.life player; layout.life other |] in
      Some
        (recipe ~duration ~blocking:true (fun ~start ->
             mk ~start ~duration (fun ~age ->
                 let fade = Float.min 1. ((duration -. age) /. 0.22) in
                 if fade > 0.02 then
                   Array.iter
                     (fun own_life ->
                       draw_style look ~own:[||] ~foe:[||] ~own_life ~foe_life:own_life ~target:None ~age fade)
                     lives)))
  | "warp.quake" ->
      let centers = slots_of other layout.slot_center in
      let floors = slots_of other layout.slot_floor in
      Some
        (recipe ~duration:1.9 ~blocking:true (fun ~start ->
             spawn_quake ~centers ~floors ~foe_pid:other ~start))
  | "limbo.nether" ->
      let row_mid pid =
        let r = slots_of pid layout.slot_center in
        let m = r.(Array.length r / 2) in
        { x = m.x; y = m.y }
      in
      let mine = row_mid player and theirs = row_mid other in
      (* the sphere hangs between the two rows *)
      let hub = { x = (mine.x +. theirs.x) /. 2.; y = (mine.y +. theirs.y) /. 2. } in
      let own_centers = Array.of_list (List.map (layout.slot_center player) (layout.occupied player)) in
      let foe_targets = Array.of_list (List.map (layout.slot_center other) (layout.occupied other)) in
      (* every creature the caster owns is given up, and each is worth 3 damage *)
      let souls = Array.length own_centers in
      Some
        (recipe ~duration:1.8 ~blocking:true (fun ~start ->
             spawn_nether_grasp ~own_centers ~foe_targets ~hub ~souls ~foe_pid:other ~start))
  | "limbo.redemption" ->
      let spot = match target with Some p -> p | None -> layout.slot_center player 0 in
      Some (recipe ~duration:1.6 ~blocking:true (fun ~start -> spawn_redemption ~target:spot ~start))
  | "paganist.offering" ->
      (* whatever the caster can give up is what feeds the stream; a bare board
         falls back to the caster itself, which is the only thing left to pay *)
      let own =
        let occ = layout.occupied player |> List.map (layout.slot_center player) |> Array.of_list in
        if Array.length occ = 0 then [| layout.life player |] else occ
      in
      Some
        (recipe ~duration:final_offering_duration ~blocking:true (fun ~start ->
             spawn_final_offering ~own ~foe_life ~start))
  | "paganist.contract" ->
      Option.map
        (fun t ->
          recipe ~duration:unholy_contract_duration ~blocking:true (fun ~start ->
              spawn_unholy_contract ~at:t ~foe_life ~start))
        target
  | "paganist.terrorize" ->
      (match (c.cmd_input, Table.target_side c) with
      | Some n, Some tp ->
          Some
            (recipe ~duration:terrorize_duration ~blocking:true (fun ~start ->
                 spawn_terrorize ~at:(layout.slot_center tp n) ~pid:tp ~num:n ~start))
      | _ -> None)
  | "paganist.obliterate" ->
      let foe = slots_of other layout.slot_center in
      Some
        (recipe ~duration:obliterate_duration ~blocking:true (fun ~start ->
             spawn_obliterate ~foe ~occupied:(layout.occupied other) ~foe_pid:other ~start))
  | "paganist.blood" ->
      (* the cut is all the spell shows; the flood follows as its own ability, and only if the caster survives *)
      Some
        (recipe ~duration:blood_cut_duration ~blocking:true (fun ~start ->
             spawn_blood_cut ~own_life:(layout.life player) ~start))
  | "blacksmith.mithrilbow" ->
      (* the wielder looses the arrow itself, and it lands on the creature
         standing in the opposite lane *)
      let lane = match c.cmd_input with Some n when in_slot_range n -> n | _ -> 4 in
      let wielder = Option.value (target_spot layout c) ~default:(layout.slot_center player lane) in
      Some
        (recipe ~duration:mithril_bow_duration ~blocking:true (fun ~start ->
             spawn_mithril_bow ~from_p:wielder ~to_p:(opposite_spot layout c lane) ~start))
  | "blacksmith.barguliangauntlet" ->
      (* the wielder's own fist punches, and the shock it throws runs the whole
         of the opponent's row *)
      let lane = match c.cmd_input with Some n when in_slot_range n -> n | _ -> 4 in
      let wielder = Option.value (target_spot layout c) ~default:(layout.slot_center player lane) in
      let foe = slots_of other layout.slot_center in
      Some
        (recipe ~duration:gauntlet_duration ~blocking:true (fun ~start ->
             spawn_gauntlet ~wielder ~foe ~foe_pid:other ~start))
  | "blacksmith.excalibur" ->
      (* the wielder is the wielder's lane: the sword is driven through them and
         comes out in the lane opposite, or into the opponent if nothing stands
         there *)
      let lane = match c.cmd_input with Some n when in_slot_range n -> n | _ -> 4 in
      let wielder = Option.value (target_spot layout c) ~default:(layout.slot_center player lane) in
      let foe_pid = Sp.other (Option.value (Table.target_side c) ~default:player) in
      let through =
        if layout.slot_card foe_pid lane = None then layout.life foe_pid
        else layout.slot_center foe_pid lane
      in
      Some
        (recipe ~duration:excalibur_duration ~blocking:true (fun ~start ->
             spawn_excalibur ~wielder ~through ~start))
  | _ -> Option.map (look_recipe layout c) (Spell_looks.get c.cmd_card.key)

(* Contextual mana: arcs from a creature to the house labels it moved. *)
let mana_arcs ~from_ ~links =
  recipe ~duration:mana_arc_duration ~blocking:false (fun ~start -> spawn_mana_arcs ~from_ ~links ~start)

let for_ability layout ~kind ~player ~num ~target =
  match kind with
  | "paganist.flood" ->
      (* only creatures take the flood, so only creatures get splashed *)
      let foe = other player in
      let foes = layout.occupied foe |> List.map (layout.slot_center foe) |> Array.of_list in
      Some
        (recipe ~duration:blood_flood_duration ~blocking:true (fun ~start ->
             spawn_blood_flood ~own_life:(layout.life player) ~foe:foes ~foe_life:(layout.life foe) ~start))
  | "zen.spark" ->
      Option.map
        (fun (tp, tn) ->
          recipe ~duration:(lightning_duration 2) ~blocking:true (fun ~start ->
              spawn_lightning ~style:spark [ layout.slot_center player num; layout.slot_center tp tn ] ~start))
        target
  | "zen.heal" ->
      (* Inertia hands its life to the creature it heals: green motes run from
         the stone to the healed card *)
      Option.map
        (fun (tp, tn) ->
          recipe ~duration:life_flow_duration ~blocking:false (fun ~start ->
              spawn_life_flow ~from_:(layout.slot_center player num) ~to_:(layout.slot_center tp tn) ~start))
        target
  | "phosphor_rays" | "phosphor.rays" | "quantumancer.phosphor" ->
      let foe = match target with Some (tp, _) -> tp | None -> other player in
      let hits =
        match layout.occupied foe with
        | [] -> slots_of foe layout.slot_center
        | ns -> Array.of_list (List.map (layout.slot_center foe) ns)
      in
      Some
        (recipe ~duration:0.7 ~blocking:true (fun ~start ->
             spawn_phosphor ~from_:(layout.slot_center player num) ~targets:hits ~start))
  | "emotion.coward" ->
      Some
        (recipe ~duration:0.58 ~blocking:true (fun ~start ->
             spawn_coward ~at:(layout.slot_center player num) ~pid:player ~num ~start))
  | "emotion.love" ->
      Some
        (recipe ~duration:0.62 ~blocking:false (fun ~start ->
             spawn_love_flash ~at:(layout.slot_center player num) ~start))
  | "emotion.hope" ->
      let allies =
        layout.occupied player
        |> List.filter (( <> ) num)
        |> List.map (layout.slot_center player)
        |> Array.of_list
      in
      let foes =
        layout.occupied (other player) |> List.map (layout.slot_center (other player)) |> Array.of_list
      in
      Some
        (recipe ~duration:1.55 ~blocking:false (fun ~start ->
             spawn_hope ~from_:(layout.slot_center player num) ~allies ~foes ~start))
  | "dark_flock_bats" | "vampires.flock" ->
      Option.map
        (fun (_, dest) ->
          recipe ~duration:1.02 ~blocking:false (fun ~start ->
              spawn_bats ~from_:(layout.slot_center player num) ~to_:(layout.slot_center player dest) ~start))
        target
  | "soul_taken" | "soul.taken" | "reaper.soul" ->
      let dead_p, slot = match target with Some t -> t | None -> (other player, num) in
      Some
        (recipe ~duration:0.95 ~blocking:false (fun ~start ->
             spawn_harvest ~from_:(layout.slot_center dead_p slot) ~to_:(layout.life player) ~souls:1 ~start))
  | "disciples.falconer" ->
      (* The Falconer sends its bird over the foe row and back: it stoops at
         every foe creature, which is exactly who the card hurts. *)
      let foe = other player in
      let foes =
        layout.occupied foe |> List.sort compare |> List.map (layout.slot_center foe) |> Array.of_list
      in
      let row = slots_of foe layout.slot_center in
      let foe_x = Array.fold_left (fun a s -> a +. s.x) 0. row /. float (Array.length row) in
      let foe_y = row.(0).y in
      let left = Array.fold_left (fun a s -> Float.min a s.x) row.(0).x row in
      let right = Array.fold_left (fun a s -> Float.max a s.x) row.(0).x row in
      let foe_span = (right -. left) /. 2. in
      Some
        (recipe ~duration:falcon_duration ~blocking:true (fun ~start ->
             spawn_falcon ~from_:(layout.slot_center player num) ~foe_x ~foe_y ~foe_span ~targets:foes
               ~start))
  | "meteomancer.storm" ->
      (* Astral Storm: arcs lash out of the storm card at every foe creature
         and the last one breaks on the foe themselves *)
      let foe = other player in
      let foes = layout.occupied foe |> List.map (layout.slot_center foe) |> Array.of_list in
      Some
        (recipe ~duration:astral_storm_duration ~blocking:true (fun ~start ->
             spawn_astral_storm
               ~from_:(layout.slot_center player num) ~targets:foes ~foe_life:(layout.life foe) ~start))
  | kind when String.starts_with ~prefix:"meteomancer.prism." kind ->
      (* Prism Mage: a ray from the foe's Called Element label is refracted by
         the mage onto every foe creature *)
      let called =
        match String.split_on_char '.' kind with
        | [ _; _; n ] -> Option.value (int_of_string_opt n) ~default:0
        | _ -> 0
      in
      let foe = other player in
      let targets = layout.occupied foe |> List.map (layout.slot_center foe) |> Array.of_list in
      Some
        (recipe ~duration:prism_duration ~blocking:true (fun ~start ->
             spawn_prism ~from_:(layout.house foe called) ~at:(layout.slot_center player num)
               ~targets ~called ~start))
  | "meteomancer.grenadier" ->
      (* Astral Grenadier: a single astral grenade is lobbed at the creature
         the foe just summoned *)
      Option.map
        (fun (tp, tn) ->
          recipe ~duration:grenade_duration ~blocking:true (fun ~start ->
              spawn_grenade ~src:(layout.slot_center player num) ~target:(layout.slot_center tp tn)
                ~pid:tp ~num:tn ~start))
        target
  | _ -> None

let demo_names =
  [
    "stones";
    "sprout";
    "eyes";
    "rise";
    "strike";
    "swirl";
    "ice";
    "shadow";
    "rays";
    "drops";
    "fireburst";
    "ring";
    "spikes";
    "lightning";
    "ball";
    "flames";
    "inferno";
    "missile";
    "heal";
    "faerie";
    "rain";
    "madden";
    "shockwave";
    "warcry";
    "bats";
    "harvest";
    "phosphor";
    "victory";
    "blackmass";
    "poison";
    "flux";
    "nether";
    "blood";
    "redemption";
    "frost";
    "drain";
    "firewall";
    "equilibrium";
    "revolt";
    "loneliness";
    "hope";
    "coward";
    "love";
    "offering";
    "contract";
    "terrorize";
    "obliterate";
    "riverofblood";
    "bow";
    "excalibur";
    "gauntlet";
    "rose";
    "poison_dart";
    "underground";
    "fusion";
    "ghosts";
    "healflow";
    "prism";
    "grenade";
    "astralstorm";
    "quake";
    "falcon";
  ]

let spawn_demo layout name =
  let now = R.get_time () in
  let own = slots_of owner layout.slot_center in
  let foe = slots_of opponent layout.slot_center in
  let own_life = layout.life owner and foe_life = layout.life opponent in
  let target = Some foe.(2) in
  let look style aim r g b power = { Spell_looks.style; aim; r; g; b; power } in
  let go e = spawn e in
  match name with
  | "stones" ->
      go
        (spawn_style
           (look Spell_looks.Stones Spell_looks.All_slots 0.4 0.36 0.3 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "sprout" ->
      go
        (spawn_style
           (look Spell_looks.Sprout Spell_looks.Own_life 0.2 0.75 0.2 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "eyes" ->
      go
        (spawn_style
           (look Spell_looks.Eyes Spell_looks.Own_life 1. 0.55 0.05 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "rise" ->
      go
        (spawn_style
           (look Spell_looks.Rise Spell_looks.Own_life 0.35 0.7 1. 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "strike" ->
      go
        (spawn_style
           (look Spell_looks.Strike Spell_looks.Foe_slots 0.7 0.15 0.9 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "swirl" ->
      go
        (spawn_style
           (look Spell_looks.Swirl Spell_looks.Foe_slots 0.75 0.9 1. 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "ice" ->
      go
        (spawn_style
           (look Spell_looks.Ice Spell_looks.All_slots 0.7 0.9 1. 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "shadow" ->
      go
        (spawn_style
           (look Spell_looks.Shadow Spell_looks.Target 0.45 0.1 0.7 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "rays" ->
      go
        (spawn_style
           (look Spell_looks.Rays Spell_looks.All_slots 0.55 0.95 0.7 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "drops" ->
      go
        (spawn_style
           (look Spell_looks.Drops Spell_looks.Target 0.6 0.75 0.2 1.3)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "fireburst" ->
      go
        (spawn_style
           (look Spell_looks.Fireburst Spell_looks.All_slots 1. 0.4 0.05 1.35)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "ring" ->
      go
        (spawn_style
           (look Spell_looks.Ring Spell_looks.Target 0.4 0.75 1. 1.)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "spikes" ->
      let foe = slots_of opponent layout.slot_floor in
      go
        (spawn_style
           (look Spell_looks.Spikes Spell_looks.Foe_slots 0.92 0.08 0.1 1.2)
           ~own ~foe ~own_life ~foe_life ~target ~start:now)
  | "lightning" -> go (spawn_lightning [ own_life; foe.(2); foe_life ] ~start:now)
  | "ball" -> go (spawn_lightning ~style:ball [ own_life; foe_life ] ~start:now)
  | "flames" -> go (spawn_flame_wave (slots_of opponent layout.slot_floor) ~start:now)
  | "inferno" ->
      let bases = slots_of opponent layout.slot_floor in
      go
        (spawn_inferno ~target:bases.(2)
           ~others:(Array.append (Array.sub bases 0 2) (Array.sub bases 3 3))
           ~start:now)
  | "missile" -> go (spawn_missiles ~pid:opponent ~bases:(slots_of opponent layout.slot_floor) ~start:now)
  | "heal" -> go (spawn_heal_motes [ own.(2); own_life ] ~start:now)
  | "faerie" -> go (spawn_row_particles faerie_fire foe ~start:now)
  | "rain" -> go (spawn_acid_rain (Array.append own foe) ~start:now)
  | "madden" ->
      go
        (spawn_madden
           ~foes:(List.init nb_slots (fun n -> (opponent, n, foe.(n))))
           ~allies:(List.init nb_slots (fun n -> (owner, n, own.(n))))
           ~eye:{ x = (foe.(2).x +. foe.(3).x) *. 0.5; y = (own.(0).y +. foe.(0).y) *. 0.5 }
           ~start:now)
  | "shockwave" -> go (spawn_shockwave foe ~start:now)
  | "warcry" -> go (spawn_warcry ~source:own_life ~foe ~foe_life ~foe_pid:opponent ~start:now)
  | "bats" -> go (spawn_bats ~from_:own.(1) ~to_:own.(4) ~start:now)
  | "harvest" -> go (spawn_harvest ~from_:foe.(2) ~to_:own_life ~souls:3 ~start:now)
  | "phosphor" -> go (spawn_phosphor ~from_:own.(3) ~targets:foe ~start:now)
  | "victory" -> victory layout ~winner:(Some owner) ~you:true ~start:now
  | "blackmass" -> go (spawn_black_mass ~source:own.(2) ~target:foe.(0) ~w:500. ~h:100. ~start:now)
  | "poison" -> go (spawn_poison ~own:own.(2) ~facing:foe.(2) ~smoke:[| own.(2); foe.(2) |] ~start:now)
  | "flux" -> go (spawn_green_flux ~from_:(layout.house owner 3) ~to_:own_life ~start:now)
  | "nether" -> go (spawn_particles nether_grasp ~x:own.(0).x ~y:own.(0).y ~start:now)
  | "blood" -> go (spawn_particles blood_burst ~x:foe.(2).x ~y:foe.(2).y ~start:now)
  | "redemption" -> go (spawn_particles redemption ~x:own.(2).x ~y:own.(2).y ~start:now)
  | "frost" -> go (spawn_lightning ~style:frost [ own_life; foe.(2); foe_life ] ~start:now)
  | "drain" -> go (spawn_lightning ~style:drain [ layout.house opponent 0; foe_life ] ~start:now)
  | "firewall" -> go (spawn_row_particles fire_wall foe ~start:now)
  | "equilibrium" ->
      let opp_hi = layout.extreme_house opponent `Highest in
      let own_lo = layout.extreme_house owner `Lowest in
      go
        (spawn_equilibrium ~opp_house:(layout.house opponent opp_hi) ~opp_life:foe_life
           ~own_house:(layout.house owner own_lo) ~own_life ~start:now)
  | "revolt" -> go (spawn_revolt ~from_points:foe ~to_:foe_life ~start:now)
  | "loneliness" ->
      go (spawn_loneliness ~spots:[| own.(0); own.(2); foe.(1); foe.(4) |] ~start:now)
  | "hope" ->
      go
        (spawn_hope ~from_:own.(2)
           ~allies:[| own.(0); own.(1); own.(3); own.(4); own.(5) |]
           ~foes:foe ~start:now)
  | "coward" -> go (spawn_coward ~at:own.(2) ~pid:owner ~num:2 ~start:now)
  | "love" -> go (spawn_love_flash ~at:own.(2) ~start:now)
  | "offering" ->
      go (spawn_final_offering ~own:[| own.(1); own.(2); own.(3) |] ~foe_life ~start:now)
  | "contract" -> go (spawn_unholy_contract ~at:foe.(2) ~foe_life ~start:now)
  | "terrorize" -> go (spawn_terrorize ~at:foe.(2) ~pid:opponent ~num:2 ~start:now)
  | "obliterate" ->
      go (spawn_obliterate ~foe ~occupied:base_slot_list ~foe_pid:opponent ~start:now)
  | "riverofblood" ->
      go (spawn_blood_cut ~own_life ~start:now);
      go (spawn_blood_flood ~own_life ~foe:[| foe.(1); foe.(2); foe.(3) |] ~foe_life ~start:(now +. blood_cut_duration))
  | "bow" -> go (spawn_mithril_bow ~from_p:own.(2) ~to_p:foe.(2) ~start:now)
  | "excalibur" -> go (spawn_excalibur ~wielder:own.(2) ~through:foe.(2) ~start:now)
  | "gauntlet" -> go (spawn_gauntlet ~wielder:own.(2) ~foe ~foe_pid:opponent ~start:now)
  | "rose" -> go (spawn_underworld_rose ~source:own.(2) ~target:foe.(2) ~start:now)
  | "poison_dart" -> go (spawn_poison_dart ~from_:own.(2) ~target:foe.(2) ~start:now)
  | "underground" ->
      let foes =
        Array.of_list
          (List.map (fun m -> (layout.slot_floor opponent m, layout.slot_center opponent m)) [ 0; 1; 3; 4; 5 ])
      in
      go (spawn_underground_poison ~target:foe.(2) ~floor:(layout.slot_floor opponent 2) ~foes ~start:now)
  | "fusion" ->
      go (spawn_fusion ~froms:[| own.(1); own.(3) |] ~to_:foe_life ~start:now)
  | "ghosts" ->
      let targets =
        Array.of_list
          (List.map (layout.slot_center opponent) (layout.occupied opponent) @ [ foe_life ])
      in
      go (spawn_ghost_invasion ~row:foe ~targets ~start:now)
  | "healflow" ->
      (* Zen's Inertia: the life runs from one card to the other as green motes *)
      go (spawn_life_flow ~from_:own.(1) ~to_:own.(4) ~start:now)
  | "prism" ->
      let targets = Array.of_list (List.map (layout.slot_center opponent) (layout.occupied opponent)) in
      go (spawn_prism ~from_:(layout.house opponent 0) ~at:own.(2) ~targets ~called:0 ~start:now)
  | "grenade" -> go (spawn_grenade ~src:own.(2) ~target:foe.(2) ~pid:opponent ~num:2 ~start:now)
  | "astralstorm" ->
      let targets = Array.of_list (List.map (layout.slot_center opponent) (layout.occupied opponent)) in
      go (spawn_astral_storm ~from_:own.(2) ~targets ~foe_life ~start:(now -. 0.9))
  | "quake" ->
      let centers = slots_of opponent layout.slot_center in
      let floors = slots_of opponent layout.slot_floor in
      go (spawn_quake ~centers ~floors ~foe_pid:opponent ~start:now)
  | "falcon" ->
      (* the Disciples' Falconer: its bird turns over the foe row and home *)
      let targets = Array.of_list (List.map (layout.slot_center opponent) (layout.occupied opponent)) in
      let row = slots_of opponent layout.slot_center in
      let foe_x = Array.fold_left (fun a s -> a +. s.x) 0. row /. float (Array.length row) in
      let foe_y = row.(0).y in
      let left = Array.fold_left (fun a s -> Float.min a s.x) row.(0).x row in
      let right = Array.fold_left (fun a s -> Float.max a s.x) row.(0).x row in
      let foe_span = (right -. left) /. 2. in
      go (spawn_falcon ~from_:own.(2) ~foe_x ~foe_y ~foe_span ~targets ~start:now)
  | _ -> ()
