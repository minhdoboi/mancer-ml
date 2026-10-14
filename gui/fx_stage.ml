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
        { Spell_looks.style = Spell_looks.Ring; aim = Spell_looks.Own_slots; r = 0.9; g = 0.9; b = 0.95; power = 1. }
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
  | "wind.BallLightning.name" ->
      Some
        (recipe ~duration:ball_duration ~blocking:true (fun ~start ->
             spawn_ball_lightning ~from_p:source ~to_p:foe_life ~start))
  | "fire.wave" ->
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
      Some (recipe ~duration:nature_care_duration ~blocking:true (fun ~start -> spawn_nature_care spots ~start))
  | "earth.therapy" ->
      let spots = [ layout.life player ] in
      Some (recipe ~duration:pollen_duration ~blocking:true (fun ~start -> spawn_pollen spots ~start))
  | "faerie.fire" ->
      let floors = slots_of other layout.slot_floor in
      Some (recipe ~duration:1.9 ~blocking:true (fun ~start -> spawn_faerie_fire ~floors ~start))
  | "water.shower" ->
      let floors = Array.append (slots_of player layout.slot_floor) (slots_of other layout.slot_floor) in
      Some (recipe ~duration:2.4 ~blocking:true (fun ~start -> spawn_acid_rain floors ~start))
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
      let everything = slots_of player layout.slot_center |> Array.to_list |> fun l -> l @ Array.to_list (slots_of other layout.slot_center) in
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
      Some (recipe ~duration:1.75 ~blocking:true (fun ~start -> spawn_scatterbomb ~pid:other ~src:source ~bases ~start))
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
      Some
        (recipe ~duration:1.8 ~blocking:true (fun ~start -> spawn_solar_wave ~own ~foe ~foe_life ~start))
  | "seasons.EternalSummer.name" ->
      let own = slots_of player layout.slot_center and foe = slots_of other layout.slot_center in
      let hand = source in
      Some
        (recipe ~duration:2.2 ~blocking:true (fun ~start -> spawn_eternal_summer ~own ~foe ~hand ~start))
  | "kinetician.warp" ->
      Option.map (fun t -> recipe ~duration:1.6 ~blocking:true (fun ~start -> spawn_warp_reality ~target:t ~start)) target
  | "kinetician.barrier" ->
      Option.map (fun t -> recipe ~duration:2.0 ~blocking:true (fun ~start -> spawn_force_barrier ~target:t ~start)) target
  | "conjurer.pain" ->
      Option.map
        (fun t -> recipe ~duration:2.0 ~blocking:true (fun ~start -> spawn_pain_of_essence ~target:t ~foe_life ~start))
        target
  | "conjurer.bravery" ->
      let spots = Array.of_list (List.map (layout.slot_center player) (layout.occupied player)) in
      Some (recipe ~duration:1.5 ~blocking:true (fun ~start -> spawn_bravery ~spots ~start))
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
  | "wind.EnergeticVortex.name" ->
      let foe_air = layout.house other 2 and own_air = layout.house player 2 in
      let life = layout.life player in
      Some
        (recipe ~duration:2.2 ~blocking:true (fun ~start -> spawn_energetic_vortex ~foe_air ~own_air ~life ~start))
  | "wind.EternalStorm.name" ->
      let slots = Array.append (slots_of player layout.slot_center) (slots_of other layout.slot_center) in
      Some (recipe ~duration:1.3 ~blocking:true (fun ~start -> spawn_eternal_storm ~slots ~start))
  | "battlemage.warcry" ->
      let foe = slots_of other layout.slot_center in
      Some
        (recipe ~duration:1.25 ~blocking:true (fun ~start ->
             spawn_warcry ~source ~foe ~foe_life ~foe_pid:other ~start))
  | _ -> Option.map (look_recipe layout c) (Spell_looks.get c.cmd_card.key)

let for_ability layout ~kind ~player ~num ~target =
  match kind with
  | "zen.spark" ->
      Option.map
        (fun (tp, tn) ->
          recipe ~duration:(lightning_duration 2) ~blocking:true (fun ~start ->
              spawn_lightning ~style:spark [ layout.slot_center player num; layout.slot_center tp tn ] ~start))
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
        (spawn_equilibrium
           ~opp_house:(layout.house opponent opp_hi)
           ~opp_life:foe_life
           ~own_house:(layout.house owner own_lo)
           ~own_life ~start:now)
  | _ -> ()
