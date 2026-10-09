# 1. Architecture

A map of the code: what sits on top of what, what the two halves of the game
model are, and what actually happens between a player clicking a card and the
board changing.

## 1. One library, six executables

`lib/` is a single wrapped dune library called `mancer`, exposed as `Mancer`
(`lib/dune:1-5`). `(include_subdirs unqualified)` means there are **no** dune
files under `lib/` — every `.ml` in the tree becomes one flat module. Wrapping
generates an alias file that re-exports all of them, which is why every
executable starts with `open Mancer` and then `open Sp`.

The raylib front end is the one exception: `gui/dune` declares a second,
**unwrapped** library `mancer_gui`, so `bin/gui.ml` opens `Gui_board` and
friends directly.

```text
bin/                 entry points, one file per executable
  main.ml            headless tournament driver
  tui.ml             terminal UI (notty)
  gui.ml             raylib window
  replay.ml          recorded game -> board snapshots
  compare_engines.ml OCaml vs Scala divergence check
  sync_assets.ml     rebuild assets/ from the Scala repo

gui/  (mancer_gui, unwrapped)
  gui_board.ml       the game screen, and the bot search domain
  gui_lobby.ml       portraits, duel and settings dialogs
  gui_campaign.ml    campaign map and story screens
  gui_draw.ml        the drawing kit
  fx*.ml             spell and ability effects

lib/  (mancer)
  core/     sp.ml engine.ml common.ml util.ml dice.ml
  houses/   basic/{fire,water,air,earth}.ml
            special/*.ml          (38 special houses)
  bots/     lecturer oracle diviner summoner strategist heuris
  game/     game session table shuffle catalog tournament campaign
            replay game_record prefs assets properties spell_looks
```

Dependencies only ever point downward:

```text
   bin/            gui/
     |               |
     +-------+-------+
             |
          Mancer  (lib/)
             |
   +---------+---------+-------------+
   |         |         |             |
 core/    game/     houses/         bots/
   |         |         |             |
   +---------+---------+-------------+
             |
       Sp (types only)
```

`houses/` and `bots/` both sit *on top of* `game/` and read the same types from
`Sp`. Houses never call bots; bots never call houses except to look up card
properties.

## 2. The two halves of the data model

This is the single most important structural idea in the codebase, and every
bot depends on understanding it. A game is described by **two** structures that
answer different questions:

| | **desc** — "what may be played?" | **state** — "what is on the board?" |
|---|---|---|
| types | `game_desc`, `player_desc`, `player_house_desc`, `card_desc` | `game_state`, `player_state`, `slot_state` |
| fields | `gd_`, `pd_`, `phd_`, `cd_` | `ps_` and bare names |
| lives in | `updater.desc`, fixed for the whole game | `updater.state`, mutated constantly |
| mutable | no | yes, all over |
| built by | `Game.make_player_desc`, `Shuffle` | `Game.initial_game_state` |
| answers | cost, enabled flag, extra mana, the list of cards in hand | life, mana, growth, the six slots, house data |
| a bot reads it to | enumerate legal moves | score a position |

The type declarations are `lib/core/sp.ml:248-283`:

```ocaml
and card_desc   = { cd_card : card; cd_cost : int; cd_enabled : bool; cd_extra_mana : int }
and desc_mod    = player_house_desc -> card_desc array -> card_desc array
and desc_reader = { init_desc : player_desc; mutable desc_mods : desc_mod list }

and player_house_desc = { phd_house : house; phd_cards : card_desc array }
and player_desc       = { pd_houses : player_house_desc array }
and game_desc         = { gd_players : player_desc array }

and player_state = {
  mutable ps_houses : house_state array;      (* mana + growth per house *)
  ps_desc : desc_reader;                      (* hand as it stands right now *)
  ps_slots : slot_state option array;         (* six slots *)
  mutable ps_slot_list : int list;            (* which slots are usable *)
  mutable ps_life : int;
  mutable ps_effects : (phase * card_effect) list;
  mutable ps_data : player_data;              (* per-house scratch space *)
  mutable ps_transitions : transition list;
}

and game_state = { players : player_state array }   (* always exactly two *)
```

Three consequences worth internalising:

**a) Cost is a property of the desc, never of the card.** A card object is
shared; `card_desc` is one player's view of it. That is how "this card costs 1
mana for you this turn" works — a `desc_mod` folds over the array
(`lib/game/game.ml:178-184`):

```ocaml
let player_houses pid (desc : game_desc) (state : game_state) =
  let d = desc.gd_players.(pid) in
  match state.players.(pid).ps_desc.desc_mods with
  | [] -> Array.to_list d.pd_houses
  | mods ->
      Array.to_list d.pd_houses
      |> List.map (fun h -> { h with phd_cards = List.fold_left (fun cards m -> m h cards) h.phd_cards mods })
```

A `desc_mod` is a *function value*, so mods are compared by physical equality
(`List.memq`, `x == m`), never with `=`.

**b) Playability is one predicate.** Everything funnels through
`lib/core/sp.ml:462`:

```ocaml
let is_available cd (h : house_state) = cd.cd_cost <= h.mana + cd.cd_extra_mana && cd.cd_enabled
```

and "can this player do anything at all" is `is_disabled` (`sp.ml:464`), which
is what makes every driver auto-pass without waking a bot.

**c) The board is a fixed six slots.** `nb_slots = 6` (`sp.ml:23`). Slot 3 is
slot 3 for both sides. `ps_slot_list` says which slots are *usable* (a house
can block one), and geometry is `adjacent`, `opposite` (`engine.ml:118-127`).

### The naming convention

Record fields that would collide across types carry a short prefix — `b_`
creature body, `cd_` card desc, `h_` house, `ps_` player state, `gd_` game
desc, `cmd_` command, `env_` effect environment. The single most central type
of each kind keeps bare names (`card`, `life`, `status` on `slot_state`).
Result: record literals never need a type annotation.

## 3. The update graph

`updater` (`lib/core/sp.ml:407-418`) is the live engine object. It owns the
`game_desc`, the current `game_state`, the two `player_update`s, the
end-of-game latch, the event listener, and the dice stream:

```ocaml
and updater = {
  mutable state : game_state;
  desc : game_desc;
  mutable player_updates : player_update array;
  mutable ended : player_id option;
  mutable listener : game_event -> unit;
  rng : Dice.t;
}
```

Each `player_update` owns a `slots_update`, which owns six `slot_update`s.
**The `slot_update.value` cells are what cards mutate.** `player_state` is a
snapshot that lags behind and is re-synced at the end of every step:

```text
   updater
     |
     +-- player_update (mutable hooks: submit_command, guard, set_data, ...)
     |     |
     |     +-- slots_update (log of board events)
     |     |     |
     |     |     +-- slot_update[0..5]  .value : slot_state option  <-- LIVE
     |     |     |
     |     |     +-- player_update.pstate.ps_slots                     <-- STALE COPY
     |     |
     |     +-- houses_update.hu_value : house_state array  <-- LIVE mana/growth
     |
     +-- state : game_state                              <-- the published snapshot
```

The sync point is `Player.result` (`lib/core/engine.ml:365`):

```ocaml
let result p = flush p; sync_houses p; refresh_slots p; p.pstate
```

and the engine step is `Updater.lift` (`engine.ml:712`), which wraps every
mutation:

```ocaml
let lift u f =
  u.ended <- None;
  Array.iter Player.sync_houses u.player_updates;
  let r = f u in
  let state = { players = Array.map Player.result u.player_updates } in
  u.state <- state;
  (r, state)
```

`Game.step u f = ignore (Updater.lift u f)` (`lib/game/game.ml:295`) is the
*only* way to mutate the engine. Every caller — the turn loop, the bots, the
tests, the simulator — goes through it.

The `mutable *_hook` fields on `player_update` are the interception points a
house decorates: `submit_command_hook`, `guard_hook` (creature shields for the
player), `add_mana_hook`, `set_data_hook`. The same trick is applied one level
down on `slot_update` (`overridable_destroy`, `add_hook`, `on_damage_hook`,
`remove_hook`, `delayed_destroy_hook`, `protect_hook`) and on `slot_state` in
the form of a `reaction` record (`sp.ml:320`), which is the flattened version
of Scala's `Reaction` class hierarchy.

## 4. One move, end to end

A card is submitted. Here is every step between the click and the next
position, all of it in `lib/game/session.ml:89-107` and `lib/game/game.ml`:

```text
Session.submit s cmd
  |
  |-- record the move          s.moves <- Game_record.move_of ...
  |-- emit Ev_submitted        what a UI animates
  |
  |-- Game.step  (Updater.lift)
  |     `-- Player.submit p cmd
  |           |-- hl_intercept_submit   a house may swallow or replace the command
  |           |                         (Wind punishes a pass, Entangler redirects)
  |           |-- is_available          cost check against the folded desc
  |           |-- cmd_cost captured      cmd_cost is frozen here, not recomputed
  |           `-- run the card's Direct effect with an `env`
  |                 `-- the effect DSL: Slot.inflict / Slots.summon / damage / heal ...
  |                        `-- each of those lands in the occupant's `reaction`
  |
  |-- Player.result            flush deaths, sync mana, re-fold attack
  |-- loud_refresh             emit Ev_refresh { silent = false } (death mana!)
  |
  `-- Session.drain s.u pid    the turn chain:
        |
        |-- pop_transition
        |     `-- Some (Wait_player next) -> hand over at once, stop
        |
        `-- otherwise, in order:
              Player.run_slots p         creatures attack (Player.run_slot)
              Ev_refresh
              Player.apply_effects p On_end_turn
              Slots.toggle_run            clear run flags for the new turn
              Player.prepare_next_turn op growth -> mana for the opponent
              Player.apply_effects op On_turn
              return `other pid`
```

Two details that trip people up:

- **`cmd_cost` is captured, not recomputed** (`sp.ml:285-291`). Fire's
  Apocalypse reads mana from *before* the payment was deducted
  (`lib/houses/basic/fire.ml:26`).
- **`loud_refresh` is not cosmetic.** Death hooks such as Prisoner's mana run
  inside `flush`/`result`, after the step; without the loud refresh the `-1`
  is swallowed by the following silent `Ev_to_move`. The comment at
  `session.ml:20-23` says so.

The four phases are `Direct | On_turn | On_end_turn | On_start` (`sp.ml:37`).
`Player.apply_effects` (`engine.ml:488`) runs **every filled creature's** phase
effect in board order first, then the player's own registered effects
most-recent-first, because `add_effect` prepends.

## 5. Events

Everything the UI animates is a `game_event` variant (`sp.ml:390-405`):
`Ev_focus`, `Ev_move`, `Ev_swap`, `Ev_run_slot`, `Ev_summon`, `Ev_die`,
`Ev_replaced`, `Ev_refresh`, `Ev_spell_played`, `Ev_ability`, and the three
turn-loop events `Ev_to_move`, `Ev_submitted`, `Ev_game_over`.

There is exactly one sink, `updater.listener`, and every event goes through
`emit` (`engine.ml:18`). The listener is passed once at session creation
(`Session.create ~on_event`) or swapped with `Updater.listen`.

Listeners run **mid-step**. Reading the board from one must use
`Updater.peek` (`engine.ml:684`), a mid-step copy that does *not* flush
pending deaths, and must never call `submit`. `Session.peek` wraps it for
convenience. `test/session.ml:76-96` pins this behaviour.

## 6. Forking: how search never touches the live game

This is the foundation every bot stands on.

```ocaml
let create ?(rng = Dice.make 0) state desc =      (* engine.ml:667 *)
  let state = copy_game_state state in             (* <-- the fork *)
  let u = { state; desc; player_updates = [||]; ended = None; listener = ignore; rng } in
  u.player_updates <- Array.init 2 (make_player u);
  ...
```

`Updater.create` copies the whole position first, then builds a private update
graph for it. `Game.simulate` (`lib/game/game.ml:269`) is the thin wrapper that
gives bots a move function:

```ocaml
let simulate ?rng (desc : game_desc) (state : game_state) pid cmd =
  let rng = match rng with Some r -> r | None -> Lazy.force sim_rng in
  let u = Updater.create ~rng state desc in
  let next = ref (other pid) in
  let _, state =
    Updater.lift u (fun u ->
        let p = Updater.player u pid in
        Player.submit p cmd;
        Array.iter Player.flush u.player_updates;
        match Player.pop_transition p with
        | Some (Wait_player (n, _)) -> next := n
        | None ->
            Player.run_slots p;
            ... On_end_turn; toggle_run; prepare_next_turn; On_turn ...)
  in
  { state; next = !next; ended = Updater.ended u }
```

Two properties fall out of this and the tests assert both:

1. **The live game is untouched.** `test_lecturer.ml` and `test_summoner.ml`
   search and then compare `state_to_string` before and after — byte identical.
2. **Simulations do not steal the game's dice.** They draw from a separate
   self-seeded `Dice` stream (`game.ml:267`). A seeded game replays the same
   whatever the search budget was.

The cost of the fork is real: every `simulate` copies both players' six slots
and all their card descs. Bots count these calls in `stats.sims` and print the
total in their stats line — that number is the real cost meter, not wall clock.

## 7. Where bots attach

```text
Table.bot_command t                                 (lib/game/table.ml:250)
  |-- check is_disabled -> None (pass) without asking the bot
  |-- t.movers.(pid)  built by movers_of            (table.ml:179)
  |     `-- Tournament.mover ~budget id             (tournament.ml:420)
  |           `-- Tournament.bot_kind id            (tournament.ml:372)
  |                 "greedy" | "summoner" | "strategist" | "lecturer"
  |                 | "oracle" | "diviner"   (also "gambler"/"apprentice" -> lecturer)
  |                 `-- make_mover kind budget     (tournament.ml:386)
  |                       `-- settings.duration <- budget_ms;  Bot.mover settings
  |
  `-- Bot.mover settings u pid = execute settings ~pid ~desc:u.desc u.state
```

Every bot has exactly this shape:

```ocaml
type settings = { mutable logging : bool; mutable stats_log : bool; mutable duration : int }
val default_settings : unit -> settings
val execute : settings -> pid:player_id -> desc:game_desc -> game_state -> command option
val mover : settings -> Game.mover
```

(Diviner's `execute` also takes an optional `?seen` list of revealed opponent
cards; its `mover` fills it in from the event stream. See document 5.)

`Game.mover = updater -> player_id -> command option` (`game.ml:257`). The
budget is a **wall clock in milliseconds**, checked with
`now_ms () < deadline` inside the search loop. That single design choice is
what makes all of them *anytime* algorithms: they must have a usable answer at
any moment, because the loop can be cut off between any two simulations.

In the raylib UI the search runs on a second domain so the window keeps drawing
(`gui/gui_board.ml:320`), with `settle` (`gui_board.ml:221`) joining before
the game is left. The TUI plays bots synchronously on its main loop and sends
bot stdout to `/dev/null` so the stats lines do not corrupt the screen.

## 8. Decks and the rest of the plumbing

`Shuffle` (`lib/game/shuffle.ml`) is a port of Scala's `CardShuffle`, where
Choco constraint solving was replaced by enumeration and rejection sampling.
It builds a legal deck as **4 cards from each of the four basic houses plus 4
from the special house**, then opening mana summing to 19 for the first player
and 18 for the second, plus a fixed 2 for the special (`shuffle.ml:169-192`).
`exclude` removes the other seat's card indexes so the two decks do not
collide.

`Catalog` (`lib/game/catalog.ml`) is the name -> card/house lookup used to
rebuild a game from a recorded JSON file. It also owns `Catalog.init`, which
runs `Game.init_cards` over every house and is a hard prerequisite for anything
that builds decks — an uninitialised house reports the same card index for all
its cards and no legal deck exists.

`Table` sits above `Session` for the front ends (seats, seeded deals, bot
dispatch, legal targets, saves); `Tournament` deliberately does **not** use it
and goes straight from `Shuffle` to `Session.play_game` (`tournament.ml:436`),
so a headless tournament never pays for UI bookkeeping.

## 9. Quick reference

| Question | Answer |
|---|---|
| Where do I add a card's behaviour? | `lib/houses/<house>.ml`, using the effect DSL at `lib/core/engine.ml:723-793` |
| Where do I add a card's types? | `lib/core/sp.ml` |
| Where do I add a bot? | `lib/bots/<name>.ml` + an arm in `Tournament.bot_kind` (`tournament.ml:372`) + `bot_ids` (`:384`) + an `ai.<name>` i18n key |
| How does a bot see the board? | `u.state`, a plain `game_state` |
| How does a bot try a move? | `Game.simulate desc state pid cmd` |
| How does a bot enumerate moves? | `Game.legal_commands`, or `Oracle.base_moves` for the pruned set |
| How does a bot stop? | `now_ms () < deadline`, with `deadline = t0 + settings.duration` |
| Where does the search run? | On the calling domain; the GUI wraps it in `Domain.spawn` |

Next: [02-game-view.md](02-game-view.md) — the bot-facing API in detail.