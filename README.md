# mancer-ml

OCaml port of freespectrogdx (Scala 3 + libGDX). The engine, all houses, the campaign, and both UIs
are in. Port status and the Scala → OCaml map live in
[docs/tech/migration.md](docs/tech/migration.md).

```sh
eval $(opam env --switch=mancer)   # OCaml 5.2.0 + dune 3.24.2
opam install notty raylib          # terminal UI, raylib UI
dune build
dune test                          # Alcotest suites + expect tests
dune promote                       # accept a reviewed expect-test diff
dune exec ./bin/tui.exe            # play or watch a game in the terminal
dune exec ./bin/gui.exe            # raylib window; in a game, F8 cycles spell effects
```

Format with `dune fmt`.

## Play

Presentation page [here](https://minhdoboi.github.io/mancer-ml).

Both UIs open the same lobby: campaign, duel, and settings.

**Duel** picks each seat (`human`, `greedy`, `summoner`, `strategist`,
`lecturer`, `oracle`, or `diviner`) and a portrait. Two humans is hot-seat;
two bots is a game to watch.

**Settings** picks both special houses, the bot budget, an optional seed,
and the language. An empty house set plays every special.

Like the Scala lobby, both UIs remember setup in `~/.mancer/user.prefs`
(the same Java-properties file as `Storage.scala`, overridable with
`$MANCER_USER_PREFS`). `$MANCER_LANG` (`fr`, `es`, `ru`, `vi`; default `en`)
wins over the saved language when set; a language picked in the GUI settings
takes effect on restart. Card text comes from `i18n/<lang>.properties` in
the assets checkout, falling back to English per key.

### Terminal

```sh
dune exec ./bin/tui.exe
```

On your move, pick a card with the arrows and press enter, then choose a
highlighted slot (arrows or 0–5) and enter again; `p` passes. Space pauses
a bot game. `w` saves to `tui-games/` in the tournament format. After a
game, `r` replays the same seed and `n` deals again.

Bots print stats to stdout, so the TUI sends stdout to `/dev/null` and
draws on a duplicate of the terminal descriptor.

### Raylib

```sh
dune exec ./bin/gui.exe
dune exec ./bin/gui.exe -- --shot out.png --bots --seed 1 --wait 8
```

The window scales a fixed 1440×900 layout. Click a lit card to play it;
legal targets light up; right-click or Esc cancels. Hovering a card shows
its text. Saved games go to `gui-games/`. **F8** cycles the spell-effect
demos on the board.

Bots search on a second domain so the window keeps drawing; their move is
submitted on the main one, and leaving a game waits for a running search
to finish.

## Tournament

`bin/main.exe` shuffles legal decks, plays the requested bots, and writes
`game-NNN.json` plus `summary.json` (W/L/D and Elo, K=16) under
`tournament-results/<timestamp>/`. Default is greedy vs greedy. `--budget`
is the search bots' milliseconds per move (default 1000). `--special`
picks the special house for both players.

```sh
dune exec ./bin/main.exe           # 20 greedy-vs-greedy games
dune exec ./bin/main.exe -- -v     # first game with an event trace
dune exec ./bin/main.exe -- --bots greedy,oracle --games 4 --budget 200 --seed 1
dune exec ./bin/main.exe -- --special zen --games 4 --seed 1
dune exec ./bin/main.exe -- --mode summary --dir tournament-results/<ts>
dune exec ./bin/compare_engines.exe -- --seed 1 --special zen --turns 80
```

The greedy bot always takes the cheapest legal card, so greedy-vs-greedy
win counts are not a balance signal. Oracle vs greedy is.

`--bots greedy,summoner,strategist,lecturer,oracle,diviner` is the full
matchup. Lecturer is a UCT tree search; tune with
`LECTURER_OPTS=exploration=0.7,maxDepth=4,...`.

## Limitations

Both UIs hide what the Scala game hides, but the bots don't play blind:
like the original, they think on the full `game_state`, so anything the
engine tracks is visible to them even when your board keeps it secret.

- **Opponent hands in search.** Strategist, lecturer, oracle, and summoner
  explore the opponent's replies with that side's real hand, never a guess.
  Diviner is the exception: it samples belief hands for the opponent's
  deck, but even its worlds keep the true board, Stalker included.
- **Not every rule of the original game is implemented.** The 1% rule and
  some of the deck-building rules are missing, so decks and matchups can
  differ from the original Spectromancer.

## License

The game engine and tools are under the
[GNU Affero General Public License v3.0 or later](LICENSE) (AGPL-3.0); the
mancer-assets media carries the same license. You may use and modify them in
open source projects; if you ship a product or hosted service that includes
this code or the mancer-assets media, you must publish corresponding source
under the same license. Bundled fonts keep their own terms
(`assets/fonts/IMFellEnglish-OFL.txt`).

## Assets

Art, sounds, and text live in the
[mancer-assets](https://github.com/minhdoboi/mancer-assets) submodule at
`assets/` (`git submodule update --init`, or point `$MANCER_ASSETS` at another
checkout), committed in the shape the game loads: `cards/<card key>.jpg`,
`backgrounds/<house key>.jpg`, `portraits/<id>.png`, `sounds/<sound key>.mp3`.

The one thing built from a source is the portraits: `assets/source/portraits/`
holds the 1024² originals (tracked with [git-lfs](https://git-lfs.com) — install
it before you clone or pull, or those arrive as pointer text), and
`bin/sync_assets.ml` cuts them to the 256² the UI draws and reports every house
card the game finds no art for.

```sh
dune exec ./bin/sync_assets.exe
scripts/push_assets.sh "message"   # commit assets/, push, stage the pointer
```

How to paint it is in [AGENTS.md](AGENTS.md).

## Package

The GUI is a native binary, so you package **on each OS** (Linux, macOS,
Windows). raylib is linked statically; the zip is the executable, `assets/`,
and a small launcher.

```sh
scripts/package.sh
```

That writes `dist/mancer-<version>-<os>-<arch>/` and a matching zip. Players
run `./mancer` on Linux, `mancer.app` on macOS, or `mancer.bat` on Windows.
The first macOS launch needs right-click → Open (the app is not signed).
Windows is built with mingw via Diskuv OCaml or opam; Git Bash can run the
same script.

Full steps, signing, and DLL notes: [docs/tech/packaging.md](docs/tech/packaging.md).

## Layout

`lib/` is the wrapped library `Mancer`.

| Path | Role |
|---|---|
| `lib/core/` | Types (`sp.ml`), engine, shared house helpers |
| `lib/houses/` | Basic and special houses |
| `lib/bots/` | Search bots |
| `lib/game/` | Session, table, shuffle, campaign, tournament, assets |
| `gui/` | The raylib front end as a library: spell effects (`fx*`) and the screens (`gui_*`) |
| `bin/` | `main`, `tui`, `gui` (entry point), `replay`, `compare_engines`, `sync_assets`, `cardlist` |
| `templates/` | The grimoire page: `cards.html` with `cards.css` and `cards.js` beside it |
| `test/` | Alcotest suites, `expect/` board snapshots, `support/` fixtures |

`session.mli` drives a live game: `Session.create`, `status`,
`legal_commands`, `submit`, plus an `on_event` listener. `table.mli` sits
on top for front ends (seats, seeded deals, bot dispatch, legal targets).
`engine.ml` is modules in dependency order (`Houses`, `Slot`, `Slots`,
`Player`, `Reaction`, `Updater`) followed by the card effect combinators.
House code reads like the Scala: `Slot.inflict s d`, `Player.inflict op d`,
`Slots.summon su num c`.

Record fields that would clash across types carry a short prefix (`b_`
creature body, `cd_` card desc, `h_` house, `ps_` player state), so
literals never need a type annotation.

A rules change shows up as an expect diff: review it, then `dune promote`.

## Docs

- [Grimoire](docs/cards.html) — every house and card with art, cost and
  rules text. The page reads [cards.json](docs/cards.json) and draws the
  list: a search box covers every house and card at once, and the list on the
  left picks the house to show. The list is published in every language the
  game ships — the English one is `cards.json`, the others
  `cards.<lang>.json` — and the menu in the header picks one, so the page
  reads in the reader's own language. Regenerate and commit after touching a
  house or the strings:

  ```sh
  dune exec ./bin/cardlist.exe
  ```

  `.github/workflows/pages.yml` publishes `docs/` and copies `assets/cards/`
  next to those files, so the art has to be in the submodule; the workflow
  checks it out for that reason. The page fetches the card list over http,
  so opening it as a `file://` URL finds nothing — serve the folder to look
  at it:

  ```sh
  python3 -m http.server --directory docs
  ```

  The page itself is a template, not program output: `templates/cards.html`
  with `templates/cards.css` and `templates/cards.js` beside it. `cardlist`
  fills the `{{css}}`, `{{js}}` and `{{langs}}` markers and writes
  `docs/cards.html`, so editing the page means editing the template and
  running the tool again. The text of a list comes from the i18n bundle of
  that language (`assets/i18n/<lang>.properties`), so a translation of the
  card names and rules text is enough to publish the list in it.
- [Tutorials](docs/tuto/README.md) — the architecture, how the AI opponents
  think (Oracle, Diviner, and the rest), the maths behind the spell effects,
  and how the lobby ocean and the blacksmith sounds are synthesised
- [Migration status and Scala map](docs/tech/migration.md)
- [House port mismatches](docs/tech/house-port-mismatches.md)
- [Spell-effect port plan](docs/tech/effects-port-plan.md)
- [Packaging for Linux, macOS, Windows](docs/tech/packaging.md)
