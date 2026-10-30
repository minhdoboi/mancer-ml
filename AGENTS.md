# Agent notes

After any code change, run `dune build` and fix compile errors before finishing.

To see whether a packaged build starts on this machine, run
`scripts/smoke.sh` (it opens the game in a hidden window and plays a few
seconds); `docs/tech/packaging.md` has the details.

Editing a house or its English strings also means the site is stale: run
`dune exec ./bin/cardlist.exe` and commit `docs/cards.html` and the card
lists (`docs/cards.json`, `docs/cards.<lang>.json`). The GitHub Pages workflow
copies `docs/` plus `assets/cards/` into the artifact, so a card's art must be
in the submodule for the page to show it (new art: `sync_assets` first,
`cardlist` after).

The page is a template, not program output: `templates/cards.html` with
`templates/cards.css` and `templates/cards.js` beside it. `cardlist` fills the
`{{css}}`, `{{js}}` and `{{langs}}` markers and writes `docs/cards.html`, so a
change to the page is a change to the template — then run `cardlist` again and
commit the result.

The list is published in every language the game ships: the English one is
`docs/cards.json`, the others `docs/cards.<lang>.json`, and the menu in the
page header picks one. The text of a list comes from the i18n bundle of that
language in the submodule (`assets/i18n/<lang>.properties`), the page's own
strings from the `grimoire.*` keys of the same bundle — so a new language
needs its bundle and nothing else.

The landing page (`docs/index.html`) is hand-written and speaks the reader's
language too: the top of it (title, tagline, the "About" story, the buttons)
is translated by `docs/index.js`, which holds the strings for the site only —
the game never reads them, so they do not belong in an i18n bundle. The
language comes from `?lang=`, then from what the reader picked (the key is
shared with the grimoire page, so both pages keep the same language), then
from the browser.

**The media lives here**, in the [mancer-assets](https://github.com/minhdoboi/mancer-assets)
submodule (`assets/`), and it is committed **in the shape the game reads it**:
a card's picture is `assets/cards/<card key>.jpg`, a board is
`assets/backgrounds/<house key>.jpg`, and so on. No other checkout is involved,
and there is no second copy of a file to keep in step with the first.

The one exception is faces. The 1024² originals live in `assets/source/portraits/`
(tracked with git-lfs — `git lfs install` before you clone or pull, or those
arrive as pointer text) because the UI only ever shows a face at 84-120px, so
`sync_assets` cuts each one to `assets/portraits/<id>.png` at 256². It also fails
listing any house card that has no picture at all.

```sh
dune exec ./bin/sync_assets.exe
scripts/push_assets.sh "message"   # commit assets/, stage the submodule pointer
```

Use `.tmp` folder for intermediate files. Keep the full-size card art **source**
in `assets/source/cards/`, named after the card key plus `.src.png`
(`assets/source/cards/lightstave.rainbowdragon.src.png` for
`assets/cards/lightstave.rainbowdragon.jpg`). That folder is not versioned, but
it is the reference to re-crop or edit a card later, so do not delete the source
after resizing.

## Adding a special class (special house)

A "class" in the lobby is a special house. Worked examples to copy:
`lib/houses/special/trappers.ml` (simple) and `beastlord.ml` (adds a
player-data reaction hook). Checklist, in order:

1. **House module** `lib/houses/special/<key>.{ml,mli}` — cards, `house`, and
   usually a `cost_fn`. Two hard rules the card list depends on:
   - **List cards in ascending-cost order.** The deck builder and card list
     assume index order == cost order; sorting after the fact breaks the dealt
     deck and the test's quarter picks.
   - **Costs come from `cost_fn i` (index-based), not `c.cost`.** A special
     house with no `cost_fn` gets the default `i + 1`. For ties at the same
     cost, cheaper-feeling cards go first. Register it in `Catalog.init`:
     `Game.init_cards ~cost_fn:<Mod>.cost_fn <Mod>.house`.
2. **Registration** — `Catalog.specials` and `Catalog.init` (both lists, keep
   them in the same place); `Table.house_groups` (`("author", ["key"])`); pick
   a free `h_house_id`.
3. **i18n** — house + card strings (`<key>=`, `<key>.<card>=`, descriptions)
   in **all five** bundles `assets/i18n/{en,es,fr,ru,vi}.properties`.
4. **Tests** — `test/support/support.ml` (add to `all_houses` and
   `init_all_houses`, with `~cost_fn` if any) and a `(test (name <key>) ...)`
   stanza in `test/dune`. Remember the dealt special deck is one card per
   quarter of the list (`Game.special_pick`): with 8 cards that's indices
   0, 2, 4, 6 — write the test's expectations against those cards.
5. **Art** — `assets/cards/<key>.<card>.jpg` (sizes/rules below),
   `assets/backgrounds/<key>.jpg`, then `sync_assets` (it lists any missing
   art) and `cardlist`.
6. **Campaign** (optional) — a node in `lib/game/campaign_map.json`
   (`{id, chapter, x, y, after|after_any, fight:{house_key, bot_id,
   portrait_id, life}}`, plus edges), the fight's `portrait_id` in
   `Campaign.campaign_portraits`, and `campaign.<node>.name/message/win/lose`
   in all five bundles. `test/campaign.ml` checks portraits and unlock paths —
   update its `*_path` assertions if you rewire edges.

## Card art (`assets/cards/`)

Name the file after the **card key** the game looks up —
`cards/fire.goblin.jpg`, `cards/zen.wooly.jpg` — not the English name. The key
is the `~key:` string in the house `.ml` under `lib/houses/`; if you are not
sure, `dune exec ./bin/sync_assets.exe` lists every house card the game finds no
art for. Keys are unique per card, so there is nothing to disambiguate.

The lookup also accepts the English card name (`cards/Goblin.jpg`) as a fallback,
but only while the game runs in English, so a few legacy files still use that
form; leave those alone, do not add more.

### File size

Save creatures as **80×80** JPEG and spells as **95×103** JPEG (the round spell
frame). Do not go larger. The game does not scale the sprite to the stone
frame: it places creatures at (3, 14) and spells at (−1, −1), then crops to the
frame. A 1024×1024 painting therefore shows only a dark corner, an 80×80 spell
sits off-center in the 95×103 hole, and a huge file blows up any UI that sizes a
thumbnail to the image height.

After generating art, first copy the full-size original to
`assets/source/cards/<card key>.src.png` as the reference source. Then downscale
with LANCZOS to the size above, lift crushed shadows, and unsharp-mask so the
subject still reads at thumbnail size. Then sync.

### Look

Painted illustration, close to Magic: The Gathering card art: oil-paint
texture, dramatic light, a single readable subject filling a square frame.
No text, letters, UI, watermark, or card border.

The mood follows the house: **dark fantasy** for sinister or earthly factions
(Undead, Insects, Fire, Rogues, …) and **celestial fantasy** for radiant or
sky-bound ones (Light, Air, Rainbows, …). Pick the palette that fits the
faction's identity rather than forcing one mood onto every card.

In-game art is the 80×80 file itself. **Contrast is required.** The subject must
separate from the background by value and hue: rim light, brighter midtones, a
clearer silhouette. Do not crush the whole picture into one dark brown or red.
A red mantis needs a non-red ground; a black wasp needs yellow stripes and a
lighter field so it still reads as a wasp.

A house is a **faction**, not a color wash. Cards in the same class must be easy
to tell apart at a glance. Vary hue, setting, and silhouette. Fire can have a
crimson dragon and a gold monk; insects can be ember-carapace, toxic dust, and
a blood-red mantis. Do not stamp one palette onto every card.

### Read the power from the picture

The art should hint at what the card does and how strong it feels. A healer or
buffer must not look like a bloodied berserker.

- Support / heal / mana: calm light, open hands, gardens, temples, soft glow.
- Tokens and cheap chaff: small, scrappy, incomplete gear.
- Walls and tanks: mass, armor, low stance — not a killing pose.
- Finishers and high-cost threats: scale, menace, occupied space.
- Spells: the phenomenon itself (swarm, storm, aurora), not a portrait of a
  caster unless the card is that person.

Read the English description in `assets/i18n/en.properties` (and attack/life in
the house `.ml`) before drawing. Token cards spawned by a house (`Wasp Drone`,
`Insect Warrior`, …) need art too.

### Houses already painted for vibe

Air, Antimancer, Battlemage, Chronomaster, Colors, Conjurer, Dark Priest,
Darksider, Disciples, Dreamweaver, Dudes, Earth, Elementalist, Entomologist,
Faerie Druid, Fairy Knight, Fire, High Priest, Junk, Kinetician, Limbo,
Mountain King, Rogue, Seasons, Shadowmage, Shaman, Snowblood, Soul Reaper,
Soulbinder, Sower, Tinker, Troopers, Vampires, Warp, Water, Wind, Zen.
Lightstave is painted in the **celestial** mood (bright radiant palette), not
the dark one.

Dudes and Troopers started as beat-em-up / sci-fi pastiche. In vibe they are
**fantasy** versions of the same roles (pit fighters of rising rank; a siege
army of infantry, barracks, and engines), not street punks or space marines.
Every Dude is an adult man; faces and builds must read as male.

## Board backgrounds (`assets/backgrounds/`)

One wide scene per house, named after the **house key** the game looks up
(`fire.jpg`, `darkpriest.jpg`, `fairyknight.jpg`, `default.jpg`); the key is the
`~key:` string in the house `.ml`, or the folder name in `lib/houses/`.
Existing vibe boards are **832×800**. Painted land art, no text, dark enough
that cards stay readable. The board uses the starting player's special house;
a house with no board of its own falls back to `default.jpg`, and
`sync_assets` lists the houses in that state.

### Layers

A board may also ship two extra files beside the flat painting, both cut from
the same picture:

- `<house key>-ground.png` — the painting with the **sky left transparent**
  (RGBA), the same 832×800 as the flat file.
- `<house key>-sky.jpg` — just the sky, **twice as wide** (1664×800): the frame
  followed by its mirror image, so the horizontal repeat has no seam.

The board draws the sky drifting sideways behind the ground, the way Scala's
`Board.scala` `Background`/`ScrollingSky` does; with no `-sky` it draws the
flat `<house key>.jpg` as before. `junk` has a `-ground` + `-smoke` pair for the
chimney, which the OCaml GUI does not draw yet — it still uses the flat junk
board (`Gui_draw.draw_sky`, `Assets.background_layer`).

## Portraits (`assets/source/portraits/`)

Faces for the lobby chooser, the in-game life column, and campaign dialogs.
`sync_assets` cuts them to **256×256** PNG at `assets/portraits/<id>.png`.
Register every new id or the file is unused:

- Player faces: `Table.player_portraits` in `lib/game/table.ml`
- AI / opponent: `Table.duel_opponents` in the same file
- Campaign fights: `Campaign.campaign_portraits` in `lib/game/campaign.ml`
- Meetings: `Campaign.npcs`

`test/campaign.ml` checks that every campaign `portrait_id` exists as a PNG
and is declared on one of those lists.

### File size

Save as **1024×1024** PNG (square, 1:1) in `assets/source/portraits/`. Existing
files are often JPEG bytes under a `.png` name; either is fine as long as the
path ends in `.png` and the pixels are square. The UI scales them down to **84**
on the board and about **120** in the duel chooser — generate large and let
`sync_assets` shrink. Do not deliver 80×80 card-art size; the face will smear.

`sync_assets` copies a source that is already 256×256 as-is, so a hand-shrunk
face there survives untouched.

After generating, keep the square crop with the head and shoulders filling most
of the frame. Downscale to 1024×1024 with LANCZOS if the source is larger. No
text, letters, UI, watermark, or frame.

### Look

Classical oil **bust**: head, neck, and shoulders against a soft painterly
ground (often a pale sky or simple wash), impasto brushwork. This is a person,
not a card illustration — no full-body scene, no creature, no spell phenomenon.
Face must read at thumbnail size: clear features, rim or key light, contrast
between skin and background. Do not crush the portrait into one dark brown.

Same fantasy setting as the vibe cards, but lighter and more portrait-studio
than battlefield art. Card-art style (MTG-like impasto), not flat cartoon or 3d
render — a humorous character design is fine, paint it like a card. A campaign
character may carry a soft, secondary background that names their place (the
junkyard behind the scrapper), as long as the face stays the subject.

### Types

- **Player** (`Table.player_portraits`) — lobby “you” faces. Kebab-case ids that
  describe the person (`ember`, `old-thinker`, `grove-crone`). Mix ages and
  genders; each face should be easy to tell from the others at 84px (hair, coat
  hue, age).
- **AI / opponent** (`Table.duel_opponents`) — one bot kind per difficulty.
  Default pair is a man (`<kindId>`) and a woman (`<kindId>-f`). Extra faces for
  the same bot are `<kindId>-2`, `<kindId>-3`, … Two named opponents must not
  share a file. Gambler is not ported; those faces still play Lecturer.
- **Campaign** (`Campaign.campaign_portraits`, plus `Campaign.npcs` for
  meetings) — a campaign character is its own person, so it gets its own file
  named after the **node id** (`disciples`, `junk`, `farmer`), not after a bot.
  A node’s `bot_id` only says which brain plays it; the face is independent. A
  node may still reuse an AI face while it has no art of its own.

Keep a **kind uniform** so the AI pair reads as the same role: same coat family,
metal, and prop (Summoner: pale robe and crystal staff; Strategist: teal officer
coat and a rolled map). Change face, age, pose, and facing so siblings are not
clones. Campaign-only faces have no such duty, but they must not look like an
AI face either — a redneck preacher reads as the character, not as a strategist
in costume.

When adding a bot, also add `ai.<id>` in i18n and a module under `lib/bots/`.
