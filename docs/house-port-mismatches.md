# House port mismatches (OCaml vs Scala)

Findings from a read-only comparison of every house in `lib/houses/**` against its Scala original in
`freespectrogdx/core/src/main/scala/priv/sp/house/`, plus the shared helpers (`house/package.scala`,
`Common.scala`, `PlayerUpdate.scala`, `SlotsUpdate.scala`). Gameplay items below were then re-checked
in both sources and fixed in OCaml unless marked otherwise.

**Scope.** All 42 houses were read. No wrong card numbers, costs, keys or missing cards were found.
Every difference was in listeners, engine helpers or flags.

Severity is the expected gameplay impact: **high**, **medium**, **low**.

## 1. Shared engine (fixed)

### 1.1 Multi-target flag on custom attacks — fixed

Scala sets `isMultiTarget = true` on `DarkHydraAttack`, `SunPriestAttack`, Dreamweaver `SwordAttack`
and Zen `RedlightAttack`. OCaml now stores `is_multi_target` on `run_attack` and
`custom_run_attack ~multi:true` for those four. `is_multi_run` is what Colors Crimson, Warp Stranger
merge, and the bots consult. Apep still uses `multi_run_attack`. Vampire Blood Ties still compares
`apply_ = Multi_target` only (same as Scala’s `!= MultiTargetAttack` reference check).

### 1.2 Attack source order — fixed

`add_attack` appends, matching Scala `:+`. `fold_attack` is oldest-first. Sirin after an Orc +2 on a
base-4 creature is `(4+2+1)/2 = 3` in both.

### 1.3 `fill_empty_slots` does not summon — fixed

`fill_empty_slots` and Faerie Cloud use `Slots.add_creature` (`slot add`), which fires `on_add` but
not `SummonEvent`. Opposing Marine / Wolf Spider no longer trigger on Cloud Rabble or Junk trash.

### 1.4 `Player.set_data` writes unconditionally — partial

Scala `updateData[A]` is a no-op when the player's data is not of class A. OCaml `Player.set_data`
still writes. House setters that were able to clobber another house’s data now match the typed
update:

- Entomologist `set_target` only writes `Player_targeting`
- Soulbinder `set_souls` / `add_soul` only write `Soulbinder_data`
- Entomologist end-of-turn recover matches Scala `RecoverTarget` (`!tracksTarget || target` defined)

A Quantumancer replaying Hivemind via Cosmic Mage no longer loses `Quantum_data`. Other houses that
call `set_data` with their own variant are unchanged.

### 1.5 Silenced creatures and id-based listener checks — fixed

`mere_mortal` keeps the card id. Listeners that used to match on id now also require
`not (is_bridled st)` (`c_body.b_data = Mortal_of _`). Affected: Snowblood Amaterasu and Guide,
Battlemage Field Mage, Antimancer Mirror Priest and Bombardier, Vampires Acolyte.

## 2. House-specific

### Warp (`warp.ml`) — fixed (was high vs Colors, Limbo, Soulbinder, Soul Reaper)

Opponent listener handling now matches Scala `OpponentListener`: Limbo, Colors, Soulbinder and Soul
Reaper (`h_key = "reaper"`) are not installed on Warp; other opponent houses still run as a proxy.
`Reaper_data` is not copied onto Warp.

`unbridle` / `Slot.overwrite_add` was not a mismatch (default `add_hook` binds `selected`).

### Kinetician (`kinetician.ml`) — fixed (was medium-high)

Manipulator re-reads `get_open` after the `4*cost` self-hit, so a fatal hit on a full board can steal
into the freed slot.

### HighPriest (`highpriest.ml`)

- **Babi mana tracking — fixed.** Reacts on `houses_update.after_update` after the whole opponent
  houses write (including `incr_mana_all`), and compares the new total with the stored one.
- **Sun Priest multi-target — fixed.** See 1.1.
- **`card_index` on the alternate set — fixed.** `Game.init_cards` now numbers `h_additional_cards`
  0..n as well, so `hp_set` is 0..7 for Eye of Wajet.
- **`choose_path` refresh — fixed.** Runs from `slots_update.after_change` (add and remove, including
  moves), matching Scala `p.slots.update`.
- **Serpent/Ouroboros timer — not a mismatch.** Both sides heal the Ouroboros slot only if it still
  holds Ouroboros, otherwise they remove the effect. Scala’s guard is `card == ouroboros` after
  `import env._` (`HighPriest.scala:240`), i.e. `env.card`, not “any occupant.” Left as in the port
  (slot identity), which is the intended rule.

### DarkPriest (`darkpriest.ml`)

- **Dark Hydra multi-target — fixed.** See 1.1.
- **Black Mass description — UI only, left static.** Scala appends a live `" [" + getBlackMassX +
  "]"`. `en.properties` has no placeholder.

### Darksider (`darksider.ml`)

- **Warlock damage cost — fixed.** Damage uses the printed `cmd_card.cost`; mana drain still uses the
  paid `cmd_cost`.
- **Lake timing — fixed.** Lake observes `submit_command` after a successful submit. Warlock still
  intercepts (a blocked spell is not recorded).

### Meteomancer (`meteomancer.ml`) — fixed (was medium)

`on_opponent_plays_called` snapshots filled slot numbers before looping, so a newly spawned
Extremophile is not visited in the same pass.

### Faerie (`faerie.ml`)

- **Cloud summon reactions — fixed.** See 1.3.
- **Ring bonus timing — fixed.** +2 and the end-of-turn removal run after `submit_command`, so a Ring
  played as the special gets +2 that turn and a Warlock-blocked spell does not.

### Fairy (`fairy.ml`)

- **Alkonost pool — fixed.** Built from the desc with mods applied, so already-hidden cards
  (Destroyed, Lake, Lesovik, Rusalka, an earlier Alkonost) are not drawn again.
- **ZGory / THGory evolve — left as-is.** OCaml adds the next stage only if the slot is empty; Scala
  adds unconditionally. Equivalent at normal death time.
- Attack source order (Sirin) — fixed via 1.2.

### Entomologist (`entomologist.ml`) — fixed (see 1.4)

### Soulbinder (`soulbinder.ml`) — fixed

Tree On_turn uses open adjacents that are also in `ps_slot_list` (`Slot.open_adjacent`). Soul writes
are typed (1.4).

### Limbo (`limbo.ml`) — fixed

`set_limbo` calls `on_limbo` only (Scala `LimboReaction.onLimbo`), not `on_death` on every creature.
Empty-slot `delayed_destroy` no longer forwards the old hook. Wraith still has no `setDirty`; attack
is recomputed on refresh.

### Colors (`colors.ml`) — fixed

`select_colour` filters `house_id == Colors.houseId`. Crimson uses `is_multi_run` (1.1).

### Elementalist (`elementalist.ml`) — fixed (Stone Golem)

`selfProtect` requires the opposite slot empty in both the committed `updater.state` and the live
board, matching Scala’s “FiXME hack.” Avalanche / Frost descriptions stay static (UI only; Frost’s
English string has no `{0}`).

### MountainKing (`mountainking.ml`) — left as-is (OCaml is the intended rule)

`hl_init` also hooks `add_hook` on the opponent’s slots. Scala hooks only `p.slots.slots`, leaving
the `else` branch of `onAdd` dead. A creature summoned opposite a Hird Soldier receives
`SoldierLowerAttack` in OCaml.

### Others

- **Tinker:** not a mismatch. Scala `Context(owner.id, Some(tinkerbomb))` is `(player, card)`;
  `selected` defaults to `noSelection`. Both sides use `-1`.
- **Zen:** `spawn_matter` still re-runs Matter’s Direct effect by hand; judged equivalent to
  `applyEffect(Direct, …)`. Redlight multi-target is 1.1 (fixed).

## 3. Houses with no remaining gameplay differences

Fire, Water, Air, Earth, Troopers, Wind, Chronomaster, Disciples, Dudes, Seasons, SoulReaper, Sower,
Shaman, Rogue, Shadowcaster, SpiritShaman, Conjurer, Quantumancer, Junk, Powermancer, Shadowmage,
Dreamweaver, Snowblood, Battlemage, Antimancer, Vampires, Tinker, Mountain King (intentional
opponent-hook difference), Fairy Gorynych evolve (equivalent at death), Zen Matter spawn (equivalent).

UI-only leftovers: Dark Priest Black Mass live X, Elementalist Avalanche/Frost live X.
