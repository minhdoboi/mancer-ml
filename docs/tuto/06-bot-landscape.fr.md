# 6. Le paysage des bots

Les six bots côte à côte : les réglages que chacun expose, et une méthode pour
mesurer un changement plutôt que deviner son effet.

## En une minute

- Il y a **six bots**, de `greedy` (joue la carte la moins chère, sans
  réfléchir) à `diviner` (imagine plusieurs decks adverses plausibles et joue
  le coup qui se défend le mieux face à l'ensemble de ces hypothèses).
- Ce ne sont **pas six programmes**. Chacun est un assemblage différent des
  mêmes quatre briques : un *générateur de coups*, un *simulateur*, une
  *fonction de score* et un *contrôleur* qui décide où dépenser le temps de
  réflexion.
- Les six « niveaux de difficulté » du lobby se contentent de choisir l'un de
  ces bots. Le **budget** (millisecondes de réflexion par coup) est le seul
  autre réglage.
- Pour savoir si un changement a aidé : **jouer beaucoup de parties et
  comparer** (`bin/main.exe`, §5). Vingt parties, c'est un test de fumée, pas
  une preuve.

| Pour… | Aller à |
|---|---|
| savoir quel bot est lequel | §1 et §2 |
| modifier le comportement d'un bot depuis le shell | §3 |
| comprendre une ligne de log `*-stats` | §4 |
| vérifier qu'un changement a réellement renforcé un bot | §5 |
| renforcer un bot, l'accélérer ou l'affaiblir volontairement | §6 |

### Mots utilisés dans ce document

| mot | sens courant |
|---|---|
| **ply** (demi-coup) | un coup joué par un camp (deux plies = je joue, l'adversaire répond) |
| **rollout** | une partie imaginaire rapide, jouée jusqu'au bout (ou à profondeur fixe) par une politique peu coûteuse, pour voir qui est en tête |
| **feuille / évaluation** | le score attribué à une position là où l'imagination s'arrête ; « à quel point la position semble-t-elle bonne pour moi ? » |
| **budget** | le temps de réflexion alloué par coup, en ms (le curseur du lobby) |
| **successive halving** | essayer un peu chaque coup candidat, écarter la moitié la plus faible, approfondir le reste, et recommencer |
| **UCT / MCTS** | une recherche qui fait croître un arbre et revisite en priorité les branches les plus prometteuses |
| **monde** (diviner) | une hypothèse sur ce que pourrait être le deck caché de l'adversaire |
| **Elo** | un classement qui monte quand on bat des adversaires, calibré pour qu'un écart de 400 points signifie « dix fois meilleur » (§5) |

Le document 3 définit ces notions proprement ; ce tableau ne sert qu'à permettre
de poursuivre la lecture.

## 1. Six bots, quatre briques

Chaque bot de `lib/bots/` est un réglage différent des mêmes quatre composants
décrits dans le [document 2](02-game-view.md) :

| | 1. générateur | 2. simulateur | 3. évaluation | 4. contrôleur |
|---|---|---|---|---|
| **greedy** | premier de `available_commands` | — | — | aucun |
| **summoner** | `legal_commands` complet | `Game.simulate` | `Heuris.of_state` | best-first + iterative deepening |
| **strategist** | complet, ordonné par rôle | `Game.simulate` | `Heuris.of_state` + `synergy`, actualisé par gamma | best-first + bandit UCB |
| **lecturer** | complet à la racine, `base_moves` en dessous | `Game.simulate` | `Oracle.Eval.score` après 4 passes | UCT / MCTS |
| **oracle** | `Oracle.base_moves` | `Game.simulate` | `Oracle.Eval.score` après 4 passes | successive halving |
| **diviner** | `Oracle.base_moves` | `Game.simulate`, un moteur par monde | `Oracle.Eval.score` par monde | successive halving × mondes |

En clair : **greedy** prend la première option possible ; **summoner** et
**strategist** regardent quelques coups à l'avance avec une formule artisanale
du type « qui gagne la course » ; **lecturer** joue des milliers de parties
quasi aléatoires depuis la position courante et s'en remet aux moyennes ;
**oracle** fait de même mais gaspille moins de temps sur les mauvais coups ;
**diviner** est un oracle qui entretient plusieurs hypothèses sur le deck
adverse au lieu d'une seule.

Deux familles d'évaluation, qui ne sont pas interchangeables :

- **`Oracle.Eval.score`** (`oracle.ml:26-66`) — lisse, pondérée par les
  ressources, `(courbe de vie, plateau, mana, croissance)`, écrasée par `tanh`.
  Utilisée par oracle, diviner et lecturer.
- **`Heuris`** (`heuris.ml`) — une **prévision de course** à 12 swings mélangée
  à un terme de ressources à long terme, ou `life_mana`, ou un `synergy`
  pondéré. Utilisée uniquement par summoner et strategist.

Elles répondent à des questions différentes. `Eval` demande « à quoi ressemble
la position si l'on s'arrête maintenant ». `Heuris.score` demande « qui gagne
la course ». `Heuris.blend` ne donne du poids au terme à long terme que là où
la course est indécise ; un bot qui l'utilise acceptera donc volontiers un
mauvais échange quand il est déjà gagnant. Savoir laquelle convient le mieux à
ce jeu est une question empirique — il faut mesurer.

## 2. L'échelle de difficulté

Le lobby de duel propose six niveaux (`lib/game/table.ml:58-73`), dans l'ordre
`AiLevel` de Scala :

| niveau | visage | bot | recherche |
|---|---|---|---|
| 1 | summoner | `summoner` | best-first, profondeur 2–8 |
| 2 | strategist | `strategist` | best-first + bandit, profondeur 2–3 |
| 3 | gambler | `lecturer` | UCT — le portage a gardé les visages, pas le cerveau |
| 4 | lecturer | `lecturer` | UCT, le même bot que le niveau 3 |
| 5 | oracle | `oracle` | successive halving |
| 6 | diviner | `diviner` | successive halving × 4 mondes |

Les niveaux 3 et 4 sont le même moteur derrière des portraits différents :
*« Gambler is not ported, so those faces still play lecturer »*
(`table.ml:56-57`). Le nom affiché vient de `opp_kind` et le moteur de
`opp_bot`, ce qui explique qu'une clé i18n `ai.gambler` existe sans aucun
Gambler derrière. Les niveaux ne portent aucun paramètre de difficulté propre —
toute la différence tient au module exécuté et au budget choisi sur le curseur
du lobby (`budget_options = [100; 250; 500; 1000; 2000; 5000]`, `table.ml:324`).

L'ordre des bots dans l'échelle n'est pas un classement de force : il ne suit
pas non plus la force mesurée. À 200 ms par coup (12 parties par paire,
`--seed 7`), Lecturer a battu Oracle 7–5 et Diviner 8–4, et Oracle a battu
Diviner 7–5. Laisser les bots voir le vrai deck (`BELIEF_SAMPLES=0`) ne change
pas cet ordre : Lecturer bat encore Oracle 11–1. Diviner partage son budget
entre ses mondes et termine donc moins de playouts qu'Oracle à petit budget.
Summoner et Strategist ne participaient pas à ces parties. Douze parties, c'est
un petit échantillon : mesurez à nouveau avant de réordonner les niveaux.

## 3. Les réglages

| bot | variable d'env. | clés |
|---|---|---|
| lecturer | `LECTURER_OPTS` | `exploration` 0.7, `maxDepth` 4, `priorVisits` 2.0, `rolloutPlies` 6, `projectPlies` 4, `passWeight` 0.5 |
| oracle | `ORACLE_OPTS` | 8 clés de recherche + 9 clés d'évaluation, voir le [document 4](04-oracle.md) |
| diviner | `DIVINER_OPTS` | `beliefCount` 3, `riskAversion` 0.35, `firstRoundSamples` 1 |
| summoner | `SUMMONER_OPTS` | **n'analyse rien** — `apply_overrides` se contente d'afficher la chaîne (`summoner.ml:19-23`) |
| strategist | *(aucune)* | pas d'`apply_overrides` du tout ; ses paramètres sont de simples constantes (`strategist.ml:148-172`) |

Le format est `key=value,key=value`, appliqué une seule fois au chargement du
module ; les clés inconnues déclenchent un avertissement. Tout est stocké dans
des `ref` globales au processus : il n'y a donc pas de réglage par siège, et
`ORACLE_OPTS` reconfigure aussi la feuille de Diviner et de Lecturer.

Deux variables de débogage existent : `ORACLE_DEBUG` (écran complet et
classement final, [document 4](04-oracle.md)) et `LECTURER_DEBUG` (plateau,
plus chaque enfant de la racine avec son nombre de visites et sa moyenne,
`lecturer.ml:267-273`). Diviner n'a pas de drapeau de débogage propre.

## 4. Lire les lignes de stats

Chaque bot de recherche affiche une ligne par coup quand `stats_log` est
actif — c'est le cas par défaut en tournoi, mais pas dans les front ends, où la
TUI redirige la sortie standard des bots vers `/dev/null` pour garder l'écran
propre.

```text
oracle-stats search playouts=60 wins=0 losses=0 sims=28640 rounds=2 spentMs=200/budgetMs=200 best=0.354 n=14 cmd=0> water.sprite at 5
lecturer-stats iterations=... wins=... losses=... sims=... spentMs=.../budgetMs=... best=... n=... cmd=...
diviner-stats beliefs=4 spentMs leftover cmd=0> fire.wave score=0.818
summoner-stats depth=3 spentMs=1000/budgetMs=1000 cmd=0> water.turtle at 0
```

Champs à connaître :

- `sims=` est le coût réel : le nombre d'appels à `Game.simulate`, dont chacun
  copie l'état et exécute la chaîne de fin de tour (la suite d'effets
  automatiques que le moteur déroule quand on passe : attaques, effets de
  maison, morts de créatures…). Comparez-le à `spentMs` pour savoir si un
  changement a réellement acheté de la précision ou s'est contenté de déplacer
  le travail.
- `rounds=` est l'étape du successive halving. `rounds=1` signifie que l'horloge
  a expiré pendant le premier tour et qu'**aucun candidat n'a été éliminé** —
  normal à faible budget. `rounds=129` signifie que la recherche a atteint le
  duel final et a dépensé le reste du budget à échantillonner les deux derniers
  candidats.
- `best=` est la valeur gagnante, dans `(−1, 1)` pour oracle/diviner/lecturer,
  et pour summoner le mélange `Heuris` de course et de long terme.
- `n=` est le nombre de rollouts du gagnant.
- `cmd=pass` sur une ligne oracle est légitime : passer peut faire gagner la
  partie via la chaîne de fin de tour (les effets automatiques déclenchés par
  le passage — un effet de maison peut par exemple tuer l'adversaire).

Deux aspérités connues dans ces lignes : `diviner-stats` affiche littéralement
les mots `spentMs` et `leftover` là où devraient figurer les temps écoulés, et
`strategist` conditionne sa ligne à `settings.logging` plutôt qu'à
`stats_log`, de sorte qu'elle ne s'affiche jamais en duel.

## 5. Mesurer un changement

Le pilote de tournoi est l'instrument de mesure. Il joue des parties complètes
et rapporte V/D/N (W/L/D) ainsi que l'Elo (K = 16) dans `summary.json`.

```sh
# the baseline: greedy always takes the cheapest legal card, so greedy vs
# greedy is not a signal.  Oracle vs greedy is.
dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 1000 --seed 1

# the interesting question for Diviner: what is honest information worth?
dune exec ./bin/main.exe -- --bots oracle,diviner --games 20 --budget 1000 --seed 1

# the full round robin
dune exec ./bin/main.exe -- --bots greedy,summoner,strategist,lecturer,oracle,diviner --games 4 --seed 1

# house matchups: both seats take the special
dune exec ./bin/main.exe -- --bots greedy,oracle --games 10 --special zen --seed 1

# replay a run's card and Elo tables without playing
dune exec ./bin/main.exe -- --mode summary --dir tournament-results/<timestamp>
```

**Résumé des deux sous-sections suivantes.** L'Elo n'est pas un taux de
victoire : +70 Elo valent environ 60 % de victoires, +170 environ 73 %. Et un
résultat sur 20 parties peut fluctuer de ±22 points ; il ne sert donc qu'à
repérer de grosses différences. Pour les petites, jouez des centaines de
parties ou comparez `sims=`. L'arithmétique suit, pour qui veut vérifier.

### Ce que signifient les deux nombres de `summary.json`

V/D/N est une proportion et n'appelle aucune explication. L'Elo, le classement
en ligne standard, mérite qu'on s'y arrête dix secondes, car ce nombre se lit
facilement — à tort — comme un taux de victoire.

```ocaml
ra' = ra +. (16.0 *. (sa -. (1.0 /. (1.0 +. (10. ** ((rb -. ra) /. 400.0))))))
```

`tournament.ml:115-116`, soit `R' = R + K·(S − E)` avec `K = 16`,
`S ∈ {1, ½, 0}` et

```text
E = 1 / (1 + 10^(−Δ/400))        the expected score against this opponent
```

`E` est une **espérance logistique, pas un taux de victoire**, et son échelle
est volontairement compressive :

| écart de classement Δ | score espéré `E` | estimation linéaire `50% + Δ/8` |
|---|---|---|
| 0 | 0.500 | 50% |
| 25 | 0.536 | 53% |
| 100 | 0.640 | 62.5% |
| 200 | 0.760 | 75% |
| 400 | 0.909 | 100% |

Un écart de 400 points est le point où le rapport vaut 10:1, d'où le diviseur :
il fait en sorte que « dix fois meilleur » signifie 400 points de classement.
Tout ce qui est en dessous est écrasé, de sorte qu'**un grand avantage en Elo
correspond à un taux de victoire plus faible que ce que le nombre laisse
croire à l'œil** — `summary.json` ne devrait jamais être cité sous forme de
pourcentage.

Exemple chiffré, avec deux bots neufs à 1500 :

```text
game 1, A wins:  E = 0.5,  ΔR = 16·(1 − 0.5)  = +8   ->  A 1508, B 1492
game 2, draw:    E_A = 1/(1 + 10^(−16/400)) = 0.5229
                 ΔR_A = 16·(0.5 − 0.5229)   = −0.37 ->  A 1507.6, B 1492.4
```

Deux choses ressortent de la seconde ligne. Un match nul coûte presque rien au
gagnant — la mise à jour s'auto-limite — et la paire dérive vers l'écart de
classement qui correspond réellement au taux de score observé :

```text
ΔR_steady = 400 · log10(p / (1 − p))
```

Ainsi un bot qui gagne **60 %** de ses parties se stabilise environ **70 Elo**
au-dessus de son adversaire, et non 200. Lu à l'envers, un résumé affichant
`+170` correspond à un score espéré de `1/(1 + 10^(−170/400)) = 0.727`, et non
aux 71 % que suggère la lecture linéaire.

### Combien de parties pour étayer une affirmation

Un taux de victoire sur `N` parties a pour erreur standard
`SE = √(p(1−p)/N)`. Comparer deux séries de `N` parties donne
`SE_diff = √(2p(1−p)/N)` ; pour déclarer réelle une différence `Δ` à 95 % de
confiance, il faut donc

```text
N  ≥  2 · 1.96² · p(1−p) / Δ²  =  1.92 / Δ²       at p = 0.5
```

| différence réelle | parties **par bras** | Elo correspondant |
|---|---|---|
| 10 points | ~192 | ~35 |
| 5 points | ~768 | ~17 |
| 2 points | ~4 800 | ~7 |

C'est l'arithmétique derrière « vingt parties au minimum ». Vingt parties à
`p = 0.5` ont `SE = 0.112`, donc l'intervalle de confiance à 95 % sur le
pourcentage affiché vaut environ **50 % ± 22 %** — un test de fumée, rien de
plus. Deux cents parties suffisent à résoudre un écart de 10 points ; il en
faut huit cents pour 5 points. Et comme les parties d'une même série partagent
des paires de bots, des decks et des schémas de sièges, ces chiffres sont à
considérer comme des bornes inférieures.

La conséquence pratique pour le réglage : un changement qui vaut moins d'environ
20 Elo ne peut pas être mesuré par un tournoi rapide. Il faut soit jouer le
nombre de parties nécessaire, soit mesurer quelque chose de moins coûteux et de
plus sensible — `sims=` par coup, nombre de tours de halving terminés à budget
fixe, ou valeurs de feuille dans `ORACLE_DEBUG` (document 4 §5). Ce sont ces
métriques qu'il faut itérer ; le nombre Elo est la dernière vérification, pas
la première.

La méthode, car les résultats bot contre bot sont bruités (liste de contrôle à
suivre) :

1. **Fixer `--seed`.** La graine pilote les donnes et les dés. La même graine
   donne les mêmes decks aux deux bots, ce qui retire la chance liée aux decks
   de la comparaison. L'ordre des sièges alterne à chaque partie
   (`ordered_pair`, `tournament.ml:368`).
2. **Utiliser `--games`, pas 1.** Vingt parties au minimum : un échantillon de
   6 parties noie régulièrement un vrai changement de ±5 % dans le bruit.
3. **Ne changer qu'une chose.** En particulier : ne pas retoucher `Eval` et le
   contrôleur dans la même expérience.
4. **Vérifier `sims=`, pas seulement les victoires.** Si un changement fait
   dépenser deux fois plus de simulations pour le même résultat, c'est une
   régression déguisée.
5. **Surveiller la courbe de budget.** Lancez à 100 ms et à 1000 ms. Un bot qui
   gagne largement à 1000 ms mais mal à 100 ms a une recherche qui a besoin
   d'un tour de halving complet pour montrer quoi que ce soit — bon à savoir
   avant que cela ne se manifeste comme un blocage dans l'UI.

Les suites de tests des bots verrouillent les propriétés faciles à casser :

```sh
dune test                       # everything
dune exec ./test/test_oracle.exe        # eval preferences + search sanity
dune exec ./test/test_diviner.exe       # risk aversion, world aggregation
dune exec ./test/test_lecturer.exe      # tree structure, budget, no mutation
dune exec ./test/heuris.exe             # prévision de course, inversion de signe entre joueurs
```

Le groupe `eval` de `test/test_oracle.ml` est le plus utile : il vérifie des
*ordres* de `Oracle.leaf_value` (les sprites font plus mal que les taureaux,
bloquer le titan est bon, un plateau large est plus menaçant qu'un plateau
étroit) plutôt que des valeurs exactes, si bien que le réajustement des poids
ne le casse pas. Conservez ce style si vous ajoutez des tests d'évaluation.

## 6. Rendre un bot plus fort, plus faible ou plus rapide

**Plus fort, à budget égal.** Acheter des rollouts : augmenter `horizon`,
diminuer `firstRoundSamples` pour que le premier tour de halving soit bon
marché et que davantage de tours se terminent. Passer `rootCards` de 8 à 4
divise la largeur par deux et double à peu près la profondeur de la recherche à
temps égal.

**Plus rapide.** `horizon` 5 → 3, `projectPlies` 4 → 2, `placementsPerCard`
2 → 1. Chacune divise grosso modo par deux le coût par rollout ; vérifiez le
coût en force à budget égal avant de supposer qu'il est nul.

**Plus faible exprès.** Ramener `rootCards` à 1 — la recherche dégénère en
« valeur de screening », ce qui fait un débutant raisonnable. Ou fixer
`horizon=0`, qui ne laisse que les quatre passes de projection.

**Un adversaire différent.** Changer la politique de base, pas l'évaluation.
`Oracle.base_moves` (`placements`, `oracle.ml:152-188`) est l'endroit où le bot
décide de ce qu'un adversaire ferait plausiblement ; faire en sorte que le
glouton des rollouts prenne occasionnellement le deuxième meilleur coup rend le
bot plus robuste, et le changement `reply_weights` du document 4 fait la même
chose à moindre coût.

## 7. Pièges

- **`strategist` a un état.** `Strategist.mover` alloue le bandit une seule
  fois et le conserve pendant toute la partie (`strategist.ml:491-493`).
  Donner à chaque siège son propre mover — c'est ce que fait `Table.movers_of`
  (`table.ml:179`).
- **`summoner` modifie ses propres réglages.** `search_pass` réécrit
  temporairement `settings.duration` autour de la recherche interne et le
  restaure dans un `Fun.protect` (`summoner.ml:212-215`). Partager un record
  `settings` entre des threads serait un bug.
- **Le commentaire de doc de `Tournament.mover` est périmé.** Il dit que
  `[apprentice]` est greedy ; le code associe `"apprentice"` et `"gambler"` à
  `Lecturer` (`tournament.ml:374,378`), et `bot_ids` (`:384`) ne les liste pas.
- **`Heuris.mana_lead` est exportée et jamais appelée.**
- **Ne pas comparer sur une seule graine.** Voir §5.
- **Le plateau est visible de tous.** Avant de conclure qu'un bot joue
  n'importe quoi face à une carte cachée, vérifiez s'il pouvait la voir. Les
  deux cas documentés se trouvent dans la section *Limitations* du README et
  dans le [document 2](02-game-view.md) §1.

## Pour aller plus loin

- Ajouter un bot : le squelette du [document 2](02-game-view.md) §6, plus la
  liste de contrôle d'enregistrement.
- Faire mieux comprendre une maison à un bot : les poids d'évaluation
  ([document 4](04-oracle.md) §7) et les tables de tempo dans
  `strategist.ml:148-210`.
- Gérer correctement l'information cachée : [document 5](05-diviner.md) §8,
  qui liste ce que Diviner ne fait pas encore.
- Les idées sous-jacentes : [document 3](03-search-concepts.md).

Et les documents voisins du projet : [état de la migration](../tech/migration.md),
[écarts de portage des maisons](../tech/house-port-mismatches.md),
[plan de portage des effets de sorts](../tech/effects-port-plan.md),
[packaging](../tech/packaging.md).