# 5. Le bot Diviner

`lib/bots/diviner.ml`, environ 220 lignes, plus les fonctions de devinette de
deck qu'il partage avec tous les autres bots dans `lib/bots/belief.ml`. Portage
de `priv/sp/bot/diviner/DivinerBot.scala`.

> **L'idée en une phrase :** lancer la recherche d'Oracle plusieurs fois sur
> plusieurs *hypothèses* concernant la main cachée de l'adversaire, puis choisir
> le coup qui est le meilleur en moyenne **et le plus sûr dans la pire
> hypothèse**.

Diviner tient en moins de 300 lignes parce que c'est Oracle plus une seule
idée : que se passe-t-il quand la position contient une information que l'on
ne possède pas.

## 1. Le problème

La recherche d'Oracle duplique l'état et joue les vingt cartes de l'adversaire
— la seule chose que le joueur humain face au bot ne peut réellement pas voir.
Elle utilise un unique deck deviné (`Belief.guess`, l'union de quelques donnes) :
elle optimise donc contre une seule image fusionnée d'un espace immense et
présente le résultat comme s'il était certain.

Diviner, lui, garde les suppositions séparées. Sortie réelle d'une partie
Oracle contre Diviner, 100 ms par coup :

```text
diviner-stats beliefs=3 spentMs leftover cmd=1> earth.hugespider at 4 score=0.115
diviner-stats beliefs=3 spentMs leftover cmd=1> Wraith at 2 score=0.370
diviner-stats beliefs=3 spentMs leftover cmd=1> air.cloud at 1 score=0.571
diviner-stats beliefs=3 spentMs leftover cmd=1> fire.orc at 0 score=0.737
```

`beliefs=3` est le `belief_count` par défaut : trois donnes échantillonnées, et
jamais le vrai deck (§3).

(`spentMs leftover` sans valeurs est un petit défaut de formatage dans la ligne
de statistiques : la chaîne de format contient les mots littéraux à la place des
emplacements du temps écoulé. Cela ne coûte rien à l'exécution.)

## 2. Mondes de croyance

Un **monde** est un état de jeu complet et cohérent qui ne diffère de la
position réelle que par la partie devinée. Concrètement, un monde est une paire
`(game_state, game_desc)` dans laquelle le `player_desc` de l'adversaire a été
remplacé par une donne légale échantillonnée, tandis que le plateau, le mana,
les points de vie, la main du bot et tous les effets de maison restent
exactement tels quels.

```text
                    la position réelle
                          |
        +-----------------+------------------+
        |                 |                  |
     monde 0          monde 1    ...      monde 3
   donne échantillonnée donne échantillonnée donne échantillonnée
        |                 |                  |
   Oracle.Search      Oracle.Search        Oracle.Search
        |                 |                  |
        +--------+--------+---------+--------+
                 |   un score par monde     |
                 +--------------------------+
                            |
                   moyenne moins pénalité de risque
```

Chaque monde possède son propre `Oracle.Search.t` — un moteur Oracle
indépendant sur un `desc` fixe (`Diviner.execute`) :

```ocaml
let worlds =
  List.map (fun belief ->
      let st, d = Belief.with_belief start desc pid belief in
      { start = st; desc = d; ai = Oracle.Search.create ~desc:d ~bot_id:pid })
    beliefs
```

C'est toute la raison pour laquelle `Oracle.Search` est un module public à part
(`module Search` dans `oracle.mli`) : il empaquette « un Oracle sur un desc
fixe » sous forme de valeur, de sorte que Diviner peut en faire tourner
plusieurs à la fois sans aucun état global.

## 3. Échantillonner les croyances

L'échantillonnage a trois parties : se souvenir des cartes adverses déjà
révélées, tirer des decks légaux qui les contiennent, et garder ces decks
jusqu'à ce qu'on apprenne du nouveau. Tout, sauf le cache, se trouve dans
`Belief`, car tous les bots en ont désormais besoin. La différence tient à ce
que chaque bot fait des donnes :

| bot | donnes par supposition | usage |
|---|---|---|
| Summoner, Strategist, Lecturer, Oracle | `Belief.superset_samples` (4) | fusionnées maison par maison en un seul deck surdimensionné (`Belief.superset`), comme le `CardGuess.createAIPlayer` de Scala |
| Diviner | `beliefCount` (3) | gardées séparées, un monde chacune |

Aucun ne cherche jamais sur le vrai deck de l'adversaire.

### Se souvenir de ce qui a été vu

L'état du jeu ne garde aucun historique de ce que l'adversaire a joué ; le bot
tient donc le sien. `mover` appelle `Belief.seen_cards`, qui enveloppe
`u.listener` une fois par partie et par siège (`track_opponent`) et enregistre
chaque carte adverse apparaissant dans un événement `Ev_summon`,
`Ev_spell_played` ou `Ev_submitted` :

```ocaml
u.listener <-
  (fun e ->
    (match e with
    | Ev_summon { player; creature; _ } when player <> pid -> note creature.c_card
    | Ev_spell_played c when c.cmd_player <> pid -> note c.cmd_card
    | Ev_submitted { player; command = Some c; _ } when player <> pid -> note c.cmd_card
    | _ -> ());
    prev e)
```

Enregistrer les événements (plutôt que lire le plateau) permet de garder dans
l'ensemble une créature morte depuis, ou un sort déjà résolu. `seen_cards`
ajoute aussi, en solution de repli, ce qui se trouve sur le plateau adverse à
l'instant présent. Il y a un traqueur par siège : quand deux bots partagent une
partie, chacun enregistre son propre adversaire ; tous les traqueurs sont
abandonnés quand l'updater change, c'est-à-dire à chaque nouvelle partie.

`required_indexes` transforme ensuite ces cartes en index de cartes par maison
(numérotés à partir de 1) pour le deck de l'adversaire. Les cartes qui n'en
font pas partie — les jetons invoqués, par exemple — ne correspondent à aucune
maison et sont écartées.

### Tirer des decks cohérents

```ocaml
let sample ~desc ~bot_id ~seen ~n rng =
  let bot = desc.gd_players.(bot_id) in
  let opp = desc.gd_players.(other bot_id) in
  let special = opp.pd_houses.(4).phd_house in
  let require = required_indexes opp seen in
  let deal require =
    try Some (Shuffle.create_player ~rng ~exclude:bot ~require ~first:false ~special Catalog.basics)
    with Failure _ -> None
  in
  ...
```

Cinq décisions, chacune importante :

**Échantillonnage par rejet via le vrai constructeur de decks.** Plutôt que
d'inventer des mains, Diviner appelle `Shuffle.create_player` — la fonction même
qui a construit la partie au départ — et intercepte son `Failure` quand elle ne
peut pas construire de deck légal. Chaque monde est donc un deck **légal** :
quatre cartes de chaque maison de base plus quatre de la maison spéciale,
respectant les règles de générateur de mana, de finisseur et de nombre de
défenses de `lib/game/shuffle.ml`.

**`~require` : les cartes vues sont imposées.** `Shuffle.create_player` accepte
un `require` optionnel, une liste d'index de cartes par maison (feu, eau, air,
terre, spéciale). Les candidats de maison de base et les choix de maison
spéciale qui ne les contiennent pas tous sont filtrés avant la recherche, de
sorte que chaque deck échantillonné contient toutes les cartes que l'adversaire
a montrées. Si les cartes vues ne peuvent former aucun deck légal,
`Belief.sample` se rabat sur un échantillonnage sans contrainte plutôt que de
ne renvoyer aucun monde.

**La vraie maison spéciale de l'adversaire.** Le bot la lit directement dans le
desc (`opp.pd_houses.(4)`) — c'est une information réellement publique, choisie
au lobby. C'est ce qui rend l'espace des croyances assez fini pour être
échantillonné. (Le `~special` de cet appel est un raccourci OCaml :
`~special:special`, avec `Catalog.basics` comme dernier argument `house list`.)

**`~exclude:bot`.** Les index de cartes que le bot détient lui-même sont
retirés du pool, de sorte qu'aucun monde ne contient une carte présente dans
les deux mains.

**Distinction, et jamais le vrai deck.** Les doublons sont détectés par la
liste triée des identifiants de cartes de la donne et rééchantillonnés. Le desc
adverse *réel* n'est pas un monde, comme dans Scala : les mondes sont
exactement les donnes échantillonnées, d'où `beliefs=3` dans la ligne de
statistiques avec `belief_count = 3` par défaut. (Les premières versions du
portage ajoutaient le vrai deck en tête (`opp :: sampled`), ce qui faisait
tricher Diviner ; c'est retiré.) Le monde `primary`, qui fournit la liste des
coups légaux, est le premier deck échantillonné. `execute` ne se rabat sur le
vrai deck que si aucun deck légal ne peut être tiré.

### Garder les mondes

Rééchantillonner à chaque coup ferait sauter la main imaginée de l'adversaire
d'un tour à l'autre. `Diviner.beliefs` met les donnes en cache par desc de
partie, siège et ensemble de cartes vues (`Belief.seen_key`), comme le
`cachedBeliefs` de Scala :

```ocaml
let beliefs ~desc ~pid ~seen rng =
  let key = Belief.seen_key seen in
  match List.find_opt (fun (d, p, k, _) -> d == desc && p = pid && k = key) !cached with
  | Some (_, _, _, b) -> b
  | None -> (* Belief.sample, stocker, renvoyer *)
```

Les mondes ne changent que lorsque l'adversaire révèle une nouvelle carte.
`Belief.guess` met en cache de la même façon le deck surdimensionné des bots à
deck unique.

## 4. Greffer un monde

```ocaml
let with_belief visible desc bot_id belief =
  let opp = other bot_id in
  let p = visible.players.(opp) in
  let players = Array.copy visible.players in
  players.(opp) <- { p with ps_desc = { init_desc = belief; desc_mods = p.ps_desc.desc_mods } };
  let gd = { gd_players = Array.copy desc.gd_players } in
  gd.gd_players.(opp) <- belief;
  ({ players }, gd)
```

Deux enregistrements doivent changer ensemble, et cela mérite d'être compris
plutôt que copié :

- la moitié **état**, pour que `Game.legal_commands` et `Game.simulate`
  énumèrent les cartes devinées quand c'est le tour de l'adversaire ;
- la moitié **desc**, pour que `Game.player_houses` résolve les coûts des
  cartes devinées. Le desc est par ailleurs fixe pour toute la partie — un
  monde de croyance est le seul endroit où il change.

Tout le reste est intact, y compris `desc_mods` (les modificateurs de coût
actifs accumulés pendant la partie réelle) et l'ensemble du plateau. La main du
bot lui-même n'est jamais devinée.

Deux limites honnêtes de cette approche :

- **Le plateau n'est jamais deviné.** Diviner partage l'angle mort de tous les
  bots sur le plateau : un Shade Stalker non révélé figure toujours dans
  `ps_slots` avec son attaque et sa vie.
- **Les croyances sont gardées jusqu'à la prochaine carte révélée, et
  contraintes.** `execute` réutilise les mêmes échantillons tant que les cartes
  vues ne changent pas et rappelle `Belief.sample` quand une nouvelle carte
  apparaît, sans pondération reportée. Chaque échantillon est forcé de
  contenir les cartes adverses vues jusque-là (§3). Un monde ne peut plus
  contredire une carte que l'adversaire a jouée ; il peut encore être
  incohérent de façons plus subtiles (une carte qu'il a choisi de *ne pas*
  jouer ne porte ici aucune information). Un véritable filtrage particulaire —
  pondérer les mondes selon la vraisemblance du jeu observé et les reporter
  d'un tour à l'autre — est l'amélioration suivante.

(Une petite inefficacité, sans conséquence : `create_player` calcule aussi le
mana d'ouverture de la donne, que `Belief.sample` ignore. L'argument
`first:false` est donc sans importance.)

## 5. Rechercher dans les mondes

Le contrôleur est celui d'Oracle, inchangé. Ce qui diffère, c'est l'axe.

**Le screening (criblage) s'exécute par monde** (la boucle `screen` de
`Diviner.search`) :

```ocaml
List.iteri (fun w world ->
    if now_ms () < deadline then (
      let step = Oracle.Search.play world.ai world.start bot_id cmd in
      if step.ended <> Some bot_id then all_won := false;
      values.(w) <- Oracle.Search.value world.ai step ~ply:1;
      seen.(w) <- true))
  worlds;
if !all_won && Array.for_all Fun.id seen then rows := [ `Lethal cmd ]
```

Le même coup à la racine est joué une fois **dans chaque monde** : une valeur
de screening est donc un vecteur, pas un scalaire. Deux conséquences :

- **Le test « lethal » (victoire immédiate) doit tenir dans tous les mondes** —
  `all_won` et `Array.for_all Fun.id seen`. Gagner dans trois hypothèses sur
  quatre n'est pas une victoire ; c'est un pari, et la pénalité de risque
  ci-dessous l'écartera.
- **Le budget peut expirer au milieu d'une commande.** L'échéance est vérifiée
  dans la boucle sur les mondes, donc `screened.(w)` est un drapeau par monde et
  une commande peut être évaluée dans deux mondes et rester aveugle dans le
  troisième. `world_mean_of` gère les trois cas (§6).

**La shortlist (présélection) et les tours de halving** sont ceux d'Oracle à
l'identique, y compris `Oracle.Params.root_cards`. La boucle interne se
contente d'itérer sur les mondes (la boucle de halving à la fin de
`Diviner.search`) :

```ocaml
List.iteri (fun w world ->
    List.iter (fun c ->
      if !time_left then (
        let rank = c.cnt.(w) mod Array.length !Oracle.Params.reply_weights in
        add c w (Oracle.Search.playout world.ai ~start:world.start ~root:c.cmd ~reply_rank:rank);
        time_left := now_ms () < deadline))
      !survivors)
  worlds
```

Un échantillon signifie *un rollout dans un monde* ; la comptabilité par
candidat est donc une matrice (`sum`, `cnt`, `screen`, `screened` — tous des
tableaux indexés par monde) plutôt que le scalaire plus trois rangs d'Oracle.
Le rang de réponse cycle indépendamment par monde, si bien qu'un même candidat
peut être éprouvé avec la meilleure réponse de l'adversaire dans le monde 0 et
sa troisième meilleure dans le monde 1.

**Coût.** Avec `W` mondes, un échantillon d'un candidat coûte `W` rollouts.
Pour le même budget de 200 ms, Diviner effectue donc environ un quart des
tours de halving d'Oracle et finit par comparer moins de candidats. C'est tout
le compromis : le même budget achète soit de la confiance dans un monde, soit
de l'étendue sur plusieurs.

## 6. Agréger les mondes

Moyenne par monde, trois cas (`world_mean_of`) :

```ocaml
let world_mean_of c w =
  if c.cnt.(w) <= 0 then c.screen.(w)                                   (* never rolled out *)
  else if not c.screened.(w) then c.sum.(w) /. float_of_int c.cnt.(w)   (* blind: playouts only *)
  else Eval.world_mean ~screen:c.screen.(w) ~sum:c.sum.(w) ~n:c.cnt.(w) (* both: blend them *)
```

et `Eval.world_mean` est le mélange screening/playout d'Oracle, inchangé :

```ocaml
let world_mean ~screen ~sum ~n =
  if n <= 0 then screen else ((!Oracle.Params.screen_weight *. screen) +. (sum /. float_of_int n))
                              /. (!Oracle.Params.screen_weight +. 1.)
```

Vient ensuite le score sensible au risque (`Eval.score`) :

```ocaml
let score ?(aversion = !Params.risk_aversion) means =
  match means with
  | [] -> neg_infinity
  | xs ->
      let n = float_of_int (List.length xs) in
      let mean = List.fold_left ( +. ) 0. xs /. n in
      mean -. (aversion *. (mean -. List.fold_left min infinity xs))
```

**moyenne − aversion × (moyenne − pire monde).** La pénalité est
proportionnelle à la dispersion et disparaît quand tous les mondes
s'accordent : des mondes qui racontent la même histoire ne coûtent donc rien à
ajouter.

### Exemples détaillés (aversion 0.35)

| moyennes par monde | moyenne | pire | score |
|---|---|---|---|
| `[0.9; −1.0]` | −0.05 | −1.0 | −0.05 − 0.35 × 0.95 = **−0.383** |
| `[0.2; 0.2]` | 0.2 | 0.2 | 0.2 − 0 = **0.200** |
| `[0.5; 0.5; −1.0]` | 0.0 | −1.0 | 0 − 0.35 × 1.0 = **−0.350** |
| `[0.5; 0.5; 0.5]` | 0.5 | 0.5 | **0.500** |

Les deux premières lignes forment le test `eval_prefers_stable` de
`test/test_diviner.ml` : un coup excellent dans une hypothèse et désastreux
dans une autre perd face à un coup médiocre partout. C'est tout l'enjeu — le
troisième monde pourrait être le vrai, et un coup qui repose sur le fait que
l'adversaire détienne exactement une carte n'est pas un coup, c'est un pari.

Notons l'interaction entre `belief_count` et `risk_aversion`. Un mauvais monde
sur quatre est dilué :

```text
[0.9; 0.9; 0.9; -1.0]   mean 0.675  worst -1.0   score 0.675 - 0.35*1.675 = 0.089
[0.9; -1.0]              mean -0.05  worst -1.0   score -0.383
```

Même désaccord, mondes différents, pénalité quatre fois plus faible. Davantage
de mondes rendent Diviner plus robuste *et* plus tolérant envers n'importe
quelle hypothèse inquiétante isolée ; augmenter `riskAversion` en même temps
que `beliefCount` s'annule en partie.

Quant à `riskAversion = 0`, il ramène tout à la simple espérance — c'est-à-dire
Oracle avec du travail gaspillé. `1.0` donne un minimax pur sur les mondes, bien
trop timoré pour jouer. La valeur par défaut `0.35` est le compromis.

## 7. Réglage

```sh
DIVINER_OPTS=riskAversion=0.7,beliefCount=5 \
  dune exec ./bin/main.exe -- --bots greedy,diviner --games 20 --budget 500 --seed 1
```

| clé | défaut | effet |
|---|---|---|
| `beliefCount` | 3 | donnes échantillonnées ; un monde chacune, jamais le vrai deck |
| `riskAversion` | 0.35 | poids de la pénalité vers le pire monde |
| `firstRoundSamples` | 1 | rollouts par candidat et par monde au tour 1 (Oracle en utilise 6, car un échantillon de Diviner coûte `W` rollouts) |

Diviner hérite aussi de `ORACLE_OPTS` pour `rootCards`, `replyWeights` et
`screenWeight` — ils sont lus dans `Oracle.Params` à l'exécution, donc définir
`ORACLE_OPTS` modifie le comportement de Diviner. `horizon`, `projectPlies`
(nombre de plies, c'est-à-dire de demi-coups simulés) et tous les poids
d'évaluation s'appliquent aussi à Diviner, via `Oracle.Search` et
`Oracle.Eval`.

Expériences à essayer :

```sh
# one merged guess vs several separate ones  (Oracle vs Diviner, same budget)
dune exec ./bin/main.exe -- --bots oracle,diviner --games 20 --budget 500 --seed 1

# timid vs trusting
DIVINER_OPTS=riskAversion=0.0 dune exec ./bin/main.exe -- --bots greedy,diviner --games 20 --budget 500 --seed 1
DIVINER_OPTS=riskAversion=1.0 dune exec ./bin/main.exe -- --bots greedy,diviner --games 20 --budget 500 --seed 1

# more worlds at a bigger budget
DIVINER_OPTS=beliefCount=7 dune exec ./bin/main.exe -- --bots greedy,diviner --games 10 --budget 2000 --seed 1
```

Il n'existe pas de `DIVINER_DEBUG`. Pour observer son raisonnement, on peut
activer les statistiques d'Oracle de l'extérieur :

```sh
ORACLE_DEBUG=1 dune exec ./bin/main.exe -- --bots diviner,greedy --games 1 --budget 100
```

`ORACLE_DEBUG` est lu dans `Oracle.execute`, que Diviner n'appelle pas ; cette
commande n'affiche donc rien. Pour instrumenter Diviner, il faut afficher
depuis `cand_score` — c'est une seule fonction et elle voit toute la matrice
par monde.

## 8. Limites

- **Le plateau reste réel.** Les croyances ne couvrent que la main de
  l'adversaire. L'angle mort sur le Stalker caché est partagé par tous les
  bots.
- **Seules les cartes révélées mettent à jour les croyances.** Les cartes vues
  sont imposées dans chaque monde, mais rien n'est déduit de ce que
  l'adversaire a gardé en main ni de sa façon de jouer. Le traqueur de cartes
  vues exige aussi que `u.listener` reste enveloppé : un appelant qui le
  remplace en cours de partie perd l'historique des événements et se rabat sur
  les cartes du plateau.
- **A priori uniforme sur les decks légaux.** `Shuffle` encode la légalité et
  quelques règles propres aux maisons, pas la façon dont les *joueurs*
  construisent leurs decks. Un joueur qui prend toujours la créature la plus
  forte serait très mal échantillonné.
- **Un mauvais échantillon reste.** Comme les mondes sont en cache jusqu'à la
  prochaine carte révélée, un jeu de donnes malchanceux dure plusieurs tours
  si l'adversaire rejoue des cartes déjà vues. Augmenter `beliefCount` y
  remédie.
- **Le coût est linéaire en nombre de mondes.** Doubler `beliefCount` divise
  par deux le nombre de tours de halving effectués dans le même budget. Que ce
  soit gagnant dépend de la mesure dans laquelle la position repose réellement
  sur des cartes cachées.
- **C'est un bot, pas une étude.** Savoir si des mondes séparés battent une
  supposition fusionnée mérite d'être mesuré plutôt que supposé — c'est le rôle
  de l'affrontement Oracle contre Diviner ci-dessus.

Suite : [06-bot-landscape.fr.md](06-bot-landscape.fr.md).