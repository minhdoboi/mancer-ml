# 3. Les concepts de recherche

Les algorithmes, présentés de façon générique. Rien ici n'est propre à un
fichier ; la dernière section rattache chaque idée au bot qui l'implémente. À
lire avant les documents 4 et 5, ou à sauter si le vocabulaire est déjà connu.

## 1. Pourquoi ne pas simplement explorer l'arbre ?

La conception évidente — construire l'arbre de jeu, faire du minimax ou du MCTS
à profondeur fixe — échoue ici pour deux raisons.

**Le facteur de branchement est énorme.** Une main compte quatre cartes de base
plus une spéciale ; chaque carte a jusqu'à six cibles. Avec trois cartes
jouables, `Game.legal_commands` renvoie une vingtaine de commandes. Une
profondeur de 5 donnerait 20⁵ ≈ 3,2 millions de positions, et chaque position
coûte un `Game.simulate` complet — une copie d'état plus toute la chaîne de fin
de tour. Même une implémentation parfaite prendrait des minutes.

**Les feuilles ne sont connues qu'à moitié.** Les points de vie ne sont pas
l'issue de ce jeu ; personne ne gagne au tour 3. La valeur d'une feuille doit
être *jugée*, et juger suppose une opinion sur ce qui se passe ensuite.

Tous les bots font donc le même compromis : **ne pas chercher en profondeur,
chercher large et peu profond, et rester honnête sur la feuille.** C'est tout
l'espace de conception, et les bots ne diffèrent que par la manière de
dépenser leur budget à l'intérieur.

## 2. Évaluation des feuilles

Un **score de feuille** associe un nombre à une position. Exigences dans ce
jeu :

- **Borné.** `(-1, 1)` ou `[0, 1]`, pour que les moyennes de nombreux
  échantillons restent interprétables et qu'une valeur aberrante ne domine pas.
- **Continu et monotone.** De petites différences doivent produire de petites
  différences de score, faute de quoi la moyenne n'a pas de sens.
- **Centré sur zéro.** Avantage du bot, et non « bon pour le joueur 0 » —
  sinon la recherche doit inverser le signe selon le tour, ce qui est
  précisément la source des bugs de signe.

`Oracle.Eval.score` satisfait les trois avec `tanh (raw / 60)` : la somme brute
de six termes pondérés n'est pas bornée, `tanh` l'écrase dans `(-1, 1)`, et le
`/ 60` est l'échelle à laquelle une position nettement gagnante sature. Une
valeur brute de +60 donne +0,76, pas +1,0 ; il faut une situation écrasante
pour approcher 1.

L'alternative est une **projection peu profonde** : au lieu de deviner à partir
de la position, la pousser de quelques plies déterministes (un *ply* est un
coup d'un seul camp ; ici, quatre passes) et évaluer le résultat. Les passes
sont presque gratuites — chacune est un `Game.simulate` — et elles forcent la
résolution des attaques en attente et de la croissance de mana, ce qui retire
une grande part du bruit d'une feuille en pleine transition. `Heuris.score`
prend la voie opposée : il simule un *combat* de 12 tours à l'intérieur de
l'évaluation, ignore totalement les cartes et se demande « qui gagne la course
à partir d'ici ».

## 3. Rollouts

Un **rollout** est une courte partie jouée par une **politique de base** peu
coûteuse. Sa valeur est le score de feuille en fin de partie. Les rollouts sont
le moyen d'acheter de la profondeur sans la payer.

```text
   root move
     |
     +--> opponent reply        |
     |    bot move              |  <- each simulated through the real engine
     |    opponent reply        |
     |    ... horizon plies     |
     |                          |
     +--> leaf score in (-1, 1)
```

Trois paramètres définissent un rollout, et chaque bot règle les trois
différemment :

| paramètre | Oracle / Diviner | Lecturer | Summoner / Strategist |
|---|---|---|---|
| politique de base | **gloutonne** (*greedy*) — l'argmax du score de feuille sur les coups élagués | **aléatoire**, avec un poids de passe | **gloutonne** sur `Heuris` |
| horizon | 5 plies | 6 plies | borné par la profondeur, 2–3 pour Strategist |
| feuille | 4 passes, puis `Oracle.Eval.score` | 4 passes, puis `Oracle.Eval.score` | directement la valeur `Heuris` du nœud |

Les politiques de base gloutonnes donnent des rollouts *corrélés* — tous les
échantillons d'un coup partagent les mêmes biais — ce qui est économique et
suffisant pour un jugement à un ply. Les politiques aléatoires donnent un bruit
*non biaisé*, ce dont le MCTS a besoin pour que les nombres de visites aient un
sens.

## 4. Algorithmes anytime et horloge murale

Chaque bot reçoit un budget en millisecondes et est testé contre
`now_ms () < deadline` dans sa boucle. Ils sont donc **anytime** : ils doivent
renvoyer une réponse utilisable quel que soit le moment où on les interrompt.

La conséquence est que le levier est l'*algorithme*, non la *profondeur*. On ne
peut pas demander « cherche à profondeur 5 » ; on demande « continue
d'échantillonner jusqu'à la fin du temps imparti, et fais compter chaque
échantillon ». Cela favorise les conceptions dont la précision s'améliore
progressivement avec le nombre d'échantillons — les méthodes de Monte Carlo —
plutôt que celles où l'on termine ou l'on n'a rien.

## 5. Élimination successive (successive halving)

Le contrôleur utilisé par Oracle et Diviner. C'est un ordonnancement de bandit
manchot à bras multiples, et c'est l'idée la plus propre de ce code.

On part de *k* candidats. On donne à chacun le même petit budget. On écarte la
moitié la plus faible. On double le budget par candidat. On répète jusqu'à ce
qu'il en reste un ou deux, puis on continue d'échantillonner la paire finale
jusqu'à épuisement du temps.

```text
  round 1:  [A B C D E F G H | pass]   9 candidates, 6 samples each   -> 54 rollouts
            keep 5
  round 2:  5 candidates, 12 samples each                          -> 60 rollouts
            keep 3
  round 3:  3 candidates, 24 samples each                          -> 72 rollouts
            keep 2
  round 4+: 2 candidates, 1 sample per round, until the deadline
```

Dans le code, la boucle d'Oracle se trouve en `lib/bots/oracle.ml:347-366` :

```ocaml
let keep = if n > 2 then (n + 1) / 2 else 2 in
survivors := !survivors |> List.sort (fun a b -> compare (cand_mean b) (cand_mean a)) |> take keep;
if List.length !survivors = 2 then per_round := 1 else per_round := !per_round * 2
```

Noter `keep = 2` quand `n <= 2` : les deux derniers ne sont **jamais**
éliminés, ils continuent simplement d'accumuler des échantillons. Anytime par
construction — on dispose toujours d'un classement, et la dernière phase est un
pur duel en face-à-face.

Pourquoi ne pas donner le même budget à chaque coup ? Parce que les mauvais
coups sont plus nombreux que les bons : avec 9 candidats et 100 rollouts fixes,
on en dépense environ 11 pour chacun et la plupart sont des perdants évidents.
Le halving dépense le budget là où la décision est encore ouverte. Si la
réponse est déjà connue — la passe de screening — tout l'ordonnancement est
bon marché.

Ce que le successive halving ne sait pas faire, c'est réutiliser : il jette
complètement les statistiques des candidats éliminés. C'est le compromis face
au MCTS, qui conserve tout ce qu'il a appris dans un arbre.

## 6. Modéliser l'adversaire sans résoudre le jeu

On ne peut pas se permettre de demander « quelle est la meilleure réponse de
l'adversaire ? » — c'est une recherche à part entière. Oracle l'approxime de
trois façons à la fois.

**Screening (criblage).** Avant tout rollout, chaque coup légal est joué une
fois et évalué (`oracle.ml:277-284`). C'est la première coupe bon marché qui
rend le reste abordable, et c'est là que meurent la plupart des candidats
écartés. La valeur de screening est aussi conservée et mélangée à la moyenne
finale, ce qui explique qu'un coup qui paraît mauvais à la lecture superficielle
ne soit pas aussitôt jeté.

**Réponses classées.** Dans un rollout, la *première* réponse de l'adversaire
n'est ni tirée au hasard ni prise de façon gloutonne — le rollout est exécuté à
un rang choisi (`nth_best scored reply_rank`, `oracle.ml:336`). Les
échantillons consécutifs parcourent `reply_rank = n mod 3` avec les poids
`[0.60; 0.25; 0.15]`, et le score du candidat est la moyenne pondérée des
moyennes par rang, les rangs non joués retombant sur la valeur de screening :

```ocaml
let played = !acc +. (c.screen *. (1.0 -. !w))     (* oracle.ml:117 *)
```

Ainsi, « la meilleure réponse de l'adversaire est mauvaise pour nous » ne vaut
crédit plein que si cela tient aussi face à ses 2ᵉ et 3ᵉ meilleures réponses.
C'est une approximation économique d'un minimax souple : jouer surtout la
meilleure ligne de l'adversaire, mais relativiser le coup si seule cette ligne
lui est défavorable.

**Glouton ensuite.** Les coups du bot et tous les coups adverses suivants sont
l'argmax du score de feuille (`max_by snd scored`, `oracle.ml:337`). Personne ne
modélise une stratégie adverse profonde ; on modélise la suivante.

## 7. UCT et MCTS

Le contrôleur de Lecturer, et le seul véritable arbre du code. Quatre étapes
répétées :

```text
  SELECT     walk down from the root, always taking the child that maximises
             UCT = mean(child) + c * sqrt( ln N(parent) / n(child) )
  EXPAND    add the children of the first node with none
  SIMULATE  play the rest randomly to the horizon, score the leaf
  BACKPROP   add one visit to every node on the path, add the (signed) reward
```

La formule réelle (`lecturer.ml:136-137`) :

```ocaml
let uct parent_visits (c : node) = mean c +. (!Params.exploration *. sqrt (log (max 1.0 parent_visits) /. c.visits))
```

Le terme d'exploitation est la récompense moyenne de l'enfant ; le terme
d'exploration décroît avec le nombre de visites de l'enfant, de sorte qu'un
coup bien essayé cesse d'être réessayé et qu'un coup négligé reçoit un nouveau
regard. `exploration` vaut 0,7 par défaut.

Trois détails le font fonctionner ici :

- **A priori (priors).** Chaque nouvel enfant est créé avec
  `prior_visits = 2.0` visites virtuelles à sa valeur heuristique
  (`lecturer.ml:112-130`), de sorte que la toute première comparaison UCT est
  informée et non arbitraire. Sans a priori, les premières visites ne sont que
  du bruit.
- **Remontée negamax.** Chaque nœud stocke les récompenses du point de vue de
  celui qui a *joué* son coup (`n.mover`), et `signed ~bot_id r n.mover`
  inverse le signe à chaque nœud en remontant. Un seul nombre sert les deux
  joueurs.
- **Branchement depuis `base_moves`.** Seule la racine se développe sur
  l'ensemble de `Game.legal_commands` ; tout ce qui est plus profond utilise
  l'ensemble élagué d'Oracle. L'arbre reste abordable et le code est partagé.

Lecturer choisit son coup par **nombre de visites**, non par valeur moyenne — le
enfant de la racine le plus visité est celui sur lequel l'arbre s'accorde.

## 8. Recherche meilleur-d'abord et approfondissement itératif

Summoner (et Strategist, qui en reprend la forme) est une recherche arborescente
plus simple :

- **Approfondissement itératif** : reconstruire de zéro à la profondeur 2, 3,
  4, … tant que le budget dure, et n'accepter la réponse d'une passe que si
  cette passe s'est *entièrement stabilisée* (`summoner.ml:242-269`). Une passe
  inachevée est écartée plutôt que crue.
- **Meilleur-d'abord au sein d'une passe** : au lieu d'un ordre fixe,
  `should_open` et `contender` choisissent le nœud non développé le plus
  prometteur, de sorte que les victoires faciles sont résolues en premier.
- **Remontée min/max selon le camp au trait** (`pull`, `summoner.ml:96-113`),
  l'alternative classique au negamax.
- **Actualisation gamma** sur les valeurs non terminales
  (`plan.gamma ** depth`, `strategist.ml:308`), afin que « à deux coups de la
  victoire » vaille moins que « à un coup de la victoire ».

Reconstruire à chaque passe est un gaspillage et Summoner en est le bot de
recherche le plus faible, mais il possède une propriété que les autres n'ont
pas : il sait s'il a terminé.

## 9. Bandits sur les plans

Strategist ajoute une couche : les paramètres de recherche eux-mêmes sont
traités comme les bras d'un bandit UCB1. Il lit d'abord un score de **tempo**
d'après le caractère de la confrontation — la rapidité avec laquelle les decks
en présence terminent habituellement la partie — et le traduit en un *plan* :
un facteur d'actualisation `gamma` (0,97 orienté valeur à 0,70 orienté tempo),
une profondeur maximale (3 ou 2) et une largeur de branchement (3,0 à 8,0),
interpolés selon le tempo et selon la confiance dans cette lecture
(`strategist.ml:148-210`).

Trois bras sont ensuite construits le long de ce cadran
(`strategist.ml:214-215`) :

```ocaml
let arms t confidence =
  List.map (fun x -> for_tempo_plan (clamp01 x) confidence)
    [ t -. Params.bandit_delta; t; t +. Params.bandit_delta ]      (* delta = 0.25 *)
```

À chaque tour, il sonde chaque bras à un ply de profondeur sur les 5 meilleurs
coups ordonnés, choisit le meilleur selon

```text
score = alpha * probe_value + (1 - alpha) * bandit_mean + bandit_bonus
```

(`strategist.ml:469-473`, avec `alpha = 0.7`, `bandit_c = 1.4`), lance la vraie
recherche avec le vainqueur, et réinjecte le résultat dans les moyennes
courantes de la bande. Au fil d'une partie, le bandit apprend quel tempo
convient à la confrontation.

L'idée se généralise : quand les paramètres sont la partie difficile, on
cherche sur les paramètres avec la même mécanique de budget.

## 10. Croyances : jouer sans information complète

Tout ce qui précède supposait que le bot connaît l'état complet. Pour ce jeu,
c'est le cas (voir le document 2), ce qui rend le problème *beaucoup* plus
facile qu'il ne devrait l'être. À quoi ressemblerait donc une version
rigoureuse ? C'est exactement ce qu'implémente Diviner, et le vocabulaire vaut
d'être appris pour lui-même.

**Partiellement observable.** Les vingt cartes de l'adversaire sont cachées. Le
bot connaît sa maison spéciale et les cartes qu'il a révélées, rien d'autre. La
recherche à information complète est optimiste : elle évalue « l'adversaire a
exactement les cartes qu'il a », ce qui n'est qu'un échantillon dans un espace
énorme.

**Espace de croyances.** Au lieu d'un unique état caché, on conserve un
ensemble d'états cachés *possibles* — ici, des distributions possibles de
l'adversaire — appelés **mondes** ou particules. Chaque monde est exploré
séparément. Trois hypothèses rendent cela praticable :

1. Échantillonner les mondes **par rejet** : tirer une distribution aléatoire
   avec le constructeur de deck du jeu (`Shuffle.create_player`), de sorte que
   chaque monde soit un deck *légal*, et non une main arbitraire. Le rejet est
   bon marché ici et ne produit jamais de position impossible. Les cartes déjà
   révélées par l'adversaire sont passées en `~require`, si bien que chaque
   monde les contient.
2. Conserver un monde qui est la **vérité** (ou la meilleure supposition
   disponible), pour que la recherche ne soit jamais uniformément pessimiste.
3. Explorer chaque monde avec un contrôleur *indépendant* — dans le cas de
   Diviner, celui d'Oracle, avec ses propres statistiques.

**L'espérance ne suffit pas.** Moyenner sur les mondes est l'agrégation
évidente, et elle est mauvaise pour jouer : un coup qui gagne dans 2 mondes sur
3 et perd lourdement dans le troisième est bien pire qu'un coup modérément bon
dans les trois, car le troisième monde pourrait être le vrai. Donc :

```text
score(move) = mean(world values) - risk_aversion * (mean - worst world)
```

Le terme de pénalité croît avec la dispersion et disparaît quand tous les
mondes s'accordent. Avec `risk_aversion = 0`, on retombe sur l'espérance simple
— le bot optimiste, de type Oracle. Avec `risk_aversion = 1`, on obtient un
minimax pur sur les mondes, beaucoup trop timoré. `0.35` est un compromis
jouable.

Noter aussi que les mondes ne sont **rééchantillonnés que lorsqu'une nouvelle
carte est révélée**, sans pondération reportée d'un tour à l'autre. Ils sont contraints de contenir les cartes
révélées, mais rien n'est déduit de ce que l'adversaire n'a *pas* joué. Un
filtrage particulaire complet (pondérer les mondes par la vraisemblance du jeu
observé, puis rééchantillonner) serait strictement meilleur et constitue la
prochaine étape évidente pour affûter Diviner.

## 11. Comment les six bots diffèrent, en un tableau

| | greedy | summoner | strategist | lecturer | oracle | diviner |
|---|---|---|---|---|---|---|
| **générateur** | premier des `available_commands` | complet à la racine | complet, ordonné par rôle | complet à la racine, `base_moves` en dessous | `base_moves` | `base_moves` |
| **profondeur** | 0 | 2–8, approfondissement itératif | 2–3 | 4 (`maxDepth`) | 5 (`horizon`) | 5 |
| **rollout** | aucun | aucun (arbre seul) | aucun (arbre seul) | aléatoire, 6 plies | glouton | glouton |
| **feuille** | aucune | `Heuris.of_state` | course + synergie, actualisée par gamma | 4 passes + `Eval.score` | 4 passes + `Eval.score` | `Eval.score` par monde |
| **contrôleur** | aucun | meilleur-d'abord | meilleur-d'abord + bandit UCB | UCT / MCTS | **successive halving** | successive halving × mondes |
| **modèle d'adversaire** | aucun | deck deviné | deck deviné | deck deviné | réponses classées | réponses classées, par monde |
| **information cachée** | aucune | une supposition | une supposition | une supposition | une supposition | **en échantillonne plusieurs** |
| **arbre entre les coups** | non | non | bandit seulement | non (reconstruit) | non | non |
| **budget par défaut** | — | 1000 ms | 1000 ms | 1000 ms | 1000 ms | 1000 ms |

## 12. Combien d'échantillons suffisent

Chaque bot ici décide par moyenne : la valeur d'un candidat est la moyenne de
*n* scores de feuille. La moyenne est non biaisée mais son **erreur standard
est `σ/√n`**, et presque toutes les surprises d'une recherche à petit budget
viennent de cette erreur plus grande que l'écart que l'on cherche à lire.

Les scores de feuille vivent dans `(-1, 1)`, mais ne sont pas uniformes sur cet
intervalle. En pratique, un rollout glouton à 5 plies tombe à quelques dixièmes
de la moyenne de son candidat, avec une queue lourde quand le rollout termine
parfois la partie d'emblée. Prendre `σ ≈ 0.4` comme valeur réaliste par
échantillon et lire les tableaux ci-dessous comme « l'écart qu'il faut pour que
cela vaille quelque chose à ce budget ».

Deux candidats avec `n` échantillons indépendants chacun ont
`SE_diff = σ·√(2/n)`, donc le plus petit écart qu'on peut affirmer à 95 % de
confiance (`z = 1.96`) est

```text
resolvable gap  =  1.96 · σ · √(2/n)  =  0.78 · √(2/n)     for σ = 0.4
```

| échantillons par candidat `n` | `SE` d'une moyenne | écart résolvable |
|---|---|---|
| 1 | 0,40 | 1,11 |
| 3 | 0,23 | 0,64 |
| 6 | 0,16 | 0,45 |
| 14 | 0,11 | 0,30 |
| 60 | 0,052 | 0,14 |
| 250 | 0,025 | 0,07 |

Inversée, c'est la règle pour dimensionner un budget : pour séparer deux coups
dont les vraies valeurs diffèrent de `Δ`, il faut

```text
n  ≥  2 · (1.96 · σ / Δ)²      samples per move
```

échantillons par coup, donc `Δ = 0.3` coûte 14 rollouts par coup, `Δ = 0.1`
en coûte 123, et `Δ = 0.02` en coûte 3 000. **Aucun budget de l'interface de ce
jeu ne peut défendre les deux derniers.** Il n'en résulte pas que les bots sont
défaillants, mais que *le bon coup n'est généralement pas le deuxième meilleur*,
si bien que des estimations grossières suffisent. Cela signifie aussi qu'une
vraie ligne de log peut être honnête et inutile à la fois :

```text
  final:  0> water.sprite at 5 mean=0.354 n=3 | 0> water.golem at 5 mean=0.342 n=2 | ...
```

Un écart de 0,012 avec `n = 3`, là où 0,64 serait nécessaire pour trancher. La
bonne lecture de cette ligne est *« ces candidats sont indiscernables, jouer
n'importe lequel »* — et non *« sprite était le meilleur »*. Défaire le mélange
screening/playout du [document 4](04-oracle.fr.md) §4 avec `screen_weight = 2`
montre qui a réellement décidé :

```text
cand_mean = (2·screen + played) / 3

sprite:  (2·0.386 + played) / 3 = 0.354  ->  played = 0.290   (3 rollouts)
golem:   (2·0.325 + played) / 3 = 0.342  ->  played = 0.376   (2 rollouts)
```

Les rollouts préféraient en réalité le golem de 0,086, et l'avance de 0,061 du
screening l'a emporté. C'est la conception qui fonctionne comme prévu à un
budget de 60 ms — la lecture superficielle est crue, la lecture coûteuse ne sert
qu'à départager — mais il faut bien voir qu'à ce budget les rollouts ne
choisissent pas le coup.

### La corrélation aggrave les choses

Avec une politique de base gloutonne, tous les échantillons d'un coup partagent
le même biais, ils ne sont donc pas indépendants. Pour des échantillons
équicorrélés, l'erreur standard est

```text
SE = σ · √( (1 + (n−1)·ρ) / n )       →  σ·√ρ   as n → ∞
```

ce qui est la formulation utile : **un estimateur corrélé a un plancher de bruit
que davantage d'échantillons ne permettent pas de franchir.** `ρ = 0.3` plafonne
déjà l'erreur standard à `0.4·√0.3 = 0.22`, de sorte que doubler le budget
n'apporte rien. C'est exactement la limite que le [document 4](04-oracle.fr.md)
§8 énonce qualitativement, et c'est pourquoi les rollouts aléatoires de
Lecturer valent leur bruit supplémentaire : pour eux `ρ ≈ 0` et l'erreur décroît
réellement en `1/√n`.

### Dégâts à sens unique

Le successive halving ne commet pas d'erreurs symétriques : un mauvais tour
*élimine le meilleur candidat* et aucun échantillon ultérieur ne peut le
ramener. Il en va de même pour la shortlist (présélection des meilleurs coups après le screening). Cette asymétrie est toute la
raison pour laquelle la valeur de screening est remélangée à `cand_mean` avec le
poids `screen_weight`, et la raison pour laquelle le raccourci « lethal » (jouer tout de suite un coup qui gagne) passe avant
la recherche et non après — si l'on voit une victoire, on la prend sans dépenser
un seul rollout sur la possibilité de se tromper.

### L'arithmétique budgétaire de l'ordonnancement

L'ordonnancement de halving par défaut dépense, par phase, `k_j · s_j` rollouts,
où `k_j` est le nombre de survivants et `s_j` le nombre d'échantillons par
candidat :

```text
round 1:   9 candidates ×  6 =  54
round 2:   5 candidates × 12 =  60
round 3:   3 candidates × 24 =  72
round 4+:  2 candidates ×  1 =   2 per round, forever
```

Atteindre le duel final coûte 186 rollouts ; aux ~180 simulations par rollout
du [document 4](04-oracle.fr.md) §6, cela fait ~33 k simulations, ce qui explique
que les exécutions à 200 ms journalisent `rounds=1` ou `2` et n'approchent
jamais le duel. La dernière ligne est la plus importante : deux candidats ne
sont jamais éliminés, la recherche dégénère donc en face-à-face plutôt qu'en
simple supposition, et chaque seconde de budget supplémentaire rend ce duel plus
fiable.

## 13. Vocabulaire

| terme | sens ici |
|---|---|
| ply | un coup d'un seul camp |
| coup (move) | une commande (ou une passe) du camp au trait |
| racine (root) | la position dans laquelle le bot doit choisir un coup |
| rollout / playout | une courte partie à partir d'un coup, jouée par la politique de base |
| politique de base | le joueur peu coûteux utilisé dans un rollout |
| glouton (*greedy*) | qui choisit à chaque fois le coup qui paraît le meilleur sur le moment, sans réfléchir plus loin ; « glouton » est le terme français usuel pour *greedy* |
| feuille (leaf) | la position où se termine un rollout |
| screening (criblage) | une évaluation superficielle et rapide de chaque coup, avant tout rollout, pour trier les candidats |
| shortlist (présélection) | les quelques coups retenus après le screening ; eux seuls reçoivent des rollouts |
| lethal | un coup qui gagne la partie immédiatement ; on le joue sans chercher plus loin |
| chaîne de fin de tour | la suite d'effets automatiques que le moteur déroule quand un joueur passe (attaques, effets de maison, morts et leurs déclencheurs, gain de mana) |
| monde / croyance | une supposition sur la main cachée de l'adversaire |
| anytime | doit produire une réponse valide si interrompu à n'importe quel instant |
| a priori (prior) | une visite virtuelle donnée à un nouvel enfant MCTS à partir d'une valeur heuristique |
| negamax | stocker les récompenses du point de vue du joueur au trait, en inversant le signe à chaque ply |
| erreur standard | `σ/√n`, l'écart attendu entre la moyenne d'échantillon et la vraie valeur |
| écart résolvable | la différence de valeur que `n` échantillons peuvent défendre ; en dessous, l'ordre n'est que du bruit |
| `ρ` | corrélation entre échantillons ; un `ρ` positif impose un plancher à l'erreur standard |

Suite : [04-oracle.fr.md](04-oracle.fr.md) — le successive halving et
l'évaluation d'Oracle, ligne par ligne.
