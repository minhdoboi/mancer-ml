# 4. Le bot Oracle

`lib/bots/oracle.ml`, 423 lignes. Portage de `priv/sp/bot/oracle/OracleBot.scala`.

> **L'idée en une phrase :** ne pas construire d'arbre — évaluer chaque coup à
> la racine en le jouant, puis en laissant un joueur glouton (*greedy* : à
> chaque tour, il joue le coup qui paraît le meilleur sur le moment, sans
> réfléchir plus loin) bon marché jouer *les deux camps* pendant cinq plies
> (demi-coups) à travers le vrai moteur, et désigner le vainqueur par
> **élimination successive (successive halving)** sous une limite de temps réel.

C'est toute la conception. Tout ce qui suit n'est que du détail.

### Quelques termes qui se traduisent mal

Ces mots restent en anglais dans le texte, car ce sont ceux que l'on retrouve
dans le code et dans les logs. Voici ce qu'ils recouvrent.

| terme | explication |
|---|---|
| **screening** | « criblage » : première évaluation *rapide* de chaque coup légal — on le joue une fois, on avance de quatre passes, on note le résultat. C'est un tri grossier, fait avant les vrais rollouts, pour savoir lesquels méritent qu'on y consacre du temps. |
| **shortlist** (« présélection ») | la courte liste de coups retenus après le screening : le meilleur placement de chaque carte, limité aux 8 meilleures cartes plus le pass. Seuls ces coups passent aux rollouts. |
| **lethal** (« raccourci létal ») | si un coup gagne la partie sur-le-champ, inutile de réfléchir : on le joue. Ce test se fait pendant le screening, avant toute recherche. |
| **chaîne de fin de tour** | quand un joueur *passe*, le tour ne fait pas que changer de main : le moteur déroule toute une série d'effets automatiques (attaques des créatures, effets de maison, mort des créatures et leurs déclencheurs, gain de mana…). `Game.simulate` exécute cette chaîne en entier, c'est pourquoi même « passer » peut tuer l'adversaire. |
| **playout / rollout** | une courte partie imaginaire jouée par la politique gloutonne (*greedy*, voir document 3). |
| **successive halving** | « élimination successive » : on essaie tous les candidats un peu, on écarte la moitié la plus faible, on recommence avec plus d'essais pour les survivants. |

## 1. Le pipeline

```text
  execute settings ~pid ~desc start            oracle.ml:268
    |
    |-- is_disabled? -> None (pass)                                :273
    |
    |-- SCREEN   play every legal command once, project, score
    |              -> [(step, screen_value)]                       :277-284
    |
    |-- LETHAL?  any command whose step.ended = Some bot_id
    |              -> return it, zero rollouts spent               :285-288
    |
    |-- SHORTLIST best screen value per card, top root_cards (8),
    |              plus pass                                        :290-312
    |
    |-- ROLL     successive halving until the clock dies           :345-366
    |              each sample = one playout                       :323-344
    |
    `-- max_by cand_mean survivors -> the command
```

`None` (passer) est un candidat à part entière à chaque étape : il a sa propre
entrée dans le screening et son propre groupe dans la shortlist.

### Étape 1 : le screening (le criblage)

On joue chaque coup légal une seule fois, juste pour obtenir une première note
rapide.

```ocaml
let all = None :: List.map (fun c -> Some c) (Game.legal_commands bot_id desc start) in
let screened : (step * float) list =
  List.map (fun c -> let s = play start bot_id c in (s, value s 1)) all
```

Chaque commande légale est jouée une fois dans un fork, puis `value` pousse le
résultat de quatre passes en avant et le note. Cela coûte
`(|legal| + 1) * 5` simulations — le coup d'ouverture ci-dessus en a pris 15
lors d'une exécution — et c'est ce qui rend tout le reste abordable.

### Étape 2 : le raccourci « lethal » (la victoire immédiate)

```ocaml
match List.find_opt (fun ((s : step), _) -> s.ended = Some bot_id) screened with
| Some (s, _) -> log_line settings t0 "lethal" s.cmd 1.0 0 stats; s.cmd
```

Si un coup termine immédiatement la partie, il est joué sans rien dépenser de
plus. Noter que cela peut produire `cmd=pass` : une exécution réelle se
termine par

```text
oracle-stats lethal playouts=0 wins=0 losses=0 sims=15 rounds=0 spentMs=0/budgetMs=60 best=1.000 n=0 cmd=pass
```

parce que la *chaîne de fin de tour* (l'enchaînement automatique d'effets que
le moteur déroule quand on passe : attaques, effets de maison, morts des
créatures et leurs déclencheurs — un *death-rattle*, par exemple) a tué
l'adversaire. `Game.simulate` exécute toute la chaîne ; le bot voit donc des
victoires qui ne résultent pas directement de la carte jouée.

### Étape 3 : la shortlist (la présélection)

On ne garde que les coups les plus prometteurs du screening ; eux seuls
recevront des rollouts.

```ocaml
let per_card =
  List.rev !keys
  |> List.map (fun k -> max_by (fun (_, v) -> v) (Hashtbl.find tbl k))
  |> List.sort (fun (_, a), (_, b) -> compare b a)
```

Les candidats sont regroupés par **identifiant de carte**, et seul le
*placement* le mieux noté de chaque carte survit. Comme
`Game.legal_commands` renvoie une commande distincte par emplacement cible,
Oracle choisit en réalité à la fois *quelle carte* jouer et *où* la poser à
partir du seul screening. C'est le plus gros raccourci heuristique du bot, et
c'est pourquoi un coup dont la meilleure cible paraît médiocre alors que ses
autres cibles seraient excellentes n'est jamais exploré.

Les `root_cards` (8) meilleures plus le pass forment les `survivors`.

### Étape 4 : élimination successive (successive halving)

Principe : on essaie chaque survivant un peu, on écarte la moitié la plus
faible, puis on recommence en donnant plus d'essais aux survivants, jusqu'à
épuisement du temps.

```ocaml
let per_round = ref !Params.first_round_samples in
while !time_left && List.length !survivors > 1 do
  while !time_left && !i < !per_round do
    List.iter (fun c ->
      if !time_left then (
        let rank = c.n mod Array.length !Params.reply_weights in
        add_sample c rank (playout c.move rank);
        time_left := now_ms () < deadline)) !survivors;
    incr i done;
  if !time_left then (
    let n = List.length !survivors in
    let keep = if n > 2 then (n + 1) / 2 else 2 in
    survivors := ... |> take keep;
    if List.length !survivors = 2 then per_round := 1 else per_round := !per_round * 2)
done;
```

Deux détails en douze lignes :

- `rank = c.n mod 3` fait tourner le rang de la réponse adverse d'un
  échantillon à l'autre : les premiers échantillons sont donc répartis entre la
  meilleure, la deuxième et la troisième meilleure réponse, plutôt que
  concentrés sur une seule.
- `keep = 2` quand `n <= 2`. Les deux derniers candidats ne sont jamais
  éliminés ; ils accumulent simplement un échantillon par round jusqu'à l'arrêt
  de l'horloge. **L'étape finale est un duel en tête-à-tête, et son résultat
  est monotone** — davantage d'échantillons ne peuvent qu'affiner l'estimation.

## 2. La fonction d'évaluation

`lib/bots/oracle.ml:26-66`. Six termes pondérés, écrasés dans `(-1, 1)`.

```ocaml
let life_term life =
  let lost = min 60.0 (max 0.0 (60.0 -. life)) in
  (!life_w *. life) -. (!life_curve *. lost *. lost /. 60.0)

let board (p : player_state) =
  Array.fold_left (fun v -> function
    | None -> v
    | Some s ->
        let held = if has_flag s on_hold then !held_factor else 1.0 in
        v +. (!attack_w *. float_of_int s.attack *. held)
          +. (!creature_life_w *. float_of_int s.life)) 0.0 p.ps_slots

let score ~projected ~before ~bot_id =
  let raw =
    life_term b_life -. life_term o_life +. board bot -. board opp
    +. (!mana_w *. (stock bot -. stock opp))
    +. (!growth_w *. (income bot -. income opp))
  in
  tanh (raw /. !scale)
```

### Terme par terme

**`life_term` est concave : un point de vie vaut plus quand il en reste
moins.** Le terme se compose de deux parties : d'abord la vie *telle quelle*
(« valeur faciale » : 50 points de vie comptent pour 50, comme une pièce vaut
le chiffre inscrit dessus), puis on en **retranche une pénalité** qui croît
très vite avec la vie déjà perdue (`lost`, au carré). Exemple à 40 de vie :
valeur faciale 40, vie perdue 20, pénalité 20²/60 = 6,67, donc `life_term` =
33,33. Plus on est bas, plus chaque point manquant pèse. Avec les poids par
défaut (`life_w = life_curve = 1.0`, `life_term x = x - (60-x)²/60`, dérivée
seconde −1/30) :

| vie | `lost` | `life_term` | valeur du prochain point de vie |
|---|---|---|---|
| 60 | 0 | 60.00 | 1.08 |
| 50 | 10 | 48.33 | 1.42 |
| 40 | 20 | 33.33 | 1.75 |
| 30 | 30 | 15.00 | 2.08 |
| 20 | 40 | −6.67 | 2.42 |

La dernière colonne est le coût des cinq prochains points de dégâts :
`life_term x - life_term (x-5)`. Perdre cinq points de vie à 60 coûte **5.4**
en valeur brute ; en perdre cinq à 30 coûte **10.4**, presque le double. Un
terme de vie linéaire traiterait les deux cas de la même façon.

Remarquer ce que ce n'est *pas* : la différence
`life_term bot - life_term opp` change de signe quand on échange les deux
joueurs (si le bot mène de +10, l'adversaire, vu de son côté, est à −10), de
sorte qu'une avance et le déficit inverse s'annulent toujours exactement. La
courbure ne fait pas pencher la balance vers la prudence — elle rend une
hémorragie lente de plus en plus coûteuse à supporter, ce qui est le
comportement voulu : le bot cesse d'accepter des échanges qui ne semblent
abordables que parce qu'il est encore en bonne santé.

**`board` décote les créatures immobilisées.** Une créature étourdie, bloquée
ou en pause contribue pour son attaque multipliée par `held_factor` (0.3), car
elle ne frappera pas au tour suivant. Sa vie compte, en revanche, en totalité.
Noter que cela lit le plateau *à la feuille* : c'est donc un jugement sur la
position projetée, pas sur les intentions.

**`stock` et `income`** sont le mana en main et la croissance par tour. La
croissance est pondérée **6×** le mana par défaut — `growth_w = 6.0` contre
`mana_w = 1.0`. Dans ce jeu, le moteur qui gagne est généralement celui qui se
développe le plus vite, et le mana en main vaut bien moins que du mana qui
arrive à chaque tour. Ce seul poids est le nombre le plus important de la
fonction ; voir le tableau de réglage.

**`tanh (raw / 60)`** ramène le score brut (non borné) dans l'intervalle
`(-1, 1)`, en l'écrasant progressivement. `raw = 60` donne `0.76`, `raw = 120`
donne `0.96`. Rien ne sature à ±1, ce qui garde la moyenne de nombreux
échantillons informative — un rollout qui gagne la course sans gagner la partie
obtient quand même un score nettement supérieur à un nul.

### Le couple `projected` / `before`

```ocaml
let b_life = float_of_int bot.ps_life +. (!extrapolate *. (float_of_int bot.ps_life -. b_now)) in
```

`before` est la racine du rollout ; `projected` est la position que l'on note
après quatre passes simulées. Le **delta de vie est doublé** avec
`extrapolate = 1.0`. Si le bot est entré dans le rollout à 50 et se trouve à 45
à la feuille, le score utilise `45 + (45 - 50) = 40`. Justification : un pass
n'est pas un coup nul, il fait avancer l'enchaînement des tours, si bien que la
vie lue sur une feuille en milieu d'enchaînement est en retard sur la position
réelle. L'extrapolation atténue ce bruit. Avec `extrapolate=0`, c'est la feuille
brute qui est notée.

### Une évaluation détaillée

Bot : vie 50, une créature 4/3 sur le plateau, mana 6+5+4+3+2 = 20,
croissance 6. Adversaire : vie 40, une créature 2/2, mana 16, croissance 5.

```text
life_term 50 - life_term 40  = 48.33 - 33.33 =  +15.00
board bot - board opp        = 4.40  - 2.40  =   +2.00   (0.8*4 + 0.4*3 vs 0.8*2 + 0.4*2)
mana_w  * (20 - 16)          = 1.0 * 4     =   +4.00
growth_w * (6 - 5)           = 6.0 * 1     =   +6.00
                                       raw =  +27.00
score = tanh(27 / 60) = tanh(0.45)          ~=  +0.42
```

Un avantage net mais pas absurde se lit à environ quatre dixièmes. C'est
l'échelle sur laquelle vit chaque échantillon de rollout.

### Valeurs terminales

```ocaml
let terminal ~bot_id winner ply =
  let v = max 0.5 (1.0 -. (!Params.win_discount *. float_of_int ply)) in
  if winner = bot_id then v else -.v
```

Un rollout terminé **n'est pas** noté par `Eval.score` ; il reçoit une valeur
**fixe**, indépendante de l'état du plateau à la fin (seul le vainqueur
compte) : **+v** si le bot gagne, **−v** s'il perd, où v décroît avec le ply
auquel la partie s'est achevée. Gagner au ply 1 vaut `0.98`, au ply 5 `0.90`,
et le plancher est `0.5` (atteint au ply 25). La rapidité a de la valeur, mais
jamais plus du double. Exemple : une victoire rapide au ply 1 vaut +0,98, une
victoire tardive +0,50 ; une défaite rapide vaut −0,98 (la pire), une défaite
tardive −0,50 (moins grave : on a tenu longtemps).

## 3. La politique de base

Un rollout consiste en cinq plies de jeu glouton (*greedy*) pour les deux
camps, à partir de l'ensemble de coups élagué propre à Oracle
(`oracle.ml:190-213`) :

```ocaml
None :: List.concat_map (fun id -> placements (Hashtbl.find by_card id) p enemy) order
```

Le pass, plus au plus `placements_per_card` (2) cibles par carte jouable. Les
deux cibles sont choisies par `placements` (`oracle.ml:152-188`) :

- **Cible côté allié** (`Select_owner_slot`, `Select_owner_creature`) :
  1. la créature ennemie à l'**attaque la plus élevée** dans cet emplacement —
     la bloquer ;
  2. la créature alliée la **moins endommagée**, `life / max_life` maximal ;
  3. l'emplacement libre le **plus vide**, `-friends` maximal, où `friends`
     compte les voisins occupés — rester groupé pour qu'une créature ait de la
     compagnie.
- **Cible ennemie** : l'ennemi à l'attaque la plus élevée, puis l'ennemi à la
  vie la plus basse.
- **Sans entrée** : la carte elle-même.

Dans un rollout, le camp qui joue prend l'argmax du score de feuille
(`max_by snd scored`, `oracle.ml:337`), sauf pour la première réponse de
l'adversaire, qui est prise à un rang choisi.

## 4. Modéliser l'adversaire

C'est l'astuce la plus distinctive d'Oracle, et elle mérite une double lecture
(`oracle.ml:323-344`).

```ocaml
let scored = List.map (fun s -> (s, signed ~bot_id (value s (!ply + 1)) pid)) steps in
step := if pid <> bot_id && not !opponent_replied then (
          opponent_replied := true; nth_best scored reply_rank)
        else fst (max_by snd scored);
```

`signed ~bot_id v pid` inverse le signe quand le camp qui joue n'est pas le
bot ; `scored` classe donc toujours les coups **du point de vue du joueur sur
le point de jouer**. `nth_best scored 0` est ainsi la *meilleure* réponse de
l'adversaire, le rang 1 sa deuxième meilleure, le rang 2 sa troisième.

Les échantillons font tourner `rank = n mod 3`, avec les poids
`[0.60; 0.25; 0.15]`, et la valeur du candidat est la moyenne pondérée des
moyennes par rang, les rangs non joués se rabattant sur la valeur du screening
(`oracle.ml:108-118`) :

```ocaml
let acc = Σ over ranks k with samples of  w_k * mean_k
let w   = Σ over ranks k with samples of  w_k
let played     = acc +. (c.screen *. (1.0 -. !w))
let cand_mean  = ((!screen_weight *. c.screen) +. played) /. (!screen_weight +. 1.0)
```

### Un `cand_mean` détaillé

Candidat avec `screen = 0.30`, rang 0 échantillonné 3× de moyenne `0.50`,
rang 1 échantillonné 2× de moyenne `0.20`, rang 2 jamais échantillonné,
`screen_weight = 2` :

```text
acc    = 0.60*0.50 + 0.25*0.20            = 0.350
w      = 0.60 + 0.25                      = 0.85
played = 0.350 + 0.30*(1 - 0.85)          = 0.395
mean   = (2.0*0.30 + 0.395) / (2.0 + 1.0) = 0.332
```

Prenons maintenant un cas où les rollouts contredisent le screening : le
screening donne `screen = 0.10`, mais chacun des trois rangs a été échantillonné
une fois et a rapporté respectivement `0.55`, `0.40` et `0.30` :

```text
acc    = 0.60*0.55 + 0.25*0.40 + 0.15*0.30 = 0.475
w      = 1.0
played = 0.475
mean   = (2.0*0.10 + 0.475) / 3.0          = 0.225
```

Les rollouts ont donc fait passer le candidat de `0.10` à `0.225` — le
screening est tiré vers les observations échantillonnées avec un poids
`screen_weight : 1`, et avec assez d'échantillons il ne compte plus.

L'intérêt des réponses classées : **un coup qui n'est bon que parce que la
meilleure réponse de l'adversaire est mauvaise ne doit pas recevoir tout le
crédit.** Pondérer le rang 0 à 0.60 plutôt qu'à 1.0 prend en compte le fait que
l'adversaire a d'autres options.

## 5. Le voir à l'œuvre

`ORACLE_DEBUG=1` affiche le plateau et les deux classements avant que le coup
ne soit choisi. Sortie réelle, budget de 60 ms, seed 7 :

```text
oracle-stats search playouts=14 wins=0 losses=0 sims=6800 rounds=1 spentMs=63/budgetMs=60 best=0.354 n=3 cmd=0> water.sprite at 5
houses 3,5,5,6,2, life 60, slots .,.,.,.,.,.
houses 6,5,5,2,2, life 60, slots .,.,.,.,.,.
  screen: 0> water.sprite at 5 screen=0.386 | 0> fire.monk at 5 screen=0.366 | 0> air.fairy at 5 screen=0.364 | 0> water.golem at 5 screen=0.325 | 0> Conscription screen=-0.050 | pass screen=-0.067
  final:  0> water.sprite at 5 mean=0.354 n=3 | 0> water.golem at 5 mean=0.342 n=2 | 0> air.fairy at 5 mean=0.331 n=2 | 0> fire.monk at 5 mean=0.316 n=3 | 0> Conscription mean=-0.055 n=2 | pass mean=-0.083 n=2
```

Comment lire cette sortie :

- `screen:` comporte une entrée par carte (le meilleur placement de chacune),
  plus le pass, dans l'ordre du screening. Six cartes en main → six entrées.
- `final:` liste les survivants, triés par `cand_mean`. Ici les six ont survécu
  — l'horloge a expiré pendant le round 1, donc **aucune élimination n'a eu
  lieu**. C'est normal à 60 ms, et c'est la propriété anytime en action : un
  classement exploitable existe dès le premier échantillon.
- `n=` est le nombre de rollouts reçus par chaque candidat. Des valeurs de 1 à 3
  signifient que c'est le budget, et non l'algorithme, qui était la contrainte
  limitante.

Voyons maintenant une décision où les deux étapes divergeaient. Même exécution,
ply ultérieur :

```text
  screen: 0> fire.wave screen=0.983 | 0> fire.monk at 0 screen=0.956 | 0> air.call at 2 screen=0.956 | ... | pass screen=0.820
  final:  0> earth.care at 1 mean=0.874 n=1 | 0> air.call at 2 mean=0.860 n=2 | 0> air.lightnin mean=0.849 n=2 | ... | 0> fire.wave mean=0.655 n=2 | 0> fire.monk at 0 mean=0.477 n=2
```

`fire.wave` est arrivée **première** au screening avec `0.983` et a fini
**huitième** avec `0.655` ; `fire.monk` était deuxième avec `0.956` et a fini
dernière avec `0.477`. Autrement dit, le screening ne regarde que quatre
passes (quatre tours où l'on ne joue plus rien) après la pose de la carte : il
ne voit donc pas ce qui se passe ensuite dans la partie, alors que les
rollouts, eux, le voient — ils jouent les cinq plies suivantes avec de vrais
coups des deux camps.

(Pourquoi le screening ne pouvait pas les départager : dans cet exemple, le bot
était déjà nettement en avance, et *tous* les candidats avaient une note
supérieure à 0.9. Or `tanh` écrase les scores près de 1 : quand une position
paraît très bonne, les bons et les excellents coups obtiennent presque la même
note, et le screening n'a plus de quoi les distinguer. Ce sont alors les petits
écarts apparus pendant les rollouts qui font la différence.) Ce renversement
est la raison d'être de la conception en deux étapes.

## 6. Coût

Un playout n'est pas une simulation. À chaque ply, chaque coup candidat de
l'ensemble élagué est joué (un fork chacun) puis projeté de quatre passes :

```text
  playout cost  ~=  1 (root) + 4 (its projection)
                +  horizon * branching * 5        (1 fork + 4 projection passes each)
```

Avec `horizon = 5` et un branching de 7 (pass + 2 placements × 3 cartes
jouables), cela donne environ **180 simulations par rollout**, avant la phase
de screening. Chiffres réels pour un budget de 200 ms sur cette machine :

```text
oracle-stats search playouts=60  wins=0 losses=0 sims=28640 rounds=2  spentMs=200/budgetMs=200 best=0.354 n=14 cmd=0> water.sprite at 5
oracle-stats search playouts=273 wins=273 losses=0 sims=21701 rounds=129 spentMs=200/budgetMs=200 best=0.976 n=134 cmd=1> earth.care at 5
```

Environ 10⁵ simulations/seconde. La première ligne a effectué deux rounds
d'élimination et donné 14 échantillons au gagnant ; la seconde a réduit les
candidats jusqu'au duel final, puis a dépensé 129 rounds supplémentaires à
échantillonner cette paire — `rounds=129` avec `n=134`, c'est l'étape de
tête-à-tête qui fait son travail.

Utilisez `sims=` comme véritable compteur de coût quand vous modifiez des
paramètres. Doubler `horizon` double les sims par rollout et divise par deux le
nombre de rounds terminés.

## 7. Réglage

`ORACLE_OPTS` est une liste `clé=valeur` séparée par des virgules, lue une
seule fois au chargement du module (`oracle.ml:68-99`). Les clés inconnues ne
provoquent qu'un avertissement.

```sh
ORACLE_OPTS=horizon=3,rootCards=4,projectPlies=2 \
  dune exec ./bin/main.exe -- --bots oracle,greedy --games 4 --budget 200 --seed 1
```

| clé | défaut | effet |
|---|---|---|
| `rootCards` | 8 | candidats entrant dans l'élimination, plus le pass |
| `horizon` | 5 | plies par rollout |
| `placementsPerCard` | 2 | branches par carte dans la politique de base |
| `replyWeights` | `0.6;0.25;0.15` | rangs de réponse adverse, séparés par `;` |
| `firstRoundSamples` | 6 | rollouts par candidat au round 1 |
| `projectPlies` | 4 | passes avant la notation de la feuille |
| `winDiscount` | 0.02 | vitesse de décroissance du ±1 terminal avec le ply |
| `screenWeight` | 2.0 | poids du screening superficiel dans `cand_mean` |
| `lifeW` | 1.0 | terme de vie |
| `lifeCurve` | 1.0 | pénalité quadratique sur la vie déjà perdue |
| `attackW` | 0.8 | attaque du plateau |
| `creatureLifeW` | 0.4 | vie des créatures du plateau |
| `manaW` | 1.0 | mana en main |
| `growthW` | 6.0 | croissance — le plus gros levier de l'évaluation |
| `scale` | 60.0 | valeur brute qui correspond à `tanh(1)` |
| `extrapolate` | 1.0 | doublement du delta de vie à la feuille |
| `heldFactor` | 0.3 | poids d'attaque d'une créature étourdie/bloquée/en pause |

Deux choses à savoir avant de régler :

- Les `Params` sont des `ref`, donc `ORACLE_OPTS` affecte **aussi Diviner** —
  il réutilise `Oracle.Params.root_cards`, `reply_weights` et `screen_weight`.
- Ils sont globaux au processus : il n'existe pas de réglage par siège.

Expériences peu coûteuses qui valent la peine d'être menées :

```sh
# depth vs breadth, same budget
ORACLE_OPTS=horizon=2 dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 500 --seed 1
ORACLE_OPTS=horizon=8 dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 500 --seed 1

# greedy evaluation vs the smooth one
ORACLE_OPTS=scale=8 dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 500 --seed 1

# a candidate that only survives if the opponent's best reply is bad
ORACLE_OPTS=replyWeights=1;1;1 dune exec ./bin/main.exe -- --bots greedy,oracle --games 20 --budget 500 --seed 1
```

## 8. Limites

- **Chaque carte n'est testée par les rollouts que sur un seul emplacement.**
  La shortlist ne garde, pour chaque carte, que l'emplacement le mieux noté par
  le screening ; les autres ne sont jamais essayés. Si le bon emplacement
  n'apparaît comme tel qu'après la réponse de l'adversaire, la recherche passe
  à côté.
- **Les rollouts gloutons (*greedy*) font tous la même erreur.** Tous les
  rollouts d'un coup sont joués par le même joueur glouton, qui se trompe
  toujours de la même manière. Faire 100 rollouts ne corrige donc pas ce biais,
  pas plus qu'interroger 100 fois le même conseiller, qui a un préjugé, ne le
  rend plus juste : l'estimation paraît très précise (les échantillons se
  ressemblent) mais reste fausse sur ce point (on parle d'erreur *corrélée*).
  Un schéma de réponses à poids uniformes (`replyWeights=1;1;1`) atténue un peu
  ce défaut ; une politique de base aléatoire, comme celle de Lecturer,
  l'atténue davantage.
- **Aucune réutilisation d'un coup à l'autre.** Chaque `execute` repart de
  zéro. Un bot qui conserverait les statistiques de la recherche du tour
  précédent serait plus affûté, au prix d'un `mover` à état.
- **Il joue contre un seul deck deviné, surdimensionné.** Comme le
  `BotKnowledge` de Scala, le `mover` ne cherche jamais sur le vrai deck de
  l'adversaire. `Belief.blind` le remplace par l'union de quelques donnes
  légales (`Belief.superset_samples`, 4), chacune contenant toutes les cartes
  adverses vues jusque-là, et garde cette supposition jusqu'à ce qu'une
  nouvelle carte apparaisse. L'adversaire peut alors répondre avec plus de
  cartes qu'une vraie main n'en contient, ce qui pèche par prudence, mais une
  carte qu'aucune donne n'a tirée reste invisible. Diviner prend l'autre voie
  et garde les donnes séparées, en mondes distincts ([Diviner](05-diviner.fr.md)).
  (Les premières versions du portage cherchaient sur le vrai deck, ce qui
  faisait de chaque bot une borne haute ; le plateau, lui, reste réel, voir
  Diviner §8.)

Suite : [05-diviner.fr.md](05-diviner.fr.md).