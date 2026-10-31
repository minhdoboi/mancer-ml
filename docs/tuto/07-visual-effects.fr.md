# 7. Effets visuels

Tout ce qui est dessiné *par-dessus* le plateau : animations de sorts, secousses
de l'écran et des emplacements, émetteurs de particules, shaders de sélection, de
nuit et de niveaux de gris. Sept fichiers dans `gui/`, et une idée unique qui les
sous-tend tous.

| fichier | contenu |
|---|---|
| `fx_core.ml` | types, transformation, timeline, bruit, shaders, secousses, les 13 « looks » partagés |
| `fx_lightning.ml` | géométrie fractale des éclairs, planification des sauts, cinq styles de couleur |
| `fx_flames.ml` | la colonne de flammes, la nappe de feu qui roule, les braises, l'inferno |
| `fx_particles.ml` | l'émetteur générique et la fumée au survol |
| `fx_dedicated.ml` | vingt-deux chorégraphies sur mesure (poison, faux, aurore, …) |
| `fx_scenes.ml` | folie, onde de choc, confettis, bannière, victoire |
| `fx_stage.ml` | la table : clé de carte → effet |

Les documents 1 à 6 portent sur le jeu et ses bots ; celui-ci porte sur les
pixels. C'est aussi le seul document de ce dossier où il n'y a rien à régler par
variable d'environnement : tout ici est une constante dans un corps de
fonction.

## En une minute

- Un effet est **une fonction de son âge** : à chaque frame, le jeu rappelle
  `draw ~age` avec « les secondes écoulées depuis le début de l'effet », et la
  fonction peint l'image entière à partir de zéro. Rien n'est mémorisé entre deux
  frames, donc rien ne peut se désynchroniser (§1).
- La lumière se dessine en **empilant** des formes translucides (*blending
  additif*) : un large halo faible, un corps plus étroit et plus brillant, un
  cœur blanc fin. Le centre blanc incandescent est la somme des couches, pas une
  couleur choisie par quelqu'un (§1.5, §2.6).
- Le mouvement paraît naturel grâce à **deux horloges à des vitesses sans
  rapport** (un balancement lent et un scintillement rapide) et à un **aléa
  stable par élément** (`noise`), jamais un nouveau nombre aléatoire à chaque
  frame (§1.6).
- La plupart des sorts n'ont besoin d'aucun code : ils choisissent l'un des **13
  looks prêts à l'emploi** dans une table de données (§5.1). Seuls les sorts
  spéciaux ont leur propre fonction (§6).
- Un effet est **bloquant** si l'animation suivante doit l'attendre ; sa
  `duration` doit alors correspondre à ce que le joueur voit réellement (§1.2).

**Par où commencer.**

| vous voulez… | lire |
|---|---|
| comprendre le modèle, quoi que vous fassiez ensuite | §1 |
| ajouter ou ajuster une animation de sort | §5.1 (table des looks), §6, §7 |
| voir comment se construit un effet réaliste, pas à pas | §4 (Maddening) — le meilleur exemple détaillé |
| apprendre les maths derrière les éclairs et les flammes | §2 et §3 — **approfondissements facultatifs**, à sauter en première lecture |
| déboguer « pourquoi c'est sous les cartes / pourquoi le tour se bloque » | §8 |

### Mots utilisés dans ce document

| mot | sens courant |
|---|---|
| **age** | secondes écoulées depuis le début de l'effet ; seule entrée de `draw` |
| **recipe** | un effet prêt à lancer : sa durée, s'il est bloquant, un clip sonore optionnel, et une fonction `make` |
| **blocking** | l'animation suivante (invocation, mort, glissement…) attend la fin de cet effet |
| **alpha** | opacité, de 0 (invisible) à 1 (opaque) |
| **additive blending** | les couleurs sont *ajoutées* à ce qu'il y a dessous ; la lumière qui se recoupe devient donc plus brillante, comme des projecteurs |
| **easing** | une courbe qui fait démarrer ou finir doucement un mouvement 0→1 (`ease_out` = rapide puis lent) |
| **envelope** | forme montée / maintien / retombée de l'intensité d'un effet dans le temps |
| **layout** | l'objet qui indique aux effets où se trouvent à l'écran les emplacements, les portraits et les sols |
| **look** | un style d'effet prêt à l'emploi (rochers, rayons, glace…) paramétré par couleur, cible et puissance |
| **shake** | un petit décalage temporisé appliqué à un emplacement pour le faire trembler |
| **fractal / déplacement du point médian** | construire une ligne dentelée en découpant des segments de façon répétée et en décalant le point du milieu |

## 1. Le pipeline : un effet est une fonction de son âge

### 1.1 La timeline

```ocaml
type t = { start : float; duration : float; draw : age:float -> unit }
type recipe = { duration : float; blocking : bool; sound : string option; make : start:float -> t }
```

`fx_core.ml:9-10`. Un effet est lancé une fois, puis *réévalué à partir de zéro à
chaque frame* avec le temps écoulé. Il n'y a ni machine à états, ni phase
`update`, ni rien qui se souvienne de ce qui était vrai à la frame précédente —
avec deux exceptions volontaires, le cache de géométrie des éclairs et les
émetteurs de particules, qui veulent tous deux leur propre mémoire.

Toute la timeline tient en neuf lignes (`fx_core.ml:84-97`) :

```ocaml
let draw_all ~now =
  active :=
    List.filter
      (fun e ->
        let age = now -. e.start in
        if age >= 0. && age < e.duration then e.draw ~age;
        now < e.start +. e.duration)
      !active
```

Trois conséquences à intégrer avant de lire un effet quelconque :

- **Un effet est reproductible à partir de `age` seul.** Chaque position dans un
  effet est une fonction explicite du temps, pas une accumulation. C'est
  pourquoi tout le système survit à un redimensionnement de fenêtre, à un
  changement de fréquence d'images et à une frame bloquée, sans étape de
  correction.
- **La durée de vie est `duration`.** Il n'y a aucune fuite possible, et un
  `age` hors de `[0, duration)` ne dessine rien. Les deux exceptions sont la
  bannière de victoire et le halo de portrait, qui utilisent `Float.infinity`
  et sont effacés par `Fx.clear` au début d'une revanche (`fx_core.ml:255-257`).
- **La durée dans la `recipe` est une promesse faite à la timeline
  d'animation**, pas seulement une coupure. Voir ci-dessous.

### 1.2 Blocage, et curseur d'animation

`gui_board.ml:542-546` :

```ocaml
let run_fx ui recipe =
  let start =
    if recipe.Fx.blocking then schedule ui recipe.duration
    else Float.max (R.get_time ()) ui.anim_at
  in
  Fx.spawn (recipe.make ~start)
```

`schedule` (`gui_board.ml:405-410`) est un curseur à un seul champ :

```ocaml
let schedule ui duration =
  let now = R.get_time () in
  if ui.anim_at < now then ui.anim_at <- now;
  let start = ui.anim_at in
  ui.anim_at <- start +. duration;
  start
```

Ainsi `blocking = true` ne fait pas que retarder l'effet : il **repousse le début
de chaque animation suivante** de sa propre durée. Invocations, morts et
glissements de cartes passent tous par le même curseur, ce qui explique pourquoi
un sort bloquant retient visiblement le tour. La maison de l'effet décide de la
convention : un effet qui *résout quelque chose* bloque (`fire.wave`,
`disciples.madden`), et un effet d'ambiance posé sur un coup déjà terminé ne
bloque pas (`vampires.flock`, `reaper.soul` — tous deux `blocking:false` dans
`fx_stage.ml:306-317`).

**Un effet bloquant doit fixer `duration` à ce que le joueur voit.** Si le code
de dessin se termine à 0,8 s mais que la recipe indique 1,5, le tour attend une
demi-seconde après la disparition de l'image.

### 1.3 La convention de fade

Presque tous les effets se terminent par les mêmes trois lignes :

```ocaml
let fade = Float.min 1. ((duration -. age) /. 0.2) in
if fade > 0.02 then ...
```

Une rampe linéaire vers zéro sur les 0,2 dernières secondes, avec une coupure
nette à 2 % d'alpha — ce qui est aussi le seuil où les primitives cessent de
dessiner quoi que ce soit (§1.4). Les effets n'ont pas de fade-*in* commun :
l'apparition se fait élément par élément, en donnant à chacun un décalage `born`
et en le laissant invisible jusque-là (`born = index · 0.07` pour un saut
d'éclair, `born = noise e 2 5 · something` pour une braise). Les easings
disponibles sont `clamp01`, `ease_out u = 1 − (1 − u)²`, et `swing_out`, un
dépassement de type back-out utilisé uniquement pour l'effet de pop d'échelle de
la bannière de victoire.

### 1.4 Coordonnées, mise à l'échelle et culling

Le plateau est conçu dans un espace de 832×768 (`gui_draw.ml:9-10`) et
transposé par deux fonctions :

```ocaml
let sx x = !off_x +. (x *. !scale)
let sy y = !off_y +. (y *. !scale)
```

`fx_core.ml:38-39`, fixées une fois par frame à partir de la taille de la
fenêtre (`bin/gui.ml:246`). Tout effet situé au-dessus de ce niveau est
indépendant de la résolution ; seul le texte se multiplie à la main par
`!scale`.

Les cinq primitives refusent toutes le travail sous-pixel, ce qui rend la vague
de flammes (§3) abordable :

```ocaml
let circle x y rad c = if rad *. !scale > 0.4 then R.draw_circle_v (v x y) (rad *. !scale) c
let line x0 y0 x1 y1 thick c = if thick *. !scale > 0.3 then ...
let ellipse x y rx ry c = if rx *. !scale > 0.4 && ry *. !scale > 0.4 then ...
```

### 1.5 Le blending additif est la façon de dessiner l'« énergie »

`additive f` enveloppe un dessin dans `BlendMode.Additive`, qui dans Raylib vaut
`(SRC_ALPHA, ONE)` : le framebuffer gagne `couleur · α`. Une seule passe en alpha
blending ne peut jamais être plus brillante que sa propre couleur, mais N passes
translucides empilées s'additionnent. Chaque lueur de ce code est donc faite de
trois ou quatre passes de rayon décroissant et d'alpha croissant — voir le
tableau des éclairs au §2.6 et celui des flammes au §3.2. `additive_style`
(`fx_core.ml:274-276`) est le seul endroit où cela est désactivé : rochers,
pousses, yeux et pointes sont des objets solides, et les additionner les ferait
virer au blanc ; ils sont donc dessinés normalement.

L'ordre de dessin compte dans l'autre sens pour les passes en alpha blending :
Raylib dessine les appels ultérieurs par-dessus les précédents, et
`bin/gui.ml:398` appelle `Fx.draw_all` après le plateau, les emplacements et les
mains, précisément pour que les éclairs atterrissent *par-dessus* les cartes. Le
commentaire de ce fichier le dit.

### 1.6 Deux sortes d'aléa

`noise a b c` (`fx_core.ml:72-76`) est un hash d'entiers sans état qui renvoie
un flottant dans `[0, 1)` :

```ocaml
let n = (a * 374761393) + (b * 668265263) + (c * 1442695041) in
let n = (n ^ (n >> 13)) * 1274126177 in
(n & 0xffffff) /. 16777215.
```

Il sert aux propriétés qui doivent être **stables pendant la durée d'une frame
mais différer entre voisins** : le côté irrégulier d'une colonne de feu, le fait
qu'une carte reçoive ou non une tranche de glitch, l'endroit où naît une
braise. Les éléments qui varient d'une frame à l'autre utilisent
`sin (t·ω + …)` ou un `Random.State`. Se tromper dans ce sens est le bug
classique d'une réécriture d'effets : un effet qui réinitialise sa graine
aléatoire à chaque frame devient de la neige télévisuelle, et un effet qui ne
hache que l'indice de temps devient une image figée.

Les couleurs passent par un unique clamp afin qu'un effet puisse dépasser sa
plage de conception sans repasser à zéro :

```ocaml
let byte x = int_of_float (Float.max 0. (Float.min 255. (x *. 255.)))
```

## 2. Éclairs

*Approfondissement : les chiffres des §2.2–2.6 expliquent pourquoi les constantes
sont ce qu'elles sont ; on peut utiliser `spawn_lightning` sans eux.*

`fx_lightning.ml`, 157 lignes, utilisé par cinq sorts (`air.call`,
`air.lightnin`, `air.chain`, `wind.BallLightning.name`, `elementalist.frost`) et
une capacité (`zen.spark`). La signature est volontairement simple : une liste
de points à traverser, et un style optionnel.

```ocaml
spawn_lightning ?(style = electric) [ own_life; foe.(2); foe_life ] ~start
```

Trois points forment une chaîne de deux sauts : l'éclair frappe la première
paire 70 ms après le début, la seconde paire 70 ms plus tard, puis tout ce qui a
été touché reste allumé pendant 0,7 s pendant que l'ensemble s'estompe.

### 2.1 Le chemin est une fractale à déplacement du point médian

L'algorithme classique du déplacement du point médian, en 28 lignes
(`fx_lightning.ml:25-52`) :

```ocaml
let jag rng a b generations sway =
  let pts = ref [| a; b |] in
  let disp = ref sway in
  for _ = 1 to generations do
    ...
    let mx = (p.x +. q.x) *. 0.5 and my = (p.y +. q.y) *. 0.5 in
    let dx = q.x -. p.x and dy = q.y -. p.y in
    let len2 = (dx *. dx) +. (dy *. dy) in
    let mid =
      if len2 > 64. then
        let inv = 1. /. sqrt len2 in
        let off = (Random.State.float rng 1. -. 0.5) *. 2. *. !disp in
        { x = mx +. (-.dy *. inv *. off); y = my +. (dx *. inv *. off) }
      else { x = mx; y = my }
    in
    ...
    disp := !disp *. 0.55
  done;
  !pts
```

Lecture des maths :

- **Perpendiculaire, pas une direction aléatoire.** Le déplacement vaut
  `off · (−dy, dx)/|d|`, c'est-à-dire la normale unitaire du segment multipliée
  par un décalage uniforme dans `(−disp, +disp)`. Déplacer le long de la normale
  laisse les extrémités fixes, ce qui rend la récursion composable : les deux
  moitiés du segment parent se rejoignent toujours.
- **Le rapport est 0,55.** L'amplitude de chaque génération vaut 0,55 de celle
  de son parent. C'est le « H » du mouvement brownien fractionnaire, et 0,55 est
  un compromis visuel réglé à la main : des rapports plus grands donnent un
  zigzag manifestement géométrique, des rapports plus petits donnent du bruit
  sans direction à grande échelle.
- **Le plancher de 8 px.** `len2 > 64.` supprime le décalage quand un segment
  mesure moins de 8 px. Un déplacement sur un segment de 2 px n'est pas une
  ondulation, c'est une discontinuité.

### 2.2 Jusqu'où il s'écarte, et pourquoi

Prenons le décalage d'une génération comme `U(−d_k, d_k)` avec
`d_k = sway · 0.55^k`. La variance de `U(−d, d)` est `d²/3` ; donc, si tous les
décalages pointaient le long de la même normale, l'écart latéral du sommet le
plus profond aurait

```text
Var = Σ_k d_k² / 3 = sway²/3 · 1/(1 − 0.55²) = 0.478 · sway²
σ   = 0.69 · sway
```

Ils ne pointent pas dans le même sens : à chaque niveau, la normale est celle
d'une polyligne que le niveau précédent a déjà courbée, si bien qu'une forte
inclinaison absorbe une partie de la composante verticale du décalage suivant.
En simulant le code réel (6 générations, `sway = 62`, corde de 300 px, 1,3 M de
sommets) :

| statistique | mesuré | idéal à normales alignées |
|---|---|---|
| RMS sur tous les sommets | 24,2 px = **0,39 · sway** | — |
| RMS au sommet le plus profond | 36,0 px = **0,58 · sway** | 0,69 · sway |
| plus grand \|décalage\| observé | 81 px = **1,31 · sway** | borne 2,16 · sway |
| borne stricte (tous les décalages à leur extrême, même direction) | — | `sway / 0.45 = 2.22 · sway` |

Conclusion : pour un éclair prévu avec `sway = 40`, attendez-vous à ce qu'il
s'écarte d'environ 16 px en général et de 50 px au pire. Le chiffre
`2.22 · sway` est la somme géométrique `sway(1 + 0.55 + 0.55² + …)` et n'est pas
atteignable : il faudrait que chacune des six uniformes indépendantes tombe sur
son extrême, dans la même direction.

### 2.3 La portée détermine la résolution

`build` (`fx_lightning.ml:72-78`) est l'endroit où la géométrie est choisie :

```ocaml
let sway = Float.min 62. (Float.max 14. (len *. 0.22)) in
let gens = if len > 220. then 6 else if len > 100. then 5 else 4 in
```

| corde | `sway = clamp(0.22·len, 14, 62)` | gens | points = 2^g+1 | segment = len/2^g | RMS (0,39·sway) |
|---|---|---|---|---|---|
| 60 px | 14,0 (0,22·60 = 13,2 → relevé au plancher) | 4 | 17 | 3,75 px | 5,5 px |
| 120 px | 26,4 | 5 | 33 | 3,75 px | 10,3 px |
| 240 px | 52,8 | 6 | 65 | 3,75 px | 20,6 px |
| 400 px | 62,0 (0,22·400 = 88 → plafonné) | 6 | 65 | 6,25 px | 24,2 px |
| 700 px | 62,0 | 6 | 65 | 10,9 px | 24,2 px |

Deux choses ressortent de ce tableau :

- **La longueur du dernier segment est presque constante, à 3,75 px.** Le nombre
  de générations est choisi précisément pour que `len / 2^g` tombe juste sous le
  seuil de 8 px du déplacement. C'est toute la raison d'être des seuils 220/100 —
  il faudrait un quatrième seuil pour les sauts vraiment longs, et il n'existe
  pas ; donc, à partir de 512 px de corde (`64 × 8`), la génération la plus fine
  ajoute des sommets mais aucun jitter.
- **L'amplitude sature.** Au-delà de 282 px de corde, `0.22·len` demande plus que
  le plafond de 62 px ; un éclair traversant tout le plateau et un éclair de
  400 px s'agitent donc de la même quantité. `62 / 0.22 ≈ 282` est la portée à
  partir de laquelle un éclair cesse de devenir plus agressif en
  s'allongeant.

Un chemin **secondaire** est généré en parallèle, pour les sauts longs
uniquement : mêmes extrémités, une génération de moins, moitié moins de sway
(`fx_lightning.ml:77`). Il est dessiné *sous* le principal et c'est lui qui fait
qu'un long éclair ressemble à un éclair plutôt qu'à un fil — deux fissures
indépendantes qui ne se rejoignent jamais.

### 2.4 Les fourches

`forks` (`fx_lightning.ml:54-68`) fait pousser une ou deux branches latérales :

```ocaml
let count = if span > 220. then 2 else 1 in
let ang = atan2 (p.y -. prev.y) (p.x -. prev.x)
          +. (if Random.State.bool rng then 0.85 else -0.85)
          +. ((Random.State.float rng 1. -. 0.5) *. 0.5) in
let bl = Float.min 78. (span *. (0.16 +. Random.State.float rng 0.18)) in
jag rng p { ... } 3 (bl *. 0.4)
```

- **±0,85 rad = ±49°** par rapport à la direction locale, avec ±14° de jitter
  supplémentaire. À peu près perpendiculaire, avec du jitter — une fourche qui
  part sous un angle faible se lit comme un coude, pas comme une fourche.
- **La longueur vaut 16–34 % de la portée, plafonnée à 78 px**, dessinée avec 3
  générations et un sway de 0,4·longueur. C'est un sway relatif *important* pour
  une branche de 40 px, donc les fourches courtes s'enroulent fortement.
- **Pas de fourche sous 8 points ou 50 px de portée.** Un saut court a trop peu
  de sommets pour y choisir un point de branchement crédible.

### 2.5 Le planning des sauts et leur alpha

`hop_delay = 0.07`, `lightning_hold = 0.7` (`fx_lightning.ml:22-23`), et

```text
duration = (points − 1) · 0.07 + 0.7
```

| sort | points | sauts | durée | début du dernier saut |
|---|---|---|---|---|
| `air.lightnin` | 2 | 1 | 0,77 s | 0,00 s |
| `air.call` | 3 | 2 | 0,84 s | 0,07 s |
| `elementalist.frost` | 3 | 2 | 0,84 s | 0,07 s |
| `air.chain` | 7 | 6 | 1,12 s | 0,42 s |

L'alpha par saut (`hop_alpha`, `fx_lightning.ml:94-102`) est le produit de trois
facteurs indépendants :

```ocaml
let strike = if age < 0.045 then 1. else 0.8 in
let flick = if (int_of_float (t *. 20.) + index) mod 4 = 0 then 0.4 else 1. in
let fade = Float.min 1. ((duration -. t) /. 0.22) in
strike *. flick *. Float.max 0. fade
```

- **`strike`** — les 45 premières ms d'un saut sont dessinées à pleine
  luminosité, puis descendent à 0,8 pour le reste de leur vie. Un vrai impact
  est un flash suivi d'une rémanence ; ceci est le flash.
- **`flick`** — une horloge à 20 Hz, un tick sur quatre atténué à 0,4. Cela donne
  une pulsation à 5 Hz avec un rapport cyclique de 25 %, de multiplicateur
  moyen 0,85. Le `+ index` est la partie importante : un saut s'atténue au tick
  où `tick ≡ −index (mod 4)`, donc le saut 0 s'atténue au tick 0, le saut 1 au
  tick 3, le saut 2 au tick 2, le saut 3 au tick 1 — la chaîne parcourt les
  quatre ticks à rebours et ne pulse jamais dans son ensemble. Elle crépite.
- **`fade`** — la rampe maison de 0,2 s, réalisée sur 0,22 s.

Exemple : le saut 3 de `air.chain` à sept points (durée 1,12) à `t = 0.26`.

```text
born  = 3 · 0.07 = 0.21,  age = 0.05
strike = 0.8                       (age > 0.045)
flick  = (int(0.26 · 20) + 3) mod 4 = (5 + 3) mod 4 = 0  -> 0.4
fade   = min 1 ((1.12 − 0.26)/0.22) = 1
alpha  = 0.8 · 0.4 · 1 = 0.32
```

Vingt millisecondes plus tôt (`t = 0.24`, tick 4), le même saut est dessiné à
0,8. Cette alternance à 20 Hz est tout le crépitement.

**La géométrie est reconstruite à ce même 20 Hz**, ce qui est une décision
distincte du scintillement et la raison pour laquelle ils sont codés comme deux
horloges (`fx_lightning.ml:142-148`) :

```ocaml
let next = int_of_float (age /. 0.05) in
if next <> !frame then (frame := next; strikes := Array.mapi ... (build rng a b) ...)
```

Reconstruire à chaque frame coûterait 6× plus de tirages `Random.State` pour une
différence que personne ne voit à 60 fps ; reconstruire moins souvent que le
scintillement donne l'impression que l'éclair clignote dans son ensemble. 20 Hz
est le point où les deux se lisent comme un seul phénomène. Les éclairs restent
allumés pour le reste de l'effet avec une géométrie *figée*, ce qui est correct :
un canal de foudre est un chemin brièvement éclairé, pas un chemin mobile.

### 2.6 Peinture : trois traits imbriqués

`paint_strike` (`fx_lightning.ml:104-125`) dessine la même polyligne huit fois.

| passe | largeur | alpha | couleur |
|---|---|---|---|
| lueur secondaire | 0,65 · glow_w | 0,28 | `glow` |
| lueur primaire | glow_w | 0,42 | `glow` |
| lueur de fourche | 0,45 · glow_w | 0,30 | `glow` |
| corps secondaire | 4,2 | 0,55 | `body` |
| corps primaire | 5,2 | 0,80 | `body` |
| corps de fourche | 2,4 | 0,70 | `body` |
| cœur primaire | 2,0 | **1,00** | `core` |
| cœur de fourche | 1,3 | 0,90 | `core` |

avec

```text
glow_w = min(18, 8 + 0.015 · span)      -> 14 px for a 400 px hop, capped at 18
```

Trois propriétés de ce tableau comptent :

- **La lueur est proportionnelle à la portée, le corps et le cœur non.** Un long
  éclair reçoit un halo proportionnellement plus épais — physiquement juste (plus
  d'air à éclairer) et cela évite que le halo ressemble à une corde enroulée
  autour d'un fil.
- **Le cœur est dessiné en dernier, à pleine alpha.** En additif, le pixel
  central reçoit `0.42 + 0.80 + 1.00 = 2.22` fois l'alpha, il sature donc à la
  couleur du cœur tandis que les bords restent dans la teinte `glow`. Le centre
  blanc incandescent n'est dessiné nulle part ; c'est la somme.
- **`stroke` dessine un cercle à chaque sommet quand `caps` est vrai**
  (`fx_lightning.ml:80-92`), de sorte que les jointures sont arrondies et que la
  polyligne n'a pas de trous aux coudes. Seules les trois passes de lueur
  demandent des caps : un cap de 14 px est une boule ronde qui masque la
  jointure, alors qu'un cap de 2 px sur le cœur aurait simplement la même largeur
  que la ligne.

Puis deux cercles sur la cible (rayon 11 à 0,45 de body, 4,5 à 1,0 de core) pour
le point d'impact.

**Coût.** Un saut de 6 générations, c'est 64 segments × 8 passes ≈ 512 appels
`line`, plus un cercle de cap par sommet sur les trois passes de lueur (~195 de
plus), soit ~700 appels de dessin par saut et par frame ; `air.chain` en lance
six. C'est le deuxième effet le plus coûteux du code (le premier est au §3.5).

### 2.7 Les cinq styles

```ocaml
let electric = { glow = (0.48, 0.22, 1.); body = (0.35, 0.78, 1.); core = (0.95, 0.98, 1.); orb = false }
let frost    = { glow = (0.25, 0.55, 1.); body = (0.7, 0.95, 1.);  core = (0.92, 1., 1.);    orb = false }
let ball     = { glow = (0.55, 0.3, 1.);  body = (0.45, 0.8, 1.);  core = (1., 0.97, 0.85);   orb = true }
let spark    = { glow = (1., 0.82, 0.12); body = (1., 0.92, 0.28); core = (1., 0.98, 0.82);   orb = false }
let drain    = { glow = (0.82, 0.88, 1.); body = (0.95, 0.97, 1.); core = (1., 1., 1.);      orb = false }
```

Quatre des cinq sont de pures couleurs. `ball` active aussi `orb`, ce qui ajoute
une sphère qui voyage le long du saut (`fx_lightning.ml:120-125`) :

```ocaml
let u = clamp01 ((t -. 0.02) /. 0.28) in
let x = from_p.x +. ((to_p.x -. from_p.x) *. u) in
circle x y 18. (rgb gr gg gb (alpha *. 0.55));
circle x y 7.  (rgb cr cg cb alpha)
```

Un transit à vitesse constante de 0,30 s avec 20 ms d'avance, dessiné comme la
même paire halo-plus-cœur que tout le reste. À noter que le transit se fait en
temps d'*effet*, pas par saut : une chaîne de boules à sept points a donc ses six
orbes en vol simultanément.

## 3. Feu

*Approfondissement : parcourez les listings de code, retenez l'idée — un front
qui avance le long de la rangée, haut et blanc en tête, avec un lit bas et
permanent derrière.*

`fx_flames.ml`, 185 lignes, pour `fire.wave` et `fire.inferno`. Trois couches :

1. `flame_column` — une flamme verticale, dessinée comme une pile de cercles.
2. `fire_sheet` — un front d'onde qui parcourt la rangée, en appelant la colonne
   35 fois par frame.
3. `sheet_embers` — des étincelles qui suivent le front.

### 3.1 La colonne

`flame_column` (`fx_flames.ml:12-48`) dessine `licks` (3 dans une nappe, 5 et 4
dans l'inferno) flammes côte à côte. Pour la langue *k* d'une colonne de hauteur
*h* :

```ocaml
let lean = sin ((t *. 3.2) +. (float id *. 0.73) +. (float k *. 1.4)) *. 0.45 +. push in
let h = height *. (0.78 +. (0.22 *. (0.5 +. (0.5 *. sin ((t *. 11.) +. ...))))) in
let steps = max 6 (min 14 (int_of_float (h /. 7.))) in
let side = (float k -. mid) *. fat *. 6.5 in
```

Quatre décisions en six lignes :

- **L'errance latérale est lente et propre à chaque langue** — `3.2 rad/s` ≈
  0,5 Hz, avec le décalage de phase par langue `1.4·k` pour que les langues ne
  bougent pas comme un seul objet.
- **La hauteur est modulée à `11 rad/s`** ≈ 1,75 Hz entre 78 % et 100 %. Le feu
  scintille plus vite qu'il ne respire ; le balancement et le scintillement sont
  volontairement à des fréquences différentes, pour la même raison que les
  éclairs ont deux horloges.
- **`steps` est une constante de résolution** : un cercle tous les 7 px de
  flamme, borné à 6–14 pas. Une colonne d'inferno de 150 px reçoit 14 pas
  d'environ 11 px ; une de 40 px en reçoit 6 d'environ 7 px.
- **`side` écarte les langues** de `fat · 6.5` px par pas à partir du centre —
  trois langues s'étalent sur ±6,5 px dans une colonne de ~17 px de rayon, ce qui
  empêche une colonne de ressembler à une grosse saucisse.

Le profil vertical d'une langue, à la fraction `v` de la base (0) à la pointe
(1) :

```ocaml
let pinch = (1. -. vv) ** 0.62 in
let wob = sin (phase +. (vv *. 4.6)) *. (2.5 +. (9. *. vv)) in
px.(s) <- x +. side +. (lean *. vv *. 34.) +. wob;
py.(s) <- y -. (vv *. h);
pr.(s) <- fat *. (6.2 +. (11. *. pinch))
```

- **L'effilement est `(1 − v)^0.62`, pas `(1 − v)`.** Le rayon vaut
  `17.2 · fat` à la base ; `6.2 · fat` à la pointe. Avec un exposant de 1, le
  profil serait un cône droit ; en dessous de 1, il reste gras sur le tiers
  inférieur puis chute — la silhouette d'une flamme plutôt que d'un triangle.
  Valeurs mesurées : `pinch(0.25) = 0.84`, `pinch(0.5) = 0.65`,
  `pinch(0.75) = 0.42`, `pinch(0.9) = 0.24`.
- **L'amplitude d'ondulation croît linéairement jusqu'à la pointe**
  (`2.5 + 9v` px), pour la même raison que la fractale du §2 devient plus
  rugueuse à mesure qu'elle s'affine : la pointe est la partie la moins
  contrainte de la forme.
- **`lean · v · 34`** transforme le balancement en courbure — le décalage croît
  avec la hauteur, si bien que la flamme se penche au lieu de glisser sur le
  côté.

### 3.2 Quatre passes, et pourquoi la partie brillante est courte

```ocaml
paint 1.    0.2  0.02 (alpha *. 0.26) 1.2   1.;                 (* outer red  *)
paint 1.    0.5  0.06 (alpha *. 0.38) 0.74  0.84;                (* orange     *)
paint 1.    0.88 0.32 (alpha *. 0.48) 0.42  0.58;                (* yellow     *)
paint 1.    0.97 0.9  (alpha *. 0.4 *. hot) 0.24 (0.28 +. 0.18 *. hot)  (* white core *)
```

Chaque passe utilise `rayon · échelle` sur les `steps · reach` premiers pas.
L'échelle du rayon passe de 1,2 à 0,24 et la portée (reach) se réduit de 1,0 à
0,28, en additif. **Le cœur blanc incandescent ne couvre que les 28 % du bas de
la flamme**, ce qui donne l'impression d'une chaleur qui monte plutôt que d'un
dégradé. La quatrième passe est conditionnée par `hot`, la valeur de crête de la
nappe (§3.3) : une colonne en tête de vague a un cœur blanc, une colonne située
300 px derrière est d'un rouge terne sur toute sa hauteur.

### 3.3 La nappe : un front qui voyage

`fire_sheet` (`fx_flames.ml:53-93`) échantillonne 35 colonnes sur la rangée et
demande, pour chacune : *à quelle distance derrière le front qui avance se
trouve-t-elle ?*

```ocaml
let reach = t *. speed in
let behind = reach -. dist in
let ignite = clamp01 ((behind +. 12.) /. 30.) in
let body = exp (-.Float.max 0. (behind -. 40.) /. 230.) in
let heat = ignite *. Float.max (ignite *. 0.35) body in
let crest = exp (-.(behind -. 25.) *. (behind -. 25.) /. 2400.) in
let h = (24. +. (56. *. heat)) *. (0.8 +. (0.9 *. crest)) *. flick *. ragged *. power in
```

Quatre fonctions, chacune avec son rôle :

- **`ignite`** — une rampe de 30 px qui va de « pas encore atteinte » à
  « entièrement embrasée », décalée de 12 px pour qu'une colonne apparaisse en
  fondu juste avant que le front ne la touche.
- **`body`** — une décroissance exponentielle avec une **longueur
  caractéristique de 230 px dans l'espace** (le front avance à vitesse
  constante, c'est donc aussi une constante de temps de `230 / speed`
  secondes). À 948 px/s, cela fait 0,24 s de combustion derrière le front.
- **`heat = ignite · max(ignite · 0.35, body)`** — le `max` interne est un
  plancher : une colonne qui brûle depuis longtemps reste à 35 % de chaleur, de
  sorte que la nappe laisse un lit bas permanent de flammes au lieu de se
  terminer brutalement. C'est aussi pourquoi la queue lointaine est faible *et*
  courte plutôt qu'invisible.
- **`crest`** — une **bosse gaussienne centrée 25 px derrière le front**,
  `σ² = 2400/2 = 1200`, donc `σ ≈ 35 px`. C'est la « tête » visuelle de la
  vague : c'est là que les colonnes sont les plus hautes et que `hot` laisse
  entrer le cœur blanc.

Hauteur, exemple chiffré (`power = 1`, scintillement moyen 0,94, ragged moyen
1,0) :

| `behind` | `heat` | `crest` | hauteur |
|---|---|---|---|
| −12 px | 0,00 | 0,00 | 18 px |
| 0 | 0,40 | 0,77 | 65 px |
| 25 (pic de crête) | 1,00 | 1,00 | **128 px** |
| 100 | 0,77 | 0,10 | 56 px |
| 300 | 0,35 (le plancher) | ~0 | 33 px |
| 700 | 0,35 (le plancher) | ~0 | 33 px |

Les deux dernières lignes forment la paire intéressante. L'exponentielle croise
le plancher là où `exp(−(b−40)/230) = 0.35`, soit à
`b = 40 + 230·ln(1/0.35) ≈ 282 px` derrière le front — et ensuite le lit **cesse
complètement de décroître** et reste à 33 px constants de flamme terne jusqu'à
la fin de l'effet. La vague est donc un front brillant au-dessus d'un long lit
permanent et bas : le feu couvre toute la rangée et y reste — à quoi ressemble
« le sol est en feu » — plutôt qu'une vague qui s'arrête visiblement.

`flick` vaut par colonne `0.88 + 0.12 sin(30t + i)` — 4,8 Hz, avec un décalage
de phase par colonne, ce qui empêche 35 colonnes de pulser à l'unisson.
`ragged` vaut `0.8 + 0.4 · noise i seed 7`, la variation permanente par colonne
du §1.6.

Les colonnes **se penchent aussi dans leur sens de déplacement** :
`push = side · 0.95 · crest`, où `side` vaut +1 ou −1 selon le côté d'où le
front est arrivé. Uniquement à la crête : la tête de la vague se courbe et la
queue reste droite.

### 3.4 La vitesse est dérivée de la géométrie

```ocaml
let bounds bases = (min_x -. 44., max_x +. 44.)        (* fx_flames.ml:116 *)
let speed = span /. 0.62                                 (* flame wave *)
let speed = span /. 0.55                                 (* inferno *)
```

Six emplacements à un pas de 100 px couvrent 500 px de centres, et `bounds`
ajoute 44 px de chaque côté, donc `span = 588 px`. La vague traverse ainsi toute
la rangée élargie en exactement 0,62 s quelle que soit la disposition du
plateau, à **948 px/s** (inferno : 1069 px/s), et le pas de colonne à l'écran
est `588 / 34 = 17.3 px` — confortablement à l'intérieur du rayon de colonne de
~17–27 px, ce qui explique que les colonnes adjacentes fusionnent en nappe au
lieu de ressembler à une palissade.

`spawn_inferno` (`fx_flames.ml:139-185`) est la même mécanique avec une
chorégraphie différente :

1. **Engloutissement**, en premier : deux colonnes sur la cible (150 px et 80 px
   de haut, `fat` 1,9 et 2,5, cinq et quatre langues) sous une enveloppe ADSR
   `rise 0.08, hold 0.7, fade 0.4`, plus un disque de lueur de 34 px. La colonne
   plus large et plus courte devant la plus haute fait que le milieu se lit
   comme une masse de feu solide.
2. **Puis la vague**, après un délai de 0,16 s, à 0,85 de puissance dans les
   *deux* directions (`dirs: 0`), de sorte qu'elle part de la cible vers
   l'extérieur plutôt que de traverser la rangée.
3. **Un anneau de choc** pendant les 0,45 premières secondes : 20 points sur un
   cercle de rayon `16 + 150t`, écrasé à 0,42 en *y* pour qu'il paraisse posé au
   sol.
4. **Étincelles** : 30 bouffées balistiques, `x = tx + cos(ang) · speed · t`,
   `y = ty − 8 + sin(ang) · speed · t − 40t`, avec `speed = 80 + 170 · noise`,
   s'estompant selon `(1 − p)`.

Les braises (`sheet_embers`, `fx_flames.ml:96-114`) sont la version économique
de la même idée — pas de trigonométrie, juste `p = a/life` :

```ocaml
let born = Float.abs d /. speed in          (* born when the front passes *)
let ey = y -. 14. -. ((70. +. (120. *. noise e 4 6)) *. p) in
let col = if noise e 9 3 > 0.4 then rgb 1. 0.92 0.55 ((1. -. p) ** 2. *. fade)
          else rgb 1. 0.45 0.05 ((1. -. p) ** 2. *. fade *. 0.9) in
```

`born` est dérivé de la *même* position de front que la nappe, de sorte que les
étincelles apparaissent sous la flamme exactement quand le feu les atteint,
plutôt que sur un minuteur indépendant. L'alpha quadratique `(1 − p)²` concentre
la luminosité au début, et la séparation `noise > 0.4` donne un mélange de
cendre pâle et de braise profonde.

### 3.5 Coût

35 colonnes × 3 langues × jusqu'à 14 pas × 4 passes ≈ **5 900 cercles par
frame**, plus 70 braises, plus le lit de braises de trois lignes. C'est l'effet
le plus coûteux du jeu, et c'est la raison pour laquelle les primitives
éliminent le travail sous-pixel et pour laquelle le nombre de `steps` est dérivé
de la hauteur au lieu d'être fixe. Sur une machine lente, c'est l'effet qui fait
chuter les frames ; c'est aussi le seul à avoir un paramètre `dirs`, et les
colonnes vers lesquelles la vague ne voyage pas sont ignorées avant tout
dessin.

## 4. Maddening

`spawn_madden` (`fx_scenes.ml:14-90`), l'hypnose de masse des Disciples. C'est
le seul effet qui touche le *plateau* au lieu de peindre dessus, et il mérite
d'être lu en entier car c'est l'exemple le plus clair d'un effet où trois
horloges indépendantes pilotent trois parties différentes de la même image.

### 4.1 Ce qu'on lui fournit

```ocaml
spawn_madden ~foes:foe_ids ~allies:ally_ids ~eye ~start
```

depuis `fx_stage.ml:102-117`. `foes` est chaque emplacement ennemi occupé (ou les
six si le plateau ennemi est vide), `allies` est chaque emplacement allié
occupé, et `eye` est calculé à partir du layout :

```ocaml
let eye = {
  x = Array.fold_left (fun s p -> s +. p.x) 0. foe_row /. float nb_slots;
  y = (foe_row.(0).y +. own_row.(0).y) *. 0.5;
}
```

— le centroïde en *x* de la rangée adverse et le milieu entre les deux rangées.
Avec six emplacements régulièrement espacés, c'est le centre horizontal du
plateau, entre les emplacements 2 et 3, là où l'œil doit se trouver : au milieu
de ce qui est hypnotisé.

`spawn_madden` enregistre aussi les **shakes** immédiatement, avant tout dessin
(§4.5).

### 4.2 Trois horloges

```ocaml
let fade    = Float.min 1. ((madden_duration -. age) /. 0.2) in   (* 1.28 s life *)
let open_   = ease_out (clamp01 (t /. 0.28)) in
let madness = ease_out (clamp01 ((t -. 0.1) /. 0.35)) in
let ramp    = clamp01 (t /. madden_duration) in
let shake   = ramp *. ramp in
```

- `fade` — la queue standard de 0,2 s.
- `open_` — la **paupière**, de 0 à 1 en 0,28 s avec `ease_out`, de sorte que la
  moitié de l'ouverture est faite dans les 0,07 premières secondes et se
  stabilise bien avant que l'œil ait fini de briller.
- `madness` — le **glitch**, retardé de 0,1 s et terminé à 0,45 s, de sorte que
  l'œil s'ouvre sur un plateau immobile et que la corruption arrive après.
- `shake = ramp²` — la **violence du plateau**, une rampe quadratique sur toute
  la durée de 1,28 s, utilisée à la fois pour le cisaillement du glitch et pour
  les secousses des emplacements.

La séparation compte : l'œil d'abord, puis la corruption, puis un long
crescendo. Trois horloges permettent aux trois de se produire sans machine à
états.

### 4.3 Le glitch

Par carte, dessiné comme un rectangle `78 × 88` (l'emplacement fait 90 × 102, le
cadre se place donc juste à l'intérieur) :

```ocaml
let split = (3. +. (shake_m *. 10.)) *. (0.6 +. (0.4 *. sin (t *. (18. +. float id)))) in
rect (x +. split)  y w h (rgb 0.95 0.08 0.1  (fade *. 0.22));   (* red copy   *)
rect (x -. split) y w h (rgb 0.05 0.75 0.85 (fade *. 0.16));   (* cyan copy  *)
```

C'est de l'**aberration chromatique** : deux copies de la silhouette de la carte
dans des couleurs complémentaires, décalées en sens opposés. Le décalage vaut
`(3 + 10·shake)` px multiplié par une oscillation par carte
`0.4 sin(18t + id)` ; au repos il est donc de 1,8–3 px (une image double à peine
perceptible) et à pleine secousse jusqu'à 13 px. Le `+ float id` dans la
fréquence fait que les cartes voisines battent les unes contre les autres au
lieu d'osciller à l'unisson.

Par-dessus les copies, quatre couches de plus :

- **Une bande de balayage** qui descend la carte : `u = mod(1.6t + noise)`,
  vitesse `1.6 + 4.5·shake` hauteurs par seconde, épaisseur `5 + 7·shake` px,
  rose pâle.
- **Une tranche déchirée** sur 55 % des cartes (`noise id 8 4 > 0.45`) : une barre
  sombre de hauteur `8 + 16n` qui dérive à `2.4 + id` hauteurs par seconde,
  décalée latéralement de `(n − 0.5)·18` px.
- **Neuf lignes de balayage horizontales**, de 1,4–2,6 px d'épaisseur, clignotant
  à `sin(30t + 1.7k + id)` — environ 4,8 cycles par seconde, avec le décalage de
  phase par ligne pour que le clignotement ressemble à du bruit.
- Les alliés reçoivent le même traitement à `0.38` de l'alpha et `0.4` de la
  secousse, de sorte que l'effet hypnotique se concentre visiblement sur
  l'ennemi.

### 4.4 L'œil

Six ellipses imbriquées (sclérotique, bord de sclérotique, milieu, intérieur,
iris, pupille), dix fibres radiales d'iris, une pupille et un point de reflet
(`fx_scenes.ml:64-85`) :

```ocaml
let rx = 40. and ry = 4. +. (22. *. open_) in
let irx = rx *. 0.48 and iry = ry *. 0.78 in
...
for k = 0 to 9 do
  let ang = (float k *. (Float.pi /. 5.)) +. (t *. 0.4) in
  let len = irx *. (0.55 +. (0.35 *. noise k 2 1)) in
  line x y (x +. (cos ang *. len)) (y +. (sin ang *. len *. (iry /. Float.max irx 1.)))
done;
```

| `t` | `open_` | `ry` | rapport de l'œil `ry/rx` | ce qui se passe |
|---|---|---|---|---|
| 0,00 | 0,00 | 4,0 | 0,10 | une fente fermée ; les copies rouge/cyan sont à peine décalées |
| 0,10 | 0,59 | 16,9 | 0,42 | l'œil est presque entièrement ouvert |
| 0,28 | 1,00 | 26,0 | 0,65 | entièrement ouvert ; le glitch est à 0,76 |
| 0,45 | 1,00 | 26,0 | 0,65 | glitch à pleine force ; **la pupille commence à se dilater** |
| 0,55 | 1,00 | 26,0 | 0,65 | pupille à sa largeur maximale |
| 1,08 | 1,00 | 26,0 | 0,65 | dernière frame avant le fade ; secousse maximale |
| 1,28 | — | — | — | `fade = 0`, rien n'est dessiné |

Deux détails dans la géométrie de l'œil :

- **L'écrasement vertical des fibres de l'iris** — `len · (iry / irx)` — est ce
  qui fait que dix lignes rayonnant depuis le centre se lisent comme un motif
  d'iris *circulaire* dans un œil *elliptique*. Sans lui, les fibres formeraient
  une étoile aplatie.
- **La pupille se dilate selon son propre calendrier** :
  `slit = 1 + 0.8 sin(22t + 1.2) · clamp01((t − 0.45)/0.1)`, de sorte que la
  pupille est une fente fixe de 5,2 px pendant les 0,45 premières secondes, puis
  se dilate à 9,4 px en 0,1 s. C'est la seule chose de l'effet qui empire à
  mesure que le sort réussit.

La dernière couche est additive : deux disques de rayon `28 + 10·open_` et 14,
pulsant à `0.55 + 0.45 sin(11t)`.

### 4.5 Les secousses du plateau

Les effets peuvent enregistrer une secousse pour un emplacement, et le rendu du
plateau demande le décalage courant à chaque frame (`fx_core.ml:240-251`,
utilisé en `gui_board.ml:913`) :

```ocaml
let percent = clamp01 ((now -. s.start) /. s.duration) in
let ramp = percent *. percent in
let freq = 10. +. (ramp *. (28. +. (42. *. s.strength))) in
let amp = (0.7 +. (ramp *. 6.2)) *. s.strength in
let phase = percent *. s.duration *. freq in
(sin (phase *. 2.17) *. amp, cos (phase *. 3.41) *. amp *. 0.42)
```

Notons que `percent · duration = elapsed`, donc `phase` est le temps écoulé ×
fréquence et le déplacement est une courbe de Lissajous à deux axes :

- **`ramp²`** — le même quadratique que le glitch utilise, de sorte que la
  secousse et la corruption visuelle montent en crescendo ensemble.
- **Amplitude ≤ 6,9 px** à pleine rampe, multipliée par `strength` (1,0 pour les
  ennemis, 0,38 pour les alliés).
- **La fréquence monte de 10 à 80 Hz** à mesure que la rampe croît.
- **Les multiplicateurs `2.17` et `3.41` sont à peu près premiers entre eux et
  assez irrationnels** pour que le motif ne se répète pas visiblement — l'astuce
  classique pour une secousse qui paraît animée à la main plutôt que bouclée.

Une conséquence agréable de l'interaction avec le fade : l'amplitude n'atteint
jamais réellement son plafond de 6,9 px, car à `ramp = 1` (`t = 1.28`) l'effet
s'est déjà complètement estompé. Le **pic visible est à `t = 1.08`**, où
`ramp = 0.84`, `shake = 0.71`, donc la plus grande secousse qu'un joueur voie
jamais est

```text
amp  = (0.7 + 0.71 · 6.2) = 5.1 px      freq = 10 + 0.71 · 70 = 59.7 Hz
```

— et 59,7 Hz est à la limite de ce qu'un écran à 60 fps peut afficher, ce qui
explique que la secousse se lise comme un bourdonnement haute fréquence plutôt
que comme un grondement. L'amplitude a été réduite en conséquence.

## 5. Le reste de la famille

### 5.1 Les treize looks partagés

La plupart des sorts n'ont pas de code sur mesure. Ils choisissent un `look`
dans la table de `lib/game/spell_looks.ml` — `{ style; aim; r; g; b; power }` —
et `draw_style` (`fx_core.ml:334-561`) le peint. `aim` décide *quels points*
dessiner (`points_of`, `fx_core.ml:261-270`) : `All_slots`, `Foe_slots`,
`Own_slots`, `Own_life`, `Target`, `Own_target`.

| style | forme | notes |
|---|---|---|
| `Stones` | 9 blocs de roche qui dégringolent par point, éclats d'impact et poussière | chute en `u^1.7`, 1,5 s, **non** additif, ombrage à facettes depuis une lumière virtuelle en haut à gauche |
| `Sprout` | trois tiges avec des feuilles qui s'ouvrent et un bourgeon | `open_²` écarte les feuilles |
| `Eyes` | deux yeux de tigre au portrait de vie | iris, pupille, point spéculaire |
| `Rise` | 7 particules par point qui montent avec un ease-out | alpha `(1−u)²` |
| `Strike` | un rayon de votre portrait vers le leur, puis une bouffée d'impact | trajet de 0,28 s |
| `Swirl` | 3 bras × 6 points, y écrasé à 0,55 | les sorts de vent |
| `Ice` | éclats qui tombent avec une traînée de mouvement | chute de 0,4 s |
| `Shadow` | taches qui planent, montent et s'estompent | 5 par point |
| `Rays` | disque pulsant plus 6 rayons en rotation | `Own_slots` / soins |
| `Drops` | 6 gouttes avec des anneaux d'éclaboussure | chute de 0,32 s |
| `Fireburst` | 7 disques empilés qui s'effilent selon `1 − 0.72v` | le feu générique |
| `Ring` | deux anneaux de 14 points qui s'étendent, espacés de `0.7 s` | y écrasé à 0,7 |
| `Spikes` | 3 triangles par point qui poussent en `ease_out` | visés sur les **sols** des emplacements, pas les centres |

Chaque style dure `1.05 s`, sauf `Stones` à `1.5 s` (`fx_core.ml:272`), et
`power` règle la taille de la plupart d'entre eux — les exceptions sont
`Sprout`, `Eyes`, `Strike` et `Ice`, qui ont une taille fixe (donc
`~power:1.1` sur une entrée Ice de la table n'a aucun effet).

`rock` mérite une ligne parce que c'est la seule primitive ombrée du code :
`6 + (seed mod 3)` sommets avec jitter, et chaque facette est ombrée par
`0.85 + 0.6 · (−mx − my)/(1.4 · rad)` borné à `[0.35, 1.45]` — une fausse
lumière venant du haut à gauche, ce qui évite qu'un tas de triangles se lise
comme une sphère.

### 5.2 Particules

`emitter_cfg` (`fx_particles.ml:15-32`) est un enregistrement déclaratif :
forme d'apparition, plage de vitesse, plage d'angle en degrés, gravité, plage de
vent, plage de taille, arrêts du dégradé de couleur et d'alpha, texture, débit
et plafond. Six émetteurs sont définis de cette façon (`faerie_fire`,
`acid_rain`, `nether_grasp`, `redemption`, `blood_burst`, et `fire_wall` comme
une recoloration de `faerie_fire`), et `nature_heal` / `pollen_*` / `smoke_cfg`
sont des mises à jour d'enregistrement les uns des autres.

L'intégrateur (`tick_emitter`, `fx_particles.ml:116-130`) est de l'Euler
explicite simple avec un accumulateur de débit :

```ocaml
let dt = if e.last = 0. then 0.016 else Float.max 0. (age -. e.last) in
e.emit <- e.emit +. (dt *. e.cfg.rate);
while e.emit >= 1. && List.length e.puffs < e.cfg.max_particles do ...
p.vy <- p.vy -. (e.cfg.gravity *. dt);
p.px <- p.px +. ((p.vx +. e.wind) *. dt);
p.py <- p.py +. (p.vy *. dt)
```

Accumuler `rate · dt` plutôt que faire apparaître à chaque frame rend le nombre
de particules indépendant de la fréquence d'images — à 250/s, la pluie fait
apparaître ~4 particules par frame à 60 fps et ~8 par frame à 30 fps, et l'image
est identique. La gravité est appliquée *avant* la mise à jour de la position, ce
qui est de l'Euler semi-implicite et donc stable pour le grand `dt` d'une frame
bloquée.

La couleur et l'alpha viennent d'arrêts linéaires par morceaux sur
`u = (age − born)/life` ; avec une texture, la bouffée est dessinée avec
`draw_texture_pro`, sans texture ce sont deux cercles additifs (un halo à 1,35×
avec 0,28 d'alpha et le cœur à 0,9 d'alpha) — le même appariement halo/cœur que
le cœur de l'éclair (§2.6), qui est la grammaire visuelle de ce code : **un
petit centre brillant à l'intérieur d'un halo plus grand et plus faible**.

La durée de vie de l'émetteur est `min 3. (duration + max life)`, afin que la
dernière particule ait toujours le temps de mourir.

### 5.3 Scènes dédiées et shaders

`fx_dedicated.ml` contient vingt-deux effets qui n'entrent pas dans une table de
looks — `spawn_poison` (une tige de Bézier, une floraison à six pétales, de la
fumée qui voyage), `spawn_scythe`, `spawn_aurora`, `spawn_apocalypse`,
`spawn_burial`, `spawn_heal_haze` et ainsi de suite. Ils partagent le même
vocabulaire `mk`/`additive`/`fade` et le même vocabulaire de layout ; il n'y a
rien de structurellement nouveau en eux, ce qui est précisément l'intérêt
d'avoir gardé le vocabulaire réduit.

Quatre shaders GLSL sont chargés depuis `assets/shaders/`
(`fx_core.ml:147-183`) : `grey` (un post-traitement de désaturation pour le
menu), `night` (mélange `u_night` pour la campagne), `sel` (l'anneau animé de
sélection de carte) et `shockwave`. Chaque effet qui en utilise un **se rabat
sur une version à primitives seules si le shader n'a pas pu être chargé**, ce
qui explique que `spawn_shockwave` ait deux implémentations. Les emplacements
des uniforms sont mis en cache par shader dans une table de hachage, et un
uniform manquant est ignoré plutôt que fatal — un shader peut être recompilé
par un pilote et perdre ses noms.

## 6. Comment un sort obtient son effet

`fx_stage.ml` est la table, et c'est le fichier à modifier quand un sort a
besoin de quelque chose que la table des looks ne peut pas exprimer.

`for_spell layout (c : command)` exécute un `match c.cmd_card.key` sur 28 clés
sur mesure, chacune renvoyant
`Some (recipe ~duration ~blocking (fun ~start -> …))`, et se rabat sur la
table des looks :

```ocaml
| _ -> Option.map (look_recipe layout c) (Spell_looks.get c.cmd_card.key)
```

L'ordre de résolution est donc : **sur mesure → table des looks → rien**. Une
carte sans entrée nulle part renvoie `None` et se joue en silence, ce qui est le
comportement par défaut correct pour une invocation de créature.

`layout` (`fx_core.ml:12-25`, rempli par `gui_board.ml:475-540`) est l'unique
pont entre la géométrie du plateau et les effets. Il expose les centres des
emplacements, les **sols** des emplacements (le bas du puits de l'emplacement,
où le feu et les pointes ont leur place), le `flame_base` (38 px au-dessus du
bas de la carte), le portrait de vie, les positions des en-têtes de maison, les
cartes d'une maison en main, et les requêtes dont un effet a besoin (`occupied`,
`slot_life`, `slot_card`, `scarecrow`). Les effets ne calculent jamais
eux-mêmes la géométrie du plateau ; si vous avez besoin d'une nouvelle ancre,
ajoutez-la ici.

Trois réécritures propres à des sorts valent d'être connues, parce que ce sont
des cas où l'effet choisit de *contredire* la visée de la carte :

- **`disciples.wind`** vise les voisins de l'épouvantail plutôt que toute la
  rangée ennemie (`oppress_foe`, `fx_stage.ml:17-22`).
- **Les looks `Spikes`** sont redirigés vers les **sols** ennemis et `Stones`
  vers les deux sols (`fx_stage.ml:34-43`), parce qu'un rocher tombe sur le sol
  et qu'une pointe en sort.
- **`elementalist.incineration`** calcule ses propres tableaux `own`/`foe` en
  choisissant la créature alliée la plus faible et la créature ennemie la plus
  forte, et les transmet au look générique `Fireburst` (`fx_stage.ml:147-174`).

`for_ability layout ~kind ~player ~num ~target` applique la même idée aux
capacités des cartes, indexée sur une chaîne `kind` libre, et — notable — deux
de ses cinq cas sont non bloquants.

### 6.1 Un sort dont la cible n'est connue qu'à la résolution

La plupart des sorts nomment leur cible avant d'être joués : le joueur choisit
un emplacement, et l'effet y est visé. Un sort joué aux dés, non. La
**Soin Instable** du Prêtre du Chaos choisit au hasard l'une des créatures du
joueur *au moment où la carte se résout* : le front ne peut donc pas savoir, au
moment du jet, sur quelle créature la guérison va tomber.

Un tel sort ne doit pas utiliser un look `Target`. `target_spot`
(`fx_stage.ml:27`) lit `c.cmd_input`, qui vaut `None` pour un sort sans entrée,
et `points_of` (`fx_core.ml:673`) se rabat alors sur `foe.(2)` — l'emplacement
du milieu **de l'adversaire** :

```ocaml
| Spell_looks.Target -> (
    match target with Some p -> [| p |] | None -> if Array.length foe > 2 then [| foe.(2) |] else own)
```

Les motes vertes se lèvent donc sur une créature adverse pendant que la
créature du joueur est soignée en silence. La table des looks ne peut pas
exprimer cela : c'est exactement à cela que sert `custom`.

Le sort annonce la créature à la place. L'effet de maison choisit l'emplacement,
le soigne et émet un `Ev_ability` qui le nomme
(`lib/houses/special/chaospriest.ml`) :

```ocaml
| Some st when st.life > 0 && st.life < st.max_life ->
    focus_slot s;
    Slot.heal s (st.max_life - st.life);
    emit env.env_updater
      (Ev_ability
         {
           kind = "chaospriest.unstablehealing";
           player = env.env_player;
           num = s.num;
           target = Some (env.env_player, s.num);
         })
```

et le plateau le dessine là (`gui/fx_stage.ml`) :

```ocaml
| "chaospriest.unstablehealing" ->
    Option.map
      (fun (tp, tn) ->
        recipe ~sound:"naturescare" ~duration:nature_care_duration ~blocking:true (fun ~start ->
            spawn_nature_care [ layout.slot_center tp tn ] ~start))
      target
```

Deux conséquences découlent de cette forme. La clé passe de `looks` à `custom`
dans `lib/game/spell_looks.ml`, car le sort n'a plus aucun effet au moment où la
carte est jouée : la carte est dessinée quand l'effet se résout, pas quand elle
est jouée. Et l'annonce est conditionnelle — l'événement n'est émis que si une
créature a réellement été soignée, pour qu'un sort qui ne trouve rien à soigner
reste muet au lieu de clignoter sur une créature inchangée.

Une capacité est l'endroit où un effet reçoit un **son qui lui est propre**. Le
clip d'un sort vient du nom anglais de la carte (`gui/gui_draw.ml:266`), mais
une capacité part d'une créature et n'a pas de carte à nommer : c'est donc la
recipe qui porte le nom du clip, et le plateau le joue quand l'effet démarre :

```ocaml
| "meteomancer.grenadier" ->
    Option.map
      (fun (tp, tn) ->
        recipe ~sound:"scatterbomb" ~duration:grenade_duration ~blocking:true (fun ~start ->
            spawn_grenade ~src:(layout.slot_center player num) ~target:(layout.slot_center tp tn)
              ~pid:tp ~num:tn ~start))
      target
```

Le nom est un fichier dans `assets/sounds/`, donc `~sound:"scatterbomb"` est
`assets/sounds/scatterbomb.mp3` — le clip du Scatterbomb du Constructeur de
Mécanismes, réutilisé ici parce qu'une grenade qui tombe et une bombe qui
tombe font le même bruit.

## 7. Écrire un nouvel effet

Un effet complet fait environ dix lignes. Pour faire un sort appelé `fire.tide`
qui brûle l'emplacement occupé le plus à gauche de l'ennemi :

```ocaml
(* gui/fx_flames.ml, or a new file opened from gui/dune *)
let tide_duration = 1.1

let spawn_fire_tide ~at ~start =
  mk ~start ~duration:tide_duration (fun ~age ->
      let fade = Float.min 1. ((tide_duration -. age) /. 0.2) in
      if fade > 0.02 then
        additive (fun () ->
            let env = envelope age 0.1 0.5 0.3 in
            flame_column ~x:at.x ~y:at.y ~height:(90. *. env) ~fat:1.2 ~hot:1.
              ~alpha:(fade *. env) ~t:age ~id:1 ~licks:4 ~push:0.))
```

et le branchement, dans `fx_stage.ml` :

```ocaml
| "fire.tide" ->
    Option.map (fun n -> recipe ~duration:tide_duration ~blocking:true (fun ~start ->
        spawn_fire_tide ~at:(layout.slot_floor other n) ~start)) c.cmd_input
```

Ajoutez ensuite `"tide"` à `demo_names` et une ligne à `spawn_demo`, afin de
pouvoir l'inspecter sans lancer une partie.

Liste de contrôle, dans l'ordre qui attrape réellement les erreurs :

- [ ] **Prendre chaque position dans `layout`.** `slot_center` pour un éclair,
      `slot_floor` pour les effets au sol, `life` pour tout ce qui vise un
      joueur, `source_of` pour « depuis la carte que je viens de jouer ».
- [ ] **Utiliser `fade` pour la queue et un `born` par élément pour la tête.** Un
      seul `fade` sur l'ensemble est plat ; ce sont les couches qui font
      l'effet.
- [ ] **`additive` pour tout ce qui est de la lumière, et seulement pour cela.**
      `Stones`, `Sprout`, `Eyes` et `Spikes` sont des objets opaques.
- [ ] **Donner à l'effet une constante `duration` et l'utiliser à la fois pour la
      timeline et pour le fade.** Pas de littéraux à deux endroits.
- [ ] **Régler `blocking` honnêtement.** `true` si et seulement si l'image doit
      retarder l'animation suivante. Les effets bloquants s'empilent : trois
      sorts bloquants dans un même tour, ce sont quatre secondes sans rien qui
      se passe.
- [ ] **L'ajouter à `demo_names`.** Appuyer sur **F8** dans un duel fait défiler
      tous les effets de démo (`bin/gui.ml:317-320`) — cela marche dans
      n'importe quel état de jeu, ce qui est le seul moyen pratique de vérifier
      les timings.
- [ ] **Nommer un clip si l'effet fait du bruit.** `~sound:"<fichier>"` sur la
      recipe, pour une capacité (le son d'un sort vient du nom de la carte).
      Le clip démarre avec l'animation, donc sa propre prise d'élan est le délai.
- [ ] **Vérifier que la cible est connue au moment du dessin.** Un look
      `Target` vise `c.cmd_input`, et un sort sans entrée se rabat sur
      l'emplacement du milieu de l'*adversaire* (§6.1). Si la carte tire sa
      cible aux dés, que la maison émet un `Ev_ability` qui la nomme, et
      dessine d'après celui-ci.

Deux règles de performance bon marché tirées du code existant : dériver tout
nombre de pas d'une longueur (`steps = clamp(6, h/7, 14)`) plutôt que de coder
en dur une borne de boucle, et laisser le culling sous-pixel des primitives
faire le reste.

## 8. Pièges

- **Deux horloges sont une fonctionnalité, pas un bug.** Le scintillement des
  éclairs comme la colonne de flammes utilisent un balancement lent plus un
  scintillement rapide à des fréquences sans rapport. Une fréquence unique se
  lit comme une boucle.
- **`noise` sert aux variations stables, `Random.State` aux changements par
  frame.** Hacher l'indice de frame fait du bruit ; utiliser un RNG pour une
  propriété par colonne fait de la neige.
- **Les passes additives s'additionnent, donc surveillez l'arithmétique de
  l'alpha.** Plusieurs endroits passent volontairement un alpha supérieur à 1 (le
  cœur des flammes, `paint … 1.2 1.`) ; `byte` le borne, ce qui est voulu.
- **`draw_all` est appelé après les cartes** (`bin/gui.ml:398`) pour que les
  éclairs les recouvrent. Si un nouvel effet semble être *sous* les cartes, il
  est dessiné au mauvais endroit, pas avec le mauvais mode de blending.
- **La fréquence de secousse peut dépasser la fréquence d'images** (jusqu'à 80 Hz
  à `strength = 1`, bien que le §4.5 montre que le pic visible est d'environ
  60 Hz). N'augmentez pas `amp` en espérant un grondement plus fort ; vous
  obtiendrez de l'aliasing.
- **La `duration` d'un effet bloquant est une promesse.** Se tromper, et le jeu
  attend sur une frame invisible.
- **`Fx.clear` existe parce que deux effets ont une durée infinie.** Tout nouvel
  effet qui n'expire jamais est un bug de revanche.
- **Mise en garde sur le saut le plus long :** `gens` sature à 6, donc au-delà de
  512 px de corde la génération la plus fine n'ajoute que des sommets et l'éclair
  paraît plus calme que prévu par la conception (§2.3).
- **La table des looks est de la donnée, pas du code.** `lib/game/spell_looks.ml`
  est le moyen le moins coûteux de donner à un sort une animation correcte ;
  n'écrivez du code sur mesure que lorsque la carte a besoin de quelque chose que
  la table ne peut pas exprimer, et consultez le §6 pour les trois cas où cette
  distinction est déjà floue.

## Pour aller plus loin

- La chorégraphie des sorts, effet par effet, face à l'original Scala :
  [plan de portage des effets](../tech/effects-port-plan.md).
- Les événements du moteur qui déclenchent tout cela (`Ev_spell_played`,
  `Ev_ability`, `Ev_focus`) : [document 1](01-architecture.md).
- La disposition du plateau à laquelle les effets sont ancrés :
  [document 2](02-game-view.md) pour l'état, `gui_board.ml` pour la géométrie.
- Et retour aux bots : [document 3](03-search-concepts.md).