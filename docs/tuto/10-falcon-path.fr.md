# 10. L'oiseau du Fauconnier : les mathématiques d’un chemin courbe

`spawn_falcon` (`gui/fx_dedicated.ml`) est la capacité du **Fauconnier** des
Disciples. L’oiseau quitte la pierre du fauconnier, décrit une large boucle
pour prendre son virage, traverse toute la rangée adverse en piquant sur chaque
créature qu’il croise, puis sort du plateau. Il n’y a pas de retour.

Le document 7 §5.3 range cette fonction parmi les dix-sept
« chorégraphies » sur mesure. Ce document l’analyse en détail, car c’est un
petit exemple complet d’une chose dont on a toujours besoin : **faire croire
qu’un objet qui bouge a un poids** — un seul chemin, aucun moteur physique,
juste une courbe et une horloge.

La fonction fait environ 120 lignes et répond à quatre questions :

1. **Où est l’oiseau ?** Un chemin composé d’une courbe cubique et d’un cercle (§3, §4).
2. **Le chemin a-t-il des cassures ?** Non — les morceaux se rejoignent
   de manière tangente l’un à l’autre (§4).
3. **À quelle vitesse va-t-il ?** Pas à distance constante : il ralentit dans
   le virage et accélère sur la ligne droite (§6).
4. **Quand passe-t-il au-dessus d’une carte ?** Les anneaux de piqué sont
   temporisés en inversant la course rectiligne (§7).

## En une minute

- Le chemin est dessiné dans un **repère local** où le fauconnier est toujours
  sous la rangée adverse ; un seul changement de signe (`sy`) le reflète pour
  un lanceur de l’autre côté (§2).
- Le virage est un **cercle de rayon 85**, suspendu sous l’emplacement adverse le plus à gauche,
  balayé sur **un angle de 270°** pour que l’oiseau arrive vers le bas et
  reparte vers la droite (§3).
- L’amenée depuis le fauconnier est une **courbe de Bézier cubique**, dont les
  deux tangentes aux extrémités sont contrôlées ; la longueur de poignée partagée
  assure une jonction lisse avec le cercle (§4).
- L’oiseau **ne se déplace pas** à vitesse constante. Chaque échantillon porte
  un **coût temporel** `ds /. speed`, et `speed` augmente quand le cap cesse de
  tourner — donc un même pas de temps correspond à moins de pixels dans le virage
  (§6).
- Tout le reste en découle : `at_cost` transforme un coût en position,
  `crossing` transforme la coordonnée `x` d’une carte en coût (§7).

### Mots utilisés dans ce document

| Mot | Sens courant |
|---|---|
| **paramètre de chemin** `u` | un nombre entre 0 et 1 qui identifie un point sur une courbe |
| **courbe de Bézier** | courbe lisse passant par un point de départ, un point d’arrivée et deux points de contrôle ; un point de la courbe est une moyenne pondérée des quatre |
| **direction tangente** | direction de la tangente à la courbe en un point ; si deux morceaux partagent cette direction à une jonction, celle-ci est invisible |
| **longueur d’arc** | distance mesurée le long de la courbe (et non en ligne droite) |
| **repère local** | système de coordonnées où un problème est simplifié ; une transformation les ramène à l’écran |
| **coût en temps** | une fausse longueur où un segment lent compte davantage ; des pas égaux de coût prennent le même temps |

## 1. Ce que le plateau fournit, et un exemple

`fx_stage.ml` appelle la fonction dans le cas `"disciples.falconer"`. Il
mesure une fois la rangée adverse et extrait la géométrie brute, sans référence aux objets carte :

```ocaml
let left  = Array.fold_left (fun a s -> Float.min a s.x) row.(0).x row in
let right = Array.fold_left (fun a s -> Float.max a s.x) row.(0).x row in
let foe_span = (right -. left) /. 2. in
```

| Argument | Sens |
|---|---|
| `from_` | le centre de l’emplacement du fauconnier — point de départ de l’oiseau |
| `foe_x`, `foe_y` | le centre de la rangée adverse |
| `foe_span` | la demi-largeur de la rangée, donc `foe_x -. foe_span` est l’emplacement **le plus à gauche** |
| `targets` | les centres des créatures ennemies, qui reçoivent un anneau de piqué |

`foe_span` correspond à la distance entre le centre de la rangée et le centre d’un emplacement.
L’oiseau entre toujours dans la rangée par ce premier **emplacement**, et non par la première créature occupée.

Les nombres ci-dessous utilisent la vraie disposition du plateau. Les cartes
font `90×102` avec un écart de `10`, donc une rangée de 5 emplacements a des
centres espacés de 100. Le bord gauche de la rangée est à
`832 − 6 − 490 = 336` (`vw = 832`, `pane_margin = 6`,
`gui_board.ml:396-408`), et les centres de rangée tombent à
`66 + 102/2 = 117` (adversaire) et `224 + 102/2 = 275` (joueur).

| Grandeur | Valeur |
|---|---|
| fauconnier `from_` | `(581, 275)` — l’emplacement du milieu côté joueur |
| rangée adverse | `foe_x = 581`, `foe_y = 117` |
| `foe_span` | `200` (cinq emplacements) |
| `entry = foe_x -. foe_span` | `(381, 117)` |
| `exit_x = world_w +. 70.` | `902` |
| `world_w` | `832` (`fx_core.ml:43`) |

## 2. Un repère local, pour qu’une seule géométrie serve les deux côtés

Le fauconnier est soit du côté joueur (sous la rangée), soit du côté
adversaire (au-dessus). Plutôt que de définir deux chemins, le code en reflète
un seul :

```ocaml
let sy = if home.y >= foe_y then 1. else -1. in
let hx = home.x -. entry.x in
let hy = (home.y -. foe_y) *. sy in
let to_world x y = (entry.x +. x, foe_y +. (sy *. y)) in
```

Dans ce repère, **l’entrée est l’origine**, et l’axe `y` croît **vers le fauconnier**.
Pour le joueur, `sy = +1.` et le fauconnier est en dessous ; pour l’adversaire,
`sy = -1.` et il est au-dessus. La boucle est dessinée une fois et reste toujours
du côté du lanceur par rapport à la rangée, ce qui correspond au rendu souhaité depuis la
place du joueur.

Nombres de l’exemple (`sy = +1.`) :

| Monde | Local |
|---|---|
| `from_ (581, 275)` | `home (200, 158)` |
| `entry (381, 117)` | `(0, 0)` |

À partir d’ici, tout est en coordonnées locales. `to_world` ramène un point local à l’écran,
et cette fonction est appliquée à chaque échantillon avant le dessin.

## 3. Le virage : un cercle, et 270° de celui-ci

Le virage est un cercle de rayon `r = 85.`, dont le centre est situé un rayon **sous l’entrée** :

```ocaml
let r = 85. in
let t = { x = r; y = r } in
let loop u =
  let ph = 1.5 *. Float.pi *. u in
  to_world (r *. Float.cos ph) (r +. (r *. Float.sin ph))
in
```

Un point d’angle `ph` a pour coordonnées `(r *. Float.cos ph, r +. r *. Float.sin ph)` :
le centre `(0, r)` auquel on ajoute `r` dans la direction `ph`.

| `ph` | Point local | Point monde | Description |
|---|---|---|---|
| `0.` | `(r, r)` | `(466, 202)` | à droite du centre — **début de la boucle** |
| `Float.pi /. 2.` | `(0., 2. *. r)` | `(381, 287)` | le bas, sous l’entrée |
| `Float.pi` | `(-. r, r)` | `(296, 202)` | à gauche du centre |
| `3. *. Float.pi /. 2.` | `(0., 0.)` | `(381, 117)` | **l’entrée** |

`loop u` parcourt `ph = 1.5 *. Float.pi *. u`, de `0.` à `3. *. Float.pi /. 2.` —
soit un balayage de **270°**. En posant `t = (r, r)`, l’oiseau arrive au point le plus à droite
du cercle et repart vers l’entrée.

La tangente à un point du cercle est `P′(ph) = r *. (-. Float.sin ph, Float.cos ph)`, donc :

- au départ (`ph = 0.`), la tangente est `(0., r)` — **tout droit vers le bas** dans le
  repère local, en s’éloignant de la rangée et vers le côté du lanceur ;
- à l’entrée (`ph = 3. *. Float.pi /. 2.`), la tangente est `(r, 0.)` — **tout droit vers la droite** (`+x`),
  exactement la direction de la course rectiligne qui suit.

Ce dernier point explique le choix des 270° : l’oiseau quitte la boucle **déjà**
orienté le long de la course, donc le raccord avec la ligne droite est invisible.
Un demi-cercle (180°) quitterait également l’entrée vers `+x`, mais il devrait partir
du **bas** de la boucle en allant vers la gauche, et l’amenée depuis le fauconnier
arriverait en allant vers la gauche — une transition moins fluide. 
Le quart supplémentaire offre à l’oiseau un plongeon sous la rangée et une arrivée vers le bas,
ce qui donne au virage l’apparence d’un **infléchissement** plutôt que d’un déclic.

## 4. L’amenée : un cubique à deux tangentes contrôlées

L’oiseau doit aller du fauconnier au point `t = (r, r)` **en arrivant tangent au cercle**.
Une courbe de Bézier cubique offre précisément ce contrôle :

```
B(u) = (1-u)^3 P0 + 3(1-u)^2 u P1 + 3(1-u) u^2 P2 + u^3 P3.
```

Dans le code, il s’agit de la fonction auxiliaire `bezier` (`fx_dedicated.ml:130`) :

```ocaml
let bezier a b c d u =
  let o = 1. -. u in
  (o *. o *. o *. a) +. (3. *. o *. o *. u *. b) +. (3. *. o *. u *. u *. c) +. (u *. u *. u *. d)
```

appelée une fois par axe. Les extrémités sont fixées : `P0 = home` et `P3 = t`.
Les deux points de contrôle intérieurs **déterminent** les deux tangentes aux extrémités,
car

```
B'(0) = 3(P1 - P0),   B'(1) = 3(P3 - P2).
```

Le code définit les tangentes, puis **utilise** une seule longueur de poignée partagée :

```ocaml
let l = Float.max 1e-3 (Float.hypot (t.x -. hx) (t.y -. hy)) in
let sd = Float.hypot hx hy in
let sdx, sdy = if sd <= 0. then (0., -1.) else (-. hx /. sd, -. hy /. sd) in
let p1 = { x = hx +. (sdx *. l /. 3.); y = hy +. (sdy *. l /. 3.) } in
let p2 = { x = t.x; y = t.y -. (l /. 3.) } in
```

- **Départ :** `(sdx, sdy)` est la direction unitaire du fauconnier vers
  **l’entrée** (`(-. hx, -. hy) /. |home|`), et la poignée mesure `l /. 3.` :
  `P1 = home + (l /. 3.) * dir`. L’oiseau part donc en visant l’emplacement.
- **Arrivée :** la tangente du cercle en `t` est `(0., r)` — **tout droit vers le bas** —
  et la poignée mesure encore `l /. 3.` :
  `P2 = t - (l /. 3.) * (0., 1.) = (r, r - l /. 3.)`. Il arrive donc en allant tout droit
  vers le bas, ce que le cercle attend.
- `l = |home - t|` correspond à la **corde** entre les extrémités. 
  Utiliser la corde comme échelle pour la poignée, plutôt que les deux vraies distances tangentes,
  est un choix économique (inspiré des splines d’Hermite) : cette approche est stable pour toute entrée
  et évite que la courbe ne se replie sur elle-même.

La condition `if sd <= 0.` gère le cas dégénéré où le fauconnier est pile au-dessus de
l’entrée — une approche verticale dont la composante en `x` **entraînerait une division par zéro**.

Nombres de l’exemple (en coordonnées locales) : 
`home (200, 158)`, `t (85, 85)`, `l = 136,2`,
`P1 = (164,4, 129,9)`, `P2 = (85, 39,6)`. 
En coordonnées monde, le polygone de contrôle parcourt 
`P0 (581, 275) → P1 (545, 247) → P2 (466, 157) → P3 (466, 202)` ; 
le cercle prend alors le relais en `P3`.

## 5. Échantillonnage : trois segments, un tableau

Les trois segments sont échantillonnés à pas fixe, puis **assemblés** dans un seul
tableau `pts` :

```ocaml
let lead_steps = 64 and loop_steps = 80 and run_steps = 64 in
let curve_steps = lead_steps + loop_steps in
let n = curve_steps + run_steps in
let pts = Array.init (n + 1) (fun i -> ... ) in
```

Avec `lead_steps = 64`, `loop_steps = 80` et `run_steps = 64`, on obtient `n = 208` et donc **209 points**.
La course rectiligne est une ligne uniforme en `x` :

```ocaml
let u = float (i - curve_steps) /. float run_steps in
(entry.x +. ((exit_x -. entry.x) *. u), foe_y)
```

ce qui permettra plus loin de **convertir directement la coordonnée `x` d’une carte en temps** (§7).
`loop_steps = 80` donne au cercle des segments de `1.5 *. Float.pi /. 80. ≈ 0,0589 rad` — 
ce n’est pas un hasard : c’est ce nombre qui fait du cercle **la zone la plus lente** (§6).

## 6. La temporisation : un coût temporel constant, pas une distance constante

Si l’oiseau parcourait une distance fixe par image, le cercle **défilait trop vite**,
tandis que la ligne droite **semble trop lente**. À la place, chaque échantillon **a un coût temporel** :

```ocaml
let wrap a =
  let a = Float.rem a (2. *. Float.pi) in
  if a > Float.pi then a -. (2. *. Float.pi)
  else if a < -. Float.pi then a +. (2. *. Float.pi)
  else a
in
let cum = Array.make (n + 1) 0. in
let prev = ref 0. and first = ref true in
for i = 1 to n do
  let (x0, y0) = pts.(i - 1) and (x1, y1) = pts.(i) in
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let ds = Float.hypot dx dy in
  let ang = Float.atan2 dy dx in
  let turn = if !first then 0. else Float.abs (wrap (ang -. !prev)) in
  prev := ang;
  first := false;
  let speed = 1. +. (0.9 *. (1. -. Float.min 1. (turn /. 0.06))) in
  cum.(i) <- cum.(i - 1) +. (ds /. speed)
done;
```

Lisez-le en trois étapes :

1. **De combien le cap a-t-il tourné ?** `ang` est la direction du segment ;
   `turn = Float.abs (wrap (ang -. !prev))` représente l’angle de rotation de l’oiseau,
   replié dans l’intervalle `(-. Float.pi, Float.pi]` pour éviter qu’un dépassement de `π`
   ne soit interprété comme un virage énorme.
2. **Transformer cette rotation en vitesse.** 
   `speed = 1. +. (0.9 *. (1. -. Float.min 1. (turn /. 0.06)))`.
   Une ligne droite (`turn = 0.`) **se déplace à une vitesse de `1,9`** ; 
   tout segment dont la rotation dépasse `0,06 rad` (≈ 3,4°) **se déplace à une vitesse de `1,0`**.
   `0,06` est **l’échelle de virage**.
3. **Calculer le coût temporel.** `cum.(i) <- cum.(i - 1) +. (ds /. speed)`. 
   Un segment court et lent **peut coûter autant qu’un segment long et rapide**.

Comme les segments sont courts, `turn ≈ courbure * ds`, donc `speed` **agit en réalité comme un détecteur de courbure** :
les endroits très courbés sont lents. La ligne droite a `turn = 0.` avec `ds = 8,1 px` ; 
le cercle tourne de `0,0589 rad` tous les `5,0 px` — juste sous le seuil `0,06`, 
donc toute la boucle reste à ≈ `1,0` pendant que la ligne droite **se déplace à `1,9`**.

`cum` est désormais une **fausse longueur** monotone de `0` à `total = cum.(n)`. 
Deux fonctions auxiliaires finalisent le travail :

```ocaml
let at_cost s =
  ... while !i < n && cum.(!i + 1) < s do incr i done;
  (* puis interpolation linéaire entre pts.(!i) et pts.(!i+1) *)
```

**trouve le point correspondant à un coût donné**, et

```ocaml
let at age =
  let s = total *. Float.max 0. (Float.min 1. (age /. falcon_duration)) in
  at_cost s
in
```

**associe une durée écoulée à une position**. 
L’oiseau parcourt la totalité du coût `total` en exactement `falcon_duration` secondes, 
donc des intervalles de temps égaux correspondent à des intervalles de **coût** égaux — 
d’où peu de pixels là où tourner coûte cher.

Nombres de l’exemple, avec `falcon_duration = 1,0 s` :

| Segment | Longueur | Coût | Part du temps |
|---|---|---|---|
| cubique d’amenée | 155 px | 98 | 13 % |
| cercle | 402 px | 392 | 51 % |
| course rectiligne | 521 px | 275 | 36 % |
| **Total** | **1 078 px** | **765** | **1,0 s** |

La ligne droite est le segment le plus long (48 % de la distance), mais **elle coûte le moins de temps (36 %)**,
car ses segments **se déplacent à `1,9`**, alors que toute la boucle est à `1,0`. 
En moyenne, l’oiseau parcourt ≈ 870 px/s dans le virage et ≈ 1 450 px/s sur la ligne droite — 
un rapport inférieur à `1,9` par segment, car la vitesse du cubique d’amenée est progressive.

## 7. Les anneaux de piqué : inverser la course rectiligne

Chaque créature ennemie **reçoit un anneau** au moment où l’oiseau **passe au-dessus d’elle**. 
L’oiseau **n’est au-dessus d’un emplacement que pendant la course rectiligne**, 
et cette course est uniforme en `x`. Ainsi, **l’indice de l’échantillon correspondant à une carte 
est linéaire en fonction de sa coordonnée `x`** :

```ocaml
let crossing (t : spot) =                  (calcule le coût temporel normalisé pour une carte donnée)
  if t.x < entry.x -. 1. || t.x > exit_x then None
  else
    let k =
      float curve_steps +. ((t.x -. entry.x) /. (exit_x -. entry.x) *. float run_steps)
    in
    let i = int_of_float (Float.floor k) in
    let f = k -. float i in
    let c = if i >= n then cum.(n) else cum.(i) +. (f *. (cum.(i + 1) -. cum.(i))) in
    Some (c /. total)
in
```

`k` **représente** l’indice d’échantillon fractionnaire, `c` **interpole** le coût correspondant 
depuis le tableau `cum`, et le résultat est **une fraction du temps total**, `c / total`. 
La boucle de dessin **convertit cette fraction en un instant** (`strike`) :

```ocaml
let strike = p *. falcon_duration in   (âge auquel l’oiseau passe au-dessus de la carte)
let window =
  Float.min 0.42 (Float.max 0.14 (falcon_duration -. strike -. 0.02))
in
```

`strike` est l'âge auquel l'oiseau est au-dessus de cet emplacement. `window`
représente la durée de vie de l’anneau, bornée pour qu'un anneau proche de la fin
reste visible : `falcon_duration − strike − 0.02` représente le temps restant avant
la fin de l’animation (moins une petite marge), avec un minimum de 0,14 s
pour rester visible et et un maximum de 0,42 s pour ne pas s'attarder.
Dans l’exemple, les cinq emplacements adverses reçoivent un anneau aux
instants 0,64, 0,71, 0,78, 0,85 et 0,92 s — cet espacement est dû à la vitesse
constante de la course rectiligne.

Si une cible n’est pas située sur la course — par exemple, une carte imaginaire
située à gauche de l’entrée ou au-delà de la sortie — `crossing` renvoie `None` et aucun anneau
n'est dessiné. La marge `entry.x − 1` permet de gérer les arrondis pour
le premier emplacement.

## 8. La traînée, le fade et l'oiseau

L’oiseau est dessiné à la position at age, et son orientation est calculée
à partir d’un point légèrement plus loin :

```ocaml
let (px, py) = at age in                  (position actuelle de l’oiseau)
let (qx, qy) = at (age +. 0.03) in        (position légèrement en avant)
let ang = atan2 (qy -. py) (qx -. px) in  (angle entre les deux points)
```

ainsi, le sprite est toujours orienté dans la direction du mouvement. Derrière lui,
cinq cercles fantômes représentent des positions passées, `at (age − k·0.032)` pour
`k = 1…5`, chacun étant plus petit et plus transparent" — une traînée de mouvement
calculée à partir de la même fonction de chemin.

L’ensemble de l’effet s’estompe pendant les 0,18 s finales :

```ocaml
let fade = Float.min 1. ((falcon_duration -. age) /. 0.18) in
if fade > 0.02 then ( ... )
```

et le battement d’ailes est simplement `sin (age *. 19.)` — soit environ 3 Hz — passé à
la fonction `draw_falcon`, qui utilise cette valeur pour animer les ailes (élargissement et variation de la transparence).

## 9. Les réglages

| Paramètre | emplacement | effet |
|---|---|---|
| `falcon_duration` | en haut du bloc du faucon | durée totale de l'animation (1,0 s) |
| `r = 85.` | `spawn_falcon` | une valeur plus grande donne un virage plus large et plus long |
| `1.5 *. Float.pi` | `loop` |  angle de balayage ; une valeur de 1.0 donnerait un demi-cercle (180°) |
| `turn /. 0.06` | temporisation | échelle de virage : une valeur plus grande réduit le nombre de segments considérés comme des virages |
| `0.9` dans `speed` | temporisation | facteur de vitesse ; détermine de combien une ligne droite est plus rapide qu’un virage |
| `lead_steps`, `loop_steps`, `run_steps` | échantillonnage |  nombre de segments pour chaque partie du chemin |
| `world_w +. 70.` | `exit_x` | distance au-delà du bord du plateau où l’oiseau sort |
| `0.42` / `0.14` | `window` des anneaux |  durée maximale / minimale d’un anneau de piqué |
| `0.18` | `fade` |  durée du fondu final |

Deux remarques importantes. Modifier loop_steps modifie l’angle de virage par segment du
cercle `1.5π/loop_steps` : il vaut `0.0589` par défaut, donc toute la boucle est perçue 
comme un seul arc lent ; si vous l’échantillonnez plus finement la proportion de temps
consacrée à la boucle commence à différer de celle de la ligne droite.
De plus, falcon_duration correspond également à la durée bloquante de la `recette`
(`fx_stage.ml`), donc l'animation suivante attendra exactement ce temps : si vous réduisez la durée du chemin,
vous devez également réduire la constante falcon_duration, sinon, le
tour restera bloqué à la fin.

## Pour aller plus loin

- Voir le document 7 §5.3 pour les autres fonctions de `fx_dedicated.ml` ainsi que le vocabulaire
  partagé.
- Voir le document 7 §1 pour une explication de la timeline et la signification de `mk ~start ~duration`.
- Consulter `gui/fx_core.ml` pour la définition de `spot`, la version adaptée pour `spot` de la fonction `bezier` à la fin du fichier, `circle`, `ring`, `additive` et la transformation.
