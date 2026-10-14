# 8. L'océan du lobby

Le lobby peut jouer une plage sans fin, générée procéduralement : vagues, vent
qui souffle en rafales, aucun fichier audio nulle part. Tout tient dans un
fichier, `gui/ocean.ml`, environ 150 lignes, portage d'un petit jouet de
navigateur (le « beach wave loop generator »). Ce document le parcourt dans
l'ordre où circule le signal : bruit → mise en forme → filtre → tampon → flux.

Pour l'activer : **Réglages → Son → Ambiance de l'océan dans le hall**. C'est
désactivé par défaut (`sound.ocean` dans le fichier de préférences,
`Prefs.ocean_sound`), et le son ne joue que lorsque le lobby est à l'écran.

## 1. L'idée centrale : un son est une fonction du temps

Il n'y a aucun échantillon à mettre en boucle. Chaque échantillon de sortie est
calculé à partir d'un seul nombre, le temps `t` en secondes :

```text
sample(t) = lowpass( waves(t) + wind(t) )
```

et chaque ingrédient de `waves` et `wind` est une fonction lisse de `t`. Le son
peut donc durer indéfiniment sans raccord, et « où en est-on dans la boucle »
se réduit à un compteur entier de frames (`frame`, 44 100 par seconde).

La version navigateur avait aussi un export WAV qui exigeait un fondu enchaîné
pour être sans couture. Nous n'exportons rien : toute cette partie a disparu.

## 2. Premier ingrédient : le bruit de valeurs

Les vagues sont du bruit ; il nous faut donc un bruit identique à chaque
exécution et assez lisse pour ne pas siffler comme de la neige. `make_noise
seed` le construit :

1. Remplir une table de 4096 entrées avec des nombres aléatoires dans [-1, 1]
   issus d'un minuscule générateur congruentiel linéaire
   (`s ← s·1664525 + 1013904223 mod 2³²`). Même graine, même table, à chaque
   exécution.
2. Pour lire le bruit à la position `x = t · 4096`, prendre les deux entrées
   voisines `a` et `b` et les mélanger avec une **smoothstep**,
   `u = f²(3 − 2f)` où `f` est la partie fractionnaire de `x`.

```ocaml
let u = f *. f *. (3. -. (2. *. f)) in
a +. ((b -. a) *. u)
```

La smoothstep a une pente nulle à chaque extrémité : la courbe rejoint ses
voisines sans cassure. Cette interpolation est déjà un doux passe-bas en
soi : le résultat est aléatoire mais arrondi, pas un flot d'échantillons
aléatoires indépendants.

L'indice de la table est pris `land 4095`, donc il **boucle**. Le bruit se
répète ; la vitesse de répétition dépend de la rapidité avec laquelle on
parcourt la table (section suivante).

Chaque canal et chaque couche de vent a sa propre table (graines 12345, 67890,
24680, 13579, 55555, 77777) : gauche et droite ne sont donc pas des copies l'un
de l'autre.

## 3. Deuxième ingrédient : les vagues

```ocaml
let l = o.n1 ((t *. 3.1) +. (sin (t *. 0.02) *. 0.5)) *. env *. surge *. 0.5
```

Trois éléments se multiplient.

**Le bruit, lu vite.** L'argument est `t · 3.1` : on parcourt la table à
3,1 × 4096 ≈ 12 700 entrées par seconde. Cela fait environ 3,5 échantillons de
sortie par entrée de table, ce qui donne un doux chuintement. La table a 4096
entrées, elle boucle donc toutes les 4096 / 12 700 ≈ **0,32 s**. Le
`sin (t · 0.02) · 0.5` supplémentaire fait osciller lentement la position de
lecture, de sorte que la répétition dérive au lieu de se caler sur un battement
fixe. Le canal droit lit à 2,9 au lieu de 3,1 et a sa propre phase, ce qui
garde les deux oreilles décorrélées.

**`envelope t` : la houle.** Trois sinusoïdes lentes de périodes environ
14,1 s (0,071 Hz), 23,3 s (0,043 Hz) et 8,5 s (0,117 Hz). Leurs fréquences ne
sont pas multiples les unes des autres, donc la somme ne se répète jamais à une
échelle utile :

```ocaml
let e = (((a +. b +. c) /. 3.) +. 1.) /. 2. in   (* squash into 0..1 *)
Float.pow e 1.6
```

La puissance 1,6 courbe la fonction pour que les valeurs basses descendent
davantage : de longs creux presque silencieux, des poussées plus courtes et
fortes. Quand `e = 0,5`, l'enveloppe ne vaut que 0,5^1,6 ≈ 0,33.

**`surge` : le va-et-vient.** `0.6 + 0.4·sin(2π · 0.35 · t)` : une oscillation à
0,35 Hz (un cycle toutes les 2,9 s) qui ne descend jamais sous 0,2 ni ne monte
au-dessus de 1. C'est chaque vague qui déferle puis se retire, à l'intérieur de
la houle lente.

Un échantillon calculé à la main, à `t = 0` :

| terme | valeur |
|---|---|
| sin 1,3 ; sin 4,1 ; sin 0,7 | 0,964 ; −0,818 ; 0,644 |
| moyenne des trois | 0,263 |
| ramenée à 0..1 | 0,632 |
| `envelope` = 0,632^1,6 | **0,479** |
| `surge` = 0,6 + 0,4·sin 0 | **0,6** |

le bruit est donc multiplié par 0,479 × 0,6 × 0,5 ≈ 0,14.

## 4. Troisième ingrédient : le vent

`wind_sample lo mid t` mélange deux couches de bruit sous une enveloppe de
rafale :

```text
gust(t)   trois sinusoïdes (19 s, 11 s, 43 s), ramenées à 0..1, puissance 1,3
whoosh    lo  lu à 0,55·t   -> la table boucle toutes les ~1,8 s : un corps grave et lent
whistle   mid lu à 4,7·t    -> boucle toutes les ~0,21 s : un chuintement plus aigu
wind = (0.75·whoosh + 0.25·whistle·gust) · (0.35 + 0.65·gust) · 0.45
```

Deux choses en font du vent plutôt qu'une deuxième piste de vagues :

- le **sifflement** est multiplié par la rafale : l'aigu n'apparaît que
  lorsqu'une rafale se lève ;
- l'**ensemble** est multiplié par `0.35 + 0.65·gust` : même dans un creux, il
  reste une brise discrète, et le vent ne se tait jamais.

Le résultat est multiplié par `wind_level` (0,35). Le canal droit est évalué à
`t + 0.37` : une rafale atteint une oreille 0,37 s après l'autre, d'où la
largeur stéréo.

## 5. Le filtre : sombre quand c'est calme, brillant quand le ressac monte

La somme passe ensuite dans un passe-bas dont la fréquence de coupure suit la
houle :

```ocaml
let k = pole (500. +. (2500. *. env)) in
```

À `env = 0`, la coupure est à 500 Hz (étouffé, lointain) ; à `env = 1`, elle est
à 3 kHz (ressac net). Une vraie vague sonne plus brillante en déferlant, et une
seule ligne suffit à le reproduire.

Le filtre est composé de deux étages **passe-bas à un pôle** en série. Un étage
s'écrit

```ocaml
s ← s + k · (x − s)            avec   k = 1 − exp(−2π · fc / sample_rate)
```

c'est-à-dire « déplacer l'état d'une fraction `k` du chemin vers l'entrée ».
Deux étages en série donnent une atténuation de 12 dB par octave, proche du
biquad Web Audio de l'original. C'est une approximation, pas un filtre
identique.

À `t = 0`, l'enveloppe vaut 0,479 : la coupure est donc 500 + 2500 × 0,479
≈ 1 700 Hz et `k` = 1 − exp(−2π · 1700 / 44100) ≈ 0,215. Aux deux extrêmes,
`k` vaut 0,069 (500 Hz) et 0,348 (3 kHz). Chaque canal garde sa propre paire
d'états (`lp_l`, `lp_r`), et cet état se conserve d'un bloc au suivant. Sans
cela, on entendrait un clic à chaque frontière de bloc.

`k` est recalculé **à chaque échantillon**. Le calculer une fois par bloc (ce
que faisait la première version) fait sauter la coupure toutes les 186 ms, et
ces sauts s'entendent.

## 6. Volume, marge et fondus

```ocaml
let gain = 0.9 *. intensity *. volume          (* 0.9 · 0.8 · sound level *)
let out x = ... x *. gain *. f *. 32767. *. 2.
```

`volume` est le Niveau de l'onglet Son (0..1), `f` le fondu (ci-dessous), et le
`2.` final un gain de compensation. Le pire cas du signal avant filtrage est
0,5 (vagues) + 0,158 (vent : 1,0 · 0,45 · 0,35) = 0,658, et
0,658 × 0,72 × 2 = 0,95 : à plein volume, on reste juste sous la pleine
échelle. L'écrêtage `max`/`min` n'est là que par précaution.

**Fondu.** Entrer dans le lobby avec un son d'un coup à plein volume est
brutal. `fade` se rapproche de sa cible (1 si actif, 0 sinon) de 0,3 par bloc à
la montée et de 0,5 par bloc à la descente, et est **interpolé linéairement
sur chaque bloc** : le gain monte en rampe au lieu de faire des marches. Le
fondu d'entrée dure environ 0,75 s, celui de sortie environ 0,4 s.

**Rafraîchissement du niveau.** Le niveau sonore vit dans le fichier de
préférences. Le relire à chaque bloc serait du gaspillage, donc `update` le
relit un bloc sur cinq (environ une fois par seconde).

## 7. Amener les échantillons jusqu'aux haut-parleurs

La synthèse est la moitié facile. L'autre moitié est l'API de flux de raylib,
qui cache un piège ayant coûté quelques tours de débogage.

```ocaml
R.set_audio_stream_buffer_size_default chunk;
let stream = R.load_audio_stream sample_rate 16 2 in   (* 44.1 kHz, 16 bit, stereo *)
R.play_audio_stream stream
```

Un flux raylib possède **deux sous-tampons** : il en joue un pendant que vous
remplissez l'autre. `is_audio_stream_processed` devient vrai quand le
périphérique a fini l'un d'eux et réclame le suivant. On lui donne alors
exactement un sous-tampon d'audio :

```ocaml
R.update_audio_stream o.stream (Ctypes.to_voidp (Ctypes.CArray.start o.buf)) chunk
```

Le piège : `set_audio_stream_buffer_size_default` fixe la taille d'**un
sous-tampon**, pas du flux entier. Si les deux nombres diffèrent, il n'y a pas
d'erreur. Mettez à jour avec la moitié d'un sous-tampon et le reste est périmé
ou silencieux, ce qui sonne exactement comme « des échantillons différents avec
une pause entre ». D'où `chunk` dans les deux appels.

`chunk = 8192` frames, c'est 8192 / 44100 ≈ 186 ms. C'est aussi toute la
tolérance à une frame lente : la boucle du jeu tourne à 60 i/s et ne recharge
que lorsque raylib le demande, donc une frame lente (fin d'une recherche de bot,
dessin lourd) doit durer moins que l'audio déjà en file. La première version
utilisait des blocs de 2048 frames (46 ms), trop courts pour absorber un
ralentissement.

`buf` est un `Ctypes.CArray` d'`int16` contenant les échantillons gauche et
droit entrelacés (`2·i` et `2·i + 1`), la disposition attendue par raylib en
stéréo.

Tout est piloté par la boucle principale, pas par un thread audio :

```ocaml
(* bin/gui.ml, once per frame *)
Ocean.update ~active:(match !screen with Lobby l -> l.gs.ocean | _ -> false)
             ~volume:sound_level
```

`update` crée le flux paresseusement la première fois qu'on en a besoin, le
remplit tant que `active` est vrai ou que le fondu de sortie reste audible, puis
se tait (plus de rechargement) une fois le fondu à 0. `unload` libère le flux à
l'arrêt, avant la fermeture du périphérique audio.

## 8. Que modifier, et ce que cela coûte

Tous les réglages sont des constantes en tête de `gui/ocean.ml` :

| constante | effet |
|---|---|
| `intensity` (0,8) | niveau global de tout le mix |
| `wind_level` (0,35) | vent par rapport aux vagues ; 0 supprime le vent |
| `speed` (1,0) | étire la houle : 2,0 double le tempo des vagues |
| `chunk` (8192) | latence contre robustesse ; ne pas descendre sous ~4096 |

À savoir avant de régler :

- `speed` n'affecte que la houle et le va-et-vient des vagues. L'enveloppe de
  rafale l'ignore, comme dans l'original.
- Comme chaque table de bruit boucle, la texture des vagues a une faible
  périodicité de 0,32 s. L'oscillation lente de phase en cache l'essentiel. Une
  table plus longue (8192 ou 16384 entrées) repousserait la répétition ; il
  faudrait changer ensemble `land 4095` et le `4096.`.
- La synthèse coûte une poignée de `sin`, un `pow`, un `exp` et les lectures de
  bruit par échantillon, environ 44 100 fois par seconde. C'est très en dessous
  du budget d'une frame, ce qui explique qu'elle puisse vivre sur le thread de
  rendu.

## 9. Différences avec l'original HTML

| original | ici |
|---|---|
| `ScriptProcessor`, blocs de 4096 frames | `AudioStream` de raylib, blocs de 8192 frames |
| passe-bas biquad Web Audio, Q 0,4, coupure lissée sur 0,2 s | deux étages à un pôle, coupure recalculée à chaque échantillon |
| curseurs pour l'intensité, le vent, la vitesse, la durée | constantes |
| export WAV avec fondu enchaîné de 3 s | aucun : le flux est sans fin |
| barres de spectre | aucune |

## À essayer

```sh
dune build
dune exec ./bin/gui.exe
# lobby -> Réglages -> Son -> cocher « Ambiance de l'océan dans le hall »
```

Pour entendre ce qu'apporte un ingrédient, mettez `wind_level` à `0.` (vagues
seules), ou remplacez par une constante l'`env` utilisé pour la coupure dans
`fill` (plus d'éclaircissement).
