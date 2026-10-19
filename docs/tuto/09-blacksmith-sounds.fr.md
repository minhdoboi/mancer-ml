# 9. Les sons du forgeron

Deux cartes de la maison Forgeron — **Bargulian Gauntlet** et **Excalibur** —
ont des clips dont tout l'intérêt est de tomber exactement sur une animation
dessinée : le coup doit atteindre l'image où le poing arrive sur la rangée
d'en face, et la déchirure doit se produire au moment où la lame ressort de
l'autre côté. Rien de téléchargeable ne donne ça, alors les clips sont
**synthétisés à partir de bruit et de filtres** par un petit script Python,
`scripts/gen_blacksmith_sounds.py` (167 lignes) :

```sh
python3 scripts/gen_blacksmith_sounds.py
# wrote .../assets/sounds/barguliangauntlet.mp3 1.65s
# wrote .../assets/sounds/excalibur.mp3 2.05s
```

Il faut `python3` avec **numpy**, et **ffmpeg** avec `libmp3lame` (la
bibliothèque standard de Python n'a pas d'encodeur mp3 ; le script écrit donc un
WAV 16 bits et laisse ffmpeg le compresser). Rien d'autre dans la compilation ne
dépend de l'un ou l'autre : le script se lance à la main, et le jeu ne voit
jamais que deux fichiers `.mp3`.

## 1. Ce que le jeu cherche

Un son de carte n'est pas indexé par la clé de la carte. Sur `Ev_spell_played`,
le plateau appelle `play_card_sound` (`gui/gui_board.ml:126`), et le nom est
construit à partir du nom **anglais** de la carte — en minuscules, espaces
supprimés (`gui/gui_draw.ml:261`) :

```ocaml
let card_sound_name (c : card) =
  Assets.english_card_name c |> String.lowercase_ascii |> String.split_on_char ' ' |> String.concat ""

let play_card_sound (c : card) = play_sound ~volume:0.9 (card_sound_name c)
```

`Assets.sound` cherche ensuite `assets/sounds/<nom>` avec les extensions
`.mp3`, `.ogg`, `.wav` (`lib/game/assets.ml:109`). Donc :

| carte | nom anglais | fichier |
|---|---|---|
| `blacksmith.excalibur` | `Excalibur` (`assets/i18n/en.properties:1275`) | `assets/sounds/excalibur.mp3` |
| `blacksmith.barguliangauntlet` | `Bargulian Gauntlet` (`:1277`) | `assets/sounds/barguliangauntlet.mp3` |

Deux conséquences à retenir. Un nom qui n'existe que dans une autre langue ne
joue jamais, puisque la recherche se fait sur la chaîne anglaise ; et la règle
est fruste — seuls les espaces sont supprimés, donc le `Wizard's Hat` du
Forgeron chercherait `wizard'shat.mp3`, apostrophe comprise. Le reste de la
maison (Mithril Spear, Mithril Bow, Mithril Shield, Flying Chariot, Pegasus,
Cloak and Dagger) n'a aucun clip : `Assets.sound` renvoie `None` et la lecture
est sautée ; jouer ces cartes est simplement silencieux.

Le volume est `0.9 · sound_level ()`, le curseur Level de Réglages → Son, lu
dans le fichier de préférences. Les sons sont décodés une fois dans un cache
`Hashtbl` (`gui/gui_draw.ml:233`) et libérés à la fermeture
(`bin/gui.ml:499`).

Les deux cartes se déclenchent depuis `card_effect`, quand le porteur **arrive
en jeu** (`lib/houses/special/blacksmith.ml:276` et `:299`) : pour entendre un
clip, il faut jouer la carte. **F8** fait défiler `Fx.demo_names`, mais
`Fx.spawn_demo` ne déclenche que le visuel — aucun événement, donc F8 montre
l'animation en silence.

## 2. Pourquoi le clip n'est pas un impact

Le son et l'animation démarrent tous deux sur le même événement
`Ev_spell_played` ; le clip est donc écrit comme un **court morceau avec une
prise d'élan**, pas comme un impact unique. Cette prise d'élan n'est pas du
temps gratuit : le gant ramène le poing en 0.42 s et frappe en 0.16, et l'épée
est forgée pendant 0.85 s, tenue, relâchée à 1.15 s, et met 0.22 s à traverser
la rangée.

Chaque nombre utilisé par le script est l'une de ces constantes, recopiée à la
main :

| visuel (`gui/fx_dedicated.ml`) | valeur | audio (`scripts/gen_blacksmith_sounds.py`) |
|---|---|---|
| `wind +. punch` (`:613`) | 0.58 | `hit = 0.58`, toutes les couches d'impact du gant |
| `gauntlet_duration` (`:609`) | 1.65 | `d = 1.65`, la longueur du tampon du gant |
| `forge` (`:440`) | 0.85 | les quatre partiels de cloche durent 0.85 s, et le « swoosh » de 0.5 s démarre à 0.35 |
| `excalibur_lunge` (`:436`) | 1.15 | le « swoosh » du coup d'épée est posé à 1.10, il se termine à 1.36 |
| `excalibur_lunge +. drive` | 1.37 | `hit = 1.37`, l'impact |
| `excalibur_duration` (`:435`) | 2.05 | `d = 2.05` |

Rien ne vérifie ça. Changez `drive` dans `fx_dedicated.ml` et vous devez
changer `hit` dans le script, sinon la déchirure n'arrive plus avec la lame.

Remarquons aussi que le gant ne touche pas toutes les rangées en même temps :

```ocaml
let hit i = wind +. punch +. (Float.abs (p.x -. wielder.x) /. far_of p.x *. wave)
```

La rangée la plus proche est frappée à 0.58 s et les plus lointaines jusqu'à
0.62 s plus tard : c'est de là que vient le voyage du front de choc. L'impact
sonore est à 0.58 s — sur le coup lui-même, pas sur la dernière rangée — donc
le son porte l'animation au lieu de la suivre.

## 3. La boîte à outils

Une poignée d'aides fait tout le travail ; le reste du script n'est qu'un
empilement de couches mélangées dans un tampon.

**Axe des temps et bruit.** `t_(d)` vaut `arange(d · 44100) / 44100`,
`noise(d)` tire `d` secondes de bruit blanc du générateur à graine fixe,
`env_exp(d, tau)` est la décroissance habituelle `exp(-t/tau)`.

**`fft_filter(x, lo, hi)`** filtre le bruit par bande de fréquences. On prend
la FFT réelle, on multiplie chaque bac par un masque approximativement
« brique », et on revient :

```python
if lo: m *= 1 / (1 + (lo / np.maximum(f, 1e-3)) ** 4)   # high-pass
if hi: m *= 1 / (1 + (f / hi) ** 4)                      # low-pass
```

La puissance quatrième donne environ 24 dB par octave de pente. Les bandes de
fréquence du script ne sont pas de l'égalisation discrète : c'est la *couleur*
de chaque couche — un tremblement de terre de 22 à 160 Hz, une déchirure de
1200 à 7500 Hz. Filtrer du bruit (et non une tonalité) laisse aussi les
transitoires intacts.

**`svf_sweep` / `swoosh(d, f0, f1, peak)`** font les souffles. `svf_sweep` est
un passe-bande à variables d'état dont la fréquence centrale change d'un
échantillon à l'autre ; `swoosh` fait donc passer du bruit dans une bande qui
balaye exponentiellement de `f0` à `f1` — 700 → 5200 Hz pour le lâcher de
l'épée. L'enveloppe

```python
e = np.sin(np.pi * u ** peak) ** 2
```

décide de la place de l'énergie dans le balayage : `peak = 0.7` la met tôt
(`u = 0.5^(1/0.7) ≈ 0.37` du parcours), `peak = 1.9` la met tard (`≈ 0.69`),
ce qui fait arriver vite le coup d'épée et déverser le souffle du gant dans
l'impact. La boucle est en Python, échantillon par échantillon ; les souffles
restent donc à 0.2–0.5 s, et tout le script s'exécute en quelques secondes.

**`put(buf, x, at, gain)`** mélange une couche dans le tampon à un instant
donné. Il tronque à la fin (`j = min(len(buf), i + len(x))`), et c'est
pourquoi la couche de 1.07 s du tremblement qui démarre à 0.60 s dans un
tampon de 1.65 s perd ses 20 dernières ms — à `exp(-1.07/0.5) ≈ 0.12` du
pic, ce qui n'a aucune importance.

**`norm(x, peak=0.89)`** est à la fois le saturateur et le niveau :

```python
x = np.tanh(x * 1.0)
return x / (np.max(np.abs(x)) + 1e-9) * peak
```

`tanh` est un écrêtage doux : tout ce qui y passe gagne les mêmes harmoniques
discrètes au lieu de s'écrêter tout net, et c'est en grande partie pour cela
qu'un mélange de neuf ou dix couches sonne comme un seul objet. Le pic final de
0.89 vaut −1.0 dBFS, et il est appliqué *après* le tanh : c'est l'écrêtage
qui décide du volume, pas la somme.

**`save(name, x, drive)`** met éventuellement l'échelle à `drive` (pour
dépasser volontairement la pleine échelle, saturer, puis redescendre),
appelle `norm`, applique un fondu de sortie de 30 ms sur les derniers
échantillons, écrit un WAV mono 16 bits à 44.1 kHz dans un fichier temporaire
puis appelle ffmpeg :

```python
subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", p, "-codec:a", "libmp3lame",
                "-b:a", "128k", out], check=True)
```

Le fondu n'est pas décoratif : sans lui, le clip s'arrête sur l'échantillon
arbitraire où la dernière couche se trouve, et c'est un claquement.

## 4. Bargulian Gauntlet, couche par couche

Le cahier des charges est dans la docstring de `gauntlet()` : *un poing
enfoncé dans le sol assez fort pour déclencher un séisme : uniquement du
grave — un thud sourd et profond, jamais rien de brillant ni de métallique*.
Chaque couche obéit.

| à | couche | ce que c'est |
|---|---|---|
| 0.00 | pression | sinue balayant 40 → 95 Hz en 0.55 s, amplitude `u²·0.5`, `tanh(·2)` |
| 0.38 | souffle | `swoosh(0.2, 120, 900, 1.8)`, gain 0.9 — se termine exactement sur 0.58 |
| 0.58 | boom | 120 → 30 Hz, la hauteur s'affaisse avec `exp(-t/0.06)`, `tanh(·2.5)·exp(-t/0.35)` |
| 0.58 | thud | bruit 35–380 Hz, `exp(-t/0.07)`, `tanh(·6)·1.6` — de la terre tassée |
| 0.58 | claque | bruit 90–650 Hz, `exp(-t/0.025)` — le poing sur la surface, fini en ~75 ms |
| 0.58 | corps | sinusoïde 78 Hz, `exp(-t/0.09)` — ce qu'on ressent |
| 0.60 | séisme | bruit 22–160 Hz, attaque 30 ms, `exp(-t/0.5)`, modulé en amplitude à 7.5 Hz |
| 0.64 | crépitement | 40 claquements/s entre 60 et 420 Hz, chacun lissé par une exponentielle de 50 ms, le tout sous `exp(-t/0.4)` |
| 0.66 | grondement | bruit 70–500 Hz, trémolo à 13 Hz élevé à la puissance 1.5, `exp(-t/0.4)` |
| — | une ligne | `b = fft_filter(b, hi=1400)` — *rien au-dessus des bas médiums, nulle part* |

Cette dernière ligne est celle qui fait lire le coup comme de la terre et non
comme un impact métallique : à 2.8 kHz, le masque est déjà 17 fois plus bas (−24
dB), donc disparaissent toutes les conséquences d'une couche trop claire — le
sifflement, l'éclat du crépitement, les harmoniques des 78 Hz produits par la
saturation. C'est aussi la seule ligne du fichier qu'aucune couche ne peut
remplacer : la saturation ajoute des harmoniques, celle-ci les retire.

C'est la modulation du séisme qui le fait trembler plutôt que gronder :

```python
quake *= 0.6 + 0.4 * np.sin(2 * np.pi * 7.5 * qt + 1.0)
```

7.5 Hz est sous les ~20 Hz où l'on cesse d'entendre une hauteur et commence à
sentir un mouvement : le sol respire sept fois par seconde sans jamais devenir
une tonalité.

`save("barguliangauntlet", b, drive=1.7)` pousse la somme à 1.7 × la pleine
échelle avant le tanh. Le gant est volontairement sur-saturé : c'est un son
mat fait de couches très faibles, et c'est la saturation qui les fusionne en un
seul objet lourd au lieu d'un empilement de coups.

## 5. Excalibur, couche par couche

Le cahier des charges est son exact miroir : *une déchirure arrachée et
crépitante plutôt qu'un cliquetis*. Aucun partiel de cloche dans l'impact, et
aucune résonance : une épée qui découpe des gens n'est pas une cloche.

| à | couche | ce que c'est |
|---|---|---|
| 0.00 | forge | quatre sinusoïdes à 1318 / 1760 / 2637 / 3520 Hz, 0.85 s, enveloppe `u²(1-u)^0.5`, gain 0.05 chacune |
| 0.35 | miroitement | `swoosh(0.5, 900, 4500, 0.9)`, gain 0.1 — se termine sur `forge` |
| 1.10 | coup | `swoosh(0.26, 700, 5200, 1.9)`, gain 0.85 — se termine à 1.36 |
| 1.35 | déchirure | bruit 1200–7500 Hz, amplitude en dents de scie, crépitements 3–12 kHz, attaque 6 ms, `exp(-(t/0.17)^1.5)` |
| 1.37 | déchirure basse | bruit 200–1400 Hz, `tanh(·3)`, `exp(-t/0.09)`, gain 0.55 |
| 1.37 | morsure | 140 → 50 Hz, hauteur qui s'affaisse avec `exp(-t/0.025)`, `exp(-t/0.07)` |
| 1.49 | sifflement | bruit 2500–9000 Hz, `exp(-t/0.1)`, gain 0.18 |

La déchirure est la couche intéressante. Deux produits la composent :

```python
grit = fft_filter(noise(n), lo=1200, hi=7500)                 # le corps de la déchirure
jag  = np.abs(fft_filter(noise(n), hi=260)); jag = (jag / jag.max()) ** 1.6
snaps = (rng.random(len(tt)) < 0.004) * rng.uniform(0.6, 1.4, len(tt))
tear = grit * (0.25 + 0.75 * jag) + fft_filter(noise(n), lo=3000, hi=12000) * snaps * 1.5
```

Le premier produit est la déchirure elle-même, modulée en amplitude par `jag` :
un bruit passe-bas, normalisé puis élevé à la puissance 1.6, ce qui transforme
des bosses lisses en blocs à bords durs — des fibres qui claquent les unes
après les autres plutôt qu'un sifflement continu. Le second est l'éclat par
dessus : `snaps` se déclenche sur 0.4 % des échantillons, soit environ **176
par seconde**, chacun convolué avec une exponentielle de 40 échantillons
(0.9 ms) pour être un clic et non un saut, et limité en bande à 3–12 kHz. La
décroissance `(t/0.17)^1.5` tombe plus vite qu'une exponentielle : une
puissance supérieure à 1 dans l'exposant tue la queue tôt, et c'est ce qui fait
que la déchirure **finit** au lieu de s'éteindre.

La déchirure démarre à 1.35 s, soit 20 ms avant l'impact visuel de 1.37 s,
parce que sa propre attaque de 6 ms et le temps de réaction de l'œil replacent
l'attaque perçue sur l'image. La couche `bite` à 1.37 s, elle, est la lame qui
entre dans quelque chose : une sinusoïde qui tombe de 140 Hz à 50 Hz en 25 ms,
la même astuce que le boom du gant, une octave et demie plus haut.

Notons que `excalibur` est enregistré sans argument `drive`. Avec les partiels
de forge à 0.05 de gain, la somme ne chauffe jamais, et l'épée doit rester
nette ; la sur-saturer transformerait la déchirure en ce grincement buzzant,
chargé d'harmoniques, qui fait penser à une corde de guitare.

## 6. La couche mp3

Deux détails à savoir avant de mesurer quoi que ce soit dans un éditeur de
formes d'onde :

- **Délai de l'encodeur.** ffmpeg annonce `start=0.025057` pour ces fichiers :
  LAME ajoute environ 25 ms de silence au début de chaque mp3. Face à une
  prise d'élan de 0.58 s, cela ne compte pas, mais c'est pourquoi les nombres
  du script sont les temps de *mélange* : une transitoire mesurée arrive 25 ms
  plus tard que ne l'indique le code.
- **Mono, 44.1 kHz, 128 kbps.** Le jeu décode le fichier une fois par raylib et
  le joue sur le même périphérique que tout le reste ; le mono est délibéré,
  puisqu'un son de sort doit rester au milieu du mélange, pas dans un coin.

Le WAV intermédiaire n'est pas un artefact de l'outillage : Python peut écrire
un WAV avec le seul module `wave` de la bibliothèque standard, et cela garde la
quantification 16 bits visible. C'est aussi la toute dernière étape : le script
multiplie les flottants normalisés par 32767 et les écrit en `<i2`, si bien que
la quantification ne peut jamais revenir dans la synthèse.

## 7. Reproductibilité, et pourquoi c'est fragile

Une seule ligne rend tout le script déterministe :

```python
rng = np.random.default_rng(7)
```

et c'est un flux *unique*, consommé dans l'ordre des appels. Relancer le
script reproduit les deux fichiers octet pour octet — je l'ai vérifié en
régénérant par-dessus les assets commités et en comparant les sommes de
contrôle (numpy 2.3.5, ffmpeg 8.0.1). À numpy identique, les échantillons sont
identiques ; les *octets* mp3 dépendent aussi de la compilation de l'encodeur,
donc un autre ffmpeg donne d'autres octets pour un son identique.

La fragilité en est le prix : ajouter un appel à `noise()` au milieu d'une
fonction décale tous les tirages suivants, si bien que le gant que vous avez
réglé n'est pas celui que vous obtenez. Ce n'est pas un bug à corriger : avec
un flux à graine fixe et aucun suivi de version pour la conception sonore, le
« petit ajustement » n'existe tout simplement pas ici. On écoute, on change deux
ou trois nombres, on réécoute.

L'autre conséquence : **il n'y a pas de diff**. Un diff git d'un mp3 ne dit rien
de ce qui a bougé. Gardez l'intention dans les commentaires, comme le fait déjà
le script.

## 8. Régler, et les pièges

Les boutons utiles, dans l'ordre où on les attraperait :

| bouton | effet |
|---|---|
| `hit` dans chaque fonction | moment de l'impact ; doit suivre `fx_dedicated.ml` |
| `d` | longueur du clip ; doit valoir `*_duration`, sinon la queue est coupée ou survit à l'animation |
| bandes de `fft_filter` | couleur de chaque couche ; le plus gros levier sur « qu'est-ce que c'est » |
| gains des `tanh` (`·2.5`, `·6`, `·7`, `·3`) | à quel point chaque couche sature |
| `tau` des `exp` | combien de temps le clip continue après l'impact |
| `drive=1.7` (gant seulement) | saturation globale ; retirez-le et le coup devient mince |
| `peak` dans `swoosh` | place de l'énergie dans un balayage : plus bas, plus tôt |
| `hi=1400` (gant seulement) | le filet de sécurité ; remontez-le et tout artefact revient |

Les pièges, tous payés de notre propre expérience :

- **Des noms, pas des clés.** Le fichier est
  `assets/sounds/barguliangauntlet.mp3`, et non
  `blacksmith.barguliangauntlet.mp3`. Renommez la carte dans
  `assets/i18n/en.properties` et il faut renommer le fichier avec elle.
- **Le son part avec l'effet.** Si la frise du plateau est encore occupée,
  `run_fx` retarde l'animation jusqu'à `ui.anim_at` (`gui/gui_board.ml:579`)
  alors que `play_card_sound` a déjà joué. Seul `NuclearMissile` utilise
  `Fx.sound_delay` (`gui/fx_stage.ml:62`) ; nos deux cartes ne l'utilisent pas,
  ce qui est correct ici puisque la prise d'élan du clip est elle-même le
  délai.
- **Ne jamais laisser une couche dépasser `d`.** `put` la tronque sans
  avertissement : un « swoosh » ou un lit de bruit trop long se fait couper au
  bord du tampon, sans la moindre erreur — vérifiez que
  `at + len(layer) <= d` pour chaque couche.
- **Les assets sont un sous-module.** Le script écrit directement dans
  `assets/sounds/` : committez avec `scripts/push_assets.sh "message"`, comme
  n'importe quel autre changement d'asset.

## Essayez

```sh
python3 scripts/gen_blacksmith_sounds.py
dune build && dune exec ./bin/gui.exe
# jouez Forgeron, mettez Excalibur ou Bargulian Gauntlet sur une créature,
# et laissez-la arriver en jeu (F8 ne montre que le visuel, en silence)
```

Pour n'entendre qu'une couche, mettez en commentaire les autres appels à
`put` — le gant en a neuf, l'épée sept, et chacune est autonome. Pour vérifier
la synchro sans jouer la carte, comptez : le gant doit frapper à 0.58 s et
l'épée à 1.37 s ; regardez donc l'animation avec un chronomètre la première
fois. Pour entendre le gant sans la terre — et voir combien fait le passe-bas —
mettez en commentaire `b = fft_filter(b, hi=1400)`.
