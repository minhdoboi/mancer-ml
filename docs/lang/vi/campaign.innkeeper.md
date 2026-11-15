# Perrin — `campaign.innkeeper`

Fiche de langue pour le texte de la rencontre `innkeeper` (chapitre *day*,
débloquée après `dudes` ; le nœud est une **rencontre**, non un duel : il figure
dans `Campaign.npcs`, son portrait est `innkeeper` et son nom `Perrin`).
C'est l'aubergiste de l'étape qui suit l'arène des Dudes : après les coups, un
lit. La rencontre offre deux `choice` — `accept` (`Nhận lời`), qui enchaîne sur
`rest` puis `wake`, et `pass` (`Đi tiếp`), qui referme le nœud.

**Ton.** L'aubergiste adresse au joueur `bạn`, la deuxième personne neutre et
amicale déjà entendue chez Clover (`campaign.rabbit.md`), jamais le `ngươi`
autoritaire de Brannoc (`campaign.dudes.md`). Ce tutoiement amical dit le
commerçant qui accueille, non le maître qui juge : la halte est une parenthèse
de douceur entre deux duels.

---

## 1. `message` — l'offre d'un lit

> Trông bạn mệt lắm. Nghỉ một chút ở đây đi. Vẫn còn chỗ.

*Tu as l'air très fatigué. Repose-toi un peu ici. Il reste encore de la place.*

### `Trông … mệt` — tu as l'air fatigué

- `trông` : regarder, paraître (**purement vietnamien**) ; suivi d'un adjectif,
  il signifie « avoir l'air ». `mệt` : fatigué, las (native). `lắm` : intensif
  placé après l'adjectif, « très, beaucoup » (native).
- Tout le premier membre est d'un seul tenant natif, sans un seul emprunt.
  L'aubergiste parle la langue du peuple.

### `Nghỉ một chút` — repose-toi un peu

- `nghỉ` : se reposer, s'arrêter (**purement vietnamien**). `một` : un (native).
  `chút` : un peu, une miette (native).
- `một chút` mesure la halte : non la nuit entière, juste « un moment ». La
  proposition reste modeste.

### `ở đây đi` — ici, va donc

- `ở` : être à, rester (native). `đây` : ici (native). `đi` : ici non pas
  « aller » mais particule d'exhortation adoucie, « allez, fais donc » (native).
- Le `đi` final transforme l'offre en invitation : ce n'est pas un ordre de
  tavernier, c'est un geste.

### `vẫn còn chỗ` — il reste encore de la place

- `vẫn` : encore, toujours (**purement vietnamien**). `còn` : rester, subsister
  (native). `chỗ` : place, endroit (native).
- Trois mots natifs pour une phrase toute simple : l'auberge a de la place, même
  pour un étranger. `chỗ` annonce `những người xa lạ` du § 2.

---

## 2. `rest` — le sommeil partagé

> Bạn nghỉ một chút giữa những người xa lạ, người vẫn còn ê ẩm.

*Tu te reposes un peu parmi des inconnus, le corps encore courbaturé.*

### `nghỉ một chút`

(`nghỉ một chút` est expliqué au § 1.)

### `giữa những người xa lạ` — parmi des inconnus

- `giữa` : parmi, au milieu de (**purement vietnamien**). `những` : pluriel
  indéterminé (native). `người` : personne (native, voir `campaign.panther.md`).
  `xa` : loin (native) ; `lạ` : étrange, inconnu (native).
- `xa lạ` assemble « loin » et « étrange » : l'inconnu est celui qui est loin de
  soi. L'image du § 1 (`vẫn còn chỗ`) se referme : la place était celle des
  autres, on la partage.

### `người` — le corps

- `người` : personne, mais aussi le corps, l'être physique (**purement
  vietnamien**). Employé seul et sans possessif, il désigne le corps du dormeur.
- Le récit quitte le `bạn` de l'adresse pour `người`, le corps : le « tu » se
  dédouble ; ce n'est plus toi qui parles, c'est ta fatigue.

### `vẫn còn ê ẩm` — toujours courbaturé

- `vẫn còn` : toujours, encore (native, écho du § 1). `ê` : endolori, engourdi
  (native) ; `ẩm` : humide, moite (native).
- `ê ẩm` est un redoublement qui dit une douleur sourde et diffuse, celle d'un
  corps mal couché. Les coups des Dudes (`campaign.dudes.md`) n'ont pas quitté
  les os.

---

## 3. `wake` — le réveil

Le titre du dialogue (`campaign.innkeeper.wake.title`) reprend `Một giờ sau`,
la même mesure de temps que la première phrase.

> Bạn tỉnh dậy một giờ sau. Bạn thấy cơn đau đã dịu bớt. Ngoài trời vẫn còn sáng.

*Tu te réveilles une heure plus tard. Tu sens que la douleur s'est apaisée.
Dehors, il fait encore jour.*

### `tỉnh dậy` — se réveiller

- `tỉnh` : s'éveiller, reprendre conscience (**purement vietnamien**). `dậy` :
  se lever, se dresser (native).
- Les deux verbes natifs disent le réveil du corps et son redressement : on ne
  sort pas d'un rêve, on se relève.

### `một giờ sau` — une heure plus tard

- `một` : un (native). `giờ` : heure (native). `sau` : après (native).
- Le temps de la halte est compté : une heure, pas la nuit. Là où les maîtres de
  la montagne parlent d'un temps indécis, l'aubergiste donne l'heure.

### `Bạn thấy cơn đau đã dịu bớt` — tu sens que la douleur s'est apaisée

- `thấy` : voir, sentir, éprouver (**purement vietnamien**). `cơn` :
  classificateur des accès et des crises (native) ; `đau` : douleur, mal
  (native). `dịu` : adouci, apaisé (native) ; `bớt` : diminué, en moins
  (native).
- Le corps reprend la parole : après le `người ... ê ẩm` du § 2, voici `cơn
  đau` (l'accès de douleur), puis `dịu bớt`. Le couple `dịu` + `bớt` dit une
  douleur qui s'adoucit, non une guérison nette : rien ne se répare d'un coup.

### `Ngoài trời vẫn còn sáng` — dehors, il fait encore jour

- `ngoài` : dehors, à l'extérieur (**purement vietnamien**), placé devant
  `trời` pour dire « au-dehors ». `Trời` : le ciel, employé ici au sens
  impersonnel de « il fait » (native, voir `campaign.meteomancer.md`). `sáng` :
  clair, lumineux (native). `vẫn còn` : encore (écho du § 1 et du § 2).
- `ngoài trời` inscrit la lumière dans le monde extérieur : le dormeur, lui,
  est resté dans la pénombre de l'auberge. Le ciel était le personnage des
  Meteomancers ; ici il n'est plus qu'un indicateur d'heure : le dormeur reste
  dans le monde ordinaire. `sáng` renvoie au jour du chapitre (*day*), et
  l'heure écoulée laisse le duel encore possible.

---

*Le nœud entier tient dans le vocabulaire natif : ni `vị`, ni `pháp sư`, ni
aucun des emprunts sino-vietnamiens qui habillent les maîtres de la montagne.
Cette absence est le registre même de l'aubergiste — la langue de tous les
jours, offerte à un étranger fatigué.*
