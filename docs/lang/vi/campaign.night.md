# Passage à la nuit — `campaign.night`

Fiche de langue pour la séquence d'entrée du chapitre *night*
(`campaign.night.*`), lue au nœud `night_start` (chapitre *night*, débloqué
après `wind`, le duel d'Aelwyn). Ce n'est pas un duel : quatre tableaux
narratifs — `rest`, `visit`, `meal`, `out` — racontent le réveil du joueur
dans une maison inconnue, l'accueil d'un hôte, le repas, puis la sortie sous
les étoiles. Chaque tableau porte un `title` que la boîte de dialogue affiche
en tête. C'est le seul passage qui fait basculer le joueur du chapitre *day*
au chapitre *night*.

**Ton.** Le récit est à la deuxième personne neutre (`Bạn`), comme l'ouverture
et l'épilogue (`campaign.intro.md`, `campaign.epilogue.md`) : c'est la voix du
narrateur. L'hôte, lui, dit `tôi` et appelle le joueur `bạn` — le registre
cordial et poli des villageois qui accueillent (comme `porter` ou `farmer`),
et non le `ta`/`ngươi` des maîtres de duel. Après le `ngươi` méprisant ou
dominateur de presque tous les combats, la campagne respire au `bạn`.

---

## 1. `campaign.night.rest` — le réveil dans l'inconnu

> Ban đêm

*Nuit.*

> Bạn thức dậy trong đêm trên một chiếc giường, trong một ngôi nhà lạ.

*Tu te réveilles la nuit, dans un lit, dans une maison inconnue.*

### `Ban đêm` — la nuit, de nuit

- `ban` : moment, heure (souvent d'un moment défini de la journée,
  **purement vietnamien**). `đêm` : nuit (native).
- `Ban đêm` = « la nuit » comme plage de temps. Voir `campaign.soulbinder.md`
  (`vào ban đêm`) et `campaign.powermancer.md` (`ban ngày`, l'exact
  parallèle diurne). Le titre ouvre le chapitre nocturne en nommant son heure.

### `thức dậy` — se réveiller, sortir du sommeil

- `thức` : veiller, être éveillé (**purement vietnamien**). `dậy` : se lever.
- `thức dậy` = « se réveiller ». Le corps passe du sommeil à la veille sans
  transition — le narrateur ne décrit ni rêve ni cauchemar, seulement le
  surgissement. À comparer à `tỉnh dậy` de Corvin
  (`campaign.antimancer.md`) : même retour à soi, mais ici sans magie.

### `chiếc giường` — le lit

- `chiếc` : classificateur des objets (bateaux, vélos, lits ; **purement
  vietnamien**). `giường` : lit (native).
- `trên một chiếc giường` = « sur un lit ». Le détail est concret et rassurant :
  on l'a couché, on ne l'a pas laissé par terre.

### `ngôi nhà lạ` — une maison inconnue

- `ngôi` : classificateur des maisons (**purement vietnamien**). `nhà` :
  maison (native). `lạ` : étranger, inconnu (native).
- `lạ` = « inconnu », plus neutre que « bizarre ». À rapprocher de
  `vùng đất xa lạ` (`campaign.intro.md`) et de `giọng nói xa lạ` (§ 2) : tout
  le chapitre commence par du `lạ` — de l'inconnu accueillant.

---

## 2. `campaign.night.visit` — la voix de l'hôte

> Một giọng nói xa lạ

*Une voix inconnue.*

> Tôi đã theo bạn lên núi. Bạn ngất xỉu sau khi rời Aelwyn, nên vợ tôi và tôi đã đưa bạn về đây.

*Je t'ai suivi dans la montagne. Tu t'es évanoui après avoir quitté Aelwyn,
alors ma femme et moi t'avons porté jusqu'ici.*

### `giọng nói xa lạ` — une voix inconnue

- `giọng` : voix, timbre (**purement vietnamien**). `nói` : parler (native).
  `xa lạ` : inconnu (voir § 1).
- Le titre apparaît avant que l'hôte ne soit nommé : la voix précède le
  visage. Écho à `tìm lại giọng nói` de `zen` (`campaign.zen.md`) — la voix
  comme première preuve d'existence.

### `theo … lên núi` — suivre dans la montagne

- `theo` : suivre (**purement vietnamien**). `lên` : monter. `núi` :
  montagne (native).
- `Tôi đã theo bạn lên núi` = « je t'ai suivi dans la montagne ». L'hôte
  révèle qu'il a fait route avec le joueur sans qu'il le sache : la
  bienveillance était déjà là, invisible.

### `ngất xỉu` — s'évanouir, perdre connaissance

- Composé **purement vietnamien** : `ngất` (« défaillir ») + `xỉu`
  (« s'affaisser »). `ngất xỉu` = tomber sans connaissance.
- `Bạn ngất xỉu` = « tu t'es évanoui ». Voir `mất ý thức`
  (`campaign.antimancer.md`) : le même effacement, mais ici expliqué par la
  fatigue, non par la magie.

### `rời` — quitter

- **Purement vietnamien** (voir `rời đi` dans `campaign.trappers.md`,
  `campaign.epilogue.md`).
- `sau khi rời Aelwyn` = « après avoir quitté Aelwyn ». Le nom du personnage du
  duel précédent (`wind`, `campaign.wind.md`) ancre la séquence dans la
  chronologie : c'est en sortant du vent qu'on entre dans la nuit.

### `vợ tôi` — ma femme

- `vợ` : épouse (**purement vietnamien**). `tôi` : je, moi (pronom de la
  première personne, **neutre et poli**, à la différence du `ta` dominateur des
  maîtres).
- Le détail du couple `vợ chồng` (§ 4) fait de l'hôte un être social, enraciné ;
  ce n'est ni un mage ni un adversaire.

### `đưa bạn về đây` — t'emmener ici

- `đưa` : conduire, porter (**purement vietnamien**). `về` : vers, de retour.
  `đây` : ici.
- `đưa bạn về đây` = « t'avoir ramené ici ». Le geste du sauveteur est raconté
  sans emphase : une phrase suffit à tout expliquer.

---

## 3. `campaign.night.meal` — le repas offert

> Bữa ăn nóng

*Un repas chaud.*

> Hãy ngồi xuống chiếc khăn và ăn chút gì đó. Sau đó bạn có thể khám phá xứ này vào ban đêm nếu muốn.

*Assieds-toi sur le linge et mange quelque chose. Ensuite tu pourras explorer
ce pays de nuit, si tu veux.*

### `Bữa ăn nóng` — le repas chaud

- `bữa` : repas, séance (**purement vietnamien**). `ăn` : manger (native).
  `nóng` : chaud (native).
- `Bữa ăn nóng` = « un repas chaud ». Le `nóng` (« chaud ») s'oppose à tout le
  froid des duels — du `Cái lạnh` de `snowblood` (`campaign.snowblood.md`) au
  `bóng đêm` des mages. La chaleur du repas est la première chaleur de la nuit.

### `Hãy` — particule d'impératif adouci

- **Purement vietnamien.** `Hãy` adoucit l'ordre : « aie la bonté de… », « fais
  donc… ».
- Voir `campaign.powermancer.md` (`Hãy cẩn thận`) : le même outil d'une
  politesse douce.

### `chiếc khăn` — le linge, la serviette

- `chiếc` : classificateur (§ 1). `khăn` : tissu, serviette, foulard (native).
- `ngồi xuống chiếc khăn` = « s'asseoir sur le tissu ». Image rustique du
  pique-nique improvisé : on a étalé un linge à même le sol pour l'hôte.

### `ăn chút gì đó` — manger un peu de quelque chose

- `ăn` : manger. `chút` : un peu (**purement vietnamien**). `gì đó` : quelque
  chose (`gì` « quoi » + `đó` « là », voir `campaign.intro.md`).
- `chút` minimise : on n'impose pas un festin, juste de quoi reprendre des
  forces. La générosité est discrète.

### `khám phá` — explorer, découvrir

- Composé savant : `khám` (« inspecter, examiner ») + `phá` (« percer »). Le
  mot de l'exploration méthodique.
- `khám phá xứ này` = « explorer ce pays ». Le jeu s'ouvre : après le couloir
  de duels du jour, la carte nocturne devient un espace à parcourir.

### `xứ này` — ce pays, cette contrée

- `xứ` : contrée, pays, **sino-vietnamien `處`** (« lieu »). `này` : ce.
- À distinguer de `đất nước` (`campaign.powermancer.md`, un pays-territoire) et
  de `vùng đất` (`campaign.intro.md`) : `xứ` est la région qu'on traverse,
  presque une province.

### `nếu muốn` — si tu veux

- `nếu` : si (**purement vietnamien**). `muốn` : vouloir (native).
- `nếu muốn` = « si tu veux ». La formule laisse toute liberté au joueur :
  l'hôte propose la découverte sans l'imposer.

---

## 4. `campaign.night.out` — la sortie sous les étoiles

> Dưới những vì sao

*Sous les étoiles.*

> Sức mạnh dần trở lại. Bạn cảm ơn vợ chồng chủ nhà và bước ra ngoài, tò mò về mùi đồng quê ban đêm, tiếng động của thế giới ban đêm.

*Ta force revient peu à peu. Tu remercies tes hôtes et tu sors, curieux de
l'odeur de la campagne la nuit, des bruits du monde nocturne.*

### `Dưới những vì sao` — sous les étoiles

- `dưới` : sous (**purement vietnamien**). `những` : marque du pluriel.
  `vì sao` : étoile (`vì` « étoile » + `sao` « étoile », paire figée).
- Titre lyrique, le seul de la séquence à être une image. Les étoiles
  remplacent le toit : le joueur quitte l'abri et retrouve le ciel.

### `Sức mạnh dần trở lại` — la force revient peu à peu

- `sức mạnh` : force (**`sức` et `mạnh` purement vietnamiens**). `dần` : peu
  à peu (native). `trở lại` : revenir (native).
- `dần` installe la lenteur de la récupération : la force n'est pas rendue d'un
  coup. C'est l'exact inverse du réveil brutal du § 1.

### `cảm ơn` — remercier

- `cảm` : ressentir, être touché, **sino-vietnamien `感`**. `ơn` : bienfait,
  faveur (**purement vietnamien**).
- `cảm ơn` = « remercier », littéralement « ressentir le bienfait ». La
  gratitude passe par le corps avant la parole.

### `vợ chồng chủ nhà` — les hôtes, le couple

- `vợ chồng` : le couple, mari et femme (**purement vietnamien**, § 2).
  `chủ nhà` : l'hôte, le maître de maison (`chủ` **sino-vietnamien `主`** +
  `nhà` native).
- `vợ chồng chủ nhà` = « les hôtes ». L'expression consacre la seule figure
  pacifique de la campagne nocturne : un foyer, pas une faction.

### `bước ra ngoài` — sortir, faire un pas dehors

- `bước` : pas, marcher (**purement vietnamien**). `ra` : sortir. `ngoài` :
  dehors.
- `bước ra ngoài` = « franchir la porte, sortir ». Le seuil est l'acte qui
  ouvre le chapitre : le joueur quitte la maison accueillante pour la carte
  nocturne.

### `tò mò` — curieux

- Redoublement expressif **purement vietnamien** : `tò` + `mò`. `tò mò` =
  « curieux », « qui veut savoir ».
- La curiosité est le moteur du joueur-narrateur, exactement comme
  `điều gì đó khiến bạn lo lắng` ferme l'ouverture (`campaign.intro.md`).

### `mùi đồng quê` — l'odeur de la campagne

- `mùi` : odeur (**purement vietnamien**). `đồng` : champ (native). `quê` :
  village natal, campagne (native).
- `đồng quê` = « la campagne, les champs ». L'odorat est le sens de cette
  séquence : on sent l'air avant de le comprendre.

### `tiếng động` — le bruit, les sons

- `tiếng` : son, voix (**purement vietnamien**). `động` : remuer, bouger
  (**sino-vietnamien `動`**).
- `tiếng động của thế giới ban đêm` = « les bruits du monde nocturne ». Le
  monde devient sonore et vivant ; `thế giới` (`世界`) reprend le mot du
  `message` de Corvin (`campaign.antimancer.md`), mais ici il n'est plus
  `tưởng tượng` — il est entendu.
