# Malachar, **Tư Tế Bóng Tối** — `campaign.darkpriest`

Fiche de langue pour le combat de campagne `darkpriest` (chapitre *night*,
débloqué après `shadowcaster` ; bot `oracle` ; **vie pleine, 60 points de vie** —
le nœud ne fixe aucune valeur ; nœud non passable). Malachar est un prêtre des
ténèbres : son texte est un rituel à trois temps — l'ordre de s'agenouiller, le
lieu saint, l'hymne écrite dans le sang. Le vocabulaire est celui du culte et de
la contrainte.

**Ton.** Malachar est **impérieux** : il ouvre par un impératif (`Quỳ xuống`) et
parle de `bài kinh` (« hymne »), de `linh hồn` (« âme ») et de `trói buộc`
(« liens »). Il dit `ta` et `ngươi`, avec la dureté d'un fonctionnaire du sacré
plutôt que d'un fanatique. Sa menace est administrative : si tu ne pries pas, on
écrira ta dernière prière à ta place.

---

## 1. `message` — le lieu saint

> Quỳ xuống. Ngươi đã bước vào một nơi thánh thiện. Ta ở đây để bảo vệ linh hồn của nó. Nếu ngươi không chịu cầu nguyện, ta sẽ viết bài kinh cuối cùng của ngươi bằng máu.

*Agenouille-toi. Tu as pénétré en un lieu saint. Je suis ici pour en protéger
l'âme. Si tu ne consens pas à prier, j'écrirai ta dernière hymne avec du sang.*

### `Quỳ xuống` — agenouille-toi

- `quỳ` : s'agenouiller (**purement vietnamien**). `xuống` : vers le bas,
  descendre (**purement vietnamien**).
- Impératif sans sujet, à l'attaque du texte : le premier mot est un ordre
  corporel. Le `win`/`lose` jugent précisément l'obéissance à ce mot (`Ngươi
  không chịu quỳ`, `quỳ lại không?`).

### `bước vào` — entrer, pénétrer

- `bước` : marcher, faire un pas (**purement vietnamien**, voir
  `campaign.trappers.md`, `campaign.faerie_druid.md`). `vào` : entrer.
- `đã bước vào` = « tu as franchi le seuil ». Le pas est déjà accompli : la
  faute est dans le passé, le jugement peut commencer.

### `nơi thánh thiện` — le lieu saint

- `nơi` : lieu, endroit (**purement vietnamien**). `thánh thiện` :
  saint, sacré (**sino-vietnamien `聖` + `善`** : `thánh` « saint », `thiện`
  « bon, vertueux »).
- Voir `thánh` dans `bài kinh` ci-dessous. Le mot `thiện` introduit la
  moralité : le lieu n'est pas seulement tabou, il est **bon**.

### `bảo vệ` — protéger, défendre

- **Sino-vietnamien `保` + `衛`** : `bảo` (« garder ») + `vệ` (« défendre »).
  Registre neutre à soutenu.
- `Ta ở đây để bảo vệ linh hồn của nó` = « je suis là pour en protéger l'âme ».
  S'ensuit une inversion : celui qui protège l'âme menace de prendre la tienne.

### `linh hồn` — l'âme

- **Sino-vietnamien `靈` + `魂`** : `linh` (« esprit, sacré ») + `hồn`
  (« âme »). Registre soutenu.
- Voir `cân đo linh hồn` (`campaign.disciples.md`) et `buộc trói linh hồn`
  (`campaign.soulbinder.md`) : l'âme est l'objet du litige dans toute la nuit.

### `chịu` — consentir, accepter

- **Purement vietnamien.** `chịu` = consentir à, supporter : `không chịu` =
  « refuser de ».
- `Nếu ngươi không chịu cầu nguyện` = « si tu ne consens pas à prier ». Ce n'est
  pas l'acte manqué qui est puni, mais le refus — le verbe souligne la volonté.
  Voir `chịu` dans `campaign.conjurer.md`.

### `cầu nguyện` — prier

- `cầu` : demander, supplier (**sino-vietnamien `求`**). `nguyện` : vœu,
  souhait (**sino-vietnamien `願`**).
- `cầu nguyện` = prier, adresser une demande au divin. Registre religieux
  orthodoxe, qui contraste avec le sang promis juste après.

### `bài kinh` — l'hymne, le psaume, la prière écrite

- `bài` : pièce, leçon, morceau (classificateur des textes ; **sino-vietnamien
  `排`**, « ordonner »). `kinh` : écriture, texte sacré (**sino-vietnamien
  `經`**).
- `bài kinh cuối cùng của ngươi` = « ta dernière hymne ». L'expression fait de
  la mort du joueur un texte liturgique : on ne le tue pas, on le **chante**.

### `cuối cùng` — dernier, final

- `cuối` : fin, extrémité (**purement vietnamien**). `cùng` : ensemble, jusqu'au
  bout (**purement vietnamien**).
- Le mot revient dans `Bài kinh cuối` au `lose` : le couple `message`/`lose`
  encadre toute la menace.

### `bằng máu` — avec du sang

- `bằng` : au moyen de, avec (instrumental, **purement vietnamien** — à
  distinguer du `bằng` « égal » de `công bằng`, `campaign.fairy_knight.md`).
  `máu` : sang (**purement vietnamien**).
- `viết … bằng máu` = « écrire avec du sang ». L'encre du prêtre est le corps du
  vaincu. Le `lose` reprend l'image sous forme de couleur (`bằng đỏ`).

---

## 2. `win` — la prière qu'on n'a pas écrite

> Ngươi không chịu quỳ, nên không có bài kinh nào được viết. Bóng tối thả ngươi ra - đi, không còn bị trói buộc.

*Tu n'as pas consenti à t'agenouiller, donc aucune hymne n'a été écrite. Les
ténèbres te relâchent — va, sans plus de liens.*

(`chịu` et `bài kinh` sont expliqués au § 1 ; `bằng đỏ` au § 3.)

### `nên` — donc, c'est pourquoi

- **Purement vietnamien.** `nên` = par conséquent (aussi « devoir, falloir »).
- `Ngươi không chịu quỳ, nên …` = « tu n'as pas voulu t'agenouiller, donc … ».
  La logique est présentée comme une déduction : la victoire du joueur annule la
  dette rituelle.

### `được viết` — est écrit (passif favorable)

- `được` : marqueur du passif **favorable** (**purement vietnamien**, voir
  `campaign.lightstave.md`). `viết` : écrire (**purement vietnamien**).
- `không có bài kinh nào được viết` = « aucune hymne n'a été écrite ». Le passif
  `được` présente la non-écriture comme une grâce, non comme un manque.

### `Bóng tối` — les ténèbres, l'ombre

- `bóng` : ombre portée (**purement vietnamien**). `tối` : sombre, la nuit
  (**purement vietnamien**).
- Voir `bóng tối mà nó kìm giữ` dans `campaign.lightstave.md` : c'est la même
  entité, ici personnifiée — elle **relâche** (`thả`).

### `thả` — relâcher, lâcher

- **Purement vietnamien.** `thả` = lâcher, libérer (un animal, une prise).
- `Bóng tối thả ngươi ra` = « les ténèbres te relâchent ». Verbe d'élevage ou de
  pêche : on te rend à la mer, vivant.

### `trói buộc` — lier, enchaîner

- `trói` : ligoter (**purement vietnamien**, voir `bị trói` dans
  `campaign.trappers.md`). `buộc` : attacher, obliger (**purement vietnamien**,
  voir `buộc trói` dans `campaign.soulbinder.md`).
- `không còn bị trói buộc` = « sans plus être enchaîné ». L'absence de liens est
  la définition de la liberté que Malachar accorde malgré lui.

---

## 3. `lose` — l'hymne en rouge

> Bài kinh cuối của ngươi đã viết xong, bằng đỏ như đã hứa. Cầu cho bài kinh lần sau xuôi nhịp hơn - quỳ lại không?

*Ta dernière hymne est écrite, en rouge comme promis. Puisses-tu scander mieux
la prochaine — tu t'agenouilles de nouveau ?*

(`bài kinh` et `bằng` sont expliqués au § 1.)

### `đã viết xong` — est achevée d'écrire

- `đã` : marqueur du passé accompli. `xong` : fini, terminé
  (**purement vietnamien**).
- `đã viết xong` = « a été écrite jusqu'au bout ». Le texte est un ouvrage livré,
  pas une menace en suspens.

### `bằng đỏ` — en rouge

- `đỏ` : rouge (**purement vietnamien**). Reprise de `bằng máu` (§ 1) : le sang
  a séché, il est devenu une couleur. La pudeur de l'ellipse rend l'image plus
  froide.

### `như đã hứa` — comme promis

- `như` : comme (**sino-vietnamien `如`**). `hứa` : promettre
  (**purement vietnamien**).
- Voir `campaign.battlemage.md` et `campaign.highpriest.md` : chaque adversaire
  tient parole à la lettre. La promesse est la seule éthique de ces duels.

### `Cầu cho` — prie pour que, plaise au ciel que

- `cầu` : demander, implorer (**sino-vietnamien `求`** ; voir `cầu nguyện`,
  § 1). `cho` : pour que (**purement vietnamien**).
- `Cầu cho bài kinh … xuôi nhịp hơn` = « puisses-tu scander mieux ». La
  formule est presque bienveillante — et c'est ce qui la rend cruelle.

### `xuôi nhịp` — couler, être bien scandé

- `xuôi` : suivre le fil, aller dans le sens du courant
  (**purement vietnamien**). `nhịp` : rythme, cadence, mesure
  (**purement vietnamien**).
- `xuôi nhịp` = « scander sans accroc ». Vocabulaire de la versification et de
  la musique : l'hymne doit bien sonner. La mort du joueur est jugée sur sa
  prosodie.

### `quỳ lại không?` — tu t'agenouilles de nouveau ?

- `quỳ` : s'agenouiller (voir l'impératif `Quỳ xuống`, § 1). `lại` : de nouveau.
  `không?` : question-tag final.
- La question rouvre la boucle du `message` : tout le rituel n'était qu'une mise
  en demeure. La politesse de la question est le dernier sarcasme.
