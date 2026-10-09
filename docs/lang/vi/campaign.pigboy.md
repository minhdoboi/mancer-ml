# Toby — `campaign.pigboy`

Fiche de langue pour le texte de la rencontre `pigboy` (chapitre *day*,
débloquée après le duel de `seasons` ; le nœud est une **rencontre**, non un
duel : `Campaign.npcs`, portrait `pigboy`, nom `Toby`). Toby n'a pas de maison :
il n'y a donc qu'un seul champ à étudier, `campaign.pigboy.message`, et pas de
`win`/`lose` ni de `message.warning` (`warning_key_of_node` ne rend rien pour
ce nœud). Le nœud ouvre le duel de `trappers` (`campaign.trappers.md`) : le
garçon qui fuit vers le marais mène droit au poseur de pièges qui referme la
forêt. Toby est un enfant de ferme — le gardien de cochons du portrait — élevé
entre un père en colère et un marais où il va ruminer sa tristesse. Son texte
est un aveu murmuré, presque une excuse.

**Ton.** Toby parle de lui avec `cháu`, le pronom du plus jeune devant un aîné
(« moi, l'enfant ») : c'est le seul personnage de la campagne à le faire, et ce
choix commande tout le message. Les maîtres du jour disent `ta` et `ngươi`
(`campaign.seasons.md`, `campaign.trappers.md`) ; l'autre enfant de la campagne,
le gamin élémentaire, dit `tao`, le « je » le plus familier et le plus insolent
(`campaign.elementalist.md`). Toby, lui, se range *sous* l'adulte sans jamais
l'interpeller : pas de `bạn`, pas de `ngươi`, aucun pronom de deuxième personne.
Le message n'est adressé à personne en particulier — ou plutôt il l'est à
l'adulte silencieux que le joueur incarne, celui devant qui l'enfant s'excuse
d'exister là. Tout y est atténué : `có lẽ` (« peut-être »), `ở đâu đó` (« quelque
part »), `một lát` (« un moment »). Là où les seigneurs du jour prennent la
parole, Toby ne demande que l'asile d'un instant.

---

## 1. `message` — la ferme, le marais, la colère du père

> Cháu sống gần đây, ở một nông trại. Khi buồn, cháu thích đi dạo quanh đầm lầy. Dạo này cháu hay giận... hay có lẽ vì bố cháu giận vì cháu buồn. Cháu không nên ở đây. Bố sẽ giận. Cháu cần nấp ở đâu đó quanh đây một lát.

*J'habite tout près, dans une ferme. Quand je suis triste, j'aime me promener
autour du marais. Ces temps-ci je me mets souvent en colère... ou peut-être
parce que mon père est en colère que je sois triste. Je ne devrais pas être ici.
Papa va être en colère. Il faut que je me cache quelque part par ici un moment.*

### `Cháu` — « moi, l'enfant »

- Pronom de première personne **purement vietnamien**. À l'origine il désigne le
  petit-enfant, le neveu ou la nièce : celui qui parle se situe dans la
  génération du dessous. Adressé à un aîné — parent, voisin, inconnu plus vieux —
  il marque à la fois la jeunesse, la déférence et l'appartenance à la famille.
- Répété cinq fois, il fait de chaque phrase un pas de côté : Toby ne dit jamais
  « je » en maître, il dit « l'enfant ». Le joueur est ainsi institué en adulte,
  en oncle, en témoin plus vieux — sans qu'aucun mot ne le nomme.
- Les maîtres du jour disent `ta` / `ngươi` (voir `campaign.trappers.md`,
  `campaign.seasons.md`) ; l'enfant élémentaire dit `tao`
  (`campaign.elementalist.md`). `cháu` est le pôle exactement opposé de `tao` :
  le même âge, la politesse inverse.

### `sống gần đây` — vivre tout près

- `sống` : vivre (**purement vietnamien**). `gần` : proche, près (native).
  `đây` : ici (native).
- Attention au piège : `gần đây` veut d'ordinaire dire « récemment ». C'est
  `sống` qui tranche pour l'espace — « j'habite tout près ». Le garçon délimite
  d'emblée un petit cercle : la ferme, le marais, et ce coin-ci.

### `ở một nông trại` — dans une ferme

- `ở` : se trouver à, demeurer (**purement vietnamien**). `một` : un.
- `nông` 農 (« agriculture ») + `trại` 寨 (« camp, domaine »), **sino-vietnamien** :
  la ferme, au sens neutre et moderne (le doublet administratif est `nông
  trường` 農場). C'est le seul mot « savant » qui sorte de la bouche de l'enfant,
  une petite formalité de vocabulaire qui tranche avec le tendre `bố` du même
  paragraphe.

### `Khi buồn` — quand je suis triste

- `khi` : quand, lorsque (**purement vietnamien**). `buồn` : triste (native) —
  le mot couvre la tristesse, l'ennui et le vague à l'âme.
- Voir `đau buồn` de la fée Liriel (`campaign.faerie_druid.md`), `Ta buồn biết
  bao` du semeur (`campaign.sower.md` § 2) et `chúng buồn` de l'enfant
  élémentaire (`campaign.elementalist.md` § 1). Chez Toby, la tristesse n'est pas
  un deuil ni une plainte : c'est un état qui appelle la promenade.

### `thích` — aimer (bien)

- **Purement vietnamien.** Plus léger que `yêu` (aimer d'amour) : `thích` dit le
  goût, l'envie. Voir `thích chơi` de l'enfant élémentaire
  (`campaign.elementalist.md` § 1).

### `đi dạo quanh đầm lầy` — se promener autour du marais

- `đi` : aller (native). `dạo` : flâner, se promener sans but (native) ; `đi
  dạo` est une locution figée. `quanh` : autour (native).
- `đầm` : étang, marais (**purement vietnamien**). `lầy` : fangeux, bourbeux
  (native). `đầm lầy` = le marais. Voir le semeur (`campaign.sower.md` § 1), où
  le marais devient un personnage patient : ici c'est le refuge du garçon, le
  lieu où sa tristesse peut circuler.

### `Dạo này` — ces temps-ci

- `dạo` : laps de temps récent (native) + `này` : ce …-ci. **Purement
  vietnamien.**
- Le mot reprend le `dạo` de `đi dạo` : « la promenade » et « ces jours-ci »
  sont un seul vocable. Toby relie ainsi son errance et son humeur — il se
  promène parce que c'est ce temps-là de sa vie.

### `hay giận` — souvent en colère

- `hay` : souvent, fréquemment (**purement vietnamien**) — mais aussi, trois
  mots plus loin, « ou » : voir ci-dessous.
- `giận` : être en colère contre quelqu'un, en vouloir à (native). C'est une
  colère relationnelle, tournée vers un autre — ici le père, puis lui-même. Voir
  `cơn giận` (`campaign.barbarians.md` § 3).

### `hay có lẽ` — ou peut-être

- Deuxième `hay`, cette fois conjonction : « ou ». `có` : avoir, il y a ; `lẽ` :
  raison, droit (native) ; `có lẽ` = « il est possible, peut-être ».
- La répétition du même `hay` est presque un bégaiement : la phrase trébuche de
  « souvent » à « ou ». Le garçon ne sait pas s'il est en colère ou s'il ne fait
  que répercuter la colère d'un autre.

### `vì bố cháu giận vì cháu buồn` — parce que mon père est en colère parce que je suis triste

- `vì` : parce que, à cause de (**purement vietnamien**), répété deux fois. La
  cause tourne en rond : il est triste, donc le père est en colère, donc il est
  en colère. Le cercle est l'image même du foyer.
- Le double `vì` encadre `bố cháu` : le père est au centre de la phrase comme il
  est au centre de la vie du garçon.

### `bố` — papa

- **Purement vietnamien**, dialecte du Nord : `bố` est le père dit
  familièrement. Registre intime, oral ; les variantes sont `ba` (Sud), `cha`
  (plus soutenu, parfois local) et `thầy` (certaines régions).
- `bố cháu` = « mon père » (le `cháu` fait office de possessif). Puis le mot
  revient seul, sans possessif : `Bố sẽ giận`. Le père devient alors un nom
  absolu, la seule autre personne nommée dans tout le message.

### `Cháu không nên ở đây` — je ne devrais pas être ici

- `không` : négation (native). `nên` : devoir, être censé (**purement
  vietnamien**, ici verbe modal). `không nên` = « il ne faut pas, je ne devrais
  pas ».
- Le garçon ne dit pas qu'il est interdit, mais qu'il *devrait* ne pas être là :
  la faute est déjà intériorisée.

### `Bố sẽ giận` — Papa va être en colère

- `sẽ` : marque du futur, **purement vietnamien**. `Bố` est nu, sans `cháu` ni
  possessif : la phrase n'a que trois mots et le père y occupe tout.
- La colère n'est plus une cause circulaire, elle est une **menace datée** :
  elle viendra dès qu'il sera retrouvé.

### `cần` — avoir besoin de

- **Purement vietnamien.** `cần` = avoir besoin, être nécessaire. Plus
  physique et plus pressant qu'un simple désir : le garçon ne veut pas se
  cacher, il le *doit*.

### `nấp` — se tapir, se cacher

- **Purement vietnamien.** `nấp` = se cacher en se baissant, se plaquer derrière
  un obstacle — le geste d'un enfant ou d'un animal qui se fait petit. Rien de
  stratégique : on ne fuit pas, on se dérobe au regard.
- À distinguer de `trốn` (fuir, se dérober à une poursuite), celui du lieur de
  l'âme (`campaign.soulbinder.md` § 1, `chạy trốn`). Le choix de `nấp` dit
  l'âge : Toby n'échappe pas à un ennemi, il s'installe dans un trou.

### `ở đâu đó` — quelque part

- `ở` : à (native). `đâu` : où (native). `đó` : là (native). `ở đâu đó` =
  « quelque part, n'importe où par là ».
- Le flou est voulu : Toby ne sait pas où, il demande seulement un coin. Voir
  `điều gì đó` (`campaign.beaver.md`, `campaign.intro.md`) pour la même
  indétermination native.

### `quanh đây` — par ici

- `quanh` : autour + `đây` : ici. **Purement vietnamien.** Troisième occurrence
  du champ « cercle » (`gần đây`, `quanh đầm lầy`) : le monde du garçon tient
  dans quelques mètres.

### `một lát` — un moment

- `một` : un. `lát` : tranche, tranche de temps (**purement vietnamien**) ;
  `một lát` = « un instant, le temps d'une tranche ».
- Après `có lẽ` et `ở đâu đó`, c'est la troisième atténuation : il ne réclame
  qu'un sursis minuscule, et la formule garde la porte ouverte à un départ
  immédiat.

### `...` — l'hésitation

- Le bundle écrit `hay giận...` : les points de suspension remplacent le tiret
  du texte anglais. La phrase se suspend au bord de l'aveu — le garçon ne va pas
  jusqu'au bout de sa pensée sur sa propre colère. C'est aussi la scansion du
  souffle d'un enfant qui parle vite.

### Le cercle et le miroir

- Le message est bâti sur une **boucle** : la colère du fils, la colère du père,
  la tristesse du fils. Les phrases finales, très courtes et toutes commencées
  par `cháu` ou `Bố`, se répondent comme deux battements : `Cháu không nên ở
  đây. Bố sẽ giận.` Puis le dernier souffle, plus long, resserre l'espace
  (`ở đâu đó quanh đây`) et le temps (`một lát`).
- Le marais et la ferme encadrent le texte — dehors, dedans. Toby est déjà
  passé de l'un à l'autre : il ne raconte pas sa fuite, il la termine.
