# Jiro — `campaign.fisherman`

Fiche de langue pour la rencontre `fisherman` (chapitre *night*, débloquée après
le duel de `zen` ; le nœud est une **rencontre**, non un duel : `Campaign.npcs`,
portrait `fisherman`, nom `Jiro`). Il ouvre sur le duel de `conjurer`. Jiro est
un pêcheur de la route nocturne : il n'a pas de maison et pas de carte à
défendre, seulement une image — la mer — pour dire ce que la route fait aux
mages. Son texte est une maxime, pas un défi.

**Ton.** Jiro ne s'adresse jamais au joueur : aucun `bạn`, aucun `ngươi`,
aucun `ta` ni `tôi`. Il parle au présent de vérité générale, comme on commente
le temps ou les marées, et décrit des tiers — d'abord `người` (neutre), puis
`kẻ` (distant, presque méprisant). Le seul mouvement du message est ce
glissement : les victimes sont d'abord des gens, puis une catégorie qu'on juge.
Le registre est celui du proverbe, avec par endroits un vocabulaire
**moderne** — `giá trị bản thân`, `ảo tưởng` — qui ne vient pas de la mer mais
de la psychologie.

---

## 1. `campaign.fisherman.message` — mesurer sa valeur, la perdre en mer

> Ở đây có không ít người đánh mất chính mình vì cố đo giá trị bản thân bằng cách tranh đua với những pháp sư quyền năng nhất. Những kẻ bị ảo tưởng này hút cạn có thể bỏ lại một phần bản thân mình phía sau, nhưng đòi lại nó thì cũng như câu cá ngoài biển khơi.

*Ici, il n'y a pas peu de gens qui se perdent eux-mêmes à force de vouloir
mesurer leur valeur en rivalisant avec les mages les plus puissants. Ceux que
cette illusion a vidés peuvent laisser derrière eux une part d'eux-mêmes, mais
la récupérer, c'est comme pêcher en haute mer.*

### `Ở đây` — ici

- `ở` : se trouver à (**purement vietnamien**) + `đây` : ici (natif).
- Même ouverture que Castor (`campaign.beaver.md`, `Ở đây có điều gì đó`) :
  la formule qui plante le lieu avant d'énoncer ce qu'on y trouve ; on la
  retrouve, sans le `Ở đây`, dans l'entrée en scène du voyageur
  (`campaign.intro.md`, `điều gì đó khiến bạn lo lắng`).

### `không ít người` — pas mal de gens

- `không` : négation (natif). `ít` : peu (**purement vietnamien**). `người` :
  classificateur des personnes (natif).
- `không ít` est une **litote** : « pas peu » veut dire « beaucoup, un bon
  nombre ». L'anglais l'a adouci en « some people » ; le vietnamien, lui,
  insiste sur la foule.

### `đánh mất chính mình` — se perdre soi-même

- `đánh mất` : perdre, égarer (`đánh` « frapper » + `mất` « perdre »,
  **purement vietnamien**). Décrit par le détail dans `campaign.darksider.md` :
  on perd une **chose**, par accident ou par faute, non une partie.
- `chính` : même, propre (**sino-vietnamien `正`**) + `mình` : soi-même
  (natif). `chính mình` = « son propre soi ».
- La réflexivité est le cœur du message : ce qu'on perd en se comparant, c'est
  **soi**.

### `vì cố` — à force de s'acharner

- `vì` : parce que, à cause de (**purement vietnamien**). `cố` : s'efforcer,
  s'acharner (natif).
- `cố` est le même mot que chez Kwame (`campaign.elder.md`, `cố xây dựng`) :
  l'effort n'est pas condamné en soi, c'est son objet qui épuise.

### `đo giá trị bản thân` — mesurer sa propre valeur

- `đo` : mesurer (**purement vietnamien**). `giá trị` : la valeur
  (**sino-vietnamien `價值`** : `giá` 價 « prix » + `trị` 值 « valeur »).
- `bản thân` : soi-même (**sino-vietnamien `本身`** : `bản` 本 « racine,
  propre » + `thân` 身 « corps, personne »). Même mot que chez le grand prêtre
  (`campaign.highpriest.md`, `Vá lại bản thân`).
- `đo giá trị bản thân` = « mesurer sa propre valeur ». Le verbe est
  d'arpenteur : on soumet l'estime de soi à une **mesure**, et c'est la mesure
  elle-même qui est fausse.

### `bằng cách` — au moyen de, en

- `bằng` : par, au moyen de (**purement vietnamien**). `cách` : manière
  (**sino-vietnamien `格`**, voir `campaign.beaver.md`, `tìm cách`).
- La locution introduit la **méthode** du suicide moral : comment on s'y prend
  pour se perdre.

### `tranh đua` — rivaliser, concourir

- `tranh` : disputer, se disputer (**sino-vietnamien `爭`**) + `đua` : faire
  la course, rivaliser (**purement vietnamien**).
- Le mot mêle l'ancien (`tranh`, la querelle) et le jeu (`đua`, la course) :
  une rivalité qui se prend pour un sport.

### `những pháp sư quyền năng nhất` — les mages les plus puissants

- `những` : marque du pluriel. `pháp sư` : mage (**sino-vietnamien `法師`** :
  `pháp` 法 « loi, magie » + `sư` 師 « maître ») — le mot neutre partagé par
  Kwame, le powermancer et l'épilogue (`campaign.elder.md`,
  `campaign.powermancer.md`, `campaign.epilogue.md`).
- `quyền năng` : la puissance (**sino-vietnamien `權能`**, voir
  `campaign.elder.md`) ; `nhất` : superlatif « le plus ».
- Le comparant n'est pas n'importe qui : se mesurer aux **plus puissants**,
  voilà la pente. Les mages que le joueur affrontera plus loin
  (`campaign.conjurer.md`) sont cette élite.

### `Những kẻ` — ceux, cette engeance

- `kẻ` : individu, type (**purement vietnamien**), souvent **péjoratif** ;
  il contraste avec `người` du début, neutre.
- Le passage de `người` à `kẻ` est le seul jugement du message : les victimes
  ne sont plus seulement nombreuses, elles sont devenues une espèce qu'on
  regarde de haut.

### `bị ảo tưởng này hút cạn` — que cette illusion a vidés

- `bị` : particule du passif, avec la connotation de **subir** (voir
  `campaign.trappers.md`, `bị trói`).
- `ảo tưởng` : l'illusion (**sino-vietnamien `幻想`** : `ảo` 幻 « illusion,
  mirage » + `tưởng` 想 « penser »). Le mot est presque clinique : ce n'est pas
  un mensonge d'autrui, c'est une croyance qu'on entretient.
- `hút cạn` : vider, drainer (`hút` : aspirer, sucer + `cạn` : à sec,
  **purement vietnamien**). Voir la même paire de ponction chez le powermancer
  (`campaign.powermancer.md`, `rút cạn`, `cạn`) et le vent
  (`campaign.wind.md`, `cạn`) : l'illusion n'égare pas, elle **aspire** et
  laisse à sec.

### `bỏ lại một phần bản thân mình phía sau` — laisser une part de soi derrière

- `bỏ lại` : laisser, abandonner (`bỏ` « quitter » + `lại` « en reste »,
  **purement vietnamien**). `phần` : part (**sino-vietnamien `分`**).
- `phía sau` : derrière, en arrière (`phía` : côté + `sau` : après,
  **purement vietnamien**).
- L'image est concrète : une **part** de soi déposée sur le bord de la route,
  comme un bagage. Elle fait écho à la route et au voyage de la campagne
  (`campaign.intro.md`, `campaign.elder.md`).

### `đòi lại nó` — la réclamer

- `đòi` : réclamer, exiger (**purement vietnamien**) + `lại` : restitution.
  `nó` : pronom neutre, « le/la » (ici la part perdue).
- `đòi` n'est pas « reprendre » mais **revendiquer** : il faut exiger de
  soi-même ce qu'on a abandonné, et le mot dit la difficulté.

### `cũng như câu cá ngoài biển khơi` — comme pêcher en haute mer

- `cũng như` : c'est aussi comme, équivaut à (`cũng` « aussi » + `như`
  « comme », natifs).
- `câu` : pêcher à la ligne (**purement vietnamien**) ; `cá` : poisson
  (natif). `câu cá` = l'acte même du pêcheur.
- `biển khơi` : la haute mer, le large (`biển` : mer, natif ; `khơi` : au
  large, purement vietnamien).
- La comparaison finale est la signature du personnage : un pêcheur ne peut
  rien promettre, seulement décrire l'ampleur de la mer. Récupérer la part
  perdue est incertain, sans fond et sans rive — exactement ce que le métier
  de Jiro lui apprend.
