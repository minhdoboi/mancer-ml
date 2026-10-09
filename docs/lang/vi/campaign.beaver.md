# Castor — `campaign.beaver`

Fiche de langue pour le texte de la rencontre `beaver` (chapitre *day*,
débloquée après `junk` ; le nœud est une **rencontre**, non un duel :
`Campaign.npcs`, portrait `beaver`, nom `Castor`). Castor est un éclaireur : il
flaire une magie résiduelle dans une forêt déjà traversée, et son avertissement
conditionnel désigne la prêtresse au dieu inventé, Maela
(`campaign.paganist.md`), que le nœud débloque avec `spiritshaman`. Comme
celui de Clover, ce `message.warning` n'apparaît qu'au joueur qui n'a pas
encore de classe de premier rang (`Campaign.meeting_warning_key`).

**Ton.** Comme Clover (`campaign.rabbit.md`), Castor s'adresse au joueur avec
`bạn`, la deuxième personne neutre et amicale. Aucune autorité, aucune menace :
un compagnon de route qui partage une sensation. Sa question finale
(`Bạn có cảm nhận được không?`) fait du joueur un témoin plutôt qu'un
adversaire.

---

## 1. `message` — la magie qui s'efface

> Bạn đang đi qua một khu rừng kỳ lạ. Ở đây có điều gì đó mang ma thuật, dù giờ có lẽ chỉ còn rất mờ nhạt. Bạn có cảm nhận được không?

*Tu traverses une forêt étrange. Il y a ici quelque chose qui porte de la magie,
quoique peut-être il n'en reste à présent que très peu. Le sens-tu ?*

### `Bạn đang đi qua` — tu es en train de traverser

- `bạn` : toi (native, « ami »). `đang` : marque l'action en cours
  (**purement vietnamien**). `đi qua` : traverser (`đi` aller + `qua` passer,
  natifs).
- L'imparfait vivant de `đang` installe le joueur dans le mouvement : il ne
  regarde pas la forêt, il la parcourt.

### `một khu rừng kỳ lạ` — une forêt étrange

- `khu` : zone, secteur (**sino-vietnamien `區`**). `rừng` : forêt
  (**purement vietnamien**). `kỳ` 奇 (« étrange ») + `lạ` : inconnu, bizarre
  (native), **sino-vietnamien + natif**.
- Même syntagme que `khu rừng này` du soulbinder (`campaign.soulbinder.md`) et
  de la fée sylvestre (`campaign.faerie_druid.md`) : le bois revient comme un
  lieu qui garde ce qu'on y laisse.

### `Ở đây có điều gì đó` — il y a ici quelque chose

- `ở` : se trouver à (**purement vietnamien**) + `đây` : ici (native).
  `điều` : chose abstraite (native). `gì đó` : quelque chose (`gì` « quoi » +
  `đó` « là »).
- Voir `điều gì đó` dans `campaign.intro.md` et `campaign.soulbinder.md` : la
  formule du pressentiment, la chose qu'on ne peut pas encore nommer.

### `mang ma thuật` — qui porte de la magie

- `mang` : porter, transporter (**purement vietnamien**). `ma` 魔 + `thuật`
  術, **sino-vietnamien `魔術`** : la magie (voir `campaign.technomage.md`).
- La magie est ici une **charge** que le lieu transporte, non un acte : quelque
  chose l'a déposée, et elle reste.

### `dù giờ` — quoique maintenant

- `dù` : bien que, quoique (**purement vietnamien**). `giờ` : maintenant
  (native).
- `dù` concède sans renoncer : la magie est bien là, seulement affaiblie.

### `chỉ còn rất mờ nhạt` — il n'en reste que très peu de chose

- `chỉ` : seulement (**purement vietnamien**) + `còn` : rester (native).
  `rất` : très. `mờ` : flou, voilé (native, voir `campaign.lightstave.md`,
  `mờ đi`). `nhạt` : pâle, affadi (native).
- `mờ nhạt` redouble l'effacement : une trace presque dissoute. Ce qui fut
  magie n'est plus qu'une odeur.

### `cảm nhận được không?` — le sens-tu ?

- `cảm` 感 + `nhận` 認, **sino-vietnamien** : percevoir, ressentir (voir
  `campaign.emotion.md`). `được` : pouvoir (résultat). `không` : marque
  d'interrogation en fin de phrase.
- La question n'attend pas d'information mais une **confirmation** : Castor
  sait déjà, il vérifie que le joueur partage sa sensibilité. Voir aussi
  `giọng nói` comme preuve d'existence (`campaign.zen.md`).

---

## 2. `message.warning` — la femme qui invente son dieu

> Trong rừng, có thể bạn sẽ gặp một người phụ nữ. Bà ta hơi điên loạn. Bà ta có thể tìm cách bẻ cong tâm trí bạn. Bà ta bảo người ta hiến tế cho một vị thần do bà ta bịa ra — và những giọng nói bà ta nghe được bảo bà ta rằng bà ta là tôi tớ của ông ta.

*Dans la forêt, tu pourrais rencontrer une femme. Elle est un peu dérangée.
Elle pourrait chercher à plier ton esprit. Elle demande aux gens de sacrifier à
un dieu qu'elle a inventé — et les voix qu'elle entend lui disent qu'elle est la
servante de celui-ci.*

### `Trong rừng` — dans la forêt

- `trong` : dans, à l'intérieur (**purement vietnamien**). `rừng` : forêt.
- Le passage de `khu rừng` (§ 1) à `trong rừng` resserre l'espace : on entre
  dans le bois, et c'est là que se tient la femme.

### `một người phụ nữ` — une femme

- `người` : classificateur des personnes (**purement vietnamien**). `phụ` 婦 +
  `nữ` 女, **sino-vietnamien `婦女`** : la femme, registre neutre et quelque peu
  soutenu.
- Castor ne la nomme pas — ni Maela ni prêtresse. Le mot générique la maintient
  à distance, comme un danger qu'on décrit sans l'appeler.

### `hơi điên loạn` — un peu dérangée

- `hơi` : un peu, légèrement (**purement vietnamien**). `điên` : fou (native).
  `loạn` : en désordre, en révolte (**sino-vietnamien `亂`**).
- `hơi` atténue, mais `loạn` aggrave : « un peu folle » cache un désordre
  profond. L'avertissement n'est pas une insulte, c'est un diagnostic prudent.

### `tìm cách bẻ cong tâm trí bạn` — chercher à plier ton esprit

- `tìm cách` : chercher un moyen (`tìm` native + `cách` **sino-vietnamien
  `格`**, « manière »). `bẻ` : plier, casser (**purement vietnamien**) ;
  `cong` : courber (native). `tâm` 心 + `trí` 智, **sino-vietnamien `心智`** :
  l'esprit, le for intérieur.
- `bẻ cong` est manuel et brutal : l'esprit n'est pas convaincu mais **tordu**,
  comme une branche. Verbe physique pour une violence mentale, le propre d'un
  esprit qui se croit inspiré.

### `bảo người ta hiến tế` — demander aux gens de sacrifier

- `bảo` : dire, demander (**purement vietnamien**). `người ta` : les gens, on
  (**purement vietnamien**, voir § 1). `hiến` 獻 + `tế` 祭, **sino-vietnamien
  `獻祭`** : offrir en sacrifice.
- `hiến tế` est le mot liturgique, celui de Maela (`lễ vật`, `sự hy sinh`,
  `campaign.paganist.md`) : Castor rapporte la cérémonie sans la comprendre.

### `một vị thần do bà ta bịa ra` — un dieu qu'elle a inventé

- `vị` : classificateur honorifique (**sino-vietnamien `位`**, voir
  `campaign.rabbit.md`). `thần` : dieu (**sino-vietnamien `神`**, voir
  `campaign.paganist.md`). `do` : par, de la part de (native). `bịa` :
  inventer, fabriquer de toutes pièces (**purement vietnamien**) + `ra`.
- Toute la phrase est un verdict : le dieu est honoré (`vị`) et faux (`bịa`).
  `bịa ra` ne laisse aucune place au doute — c'est une invention, pas une
  révélation.

### `những giọng nói bà ta nghe được` — les voix qu'elle entend

- `giọng` : voix, timbre (**purement vietnamien**) + `nói` : parler (native).
  `nghe được` : entendre, percevoir (`nghe` native + `được` résultat).
- Voir `giọng nói xa lạ` (`campaign.night.md`) et `tìm lại giọng nói`
  (`campaign.zen.md`) : dans cette campagne, la voix est la première preuve
  qu'un être existe. Ici elle est la preuve qu'un dieu **n'existe pas** — ce sont
  les propres oreilles de la femme qui parlent.

### `tôi tớ của ông ta` — sa servante

- `tôi` : serviteur, domestique ; ancien pronom de modestie, celui du serviteur
  devant son maître ; `tớ` : valet (**purement vietnamien**). `tôi tớ` =
  domesticité, les gens de maison. `của ông ta` : de lui (`ông` honorifique
  masculin + `ta`).
- Le mot est social, presque ménager : elle n'est pas la prêtresse d'un dieu
  mais sa **domestique**. Et `ông ta`, le dieu, est désigné comme un homme
  ordinaire — la chute fait retomber la divinité dans la cuisine.
