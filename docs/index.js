/* The landing page speaks the reader's language.

   The strings of the top of the page — the title, the tagline, the "About"
   story and the buttons — live here, on the site. The game has its own
   bundles (assets/i18n/<lang>.properties) for what it shows on the board and
   it never reads this file: the page and the game only share the name.

   The language is the one in the link (?lang=fr), then the one the reader
   picked here or on the grimoire page, then the one the browser asks for. */

const LANGS = [
  { code: 'en', name: 'English' },
  { code: 'fr', name: 'Français' },
  { code: 'es', name: 'Español' },
  { code: 'ru', name: 'Русский' },
  { code: 'vi', name: 'Tiếng Việt' }
];

const TEXTS = {
  en: {
    title: "The Mancer's echo",
    tag: 'A fantasy card duel game, small tribute to Spectromancer©.',
    about: 'About',
    story: 'The unity of the elements seems to have been replaced by an order you no longer understand. Unsure whether the cause lies only within you, you set off on a journey—a voyage to a fantastical land where you will encounter a community that lives with mages of various persuasions.',
    download: 'Download',
    grimoire: 'Grimoire',
    gallery: 'Gallery',
    language: 'Language',
    source: 'Source on GitHub',
    shot: 'Mancers duel board screenshot',
    description: 'Mancers: a duel card game of 42 houses, a campaign and AI opponents, written in OCaml.',
    listen: 'Listen to the About text',
    stop: 'Stop the audio'
  },
  fr: {
    title: "L'écho des manciens",
    tag: 'Un jeu de duel de cartes fantastique, petit hommage à Spectromancer©.',
    about: 'À propos',
    story: "L'unité des éléments semble avoir été remplacée par un ordre que vous ne comprenez plus. Vous demandant si la cause n'était pas qu'en vous, vous vous mettez en route — un voyage vers une terre fantastique où vous rencontrerez une communauté où l'on côtoie des mages de diverses obédiences.",
    download: 'Télécharger',
    grimoire: 'Grimoire',
    gallery: 'Galerie',
    language: 'Langue',
    source: 'Code source sur GitHub',
    shot: "Capture d'écran du plateau de duel de Mancers",
    description: 'Mancers : un jeu de cartes en duel de 42 maisons, une campagne et des adversaires IA, écrit en OCaml.',
    listen: 'Écouter le texte À propos',
    stop: "Arrêter l'audio"
  },
  es: {
    title: 'El eco del Mancer',
    tag: 'Un juego de duelo de cartas de fantasía, pequeño homenaje a Spectromancer©.',
    about: 'Acerca de',
    story: 'La unidad de los elementos parece haber sido reemplazada por un orden que ya no entiendes. Sin saber si la causa está en ti, te pones en camino: un viaje a una tierra fantástica donde encontrarás una comunidad que vive con magos de distintas tendencias.',
    download: 'Descargar',
    grimoire: 'Grimoire',
    gallery: 'Galería',
    language: 'Idioma',
    source: 'Código fuente en GitHub',
    shot: 'Captura de pantalla del tablero de duelo de Mancers',
    description: 'Mancers: un juego de cartas en duelo de 42 casas, una campaña y rivales IA, escrito en OCaml.',
    listen: 'Escuchar el texto Acerca de',
    stop: 'Detener el audio'
  },
  ru: {
    title: 'Эхо Мансера',
    tag: 'Фэнтезийная карточная игра-дуэль, маленькая дань Spectromancer©.',
    about: 'О игре',
    story: 'Единство стихий, по-видимому, уступило место порядку, которого вы больше не понимаете. Не зная, в вас ли причина, вы отправляетесь в путь — путешествие в фантастическую страну, где вы встретите общину, живущую с магами самых разных убеждений.',
    download: 'Скачать',
    grimoire: 'Гримуар',
    gallery: 'Галерея',
    language: 'Язык',
    source: 'Исходный код на GitHub',
    shot: 'Скриншот дуэльного стола Mancers',
    description: 'Mancers: карточная игра-дуэль с 42 домами, кампания и противники-ИИ, написанная на OCaml.',
    listen: 'Прослушать текст «О игре»',
    stop: 'Остановить звук'
  },
  vi: {
    title: 'Dư âm của pháp sư',
    tag: 'Một trò chơi thẻ bài giả tưởng — lời tri ân nhỏ dành cho Spectromancer©',
    about: 'Giới thiệu',
    story: 'Sự hài hòa giữa các yếu tố dường như đã nhường chỗ cho một trật tự mà bạn không còn hiểu nổi. Tự hỏi liệu nguyên nhân có phải nằm ở chính mình chăng, bạn quyết định lên đường — một hành trình đến vùng đất kỳ ảo, nơi bạn sẽ gặp một cộng đồng gồm đủ loại pháp sư thuộc nhiều khuynh hướng khác nhau cùng chung sống.',
    download: 'Tải về',
    grimoire: 'Sách pha chú',
    gallery: 'Thư viện',
    language: 'Ngôn ngữ',
    source: 'Mã nguồn trên GitHub',
    shot: 'Ảnh chụp bàn đấu Mancers',
    description: 'Mancers: trò chơi đấu bài với 42 lớp, một chiến dịch và các đối thủ AI, viết bằng OCaml.',
    listen: 'Nghe phần Giới thiệu',
    stop: 'Dừng âm thanh'
  }
};

const el = id => document.getElementById(id);
let LANG = 'en';

/* The About story has a Vietnamese voice-over (site.about in the bundle).
   The speaker button only shows for Vietnamese and stops when the reader
   switches language. */
const aboutAudio = el('about-audio');
const aboutSound = el('about-sound');

function aboutLabel() {
  const t = TEXTS[LANG] || TEXTS.en;
  const label = aboutAudio.paused ? t.listen : t.stop;
  aboutSound.setAttribute('aria-label', label);
  aboutSound.title = label;
}

function stopAboutAudio() {
  aboutAudio.pause();
  aboutAudio.currentTime = 0;
  aboutSound.classList.remove('playing');
  aboutLabel();
}

aboutSound.onclick = () => {
  if (aboutAudio.paused) {
    aboutAudio.play();
    aboutSound.classList.add('playing');
  } else {
    stopAboutAudio();
  }
  aboutLabel();
};
aboutAudio.onended = stopAboutAudio;

/* the texts of one language on the page */
function apply(code) {
  const t = TEXTS[code] || TEXTS.en;
  LANG = code;
  document.documentElement.lang = code;
  document.title = t.title + ' — Mancers';
  el('metadesc').setAttribute('content', t.description);
  el('title').textContent = t.title;
  el('tag').textContent = t.tag;
  el('about').textContent = t.about;
  el('story').textContent = t.story;
  el('download').textContent = t.download;
  el('grimtext').textContent = t.grimoire;
  el('gallerytext').textContent = t.gallery;
  /* the grimoire page speaks the same language */
  el('grimoire').setAttribute('href', 'cards.html?lang=' + code);
  el('gallery').setAttribute('href', 'gallery/?lang=' + code);
  el('github').title = t.source;
  el('github').setAttribute('aria-label', t.source);
  el('shot').alt = t.shot;
  const sel = el('lang');
  sel.value = code;
  sel.title = t.language;
  sel.setAttribute('aria-label', t.language);
  /* the story is spoken only in Vietnamese */
  const voiced = code === 'vi';
  aboutSound.hidden = !voiced;
  if (!voiced) stopAboutAudio();
  aboutLabel();
}

/* the language the reader picked: this page's own key, then the grimoire
   page's one, so a reader keeps the same language on both pages */
const STORE = 'mancer.lang';
const STORE_GRIMOIRE = 'grimoire.lang';

function remember(code) {
  try {
    localStorage.setItem(STORE, code);
    localStorage.setItem(STORE_GRIMOIRE, code);
  } catch (e) { }
}

function remembered() {
  try {
    return localStorage.getItem(STORE) || localStorage.getItem(STORE_GRIMOIRE);
  } catch (e) {
    return null;
  }
}

const known = code => LANGS.some(l => l.code === code);

function wantedLang() {
  const asked = new URLSearchParams(location.search).get('lang');
  if (asked && known(asked)) {
    remember(asked);
    return asked;
  }
  const saved = remembered();
  if (saved && known(saved)) return saved;
  const nav = (navigator.language || 'en').toLowerCase();
  const hit = LANGS.find(l => nav === l.code || nav.startsWith(l.code + '-'));
  return hit ? hit.code : 'en';
}

function drawLangMenu() {
  const sel = el('lang');
  sel.innerHTML = '';
  LANGS.forEach(l => {
    const o = document.createElement('option');
    o.value = l.code;
    o.textContent = l.name;
    sel.appendChild(o);
  });
  sel.hidden = false;
  sel.onchange = () => {
    remember(sel.value);
    apply(sel.value);
    history.replaceState(null, '', '?lang=' + sel.value);
  };
}

drawLangMenu();
apply(wantedLang());
