/* The landing page speaks the reader's language.

   The strings of the top of the page — the title, the tagline, the "About"
   story and the buttons — live here, on the site. The game has its own
   bundles (assets/i18n/<lang>.properties) for what it shows on the board and
   it never reads this file: the page and the game only share the name.

   The language is Vietnamese by default, then the one in the link (?lang=fr),
   then the one the reader picked here or on the grimoire page. */

const LANGS = [
  { code: 'vi', name: 'Tiếng Việt' },
  { code: 'en', name: 'English' },
  { code: 'fr', name: 'Français' },
  { code: 'es', name: 'Español' },
  { code: 'ru', name: 'Русский' }
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
let LANG = 'vi';

/* The lower part of the page is not in the five languages above: it speaks
   Vietnamese for a Vietnamese reader and English for everyone else. The
   English is the markup the page shipped with; the Vietnamese is the literal
   below, so the two are easy to keep in step. */
const LOWER_EN = el('lowertext').innerHTML;
const LOWER_VI = `
    <p>Trò chơi là một bản chuyển thể do AI thực hiện từ một dự án cá nhân, kèm theo một số cải tiến (sửa lỗi, nâng cấp) và phần hình ảnh chung do AI tạo theo phong cách giả tưởng đen tối kiểu MTG. Các nhà nguyên tố gốc đã được thay bằng những nhà khác để tôn trọng bản quyền. Nhưng thành thật mà nói, đây giống một bản làm lại hơn là một sáng tác nguyên bản. Chơi trò chơi bằng tiếng Việt kèm âm thanh sẽ hay hơn, nhưng bản dịch là do AI thực hiện vì tôi nói tiếng Việt không giỏi.</p>
    <p>Bạn nên chơi trò chơi gốc trước để hiểu rõ cơ chế của game.</p>
    <div class="grid">
      <div><h2>Các nhà do cộng đồng gợi ý</h2><p>Vampires, Shadowmage, Entomologist và nhiều nhà khác, mỗi nhà có sinh vật và phép thuật riêng.</p></div>
      <div><h2>Chiến dịch &amp; các trận đấu</h2><p>Hoàn thành chiến dịch, hoặc chọn một trận đấu với máy.</p></div>
      <div><h2>Đa ngôn ngữ</h2><p>Có bản dịch do AI thực hiện bằng nhiều thứ tiếng (Anh, Pháp, Tây Ban Nha, Nga, Việt).<br/>
        Có âm thanh và các bản giải thích ngôn ngữ trong kho mã nguồn để học tiếng Việt. (hãy nghe trước khi đọc, bạn cũng có thể không đọc để giữ lại sự mơ hồ và những câu hỏi)
      </p></div>
    </div>
    <h2>Ghi công</h2>
    <p>Bạn có thể ủng hộ trò chơi gốc bằng cách mua nó trên <a href="https://www.spectromancer.com/">Spectromancer.com</a>.
    Phần hình ảnh rất đẹp (âm nhạc và đồ họa). Có hướng dẫn, một chiến dịch thân thiện với người mới, chế độ nhiều người chơi, và các lớp được cân bằng và mang tính nguyên mẫu.</p>
    <p>Cảm ơn cộng đồng Spectromancer đã chia sẻ những gợi ý về lớp dành cho trò chơi gốc,
    và cảm ơn các công cụ AI đã giúp dự án này thành hiện thực. Tôi xin lỗi trước nếu tôi đã sử dụng sai ý tưởng của bất kỳ ai.</p>
    <div class="notes">
      <div>
        <p>
          <strong>Hạn chế:</strong>
          <ul>
            <li>Không phải mọi luật của trò chơi gốc đều được cài đặt (luật 1%, một số luật về bộ bài).</li>
            <li>Vẫn còn một số lỗi.</li>
            <li>Thông tin ẩn không hoàn toàn bị che giấu với một số bot (thẻ tàng hình, hoặc meteomancer gọi nguyên tố với mistwalker chẳng hạn).</li>
            <li>Các lớp chưa được cân bằng tốt.</li>
            <li>Bot có thể hành xử rất ngớ ngẩn.</li>
          </ul>
        </p>
        <p>
          <strong>Mẹo:</strong>
          <ul><li>Dùng những lớp mạnh hơn: Người mới có thể chơi các lớp mạnh như Technomage, Paganist, High Priest hoặc Seasons.</li>
          <li>Tránh đấu với các lớp mạnh hơn: Bạn có thể cố tránh chúng trong chiến dịch nếu chơi một lớp yếu hơn. Có những thế đối kháng nhất định, nhưng dù vậy bạn gần như không có cơ hội thắng Technomage khi chơi Snowblood.</li>
          <li>Hiểu rõ cơ chế riêng của từng nhà: Một số không dễ nhưng có thể rất hiệu quả khi đã hiểu, ví dụ Antimancer, Conjurer.</li>
          <li>
            Bạn có thể điều chỉnh độ khó của các trận đấu bằng cách cho AI nhiều hay ít thời gian suy nghĩ. Điều này đã được áp dụng trong các chế độ khác nhau của chiến dịch. Chế độ Dễ cho AI ít thời gian suy nghĩ hơn.
          </li>
          </ul>
        </p>
      </div>
      <div>
        <strong>Cập nhật lớp:</strong>
        <ul>
          <li><strong>Blacksmith</strong> — <em>Pegasus</em>: đòn đánh cuối lượt giảm từ X+6 xuống X+4.</li>
        </ul>
        <ul>
          <li><strong>Zen</strong>, <strong>Junk</strong>, <strong>Snowblood</strong>, <strong>Warp</strong> — làm lại các lớp.</li>
        </ul>
      </div>
    </div>`;

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
  /* the lower part is Vietnamese or English, so its button follows it */
  el('gallerytext').textContent = code === 'vi' ? 'Thư viện' : 'Gallery';
  /* the grimoire page speaks the same language */
  el('grimoire').setAttribute('href', 'cards.html?lang=' + code);
  el('gallery').setAttribute('href', 'gallery/?lang=' + code);
  el('github').title = t.source;
  el('github').setAttribute('aria-label', t.source);
  el('shot').alt = t.shot;
  /* the lower part: Vietnamese or English, nothing else */
  el('lowertext').innerHTML = code === 'vi' ? LOWER_VI : LOWER_EN;
  drawLangMenu();
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
  /* Vietnamese is the site's default; the reader can pick another language */
  return 'vi';
}

function drawLangMenu() {
  LangMenu.render(el('lang'), {
    langs: LANGS,
    current: LANG,
    label: (TEXTS[LANG] || TEXTS.en).language,
    onPick: code => {
      remember(code);
      apply(code);
      history.replaceState(null, '', '?lang=' + code);
    }
  });
}

apply(wantedLang());
