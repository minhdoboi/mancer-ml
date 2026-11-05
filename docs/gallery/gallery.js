/* The gallery page: it draws the pictures listed in photos.json — the manifest
   scripts/gallery.sh writes from the files in this folder — ten at a time, and
   adds the rest as the reader scrolls.

   Like the landing page, the strings of the page live here, on the site; the
   game never reads them. The language is the one in the link (?lang=fr), the
   one the reader picked on any page, then the one the browser asks for. */

const LANGS = [
  { code: 'en', name: 'English' },
  { code: 'fr', name: 'Français' },
  { code: 'es', name: 'Español' },
  { code: 'ru', name: 'Русский' },
  { code: 'vi', name: 'Tiếng Việt' }
];

const TEXTS = {
  en: { title: 'Gallery', back: 'Back', language: 'Language', photo: 'Picture', none: 'No picture to show.',
    prev: 'Previous picture', next: 'Next picture', description: 'A gallery of Mancers artwork.' },
  fr: { title: 'Galerie', back: 'Retour', language: 'Langue', photo: 'Image', none: 'Aucune image à afficher.',
    prev: 'Image précédente', next: 'Image suivante', description: 'Une galerie des illustrations de Mancers.' },
  es: { title: 'Galería', back: 'Volver', language: 'Idioma', photo: 'Imagen', none: 'No hay imágenes que mostrar.',
    prev: 'Imagen anterior', next: 'Imagen siguiente', description: 'Una galería del arte de Mancers.' },
  ru: { title: 'Галерея', back: 'Назад', language: 'Язык', photo: 'Изображение', none: 'Нет изображений для показа.',
    prev: 'Предыдущее изображение', next: 'Следующее изображение', description: 'Галерея иллюстраций Mancers.' },
  vi: { title: 'Thư viện', back: 'Quay lại', language: 'Ngôn ngữ', photo: 'Hình ảnh', none: 'Không có hình ảnh để hiển thị.',
    prev: 'Hình trước', next: 'Hình sau', description: 'Thư viện hình ảnh của Mancers.' }
};

/* how many pictures are added each time the reader reaches the bottom */
const PAGE = 10;

const el = id => document.getElementById(id);
const esc = s => s.replace(/[&<>"]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));

let LANG = 'en';
let PHOTOS = [];
let SHOWN = 0;
let INDEX = 0;

/* the texts of one language on the page */
function apply(code) {
  const t = TEXTS[code] || TEXTS.en;
  LANG = code;
  document.documentElement.lang = code;
  document.title = t.title + ' — Mancers';
  const meta = el('metadesc');
  if (meta) meta.setAttribute('content', t.description);
  el('title').textContent = t.title;
  el('back').textContent = t.back;
  el('langlabel').textContent = t.language;
  el('back').parentElement.setAttribute('href', '../index.html?lang=' + code);
  const sel = el('lang');
  sel.value = code;
  sel.title = t.language;
  sel.setAttribute('aria-label', t.language);
  el('prev').setAttribute('aria-label', t.prev);
  el('next').setAttribute('aria-label', t.next);
}

/* the language the reader picked: the same keys the other pages use, so the
   choice follows the reader from one page to the next */
const STORE = 'mancer.lang';
const STORE_GRIMOIRE = 'grimoire.lang';

function remember(code) {
  try {
    localStorage.setItem(STORE, code);
    localStorage.setItem(STORE_GRIMOIRE, code);
  } catch (e) { }
}

const known = code => LANGS.some(l => l.code === code);

function wantedLang() {
  const asked = new URLSearchParams(location.search).get('lang');
  if (asked && known(asked)) { remember(asked); return asked; }
  let saved = null;
  try { saved = localStorage.getItem(STORE) || localStorage.getItem(STORE_GRIMOIRE); } catch (e) { }
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
  sel.onchange = () => {
    remember(sel.value);
    apply(sel.value);
    history.replaceState(null, '', '?lang=' + sel.value);
  };
}

/* one picture in the grid; the name from the manifest is URL-encoded, since a
   file may carry a space or an accent */
function tile(name) {
  const a = document.createElement('button');
  a.className = 'tile';
  a.type = 'button';
  const img = document.createElement('img');
  img.loading = 'lazy';
  img.alt = name.replace(/\.[^.]+$/, '');
  img.className = 'pending';
  img.addEventListener('load', () => img.classList.remove('pending'));
  img.addEventListener('error', () => img.classList.add('broken'));
  img.src = encodeURIComponent(name);
  a.appendChild(img);
  a.addEventListener('click', () => openLightbox(name));
  return a;
}

/* add the next ten pictures */
function more() {
  if (SHOWN >= PHOTOS.length) return;
  const grid = el('grid');
  PHOTOS.slice(SHOWN, SHOWN + PAGE).forEach(name => grid.appendChild(tile(name)));
  SHOWN = Math.min(SHOWN + PAGE, PHOTOS.length);
}

/* When the sentinel is in view there is room for another row, so add the next
   ten. A scroll listener is enough: it also covers the reader who lands at
   the bottom of a short first page. */
function check() {
  if (SHOWN >= PHOTOS.length) return;
  const s = el('sentinel');
  if (s.getBoundingClientRect().top < (window.innerHeight || document.documentElement.clientHeight)) more();
}

/* the picture the lightbox shows: the grid grows to it, so the tile is there
   when the reader goes back to the page */
function show(i) {
  if (PHOTOS.length === 0) return;
  INDEX = (i + PHOTOS.length) % PHOTOS.length;
  while (SHOWN <= INDEX && SHOWN < PHOTOS.length) more();
  const t = TEXTS[LANG] || TEXTS.en;
  const name = PHOTOS[INDEX];
  const img = el('lightimg');
  img.src = encodeURIComponent(name);
  img.alt = t.photo;
  el('counter').textContent = (INDEX + 1) + ' / ' + PHOTOS.length;
}

function openLightbox(name) {
  show(PHOTOS.indexOf(name));
  el('lightbox').classList.add('on');
}

function closeLightbox() {
  el('lightbox').classList.remove('on');
}

function message(text) {
  const p = el('loading');
  p.innerHTML = text;
  p.style.display = 'block';
}

async function load() {
  const res = await fetch('photos.json');
  if (!res.ok) throw new Error(res.status + ' ' + res.statusText);
  PHOTOS = await res.json();
  if (!Array.isArray(PHOTOS) || PHOTOS.length === 0) {
    message(esc((TEXTS[LANG] || TEXTS.en).none));
    return;
  }
  el('loading').style.display = 'none';
  more();
  check();
}

el('lightbox').addEventListener('click', closeLightbox);
/* the arrows sit over the box: a click on one must not close it */
el('prev').addEventListener('click', e => { e.stopPropagation(); show(INDEX - 1); });
el('next').addEventListener('click', e => { e.stopPropagation(); show(INDEX + 1); });
document.addEventListener('keydown', e => {
  if (e.key === 'Escape') closeLightbox();
  else if (el('lightbox').classList.contains('on') && e.key === 'ArrowLeft') show(INDEX - 1);
  else if (el('lightbox').classList.contains('on') && e.key === 'ArrowRight') show(INDEX + 1);
});
window.addEventListener('scroll', check, { passive: true });
window.addEventListener('resize', check);

drawLangMenu();
apply(wantedLang());
load().catch(err => {
  message('The gallery list lives in <code>photos.json</code>, next to this page. ' + esc(String(err)) +
    '<br>A page opened as a file cannot read it: serve the folder over http — ' +
    '<code>python3 -m http.server --directory docs</code>');
});
