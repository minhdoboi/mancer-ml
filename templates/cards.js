/* The grimoire page: it reads the card list of one language and draws it.
   The list is one JSON per language (cards.json, cards.fr.json, ...), the
   menu in the header picks one, and the page redraws with its text. */

let DATA = null, SELECTED = null, QUERY = '', LANG = 'en';

/* the languages the page was built with, each with the file it lives in */
const LANGS = typeof PAGE_LANGS === 'undefined' ? [] : PAGE_LANGS;

const el = (tag, cls, html) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (html !== undefined) e.innerHTML = html;
  return e;
};
const esc = s => s.replace(/[&<>"]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c]));

/* the strings of the page, in the language of the list */
const t = key => (DATA && DATA.ui && DATA.ui[key] != null) ? DATA.ui[key] : key;
/* a string with its {0} and {1} filled in */
const fill = (tmpl, args) => args.reduce((s, v, i) => s.split('{' + i + '}').join(v), tmpl);

const ASSETS_URL = 'https://github.com/minhdoboi/mancer-assets';
const langFile = code => {
  const l = LANGS.find(l => l.code === code);
  return l && l.file ? l.file : (code === 'en' ? 'cards.json' : 'cards.' + code + '.json');
};

function cardEl(c) {
  const art = c.art
    ? `<img src="${c.art}" alt="${esc(c.name)}" loading="lazy" width="80" height="80">`
    : `<div class="noart">${esc(t('noart'))}</div>`;
  // the top of the card: the picture on the left, the facts on the right
  const type = c.spell ? t('spell') : t('creature');
  const stats = c.spell ? '' : `<span class="stat">${c.attack === null ? 'X' : c.attack} / ${c.life}</span>`;
  const cost = c.cost === 0 ? t('token') : c.cost;
  const a = el('article', 'card' + (c.spell ? ' spell' : ''));
  a.id = c.key;
  a.dataset.text = (c.name + ' ' + c.houseName + ' ' + c.text + ' ' + c.key).toLowerCase();
  a.innerHTML = `<div class="top">
      <div class="art">${art}</div>
      <div class="head">
        <h3>${esc(c.name)} <span class="cost">${cost}</span></h3>
        <p class="meta"><span class="type">${esc(type)}</span>${stats}</p>
      </div>
    </div>
    ${c.text ? `<p class="desc">${esc(c.text)}</p>` : ''}`;
  return a;
}

function houseEl(h) {
  const s = el('section', 'house');
  s.id = h.key;
  const creatures = h.cards.filter(c => !c.spell).length;
  const spells = h.cards.length - creatures;
  const summary = fill(t('summary'), [creatures, spells]);
  const credit = h.author ? esc(h.author) : h.cards.length;
  s.innerHTML = `<h2>${esc(h.name)} <span class="count">(${credit})</span></h2>
    ${h.blurb ? `<p class="blurb">${esc(h.blurb)}</p>` : ''}
    <p class="summary">${esc(summary)}</p>`;
  const grid = el('div', 'grid');
  h.cards.forEach(c => grid.appendChild(cardEl(c)));
  s.appendChild(grid);
  return s;
}

function drawNav() {
  const nav = el('nav', 'sidebar');
  const q = el('input');
  q.id = 'q';
  q.type = 'search';
  q.placeholder = t('search');
  q.value = QUERY;
  q.autocomplete = 'off';
  q.spellcheck = false;
  q.addEventListener('input', () => { QUERY = q.value; apply(); });
  nav.appendChild(q);
  const ul = el('ul');
  DATA.houses.forEach(h => {
    const li = el('li');
    const a = el('a', null, esc(h.name));
    a.href = '#' + h.key;
    a.addEventListener('click', e => {
      e.preventDefault();
      select(h.key);
      history.replaceState(null, '', location.search + '#' + h.key);
    });
    li.appendChild(a);
    ul.appendChild(li);
  });
  nav.appendChild(ul);
  return nav;
}

function drawLangMenu() {
  const sel = document.getElementById('lang');
  sel.innerHTML = '';
  LANGS.forEach(l => {
    const o = el('option', null, esc(l.name));
    o.value = l.code;
    o.selected = l.code === LANG;
    sel.appendChild(o);
  });
  sel.onchange = () => setLang(sel.value);
}

/* the parts of the page that carry text: the title, the menu, the footer */
function drawChrome() {
  const title = t('title');
  document.title = title + ' — Mancers';
  document.getElementById('title').textContent = title;
  document.getElementById('back').textContent = t('back');
  document.getElementById('langlabel').textContent = t('language');
  const meta = document.getElementById('metadesc');
  if (meta) meta.setAttribute('content', t('description'));
  document.getElementById('none').textContent = t('none');
  const file = langFile(LANG);
  const footer = fill(esc(t('footer')), [
    '<a href="' + ASSETS_URL + '">mancer-assets</a>',
    '<a href="' + file + '">' + file + '</a>'
  ]);
  document.getElementById('footer').innerHTML = footer;
}

function select(key) {
  SELECTED = key;
  apply();
  window.scrollTo({top: 0, behavior: 'smooth'});
}

function apply() {
  const terms = QUERY.toLowerCase().split(/\s+/).filter(Boolean);
  const searching = terms.length > 0;
  let shown = 0;
  DATA.houses.forEach(h => {
    const section = document.getElementById(h.key);
    // a search looks at every house; browsing shows the one that was picked
    const onShow = searching || h.key === SELECTED;
    let any = false;
    section.querySelectorAll('.card').forEach(c => {
      const hit = !searching || terms.every(t => c.dataset.text.includes(t));
      c.classList.toggle('hidden', !hit);
      if (hit) any = true;
    });
    if (onShow && any) shown += section.querySelectorAll('.card:not(.hidden)').length;
    section.classList.toggle('hidden', !any || !onShow);
  });
  document.getElementById('none').style.display = shown ? 'none' : 'block';
  document.querySelectorAll('.sidebar a').forEach(a => {
    a.classList.toggle('on', !searching && a.getAttribute('href') === '#' + SELECTED);
  });
}

async function load(code) {
  const file = langFile(code);
  const res = await fetch(file);
  if (!res.ok) throw new Error(res.status + ' ' + res.statusText);
  const data = await res.json();
  DATA = data;
  LANG = data.lang || code;
  document.documentElement.lang = LANG;
  const byHouse = {};
  DATA.cards.forEach(c => {
    if (!byHouse[c.house]) byHouse[c.house] = [];
    byHouse[c.house].push(c);
  });
  const names = {};
  DATA.houses.forEach(h => {
    names[h.key] = h.name;
    h.cards = byHouse[h.key] || [];
  });
  DATA.cards.forEach(c => { c.houseName = names[c.house] || ''; });
  const houses = document.getElementById('houses');
  houses.innerHTML = '';
  DATA.houses.forEach(h => houses.appendChild(houseEl(h)));
  const nav = document.getElementById('nav');
  nav.innerHTML = '';
  nav.appendChild(drawNav());
  drawLangMenu();
  drawChrome();
  document.getElementById('loading').style.display = 'none';
  // a link may point at a house: #fire
  const wanted = location.hash.slice(1);
  SELECTED = DATA.houses.some(h => h.key === wanted) ? wanted : DATA.houses[0].key;
  apply();
}

function known(code) {
  return LANGS.some(l => l.code === code);
}

function remember(code) {
  try { localStorage.setItem('grimoire.lang', code); } catch (e) { }
}

/* the language to open with: the one in the link, then the one the reader
   picked last, then the one the browser asks for */
function wantedLang() {
  const asked = new URLSearchParams(location.search).get('lang');
  if (asked && known(asked)) { remember(asked); return asked; }
  let saved = null;
  try { saved = localStorage.getItem('grimoire.lang'); } catch (e) { }
  if (saved && known(saved)) return saved;
  const nav = (navigator.language || 'en').toLowerCase();
  const hit = LANGS.find(l => nav === l.code || nav.startsWith(l.code + '-'));
  return hit ? hit.code : 'en';
}

async function setLang(code) {
  if (code === LANG || !known(code)) return;
  LANG = code;
  document.documentElement.lang = code;
  remember(code);
  history.replaceState(null, '', '?lang=' + code + location.hash);
  const loading = document.getElementById('loading');
  loading.textContent = t('loading');
  loading.style.display = 'block';
  await load(code);
}

load(wantedLang()).catch(err => {
  document.getElementById('loading').innerHTML =
    'The card list lives in <code>cards.json</code>, next to this page. ' + esc(String(err)) +
    '<br>A page opened as a file cannot read it: serve the folder over http — ' +
    '<code>python3 -m http.server --directory docs</code>';
});
