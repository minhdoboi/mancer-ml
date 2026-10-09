/* The language menu of the site's pages. A native <select> cannot show an
   image, so the pages mount this little picker instead: the button carries the
   flag of the language it is set to, and the list shows every language with
   its flag. The flags are inline SVG, so the menu needs no picture files.

   The pages keep their own LANGS and TEXTS; this file only draws the menu. */
(function () {
  var FLAGS = {
    en: '<svg viewBox="0 0 60 30" preserveAspectRatio="none" aria-hidden="true"><rect width="60" height="30" fill="#012169"/><path d="M0 0l60 30M60 0L0 30" stroke="#fff" stroke-width="6"/><path d="M0 0l60 30M60 0L0 30" stroke="#c8102e" stroke-width="3"/><path d="M30 0v30M0 15h60" stroke="#fff" stroke-width="10"/><path d="M30 0v30M0 15h60" stroke="#c8102e" stroke-width="6"/></svg>',
    fr: '<svg viewBox="0 0 3 2" preserveAspectRatio="none" aria-hidden="true"><rect width="3" height="2" fill="#fff"/><rect width="1" height="2" fill="#002395"/><rect x="2" width="1" height="2" fill="#ed2939"/></svg>',
    es: '<svg viewBox="0 0 3 2" preserveAspectRatio="none" aria-hidden="true"><rect width="3" height="2" fill="#aa151b"/><rect y="0.5" width="3" height="1" fill="#f1bf00"/></svg>',
    ru: '<svg viewBox="0 0 9 6" preserveAspectRatio="none" aria-hidden="true"><rect width="9" height="6" fill="#fff"/><rect y="2" width="9" height="2" fill="#0039a6"/><rect y="4" width="9" height="2" fill="#d52b1e"/></svg>',
    vi: '<svg viewBox="0 0 30 20" preserveAspectRatio="none" aria-hidden="true"><rect width="30" height="20" fill="#da251d"/><path d="M15 4l1.8 5.5H22.5l-4.6 3.4 1.8 5.5-4.7-3.4-4.7 3.4 1.8-5.5L7.5 9.5h5.7z" fill="#ff0"/></svg>'
  };

  function esc(s) {
    return String(s).replace(/[&<>"]/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c];
    });
  }

  function flag(code) {
    return FLAGS[code] || '';
  }

  function close(host) {
    host.classList.remove('open');
    var list = host.querySelector('.lang-list');
    var btn = host.querySelector('.lang-btn');
    if (list) list.hidden = true;
    if (btn) btn.setAttribute('aria-expanded', 'false');
  }

  /* rebuild the menu inside host: langs is the list of { code, name },
     current the code it is set to, label the word for "Language" in that
     language, and onPick(code) is called with the reader's choice */
  function render(host, opts) {
    if (!host) return;
    var langs = opts.langs || [];
    var cur = null;
    for (var i = 0; i < langs.length; i++) if (langs[i].code === opts.current) cur = langs[i];
    if (!cur) cur = langs[0] || { code: '', name: '' };

    var btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'lang-btn';
    btn.setAttribute('aria-haspopup', 'listbox');
    btn.setAttribute('aria-expanded', 'false');
    btn.setAttribute('aria-label', (opts.label || 'Language') + ': ' + cur.name);
    btn.title = opts.label || 'Language';
    btn.innerHTML = '<span class="lang-flag">' + flag(cur.code) + '</span>' +
      '<span class="lang-name">' + esc(cur.name) + '</span>' +
      '<span class="lang-caret" aria-hidden="true">\u25be</span>';

    var list = document.createElement('ul');
    list.className = 'lang-list';
    list.setAttribute('role', 'listbox');
    list.hidden = true;

    langs.forEach(function (l) {
      var li = document.createElement('li');
      li.className = 'lang-item' + (l.code === cur.code ? ' on' : '');
      li.setAttribute('role', 'option');
      li.setAttribute('aria-selected', l.code === cur.code ? 'true' : 'false');
      li.tabIndex = 0;
      li.innerHTML = '<span class="lang-flag">' + flag(l.code) + '</span>' +
        '<span class="lang-name">' + esc(l.name) + '</span>';
      function pick() { close(host); if (opts.onPick) opts.onPick(l.code); }
      li.addEventListener('click', pick);
      li.addEventListener('keydown', function (e) {
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); pick(); }
      });
      list.appendChild(li);
    });

    btn.addEventListener('click', function (e) {
      e.stopPropagation();
      if (list.hidden) {
        list.hidden = false;
        btn.setAttribute('aria-expanded', 'true');
        host.classList.add('open');
      } else {
        close(host);
      }
    });

    host.innerHTML = '';
    host.appendChild(btn);
    host.appendChild(list);
  }

  /* a click anywhere else, or Escape, closes the open menu */
  document.addEventListener('click', function () {
    var open = document.querySelector('.langpicker.open');
    if (open) close(open);
  });
  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Escape') return;
    var open = document.querySelector('.langpicker.open');
    if (open) close(open);
  });

  window.LangMenu = { render: render };
})();
