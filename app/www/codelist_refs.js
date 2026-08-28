// Codelist references in cohort free text.
//
// The convention (see R/cohort_kinds.R) is that a cohort's text cites its
// codelist inline: "Influenza vaccination [cs_influenza_vaccine]". Two things
// make that convention usable rather than merely documented:
//
//   1. AUTOCOMPLETE: typing "[" in a marked textarea offers the codelists the
//      SAP defines, filtered as you type; Enter/Tab/click inserts "name]".
//   2. HIGHLIGHT: every "[...]" token is tinted like a selectize chip -- the
//      same look as the CDM-source tokens -- blue when the name resolves to a
//      codelist on the Codelists tab, amber when nothing defines it (the same
//      distinction codelist_reference_problems() reports on Review).
//
// The highlight is a backdrop DIV behind the textarea rendering the same text
// with tokens wrapped in <mark>; the textarea's own background goes transparent
// so the tint shows through under its (opaque) text. The backdrop copies the
// textarea's computed font/padding/border metrics, so the two layers line up.
//
// Textareas opt in with the .codelist-aware class on their container (see
// cohort_definition_ui). Cards are inserted dynamically, so a MutationObserver
// enhances new ones as they appear.
(function () {
  'use strict';

  var NAMES = [];

  // -- highlight --------------------------------------------------------------

  function esc(s) {
    return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  function renderBackdrop(ta) {
    var bd = ta._clrefBackdrop;
    if (!bd) return;
    var html = esc(ta.value).replace(/\[([^\[\]\n]*)\]/g, function (m, name) {
      var known = NAMES.indexOf(name.trim()) !== -1;
      return '<mark class="' + (known ? 'clref-known' : 'clref-unknown') + '">' +
             esc0(m) + '</mark>';
    });
    // A trailing newline collapses in HTML; the zero-width sentinel keeps the
    // backdrop the same height as the textarea's content so scrollTop stays in
    // step.
    bd.innerHTML = html + '\u200b';
    bd.scrollTop = ta.scrollTop;
  }

  // The token text was already escaped by the outer replace's input -- it came
  // out of esc() -- so it must NOT be escaped twice.
  function esc0(s) { return s; }

  function copyMetrics(ta, bd) {
    var cs = window.getComputedStyle(ta);
    ['fontFamily', 'fontSize', 'fontWeight', 'lineHeight', 'letterSpacing',
     'paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft',
     'borderTopWidth', 'borderRightWidth', 'borderBottomWidth', 'borderLeftWidth',
     'borderRadius', 'boxSizing'].forEach(function (p) { bd.style[p] = cs[p]; });
    bd.style.borderStyle = 'solid';
    bd.style.borderColor = 'transparent';
  }

  // -- autocomplete -----------------------------------------------------------

  // The open bracket the caret is inside, or null: the last "[" before the
  // caret with no "]" or newline between them.
  function bracketContext(ta) {
    var upto = ta.value.slice(0, ta.selectionStart);
    var open = upto.lastIndexOf('[');
    if (open === -1) return null;
    var inside = upto.slice(open + 1);
    if (/[\]\n]/.test(inside)) return null;
    return { start: open + 1, typed: inside };
  }

  function closeMenu(ta) {
    if (ta._clrefMenu) { ta._clrefMenu.remove(); ta._clrefMenu = null; }
  }

  function openMenu(ta) {
    var ctx = bracketContext(ta);
    if (!ctx || !NAMES.length) { closeMenu(ta); return; }
    var typed = ctx.typed.trim().toLowerCase();
    var hits = NAMES.filter(function (n) {
      return !typed || n.toLowerCase().indexOf(typed) !== -1;
    });
    if (!hits.length) { closeMenu(ta); return; }

    closeMenu(ta);
    var menu = document.createElement('div');
    menu.className = 'clref-menu';
    hits.slice(0, 8).forEach(function (n, i) {
      var it = document.createElement('button');
      it.type = 'button';
      it.className = 'clref-item' + (i === 0 ? ' active' : '');
      it.textContent = n;
      // mousedown, not click: click fires after the textarea's blur has already
      // torn the menu down.
      it.addEventListener('mousedown', function (e) {
        e.preventDefault();
        insert(ta, n);
      });
      menu.appendChild(it);
    });
    ta._clrefMenu = menu;
    ta._clrefWrap.appendChild(menu);
  }

  function insert(ta, name) {
    var ctx = bracketContext(ta);
    if (!ctx) { closeMenu(ta); return; }
    var after = ta.value.slice(ta.selectionStart);
    var closing = after.charAt(0) === ']' ? '' : ']';
    ta.value = ta.value.slice(0, ctx.start) + name + closing + after;
    var caret = ctx.start + name.length + 1;
    ta.setSelectionRange(caret, caret);
    closeMenu(ta);
    // Hand the programmatic edit to Shiny's input binding.
    ta.dispatchEvent(new Event('input', { bubbles: true }));
    ta.focus();
  }

  function moveActive(menu, dir) {
    var items = menu.querySelectorAll('.clref-item');
    var at = Array.prototype.findIndex.call(items, function (it) {
      return it.classList.contains('active');
    });
    items[at] && items[at].classList.remove('active');
    var next = (at + dir + items.length) % items.length;
    items[next].classList.add('active');
  }

  // -- wiring -----------------------------------------------------------------

  function enhance(ta) {
    if (ta._clrefWrap) return;
    var wrap = document.createElement('div');
    wrap.className = 'clref-wrap';
    ta.parentNode.insertBefore(wrap, ta);

    var bd = document.createElement('div');
    bd.className = 'clref-backdrop';
    wrap.appendChild(bd);
    wrap.appendChild(ta);

    ta._clrefWrap = wrap;
    ta._clrefBackdrop = bd;
    copyMetrics(ta, bd);
    renderBackdrop(ta);

    ta.addEventListener('input', function () { renderBackdrop(ta); openMenu(ta); });
    ta.addEventListener('scroll', function () { bd.scrollTop = ta.scrollTop; });
    ta.addEventListener('blur', function () { closeMenu(ta); });
    ta.addEventListener('click', function () { openMenu(ta); });
    ta.addEventListener('keydown', function (e) {
      var menu = ta._clrefMenu;
      if (!menu) return;
      if (e.key === 'ArrowDown') { e.preventDefault(); moveActive(menu, 1); }
      else if (e.key === 'ArrowUp') { e.preventDefault(); moveActive(menu, -1); }
      else if (e.key === 'Enter' || e.key === 'Tab') {
        var it = menu.querySelector('.clref-item.active');
        if (it) { e.preventDefault(); insert(ta, it.textContent); }
      } else if (e.key === 'Escape') { closeMenu(ta); }
    });
  }

  function enhanceAll(root) {
    (root.querySelectorAll ? root.querySelectorAll('.codelist-aware textarea') : [])
      .forEach(enhance);
  }

  function refreshAll() {
    document.querySelectorAll('.codelist-aware textarea').forEach(renderBackdrop);
  }

  document.addEventListener('DOMContentLoaded', function () {
    enhanceAll(document);
    // Cohort cards arrive via insertUI, so watch for them.
    new MutationObserver(function (muts) {
      muts.forEach(function (m) {
        m.addedNodes.forEach(function (n) {
          if (n.nodeType === 1) enhanceAll(n);
        });
      });
    }).observe(document.body, { childList: true, subtree: true });
  });

  if (window.Shiny) {
    Shiny.addCustomMessageHandler('codelist-names', function (names) {
      NAMES = (names || []).map(String);
      refreshAll();
    });
  }
})();
