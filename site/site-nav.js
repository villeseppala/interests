/* site-nav.js — builds the shared top nav bar (#site-nav) on the graph page,
 * the articles landing page, and every rendered article page.
 * Reads articles.json for the "Draft/final projects" dropdown list. */
(function () {
  // Article pages live one level down (/articles/<id>.html); everything else is at root.
  var prefix = /\/articles\//.test(location.pathname) ? '../' : '';
  // Inside the Shiny apps the nav destinations don't exist locally, so point links at the
  // deployed site (set window.SITE_NAV_BASE there). On the static site this stays relative.
  var linkBase = (typeof window.SITE_NAV_BASE === 'string' && window.SITE_NAV_BASE) || prefix;

  function esc(s) {
    return String(s == null ? '' : s).replace(/[&<>"]/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c];
    });
  }

  function render(articles) {
    var el = document.getElementById('site-nav');
    if (!el) return;
    var items;
    if (articles && articles.length) {
      items = articles.map(function (a) {
        return '<a href="' + linkBase + esc(a.url) + '">' + esc(a.title) + '</a>';
      }).join('');
    } else {
      items = '<span class="nav-menu-empty">No articles yet</span>';
    }
    el.innerHTML =
      '<a class="nav-brand" href="' + linkBase + 'index.html">Interests</a>' +
      '<div class="nav-dropdown">' +
        '<a href="' + linkBase + 'articles.html">Draft/final projects<span class="nav-caret">&#9660;</span></a>' +
        '<div class="nav-menu">' + items + '</div>' +
      '</div>';
  }

  function start() {
    // Manifest path can be overridden (e.g. author app serves www under app_www/).
    var manifest = (typeof window.SITE_NAV_MANIFEST === 'string' && window.SITE_NAV_MANIFEST) || (prefix + 'articles.json');
    fetch(manifest)
      .then(function (r) { return r.ok ? r.json() : []; })
      .then(function (list) { render(Array.isArray(list) ? list : (list.articles || [])); })
      .catch(function () { render([]); });
  }

  // In the Shiny apps this loads from <head>, before #site-nav exists — wait for the DOM.
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
