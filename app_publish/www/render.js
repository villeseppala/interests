/* render.js — Shared Cytoscape rendering for concept map
 * 1:1 aspect ratio on desktop, vertical stretch on mobile.
 * Independent selected + hovered highlights,
 * no bottom row, thinner borders, no ID badges.
 * Mobile: draggable bottom sheet for descriptions + info, tap-only, pinch-zoom.
 * Author app: forceMobile preview mode via checkbox.
 */

/* ── Static-mode shim: makes Shiny.* calls work without a Shiny server ───── */
if (typeof Shiny === 'undefined') {
  window.Shiny = {
    _isStatic: true,
    _handlers: {},
    addCustomMessageHandler: function(name, fn) { this._handlers[name] = fn; },
    setInputValue: function(name, value) {
      // Route clicked_node_id directly to showDescPanel using pre-loaded descriptions
      if (name === 'clicked_node_id' && window.staticNodeDescs) {
        var desc = window.staticNodeDescs[String(value)];
        if (desc) {
          var fn = Shiny._handlers['showDescPanel'];
          if (fn) fn({ title: desc.title, title_fi: desc.title_fi || '',
                       text: desc.text || '', text_fi: desc.text_fi || '',
                       nodeId: value, group: desc.group,
                       hasArticle: !!desc.hasArticle, articleUrl: desc.articleUrl || '',
                       articleInline: desc.articleInline || '' });
        }
      }
    }
  };
}
// Minimal jQuery shim so $(document).on(...) doesn't throw in static mode
if (typeof $ === 'undefined') { window.$ = function() { return { on: function() {} }; }; }

var cy = null;
var lastData = null;
var layoutSnapshot = null;    // node geometry before autoFitProjectWidth modifies it
var currentLayoutMode = null; // 'single' | 'two' — last chosen mode
var baseEdgeWidth = 2.5;
var edgeBands = true;   // when on, edges are ribbons that fill each node's height in stacked bands
var edgeGap = 3;        // px of empty space between edge bands where they meet a node (author-controllable)
var edgeSankey = false; // within band mode: taper smoothly between node bands (Sankey) instead of pinching thin mid-span
var edgeOpacity = 0.82; // normal edge opacity (author-controllable via the transparency slider)
var edgeMinWidth = 2.5; // width (px) at the thinnest point mid-span between columns (band mode pinch)
var edgeMinOn = true;   // whether the thinnest-point width applies at all (else ribbons taper naturally to a point)
var edgeCurve = 1;      // exponent for the ribbon width taper into the mid-span pinch (1 = linear; >1 vs <1 = concave/convex)
var edgePinHeader = false; // when on, edges fill a node's header (collapsed) height; off = fill the full border
var gradientExtent = 20;  // how far node edge-color gradients reach, as % of node width (author-controllable)
var nodeOutlineWidth = 3; // resting Theme/Skill/About border width in px; highlight widths scale from this (author-controllable)
var projectOutlineWidth = 3; // resting Project outline width in px (separate control; Theme/Skill share nodeOutlineWidth)
var outlineSaturation = 1;   // multiplier on node-outline color saturation (HSV S); 1 = source colors, 0 = grey
var outlineOpacity = 1;      // node-outline opacity (0 = fully transparent, 1 = solid); from the transparency slider
var nodeTextPad = 0;         // extra horizontal padding (px) between a node's title and its edges (author-controllable)
var nodeBgSameAsGraph = false;
var inlineMode = false;      // experimental "inline UI" paradigm: descriptions render inside nodes, sidebar moves to a top panel
var inlineExpandedMap = {};  // id -> { descHtml, h, openedAt } for every node currently expanded inline
var _openSeq = 0;            // increments on each inline open; the highest openedAt = most recently opened node
var fillNodeW = 0;           // % of extra horizontal space used to widen Theme/Skill (+About) nodes
var fillProjW = 0;           // % of extra horizontal space used to widen Project nodes
var fillColGap = 0;          // % of extra horizontal space used to widen column spacing
var fillNodePad = 0;         // % of extra horizontal space used to pad the DESCRIPTION text (widen the
                             // box, keep the description text column fixed — extra becomes side padding)
var inlineFillBase = null;   // { nodes:{id:{w,x}}, headers:[x] } base horizontal geometry, for re-distributing space
var uiZoom = 1;              // inline-mode magnification on top of the fit zoom (1 = fit width; up to 3)
var inlinePanX = 0;          // horizontal pan (screen px) when zoomed in past the viewport width
var _zoomFocal = null;       // { cx, contentX } — keep a zoom's horizontal focal point under the cursor
var _zoomFocalY = null;      // cursor Y (relative to graph-area) — vertical focal (per-column, clamped)
var inlineBase = null;       // id -> { y, h } base layout snapshot taken while expanding
var inlineColScroll = { Theme: 0, Project: 0, Skill: 0 };  // per-column vertical scroll offset (cyto units)
var inlineColShiftUp = { Theme: 0, Project: 0, Skill: 0 };  // per-column upward shift into the space above (cyto units)
// "About" nodes sit in the Theme column (below the themes) so they share its stacking/scroll while
// staying a distinct group for styling + their own sub-header. stackCol maps a group to its column.
function stackCol(g) { return g === 'About' ? 'Skill' : g; }
function isColNode(g) { return g === 'Theme' || g === 'Project' || g === 'Skill' || g === 'About'; }
var nodeGradients = {};      // nodeId → {side:'left'|'right'|'both', color:rgbaStr}
var nodeBorderBands = {};    // project id → {left:[solid colors], right:[solid colors]} for the outline
var gradientCurve = 1;       // exponent for the gradient fill falloff (1 = linear; >1 = fades faster near border)
var authorEditable = false;  // author app only: when on, node titles are editable in place (inline UI)
var _inlineClickTimer = null; // pending single-click (toggle) on a title/description, cancelled if a double-click (edit) follows
var accordionIcon = 'triangle'; // symbol on node titles marking them openable (inline UI); see ACC_ICONS
var accordionIconSize = 14;  // px font-size of the accordion symbol (author-controllable)
// Accordion indicator specs: c = closed glyph, o = open glyph, oRot = degrees to rotate the open
// glyph (so chevron uses one glyph and only the angle changes), mult = per-style size multiplier
// (chevron glyphs read smaller per font-size, so they get scaled up).
var ACC_ICONS = {
  triangle:  { c: '▸', o: '▾' },
  filled:    { c: '▶', o: '▼' },
  plusminus: { c: '+', o: '−' },
  circled:   { c: '⊕', o: '⊖' },
  chevron:   { c: '❯', o: '❯', oRot: 90, mult: 1.7 },  // same glyph, only the angle changes
  none:      { c: '', o: '' }
};
var gradientHoverMult = 2;   // hover widens the node's base gradient extent by this factor (1 = no change, 0 = hidden)

// A color→transparent gradient whose alpha falls off as (1-t)^gradientCurve across the band, so the
// author can make the fill hug the border (high curve) or spread inward (low curve). `col` is rgba(...).
function expGradient(col, dir) {
  if (gradientCurve === 1) return 'linear-gradient(' + dir + ',' + col + ',transparent)';
  var m = /rgba?\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*(?:,\s*([\d.]+)\s*)?\)/.exec(col);
  if (!m) return 'linear-gradient(' + dir + ',' + col + ',transparent)';
  var r = m[1], g = m[2], b = m[3], a0 = (m[4] != null) ? parseFloat(m[4]) : 1;
  var N = 8, stops = [];
  for (var i = 0; i <= N; i++) {
    var t = i / N;
    var a = a0 * Math.pow(1 - t, gradientCurve);
    stops.push('rgba(' + r + ',' + g + ',' + b + ',' + a.toFixed(3) + ') ' + (t * 100).toFixed(1) + '%');
  }
  return 'linear-gradient(' + dir + ',' + stops.join(',') + ')';
}
var articlesEnabled = false; // when on, nodes with a full-text article show a "Read full article" link
var nodeHoverGradients = {}; // same, for hovered node's neighbours
var nodeBaseGradients = {};  // persistent edge-color gradients: {side,color} for Theme/Skill, {bands:{left,right}} for Project
// Node-gradient opacity at the edge wall (1 = the true edge color). Higher = brighter/more saturated.
// Author-controllable via the "Gradient transparency" slider — affects only the inside fill, not the outline.
var GRAD_ALPHA_BASE = 0.6, GRAD_ALPHA_HOVER = 0.7, GRAD_ALPHA_SELECT = 0.7;
// Set the base gradient fill opacity (0..1); hover/select stay a touch more opaque, as before.
function setGradientFillAlpha(a) {
  a = Math.max(0, Math.min(1, a));
  GRAD_ALPHA_BASE = a;
  GRAD_ALPHA_HOVER = Math.min(1, a + 0.1);
  GRAD_ALPHA_SELECT = Math.min(1, a + 0.1);
}
var projectNodeWidth = 444;
var projectMaxWidth = 0;     // inline: max project node width (px) before a long title wraps to 2 rows.
                             // 0 = auto (cap at the base project width — never widen the column; wrap
                             // instead). >0 raises the cap: widen up to it, then wrap. (author slider)
var narrowGapMult = 1;       // on narrow screens, multiply the base column gap by this (tighter/looser)
var narrowNodeMult = 1;      // on narrow screens, multiply all node widths by this. Together they set
                             // the gap:node proportion on phones (fit-zoom normalises absolute scale).
var ptypePct = 10;
var mobileData = null;
var selectedNodeId = null;
var hoveredNodeId = null;
var _edgeHoverActive = false;   // true while the hover comes from pointing at an edge (not a node)
var mobileMode = false;
var forceMobile = false;
var unifiedUI = true;        // one UI everywhere: narrow screens use the inline node UI, not the old
                             // separate mobile layout (which stays dormant behind this flag — set to
                             // false to restore it). See useMobileLayout().
var autoFitOnOpen = false;   // when a node is opened, zoom so its text column fills the viewport width
                             // (author-controllable). Reading is then vertical-scroll only, no pan.
var autoFitArmed = false;    // suppresses auto-fit until the first user interaction, so the page still
                             // loads as the whole map even when a node opens by default.
var previewWidth = 390;
var previewHeight = 844;
var sheetMode = null; // 'desc' | 'info' | null
var MOBILE_BREAKPOINT = 768;
var fontNode = 12;
var fontPtype = 12;
var fontSubs = 15;
var fontHdr1 = 22;
var fontHdr2 = 15;
var watermarkText = '';
var watermarkSize = 10;
var descFontSize = 18;
var descPad = 10;   // horizontal padding (px) of the inline description text; vertical scales from it
var descPadFill = 0; // extra description-text padding from the "extra width -> description padding" fill
var colBg = '#0b3552';
var colSidebarBg = '#081626';
var colNodeBg = '#081626';
var colTheme = '#3be37a';
var colProject = '#ffad33';
var colSkill = '#78e6e7';
var lightMode = false;
var lightColBg = '#f0f4f8';
var lightColSidebarBg = '#e2eaf3';
var lightColNodeBg = '#e2eaf3';
var lightColTheme = '#1e7c45';
var lightColProject = '#c06000';
var lightColSkill = '#1a7a7b';
var lightEdgeColor = '#555555';
var darkColBg = '#0b3552';
var darkColSidebarBg = '#081626';
var darkColNodeBg = '#081626';
var darkColTheme = '#3be37a';
var darkColProject = '#ffad33';
var darkColSkill = '#78e6e7';
var currentLang  = (new URLSearchParams(window.location.search)).get('lang')   === 'fi' ? 'fi' : 'en';
var pendingNodeParam = (window.location.hash || '').replace(/^#/, '') || null;  // deep link (#<id>): open this node on load
var pendingScrollNode = null;   // node id to scroll into view once it expands (from a deep link)
var isExportMode = (new URLSearchParams(window.location.search)).get('export') === '1';
var accTitleData = {};
var langData = {};
var lastDescMsg = null;
document.addEventListener('DOMContentLoaded', function() {
  if (currentLang === 'fi') document.body.classList.add('lang-fi');
  if (isExportMode) document.documentElement.classList.add('export-mode');
  if (unifiedUI) document.body.classList.add('unified-ui');   // one UI everywhere (hides old mobile chrome)
});

// Raw narrow-viewport detection — a phone-sized screen (or the author's mobile preview). Independent
// of unifiedUI: used for touch-friendly tuning (deeper max zoom for reading) even though we now render
// the SAME inline UI at every width.
function isNarrow() {
  if (forceMobile) return true;
  if (window.matchMedia) {
    if (window.matchMedia('(max-width:' + MOBILE_BREAKPOINT + 'px)').matches) return true;
    // Landscape phone: narrow height, not a tablet
    if (window.matchMedia('(max-height:500px) and (max-width:1100px)').matches) return true;
    return false;
  }
  return (document.documentElement.clientWidth || window.innerWidth) <= MOBILE_BREAKPOINT;
}

// "Should we use the OLD, separate mobile layout?" With unifiedUI on (the default) the answer is always
// no — narrow screens fall through to the inline UI, and every `!useMobileLayout()` gate on the inline
// path stays true on phones. Set unifiedUI = false to bring the old bottom-sheet mobile UI back.
function useMobileLayout() {
  if (unifiedUI) return false;
  return isNarrow();
}

// Max inline magnification. On phones allow a deep zoom so a single node's text can fill the screen
// (whole map fits tiny, so reading needs more than the desktop 3× cap); on desktop keep it at 3×.
function uiZoomMax() { return isNarrow() ? 8 : 3; }

function esc(s) {
  return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function blendWithWhite(hex, alpha) {
  var r = parseInt(hex.slice(1,3),16)||0, g = parseInt(hex.slice(3,5),16)||0, b = parseInt(hex.slice(5,7),16)||0;
  r = Math.min(255,Math.round(r*(1-alpha)+255*alpha));
  g = Math.min(255,Math.round(g*(1-alpha)+255*alpha));
  b = Math.min(255,Math.round(b*(1-alpha)+255*alpha));
  return '#'+r.toString(16).padStart(2,'0')+g.toString(16).padStart(2,'0')+b.toString(16).padStart(2,'0');
}

function blendWithBlack(hex, alpha) {
  function h(n) { var s=n.toString(16); return s.length<2?'0'+s:s; }
  var r = parseInt(hex.slice(1,3),16)||0, g = parseInt(hex.slice(3,5),16)||0, b = parseInt(hex.slice(5,7),16)||0;
  return '#'+h(Math.round(r*(1-alpha)))+h(Math.round(g*(1-alpha)))+h(Math.round(b*(1-alpha)));
}

function hexToRgba(hex, alpha) {
  var r = parseInt(hex.slice(1,3),16)||0, g = parseInt(hex.slice(3,5),16)||0, b = parseInt(hex.slice(5,7),16)||0;
  return 'rgba(' + r + ',' + g + ',' + b + ',' + alpha + ')';
}

// Scale a hex color's saturation on the HSV scale by `mult` (1 = unchanged, 0 = grey). Used for the
// node-outline saturation control; leaves hue/value untouched so only vividness changes.
function saturateColor(hex, mult) {
  if (mult == null || mult === 1) return hex;
  if (typeof hex !== 'string' || hex.charAt(0) !== '#' || hex.length < 7) return hex;
  var r = (parseInt(hex.slice(1,3),16)||0)/255, g = (parseInt(hex.slice(3,5),16)||0)/255, b = (parseInt(hex.slice(5,7),16)||0)/255;
  var max = Math.max(r,g,b), min = Math.min(r,g,b), v = max, d = max - min;
  var s = max === 0 ? 0 : d / max;
  var h = 0;
  if (d !== 0) {
    if (max === r) h = ((g - b) / d) % 6;
    else if (max === g) h = (b - r) / d + 2;
    else h = (r - g) / d + 4;
    h *= 60; if (h < 0) h += 360;
  }
  s = Math.max(0, Math.min(1, s * mult));
  var c = v * s, x = c * (1 - Math.abs((h / 60) % 2 - 1)), m = v - c, rr = 0, gg = 0, bb = 0;
  if (h < 60)      { rr = c; gg = x; }
  else if (h < 120){ rr = x; gg = c; }
  else if (h < 180){ gg = c; bb = x; }
  else if (h < 240){ gg = x; bb = c; }
  else if (h < 300){ rr = x; bb = c; }
  else             { rr = c; bb = x; }
  function h2(n) { var s2 = Math.round((n + m) * 255).toString(16); return s2.length < 2 ? '0' + s2 : s2; }
  return '#' + h2(rr) + h2(gg) + h2(bb);
}

/* ── Node HTML Label Plugin ──────────────────────────────────────────────── */

(function () {
  function register(cyLib) {
    cyLib('core', 'nodeHtmlLabel', function (opts) {
      var inst = this, ctr = inst.container();
      var ov = document.createElement('div');
      ov.style.cssText =
        'position:absolute;top:0;left:0;pointer-events:none;z-index:9;' +
        'overflow:visible;width:100%;height:100%;';
      ctr.style.position = 'relative'; ctr.appendChild(ov);
      // Events on an inline description / copy-link button bubble up to the cy container and would
      // trigger a node tap (collapse). Stop them here (below the container) so only the header
      // collapses, while the description stays selectable and its links / the copy button work.
      ['mousedown','mouseup','dblclick','touchstart','touchend'].forEach(function (evt) {
        ov.addEventListener(evt, function (e) {
          var t = e.target;
          if (t && t.closest && (t.closest('.inline-node-desc') || t.closest('.inline-copy-link') || t.closest('.inline-article-link') || t.closest('.inline-article-expand') || t.closest('.node-title-edit'))) e.stopPropagation();
        }, false);
      });
      // Inline editing (author app): double-click a title or description to edit it, commit on blur.
      ov.addEventListener('dblclick', function (e) {
        if (!authorEditable) return;
        var t = e.target;
        if (_inlineClickTimer) { clearTimeout(_inlineClickTimer); _inlineClickTimer = null; }  // cancel the single-click close
        var span = t && t.closest && t.closest('.node-title-edit');
        if (span) {
          e.stopPropagation(); e.preventDefault();
          span.setAttribute('contenteditable', 'true');
          span.focus();
          try { var r = document.createRange(); r.selectNodeContents(span);
                var s = window.getSelection(); s.removeAllRanges(); s.addRange(r); } catch (err) {}
          return;
        }
        var descEl = t && t.closest && t.closest('.inline-node-desc');
        if (descEl && !(t.closest && (t.closest('.inline-article-link') || t.closest('.inline-article-expand') || t.closest('.inline-article-body')))) {
          e.stopPropagation(); e.preventDefault();
          startDescEdit(descEl.getAttribute('data-node-id'));
        }
      }, false);
      ov.addEventListener('focusout', function (e) {
        var t = e.target;
        if (!(t && t.classList && t.classList.contains('node-title-edit'))) return;
        t.removeAttribute('contenteditable');   // back to non-editable until next double-click
        var id = t.getAttribute('data-node-id'), lang = t.getAttribute('data-lang');
        var text = (t.textContent || '').replace(/\s+/g, ' ').trim();
        var node = cy && cy.getElementById(String(id));
        if (node && !node.empty()) node.data(lang === 'fi' ? 'label_fi' : 'label', text);  // reflect locally, no rebuild
        if (window.Shiny && Shiny.setInputValue)
          Shiny.setInputValue('node_title_edit', { id: id, lang: lang, text: text }, { priority: 'event' });
      }, false);
      ov.addEventListener('keydown', function (e) {
        var t = e.target;
        if (!(t && t.classList && t.classList.contains('node-title-edit'))) return;
        e.stopPropagation();
        if (e.key === 'Enter') { e.preventDefault(); t.blur(); }
        else if (e.key === 'Escape') { t.textContent = t.getAttribute('data-orig') || ''; t.blur(); }
      }, false);
      ov.addEventListener('click', function (e) {
        var t = e.target;
        var btn = t && t.closest && t.closest('.inline-copy-link');
        if (btn) {
          e.stopPropagation(); e.preventDefault();
          var url = new URL(window.location.href); url.hash = String(btn.getAttribute('data-node-id'));
          var flash = function () { btn.textContent = '✓'; setTimeout(function () { btn.textContent = '🔗'; }, 1000); };
          if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(url.toString()).then(flash, flash);
          else flash();
          return;
        }
        // "Read here" toggle: unfold/fold the article body inside the node (both apps).
        var xbtn = t && t.closest && t.closest('.inline-article-expand');
        if (xbtn) {
          e.stopPropagation(); e.preventDefault();
          toggleArticleInline(xbtn.getAttribute('data-node-id'));
          return;
        }
        // Author app: a single click on the unfolded article body edits its raw markdown in place
        // (published/static just lets you read/select it). Text selection is preserved.
        var artBody = t && t.closest && t.closest('.inline-article-body');
        if (inlineMode && authorEditable && artBody) {
          e.stopPropagation();
          if (window.getSelection && String(window.getSelection())) return;   // selecting text: leave as is
          startArticleEdit(artBody.getAttribute('data-node-id'));
          return;
        }
        // Published/static: clicking a description closes the node. Author app: a single click on the
        // text starts editing it (it never closes — only the title does).
        // Text selection (drag) is preserved — don't act if text is selected.
        var descEl = t && t.closest && t.closest('.inline-node-desc');
        if (inlineMode && descEl && !(t.closest && (t.closest('.inline-article-link') || t.closest('.inline-article-expand') || t.closest('.inline-article-body')))) {
          e.stopPropagation();
          if (window.getSelection && String(window.getSelection())) return;   // selecting text: leave as is
          var did = descEl.getAttribute('data-node-id');
          if (!authorEditable) toggleNodeInline(did);                          // published: single click closes
          else startDescEdit(did);                                            // author: single click edits the text
          return;
        }
        // Title (edit mode): a single click toggles the node (open/collapse), delayed so that a
        // double-click edits instead. When not editing titles, titles aren't .node-title-edit and
        // clicks fall through to the normal Cytoscape tap.
        var titleEl = t && t.closest && t.closest('.node-title-edit');
        if (titleEl && authorEditable) {
          e.stopPropagation();
          if (titleEl.getAttribute('contenteditable') === 'true') return;   // currently editing: just place caret
          if (_inlineClickTimer) { clearTimeout(_inlineClickTimer); _inlineClickTimer = null; return; }  // 2nd click → dblclick edits
          var tid = titleEl.getAttribute('data-node-id');
          _inlineClickTimer = setTimeout(function () { _inlineClickTimer = null; toggleNodeInline(tid); }, 250);
          return;
        }
        // Let the article link navigate normally, but keep the tap from collapsing the node.
        if (t && t.closest && t.closest('.inline-article-link')) e.stopPropagation();
      }, false);
      function upd() {
        ov.innerHTML = '';
        var pan = inst.pan(), zoom = inst.zoom();
        inst.nodes().forEach(function (node) {
          var opt = opts[0]; if (!opt) return;
          var html = opt.tpl(node.data()); if (!html) return;
          var pos = node.position(), w = node.data('w') || 160, h = node.data('h') || 44;
          var d = document.createElement('div');
          d.style.cssText = 'position:absolute;box-sizing:border-box;pointer-events:none;overflow:hidden;';
          // Opaque node background so the edge ribbons (drawn beneath the labels) don't bleed over the
          // node body — they stay visible only in the gaps between columns.
          var _nbg = nodeBgSameAsGraph ? colBg : colNodeBg;
          d.style.background = node.hasClass('selected') ? blendWithWhite(_nbg, 0.15) : _nbg;
          d.style.left = ((pos.x - w / 2) * zoom + pan.x) + 'px';
          d.style.top = ((pos.y - h / 2) * zoom + pan.y) + 'px';
          d.style.width = w + 'px'; d.style.height = h + 'px';
          d.style.transform = 'scale(' + zoom + ')'; d.style.transformOrigin = 'top left';
          d.innerHTML = html; ov.appendChild(d);
        });
        if (typeof positionDescEditor === 'function') positionDescEditor();  // keep the desc editor aligned
        if (typeof positionArticleEditor === 'function') positionArticleEditor();  // and the article editor
      }
      inst.on('render', upd); inst.on('pan zoom', upd); upd();
    });
  }
  if (typeof cytoscape !== 'undefined') register(cytoscape);
  else document.addEventListener('DOMContentLoaded', function () {
    if (typeof cytoscape !== 'undefined') register(cytoscape);
  });
})();

/* ── Apply dynamic colors to DOM ──────────────────────────────────────────── */

function applyColors() {
  var ga = document.getElementById('graph-area');
  var cy_el = document.getElementById('cy');
  var sb = document.getElementById('info-sidebar');
  if (ga) ga.style.background = colBg;
  if (cy_el) cy_el.style.background = colBg;
  if (sb) sb.style.background = colSidebarBg;
  var ph = document.getElementById('page-title');
  if (ph) ph.style.background = colBg;
  document.querySelectorAll('.col-spacer').forEach(function(el) { el.style.background = colBg; });
  document.body.style.background = colBg;
  // Accordion header uses graph background color
  var r = document.documentElement.style;
  r.setProperty('--acc-bg', colBg);
  r.setProperty('--acc-bg-hover', blendWithWhite(colBg, 0.07));
  r.setProperty('--col-project', colProject);
  r.setProperty('--col-project-dim', hexToRgba(colProject, 0.4));
  r.setProperty('--col-project-hover', hexToRgba(colProject, 0.12));
  r.setProperty('--col-project-dim2', hexToRgba(colProject, 0.25));
  if (cy) cy.style(buildStyle());
  applySidebarFonts();
  // Toggle light-mode class on body based on background brightness
  var bgR = parseInt(colBg.slice(1,3),16)||0, bgG = parseInt(colBg.slice(3,5),16)||0, bgB = parseInt(colBg.slice(5,7),16)||0;
  var bgBright = (bgR * 299 + bgG * 587 + bgB * 114) / 1000;
  if (bgBright > 128) document.body.classList.add('light-mode');
  else document.body.classList.remove('light-mode');
}

/* ── Resize handle height sync ───────────────────────────────────────────── */

function syncResizeHandle() {
  var pt = document.getElementById('page-title');
  var rh = document.getElementById('sidebar-resize-handle');
  if (!pt || !rh) return;
  var h = pt.offsetHeight;
  var ts = document.getElementById('top-strip');
  var bp = document.getElementById('bottom-panel');
  var bh = ts ? ts.offsetHeight : (bp ? bp.offsetHeight : 0);
  rh.style.height = 'calc(100vh - ' + (h + bh) + 'px)';
  rh.style.marginTop = h + 'px';
}

function applySidebarFonts() {
  // Use CSS custom properties so Shiny renderUI re-renders don't wipe styles
  var r = document.documentElement.style;
  r.setProperty('--desc-font', descFontSize + 'px');
  r.setProperty('--desc-heading', (descFontSize + 2) + 'px');
  r.setProperty('--desc-title-font', ((descFontSize + 4) * 0.75).toFixed(1) + 'px');
}

/* ── Resize: fill container on desktop, fit-width on mobile ──────────────── */

function resizeCy() {
  var el = document.getElementById('cy'); if (!el) return;
  var w = el.parentElement.clientWidth;
  if (w < 10) return; // not laid out yet
  if (inlineMode && !useMobileLayout()) { if (cy) layoutInlineScroll(); return; }
  if (useMobileLayout()) {
    // Fill the graph-area container exactly — fitWithHeaders handles content placement.
    var gaH = el.parentElement.clientHeight;
    el.style.height = (gaH > 10 ? gaH : Math.round(window.innerHeight * 0.60)) + 'px';
  } else {
    var parentH = el.parentElement.clientHeight || window.innerHeight;
    el.style.height = parentH + 'px';
    if (cy) {
      cy.resize();
      fitWithHeaders();
      alignGraphLeft();
      if (lastData) { positionHeaders(lastData); drawEdgeOverlay(); drawNodeConnector(); }
    }
    return;
  }
  if (cy) {
    cy.resize();
    fitWithHeaders();
    if (!useMobileLayout()) alignGraphLeft();
    if (lastData) { positionHeaders(lastData); drawEdgeOverlay(); }
  }
}

/* ── Fit including header space ──────────────────────────────────────────── */

function fitWithHeaders() {
  if (!cy) return;
  var container = document.getElementById('graph-area');
  var W = container ? container.clientWidth : window.innerWidth;
  var H = container ? container.clientHeight : window.innerHeight;
  var bb = cy.elements().boundingBox();
  if (!bb || bb.w === 0) { cy.fit(undefined, 20); return; }
  // Compute zoom so content fits with 20px side/bottom margins AND
  // 8px + hm*zoom header clearance at top (hm = headerMargin from R payload).
  // Vertical:   8 + hm*zoom + bb.h*zoom + 20 = H  →  zoom = (H−28)/(bb.h+hm)
  // Horizontal: 20 + bb.w*zoom + 20           = W  →  zoom = (W−40)/bb.w
  var hm = (lastData && lastData.headerMargin) || 70;
  // On mobile use zoomW (fill width) — the graph is typically taller than wide, so
  // height-constraining the zoom leaves side margins. Users can pan vertically.
  // On desktop use min(zoomH, zoomW) so nothing overflows.
  var extraHdr = useMobileLayout() ? (fontHdr1 * 2.4 + fontHdr2 * 1.3) : 0;
  var zoomH = (H - 28) / (bb.h + hm + extraHdr);
  var zoomW = useMobileLayout() ? (W - 14) / bb.w : (W - 40) / bb.w;
  var zoom  = useMobileLayout() ? zoomW : Math.min(zoomH, zoomW);
  zoom = Math.max(cy.minZoom(), Math.min(cy.maxZoom(), zoom));
  cy.zoom(zoom);
  // Centre horizontally; top-align so headers have exactly 8px clearance.
  var mobileHdrExtra = useMobileLayout() ? (fontHdr1 * 2.4 + fontHdr2 * 1.3) * zoom : 0;
  cy.pan({
    x: W / 2 - (bb.x1 + bb.w / 2) * zoom,
    y: 8 + hm * zoom - bb.y1 * zoom + mobileHdrExtra / 2
  });
}

/* ── Left-align graph content after fit ──────────────────────────────────── */

function alignGraphLeft() {
  if (!cy) return;
  var container = document.getElementById('graph-area');
  if (container && container.clientWidth < 800) return;
  var bb = cy.elements().boundingBox();
  if (!bb || bb.w === 0) return;
  var pan = cy.pan(), zoom = cy.zoom();
  var contentLeftScreen = bb.x1 * zoom + pan.x;
  var dx = 20 - contentLeftScreen;
  cy.pan({ x: pan.x + dx, y: pan.y });
}

/* ── Inline UI mode: fit-to-view by default, scroll once descriptions open ── */
// Base (unexpanded) bounding box — gives a STABLE fit zoom that doesn't jump on expand.
function inlineBaseBBox() {
  if (!cy) return null;
  var x1 = Infinity, x2 = -Infinity, y1 = Infinity, y2 = -Infinity, any = false;
  cy.nodes().forEach(function (n) {
    var g = n.data('group'); if (!isColNode(g)) return;
    var w = n.data('w') || 200, x = n.position('x');
    var b = (inlineBase && inlineBase[n.id()]) ? inlineBase[n.id()] : { y: n.position('y'), h: n.data('h') };
    x1 = Math.min(x1, x - w / 2); x2 = Math.max(x2, x + w / 2);
    y1 = Math.min(y1, b.y - b.h / 2); y2 = Math.max(y2, b.y + b.h / 2); any = true;
  });
  if (!any) return null;
  return { x1: x1, x2: x2, y1: y1, y2: y2, w: x2 - x1, h: y2 - y1 };
}

// Snapshot base horizontal geometry (node width + x, header x) so fill distribution
// can restore a clean base on every fit. Call after the graph is (re)built.
function snapshotInlineFillBase() {
  if (!cy) { inlineFillBase = null; return; }
  var nodes = {};
  cy.nodes().forEach(function (n) {
    var g = n.data('group'); if (!isColNode(g)) return;
    nodes[n.id()] = { w: n.data('w') || 200, x: n.position('x') };
  });
  var headers = (lastData && lastData.headers) ? lastData.headers.map(function (h) { return h.x; }) : [];
  // Stable content height for the fill's slack math — so re-measuring heights at the widened width
  // (syncInlineHeights) can't feed back into how much the fill widens (which would oscillate on scroll).
  var y1 = Infinity, y2 = -Infinity;
  cy.nodes().forEach(function (n) {
    var g = n.data('group'); if (!isColNode(g)) return;
    var h = n.data('h') || 46, y = n.position('y');
    y1 = Math.min(y1, y - h / 2); y2 = Math.max(y2, y + h / 2);
  });
  inlineFillBase = { nodes: nodes, headers: headers, bboxH: (y2 > y1) ? (y2 - y1) : 0 };
}

// Distribute extra horizontal slack (inline mode): widen nodes / spread columns to fill
// the browser width, leaving the remainder as left/right padding. Restores base each call
// so it's idempotent; only touches w / x / header-x (vertical layout untouched).
function applyInlineFill() {
  if (!cy || !inlineFillBase) return;
  // 1) Restore clean base horizontal geometry.
  cy.nodes().forEach(function (n) {
    var b = inlineFillBase.nodes[n.id()];
    if (b) { n.data('w', b.w); n.position('x', b.x); }
  });
  descPadFill = 0;   // reset the distributed description padding (re-applied below if requested)
  if (lastData && lastData.headers) lastData.headers.forEach(function (h, i) {
    if (inlineFillBase.headers[i] != null) h.x = inlineFillBase.headers[i];
  });
  // 2) Only in inline mode, and only if some slack is requested.
  if (!inlineMode || mobileMode) return;
  var total = (fillNodeW || 0) + (fillProjW || 0) + (fillColGap || 0) + (fillNodePad || 0);
  if (total <= 0) return;
  var ga = document.getElementById('graph-area'); if (!ga) return;
  var W = ga.clientWidth, viewH = ga.clientHeight || window.innerHeight;
  var bb = inlineBaseBBox(); if (!bb || bb.w === 0) return;
  var hm = (lastData && lastData.headerMargin) || 70;
  // 3) Height-fit zoom (using the stable cached content height, not the live one) and horizontal slack.
  var bbH = (inlineFillBase.bboxH > 0) ? inlineFillBase.bboxH : bb.h;
  var zh = (viewH - 28) / (bbH + hm);
  if (zh <= 0) return;
  var slackPx = (W - 40) - bb.w * zh;
  if (slackPx <= 0) return; // already width-constrained → nothing to distribute
  // 4) Cyto-space width to consume, split between three shares (theme/skill width, project width,
  //    column gaps). Δwidth = 2·dwTS + 1·dwProj + 2·gAdd  (Theme+Skill widen, Project widens, 2 gaps).
  var usableCyto = (slackPx / zh) * (total / 100);
  var dwTS   = (usableCyto * (fillNodeW / total)) / 2;   // Theme + Skill (+About) text-area widen
  var dwProj = (usableCyto * (fillProjW / total));        // Project text-area widen
  var gAdd   = (usableCyto * (fillColGap / total)) / 2;   // two column gaps
  var dPad   = (usableCyto * (fillNodePad / total)) / 3;  // padding widen, spread over theme+project+skill
  // The padding share widens the box but is added back as DESCRIPTION-text padding (descPadFill), so the
  // description text column stays fixed and the extra width shows as inner padding around descriptions.
  var dwTSp   = dwTS + dPad;                              // theme/skill total box widen
  var dwProjp = dwProj + dPad;                            // project total box widen
  var shiftP = gAdd + dwTSp / 2 + dwProjp / 2;            // Project column shift (keeps gap growth even)
  var shiftS = 2 * gAdd + dwTSp + dwProjp;                // Skill column shift
  if (dPad > 0) descPadFill = dPad / 2;                   // half each side; box grew by dPad → desc column fixed
  // 5) Widen node boxes; shift Project/Skill columns (Theme stays; About rides Skill).
  cy.nodes().forEach(function (n) {
    var g = n.data('group');
    if (!isColNode(g)) return;
    n.data('w', (n.data('w') || 0) + (g === 'Project' ? dwProjp : dwTSp));
    var sc = stackCol(g);   // About rides with its column (Skill) so its x-shift matches
    if (sc === 'Project') n.position('x', n.position('x') + shiftP);
    else if (sc === 'Skill') n.position('x', n.position('x') + shiftS);
  });
  if (lastData && lastData.headers) {
    if (lastData.headers[1]) lastData.headers[1].x += shiftP;
    if (lastData.headers[2]) lastData.headers[2].x += shiftS;
  }
}

// Re-measure each collapsed node's height at its CURRENT (post-fill) width and re-stack each column,
// preserving inter-node gaps. Fixes tall boxes left behind when the fill widens a node so its title
// drops from two rows to one. Idempotent once heights match the width (the fill uses a cached stable
// height, so this can't feed back into the widening). Operates on inlineBase when it exists, else on
// the live nodes directly.
function syncInlineHeights() {
  if (!cy || !inlineMode) return;
  var useBase = !!inlineBase;
  var arr = [];
  cy.nodes().forEach(function (n) {
    if (n.data('group') !== 'Project') return;   // Project column only — Theme/Skill keep their heights
    if (useBase && !inlineBase[n.id()]) return;
    arr.push(n);
  });
  if (!arr.length) return;
  function yOf(n) { return useBase ? inlineBase[n.id()].y : n.position('y'); }
  function hOf(n) { return useBase ? inlineBase[n.id()].h : (n.data('h') || 46); }
  arr.sort(function (a, b) { return yOf(a) - yOf(b); });
  var pob = null, pnb = null;
  arr.forEach(function (n) {
    var bh = hOf(n), by = yOf(n);
    var oldTop = by - bh / 2, oldBottom = by + bh / 2;
    var newH = inlineExpandedMap[n.id()] ? bh : collapsedNodeHeight(n.data());
    var nTop = (pnb == null) ? oldTop : pnb + (oldTop - pob);   // preserve the gap above this node
    var newY = nTop + newH / 2;
    if (useBase) { inlineBase[n.id()].y = newY; inlineBase[n.id()].h = newH; }
    else { n.position('y', newY); n.data('h', newH); }
    pob = oldBottom; pnb = nTop + newH;
  });
}

function layoutInlineScroll() {
  if (!cy) return;
  var ga = document.getElementById('graph-area'), el = document.getElementById('cy');
  if (!ga || !el) return;
  applyInlineFill();                 // redistribute extra horizontal space before fitting
  syncInlineHeights();               // re-measure heights at the widened width (fixes tall boxes) + re-stack
  var W = ga.clientWidth, viewH = ga.clientHeight || window.innerHeight;
  var baseBB = inlineBaseBBox();
  if (!baseBB || baseBB.w === 0) return;
  var hm = (lastData && lastData.headerMargin) || 70;
  var prevZoom = cy.zoom();           // zoom before this re-fit (for the vertical focal shift below)
  // Fit BOTH dimensions of the base layout so the no-description view shows every node, then apply
  // the user's magnification (uiZoom). Columns still scroll vertically per-column at any zoom.
  var fitW = (W - 40) / baseBB.w;
  var fitH = (viewH - 28) / (baseBB.h + hm);
  // Desktop: fit the whole map (min of both). Narrow screens: fit to WIDTH only — the columns fill the
  // width (minimal side margins) and the content scrolls vertically. Width-only also keeps the zoom
  // independent of node heights, so the node-height multiplier makes nodes taller without shrinking text.
  var fitZoom = isNarrow() ? fitW : Math.min(fitW, fitH);
  var zoom = Math.max(cy.minZoom(), Math.min(cy.maxZoom(), fitZoom * (uiZoom || 1)));
  var topPad = 8 + hm * zoom;
  // Keep the graph below the fixed header bar even when it wraps to 2–3 rows on a narrow screen,
  // so the wrapped controls never cover the column headers.
  var hdrBar = document.getElementById('inline-header-right');
  if (hdrBar) { var hbH = hdrBar.getBoundingClientRect().height; if (hbH > 0) topPad = Math.max(topPad, hbH + 6); }
  el.style.height = viewH + 'px';    // viewport-sized; columns scroll internally (per column)
  cy.resize();
  cy.zoomingEnabled(true);            // briefly allow the programmatic zoom below
  cy.zoom(zoom);
  // Horizontal placement: centre the leftover padding when the graph fits; once zoomed in past the
  // viewport width, use the (focal-adjusted) horizontal pan, clamped so both edges stay reachable.
  var contentW = baseBB.w * zoom;
  if (contentW <= W - 40) {
    inlinePanX = Math.max(20, (W - contentW) / 2) - baseBB.x1 * zoom;   // centred (min 20px each side)
  } else {
    if (_zoomFocal) inlinePanX = _zoomFocal.cx - _zoomFocal.contentX * zoom;
    var maxPanX = 20 - baseBB.x1 * zoom;                                // left edge at 20px
    var minPanX = (W - 20) - (baseBB.x1 * zoom + contentW);             // right edge at W-20px
    inlinePanX = Math.min(maxPanX, Math.max(minPanX, inlinePanX));
  }
  _zoomFocal = null;
  cy.pan({ x: inlinePanX, y: topPad - baseBB.y1 * zoom });
  cy.zoomingEnabled(false);           // then fully block wheel/gesture zoom while inline
  // Magnified past the vertical fit → capture the base so per-column scroll works even before any
  // node is expanded (otherwise you couldn't scroll down to nodes pushed off the bottom by zoom).
  if (!inlineBase && baseBB.h * zoom > viewH - topPad - 8) captureInlineBase();
  // Vertical focal: shift every column's scroll by the same Δ so the point under the cursor stays put
  // while zooming. Node screen-Y = 8 + (layoutY − scroll)·zoom, so keeping a point fixed needs
  // Δscroll = (cursorY − 8)·(1/prevZoom − 1/zoom). Each column then clamps to its own range (a short
  // column can lag at the extremes — an accepted trade-off for keeping per-column scroll).
  if (_zoomFocalY != null && prevZoom > 0 && inlineBase) {
    var dS = (_zoomFocalY - 8) * (1 / prevZoom - 1 / zoom);
    inlineColScroll.Theme   = (inlineColScroll.Theme   || 0) + dS;
    inlineColScroll.Project = (inlineColScroll.Project || 0) + dS;
    inlineColScroll.Skill   = (inlineColScroll.Skill   || 0) + dS;
  }
  _zoomFocalY = null;
  applyInlinePositions();             // stack expansions + apply per-column scroll offsets (clamped)
  cy.emit('render');
  if (lastData) positionHeaders(lastData);
  drawEdgeOverlay();
  updateZoomLabel();
}

// Set the inline magnification, keeping the point under the cursor (clientX/clientY) fixed, then re-fit.
function setUiZoom(newUi, clientX, clientY) {
  if (!cy || !inlineMode || useMobileLayout()) return;
  newUi = Math.max(1, Math.min(uiZoomMax(), newUi));
  if (Math.abs(newUi - uiZoom) < 1e-4) return;
  var ga = document.getElementById('graph-area');
  if (ga && clientX != null) {
    var r = ga.getBoundingClientRect();
    _zoomFocal = { cx: clientX - r.left, contentX: (clientX - r.left - cy.pan().x) / cy.zoom() };
    if (clientY != null) _zoomFocalY = clientY - r.top;   // vertical focal (applied per column below)
  }
  uiZoom = newUi;
  layoutInlineScroll();
}

// Lightweight horizontal pan (zoomed-in state): shift x only, keep per-column vertical scroll intact.
function applyInlinePanX(dxScreen) {
  if (!cy || !inlineMode) return;
  var ga = document.getElementById('graph-area'); if (!ga) return;
  var W = ga.clientWidth, bb = inlineBaseBBox(); if (!bb) return;
  var zoom = cy.zoom(), contentW = bb.w * zoom;
  if (contentW <= W - 40) return;                          // nothing to pan when it fits
  var maxPanX = 20 - bb.x1 * zoom, minPanX = (W - 20) - (bb.x1 * zoom + contentW);
  inlinePanX = Math.min(maxPanX, Math.max(minPanX, inlinePanX + (dxScreen || 0)));
  cy.pan({ x: inlinePanX, y: cy.pan().y });
  cy.emit('render');
  if (lastData) positionHeaders(lastData);
  drawEdgeOverlay();
}

// Apply a drag step: clamp the horizontal pan (when zoomed in) and re-apply per-column scroll.
function applyInlineDrag() {
  if (!cy || !inlineMode) return;
  var ga = document.getElementById('graph-area'); if (!ga) return;
  var W = ga.clientWidth, bb = inlineBaseBBox(); if (!bb) return;
  var zoom = cy.zoom(), contentW = bb.w * zoom;
  if (contentW > W - 40) {
    var maxPanX = 20 - bb.x1 * zoom, minPanX = (W - 20) - (bb.x1 * zoom + contentW);
    inlinePanX = Math.min(maxPanX, Math.max(minPanX, inlinePanX));
    cy.pan({ x: inlinePanX, y: cy.pan().y });
  }
  applyInlinePositions();             // vertical per-column scroll (clamped) + expansions
  cy.emit('render');
  if (lastData) positionHeaders(lastData);
  drawEdgeOverlay();
}

function updateZoomLabel() {
  var el = document.getElementById('inline-zoom-pct');
  if (el) el.textContent = Math.round((uiZoom || 1) * 100) + '%';
}

// Re-stack each column from its base layout, growing expanded nodes downward. A growing Theme/Skill
// column first rises into the empty space above it (up to the Project column's top / header level)
// before extending below, then scrolls independently if it still overflows.
function applyInlinePositions() {
  if (!cy || !inlineBase) return;
  var ga = document.getElementById('graph-area');
  var viewH = ga ? (ga.clientHeight || window.innerHeight) : window.innerHeight;
  var zoom = cy.zoom(), panY = cy.pan().y;
  var viewportBottomCyto = (viewH - panY) / zoom;
  var cols = {};
  cy.nodes().forEach(function (n) {
    if (inlineBase[n.id()]) { var c = stackCol(n.data('group')); (cols[c] = cols[c] || []).push(n); }
  });
  // Top of the Project column (base) — the highest level Theme/Skill columns may rise to.
  var projTop = Infinity;
  (cols.Project || []).forEach(function (n) { var b = inlineBase[n.id()]; if (b) projTop = Math.min(projTop, b.y - b.h / 2); });
  if (!isFinite(projTop)) projTop = (inlineBaseBBox() || { y1: 0 }).y1;
  Object.keys(cols).forEach(function (g) {
    var arr = cols[g];
    arr.sort(function (a, b) { return inlineBase[a.id()].y - inlineBase[b.id()].y; });
    var firstTop = inlineBase[arr[0].id()].y - inlineBase[arr[0].id()].h / 2;
    // First pass: total growth + base-stacked bottom
    var cum = 0, bottom = -Infinity;
    arr.forEach(function (n) {
      var b = inlineBase[n.id()], ex = inlineExpandedMap[n.id()];
      var h = ex ? ex.h : b.h;
      bottom = (b.y - b.h / 2) + cum + h;
      cum += (h - b.h);
    });
    var totalDelta = cum;
    // Rise into the space above (Theme/Skill only; Project is already at the top)
    var shiftUp = Math.min(totalDelta, Math.max(0, firstTop - projTop));
    inlineColShiftUp[g] = shiftUp;
    // Clamp this column's own scroll to what still overflows after the upward shift. A small bottom
    // clearance lets the last node scroll fully into view instead of sitting flush against the edge.
    var bottomClear = 20 / zoom;
    var maxScroll = Math.max(0, (bottom - shiftUp) - viewportBottomCyto + bottomClear);
    var off = Math.min(Math.max(inlineColScroll[g] || 0, 0), maxScroll);
    inlineColScroll[g] = off;
    // Second pass: apply positions (shifted up by shiftUp, then by the scroll offset)
    cum = 0;
    arr.forEach(function (n) {
      var b = inlineBase[n.id()], ex = inlineExpandedMap[n.id()];
      var h = ex ? ex.h : b.h;
      n.data('h', h);
      n.position('y', (b.y - b.h / 2) + cum + h / 2 - shiftUp - off);
      cum += (h - b.h);
    });
  });
}

// Which column (group) is under the given screen X — for routing wheel scroll to one column.
function inlineColumnAt(clientX) {
  if (!cy) return null;
  var ga = document.getElementById('graph-area'); if (!ga) return null;
  var rect = ga.getBoundingClientRect();
  var cytoX = (clientX - rect.left - cy.pan().x) / cy.zoom();
  var groups = ['Theme', 'Project', 'Skill'], nearest = null, nd = Infinity;
  for (var i = 0; i < groups.length; i++) {
    var n = cy.nodes('[group = "' + groups[i] + '"]').first();
    if (!n.length) continue;
    var x = n.position('x'), w = n.data('w') || 200;
    if (cytoX >= x - w / 2 && cytoX <= x + w / 2) return groups[i];
    var d = Math.abs(cytoX - x);
    if (d < nd) { nd = d; nearest = groups[i]; }
  }
  return nearest;
}

// Fit the graph: scrollable width-fit in inline mode, otherwise the normal viewport fit.
function fitGraph() {
  if (inlineMode && !useMobileLayout()) {
    if (cy) { cy.userZoomingEnabled(false); cy.userPanningEnabled(false); }
    layoutInlineScroll();
  } else {
    if (cy) cy.zoomingEnabled(true);
    fitWithHeaders();
    if (!useMobileLayout()) alignGraphLeft();
  }
}
/* resize handled by viewport change listener at bottom */

/* ── Cytoscape Styles — border 1.1px ─────────────────────────────────────── */

function buildStyle() {
  var borderColor = function (e) {
    var g = e.data('group');
    // Theme/Skill/Project outlines are painted by the HTML overlay (nodeBorderBandsHtml) in the
    // node's source colors — keep the Cytoscape border transparent so it can't leak the column color.
    if (g === 'Theme' || g === 'Skill' || g === 'Project') return 'transparent';
    if (g === 'About') return lightMode ? '#8a99a8' : '#7d8b98';
    return colProject;
  };
  var bm = nodeOutlineWidth / 3;  // highlight border widths scale from the resting outline width (3 = original)
  return [
    { selector: 'node', style: {
        shape: 'rectangle', width: 'data(w)', height: 'data(h)',
        'background-fill': 'flat',
        'background-color': function() { return nodeBgSameAsGraph ? colBg : colNodeBg; },
        // Only About uses the Cytoscape border; Theme/Skill/Project draw their outline via the HTML
        // overlay, so keep their Cytoscape border width 0 (a 'transparent' colour + border-opacity
        // otherwise renders as a thin black frame, since the opacity is applied to the colour's RGB).
        'border-width': function(e){ return e.data('group') === 'About' ? nodeOutlineWidth : 0; },
        'border-color': borderColor, 'border-opacity': function() { return outlineOpacity; }, 'border-style': 'solid', label: '',
        'shadow-opacity': 0, 'outline-width': 0, 'outline-opacity': 0, 'underlay-opacity': 0, 'overlay-opacity': 0,
        cursor: function (e) {
          var g = e.data('group');
          return (g === 'Project' || g === 'Theme' || g === 'Skill') ? 'pointer' : 'default';
        },
    }},
    { selector: 'node.nbr-hi', style: {
        'border-opacity': 1, 'outline-width': 0,   // neighbor emphasis = solid outline (About border), no thickening
    }},
    { selector: 'node.selected', style: {
        'background-color': function() { var b = nodeBgSameAsGraph ? colBg : colNodeBg; return blendWithWhite(b, 0.15); },
        'border-width': function(e){ return e.data('group') === 'About' ? (mobileMode ? 14 : 9) * bm : 0; }, 'shadow-opacity': 0, 'outline-width': 0, 'outline-opacity': 0,
    }},
    { selector: 'node.hovered', style: {
        'border-opacity': 1, 'outline-width': 0,   // hover clears the outline transparency (About border); no thickening
    }},
    { selector: 'node.selected.hovered', style: {
        'outline-width': 0, 'outline-opacity': 0,
    }},
    { selector: 'edge', style: { opacity: 0, width: 0 } },
    { selector: 'edge.selected', style: { opacity: 0, width: 0 } },
    { selector: 'edge.hovered', style: { opacity: 0, width: 0 } },
  ];
}

function buildElements(data) {
  var els = [];
  (data.nodes || []).forEach(function (n) { els.push({ data: n.data, position: n.position }); });
  (data.edges || []).forEach(function (e) { els.push({ data: e.data }); });
  return els;
}

/* ── Node HTML — font sizes from layout sliders ──────────────────────────── */

function dualLabel(en, fi) {
  var en_esc = esc(en || '');
  var fi_esc = fi ? esc(fi) : en_esc;
  return '<span class="en-only">' + en_esc + '</span><span class="fi-only">' + fi_esc + '</span>';
}

// Node title, made editable in place when the author-edit mode is on (author app, inline UI only).
// Each language span is separately editable and shows its own raw stored value; commit is on blur.
function editableLabel(data) {
  if (!(authorEditable && inlineMode && isColNode(data.group)))
    return dualLabel(data.label, data.label_fi);
  var en = esc(data.label || ''), fi = esc(data.label_fi || '');
  // Not editable until double-clicked (see the dblclick handler on the overlay); single clicks are
  // no-ops here so they neither edit nor collapse the node.
  function span(cls, lang, txt) {
    var attr = txt.replace(/"/g, '&quot;');  // safe inside the data-orig attribute
    return '<span class="' + cls + ' node-title-edit" spellcheck="false" title="Double-click to edit" ' +
      'data-node-id="' + data.id + '" data-lang="' + lang + '" data-orig="' + attr + '" ' +
      'style="pointer-events:auto;cursor:text;outline:none;border-radius:2px;">' + txt + '</span>';
  }
  return span('en-only', 'en', en) + span('fi-only', 'fi', fi);
}

function hexRgba(hex, a) {
  var r = parseInt(hex.slice(1,3),16)||0, g = parseInt(hex.slice(3,5),16)||0, b = parseInt(hex.slice(5,7),16)||0;
  return 'rgba(' + r + ',' + g + ',' + b + ',' + a + ')';
}
// Project gradient bands: Theme edges on the left, Skill edges on the right, each in the
// connecting edge's color (at the given alpha), sorted by the other node's Y so band order
// matches the layout (no crossings). Returns { left:[...colors], right:[...colors] }.
function projectBandColors(n, alpha) {
  var leftArr = [], rightArr = [];
  n.connectedEdges().forEach(function(edge) {
    var otherId = edge.data('source') === n.id() ? edge.data('target') : edge.data('source');
    var on = cy.getElementById(otherId); if (!on || on.empty()) return;
    var og = on.data('group');
    if (og !== 'Theme' && og !== 'Skill') return;
    var raw = (lightMode ? edge.data('lightColor') : edge.data('color')) || (lightMode ? '#000000' : '#ffffff');
    var entry = { y: on.position().y, col: hexRgba(raw, alpha) };
    if (og === 'Theme') leftArr.push(entry); else rightArr.push(entry);
  });
  var byY = function(a, b) { return a.y - b.y; };
  leftArr.sort(byY); rightArr.sort(byY);
  return {
    left:  leftArr.map(function(e) { return e.col; }),
    right: rightArr.map(function(e) { return e.col; })
  };
}

// Vertical geometry (in % of node height) of a project side's `count` bands, matching where the edge
// ribbons stack against the node wall (equal bands filling the height, with `edgeGap` px between them
// in band mode). Returns [{ top, height }] in %; used so the gradient + outline line up with the edges.
function projectBandGeom(id, count) {
  if (count <= 0) return [];
  var node = cy ? cy.getElementById(String(id)) : null;
  var fullH = (node && !node.empty()) ? (node.data('h') || 46) : 46;
  var ex = (inlineMode && node && !node.empty()) ? inlineExpandedMap[String(id)] : null;
  var baseH = ex ? (ex.origH || fullH) : fullH;             // header (collapsed) height
  var H = edgePinHeader ? baseH : fullH;                    // fill the full border by default; header when pinning
  var g = (edgeBands && cy) ? (edgeGap / cy.zoom()) : 0;    // gap in cyto units (0 when not banding)
  var band = (H - (count - 1) * g) / count;
  if (band < 1) { band = H / count; g = 0; }
  var geom = [];                                            // matches drawEdgeBands: stack from the node top
  for (var i = 0; i < count; i++) geom.push({ top: (i * (band + g)) / fullH * 100, height: band / fullH * 100 });
  return geom;
}

// Source-colored node outlines, drawn as an HTML overlay (reliable: re-renders per node like the
// gradients do). Theme/Skill get a full-perimeter border in the node's own source color. Project
// gets solid source-colored bars — Theme edge colors on the LEFT, Skill edge colors on the RIGHT —
// split into equal vertical bands when a side has several edges.
// Width tracks the node's current Cytoscape border width so it matches resting + highlight states.
function nodeBorderBandsHtml(data) {
  if (!cy) return '';
  var n = cy.getElementById(String(data.id));
  if (!n || n.empty()) return '';
  var grp = data.group;
  var baseW = (grp === 'Project') ? projectOutlineWidth : nodeOutlineWidth;  // Project has its own thickness
  var bm = baseW / 3;                   // highlight widths scale from the resting outline width
  // Hover/neighbor emphasis = clear the outline's chosen transparency (full opacity), NOT a thicker
  // border. `emph` is true for the hovered node AND for its neighbours (so hovering a Project no longer
  // thickens the connected Theme/Skill outlines — it just makes them solid).
  var emph = n.hasClass('hovered') || n.hasClass('nbr-hi');
  var op = emph ? 1 : outlineOpacity;
  var bw = n.hasClass('selected') ? (mobileMode ? 14 : 9) * bm : baseW;   // only selection changes width
  var z = 'position:absolute;pointer-events:none;z-index:7;';
  if (grp === 'Theme' || grp === 'Skill') {
    // Use the same colour source as the node's gradient: the connected edge's colour first
    // (this renders reliably per-node), then the node's own edgeColor, then the group colour.
    var edge = n.connectedEdges()[0];
    var col = (edge && (lightMode ? edge.data('lightColor') : edge.data('color')))
              || (lightMode ? (n.data('lightEdgeColor') || n.data('edgeColor')) : n.data('edgeColor'))
              || (grp === 'Theme' ? colTheme : colSkill);
    col = saturateColor(col, outlineSaturation);
    return '<div style="' + z + 'opacity:' + op + ';top:0;left:0;right:0;bottom:0;box-sizing:border-box;border:' + bw + 'px solid ' + col + ';"></div>';
  }
  if (grp !== 'Project') return '';
  // Prefer bands computed in buildBaseGradients (while edges were present) so the outline always
  // matches the visible gradient; fall back to a live recompute if the map isn't built yet.
  var out = '';
  // Top & bottom outline only, in the project's own colour (from the Color tab). The left/right walls
  // stay open so the edge ribbons flow into the node's sides; connection colours still read from the
  // node's internal gradient. Solid on hover / neighbour-highlight, else the chosen outline opacity.
  var pcol = saturateColor(colProject, outlineSaturation);
  out += '<div style="' + z + 'opacity:' + op + ';left:0;top:0;width:100%;height:' + bw + 'px;background:' + pcol + ';"></div>';
  out += '<div style="' + z + 'opacity:' + op + ';left:0;bottom:0;width:100%;height:' + bw + 'px;background:' + pcol + ';"></div>';
  return out;
}

// Which band a given neighbor occupies among project `proj`'s connections of `neighborGroup`
// ('Theme' = left bands, 'Skill' = right bands). Ordering matches projectBandColors (sorted by Y).
// Returns { index, count }; index is -1 if not found.
function projectBandIndex(proj, neighborGroup, neighborId) {
  var arr = [];
  proj.connectedEdges().forEach(function(edge) {
    var otherId = edge.data('source') === proj.id() ? edge.data('target') : edge.data('source');
    var on = cy.getElementById(otherId); if (!on || on.empty()) return;
    if (on.data('group') !== neighborGroup) return;
    arr.push({ id: otherId, y: on.position().y });
  });
  arr.sort(function(a, b) { return a.y - b.y; });
  var idx = -1;
  for (var i = 0; i < arr.length; i++) if (arr[i].id === String(neighborId)) { idx = i; break; }
  return { index: idx, count: arr.length };
}

function buildBaseGradients() {
  nodeBaseGradients = {};
  nodeBorderBands = {};
  if (!cy) return;
  cy.nodes().forEach(function(n) {
    var grp = n.data('group');
    if (grp === 'Project') {
      var bands = projectBandColors(n, GRAD_ALPHA_BASE);
      if (bands.left.length || bands.right.length) nodeBaseGradients[n.id()] = { bands: bands };
      // Solid (full-alpha) copy for the outline, computed here while edges are present.
      nodeBorderBands[n.id()] = projectBandColors(n, 1);
      return;
    }
    if (grp !== 'Theme' && grp !== 'Skill') return;
    var side = grp === 'Theme' ? 'right' : 'left';
    n.connectedEdges().forEach(function(edge) {
      if (nodeBaseGradients[n.id()]) return; // first edge color only (consistent with hover behavior)
      var raw = (lightMode ? edge.data('lightColor') : edge.data('color')) || (lightMode ? '#000000' : '#ffffff');
      var edgeCol = hexRgba(raw, GRAD_ALPHA_BASE);
      nodeBaseGradients[n.id()] = { side: side, color: edgeCol };
    });
    // Edgeless nodes still get gradient using their own edgeColor (or group color as last resort)
    if (!nodeBaseGradients[n.id()]) {
      var rawFb = lightMode ? (n.data('lightEdgeColor') || n.data('edgeColor')) : n.data('edgeColor');
      var fallback = rawFb || (grp === 'Theme' ? colTheme : colSkill);
      nodeBaseGradients[n.id()] = { side: side, color: hexRgba(fallback, GRAD_ALPHA_BASE) };
    }
  });
}

function applyNodeBorderColors() {
  if (!cy) return;
  cy.nodes().forEach(function(n) {
    var grp = n.data('group');
    if (grp !== 'Theme' && grp !== 'Skill') return;
    // Outline = the node's own source colour (same field that drives its gradient), falling back to
    // a connected edge's colour, then the group colour. This keeps each node's outline distinct.
    var edge = n.connectedEdges()[0];
    var fb = grp === 'Theme' ? colTheme : colSkill;
    n.data('_borderColDark',  n.data('edgeColor')      || (edge && edge.data('color'))      || fb);
    n.data('_borderColLight', n.data('lightEdgeColor') || n.data('edgeColor') || (edge && edge.data('lightColor')) || fb);
  });
}

function gradientOverlay(id, pct, baseOnly) {
  var sel = baseOnly ? null : nodeGradients[id], hov = baseOnly ? null : nodeHoverGradients[id], base = nodeBaseGradients[id];
  if (!sel && !hov && !base) return '';
  var sty = 'position:absolute;pointer-events:none;z-index:5;';
  var out = '';
  // `clip` (a % of node height, or null) caps how far down a gradient may reach — used to keep the
  // hover extension within the node's title/header region on inline-expanded nodes.
  function addDiv(side, col, w, top, height, clip) {
    var t = top || 0, h = (height == null ? 100 : height);
    if (clip != null) { if (t >= clip) return; if (t + h > clip) h = clip - t; }
    var ws = w + '%';
    var box = 'top:' + t + '%;height:' + h + '%;';
    if (side === 'left'  || side === 'both')
      out += '<div style="' + sty + box + 'width:' + ws + ';left:0;background:' + expGradient(col, 'to right') + ';"></div>';
    if (side === 'right' || side === 'both')
      out += '<div style="' + sty + box + 'width:' + ws + ';right:0;background:' + expGradient(col, 'to left') + ';"></div>';
  }
  function addBands(side, colors, w, clip) {
    var n = colors.length; if (!n) return;
    var geom = projectBandGeom(id, n);   // line the gradient bands up with the edge ribbons
    for (var i = 0; i < n; i++) addDiv(side, colors[i], w, geom[i].top, geom[i].height, clip);
  }
  // Render a gradient: multi-band {bands:{left,right}}, or a single {side,color} optionally
  // confined to a vertical slice via {top,height} (used to highlight one project band).
  function render(g, w, clip) {
    if (!g) return;
    if (g.bands) { addBands('left', g.bands.left || [], w, clip); addBands('right', g.bands.right || [], w, clip); }
    else addDiv(g.side, g.color, w, g.top, g.height, clip);
  }
  var basePct = pct || 10;
  // Base gradient at the resting extent (full node height).
  if (base) render(base, basePct);
  // Hover widens just the hover-affected part by gradientHoverMult, using the base gradient (not an
  // extra one). On inline-expanded nodes the extension is confined to the title/header region so the
  // description area keeps only the base gradient.
  if (hov) {
    var ex = inlineMode ? inlineExpandedMap[String(id)] : null;
    var clip = (ex && ex.h) ? Math.max(0, Math.min(100, (ex.origH / ex.h) * 100)) : null;
    render(hov, basePct * gradientHoverMult, clip);
  }
  if (sel) render(sel, basePct * (sel.widthMult || 1));
  return out;
}

var _measureCanvas = null;
function measureTextPx(text, sizePx) {
  if (!_measureCanvas) _measureCanvas = document.createElement('canvas');
  var ctx = _measureCanvas.getContext('2d');
  ctx.font = 'bold ' + sizePx + 'px Arial,Helvetica,sans-serif';
  return ctx.measureText(text).width;
}

function nodeHtml(data) {
  // Inline UI: an expanded node keeps its normal content as a fixed "header" and shows the
  // description below it (selectable; only the header collapses the node on click).
  var exEntry = (inlineMode && inlineExpandedMap[String(data.id)]) || null;
  if (exEntry) return inlineExpandedNodeHtml(data, exEntry);
  return nodeBodyHtml(data);
}

// Small open/closed indicator on a node title, marking it as an openable accordion (inline UI only).
// Sits on the side opposite the title text; for expanded nodes it's centered in the header (headerH).
function accordionIconHtml(open, group, headerH) {
  if (!inlineMode) return '';
  var spec = ACC_ICONS[accordionIcon] || ACC_ICONS.triangle;
  var glyph = open ? spec.o : spec.c;
  if (!glyph) return '';
  var vpos = (headerH != null) ? ('top:' + (headerH / 2) + 'px;') : 'top:50%;';
  var col = lightMode ? 'rgba(0,0,0,0.5)' : 'rgba(255,255,255,0.7)';
  var rot = (open && spec.oRot) ? (' rotate(' + spec.oRot + 'deg)') : '';
  var size = accordionIconSize * (spec.mult || 1);
  // A symbol on both ends. When closed, mirror the right one so both point inward; when open both are
  // identical (e.g. both point down for the chevron).
  function icon(sideCss, mirror) {
    var tf = 'translateY(-50%)' + (mirror ? ' scaleX(-1)' : '') + rot;
    return '<div class="acc-node-icon" style="position:absolute;' + sideCss + vpos +
      'transform:' + tf + ';z-index:8;font-size:' + size + 'px;line-height:1;color:' + col + ';pointer-events:none;">' + glyph + '</div>';
  }
  // Projects carry a type label (Website/Text/…) on the right, so only show the left chevron there
  // to avoid overlapping it; other groups show a chevron at both ends.
  if (group === 'Project') return icon('left:10px;', false);
  return icon('left:10px;', false) + icon('right:10px;', !open);
}

function nodeBodyHtml(data, noGradient) {
  var g = data.group;
  var label = editableLabel(data);
  var fn = fontNode + 'px', fs = fontSubs + 'px';
  var wrap = 'word-wrap:break-word;overflow-wrap:break-word;';
  function grad(pct) { return noGradient ? '' : gradientOverlay(data.id, pct); }
  // Project outline bands render at full node height; for inline-expanded nodes they're added by
  // inlineExpandedNodeHtml at the outer (full-height) level instead, so suppress them in the header.
  function border() { return noGradient ? '' : nodeBorderBandsHtml(data); }
  // Accordion open/closed indicator — only on a standalone (collapsed) node here; the expanded node
  // adds its own (open) icon in inlineExpandedNodeHtml, so suppress it in that header (noGradient).
  function acc() { return noGradient ? '' : accordionIconHtml(false, g, null); }
  // Reserve a gutter beside the accordion symbol so the title doesn't crowd it (Theme icon is on the
  // right, Skill on the left). Sized from the symbol's rendered width plus a gap.
  var _accSpec = (inlineMode && accordionIcon !== 'none') ? ACC_ICONS[accordionIcon] : null;
  var accGutter = (_accSpec && _accSpec.c) ? Math.round(accordionIconSize * (_accSpec.mult || 1) * 0.7) + 16 : 0;

  if (g === 'Theme') {
    return '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;' +
      'display:flex;align-items:center;justify-content:flex-end;padding:3px ' + (7 + nodeTextPad) + 'px;overflow:hidden;">' +
      '<span style="color:' + colTheme + ';font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'font-weight:bold;text-align:right;line-height:1.25;padding-right:' + Math.max(14, accGutter) + 'px;position:relative;z-index:6;' + wrap + '">' + label + '</span>' +
      grad(gradientExtent) + border() + acc() + '</div>';
  }
  if (g === 'Skill') {
    var html = '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;' +
      'display:flex;flex-direction:column;justify-content:center;padding:4px ' + (7 + nodeTextPad) + 'px 4px ' + (Math.max(7, accGutter) + nodeTextPad) + 'px;overflow:hidden;">' +
      '<div style="color:' + colSkill + ';font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'font-weight:bold;line-height:1.25;position:relative;z-index:6;' + wrap + '">' + label + '</div>';
    var subs = data.subs || '';
    if (subs) {
      var items = subs.split('||');
      for (var i = 0; i < items.length; i++)
        html += '<div style="color:' + colSkill + ';opacity:0.7;font-family:Arial,Helvetica,sans-serif;' +
          'font-size:' + fs + ';line-height:1.3;padding-left:10px;position:relative;z-index:6;' + wrap + '">' + esc(items[i]) + '</div>';
    }
    return html + grad(gradientExtent) + border() + acc() + '</div>';
  }
  if (g === 'About') {
    // Neutral informational node in the Theme column; right-aligned like the Theme nodes above it.
    var aboutCol = lightMode ? '#5a6b7a' : '#c4d0da';
    return '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;' +
      'display:flex;align-items:center;justify-content:flex-end;padding:3px ' + (7 + nodeTextPad) + 'px;overflow:hidden;">' +
      '<span style="color:' + aboutCol + ';font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'font-weight:bold;text-align:right;line-height:1.25;padding-right:' + Math.max(14, accGutter) + 'px;position:relative;z-index:6;' + wrap + '">' + label + '</span>' +
      grad(gradientExtent) + border() + acc() + '</div>';
  }
  if (g === 'Project') {
    var ptypeRaw = data.ptype || '';
    var ptypeFi = { 'Text': 'Teksti', 'Text, long': 'Pitkä teksti', 'Text, short': 'Lyhyt teksti', 'Website': 'Nettisivu' };
    var ptypeLabel = dualLabel(ptypeRaw, ptypeFi[ptypeRaw] || ptypeRaw);
    var ptypeFontSize = (fontPtype + 2) + 'px';
    var nodeW = data.w || projectNodeWidth;
    var ptypeColW = (!mobileMode && ptypeRaw) ? Math.round(nodeW * ptypePct / 100) : 0;
    // Title side padding: LEFT clears the accordion chevron; RIGHT is minimal (projects have no right
    // chevron), so the node fits a tighter horizontal space. autoFitProjectWidth matches these.
    var projPadL = Math.max(9, accGutter) + nodeTextPad;
    var projPadR = 9 + nodeTextPad;
    var availW = nodeW - ptypeColW - projPadL - projPadR;
    var enText = data.label || '';
    var fiText = data.label_fi || enText;
    var enOneLine = measureTextPx(enText, fontNode) <= availW;
    var projFn = fontNode;
    if (enOneLine && currentLang === 'fi' && fiText !== enText) {
      var fiW = measureTextPx(fiText, fontNode);
      if (fiW > availW) projFn = Math.max(10, Math.floor(fontNode * availW / fiW));
    }
    var typeCol = (!mobileMode && ptypeRaw)
      ? '<div style="width:' + ptypeColW + 'px;flex-shrink:0;border-left:1.1px solid ' + colProject + ';' +
        'display:flex;align-items:center;justify-content:center;padding:0 5px;' +
        'color:' + colProject + ';font-family:Arial,Helvetica,sans-serif;font-size:' + ptypeFontSize + ';' +
        'font-weight:bold;text-align:center;line-height:1.25;position:relative;z-index:6;">' + ptypeLabel + '</div>'
      : '';
    return '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;overflow:hidden;' +
      'display:flex;align-items:stretch;">' +
      '<div style="flex:1;display:flex;align-items:center;justify-content:center;padding:4px ' + projPadR + 'px 4px ' + projPadL + 'px;' +
      'text-align:center;overflow:hidden;position:relative;z-index:6;">' +
      '<div style="color:' + colProject + ';font-family:Arial,Helvetica,sans-serif;font-size:' + projFn + 'px;' +
      'font-weight:bold;line-height:1.3;white-space:' + (enOneLine ? 'nowrap' : 'normal') + ';' + wrap + '">' + label + '</div></div>' +
      typeCol + grad(gradientExtent / 2) + border() + acc() + '</div>';
  }
  return '';
}

// Article controls under a node's description, shown when the feature is on and this node has an
// article. `d` is a description record carrying hasArticle/articleUrl and (for opted-in articles)
// articleInline — the body markdown for the on-node quick-read. Renders:
//   • "▸ Read here" toggle (only when articleInline exists) that unfolds the article in place, and
//   • "Read full article →" link to the external full-fidelity Quarto page.
// `open` controls whether the inline body is currently unfolded.
function articleControlsHtml(d, open) {
  if (!articlesEnabled || !d || !d.hasArticle) return '';
  var hasInline = !!(d.articleInline && String(d.articleInline).trim());
  var out = '<div class="inline-article-row">';
  if (hasInline) {
    var lbl = open ? ((currentLang === 'fi') ? '▾ Piilota' : '▾ Hide')
                   : ((currentLang === 'fi') ? '▸ Lue tästä' : '▸ Read here');
    out += '<span class="inline-article-expand" data-node-id="' + d.nodeId + '">' + lbl + '</span>';
  }
  if (d.articleUrl) {
    var label = (currentLang === 'fi') ? 'Lue koko artikkeli →' : 'Read full article →';
    out += '<a class="inline-article-link article-link" href="' + d.articleUrl + '">' + label + '</a>';
  }
  out += '</div>';
  if (hasInline && open)
    out += '<div class="inline-article-body" data-node-id="' + d.nodeId + '">' + mdToHtml(d.articleInline) + '</div>';
  return out;
}

// Rebuild the article-controls fragment for an expanded node from its stored article record + open state.
function inlineArticleHtml(ex) {
  return (ex && ex.art) ? articleControlsHtml(ex.art, !!ex.articleOpen) : '';
}

// Toggle the on-node article body for an expanded node: re-measure the node's height and reflow.
function toggleArticleInline(id) {
  var ex = inlineExpandedMap[String(id)]; if (!ex || !ex.art) return;
  if (articleEditId === String(id)) commitArticleEdit();   // folding while editing → commit first
  ex.articleOpen = !ex.articleOpen;
  ex.articleLink = inlineArticleHtml(ex);
  ex.descHtml = mdToHtml(ex.raw) + ex.articleLink;
  setExpandedHeight(id, false); reflowInline();
}

// Expanded inline node = the unchanged normal node as a fixed-height header (click it to close)
// + a selectable description below it (clicking the text selects/copies, doesn't collapse).
function inlineExpandedNodeHtml(data, exEntry) {
  var origH = exEntry.origH || (data.h || 46);
  var gpct = (data.group === 'Project') ? gradientExtent / 2 : gradientExtent;
  var descCol = lightMode ? 'rgba(0,0,0,0.86)' : 'rgba(255,255,255,0.9)';
  var lineCol = lightMode ? 'rgba(0,0,0,0.25)' : 'rgba(255,255,255,0.5)';
  var desc = '<div class="inline-node-desc" data-node-id="' + data.id + '" style="pointer-events:auto;user-select:text;-webkit-user-select:text;cursor:text;' +
    'border-top:1px solid ' + lineCol + ';' +   // thin line separating header from text
    'color:' + descCol + ';font-family:Arial,Helvetica,sans-serif;font-size:' + descFontSize + 'px;line-height:1.45;' +
    'padding:' + descPadCss() + ';position:relative;z-index:7;text-align:left;word-wrap:break-word;overflow-wrap:break-word;">' +
    exEntry.descHtml + '</div>';
  var copyBtn = '<div class="inline-copy-link" data-node-id="' + data.id + '" title="Copy link to this node" ' +
    'style="pointer-events:auto;cursor:pointer;position:absolute;top:3px;right:4px;z-index:8;font-size:13px;line-height:1;' +
    'padding:2px 5px;border-radius:4px;color:' + (lightMode ? 'rgba(0,0,0,0.55)' : 'rgba(255,255,255,0.8)') + ';' +
    'background:' + (lightMode ? 'rgba(0,0,0,0.08)' : 'rgba(0,0,0,0.28)') + ';">🔗</div>';
  return '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;display:flex;flex-direction:column;overflow:hidden;">' +
    '<div style="height:' + origH + 'px;flex-shrink:0;position:relative;overflow:hidden;z-index:6;">' + nodeBodyHtml(data, true) + '</div>' +
    desc + copyBtn + accordionIconHtml(true, data.group, origH) +
    gradientOverlay(data.id, gpct) + nodeBorderBandsHtml(data) + '</div>';   // gradient + source-color outline span the whole node
}

/* ── Inline UI mode: descriptions rendered inside the nodes ──────────────── */

// Measure the height (cyto units = CSS px at zoom 1) of just the description block, which sits
// full-width below the node's normal-content header. Total node height = header (origH) + this.
// Description padding: `descPad` is the horizontal inset; vertical scales from it (defaults to 6/10/8).
function descPadTop() { return Math.round(descPad * 0.6 + descPadFill); }   // descPadFill (extra-width fill) pads all sides
function descPadBot() { return Math.round(descPad * 0.8 + descPadFill); }
function descPadCss() { return descPadTop() + 'px ' + (descPad + descPadFill) + 'px ' + descPadBot() + 'px'; }
var _inlineProbe = null;
function measureDescHeight(node, descHtml) {
  if (!_inlineProbe) {
    _inlineProbe = document.createElement('div');
    _inlineProbe.style.cssText = 'position:absolute;left:-99999px;top:0;visibility:hidden;box-sizing:border-box;';
    document.body.appendChild(_inlineProbe);
  }
  var w = node.data('w') || 200;
  var wr = 'word-wrap:break-word;overflow-wrap:break-word;';
  _inlineProbe.style.width = Math.max(w - 2 * (descPad + descPadFill), 20) + 'px';  // desc horizontal padding each side
  _inlineProbe.innerHTML =
    '<div style="font-family:Arial,Helvetica,sans-serif;font-size:' + descFontSize + 'px;line-height:1.45;' + wr + '">' + descHtml + '</div>';
  return _inlineProbe.offsetHeight + descPadTop() + descPadBot() + 4; // desc vertical padding + buffer
}

// Height of the RAW markdown text (as shown in the editor), preserving newlines (pre-wrap).
function measureRawHeight(node, raw) {
  if (!_inlineProbe) {
    _inlineProbe = document.createElement('div');
    _inlineProbe.style.cssText = 'position:absolute;left:-99999px;top:0;visibility:hidden;box-sizing:border-box;';
    document.body.appendChild(_inlineProbe);
  }
  var w = node.data('w') || 200;
  _inlineProbe.style.width = Math.max(w - 2 * (descPad + descPadFill), 20) + 'px';
  _inlineProbe.innerHTML = '<div style="font-family:Arial,Helvetica,sans-serif;font-size:' + descFontSize +
    'px;line-height:1.45;white-space:pre-wrap;word-wrap:break-word;overflow-wrap:break-word;">' + (esc(raw) || '&nbsp;') + '</div>';
  return _inlineProbe.offsetHeight + descPadTop() + descPadBot() + 4;
}

// Update an expanded node's total height from its description (raw text while editing, rendered otherwise).
function setExpandedHeight(id, useRaw) {
  var ex = inlineExpandedMap[String(id)]; if (!ex || !cy) return;
  var node = cy.getElementById(String(id)); if (!node || node.empty()) return;
  ex.h = ex.origH + (useRaw ? measureRawHeight(node, ex.raw || '') : measureDescHeight(node, ex.descHtml));
}

/* ── Inline description editor (author app): a floating textarea over the node's desc region.
   It lives outside the Cytoscape overlay (which the plugin rebuilds every render), so the cursor
   survives reflow/scroll; we just keep it positioned over the node on each render. ── */
var descEditId = null;      // id of the node whose description is being edited (or null)
var descEditCancel = false; // set by Escape so the blur handler discards the edit
var _descEditor = null;

function ensureDescEditor() {
  if (_descEditor) return _descEditor;
  var ta = document.createElement('textarea');
  ta.id = 'inline-desc-editor'; ta.spellcheck = false;
  ta.style.cssText = 'position:absolute;z-index:20;display:none;box-sizing:border-box;resize:none;overflow:hidden;' +
    'border:1px solid rgba(120,180,255,0.85);border-radius:3px;outline:none;text-align:left;' +
    'font-family:Arial,Helvetica,sans-serif;transform-origin:top left;';
  ta.addEventListener('input', function () {
    var ex = inlineExpandedMap[String(descEditId)]; if (!ex) return;
    ex.raw = ta.value; setExpandedHeight(descEditId, true); reflowInline();  // grows the node; render re-aligns us
  });
  ta.addEventListener('blur', function () { commitDescEdit(); });
  ta.addEventListener('keydown', function (e) {
    e.stopPropagation();
    if (e.key === 'Escape') { e.preventDefault(); descEditCancel = true; ta.blur(); }
    else if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) { e.preventDefault(); ta.blur(); }  // Ctrl/Cmd+Enter commits
  });
  ta.addEventListener('wheel', function (e) { e.stopPropagation(); }, { passive: true });
  // Keep clicks/taps inside the editor from bubbling to the Cytoscape container, where they'd be
  // read as a tap on the node and collapse it (closing the editor). The textarea still handles them
  // natively, so clicking places the caret at that spot.
  ['mousedown', 'mouseup', 'click', 'dblclick', 'touchstart', 'touchend'].forEach(function (evt) {
    ta.addEventListener(evt, function (e) { e.stopPropagation(); }, false);
  });
  var ctr = cy && cy.container(); (ctr || document.body).appendChild(ta);
  _descEditor = ta; return ta;
}

function startDescEdit(id) {
  if (!cy) return;
  var ex = inlineExpandedMap[String(id)]; if (!ex) return;
  if (articleEditId) commitArticleEdit();
  if (descEditId && descEditId !== String(id)) commitDescEdit();
  var ta = ensureDescEditor();
  descEditId = String(id); descEditCancel = false;
  ex._origRaw = ex.raw || '';   // for Escape revert
  ta.value = ex.raw || '';
  ta.style.display = 'block';
  setExpandedHeight(id, true); reflowInline();
  positionDescEditor();
  ta.focus();
}

function positionDescEditor() {
  if (!descEditId || !_descEditor || !cy) return;
  var node = cy.getElementById(descEditId); if (!node || node.empty()) return;
  var ex = inlineExpandedMap[descEditId]; if (!ex) return;
  var zoom = cy.zoom(), pan = cy.pan(), pos = node.position();
  var w = node.data('w') || 200, h = node.data('h') || 46;
  var ta = _descEditor;
  ta.style.left = ((pos.x - w / 2) * zoom + pan.x) + 'px';
  ta.style.top  = ((pos.y - h / 2 + ex.origH) * zoom + pan.y) + 'px';
  ta.style.width = w + 'px';
  ta.style.height = Math.max(h - ex.origH, 16) + 'px';
  ta.style.transform = 'scale(' + zoom + ')';
  ta.style.fontSize = descFontSize + 'px';
  ta.style.lineHeight = '1.45';
  ta.style.padding = descPadCss();
  ta.style.color = lightMode ? 'rgba(0,0,0,0.86)' : 'rgba(255,255,255,0.92)';
  ta.style.background = (nodeBgSameAsGraph ? colBg : colNodeBg);
}

function commitDescEdit() {
  if (!descEditId) return;
  var id = descEditId, ta = _descEditor, ex = inlineExpandedMap[id];
  descEditId = null;
  if (ta) ta.style.display = 'none';
  if (!ex) return;
  if (!descEditCancel && ta) {
    ex.raw = ta.value;
    ex.articleLink = inlineArticleHtml(ex);   // rebuild controls (preserves any open article body)
    ex.descHtml = mdToHtml(ex.raw) + ex.articleLink;
    if (window.Shiny && Shiny.setInputValue)
      Shiny.setInputValue('node_desc_edit', { id: id, lang: ex.lang || 'en', text: ex.raw }, { priority: 'event' });
  } else if (descEditCancel) {
    ex.raw = ex._origRaw || '';   // revert unsaved changes
  }
  descEditCancel = false;
  setExpandedHeight(id, false); reflowInline();   // back to rendered height
}

/* ── Inline article editor (author app): edit an unfolded article's raw markdown in place, exactly
   like the description editor. Writes back to articles/<id>.qmd on the server (front matter kept).
   A floating textarea overlays the .inline-article-body region (found live in the overlay). ── */
var articleEditId = null;      // node id whose article body is being edited (or null)
var articleEditCancel = false; // set by Escape so the blur handler discards the edit
var _articleEditor = null;

function ensureArticleEditor() {
  if (_articleEditor) return _articleEditor;
  var ta = document.createElement('textarea');
  ta.id = 'inline-article-editor'; ta.spellcheck = false;
  ta.style.cssText = 'position:absolute;z-index:20;display:none;box-sizing:border-box;resize:none;overflow:auto;' +
    'border:1px solid rgba(120,180,255,0.85);border-radius:3px;outline:none;text-align:left;' +
    'font-family:Arial,Helvetica,sans-serif;transform-origin:top left;';
  ta.addEventListener('blur', function () { commitArticleEdit(); });
  ta.addEventListener('keydown', function (e) {
    e.stopPropagation();
    if (e.key === 'Escape') { e.preventDefault(); articleEditCancel = true; ta.blur(); }
    else if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) { e.preventDefault(); ta.blur(); }  // Ctrl/Cmd+Enter commits
  });
  ta.addEventListener('wheel', function (e) { e.stopPropagation(); }, { passive: true });  // scroll the editor, not the column
  ['mousedown', 'mouseup', 'click', 'dblclick', 'touchstart', 'touchend'].forEach(function (evt) {
    ta.addEventListener(evt, function (e) { e.stopPropagation(); }, false);
  });
  var ctr = cy && cy.container(); (ctr || document.body).appendChild(ta);
  _articleEditor = ta; return ta;
}

function startArticleEdit(id) {
  if (!cy) return;
  var ex = inlineExpandedMap[String(id)]; if (!ex || !ex.art || !ex.articleOpen) return;
  if (descEditId) commitDescEdit();
  if (articleEditId && articleEditId !== String(id)) commitArticleEdit();
  var ta = ensureArticleEditor();
  articleEditId = String(id); articleEditCancel = false;
  ex.art._origInline = ex.art.articleInline || '';   // for Escape revert
  ta.value = ex.art.articleInline || '';
  ta.style.display = 'block';
  positionArticleEditor();
  ta.focus();
}

// Keep the article editor aligned over its .inline-article-body region (re-run on every render).
function positionArticleEditor() {
  if (!articleEditId || !_articleEditor || !cy) return;
  var ctr = cy.container(); if (!ctr) return;
  var bodyEl = ctr.querySelector('.inline-article-body[data-node-id="' + articleEditId + '"]');
  if (!bodyEl) { _articleEditor.style.display = 'none'; return; }   // body scrolled out / collapsed
  var cr = ctr.getBoundingClientRect(), br = bodyEl.getBoundingClientRect();
  var zoom = cy.zoom(), ta = _articleEditor;
  ta.style.display = 'block';
  ta.style.left = (br.left - cr.left) + 'px';
  ta.style.top  = (br.top - cr.top) + 'px';
  ta.style.width  = (br.width  / zoom) + 'px';   // unscaled; transform scales it to match the region
  ta.style.height = (br.height / zoom) + 'px';
  ta.style.transform = 'scale(' + zoom + ')';
  ta.style.fontSize = descFontSize + 'px';
  ta.style.lineHeight = '1.45';
  ta.style.padding = '8px 4px 2px';
  ta.style.color = lightMode ? 'rgba(0,0,0,0.86)' : 'rgba(255,255,255,0.92)';
  ta.style.background = (nodeBgSameAsGraph ? colBg : colNodeBg);
}

function commitArticleEdit() {
  if (!articleEditId) return;
  var id = articleEditId, ta = _articleEditor, ex = inlineExpandedMap[id];
  articleEditId = null;
  if (ta) ta.style.display = 'none';
  if (!ex || !ex.art) return;
  if (!articleEditCancel && ta) {
    ex.art.articleInline = ta.value;
    ex.articleLink = inlineArticleHtml(ex);          // re-render the article body from the new markdown
    ex.descHtml = mdToHtml(ex.raw) + ex.articleLink;
    if (window.Shiny && Shiny.setInputValue && !Shiny._isStatic)
      Shiny.setInputValue('article_edit', { id: id, text: ex.art.articleInline }, { priority: 'event' });
  } else if (articleEditCancel) {
    ex.art.articleInline = ex.art._origInline || '';   // revert unsaved changes
    ex.articleLink = inlineArticleHtml(ex);
    ex.descHtml = mdToHtml(ex.raw) + ex.articleLink;
  }
  articleEditCancel = false;
  setExpandedHeight(id, false); reflowInline();        // node height follows the (re-rendered) body
}

// After an inline reflow: re-fit width + resize the scroll area (inline), else just redraw.
function inlineRefreshView() {
  if (inlineMode) layoutInlineScroll();
  else if (cy) { drawEdgeOverlay(); if (lastData) positionHeaders(lastData); }
}

// Snapshot every node's base (unexpanded) y + height; taken while nothing is expanded.
function captureInlineBase() {
  inlineBase = {};
  cy.nodes().forEach(function (n) {
    var g = n.data('group');
    if (!isColNode(g)) return;
    inlineBase[n.id()] = { y: n.position('y'), h: n.data('h') };
  });
}

// Recompute positions (stack expansions + per-column scroll) and re-fit the view.
// Report the currently-open inline node ids to the Shiny server (author app), so PNG export can
// reproduce the same open nodes. No-op on the static site (Shiny shim).
function syncOpenIdsToShiny() {
  if (window.Shiny && Shiny.setInputValue && !Shiny._isStatic)
    Shiny.setInputValue('inline_open_ids', Object.keys(inlineExpandedMap), { priority: 'event' });
}

function reflowInline() {
  if (!cy || !inlineBase) return;
  inlineRefreshView();   // layoutInlineScroll() re-fits and calls applyInlinePositions()
  syncOpenIdsToShiny();
}

// Toggle a node's inline expansion (same as tapping it): collapse if open, else request its content.
function toggleNodeInline(id) {
  if (inlineExpandedMap[String(id)]) { collapseNodeInline(id); return; }
  if (window.Shiny) Shiny.setInputValue('clicked_node_id', id, { priority: 'event' });
}

// Expand a node inline (keeping any others already open). `raw`/`lang`/`articleLink` support
// in-place description editing in the author app (see the inline description editor above).
// `skipReflow` lets a bulk "open all" add many nodes and reflow / update the URL just once.
function expandNodeInline(id, descHtml, raw, lang, articleLink, skipReflow, art) {
  if (!cy) return;
  var node = cy.getElementById(String(id));
  if (!node || node.empty()) return;
  if (!inlineBase) captureInlineBase();
  var base = inlineBase[String(id)];
  var origH = base ? base.h : node.data('h');
  var descH = measureDescHeight(node, descHtml);
  inlineExpandedMap[String(id)] = { descHtml: descHtml, h: origH + descH, origH: origH,
    raw: raw || '', lang: lang || 'en', articleLink: articleLink || '',
    art: art || null, articleOpen: false, openedAt: ++_openSeq };
  if (skipReflow) return;
  reflowInline();
  setNodeUrl(id);
  hoveredNodeId = String(id);   // highlight the just-opened node + its links, like a desktop hover
  applyHighlightState();
  if (String(id) === pendingScrollNode) { scrollColumnToNode(id); pendingScrollNode = null; }
  else if (autoFitOnOpen && autoFitArmed) autoFitOpenedNode(id);
}

// Zoom/pan so the just-opened node's text column fills the viewport width, then bring its top under the
// header. Reading is then vertical-scroll only — no horizontal panning. Gated by the author's
// "auto-fit on open" toggle and armed only after the first user interaction (so initial load still
// shows the whole map). Mainly for narrow screens, where the whole map fits too small to read.
function autoFitOpenedNode(id) {
  if (!cy || !inlineMode) return;
  var node = cy.getElementById(String(id)); if (!node || node.empty()) return;
  var ga = document.getElementById('graph-area'); if (!ga) return;
  var W = ga.clientWidth, viewH = ga.clientHeight || window.innerHeight;
  var baseBB = inlineBaseBBox(); if (!baseBB || baseBB.w === 0) return;
  var hm = (lastData && lastData.headerMargin) || 70;
  var fitW = (W - 40) / baseBB.w, fitH = (viewH - 28) / (baseBB.h + hm);   // match layoutInlineScroll's fit
  var fitZoom = isNarrow() ? fitW : Math.min(fitW, fitH);
  if (!(fitZoom > 0)) return;
  var w = node.data('w') || 200;
  uiZoom = Math.max(1, Math.min(uiZoomMax(), (W - 24) / (w * fitZoom)));  // node width → viewport width
  _zoomFocal = { cx: W / 2, contentX: node.position('x') };              // centre the node horizontally
  layoutInlineScroll();
  scrollColumnToNode(id);                                                // node top just under the header
}

// Restore every node to its base (unexpanded) position/height once nothing is expanded.
function restoreInlineBase() {
  if (cy && inlineBase) cy.nodes().forEach(function (n) {
    var b = inlineBase[n.id()]; if (b) { n.data('h', b.h); n.position('y', b.y); }
  });
  inlineBase = null;
  inlineColScroll = { Theme: 0, Project: 0, Skill: 0 };
  inlineColShiftUp = { Theme: 0, Project: 0, Skill: 0 };
  if (cy) { cy.emit('render'); inlineRefreshView(); }
  syncOpenIdsToShiny();
}

// Collapse one node (id given) or all (id omitted). Restores base layout when none remain.
function collapseNodeInline(id) {
  if (descEditId && (id == null || String(id) === descEditId)) commitDescEdit();
  if (articleEditId && (id == null || String(id) === articleEditId)) commitArticleEdit();
  if (id == null) inlineExpandedMap = {};
  else delete inlineExpandedMap[String(id)];
  if (Object.keys(inlineExpandedMap).length === 0) restoreInlineBase();
  else reflowInline();
  var remaining = Object.keys(inlineExpandedMap);
  setNodeUrl(remaining.length ? remaining[remaining.length - 1] : null);
  hoveredNodeId = remaining.length ? String(remaining[remaining.length - 1]) : null;   // highlight the new last-opened
  applyHighlightState();
}

// Clear inline expansion state without moving anything (used when the graph is rebuilt).
function resetInlineExpansion() { inlineExpandedMap = {}; inlineBase = null; inlineColScroll = { Theme: 0, Project: 0, Skill: 0 }; inlineColShiftUp = { Theme: 0, Project: 0, Skill: 0 }; }

// Expand a node inline from a description record
// {nodeId, text, text_fi, group, hasArticle, articleUrl, articleInline}.
function expandNodeFromDesc(d, skipReflow) {
  var useFi = (currentLang === 'fi' && d.text_fi != null && d.text_fi !== '');
  var dText = useFi ? d.text_fi : (d.text || '');
  var dLang = useFi ? 'fi' : 'en';
  var art = { nodeId: d.nodeId, hasArticle: !!d.hasArticle,
              articleUrl: d.articleUrl || '', articleInline: d.articleInline || '' };
  var artHtml = articleControlsHtml(art, false);
  expandNodeInline(d.nodeId, mdToHtml(dText) + artHtml, dText, dLang, artHtml, skipReflow, art);
}

// Collapse every open inline node, or only those of `group` ('Theme'|'Project'|'Skill') when given.
function collapseAllInline(group) {
  if (!inlineMode || !cy) return;
  if (!group) { collapseNodeInline(); return; }
  var ids = Object.keys(inlineExpandedMap).filter(function (id) {
    var n = cy.getElementById(id); return n && !n.empty() && n.data('group') === group;
  });
  if (!ids.length) return;
  ids.forEach(function (id) {
    if (descEditId && String(descEditId) === id) commitDescEdit();
    delete inlineExpandedMap[id];
  });
  if (Object.keys(inlineExpandedMap).length === 0) restoreInlineBase();
  else reflowInline();
  var rem = Object.keys(inlineExpandedMap);
  setNodeUrl(rem.length ? rem[rem.length - 1] : null);
  hoveredNodeId = rem.length ? String(rem[rem.length - 1]) : null;   // highlight the new last-opened
  applyHighlightState();
}

// Open every openable node inline, or only those of `group` when given. Static site has all
// descriptions client-side; the Shiny apps answer a request with a single expandAllInline batch.
function openAllInline(group) {
  if (!cy || !inlineMode) return;
  if (window.staticNodeDescs) {
    var nodes = [];
    cy.nodes().forEach(function (n) {
      var g = n.data('group'); if (!isColNode(g)) return;
      if (group && g !== group) return;
      var d = window.staticNodeDescs[n.id()]; if (d) nodes.push(Object.assign({ nodeId: n.id() }, d));
    });
    Shiny._handlers['expandAllInline']({ nodes: nodes });
  } else if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue('open_all_nodes', { group: group || '', t: Date.now() }, { priority: 'event' });
  }
}

// ── Deep links: open a specific node via ?node=<id>, and reflect the open node in the URL ──
function setNodeUrl(id) {
  var url = new URL(window.location.href);
  url.hash = (id == null || id === '') ? '' : String(id);  // e.g. .../interests/#305 (dots are fine)
  window.history.replaceState(null, '', url.toString());
}

// Open a node programmatically (same path as a click): expands it inline / opens the sidebar.
function openNodeById(id) {
  if (!cy || id == null) return;
  var node = cy.getElementById(String(id));
  if (!node || node.empty()) return;
  var g = node.data('group');
  if (!isColNode(g)) return;
  if (!inlineMode) selectNode(parseFloat(id));
  if (window.Shiny) Shiny.setInputValue('clicked_node_id', parseFloat(id), { priority: 'event' });
}

// Expand the node flagged openDefault in the graph data (inline desktop), when nothing else is
// pending/open — used on load so the "What is this site about" node starts open.
function openDefaultInline() {
  if (!cy || !inlineMode || mobileMode) return;
  if (pendingNodeParam || Object.keys(inlineExpandedMap).length) return;
  var target = null;
  cy.nodes().forEach(function (n) { if (!target && n.data('openDefault')) target = n; });
  if (target) { pendingScrollNode = null; openNodeById(target.id()); }
}

// Scroll the node's column so the node sits near the top of the view (inline mode).
function scrollColumnToNode(id) {
  if (!cy || !inlineBase) return;
  var node = cy.getElementById(String(id));
  if (!node || node.empty()) return;
  var g = stackCol(node.data('group'));
  var bb = inlineBaseBBox(); if (!bb) return;
  var curOff = inlineColScroll[g] || 0;
  var stackedTop = node.position('y') + curOff - node.data('h') / 2;
  inlineColScroll[g] = stackedTop - bb.y1;   // bring the node top to the top of the content area
  applyInlinePositions();                    // clamps to the scrollable range
  cy.emit('render');
  if (lastData) positionHeaders(lastData);   // headers scroll with their column
  drawEdgeOverlay();
}

/* ── Description Panel / Bottom Sheet ────────────────────────────────────── */

function hideDescPanel() {
  if (inlineMode) collapseNodeInline();
  else setNodeUrl(null);
  lastDescMsg = null;
  // Desktop: hide sidebar panel
  var p = document.getElementById('desc-panel'); if (p) p.style.display = 'none';
  var accDescEl = document.getElementById('acc-desc');
  if (accDescEl) {
    accDescEl.classList.remove('desc-visible');
    var ab = accDescEl.querySelector('.acc-body'); if (ab) ab.style.height = '';
  }
  // Mobile: hide bottom sheet
  hideBottomSheet();
  // Restore hint text
  var hint = document.getElementById('sidebar-hint'); if (hint) hint.style.display = '';
  selectedNodeId = null; applyHighlightState();
}

function hideBottomSheet() {
  var bs = document.getElementById('mobile-bottom-sheet');
  if (bs) { bs.classList.remove('visible'); bs.style.maxHeight = '50vh'; }
  sheetMode = null;
  mobCloseDesc();
}

function showBottomSheet() { /* mob-panel is always visible; no-op */ }
function updateInfoBtnVisibility() { /* info button removed; no-op */ }

function showInfoSheet() {
  mobShowTab('about');
  selectedNodeId = null; applyHighlightState();
}

/* ── Mobile tab / description helpers ───────────────────────────────────── */

function mobShowTab(tab) {
  ['about','vote','fund','settings'].forEach(function(t) {
    var btn  = document.getElementById('mob-tab-' + t);
    var pane = document.getElementById('mob-content-' + t);
    var on   = (t === tab);
    if (btn)  btn.classList.toggle('mob-tab-active', on);
    if (pane) pane.classList.toggle('mob-tab-pane-active', on);
  });
}

function initSettingsTab() {
  var el = document.getElementById('mob-content-settings');
  if (!el) return;
  var gb = document.getElementById('github-btn');
  var githubHref = gb ? gb.getAttribute('href') : '#';
  var modeIcon = lightMode ? '\u263d' : '\u2600';
  var secStyle = 'margin-bottom:20px;';
  var lbl = 'font-size:11px;opacity:0.55;margin-bottom:8px;text-transform:uppercase;' +
    'letter-spacing:0.05em;font-family:Arial,Helvetica,sans-serif;display:block;';
  el.innerHTML =
    '<div style="padding:16px;">' +
    '<div style="' + secStyle + '">' +
    '<span style="' + lbl + '">Language</span>' +
    '<div style="display:flex;gap:8px;">' +
    '<button id="mob-lang-btn-en" class="lang-btn' + (currentLang === 'en' ? ' lang-active' : '') + '" onclick="setLanguage(\'en\')"><span class="fi fi-gb"></span></button>' +
    '<button id="mob-lang-btn-fi" class="lang-btn' + (currentLang === 'fi' ? ' lang-active' : '') + '" onclick="setLanguage(\'fi\')"><span class="fi fi-fi"></span></button>' +
    '</div></div>' +
    '<div style="' + secStyle + '">' +
    '<span style="' + lbl + '"><span class="en-only">Appearance</span><span class="fi-only">Ulkoasu</span></span>' +
    '<button id="mob-mode-btn" onclick="toggleLightMode()" style="font-size:20px;line-height:1;background:none;border:none;cursor:pointer;padding:0;color:inherit;">' + modeIcon + '</button>' +
    '</div>' +
    (githubHref && githubHref !== '#' ?
      '<div style="' + secStyle + '">' +
      '<span style="' + lbl + '">GitHub</span>' +
      '<a href="' + githubHref + '" target="_blank" rel="noopener" style="color:inherit;opacity:0.8;font-family:Arial,Helvetica,sans-serif;font-size:14px;text-decoration:underline;">View on GitHub</a>' +
      '</div>' : '') +
    '</div>';
}

function mobOpenDesc() {
  var p = document.getElementById('mob-desc-panel');
  if (p) p.classList.add('mob-desc-visible');
}

function mobCloseDesc() {
  var p = document.getElementById('mob-desc-panel');
  if (p) p.classList.remove('mob-desc-visible');
  if (sheetMode === 'desc') sheetMode = null;
}

function syncMobileTabs() {
  var map = { 'acc-about-body': 'mob-content-about', 'acc-vote-body': 'mob-content-vote', 'acc-fund-body': 'mob-content-fund' };
  Object.keys(map).forEach(function(srcId) {
    var src = document.getElementById(srcId);
    var dst = document.getElementById(map[srcId]);
    if (src && dst) dst.innerHTML = src.innerHTML;
  });
}

/* ── Mobile split-pane drag handle ──────────────────────────────────────── */

(function () {
  var dragging = false, startY = 0, startH = 0;
  document.addEventListener('DOMContentLoaded', function () {
    var handle = document.getElementById('mob-handle');
    var ga     = document.getElementById('graph-area');
    if (!handle || !ga) return;

    function onStart(y) {
      dragging = true; startY = y; startH = ga.offsetHeight;
      document.body.style.userSelect = 'none';
    }
    function onMove(y) {
      if (!dragging) return;
      var total = window.innerHeight;
      var newH = Math.max(total * 0.2, Math.min(total * 0.85, startH + (y - startY)));
      ga.style.height = newH + 'px';
      resizeCy(); positionHeaders(lastData);
    }
    function onEnd() {
      if (!dragging) return;
      dragging = false; document.body.style.userSelect = '';
      resizeCy();
    }
    handle.addEventListener('mousedown',   function(e) { e.preventDefault(); onStart(e.clientY); });
    document.addEventListener('mousemove', function(e) { if (dragging) onMove(e.clientY); });
    document.addEventListener('mouseup',   onEnd);
    handle.addEventListener('touchstart',  function(e) { e.preventDefault(); onStart(e.touches[0].clientY); }, { passive:false });
    document.addEventListener('touchmove', function(e) { if (dragging) { e.preventDefault(); onMove(e.touches[0].clientY); } }, { passive:false });
    document.addEventListener('touchend',  onEnd);
  });

  // Watch sidebar accordion bodies and sync to mobile panes when content changes
  document.addEventListener('DOMContentLoaded', function () {
    var pairs = [
      ['acc-about-body', 'mob-content-about'],
      ['acc-vote-body',  'mob-content-vote'],
      ['acc-fund-body',  'mob-content-fund']
    ];
    pairs.forEach(function(pair) {
      var src = document.getElementById(pair[0]);
      if (!src) return;
      new MutationObserver(function() {
        var dst = document.getElementById(pair[1]);
        if (dst) dst.innerHTML = src.innerHTML;
      }).observe(src, { childList: true, subtree: true });
    });
  });
})();

// Sync mobile tabs when Shiny renders sidebar accordion content
if (window.jQuery) {
  jQuery(document).on('shiny:value', function(event) {
    if (event.name === 'col_intro_ui' || event.name === 'vote_section_ui' || event.name === 'funding_ui') {
      setTimeout(syncMobileTabs, 100);
    }
  });
}

/* ── Independent Highlight: selected + hovered coexist ───────────────────── */

function applyHighlightState() {
  if (!cy) return;
  cy.elements('.selected').removeClass('selected');
  cy.elements('.hovered').removeClass('hovered');
  cy.elements('.nbr-hi').removeClass('nbr-hi');
  nodeGradients = {};
  nodeHoverGradients = {};
  function gradSide(srcGrp, dstGrp) {
    if (srcGrp === 'Theme'   && dstGrp === 'Project') return 'left';
    if (srcGrp === 'Skill'   && dstGrp === 'Project') return 'right';
    if (srcGrp === 'Project' && dstGrp === 'Theme')   return 'right';
    if (srcGrp === 'Project' && dstGrp === 'Skill')   return 'left';
    return null;
  }
  function oppSide(s) { return s === 'left' ? 'right' : 'left'; }
  function mergeHovGrad(map, id, side, color) {
    var cur = map[id];
    if (!cur) { map[id] = { side: side, color: color }; return; }
    if (cur.side !== side) cur.side = 'both';
  }
  if (selectedNodeId) {
    var sn = cy.getElementById(String(selectedNodeId));
    if (sn && !sn.empty()) {
      sn.addClass('selected');
      sn.connectedEdges().addClass('selected');
      var selGrp = sn.data('group');
      // Selected project lights up in its connecting edge colors (stronger + wider bands); Theme/Skill keep their base colored gradient
      if (selGrp === 'Project')
        nodeGradients[String(selectedNodeId)] = { bands: projectBandColors(sn, GRAD_ALPHA_SELECT), widthMult: 2 };
      sn.connectedEdges().forEach(function(edge) {
        var otherId = edge.data('source') === String(selectedNodeId) ? edge.data('target') : edge.data('source');
        var on = cy.getElementById(otherId); if (!on || on.empty()) return;
        on.addClass('nbr-hi');
        var side = gradSide(selGrp, on.data('group'));
        if (!side) return;
        // Connected node lights up in the connecting edge's source color (stronger + wider than its base gradient)
        var rawSel = (lightMode ? edge.data('lightColor') : edge.data('color')) || (lightMode ? '#000000' : '#ffffff');
        var selG = { side: side, color: hexRgba(rawSel, GRAD_ALPHA_SELECT), widthMult: 2 };
        // When the selected node is a Theme/Skill, confine the project's highlight to that node's band
        if (on.data('group') === 'Project' && (selGrp === 'Theme' || selGrp === 'Skill')) {
          var bi = projectBandIndex(on, selGrp, selectedNodeId);
          if (bi.count > 0 && bi.index >= 0) { var bg = projectBandGeom(on.id(), bi.count)[bi.index]; selG.top = bg.top; selG.height = bg.height; }
        }
        nodeGradients[otherId] = selG;
      });
    }
  }
  if (hoveredNodeId) {
    var hn = cy.getElementById(String(hoveredNodeId));
    if (hn && !hn.empty()) {
      hn.addClass('hovered');
      hn.connectedEdges().addClass('hovered');
      var hovGrp = hn.data('group');
      hn.connectedEdges().forEach(function(edge) {
        var otherId = edge.data('source') === String(hoveredNodeId) ? edge.data('target') : edge.data('source');
        var on2 = cy.getElementById(otherId); if (!on2 || on2.empty()) return;
        on2.addClass('nbr-hi');
        var og2 = on2.data('group');
        var side2 = gradSide(hovGrp, og2);
        if (!side2) return;
        var rawHov = (lightMode ? edge.data('lightColor') : edge.data('color')) || (lightMode ? '#000000' : '#ffffff');
        var edgeCol = hexRgba(rawHov, GRAD_ALPHA_BASE);  // hover reuses the base gradient (same opacity), just wider
        // When hovering a Theme/Skill, confine the project's highlight to that node's band; else full-height
        if (og2 === 'Project' && (hovGrp === 'Theme' || hovGrp === 'Skill')) {
          var bi2 = projectBandIndex(on2, hovGrp, hoveredNodeId);
          if (bi2.count > 0 && bi2.index >= 0) {
            var bg2 = projectBandGeom(on2.id(), bi2.count)[bi2.index];
            nodeHoverGradients[otherId] = { side: side2, color: edgeCol, top: bg2.top, height: bg2.height };
          } else {
            mergeHovGrad(nodeHoverGradients, otherId, side2, edgeCol);
          }
        } else {
          mergeHovGrad(nodeHoverGradients, otherId, side2, edgeCol);
        }
        // Theme/Skill self lights up in the edge color; Project self uses banded edge colors (set below)
        if (hovGrp !== 'Project')
          mergeHovGrad(nodeHoverGradients, String(hoveredNodeId), oppSide(side2), edgeCol);
      });
      // Hovered project lights up in its connecting edge colors (wider bands), not the orange group color
      if (hovGrp === 'Project')
        nodeHoverGradients[String(hoveredNodeId)] = { bands: projectBandColors(hn, GRAD_ALPHA_BASE) };
    }
  }
  cy.forceRender();
  drawEdgeOverlay();
  drawNodeConnector();
}

function selectNode(nodeId) { selectedNodeId = nodeId; applyHighlightState(); }
function clearSelection() { selectedNodeId = null; applyHighlightState(); }

function toggleLightMode() {
  lightMode = !lightMode;
  if (typeof Shiny !== 'undefined' && !Shiny._isStatic)
    Shiny.setInputValue('light_mode_active', lightMode, {priority: 'event'});
  var btn = document.getElementById('mode-btn');
  var mobBtn = document.getElementById('mob-mode-btn');
  if (lightMode) {
    colBg = lightColBg; colSidebarBg = lightColSidebarBg; colNodeBg = lightColNodeBg;
    colTheme = lightColTheme; colProject = lightColProject; colSkill = lightColSkill;
    if (btn) btn.textContent = '\u263d'; // crescent for "go dark"
    if (mobBtn) mobBtn.textContent = '\u263d';
  } else {
    colBg = darkColBg; colSidebarBg = darkColSidebarBg; colNodeBg = darkColNodeBg;
    colTheme = darkColTheme; colProject = darkColProject; colSkill = darkColSkill;
    if (btn) btn.textContent = '\u2600'; // sun for "go light"
    if (mobBtn) mobBtn.textContent = '\u2600';
  }
  applyColors();
  if (cy) { cy.style(buildStyle()); if (lastData) positionHeaders(lastData); buildBaseGradients(); drawEdgeOverlay(); cy.trigger('render'); }
  // Re-apply description panel accent color with updated globals
  if (lastDescMsg) {
    var c = descAccentColor(lastDescMsg);
    var panel = document.getElementById('desc-panel');
    var title = document.getElementById('desc-title');
    var close = document.getElementById('desc-close');
    if (panel && panel.style.display !== 'none') {
      panel.style.borderColor = c;
      if (title) title.style.color = c;
      if (close) { close.style.color = c; close.style.borderColor = c; }
    }
    var mdPanel = document.getElementById('mob-desc-panel');
    if (mdPanel && mdPanel.classList.contains('mob-desc-visible')) {
      var mdTitle = document.getElementById('mob-desc-title');
      var mdClose = document.getElementById('mob-desc-close');
      if (mdTitle) mdTitle.style.color = c;
      if (mdClose) { mdClose.style.color = c; mdClose.style.borderColor = c; }
    }
  }
}

/* ── Edge SVG Overlay ────────────────────────────────────────────────────── */

function drawEdgeOverlay() {
  var area = document.getElementById('graph-area');
  var oldSvg = document.getElementById('edge-overlay'); if (oldSvg) oldSvg.remove();
  if (!cy) return;
  var ctr = cy.container(), w = ctr.clientWidth, h = ctr.clientHeight;
  var pan = cy.pan(), zoom = cy.zoom();
  var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.id = 'edge-overlay'; svg.setAttribute('width', w); svg.setAttribute('height', h);
  svg.style.cssText = 'position:absolute;top:0;left:0;pointer-events:none;z-index:5;overflow:visible;';
  area.insertBefore(svg, document.getElementById('cy'));

  // Pass 1: collect raw endpoint data for each edge (in cyto units for Y, screen px for X)
  // For band mode, edges attach within a node's HEADER (base) height — an inline-expanded node grows
  // downward, so bands stay pinned to the header rather than fanning across the tall description box.
  var rawEdges = [];
  function attachGeom(node) {
    var h = node.data('h') || 46, cyy = node.position().y;
    var ex = inlineMode ? inlineExpandedMap[node.id()] : null;
    return { baseH: ex ? (ex.origH || h) : h, fullH: h, cy: cyy };  // baseH = collapsed height (band thickness budget)
  }
  cy.edges().forEach(function (edge) {
    var d = edge.data();
    var src = cy.getElementById(d.source), tgt = cy.getElementById(d.target);
    if (!src || src.empty() || !tgt || tgt.empty()) return;
    var sp = src.position(), tp = tgt.position();
    var sw = src.data('w') || 160, tw = tgt.data('w') || 160;
    var srcEp = d.srcEp || '', tgtEp = d.tgtEp || '';
    var syBase = sp.y, tyBase = tp.y;
    if (srcEp.indexOf('px') >= 0) { var sp2 = srcEp.split(/\s+/); syBase = sp.y + parseFloat(sp2[1] || '0'); }
    if (tgtEp.indexOf('px') >= 0) { var tp2 = tgtEp.split(/\s+/); tyBase = tp.y + parseFloat(tp2[1] || '0'); }
    var sa = attachGeom(src), ta = attachGeom(tgt);
    // The Theme/Skill endpoint — hovering/clicking this edge highlights it (like hovering that node).
    var sGrp = src.data('group');
    var hlId = (sGrp === 'Theme' || sGrp === 'Skill') ? String(d.source) : String(d.target);
    rawEdges.push({
      d: d, edge: edge, hlId: hlId,
      sx: (sp.x + sw / 2) * zoom + pan.x,
      tx: (tp.x - tw / 2) * zoom + pan.x,
      syBase: syBase, tyBase: tyBase,
      syOff: 0, tyOff: 0,
      srcCy: sa.cy, srcBaseH: sa.baseH, srcFullH: sa.fullH,   // band thickness from base height; spread over full height
      tgtCy: ta.cy, tgtBaseH: ta.baseH, tgtFullH: ta.fullH
    });
  });

  // Band (ribbon) mode: edges fill each node's height in stacked bands, tapering thin between nodes.
  if (edgeBands) { drawEdgeBands(svg, rawEdges, zoom, pan); return; }

  // Pass 2: spread endpoints that share a node wall, sorted by other-end Y.
  // Center-to-center spacing = line width + 5px clear gap, so there's always 5px of
  // whitespace BETWEEN lines regardless of their width. Hover is ignored here so edges
  // don't shift (and cause hover flicker) when the pointer moves over them.
  var clearGapCyto = 3 / zoom;                              // 3 screen-px of whitespace between lines
  var baseWCyto = baseEdgeWidth * (mobileMode ? 1.5 : 1);   // cyto-unit stroke width (strokeW = this * zoom)
  function spacingWCyto(re) { return baseWCyto * (re.edge.hasClass('selected') ? 1.4 : 1); }
  var wallGroups = {};
  rawEdges.forEach(function(re, i) {
    var w = spacingWCyto(re);
    var kr = re.d.source + ':r', kl = re.d.target + ':l';
    if (!wallGroups[kr]) wallGroups[kr] = [];
    if (!wallGroups[kl]) wallGroups[kl] = [];
    wallGroups[kr].push({ i: i, baseY: re.syBase, otherY: re.tyBase, isRight: true,  w: w });
    wallGroups[kl].push({ i: i, baseY: re.tyBase, otherY: re.syBase, isRight: false, w: w });
  });
  Object.keys(wallGroups).forEach(function(key) {
    var grp = wallGroups[key];
    if (grp.length < 2) return;
    grp.sort(function(a, b) { return a.otherY - b.otherY; });
    var n = grp.length;
    var maxW = grp.reduce(function(m, g) { return Math.max(m, g.w); }, 0);
    var stepCyto = maxW + clearGapCyto;                     // center-to-center spacing
    var totalSpan = (n - 1) * stepCyto;
    var centerY = grp.reduce(function(s, g) { return s + g.baseY; }, 0) / n;
    grp.forEach(function(g, idx) {
      var delta = (centerY - totalSpan / 2 + idx * stepCyto) - g.baseY;
      if (g.isRight) rawEdges[g.i].syOff += delta;
      else           rawEdges[g.i].tyOff += delta;
    });
  });

  // Pass 3: build path strings with spread-adjusted Y values
  var edgePaths = [];
  rawEdges.forEach(function(re) {
    var x1 = re.sx, y1 = (re.syBase + re.syOff) * zoom + pan.y;
    var x2 = re.tx, y2 = (re.tyBase + re.tyOff) * zoom + pan.y;
    var cx1 = x1 + (x2 - x1) * 0.45, cx2 = x2 - (x2 - x1) * 0.45;
    edgePaths.push({
      pathD: 'M' + x1 + ',' + y1 + ' C' + cx1 + ',' + y1 + ' ' + cx2 + ',' + y2 + ' ' + x2 + ',' + y2,
      color: re.d.color || '#ffffff', lightColor: re.d.lightColor || lightEdgeColor, dashes: re.d.dashes,
      isSel: re.edge.hasClass('selected'), isHov: re.edge.hasClass('hovered'), hlId: re.hlId
    });
  });
  var strokeW = baseEdgeWidth * zoom * (mobileMode ? 1.5 : 1);
  function makePath(d, stroke, width, opacity, dashes, hlId) {
    var p = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    p.setAttribute('d', d); p.setAttribute('fill', 'none');
    p.setAttribute('stroke', stroke); p.setAttribute('stroke-width', width);
    if (opacity != null) p.setAttribute('opacity', opacity);
    if (dashes) p.setAttribute('stroke-dasharray', (5 * zoom) + ',' + (3 * zoom));
    if (hlId != null) { p.setAttribute('data-hl', hlId); p.style.pointerEvents = 'stroke'; }  // hover/click target
    return p;
  }
  var anySel = edgePaths.some(function (ep) { return ep.isSel; });
  var NORM_OP = edgeOpacity;  // default edge opacity (author-controllable)
  var DIM_OP = edgeOpacity * (lightMode ? 0.6 : 0.82);  // faded context when a node is selected
  if (anySel) {
    // Focus mode: dim everything, then emphasize the selected path on top
    // Bottom: non-selected, non-hovered edges — faded to quiet context
    edgePaths.forEach(function (ep) {
      if (ep.isSel || ep.isHov) return;
      svg.appendChild(makePath(ep.pathD, lightMode ? ep.lightColor : ep.color, strokeW, DIM_OP, ep.dashes, ep.hlId));
    });
    // Hovered (not selected) — keep hover preview working during a selection
    edgePaths.forEach(function (ep) {
      if (!ep.isHov || ep.isSel) return;
      svg.appendChild(makePath(ep.pathD, lightMode ? ep.lightColor : ep.color, strokeW * 2.25, NORM_OP, ep.dashes, ep.hlId));
    });
    // Top: selected edges — full color, fully opaque, slightly thicker
    edgePaths.forEach(function (ep) {
      if (!ep.isSel) return;
      svg.appendChild(makePath(ep.pathD, lightMode ? ep.lightColor : ep.color, strokeW * 1.4, 1, ep.dashes, ep.hlId));
    });
  } else {
    // No selection: hovered edge sits below normal edges so other connections stay visible
    edgePaths.forEach(function (ep) {
      if (!ep.isHov) return;
      svg.appendChild(makePath(ep.pathD, lightMode ? ep.lightColor : ep.color, strokeW * 2.25, NORM_OP, ep.dashes, ep.hlId));
    });
    edgePaths.forEach(function (ep) {
      if (ep.isHov) return;
      svg.appendChild(makePath(ep.pathD, lightMode ? ep.lightColor : ep.color, strokeW, NORM_OP, ep.dashes, ep.hlId));
    });
  }
}

// Ribbon edges: at each node wall the connected edges stack to fill the node's height in equal bands
// (with `edgeGap` px between them); each edge is band-height at both nodes and pinches to the base
// edge width in the horizontal middle ("thick at nodes, thin in the middle").
function drawEdgeBands(svg, rawEdges, zoom, pan) {
  var SVGNS = 'http://www.w3.org/2000/svg';
  var gapC = (edgeGap || 0) / zoom;   // gap in cyto units

  // Assign each edge a band (center Y + height, cyto units) on its source-right and target-left walls.
  var walls = {};
  rawEdges.forEach(function (re, i) {
    var kr = re.d.source + ':r', kl = re.d.target + ':l';
    if (!walls[kr]) walls[kr] = { baseH: re.srcBaseH, fullH: re.srcFullH, cy: re.srcCy, items: [] };
    if (!walls[kl]) walls[kl] = { baseH: re.tgtBaseH, fullH: re.tgtFullH, cy: re.tgtCy, items: [] };
    walls[kr].items.push({ i: i, otherY: re.tyBase, end: 's' });
    walls[kl].items.push({ i: i, otherY: re.syBase, end: 't' });
  });
  Object.keys(walls).forEach(function (key) {
    var wg = walls[key], items = wg.items, n = items.length;
    items.sort(function (a, b) { return a.otherY - b.otherY; });   // order by other end to avoid crossings
    // Fill the whole border by default; pin-header fills only the header (collapsed) height instead.
    var H = edgePinHeader ? wg.baseH : wg.fullH;
    var g = gapC, band = (H - (n - 1) * g) / n;
    if (band < 1) { band = H / n; g = 0; }
    var top = wg.cy - wg.fullH / 2;                                // node top (header is at the top)
    items.forEach(function (it, idx) {
      var center = top + idx * (band + g) + band / 2;
      if (it.end === 's') { rawEdges[it.i].sBandY = center; rawEdges[it.i].sBandH = band; }
      else                { rawEdges[it.i].tBandY = center; rawEdges[it.i].tBandH = band; }
    });
  });

  var minHalf = edgeMinOn ? Math.max(edgeMinWidth * (mobileMode ? 1.5 : 1) / 2, 0.25) : 0;  // thinnest mid-span half-width (0 = not applied)
  var anySel = rawEdges.some(function (re) { return re.edge.hasClass('selected'); });
  var NORM_OP = edgeOpacity, DIM_OP = edgeOpacity * (lightMode ? 0.6 : 0.82);

  function ribbon(re, op) {
    var color = lightMode ? (re.d.lightColor || lightEdgeColor) : (re.d.color || '#ffffff');
    var x1 = re.sx, x2 = re.tx;
    var y1 = re.sBandY * zoom + pan.y, y2 = re.tBandY * zoom + pan.y;
    var hsH = re.sBandH * zoom / 2, htH = re.tBandH * zoom / 2;
    var mFloor = minHalf * zoom;                          // minimum half-width (Sankey floor)
    var mPinch = Math.min(mFloor, hsH, htH);              // pinch target (band mode) — never wider than the ends
    var cx1 = x1 + (x2 - x1) * 0.45, cx2 = x2 - (x2 - x1) * 0.45;
    function bez(t, a, b, c, d) { var u = 1 - t; return u*u*u*a + 3*u*u*t*b + 3*u*t*t*c + t*t*t*d; }
    function sm(a, b, s) { return a + (b - a) * (s * s * (3 - 2 * s)); }
    // Half-width (vertical) along the ribbon. Offset is vertical (not perpendicular) so ribbons stay
    // uniform and don't warp on steep edges (e.g. while scrolling a column).
    function halfW(t) {
      if (edgeSankey) return Math.max(mFloor, sm(hsH, htH, t));  // Sankey: taper source->target, but never thinner than the slider
      // Band mode: taper into the mid-span pinch with an author-controlled curve exponent
      // (edgeCurve): 1 = straight, >1 stays wide then plunges, <1 plunges early then eases in.
      var k = edgeCurve > 0 ? edgeCurve : 1;
      if (t <= 0.5) { var a = t / 0.5;         return hsH + (mPinch - hsH) * Math.pow(a, k); }
      var b = (t - 0.5) / 0.5;                 return mPinch + (htH - mPinch) * (1 - Math.pow(1 - b, k));
    }
    var N = 26, tp = [], bt = [];
    for (var k = 0; k <= N; k++) {
      var t = k / N;
      var x = bez(t, x1, cx1, cx2, x2), y = bez(t, y1, y1, y2, y2), hw = halfW(t);
      tp.push(x.toFixed(1) + ',' + (y - hw).toFixed(1));
      bt.push(x.toFixed(1) + ',' + (y + hw).toFixed(1));
    }
    var d = 'M' + tp.join(' L') + ' L' + bt.reverse().join(' L') + ' Z';
    var p = document.createElementNS(SVGNS, 'path');
    p.setAttribute('d', d); p.setAttribute('fill', color); p.setAttribute('stroke', 'none'); p.setAttribute('opacity', op);
    if (re.hlId != null) { p.setAttribute('data-hl', re.hlId); p.style.pointerEvents = 'auto'; }  // hover/click target
    return p;
  }

  function ok(re) { return re.sBandY != null && re.tBandY != null; }
  var topId = topOpenNodeId();   // most recently opened (still open) node — its edges sit above the rest
  function isTop(re) { return topId != null && (String(re.d.source) === topId || String(re.d.target) === topId); }
  var busy = function (re) { return re.edge.hasClass('selected') || re.edge.hasClass('hovered'); };
  rawEdges.forEach(function (re) {                                   // base (dimmed when something's selected)
    if (!ok(re) || busy(re) || isTop(re)) return;
    svg.appendChild(ribbon(re, anySel ? DIM_OP : NORM_OP));
  });
  rawEdges.forEach(function (re) {                                   // most-recently-opened node's edges, above base
    if (!ok(re) || busy(re) || !isTop(re)) return;
    svg.appendChild(ribbon(re, anySel ? DIM_OP : NORM_OP));
  });
  rawEdges.forEach(function (re) {                                   // hovered (takes precedence)
    if (!ok(re) || !re.edge.hasClass('hovered') || re.edge.hasClass('selected')) return;
    svg.appendChild(ribbon(re, 1));
  });
  rawEdges.forEach(function (re) {                                   // selected on top
    if (!ok(re) || !re.edge.hasClass('selected')) return;
    svg.appendChild(ribbon(re, 1));
  });
}

// The most recently opened node that is still open (inline mode), by open sequence.
function topOpenNodeId() {
  if (!inlineMode) return null;
  var best = null, bestSeq = -1;
  Object.keys(inlineExpandedMap).forEach(function (id) {
    var s = inlineExpandedMap[id].openedAt || 0;
    if (s > bestSeq) { bestSeq = s; best = id; }
  });
  return best;
}

/* ── Node-to-panel connector line ────────────────────────────────────────── */

function drawNodeConnector() {
  var old = document.getElementById('node-connector'); if (old) old.remove();
  if (mobileMode || inlineMode || !selectedNodeId || !cy) return;
  var panel = document.getElementById('desc-panel');
  if (!panel || panel.style.display === 'none') return;
  var node = cy.getElementById(String(selectedNodeId));
  if (!node || node.empty()) return;
  var pos = node.position(), nw = node.data('w') || 160, nh = node.data('h') || 46;
  var pan = cy.pan(), zoom = cy.zoom();
  var ctr = cy.container().getBoundingClientRect();
  var grp = node.data('group');
  var nodeColor = grp === 'Theme' ? colTheme : grp === 'Project' ? colProject : colSkill;

  var pr = panel.getBoundingClientRect();

  // Hide entirely if desc-panel has scrolled out of the sidebar-scroll viewport
  var sideScroll = document.getElementById('sidebar-scroll');
  if (sideScroll) {
    var sr = sideScroll.getBoundingClientRect();
    if (pr.bottom < sr.top || pr.top > sr.bottom) return;
  }

  var px = pr.right;
  // Clamp attachment point so it never floats above the page-title bar
  var pageTitleEl = document.getElementById('page-title');
  var minPy = pageTitleEl ? pageTitleEl.getBoundingClientRect().bottom + 4 : 0;
  var py = Math.max(pr.top + 8, minPy);
  // Project/Skill: start 1/3 down (above center, clear of edge attachment); Theme: center
  var ny = (grp === 'Project' || grp === 'Skill')
    ? (pos.y - nh / 6) * zoom + pan.y + ctr.top
    : pos.y * zoom + pan.y + ctr.top;

  var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.id = 'node-connector';
  svg.style.cssText = 'position:fixed;top:0;left:0;width:100vw;height:100vh;pointer-events:none;z-index:1;overflow:visible;';
  document.body.appendChild(svg);
  var sw = baseEdgeWidth * zoom * 0.75;
  var path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  var d, nx_start;

  // Compute the routing lane Y: below all column headers AND above all nodes
  // in columns the horizontal segment will cross.
  function computeRouteY(groupsInPath) {
    var hdrs = document.querySelectorAll('.col-hdr');
    var ry = ctr.top + 20;
    for (var i = 0; i < hdrs.length; i++) {
      var hb = hdrs[i].getBoundingClientRect();
      if (hb.bottom > ry) ry = hb.bottom;
    }
    ry += 14;
    var minNodeTop = Infinity;
    cy.nodes().forEach(function(n) {
      if (groupsInPath.indexOf(n.data('group')) < 0) return;
      var nh2 = n.data('h') || 46;
      var top = (n.position().y - nh2 / 2) * zoom + pan.y + ctr.top;
      if (top < minNodeTop) minNodeTop = top;
    });
    if (isFinite(minNodeTop)) ry = Math.min(ry, minNodeTop - 8);
    return ry;
  }

  function snakePath(nx_s, ny_s, route_y_s) {
    if (ny_s > route_y_s + 4) {
      var bend = Math.min(18, (ny_s - route_y_s) / 2);
      var t = Math.min((ny_s - route_y_s) * 0.3, 45);
      var d_end = (py > route_y_s + bend)
        ? ' Q' + px.toFixed(1) + ',' + route_y_s.toFixed(1) +
          ' ' + px.toFixed(1) + ',' + (route_y_s + bend).toFixed(1) +
          ' L' + px.toFixed(1) + ',' + py.toFixed(1)
        : ' L' + px.toFixed(1) + ',' + py.toFixed(1);
      var p3x = Math.max(nx_s - t - bend, px + bend * 2);
      return 'M' + nx_s.toFixed(1) + ',' + ny_s.toFixed(1) +
        ' C' + (nx_s - t).toFixed(1) + ',' + (ny_s + t * 0.5).toFixed(1) +
        ' ' + (nx_s - t).toFixed(1) + ',' + route_y_s.toFixed(1) +
        ' ' + p3x.toFixed(1) + ',' + route_y_s.toFixed(1) +
        ' L' + (px + bend).toFixed(1) + ',' + route_y_s.toFixed(1) +
        d_end;
    } else {
      var dxfb = px - nx_s;
      return 'M' + nx_s.toFixed(1) + ',' + ny_s.toFixed(1) +
        ' C' + (nx_s + dxfb * 0.4).toFixed(1) + ',' + ny_s.toFixed(1) +
        ' ' + (px - dxfb * 0.4).toFixed(1) + ',' + py.toFixed(1) +
        ' ' + px.toFixed(1) + ',' + py.toFixed(1);
    }
  }

  nx_start = (pos.x - nw / 2) * zoom + pan.x + ctr.left;

  if (grp === 'Theme') {
    // Simple bezier — Theme column is leftmost, same side as panel, no nodes to cross
    var dxfb_th = px - nx_start;
    d = 'M' + nx_start.toFixed(1) + ',' + ny.toFixed(1) +
      ' C' + (nx_start + dxfb_th * 0.4).toFixed(1) + ',' + ny.toFixed(1) +
      ' ' + (px - dxfb_th * 0.4).toFixed(1) + ',' + py.toFixed(1) +
      ' ' + px.toFixed(1) + ',' + py.toFixed(1);
  } else if (grp === 'Skill') {
    // Route above Theme and Project nodes
    var ry_skill = computeRouteY(['Theme', 'Project']);
    d = snakePath(nx_start, ny, ry_skill);
  } else {
    // Project: route above Theme nodes; don't go higher than panel top
    var ry_proj = Math.max(computeRouteY(['Theme']), py);
    d = snakePath(nx_start, ny, ry_proj);
  }

  // Glow pass (behind main path)
  var glow = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  glow.setAttribute('d', d);
  glow.setAttribute('fill', 'none');
  glow.setAttribute('stroke', lightMode ? '#000000' : '#ffffff');
  glow.setAttribute('stroke-width', sw * 2.5);
  glow.setAttribute('stroke-dasharray', (14 * zoom) + ',' + (5 * zoom));
  glow.setAttribute('opacity', '1');
  svg.appendChild(glow);

  path.setAttribute('d', d);
  path.setAttribute('fill', 'none'); path.setAttribute('stroke', nodeColor);
  path.setAttribute('stroke-width', sw);
  path.setAttribute('stroke-dasharray', (14 * zoom) + ',' + (5 * zoom));
  path.setAttribute('opacity', '0.7');
  svg.appendChild(path);
}

/* ── Column Headers — subtitle 15px ──────────────────────────────────────── */

function positionHeaders(data) {
  document.querySelectorAll('.col-hdr').forEach(function (el) { el.remove(); });
  var old_wm = document.getElementById('watermark-text'); if (old_wm) old_wm.remove();
  if (!cy || !data || !data.headers) return;
  var area = document.getElementById('graph-area');
  var pan = cy.pan(), zoom = cy.zoom();
  // On mobile, derive header x from actual node positions so layout shifts are reflected
  if (mobileMode && cy) {
    var groupData = {};
    ['Theme','Project','Skill'].forEach(function(g) { groupData[g] = { xs: [], minY: Infinity }; });
    cy.nodes().forEach(function(n) {
      var g = n.data('group'); if (!groupData[g]) return;
      groupData[g].xs.push(n.position('x'));
      var top = n.position('y') - (n.data('h') || 46) / 2;
      if (top < groupData[g].minY) groupData[g].minY = top;
    });
    var grpOrder = ['Theme', 'Project', 'Skill'];
    data.headers.forEach(function(h, i) {
      if (h.sub) return;                       // sub-headers positioned relative to their anchor node
      var gd = groupData[grpOrder[i]];
      if (gd.xs.length) h.x = gd.xs.reduce(function(a, b) { return a + b; }, 0) / gd.xs.length;
      if (gd.minY !== Infinity)
        h.y = gd.minY - (fontHdr1 + fontHdr2) * 1.25 - 6 / zoom;
    });
  }
  var hdrGroups = ['Theme', 'Project', 'Skill'];
  data.headers.forEach(function (h, i) {
    if (h.sub) return;                          // handled below (anchored to its About node)
    // In inline mode a Theme/Skill column can rise into the space above; its header follows.
    var shiftUp = inlineMode ? (inlineColShiftUp[hdrGroups[i]] || 0) : 0;
    // ...and the header scrolls with its own column, so scrolled content never slides underneath it
    // (it slips up out of view instead of sitting on top of the text or peeking between nodes).
    var scrollOff = inlineMode ? (inlineColScroll[hdrGroups[i]] || 0) : 0;
    var sx = h.x * zoom + pan.x, sy = (h.y - shiftUp - scrollOff) * zoom + pan.y;
    var div = document.createElement('div');
    var hcolor = i === 0 ? colTheme : (i === 1 ? colProject : colSkill);
    div.className = 'col-hdr'; div.id = 'colhdr-' + i; div.style.color = hcolor;
    div.style.transform = 'scale(1)'; div.style.transformOrigin = 'top center';
    // Title/subtitle on the left, per-column Open all / Collapse all buttons on the right (stacked so
    // they fit within the existing two-line header height). openAllInline/collapseAllInline are global.
    var hgrp = hdrGroups[i];
    var btnFs = Math.max(7, Math.round(fontHdr2 * 0.7));
    // More breathing room between the title and the buttons when the browser is wide; small on narrow.
    var hdrGap = Math.max(6, Math.min(32, Math.round(((area.clientWidth || window.innerWidth) - 700) / 55 + 8)));
    div.innerHTML =
      '<div class="col-hdr-inner" style="gap:' + hdrGap + 'px;">' +
        '<div class="col-hdr-text">' +
          '<b style="font-size:' + fontHdr1 + 'px;white-space:nowrap">' + dualLabel(h.line1, h.line1_fi) + '</b>' +
          '<span style="font-size:' + fontHdr2 + 'px;white-space:nowrap">' + dualLabel(h.line2, h.line2_fi) + '</span>' +
        '</div>' +
        '<div class="col-hdr-btns" style="font-size:' + btnFs + 'px;">' +
          '<button type="button" class="col-hdr-btn" onclick="openAllInline(\'' + hgrp + '\')">' + (currentLang === 'fi' ? 'Avaa kaikki' : 'Open all') + '</button>' +
          '<button type="button" class="col-hdr-btn" onclick="collapseAllInline(\'' + hgrp + '\')">' + (currentLang === 'fi' ? 'Sulje kaikki' : 'Collapse all') + '</button>' +
        '</div>' +
      '</div>';
    div.style.visibility = 'hidden'; div.style.top = '0'; div.style.left = '0';
    area.appendChild(div);
    var natW = div.offsetWidth;
    div.style.width = natW + 'px';  // lock width before moving left, prevents text-wrap shift
    div.style.transform = 'scale(' + zoom + ')';
    div.style.left = Math.round(sx - natW / 2) + 'px';
    div.style.top = Math.round(sy) + 'px'; div.style.visibility = '';
  });

  // Sub-headers (e.g. "About"): anchored just above their first node so they track the column's
  // scroll/expansion, using the same title font as the top-of-column headers.
  data.headers.forEach(function (h, i) {
    if (!h.sub) return;
    var an = h.anchorId != null ? cy.getElementById(String(h.anchorId)) : null;
    if (!an || an.empty()) return;
    var ntop = an.position('y') - (an.data('h') || 46) / 2;
    var gapCy = fontHdr1 * 1.6 + 8;
    var sx = an.position('x') * zoom + pan.x, sy = (ntop - gapCy) * zoom + pan.y;
    var div = document.createElement('div');
    div.className = 'col-hdr'; div.id = 'colhdr-sub-' + i; div.style.color = h.color || (lightMode ? '#5a6b7a' : '#c4d0da');
    div.style.transform = 'scale(1)'; div.style.transformOrigin = 'top center';
    div.innerHTML = '<b style="font-size:' + fontHdr1 + 'px;white-space:nowrap">' + dualLabel(h.line1, h.line1_fi) + '</b>';
    div.style.visibility = 'hidden'; div.style.top = '0'; div.style.left = '0';
    area.appendChild(div);
    var natW = div.offsetWidth;
    div.style.width = natW + 'px';
    div.style.transform = 'scale(' + zoom + ')';
    div.style.left = Math.round(sx - natW / 2) + 'px';
    div.style.top = Math.round(sy) + 'px'; div.style.visibility = '';
  });

  // Watermark bottom-left of graph area
  if (watermarkText) {
    var wm = document.createElement('div');
    wm.id = 'watermark-text';
    wm.style.cssText = 'position:absolute;bottom:12px;left:12px;color:rgba(255,255,255,0.8);' +
      'font-family:Arial,Helvetica,sans-serif;font-size:' + (watermarkSize * zoom) + 'px;' +
      'pointer-events:none;z-index:10;white-space:pre-wrap;';
    wm.textContent = watermarkText;
    area.appendChild(wm);
  }
}

/* ── Sidebar Resize ──────────────────────────────────────────────────────── */

(function () {
  var handle = null, sidebar = null, dragging = false, startX = 0, startW = 0;
  document.addEventListener('DOMContentLoaded', function () {
    handle = document.getElementById('sidebar-resize-handle');
    sidebar = document.getElementById('info-sidebar');
    if (!handle || !sidebar) return;
    handle.addEventListener('mousedown', function (e) {
      e.preventDefault(); dragging = true; startX = e.clientX; startW = sidebar.offsetWidth;
      handle.classList.add('dragging'); document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
    });
    document.addEventListener('mousemove', function (e) {
      if (!dragging) return;
      sidebar.style.width = Math.max(180, Math.min(700, startW + (e.clientX - startX))) + 'px';
      requestAnimationFrame(resizeCy);
    });
    document.addEventListener('mouseup', function () {
      if (!dragging) return; dragging = false; handle.classList.remove('dragging');
      document.body.style.cursor = ''; document.body.style.userSelect = '';
      setTimeout(function() { resizeCy(); refreshLayout(); }, 60);
    });
  });
})();

/* ── Pick data for current viewport ───────────────────────────────────────── */

function pickData(payload) {
  mobileMode = useMobileLayout();
  if (mobileMode && payload.mobile) {
    mobileData = payload.mobile;
    return JSON.parse(JSON.stringify(payload.mobile)); // deep clone so mutations don't corrupt rawPayload
  }
  mobileData = payload.mobile || null;
  return JSON.parse(JSON.stringify(payload)); // deep clone
}

function applyDataGlobals(data) {
  fontNode = data.fontNode || 12;
  fontPtype = data.fontPtype || 12;
  fontSubs = data.fontSubs || 15;
  descFontSize = data.fontDesc || 18;
  fontHdr1 = data.fontHdr1 || 22;
  fontHdr2 = data.fontHdr2 || 15;
  watermarkText = data.watermarkText || '';
  watermarkSize = data.watermarkSize || 10;
  var ph = document.getElementById('page-title');
  if (ph) { ph.style.fontSize = (data.fontHdr1 || 22) + 'px'; requestAnimationFrame(syncResizeHandle); }
  // Store dark and light color sets
  darkColBg         = data.colBg         || darkColBg;
  darkColSidebarBg  = data.colSidebarBg  || darkColSidebarBg;
  darkColNodeBg     = data.colNodeBg     || darkColNodeBg;
  darkColTheme      = data.colTheme      || darkColTheme;
  darkColProject    = data.colProject    || darkColProject;
  darkColSkill      = data.colSkill      || darkColSkill;
  if (data.lightColBg)         lightColBg         = data.lightColBg;
  if (data.lightColSidebarBg)  lightColSidebarBg  = data.lightColSidebarBg;
  if (data.lightColNodeBg)     lightColNodeBg      = data.lightColNodeBg;
  if (data.lightColTheme)      lightColTheme       = data.lightColTheme;
  if (data.lightColProject)    lightColProject     = data.lightColProject;
  if (data.lightColSkill)      lightColSkill       = data.lightColSkill;
  if (data.lightEdgeColor)     lightEdgeColor      = data.lightEdgeColor;
  // Apply active color set
  if (lightMode) {
    colBg = lightColBg; colSidebarBg = lightColSidebarBg; colNodeBg = lightColNodeBg;
    colTheme = lightColTheme; colProject = lightColProject; colSkill = lightColSkill;
  } else {
    colBg = darkColBg; colSidebarBg = darkColSidebarBg; colNodeBg = darkColNodeBg;
    colTheme = darkColTheme; colProject = darkColProject; colSkill = darkColSkill;
  }
}

/* ── Init Cytoscape ──────────────────────────────────────────────────────── */

function initAccordions() {
  document.querySelectorAll('.acc-section.acc-open .acc-body').forEach(function(b) {
    var section = b.parentElement;
    if (section && section.id === 'acc-desc') {
      b.style.maxHeight = '';
      b.style.height = '';
      b.style.overflowY = '';
    } else {
      b.style.maxHeight = 'none';
    }
  });
}

/* ── Layout snapshot: save/restore node geometry before autoFit mutates it ── */

function saveLayoutSnapshot(data) {
  if (!data || !data.nodes) return;
  layoutSnapshot = {
    isMobile: mobileMode,   // track which mode this snapshot belongs to
    nodes: (data.nodes).map(function(n) {
      return { id: n.data && n.data.id, w: n.data && n.data.w, h: n.data && n.data.h,
               x: n.position && n.position.x, y: n.position && n.position.y };
    }),
    headers: (data.headers || []).map(function(h) { return { x: h.x, y: h.y }; })
  };
}

function restoreLayoutSnapshot(data) {
  // Don't apply a snapshot from a different layout mode — it would corrupt node geometry.
  if (!layoutSnapshot || !data || !data.nodes) return;
  if (layoutSnapshot.isMobile !== mobileMode) return;
  var byId = {};
  layoutSnapshot.nodes.forEach(function(s) { if (s.id != null) byId[String(s.id)] = s; });
  data.nodes.forEach(function(n) {
    var s = n.data && byId[String(n.data.id)];
    if (!s) return;
    if (n.data)     { n.data.w = s.w; n.data.h = s.h; }
    if (n.position) { n.position.x = s.x; n.position.y = s.y; }
  });
  (data.headers || []).forEach(function(h, i) {
    if (layoutSnapshot.headers[i]) { h.x = layoutSnapshot.headers[i].x; h.y = layoutSnapshot.headers[i].y; }
  });
}

/* Re-evaluate layout mode with current viewport; rebuild elements if it changed. */
function refreshLayout() {
  if (!lastData || !cy) return;
  restoreLayoutSnapshot(lastData);
  autoFitProjectWidth(lastData);
  // Update node data and positions in-place — avoids zombie nodeHtmlLabel overlays
  // that occur when elements are removed and re-added.
  (lastData.nodes || []).forEach(function(n) {
    if (!n.data || n.data.id == null) return;
    var ele = cy.getElementById(String(n.data.id));
    if (ele.empty()) return;
    ele.data({ w: n.data.w, h: n.data.h });
    ele.position({ x: n.position.x, y: n.position.y });
  });
  fitGraph();
  positionHeaders(lastData);
  drawEdgeOverlay();
  drawNodeConnector();
}

/* ── Auto-fit project node width/height to viewport aspect ratio ─────────── */
/* On wide viewports: widen nodes so titles fit on one row.                   */
/* On narrow viewports: keep base width and double node height so titles wrap  */
/* to two rows — this allows a larger zoom and visually larger font.          */

// Narrow screens: multiply the base column gap and node widths by the author's narrow multipliers,
// then re-measure heights and re-stack the Theme and Skill(+About) columns (preserving R's per-node
// vertical gaps). Projects are left to autoFitProjectWidth downstream. Runs on the fresh per-init
// payload clone before saveLayoutSnapshot/autoFit, so the scaled layout becomes the working base.
function applyNarrowScale(data) {
  if (!unifiedUI || !isNarrow() || !data || !data.nodes) return;
  var gm = (narrowGapMult > 0) ? narrowGapMult : 1;
  var nm = (narrowNodeMult > 0) ? narrowNodeMult : 1;
  if (gm === 1 && nm === 1) return;

  // Uniform per-column base widths + the Project column x (to recover the base edge gap).
  var themeW = 0, projW = 0, projX = null;
  data.nodes.forEach(function (n) {
    if (!n.data) return;
    var g = n.data.group;
    if (g === 'Theme' || g === 'Skill' || g === 'About') themeW = Math.max(themeW, n.data.w || 0);
    else if (g === 'Project') { projW = Math.max(projW, n.data.w || 0); if (projX == null && n.position) projX = n.position.x; }
  });
  if (!(themeW > 0) || !(projW > 0) || projX == null) return;
  var baseGap = projX - themeW / 2 - projW / 2;            // R laid columns out edge-to-edge

  var newThemeW = Math.max(40, themeW * nm);
  var newProjW  = Math.max(40, projW * nm);
  var newGap    = Math.max(0, baseGap * gm);
  var newProjX  = newThemeW / 2 + newGap + newProjW / 2;
  var newSkillX = newProjX + newProjW / 2 + newGap + newThemeW / 2;

  // Apply widths + column x; record original y/h; re-measure Theme/Skill/About heights at the new width.
  data.nodes.forEach(function (n) {
    if (!n.data || !n.position) return;
    n.data._oy = n.position.y; n.data._oh = n.data.h;
    var g = n.data.group;
    if (g === 'Theme')        { n.data.w = newThemeW; n.position.x = 0;         n.data.h = measureThemeNodeHeight(n.data, newThemeW); }
    else if (g === 'About')   { n.data.w = newThemeW; n.position.x = newSkillX; n.data.h = measureThemeNodeHeight(n.data, newThemeW); }
    else if (g === 'Skill')   { n.data.w = newThemeW; n.position.x = newSkillX; n.data.h = measureSkillNodeHeight(n.data, newThemeW); }
    else if (g === 'Project') { n.data.w = newProjW;  n.position.x = newProjX; }  // height/re-stack: autoFitProjectWidth
  });
  (data.headers || []).forEach(function (h, i) {
    if (i === 0) h.x = 0; else if (i === 1) h.x = newProjX; else if (i === 2) h.x = newSkillX;
  });

  // Re-stack a column top-down, preserving each original inter-node gap while accommodating new heights.
  function restack(nodesInCol) {
    nodesInCol.sort(function (a, b) { return a.data._oy - b.data._oy; });
    var prevOrigBottom = null, prevNewBottom = null;
    nodesInCol.forEach(function (n) {
      var oTop = n.data._oy - n.data._oh / 2;
      var nTop = (prevNewBottom == null) ? oTop : prevNewBottom + (oTop - prevOrigBottom);
      n.position.y = nTop + n.data.h / 2;
      prevOrigBottom = oTop + n.data._oh;
      prevNewBottom  = nTop + n.data.h;
    });
  }
  var themeCol = [], skillCol = [];
  data.nodes.forEach(function (n) {
    if (!n.data) return;
    if (n.data.group === 'Theme') themeCol.push(n);
    else if (n.data.group === 'Skill' || n.data.group === 'About') skillCol.push(n);
  });
  restack(themeCol); restack(skillCol);
}

// Collapsed (unexpanded) height of a node measured at its CURRENT width. Used to re-measure heights
// after the "extra width" fill widens nodes — otherwise a title that now fits on one row is left in a
// box that was sized for two rows. On narrow screens Project nodes also get at least a two-row height
// (comfortable finger targets). Everything else is measured to fit its title.
function collapsedNodeHeight(nd) {
  var g = nd.group, w = nd.w || 200, h;
  if (g === 'Project') h = measureProjectNodeHeight(nd, w);
  else if (g === 'Skill') h = measureSkillNodeHeight(nd, w);
  else if (g === 'Theme' || g === 'About') h = measureThemeNodeHeight(nd, w);
  else return nd.h || 46;
  if (g === 'Project' && isNarrow()) h = Math.max(h, Math.round(2 * fontNode * 1.3 + 8));  // two-row floor
  return h;
}

function autoFitProjectWidth(data) {
  if (useMobileLayout()) return;
  var canvas = document.createElement('canvas');
  var ctx = canvas.getContext('2d');
  ctx.font = 'bold ' + fontNode + 'px Arial,Helvetica,sans-serif';

  var maxTextW = 0, baseW = 0, baseH = 0, projNodes = [], themeNodes = [], skillNodes = [];
  (data.nodes || []).forEach(function(n) {
    if (!n.data) return;
    if (n.data.group === 'Project') {
      var label = (currentLang === 'fi' && n.data.label_fi) ? n.data.label_fi : (n.data.label || '');
      var tw = ctx.measureText(label).width;
      if (tw > maxTextW) maxTextW = tw;
      if ((n.data.w || 0) > baseW) baseW = n.data.w;
      if ((n.data.h || 0) > baseH) baseH = n.data.h;
      projNodes.push(n);
    } else if (n.data.group === 'Theme') { themeNodes.push(n); }
    else if (n.data.group === 'Skill')   { skillNodes.push(n); }
  });
  if (maxTextW === 0 || baseW === 0) return;
  projNodes.sort(function(a, b) { return a.position.y - b.position.y; });

  var typeF = ptypePct / 100;
  // Title side padding must match nodeBodyHtml: accordion gutter + one symbol width, each side,
  // plus a small buffer against canvas/browser measurement discrepancy.
  var _aspec = (inlineMode && accordionIcon !== 'none') ? ACC_ICONS[accordionIcon] : null;
  var _agut = (_aspec && _aspec.c) ? Math.round(accordionIconSize * (_aspec.mult || 1) * 0.7) + 16 : 0;
  var projPadL = Math.max(9, _agut) + nodeTextPad;   // left clears the chevron; matches nodeBodyHtml
  var projPadR = 9 + nodeTextPad;                     // right minimal (no chevron on projects)
  var requiredSingle = Math.ceil((maxTextW + projPadL + projPadR + 12) / (1 - typeF));
  if (requiredSingle <= baseW) return; // already fits at base width
  var delta = requiredSingle - baseW;

  // Infer gap between project nodes from layout
  var gapV = 18;
  if (projNodes.length >= 2)
    gapV = Math.max(4, Math.round(projNodes[1].position.y - projNodes[0].position.y -
                                  (projNodes[0].data.h + projNodes[1].data.h) / 2));

  // ── Inline mode: cap project width and WRAP long titles instead of widening the whole column ──
  // Vertical space is cheap here (columns scroll independently); horizontal is shared and scarce —
  // on a phone a wide column shrinks the whole-map fit-zoom. Auto cap = the base project width
  // (never widen — wrap immediately); the optional "max project width" slider raises the cap (widen
  // up to it, then wrap). requiredSingle > baseW here (the already-fits case returned above).
  if (inlineMode) {
    var cap = (projectMaxWidth && projectMaxWidth > 0) ? projectMaxWidth : baseW;
    var targetW = Math.max(80, Math.min(requiredSingle, cap));
    var dW = targetW - baseW;
    currentLayoutMode = (targetW >= requiredSingle) ? 'single' : 'two';
    var pTop = projNodes[0].position.y - projNodes[0].data.h / 2;   // anchor the first project's top
    var yC = pTop;
    projNodes.forEach(function (n) {
      n.data.w = targetW;
      n.data.h = measureProjectNodeHeight(n.data, targetW);         // wrap-aware height at the cap
      n.position.y = yC + n.data.h / 2;                             // re-stack downward, no overlap
      yC += n.data.h + gapV;
    });
    // Keep columns balanced around the (possibly) resized project column.
    (data.nodes || []).forEach(function (n) {
      if (!n.data || !n.position) return;
      if (n.data.group === 'Project') n.position.x += dW / 2;
      else if (n.data.group === 'Skill' || n.data.group === 'About') n.position.x += dW;
    });
    (data.headers || []).forEach(function (h, i) {
      if (i === 1) h.x += dW / 2; else if (i === 2) h.x += dW;
    });
    return;
  }

  // Bounding box at base (unmodified) node sizes
  var bx1 = Infinity, bx2 = -Infinity, by1 = Infinity, by2 = -Infinity;
  (data.nodes || []).forEach(function(n) {
    if (!n.data || !n.position) return;
    var w = n.data.w || 200, h = n.data.h || 46;
    bx1 = Math.min(bx1, n.position.x - w/2); bx2 = Math.max(bx2, n.position.x + w/2);
    by1 = Math.min(by1, n.position.y - h/2); by2 = Math.max(by2, n.position.y + h/2);
  });
  var origBBW = bx2 - bx1;

  // Viewport dimensions — #cy has no height before resizeCy() runs, use parent
  var cyCon = document.getElementById('cy');
  var W = cyCon ? cyCon.offsetWidth : 800;
  var H = (cyCon && cyCon.offsetHeight > 10) ? cyCon.offsetHeight
        : (cyCon && cyCon.parentElement && cyCon.parentElement.clientHeight > 10)
            ? cyCon.parentElement.clientHeight : window.innerHeight;
  var hm = data.headerMargin || 70;

  // Two-row node height: sized to font metrics rather than a blind 2×baseH,
  // so the project column isn't unnecessarily tall.
  function colExtentH(nodes) {
    var y1 = Infinity, y2 = -Infinity;
    nodes.forEach(function(n) { var h=n.data.h||46; y1=Math.min(y1,n.position.y-h/2); y2=Math.max(y2,n.position.y+h/2); });
    return y1 > y2 ? 0 : y2 - y1;
  }
  var twoRowH = Math.max(Math.round(2 * fontNode * 1.35 + 20), Math.round(baseH * 1.2));
  var projColHTwoRow = projNodes.length * twoRowH + (projNodes.length - 1) * gapV;
  var bbHTwoRow = Math.max(projColHTwoRow, colExtentH(themeNodes), colExtentH(skillNodes));

  // Balanced two-row width: solve for Δ where zoom_W = zoom_H in two-row mode.
  // (W-40)/(origBBW+Δ) = (H-28)/(bbHTwoRow+hm)  →  Δ = (W-40)*(bbHTwoRow+hm)/(H-28) - origBBW
  var balancedDelta = Math.round((W - 40) * (bbHTwoRow + hm) / (H - 28) - origBBW);
  var twoRowNodeW   = baseW + Math.max(0, balancedDelta);

  // (Inline mode returned above with its own cap/wrap handling.) Non-inline balances width vs height.
  if (twoRowNodeW >= requiredSingle) {
    // ── Single-row: balanced width exceeds the one-row requirement; just use single-row ──
    currentLayoutMode = 'single';
    (data.nodes || []).forEach(function(n) {
      if (!n.data || !n.position) return;
      if (n.data.group === 'Project') { n.data.w = requiredSingle; n.position.x += delta / 2; }
      else if (n.data.group === 'Skill' || n.data.group === 'About') { n.position.x += delta; }
    });
    (data.headers || []).forEach(function(h, i) {
      if (i === 1) h.x += delta / 2;
      else if (i === 2) h.x += delta;
    });
  } else {
    // ── Two-row: widen to balanced width, resize height to font metrics, redistribute y ──
    currentLayoutMode = 'two';
    var dtwo = Math.max(0, balancedDelta);
    // Shift x positions to use available horizontal space
    (data.nodes || []).forEach(function(n) {
      if (!n.data || !n.position) return;
      if (n.data.group === 'Project') { n.data.w = twoRowNodeW; n.position.x += dtwo / 2; }
      else if (n.data.group === 'Skill' || n.data.group === 'About') { n.position.x += dtwo; }
    });
    (data.headers || []).forEach(function(h, i) {
      if (i === 1) h.x += dtwo / 2;
      else if (i === 2) h.x += dtwo;
    });
    // Redistribute project node y positions with new height
    var projY1 = projNodes[0].position.y - projNodes[0].data.h / 2;
    var projY2 = projNodes[projNodes.length-1].position.y + projNodes[projNodes.length-1].data.h / 2;
    var projCenter = (projY1 + projY2) / 2;
    var newProjTop  = projCenter - projColHTwoRow / 2;
    projNodes.forEach(function(n, i) {
      n.data.h = twoRowH;
      n.position.y = newProjTop + twoRowH / 2 + i * (twoRowH + gapV);
    });
    // Re-centre theme and skill columns at the same vertical midpoint
    [themeNodes, skillNodes].forEach(function(col) {
      if (!col.length) return;
      var cy1 = Infinity, cy2 = -Infinity;
      col.forEach(function(n) { var h=n.data.h||46; cy1=Math.min(cy1,n.position.y-h/2); cy2=Math.max(cy2,n.position.y+h/2); });
      var shift = projCenter - (cy1 + cy2) / 2;
      if (Math.abs(shift) > 1) col.forEach(function(n) { n.position.y += shift; });
    });
    // Per-column header Y: same gap (hm) above each column's topmost node
    var projHdrY = newProjTop - hm;
    function colTopY(nodes) {
      var t = Infinity;
      nodes.forEach(function(n) { t = Math.min(t, n.position.y - (n.data.h||46)/2); });
      return isFinite(t) ? t : newProjTop;
    }
    var themeHdrY = colTopY(themeNodes) - hm;
    var skillHdrY = colTopY(skillNodes) - hm;
    (data.headers || []).forEach(function(h, i) {
      if (i === 0) h.y = themeHdrY;
      else if (i === 1) h.y = projHdrY;
      else if (i === 2) h.y = skillHdrY;
    });
  }
}

// Canvas-based text line counter.
// containerW is in CSS pixels (= cyto units) — the nodeHtmlLabel uses transform:scale(zoom)
// so word-wrap happens at the full cyto-unit width, not the scaled screen width.
var _cvsMeasure = document.createElement('canvas');
function canvasTextLines(text, fontSize, fontWeight, containerW) {
  if (!text || containerW <= 0) return 1;
  var ctx = _cvsMeasure.getContext('2d');
  ctx.font = (fontWeight || 'normal') + ' ' + fontSize + 'px Arial,Helvetica,sans-serif';
  var words = text.split(' ');
  var lines = 1, lineW = 0;
  var spW = ctx.measureText(' ').width;
  for (var i = 0; i < words.length; i++) {
    var ww = ctx.measureText(words[i]).width;
    if (lineW > 0 && lineW + spW + ww > containerW) { lines++; lineW = ww; }
    else { lineW = lineW > 0 ? lineW + spW + ww : ww; }
  }
  return lines;
}

// Returns node height in cyto units (= CSS px, since nodeHtmlLabel scales via transform).
// containerW: CSS-pixel width of the text area (node width minus horizontal padding).
// vPad: total vertical CSS-px padding nodeHtml adds (top+bottom).
function measureNodeHeight(lines, containerW, vPad) {
  var hPx = 0;
  lines.forEach(function(ln) {
    var usableW = Math.max(containerW - (ln.paddingLeft || 0), 4);
    var n = canvasTextLines(ln.text, ln.fontSize, ln.fontWeight || 'normal', usableW);
    hPx += n * ln.fontSize * (ln.lineHeight || 1.25);
  });
  return Math.max(hPx + (vPad || 8), 8);
}

function measureProjectNodeHeight(nodeData, w) {
  // Text area width must match nodeBodyHtml's Project branch: node width minus the type column and
  // the asymmetric title padding (left clears the chevron, right minimal). Otherwise wrapped-line
  // counting disagrees with the rendered layout.
  var _spec = (inlineMode && accordionIcon !== 'none') ? ACC_ICONS[accordionIcon] : null;
  var _gut  = (_spec && _spec.c) ? Math.round(accordionIconSize * (_spec.mult || 1) * 0.7) + 16 : 0;
  var padL = Math.max(9, _gut) + nodeTextPad;
  var padR = 9 + nodeTextPad;
  var ptypeColW = (!mobileMode && nodeData.ptype) ? Math.round(w * ptypePct / 100) : 0;
  var containerW = Math.max(w - ptypeColW - padL - padR, 4);   // 4px 8px vertical padding → vPad 8
  var en = String(nodeData.label || ''), fi = String(nodeData.label_fi || '');
  // Match nodeBodyHtml's Project branch exactly: if the English title fits on one line it renders
  // nowrap (one row; a longer Finnish title is font-shrunk to fit), otherwise both wrap. Using the
  // same whole-string test — not a word-by-word line count — avoids the tall-box artifact where a
  // borderline title measured two rows but actually renders on one.
  var lines = (measureTextPx(en, fontNode) <= containerW) ? 1
            : Math.max(canvasTextLines(en, fontNode, 'bold', containerW),
                       canvasTextLines(fi, fontNode, 'bold', containerW));
  return Math.max(lines * fontNode * 1.3 + 8, 8);
}

function measureThemeNodeHeight(nodeData, w) {
  // Theme outer: padding:3px 7px → 6px vertical; span padding-right:14px → 28px horizontal
  var containerW = Math.max(w - 28, 4);
  var label = String(nodeData.label || '');
  var labelFi = String(nodeData.label_fi || '');
  return measureNodeHeight(
    [{ text: labelFi.length > label.length ? labelFi : label, fontSize: fontNode, fontWeight: 'bold', lineHeight: 1.25 }],
    containerW, 6
  );
}

function measureSkillNodeHeight(nodeData, w) {
  // Skill outer: padding:4px 7px → 8px vertical, 14px horizontal
  var containerW = Math.max(w - 14, 4);
  var label = String(nodeData.label || '');
  var labelFi = String(nodeData.label_fi || '');
  var lines = [{ text: labelFi.length > label.length ? labelFi : label, fontSize: fontNode, fontWeight: 'bold', lineHeight: 1.25 }];
  var subsStr = nodeData.subs || '';
  if (subsStr) {
    subsStr.split('||').forEach(function(item) {
      lines.push({ text: item, fontSize: fontSubs, fontWeight: 'normal', lineHeight: 1.3, paddingLeft: 10 });
    });
  }
  return measureNodeHeight(lines, containerW, 8);
}

function applyMobileNodeSizes(data) {
  if (!useMobileLayout() || !data.headers || data.headers.length < 2) return;
  var origColGap = data.headers[1].x - data.headers[0].x;

  // Infer vertical gap from original project node spacing (before resizing)
  var projNodes = (data.nodes || []).filter(function(n) { return n.data && n.data.group === 'Project'; });
  projNodes.sort(function(a, b) { return a.position.y - b.position.y; });
  var gapProject = 18;
  if (projNodes.length >= 2) {
    gapProject = Math.max(4, Math.round(
      projNodes[1].position.y - projNodes[0].position.y -
      (projNodes[0].data.h + projNodes[1].data.h) / 2
    ));
  }

  // Compute canvas dimensions and mobile zoom early (needed for measurement)
  var ga = document.getElementById('graph-area');
  var W  = ga && ga.clientWidth  > 10 ? ga.clientWidth  : (forceMobile ? previewWidth : window.innerWidth);
  var H  = ga && ga.clientHeight > 50 ? ga.clientHeight : Math.round(window.innerHeight * 0.60);
  lastMobileW = W;

  // 20% narrower columns: scale all x-positions once.
  // Guard against double-scaling if this data object was already transformed.
  var colScale = data._mobXScaled ? 1 : 0.8;
  data._mobXScaled = true;
  (data.nodes || []).forEach(function(n) {
    if (n.position) n.position.x = Math.round(n.position.x * colScale);
  });
  (data.headers || []).forEach(function(h) { h.x = Math.round(h.x * colScale); });
  var colGap = Math.round(origColGap * colScale);
  var projectW    = Math.round(colGap * 0.93);
  var themeSkillW = Math.round(colGap * 0.50);

  // Compute zoomW now that colGap is known (matches fitWithHeaders mobile path: (W-4)/bbW)
  var bbW   = colGap * 2.5;
  var zoomW = (W - 14) / bbW;

  // Halve all graph font sizes for mobile starting point
  fontNode  = Math.round(fontNode  * 0.5 * 10) / 10;
  fontSubs  = Math.round(fontSubs  * 0.5 * 10) / 10;
  // Extra 15% reduction on header fonts; extra 12% on small devices
  fontHdr1  = Math.round(fontHdr1  * 0.5 * 0.85 * 10) / 10;
  fontHdr2  = Math.round(fontHdr2  * 0.5 * 0.85 * 10) / 10;
  if (W < 400) {
    fontNode = Math.round(fontNode * 0.88 * 10) / 10;
    fontSubs = Math.round(fontSubs * 0.88 * 10) / 10;
  }
  // Mobile payload fontDesc is scaled up for node layout — cap to readable sidebar size
  descFontSize = Math.min(descFontSize, 13);
  applySidebarFonts();

  // Set node widths (heights depend on font, set below)
  (data.nodes || []).forEach(function(n) {
    if (!n.data) return;
    var grp = n.data.group;
    if (grp === 'Project') { if ((n.data.w || 0) < projectW) n.data.w = projectW; }
    else if (grp === 'Theme' || grp === 'Skill' || grp === 'About') { n.data.w = themeSkillW; }
  });

  // Group nodes by column (sort order preserved across re-stacks). About rides in the Skill
  // column (stackCol), so it gets restacked with it instead of keeping its pre-mobile y.
  var colNodes = { Theme: [], Project: [], Skill: [] };
  (data.nodes || []).forEach(function(n) {
    if (!n.data || !n.position) return;
    var grp = stackCol(n.data.group);
    if (colNodes[grp]) colNodes[grp].push(n);
  });
  ['Theme', 'Project', 'Skill'].forEach(function(grp) {
    colNodes[grp].sort(function(a, b) { return a.position.y - b.position.y; });
  });

  function remeasureHeights() {
    (data.nodes || []).forEach(function(n) {
      if (!n.data) return;
      var grp = n.data.group;
      if      (grp === 'Project') n.data.h = measureProjectNodeHeight(n.data, n.data.w);
      else if (grp === 'Theme')   n.data.h = measureThemeNodeHeight(n.data, n.data.w);
      else if (grp === 'Skill')   n.data.h = measureSkillNodeHeight(n.data, n.data.w);
      else if (grp === 'About')   n.data.h = measureThemeNodeHeight(n.data, n.data.w);  // title-only, like Theme
    });
  }

  function restack(gp, gts) {
    var colTotals = {};
    var aboutHdrGap = Math.round(fontHdr1 * 2.2);   // room for the "About" sub-header above its first node
    ['Theme', 'Project', 'Skill'].forEach(function(grp) {
      var nodes = colNodes[grp];
      if (!nodes.length) { colTotals[grp] = 0; return; }
      var gap = (grp === 'Project') ? gp : gts;
      var curY = 0;
      nodes.forEach(function(n, i) {
        var h = n.data.h || 46;
        var g = gap;
        // First About node in the column: leave extra room for the "About" sub-header
        if (i > 0 && n.data.group === 'About' && nodes[i - 1].data.group !== 'About') g = gap + aboutHdrGap;
        curY = (i === 0) ? h / 2 : curY + (nodes[i - 1].data.h || 46) / 2 + g + h / 2;
        n.position.y = curY;
      });
      var last = nodes[nodes.length - 1];
      colTotals[grp] = last.position.y + (last.data.h || 46) / 2;
    });
    var maxH = Math.max(colTotals.Theme || 0, colTotals.Project || 0, colTotals.Skill || 0);
    ['Theme', 'Project', 'Skill'].forEach(function(grp) {
      var off = (maxH - (colTotals[grp] || 0)) / 2;
      if (off > 0) colNodes[grp].forEach(function(n) { n.position.y += off; });
    });
    return maxH;
  }

  function applyHeaderY(gp) {
    var hdrH = fontHdr1 * 1.2 + fontHdr2 * 1.3;
    // Add one title-row above header and one subtitle-row between header and first node
    var hdrY = -Math.round(gp + fontHdr2 * 1.3 + hdrH + fontHdr1 * 1.2);
    data.headers.forEach(function(h) { h.y = hdrY; });
    return hdrY;
  }

  // Initial layout
  remeasureHeights();
  var gapThemeSkill = gapProject * 2;
  var maxColH = restack(gapProject, gapThemeSkill);
  applyHeaderY(gapProject);

  // Scale fonts/gaps to fill available vertical space (both up and down).
  // fitWithHeaders() zooms by WIDTH only on mobile: zoomW = (W-4)/bbW
  // Node heights are in CSS/cyto units (nodeHtmlLabel scales via transform:scale(zoom),
  // so word-wrap happens at the full cyto-unit width, not screen px).
  // pan.y is shifted down by extraHdrH*zoomW to reveal the applyHeaderY spacing, so
  // targetBBH must subtract extraHdrH from the usable vertical space per iteration.
  var hm = (data.headerMargin) || 70;
  for (var iter = 0; iter < 6; iter++) {
    var extraHdrH = fontHdr1 * 2.4 + fontHdr2 * 1.3; // must match mobileHdrExtra in fitWithHeaders
    var targetBBH = ((H - 28) / zoomW - hm - extraHdrH) * 0.96;
    var scale = Math.max(0.25, Math.min(4.0, targetBBH / maxColH));
    if (Math.abs(scale - 1) < 0.02) break;
    fontNode  = Math.max(5, Math.round(fontNode  * scale * 10) / 10);
    fontSubs  = Math.max(4, Math.round(fontSubs  * scale * 10) / 10);
    fontHdr1  = Math.round(fontHdr1  * scale * 10) / 10;
    fontHdr2  = Math.round(fontHdr2  * scale * 10) / 10;
    gapProject    = Math.max(2, Math.round(gapProject * scale));
    gapThemeSkill = gapProject * 2;
    remeasureHeights();
    maxColH = restack(gapProject, gapThemeSkill);
    applyHeaderY(gapProject);
  }
  // Equalize node heights within each group so columns have uniform rows.
  // Theme nodes get an extra 20% height to improve clickability.
  ['Theme', 'Project', 'Skill'].forEach(function(grp) {
    var maxH = 0;
    colNodes[grp].forEach(function(n) { maxH = Math.max(maxH, n.data.h || 0); });
    if (maxH > 0) {
      if (grp === 'Theme') maxH = Math.round(maxH * 1.2);
      colNodes[grp].forEach(function(n) { n.data.h = maxH; });
    }
  });
  maxColH = restack(gapProject, gapThemeSkill);
  applyHeaderY(gapProject);

  // After iteration: fill remaining vertical slack by expanding gaps only (no font change).
  // fitWithHeaders' min(zoomW,zoomH) is the safety net against overflow.
  var slackCyto = (H - 28) / zoomW - hm - extraHdrH - maxColH;
  if (slackCyto > 5) {
    var gapScale = (maxColH + slackCyto * 0.92) / maxColH;
    gapProject    = Math.max(2, Math.round(gapProject * gapScale));
    gapThemeSkill = gapProject * 2;
    maxColH = restack(gapProject, gapThemeSkill);
    applyHeaderY(gapProject);
  }
}

function initCyGraph(data) {
  lastData = data;
  resetInlineExpansion();   // fresh graph — drop any stale inline expansion state
  applyDataGlobals(data);
  applyColors();
  applyMobileLayout();
  applyNarrowScale(data);   // narrow screens: scale base gap/node widths before snapshot & auto-fit
  saveLayoutSnapshot(data);
  autoFitProjectWidth(data);
  applyMobileNodeSizes(data);
  initAccordions();
  resizeCy();
  var cyContainer = document.getElementById('cy');
  if (cyContainer) cyContainer.innerHTML = ''; // remove stale nodeHtmlLabel overlay from prior instance
  cy = cytoscape({
    container: cyContainer, elements: buildElements(data),
    layout: { name: 'preset' }, style: buildStyle(),
    userZoomingEnabled: true, userPanningEnabled: true,
    boxSelectionEnabled: false, autoungrabify: true,
  });
  buildBaseGradients();
  applyNodeBorderColors();
  cy.style(buildStyle());
  cy.nodeHtmlLabel([{ query: 'node', tpl: function (d) { return nodeHtml(d); } }]);
  snapshotInlineFillBase();   // base horizontal geometry for fill-space distribution
  fitGraph();
  // Deep link: open the node named in ?node=<id> once the graph is ready
  if (pendingNodeParam) {
    var _pn = pendingNodeParam; pendingNodeParam = null; pendingScrollNode = String(_pn);
    setTimeout(function () { openNodeById(_pn); }, 500);
  } else {
    setTimeout(openDefaultInline, 500);   // otherwise open the openDefault node (e.g. "What is this site about")
  }
  // On mobile, disable panning for touches starting in the top 55px of the graph
  // area so the browser's native pull-to-refresh gesture still works.
  if (mobileMode) {
    cy.on('touchstart', function(e) {
      var oe = e.originalEvent;
      if (!oe || !oe.touches || !oe.touches[0]) return;
      var ga = document.getElementById('graph-area');
      if (!ga) return;
      var touchY = oe.touches[0].clientY - ga.getBoundingClientRect().top;
      cy.userPanningEnabled(touchY > 55);
    });
    cy.on('touchend touchcancel', function() { cy.userPanningEnabled(true); });
  // Pull-to-refresh zone: created once, survives cy reinits (lives in #graph-area not #cy)
  if (!document.getElementById('mobile-top-zone')) {
    var ptrZone = document.createElement('div');
    ptrZone.id = 'mobile-top-zone';
    var ptrGa = document.getElementById('graph-area');
    if (ptrGa) {
      ptrGa.appendChild(ptrZone);
      var ptrStartY = null;
      ptrZone.addEventListener('touchstart', function(e) {
        ptrStartY = e.touches[0].clientY;
      }, { passive: true });
      ptrZone.addEventListener('touchmove', function(e) {
        if (ptrStartY !== null && e.touches[0].clientY - ptrStartY > 80) {
          ptrStartY = null;
          location.reload();
        }
      }, { passive: true });
      ptrZone.addEventListener('touchend', function() { ptrStartY = null; }, { passive: true });
    }
  }
  }
  cy.on('tap', function (evt) { if (evt.target === cy && !inlineMode) hideDescPanel(); });
  cy.on('tap', 'node', function (evt) {
    var d = evt.target.data(), g = d.group;
    if (isColNode(g)) {
      var id = parseFloat(d.id);
      // Inline mode: toggle this node independently; multiple can stay open at once. No click highlight.
      if (inlineMode) {
        if (inlineExpandedMap[String(id)]) { collapseNodeInline(id); return; }
        if (window.Shiny) Shiny.setInputValue('clicked_node_id', id, { priority: 'event' });
        return;
      }
      if (selectedNodeId === id) { hideDescPanel(); return; }
      selectNode(id);
      if (window.Shiny) Shiny.setInputValue('clicked_node_id', id, { priority: 'event' });
    }
  });
  // Hover only on desktop
  if (!mobileMode) {
    cy.on('mouseover', 'node', function (evt) { hoveredNodeId = evt.target.data('id'); _edgeHoverActive = false; applyHighlightState(); });
    cy.on('mouseout', 'node', function () { if (_edgeHoverActive) return; hoveredNodeId = null; applyHighlightState(); });
  }
  cy.on('pan zoom', function () { positionHeaders(lastData); drawEdgeOverlay(); drawNodeConnector(); });
  // Dragging on a node pans the graph (nodes are non-grabbable)
  cy.on('vmousedown', 'node', function(evt) {
    if (inlineMode) return;   // inline mode scrolls the page instead of panning the graph
    var oe = evt.originalEvent; if (!oe) return;
    var t = oe.touches && oe.touches[0];
    var sx = t ? t.clientX : oe.clientX, sy = t ? t.clientY : oe.clientY;
    var px0 = cy.pan().x, py0 = cy.pan().y, moved = false;
    function move(e) {
      var tt = e.touches && e.touches[0];
      var cx = tt ? tt.clientX : e.clientX, cy_ = tt ? tt.clientY : e.clientY;
      if (!moved && Math.abs(cx - sx) < 4 && Math.abs(cy_ - sy) < 4) return;
      moved = true;
      cy.pan({ x: px0 + cx - sx, y: py0 + cy_ - sy });
    }
    function up() {
      document.removeEventListener('mousemove', move);
      document.removeEventListener('touchmove', move);
      document.removeEventListener('mouseup', up);
      document.removeEventListener('touchend', up);
    }
    document.addEventListener('mousemove', move);
    document.addEventListener('touchmove', move, { passive: false });
    document.addEventListener('mouseup', up);
    document.addEventListener('touchend', up);
  });
  positionHeaders(data); drawEdgeOverlay();
  setTimeout(syncMobileTabs, 300);
  setTimeout(syncMobileTabs, 1200);
  if (mobileMode) initSettingsTab();
}

/* ── Mobile layout toggle ────────────────────────────────────────────────── */

function applyMobileLayout() {
  var body = document.body;
  var ga = document.getElementById('graph-area');
  
  if (mobileMode) {
    // In forceMobile (author preview on wide screen), constrain graph-area width
    if (forceMobile && window.innerWidth > MOBILE_BREAKPOINT) {
      body.classList.remove('mobile-mode');
      body.classList.add('mobile-preview');
      if (ga) {
        ga.style.maxWidth = previewWidth + 'px';
        ga.style.height = previewHeight + 'px';
        ga.style.margin = '0 auto';
        ga.style.borderLeft = '2px solid rgba(255,255,255,0.15)';
        ga.style.borderRight = '2px solid rgba(255,255,255,0.15)';
      }
    } else {
      // Real mobile viewport
      body.classList.add('mobile-mode');
      body.classList.remove('mobile-preview');
      if (ga) { ga.style.maxWidth = ''; ga.style.height = ''; ga.style.margin = ''; ga.style.borderLeft = ''; ga.style.borderRight = ''; }
    }
  } else {
    body.classList.remove('mobile-mode');
    body.classList.remove('mobile-preview');
    if (ga) { ga.style.maxWidth = ''; ga.style.margin = ''; ga.style.borderLeft = ''; ga.style.borderRight = ''; ga.style.height = ''; }
  }
  var handle = document.getElementById('mob-handle');
  var panel  = document.getElementById('mob-panel');
  var show = mobileMode ? 'flex' : 'none';
  if (handle) handle.style.display = show;
  if (panel)  panel.style.display  = show;
}

/* ── Shiny Message Handlers ──────────────────────────────────────────────── */

var rawPayload = null; // store full payload for resize switching

Shiny.addCustomMessageHandler('initCy', function (data) {
  rawPayload = data;
  if (postInitResizeHandler) { window.removeEventListener('resize', postInitResizeHandler); postInitResizeHandler = null; }
  var picked = pickData(data);
  if (cy) { cy.destroy(); cy = null; }
  initCyGraph(picked);
  if (isExportMode) {
    setTimeout(function() {
      var r = document.createElement('div');
      r.id = 'export-ready'; r.style.display = 'none';
      document.body.appendChild(r);
    }, 1200);
  }
  // After init, check once whether the viewport has settled to different dimensions.
  // Handles DevTools phone emulation (and some mobile browsers) where viewport is
  // applied after initCy fires. Both a resize-event trigger and a 300ms timeout are
  // used; a shared flag ensures only the first one reinitialises.
  // After init, watch for the viewport settling to different dimensions.
  // Handles Chrome DevTools phone emulation applying dimensions after initCy fires.
  // Two mechanisms: debounced resize handler (fast, event-driven) + 1500ms fallback.
  // Whichever fires first cancels the other to avoid double reinit.
  var capturedMM = mobileMode, capturedW = lastMobileW;
  var postInitDebounce = null, postInitFallback = null;
  function postInitReinit() {
    if (!rawPayload) return;
    var nowMobile = useMobileLayout();
    var gaEl = document.getElementById('graph-area');
    var nowW = gaEl ? gaEl.clientWidth : window.innerWidth;
    if (nowMobile !== capturedMM || (nowMobile && Math.abs(nowW - capturedW) > 20)) {
      lastMobileState = nowMobile;
      var p = pickData(rawPayload);
      if (cy) { cy.destroy(); cy = null; }
      initCyGraph(p);
    }
  }
  postInitResizeHandler = function () {
    clearTimeout(postInitDebounce);
    clearTimeout(postInitFallback);
    postInitDebounce = setTimeout(function () {
      if (postInitResizeHandler) { window.removeEventListener('resize', postInitResizeHandler); postInitResizeHandler = null; }
      postInitReinit();
    }, 200);
  };
  window.addEventListener('resize', postInitResizeHandler);
  postInitFallback = setTimeout(function () {
    clearTimeout(postInitDebounce);
    if (postInitResizeHandler) { window.removeEventListener('resize', postInitResizeHandler); postInitResizeHandler = null; }
    postInitReinit();
  }, 1500);
});

Shiny.addCustomMessageHandler('updateCy', function (data) {
  rawPayload = data;
  var picked = pickData(data);
  if (!cy) { initCyGraph(picked); return; }
  resetInlineExpansion();   // rebuilt elements — drop any stale inline expansion state
  lastData = picked; var prevSel = selectedNodeId;
  applyDataGlobals(picked);
  applyColors();
  applyMobileLayout();
  applyNarrowScale(picked);   // narrow screens: scale base gap/node widths before snapshot & auto-fit
  saveLayoutSnapshot(picked);
  autoFitProjectWidth(picked);
  applyMobileNodeSizes(picked);
  cy.elements().remove(); cy.add(buildElements(picked));
  buildBaseGradients(); applyNodeBorderColors(); cy.style(buildStyle());
  cy.layout({ name: 'preset' }).run(); positionHeaders(picked);
  snapshotInlineFillBase();   // base horizontal geometry for fill-space distribution
  if (prevSel) selectNode(prevSel); else drawEdgeOverlay();
  if (inlineMode && !useMobileLayout()) { cy.userZoomingEnabled(false); cy.userPanningEnabled(false); layoutInlineScroll(); }
});


/* ── Accordion ───────────────────────────────────────────────────────────── */

function toggleAcc(header) {
  var section = header.parentElement;
  var body = section.querySelector('.acc-body');
  if (!body) return;
  var isDescAcc = section.id === 'acc-desc';
  if (section.classList.contains('acc-open')) {
    if (!isDescAcc) body.style.maxHeight = body.scrollHeight + 'px';
    section.classList.remove('acc-open');
    if (!isDescAcc) {
      requestAnimationFrame(function() {
        requestAnimationFrame(function() { body.style.maxHeight = '0px'; });
      });
    }
    if (isDescAcc) hideDescPanel();
  } else {
    section.classList.add('acc-open');
    if (!isDescAcc) {
      body.style.maxHeight = body.scrollHeight + 'px';
      body.addEventListener('transitionend', function onEnd() {
        body.removeEventListener('transitionend', onEnd);
        if (section.classList.contains('acc-open')) body.style.maxHeight = 'none';
      });
    }
  }
}

function applyAccTitles() {
  var map = {
    'acc-title-desc':  (currentLang === 'fi' && accTitleData.details_title_fi) ? accTitleData.details_title_fi : accTitleData.details_title_en,
    'acc-title-about': (currentLang === 'fi' && accTitleData.intro_title_fi)   ? accTitleData.intro_title_fi   : accTitleData.intro_title_en,
    'acc-title-vote':  (currentLang === 'fi' && accTitleData.vote_title_fi)    ? accTitleData.vote_title_fi    : accTitleData.vote_title_en,
    'acc-title-fund':  (currentLang === 'fi' && accTitleData.fund_title_fi)    ? accTitleData.fund_title_fi    : accTitleData.fund_title_en
  };
  Object.keys(map).forEach(function(id) {
    var el = document.getElementById(id);
    if (el && map[id]) el.textContent = map[id];
  });
}

function applyDescPanelLang() {
  if (!lastDescMsg) return;
  var msg = lastDescMsg;
  var dTitle = (currentLang === 'fi' && msg.title_fi) ? msg.title_fi : (msg.title || '');
  var dText  = (currentLang === 'fi' && msg.text_fi)  ? msg.text_fi  : (msg.text  || '');
  var title = document.getElementById('desc-title');
  var body  = document.getElementById('desc-body');
  if (title) title.textContent = dTitle;
  if (body)  body.innerHTML = mdToHtml(dText);
  var bsTitle = document.getElementById('mobile-bs-title');
  var bsBody  = document.getElementById('mobile-bs-body');
  if (bsTitle) bsTitle.textContent = dTitle;
  if (bsBody)  bsBody.innerHTML = mdToHtml(dText);
}

function setLanguage(lang) {
  currentLang = lang;
  if (typeof Shiny !== 'undefined' && !Shiny._isStatic)
    Shiny.setInputValue('current_lang_active', lang, {priority: 'event'});
  document.body.classList.toggle('lang-fi', lang === 'fi');
  var url = new URL(window.location.href);
  if (lang === 'fi') { url.searchParams.set('lang', 'fi'); } else { url.searchParams.delete('lang'); }
  window.history.replaceState(null, '', url.toString());
  var btnEn = document.getElementById('lang-btn-en');
  var btnFi = document.getElementById('lang-btn-fi');
  if (btnEn) btnEn.classList.toggle('lang-active', lang === 'en');
  if (btnFi) btnFi.classList.toggle('lang-active', lang === 'fi');
  var mobBtnEn = document.getElementById('mob-lang-btn-en');
  var mobBtnFi = document.getElementById('mob-lang-btn-fi');
  if (mobBtnEn) mobBtnEn.classList.toggle('lang-active', lang === 'en');
  if (mobBtnFi) mobBtnFi.classList.toggle('lang-active', lang === 'fi');
  applyAccTitles();
  applyDescPanelLang();
  var titleStr = (lang === 'fi' ? langData.page_title_fi : langData.page_title_en) || langData.page_title_en;
  if (titleStr) document.title = titleStr;
  if (cy && lastData) positionHeaders(lastData);
}

Shiny.addCustomMessageHandler('updateAccTitles', function(t) {
  accTitleData.details_title_en = t.details_title;
  accTitleData.intro_title_en   = t.intro_title;
  accTitleData.vote_title_en    = t.vote_title;
  accTitleData.fund_title_en    = t.fund_title;
  applyAccTitles();
});

Shiny.addCustomMessageHandler('setLanguageData', function(d) {
  langData.page_title_en       = d.page_title_en;
  langData.page_title_fi       = d.page_title_fi;
  accTitleData.details_title_fi = d.details_title_fi;
  accTitleData.intro_title_fi   = d.intro_title_fi;
  accTitleData.vote_title_fi    = d.vote_title_fi;
  accTitleData.fund_title_fi    = d.fund_title_fi;
  var titleFiEl = document.getElementById('page-title-fi');
  if (titleFiEl) titleFiEl.textContent = d.page_title_fi || d.page_title_en || '';
  var btnEn = document.getElementById('lang-btn-en');
  var btnFi = document.getElementById('lang-btn-fi');
  if (btnEn) btnEn.classList.toggle('lang-active', currentLang === 'en');
  if (btnFi) btnFi.classList.toggle('lang-active', currentLang === 'fi');
  applyAccTitles();
});

/* Convert bare https?:// URLs in already-HTML content (e.g. with &lt;br&gt; tags) to links */
function linkifyHtml(html) {
  return String(html).replace(/(https?:\/\/[^\s<>"&]+)/g, function(url) {
    var s = 'color:inherit;opacity:0.85;text-decoration:underline;cursor:pointer;';
    return '<a href="' + url + '" target="_blank" rel="noopener" style="' + s + '">' + url + '</a>';
  });
}

/* Inline markdown: [label](url) and bare https?:// URLs become clickable links */
// Bold/italic on already-escaped text: **x**/__x__ → <strong>, *x*/_x_ → <em>.
// Bold is applied before italic so the inner ** isn't eaten by the * rule.
function inlineEmph(s) {
  s = s.replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>');
  s = s.replace(/__([^_]+)__/g, '<strong>$1</strong>');
  s = s.replace(/\*([^*\n]+)\*/g, '<em>$1</em>');
  s = s.replace(/_([^_\n]+)_/g, '<em>$1</em>');
  return s;
}

function processInline(text) {
  var re = /\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)|(https?:\/\/\S+)/g;
  var out = '', last = 0, m;
  while ((m = re.exec(text)) !== null) {
    out += inlineEmph(esc(text.slice(last, m.index)));
    var linkStyle = 'color:inherit;opacity:0.85;text-decoration:underline;cursor:pointer;';
    if (m[1]) {
      out += '<a href="' + esc(m[2]) + '" target="_blank" rel="noopener" style="' + linkStyle + '">' + inlineEmph(esc(m[1])) + '</a>';
    } else {
      out += '<a href="' + esc(m[3]) + '" target="_blank" rel="noopener" style="' + linkStyle + '">' + esc(m[3]) + '</a>';
    }
    last = m.index + m[0].length;
  }
  return out + inlineEmph(esc(text.slice(last)));
}

/* Simple markdown to HTML for descriptions */
function mdToHtml(text) {
  var lines = String(text).split('\n');
  var out = [];
  var inFence = false;
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    var t = line.trim();
    // Fenced code blocks: drop entirely (no highlighting inline; the external page has the real thing).
    if (/^(```|~~~)/.test(t)) { inFence = !inFence; continue; }
    if (inFence) continue;
    // Block syntax the minimal renderer can't do — skip so it doesn't print as raw text on the node.
    if (/^!\[/.test(t)) continue;                    // images
    if (/^\|/.test(t)) continue;                     // table rows
    if (/^:::/.test(t)) continue;                    // pandoc/quarto fenced divs & callouts
    if (/^\{\{[<%]/.test(t)) continue;               // quarto shortcodes / includes
    if (/^(-{3,}|\*{3,}|_{3,})$/.test(t)) continue;  // horizontal rules / thematic breaks
    var m = line.match(/^\s*(#{1,6})\s+(.+)$/);      // ATX headings (space required after #s)
    if (m) {
      var level = Math.min(m[1].length + 1, 6);
      out.push('<h' + level + ' style="margin:12px 0 4px;font-size:' + (descFontSize + 2) + 'px;font-weight:bold;">' + processInline(m[2]) + '</h' + level + '>');
    } else if (t === '') {
      out.push('<br>');
    } else if (/^>\s?/.test(t)) {
      out.push(processInline(t.replace(/^>\s?/, '')));   // blockquote → plain line
    } else {
      out.push(processInline(line));
    }
  }
  return out.join('<br>');
}

// Description-panel accent color: Theme/Skill use the node's own edge (source) color;
// Project uses the project group color (which equals the "one color" when that mode is on).
function descAccentColor(msg) {
  var grp = (msg && msg.group) || 'Project';
  if (grp === 'Theme' || grp === 'Skill') {
    var bn = (msg && msg.nodeId != null && cy) ? cy.getElementById(String(msg.nodeId)) : null;
    if (bn && bn.length && !bn.empty()) {
      var c = lightMode ? (bn.data('_borderColLight') || bn.data('lightEdgeColor') || bn.data('edgeColor'))
                        : (bn.data('_borderColDark')  || bn.data('edgeColor'));
      if (c) return c;
    }
    return grp === 'Theme' ? colTheme : colSkill;
  }
  return colProject;
}

Shiny.addCustomMessageHandler('showDescPanel', function (msg) {
  lastDescMsg = msg;
  var grp = msg.group || 'Project';
  var c = descAccentColor(msg);
  var dTitle = (currentLang === 'fi' && msg.title_fi) ? msg.title_fi : (msg.title || '');
  var dText  = (currentLang === 'fi' && msg.text_fi) ? msg.text_fi : (msg.text || '');

  // Inline UI mode: render the description inside the node instead of the sidebar
  if (inlineMode) { expandNodeFromDesc(msg); return; }

  setNodeUrl(msg.nodeId);   // classic sidebar: reflect the open node in the URL

  // On mobile, append project type in parentheses after title
  if (grp === 'Project' && msg.nodeId && cy) {
    var nodeEl = cy.getElementById(String(msg.nodeId));
    var ptype = nodeEl && !nodeEl.empty() ? (nodeEl.data('ptype') || '') : '';
    if (ptype) {
      var ptypeFiMap = { 'Text': 'Teksti', 'Text, long': 'Pitkä teksti', 'Text, short': 'Lyhyt teksti', 'Website': 'Nettisivu' };
      var ptypeDisp = (currentLang === 'fi' && ptypeFiMap[ptype]) ? ptypeFiMap[ptype] : ptype;
      dTitle = dTitle + ' (' + ptypeDisp + ')';
    }
  }

  if (mobileMode) {
    var mdTitle = document.getElementById('mob-desc-title');
    var mdBody  = document.getElementById('mob-desc-body');
    var mdClose = document.getElementById('mob-desc-close');
    var mdHdr   = document.getElementById('mob-desc-header');
    if (!mdTitle || !mdBody) return;
    mdTitle.textContent = dTitle;
    // Mobile sheet / desktop sidebar are plain panels (no inline-node expand): link to the article only.
    mdBody.innerHTML = mdToHtml(dText) +
      articleControlsHtml({ nodeId: msg.nodeId, hasArticle: msg.hasArticle, articleUrl: msg.articleUrl }, false);
    mdTitle.style.color = c;
    if (mdClose) { mdClose.style.color = c; mdClose.style.borderColor = c; }
    if (mdHdr) mdHdr.style.borderBottomColor = c;
    sheetMode = 'desc';
    mobOpenDesc();
  } else {
    // Desktop: show sidebar panel — ensure description accordion is open
    var accDesc = document.getElementById('acc-desc');
    if (accDesc && !accDesc.classList.contains('acc-open')) {
      accDesc.classList.add('acc-open');
    }
    var panel = document.getElementById('desc-panel');
    var title = document.getElementById('desc-title');
    var body = document.getElementById('desc-body');
    var close = document.getElementById('desc-close');
    if (!panel || !title || !body) return;
    title.textContent = dTitle;
    body.innerHTML = mdToHtml(dText) +
      articleControlsHtml({ nodeId: msg.nodeId, hasArticle: msg.hasArticle, articleUrl: msg.articleUrl }, false);
    applySidebarFonts();
    panel.style.borderColor = c; title.style.color = c;
    if (close) { close.style.color = c; close.style.borderColor = c; }
    panel.style.display = 'flex';
    if (accDesc) {
      accDesc.classList.add('desc-visible');
      var accBody = accDesc.querySelector('.acc-body');
      if (accBody) {
        accBody.style.height = 'auto'; // lift constraint so flex:1 child fills content
        void accBody.offsetHeight;     // force synchronous reflow
        var contentH = accBody.scrollHeight;
        var minH = window.innerHeight * 0.4;
        var maxH = window.innerHeight * 0.6;
        accBody.style.height = Math.min(maxH, Math.max(minH, contentH)) + 'px';
      }
    }
    // Hide hint text while description is showing
    var hint = document.getElementById('sidebar-hint'); if (hint) hint.style.display = 'none';
  }
  if (msg.nodeId) selectNode(msg.nodeId);
  drawNodeConnector();
});

/* ── File download trigger ───────────────────────────────────────────────── */

Shiny.addCustomMessageHandler('triggerDownload', function (msg) {
  var a = document.createElement('a');
  a.href = msg.url;
  a.download = msg.filename || 'download';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
});

var colorPickerIds = ['col_bg','col_sidebar_bg','col_node_bg','col_theme','col_project','col_skill','col_all',
                      'light_col_bg','light_col_sidebar_bg','light_col_node_bg','light_col_theme','light_col_project','light_col_skill','light_col_all'];

function bindColorPickers() {
  colorPickerIds.forEach(function(id) {
    var el = document.getElementById(id);
    if (!el || el._colorBound) return;
    el._colorBound = true;
    // 'input' fires while dragging; 'change' fires on close — listen to both
    ['input', 'change'].forEach(function(evt) {
      el.addEventListener(evt, function () {
        if (window.Shiny) Shiny.setInputValue(id, el.value, { priority: 'event' });
      });
    });
  });
}
// Attach after Shiny is ready (pickers are in Shiny-rendered HTML)
$(document).on('shiny:sessioninitialized', bindColorPickers);

Shiny.addCustomMessageHandler('setColorInputs', function (msg) {
  colorPickerIds.forEach(function(id) {
    var el = document.getElementById(id);
    if (el && msg[id]) { el.value = msg[id]; Shiny.setInputValue(id, msg[id]); }
  });
});

Shiny.addCustomMessageHandler('setEdgeWidth', function (msg) {
  baseEdgeWidth = msg.width || 2.5;
  drawEdgeOverlay();
  drawNodeConnector();
});

// Ribbon/band edges toggle + the gap between bands at a node.
Shiny.addCustomMessageHandler('setEdgeBands', function (msg) {
  edgeBands = !!(msg && msg.value);
  drawEdgeOverlay();
});
Shiny.addCustomMessageHandler('setEdgeGap', function (msg) {
  edgeGap = (msg && msg.gap != null) ? msg.gap : 3;
  if (edgeBands) drawEdgeOverlay();
});
Shiny.addCustomMessageHandler('setEdgeSankey', function (msg) {
  edgeSankey = !!(msg && msg.value);
  if (edgeBands) drawEdgeOverlay();
});
Shiny.addCustomMessageHandler('setEdgeTransparency', function (msg) {
  var pct = (msg && msg.pct != null) ? msg.pct : 18;
  edgeOpacity = Math.max(0, Math.min(1, 1 - pct / 100));
  drawEdgeOverlay();
});
Shiny.addCustomMessageHandler('setEdgeMinWidth', function (msg) {
  edgeMinWidth = (msg && msg.width != null) ? msg.width : 2.5;
  if (edgeBands) drawEdgeOverlay();
});
Shiny.addCustomMessageHandler('setEdgeMinOn', function (msg) {
  edgeMinOn = !!(msg && msg.value);
  if (edgeBands) drawEdgeOverlay();
});
Shiny.addCustomMessageHandler('setEdgeCurve', function (msg) {
  edgeCurve = (msg && msg.exp != null && msg.exp > 0) ? msg.exp : 1;
  if (edgeBands) drawEdgeOverlay();
});
Shiny.addCustomMessageHandler('setEdgePinHeader', function (msg) {
  edgePinHeader = !!(msg && msg.value);
  drawEdgeOverlay();
  if (cy) cy.emit('render');   // gradient/outline bands realign to the header region
});
Shiny.addCustomMessageHandler('setFillNodeW', function (msg) {
  fillNodeW = (msg && msg.pct != null) ? Math.max(0, msg.pct) : 0;
  if (inlineMode && !useMobileLayout()) layoutInlineScroll();
});
Shiny.addCustomMessageHandler('setFillProjW', function (msg) {
  fillProjW = (msg && msg.pct != null) ? Math.max(0, msg.pct) : 0;
  if (inlineMode && !useMobileLayout()) layoutInlineScroll();
});
Shiny.addCustomMessageHandler('setFillColGap', function (msg) {
  fillColGap = (msg && msg.pct != null) ? Math.max(0, msg.pct) : 0;
  if (inlineMode && !useMobileLayout()) layoutInlineScroll();
});
Shiny.addCustomMessageHandler('setFillNodePad', function (msg) {
  fillNodePad = (msg && msg.pct != null) ? Math.max(0, msg.pct) : 0;
  if (inlineMode && !useMobileLayout()) layoutInlineScroll();
});

Shiny.addCustomMessageHandler('setGradientExtent', function (msg) {
  gradientExtent = (msg.pct != null) ? msg.pct : 20;
  if (cy) cy.emit('render');  // re-runs nodeHtmlLabel -> nodeHtml -> gradientOverlay with new extent
});

// Transparency of the node's inside gradient fill (msg.pct = % transparent; 0 = opaque). Outline unaffected.
Shiny.addCustomMessageHandler('setGradientTransparency', function (msg) {
  var pct = (msg.pct != null) ? msg.pct : 40;
  setGradientFillAlpha(1 - pct / 100);
  if (cy) { buildBaseGradients(); applyHighlightState(); cy.emit('render'); }
});

// Falloff curve of the inside gradient (msg.curve: 1 = linear, >1 = fades faster toward the border).
Shiny.addCustomMessageHandler('setGradientCurve', function (msg) {
  gradientCurve = (msg.curve != null && msg.curve > 0) ? msg.curve : 1;
  if (cy) cy.emit('render');  // re-runs gradientOverlay with the new falloff
});

// Hover extent multiplier: hover widens the base gradient by this factor (1 = none, 0 = hidden on hover).
Shiny.addCustomMessageHandler('setGradientHoverMult', function (msg) {
  gradientHoverMult = (msg.mult != null && msg.mult >= 0) ? msg.mult : 2;
  if (cy) cy.emit('render');
});

// Accordion open/closed indicator style on node titles (inline UI). msg.style keys into ACC_ICONS.
Shiny.addCustomMessageHandler('setAccordionIcon', function (msg) {
  accordionIcon = (msg.style && ACC_ICONS[msg.style]) ? msg.style : 'triangle';
  if (cy) cy.emit('render');
});

// Font size (px) of the accordion symbol.
Shiny.addCustomMessageHandler('setAccordionIconSize', function (msg) {
  accordionIconSize = (msg.size != null && msg.size > 0) ? msg.size : 14;
  if (cy) cy.emit('render');
});

// Author app only: toggle in-place editing of node titles + descriptions (inline UI).
Shiny.addCustomMessageHandler('setAuthorEditable', function (msg) {
  authorEditable = !!(msg && msg.value);
  document.body.classList.toggle('author-editable', authorEditable);
  if (!authorEditable && descEditId) commitDescEdit();
  if (!authorEditable && articleEditId) commitArticleEdit();
  if (cy) cy.emit('render');
});

Shiny.addCustomMessageHandler('setNodeOutline', function (msg) {
  nodeOutlineWidth = (msg.width != null) ? msg.width : 3;
  if (cy) cy.style(buildStyle());  // re-apply node border widths
});

Shiny.addCustomMessageHandler('setProjectOutline', function (msg) {
  projectOutlineWidth = (msg.width != null) ? msg.width : 3;
  if (cy) { cy.style(buildStyle()); cy.emit('render'); }  // re-draw Project outline overlay
});

Shiny.addCustomMessageHandler('setOutlineSaturation', function (msg) {
  outlineSaturation = (msg.value != null) ? msg.value : 1;
  if (cy) cy.emit('render');  // re-draw outline overlays with new saturation
});

Shiny.addCustomMessageHandler('setOutlineTransparency', function (msg) {
  var pct = (msg.pct != null) ? msg.pct : 0;
  outlineOpacity = Math.max(0, Math.min(1, 1 - pct / 100));
  if (cy) { cy.style(buildStyle()); cy.emit('render'); }  // About border + overlay outlines
});

Shiny.addCustomMessageHandler('setNodePad', function (msg) {
  nodeTextPad = (msg.px != null) ? msg.px : 0;
  if (cy) refreshLayout();  // re-fit project widths (autoFitProjectWidth) + re-render node bodies
});

Shiny.addCustomMessageHandler('setDescPad', function (msg) {
  descPad = (msg.px != null) ? msg.px : 10;
  // Re-measure every open description (its height depends on padding) and reflow.
  if (cy) {
    Object.keys(inlineExpandedMap).forEach(function (id) { setExpandedHeight(id, false); });
    reflowInline();
    cy.emit('render');
  }
});

/* ── Force Mobile Preview (author app) ───────────────────────────────────── */

Shiny.addCustomMessageHandler('setForceMobile', function (msg) {
  var newVal = !!msg.value;
  var newW = msg.width || 390;
  var newH = msg.height || 844;
  var changed = (newVal !== forceMobile) || (newW !== previewWidth) || (newH !== previewHeight);
  forceMobile = newVal;
  previewWidth = newW;
  previewHeight = newH;
  // Clean up sheet state when toggling
  if (!newVal) hideBottomSheet();
  if (changed && rawPayload) {
    var picked = pickData(rawPayload);
    if (cy) { cy.destroy(); cy = null; }
    initCyGraph(picked);
  }
});

Shiny.addCustomMessageHandler('setNodeBgSameAsGraph', function (val) {
  nodeBgSameAsGraph = !!val;
  if (cy) cy.style(buildStyle());
});

// Cytoscape swallows wheel events, so in inline mode we scroll the graph area ourselves.
var _inlineWheelBound = false;
function bindInlineWheel() {
  if (_inlineWheelBound) return;
  var ga = document.getElementById('graph-area');
  if (!ga) return;
  ga.addEventListener('wheel', function (e) {
    if (!inlineMode || !cy) return;     // only intercept in inline mode
    // Ctrl/⌘ + wheel (trackpad pinch sends this automatically) → magnify, focused on the cursor.
    if (e.ctrlKey || e.metaKey) {
      e.preventDefault(); e.stopPropagation();
      setUiZoom((uiZoom || 1) * Math.exp(-e.deltaY * 0.005), e.clientX, e.clientY);  // 0.005 = half the old sensitivity
      return;
    }
    var dy = e.deltaY, dx = e.deltaX;
    if (e.deltaMode === 1) { dy *= 16; dx *= 16; }               // lines → px
    else if (e.deltaMode === 2) { dy *= ga.clientHeight; dx *= ga.clientWidth; } // pages → px
    // A scrollable inline article body consumes the wheel first (nested scroll) before the column,
    // but only while it still has room to scroll in that direction — otherwise fall through to the
    // column scroll below. We scroll it ourselves (cyto wheel-zoom is off inline) so nothing else moves.
    var ab = e.target && e.target.closest && e.target.closest('.inline-article-body');
    if (ab && !e.shiftKey && Math.abs(dy) >= Math.abs(dx)) {
      var atTop = ab.scrollTop <= 0;
      var atBot = ab.scrollTop + ab.clientHeight >= ab.scrollHeight - 1;
      if ((dy < 0 && !atTop) || (dy > 0 && !atBot)) {
        ab.scrollTop += dy; e.preventDefault(); e.stopPropagation(); return;
      }
    }
    // Shift + wheel (or a horizontal wheel) → pan left/right when zoomed in past the viewport.
    if (e.shiftKey || Math.abs(dx) > Math.abs(dy)) {
      e.preventDefault(); e.stopPropagation();
      if (uiZoom > 1) applyInlinePanX(-(e.shiftKey ? dy : dx));
      return;
    }
    var g = inlineColumnAt(e.clientX);  // plain wheel → scroll only the column under the pointer
    if (g && inlineBase) {              // only scrollable once something is expanded / magnified
      inlineColScroll[g] = (inlineColScroll[g] || 0) + dy / cy.zoom();  // screen px → cyto units
      applyInlinePositions();           // clamps to the column's scrollable range
      cy.emit('render');
      if (lastData) positionHeaders(lastData);   // headers scroll with their column
      drawEdgeOverlay();
    }
    e.preventDefault();                 // stop Cytoscape zoom
    e.stopPropagation();
  }, { passive: false, capture: true });

  // Two-finger pinch on a touchscreen laptop (inline mode; native pinch is disabled here) → magnify.
  var _pinchD = null;
  function tDist(t) { var a = t[0], b = t[1]; return Math.hypot(a.clientX - b.clientX, a.clientY - b.clientY); }
  ga.addEventListener('touchstart', function (e) {
    if (inlineMode && !useMobileLayout() && e.touches.length === 2) _pinchD = tDist(e.touches);
  }, { passive: false });
  ga.addEventListener('touchmove', function (e) {
    if (!inlineMode || useMobileLayout() || _pinchD == null || e.touches.length !== 2) return;
    e.preventDefault();
    var d = tDist(e.touches), midX = (e.touches[0].clientX + e.touches[1].clientX) / 2,
        midY = (e.touches[0].clientY + e.touches[1].clientY) / 2;
    if (_pinchD > 0) setUiZoom((uiZoom || 1) * Math.pow(d / _pinchD, 0.5), midX, midY);  // sqrt = half response
    _pinchD = d;
  }, { passive: false });
  ga.addEventListener('touchend', function (e) { if (e.touches.length < 2) _pinchD = null; });

  // Drag to pan: horizontal moves the whole view (when zoomed in), vertical scrolls the column the
  // drag started over. A small move threshold preserves taps (node open/close) and text selection.
  var _drag = null;
  function dragStart(x, y) {
    if (!inlineMode || useMobileLayout() || !cy) return;
    _drag = { x: x, y: y, col: inlineColumnAt(x), moved: false };
  }
  function dragMove(x, y) {
    if (!_drag || !cy) return false;
    var dx = x - _drag.x, dy = y - _drag.y;
    if (!_drag.moved && Math.abs(dx) + Math.abs(dy) < 4) return false;   // still a tap
    _drag.moved = true; _drag.x = x; _drag.y = y;
    var z = cy.zoom();
    if (_drag.col && inlineBase) inlineColScroll[_drag.col] = (inlineColScroll[_drag.col] || 0) - dy / z;
    if (uiZoom > 1) inlinePanX += dx;
    applyInlineDrag();
    return true;
  }
  function dragEnd() { _drag = null; }
  ga.addEventListener('mousedown', function (e) {
    if (e.button !== 0) return;
    // Don't start a pan on selectable/clickable content — let text select and links/buttons work.
    if (e.target && e.target.closest && e.target.closest('.inline-node-desc, a, button, input, textarea, .inline-copy-link')) return;
    dragStart(e.clientX, e.clientY);
  });
  window.addEventListener('mousemove', function (e) { if (dragMove(e.clientX, e.clientY)) e.preventDefault(); });
  window.addEventListener('mouseup', dragEnd);
  // Capture phase so a one-finger drag that starts ON a description still scrolls the column: the
  // description overlay calls stopPropagation on touchstart (to keep text selectable / links working),
  // which would otherwise swallow the drag before it reaches this bubble-phase listener.
  ga.addEventListener('touchstart', function (e) {
    if (inlineMode && !useMobileLayout() && e.touches.length === 1) dragStart(e.touches[0].clientX, e.touches[0].clientY);
  }, { passive: false, capture: true });
  ga.addEventListener('touchmove', function (e) {
    if (_pinchD != null || e.touches.length !== 1) return;
    if (dragMove(e.touches[0].clientX, e.touches[0].clientY)) e.preventDefault();
  }, { passive: false, capture: true });
  ga.addEventListener('touchend', dragEnd);
  // Edge hover/click → highlight the Theme/Skill node the edge originates from, exactly like hovering
  // that node. Edge paths carry data-hl (their Theme/Skill endpoint) and their own pointer-events, so
  // e.target identifies the edge even though the SVG overlay is rebuilt on every highlight change.
  ga.addEventListener('mousemove', function (e) {
    if (_drag && _drag.moved) return;                                  // mid-drag: ignore
    var hl = e.target && e.target.getAttribute && e.target.getAttribute('data-hl');
    if (hl) {
      if (hoveredNodeId !== hl) { hoveredNodeId = hl; _edgeHoverActive = true; applyHighlightState(); }
    } else if (_edgeHoverActive) {
      _edgeHoverActive = false; hoveredNodeId = null; applyHighlightState();
    }
  });
  ga.addEventListener('click', function (e) {
    var hl = e.target && e.target.getAttribute && e.target.getAttribute('data-hl');
    if (hl) { hoveredNodeId = hl; _edgeHoverActive = true; applyHighlightState(); }  // also serves a mobile tap
  });
  // Arm auto-fit-on-open only after the first user gesture, so the page still loads as the whole map
  // even when a node opens by default.
  ga.addEventListener('pointerdown', function () { autoFitArmed = true; }, true);
  _inlineWheelBound = true;
}

// Inline-mode floating toolbar: Open/Collapse-all rows. (The old "☰ Info" panel is gone — the
// site info now lives in the "About" nodes in the Theme column.)
function ensureInlineSidebarBtn() {
  if (document.getElementById('inline-allbtns')) return;

  function mkBtn(text, fn) {
    var b = document.createElement('button'); b.type = 'button'; b.className = 'inline-allbtn';
    b.textContent = text; b.onclick = fn; return b;
  }
  // Global controls: open / collapse every column at once (per-column controls now live in each
  // column's header). openAllInline()/collapseAllInline() with no group act on all columns.
  var wrap = document.createElement('div'); wrap.id = 'inline-allbtns';
  wrap.appendChild(mkBtn('Open all',     function () { openAllInline(); }));
  wrap.appendChild(mkBtn('Collapse all', function () { collapseAllInline(); }));

  // Zoom controls (−  100%  +  ⤢). Pinch / Ctrl+wheel also zoom; ⤢ resets to fit width.
  var zc = document.createElement('div'); zc.id = 'inline-zoom';
  function zbtn(txt, title, fn) {
    var b = document.createElement('button'); b.type = 'button'; b.className = 'inline-zoombtn';
    b.textContent = txt; b.title = title; b.onclick = fn; return b;
  }
  var pct = document.createElement('span'); pct.id = 'inline-zoom-pct'; pct.textContent = '100%';
  zc.appendChild(zbtn('−', 'Zoom out', function () { setUiZoom((uiZoom || 1) / 1.2); }));
  zc.appendChild(pct);
  zc.appendChild(zbtn('+', 'Zoom in', function () { setUiZoom((uiZoom || 1) * 1.2); }));
  zc.appendChild(zbtn('⤢', 'Reset zoom (fit width)', function () { uiZoom = 1; inlinePanX = 0; _zoomFocal = null; layoutInlineScroll(); }));

  // Header cluster spanning the bar: title/controls on the LEFT, zoom + Open/Collapse on the RIGHT.
  var hdr = document.getElementById('inline-header-right');
  if (!hdr) { hdr = document.createElement('div'); hdr.id = 'inline-header-right'; document.body.appendChild(hdr); }
  var sb = document.getElementById('info-sidebar');
  if (sb && sb.parentNode !== hdr) hdr.appendChild(sb);   // controls — left
  hdr.appendChild(zc);                                    // zoom — pushed right (margin-left:auto)
  hdr.appendChild(wrap);                                  // Open/Collapse toolbar — right
}

Shiny.addCustomMessageHandler('setInlineMode', function (msg) {
  var on = !!(msg && msg.value) && !useMobileLayout();  // inline UI is desktop-only
  inlineMode = on;
  document.body.classList.toggle('inline-mode', on);
  if (on) { ensureInlineSidebarBtn(); bindInlineWheel(); selectedNodeId = null; applyHighlightState(); }
  collapseNodeInline();
  if (cy) {
    if (on && !mobileMode) {
      // Static, width-fitted graph that the browser scrolls vertically
      cy.userZoomingEnabled(false); cy.userPanningEnabled(false);
      layoutInlineScroll();
    } else {
      // Restore normal pan/zoom + viewport-fitting
      cy.zoomingEnabled(true); cy.userZoomingEnabled(true); cy.userPanningEnabled(true);
      var el = document.getElementById('cy'); if (el) el.style.height = '';
      resizeCy(); fitWithHeaders(); if (!mobileMode) alignGraphLeft();
      drawEdgeOverlay(); if (lastData) positionHeaders(lastData);
    }
  }
});

Shiny.addCustomMessageHandler('setAutoFitOpen', function (msg) {
  autoFitOnOpen = !!(msg && msg.value);
});

// Inline project-width cap (0 = auto). Changing it re-runs the layout: reuse the full rebuild path
// (restore base geometry → autoFitProjectWidth applies the new cap → re-fit). During initial load cy
// isn't built yet, so this just sets the global and initCy picks it up.
Shiny.addCustomMessageHandler('setProjectMaxWidth', function (msg) {
  projectMaxWidth = (msg && msg.px != null) ? +msg.px : 0;
  if (cy && rawPayload) Shiny._handlers['updateCy'](rawPayload);
});

// Narrow-screen gap / node-width multipliers. Only relevant when narrow; changing them re-runs the
// full layout (applyNarrowScale rescales the base, then autoFit/fit follow). Rebuild only if narrow.
Shiny.addCustomMessageHandler('setNarrowGapMult', function (msg) {
  narrowGapMult = (msg && msg.mult != null && +msg.mult > 0) ? +msg.mult : 1;
  if (cy && rawPayload && isNarrow()) Shiny._handlers['updateCy'](rawPayload);
});
Shiny.addCustomMessageHandler('setNarrowNodeMult', function (msg) {
  narrowNodeMult = (msg && msg.mult != null && +msg.mult > 0) ? +msg.mult : 1;
  if (cy && rawPayload && isNarrow()) Shiny._handlers['updateCy'](rawPayload);
});

Shiny.addCustomMessageHandler('setArticlesEnabled', function (msg) {
  articlesEnabled = !!(msg && msg.value);
  // Re-render any currently open description so the article link appears/disappears.
  if (lastDescMsg) { try { Shiny._handlers['showDescPanel'](lastDescMsg); } catch (e) {} }
});

// Bulk-expand every node inline from a batch of description records, then reflow once.
Shiny.addCustomMessageHandler('expandAllInline', function (msg) {
  if (!inlineMode || !cy || !msg || !msg.nodes) return;
  msg.nodes.forEach(function (d) {
    if (d && d.nodeId != null && !inlineExpandedMap[String(d.nodeId)]) expandNodeFromDesc(d, true);
  });
  reflowInline();
});

Shiny.addCustomMessageHandler('setPtypeLayout', function (msg) {
  if (msg.ptypePct !== undefined) ptypePct = msg.ptypePct;
  if (msg.projectNodeWidth !== undefined) projectNodeWidth = msg.projectNodeWidth;
  if (cy) cy.emit('render');
  drawNodeConnector();
});

/* ── Static app entry point ─────────────────────────────────────────────── */

window.initStaticApp = function(payload) {
  if (payload.descriptions) window.staticNodeDescs = payload.descriptions;
  // Update GitHub link
  var gb = document.getElementById('github-btn');
  if (gb && payload.github_url && payload.github_url !== '#') gb.href = payload.github_url;
  // Apply layout params then graph data
  if (payload.ptypeLayout && Shiny._handlers['setPtypeLayout'])
    Shiny._handlers['setPtypeLayout'](payload.ptypeLayout);
  if (payload.edge_width) Shiny._handlers['setEdgeWidth']({ width: payload.edge_width });
  if (payload.edge_bands != null) Shiny._handlers['setEdgeBands']({ value: payload.edge_bands });
  if (payload.edge_gap != null) Shiny._handlers['setEdgeGap']({ gap: payload.edge_gap });
  if (payload.edge_sankey != null) Shiny._handlers['setEdgeSankey']({ value: payload.edge_sankey });
  if (payload.edge_transparency != null) Shiny._handlers['setEdgeTransparency']({ pct: payload.edge_transparency });
  if (payload.edge_min_width != null) Shiny._handlers['setEdgeMinWidth']({ width: payload.edge_min_width });
  if (payload.edge_min_on != null) Shiny._handlers['setEdgeMinOn']({ value: payload.edge_min_on });
  if (payload.edge_curve != null) Shiny._handlers['setEdgeCurve']({ exp: payload.edge_curve });
  if (payload.edge_pin_header != null) Shiny._handlers['setEdgePinHeader']({ value: payload.edge_pin_header });
  if (payload.fill_nodew != null) Shiny._handlers['setFillNodeW']({ pct: payload.fill_nodew });
  if (payload.fill_nodepad != null) Shiny._handlers['setFillNodePad']({ pct: payload.fill_nodepad });
  if (payload.fill_projw != null) Shiny._handlers['setFillProjW']({ pct: payload.fill_projw });
  if (payload.fill_colgap != null) Shiny._handlers['setFillColGap']({ pct: payload.fill_colgap });
  if (payload.gradient_extent != null) Shiny._handlers['setGradientExtent']({ pct: payload.gradient_extent });
  if (payload.gradient_transparency != null) Shiny._handlers['setGradientTransparency']({ pct: payload.gradient_transparency });
  if (payload.gradient_curve != null) Shiny._handlers['setGradientCurve']({ curve: payload.gradient_curve });
  if (payload.gradient_hover_mult != null) Shiny._handlers['setGradientHoverMult']({ mult: payload.gradient_hover_mult });
  if (payload.accordion_icon != null) Shiny._handlers['setAccordionIcon']({ style: payload.accordion_icon });
  if (payload.accordion_icon_size != null) Shiny._handlers['setAccordionIconSize']({ size: payload.accordion_icon_size });
  if (payload.node_outline != null) Shiny._handlers['setNodeOutline']({ width: payload.node_outline });
  if (payload.project_outline != null) Shiny._handlers['setProjectOutline']({ width: payload.project_outline });
  if (payload.outline_saturation != null) Shiny._handlers['setOutlineSaturation']({ value: payload.outline_saturation });
  if (payload.outline_transparency != null) Shiny._handlers['setOutlineTransparency']({ pct: payload.outline_transparency });
  if (payload.node_pad != null) Shiny._handlers['setNodePad']({ px: payload.node_pad });
  if (payload.project_max_width != null) Shiny._handlers['setProjectMaxWidth']({ px: payload.project_max_width });
  if (payload.narrow_gap_mult != null) Shiny._handlers['setNarrowGapMult']({ mult: payload.narrow_gap_mult });
  if (payload.narrow_node_mult != null) Shiny._handlers['setNarrowNodeMult']({ mult: payload.narrow_node_mult });
  if (payload.desc_pad != null) Shiny._handlers['setDescPad']({ px: payload.desc_pad });
  if (payload.inline_mode != null) Shiny._handlers['setInlineMode']({ value: payload.inline_mode });
  if (payload.auto_fit_open != null) Shiny._handlers['setAutoFitOpen']({ value: payload.auto_fit_open });
  if (payload.articles_enabled != null) Shiny._handlers['setArticlesEnabled']({ value: payload.articles_enabled });
  // Defer graph init to a new macrotask so any pending viewport-settle resize events
  // are processed first — ensures useMobileLayout() reads the correct dimensions.
  setTimeout(function() { Shiny._handlers['initCy'](payload); }, 0);
  // Accordion titles + language
  if (payload.sidebar) {
    var sb = payload.sidebar;
    if (Shiny._handlers['updateAccTitles'])
      Shiny._handlers['updateAccTitles']({ details_title: sb.details_title,
        intro_title: sb.intro_title, vote_title: sb.vote_title, fund_title: sb.fund_title });
    if (Shiny._handlers['setLanguageData'])
      Shiny._handlers['setLanguageData']({ page_title_en: sb.page_title_en,
        page_title_fi: sb.page_title_fi, details_title_fi: sb.details_title_fi,
        intro_title_fi: sb.intro_title_fi, vote_title_fi: sb.vote_title_fi,
        fund_title_fi: sb.fund_title_fi });
    var titleEnEl = document.getElementById('page-title-en');
    var enTitle = sb.page_title_en || (titleEnEl && titleEnEl.textContent) || 'My interests';
    document.title = enTitle;
  }
  populateStaticSidebar(payload);
  // Honour ?lang= query param
  var lang = new URLSearchParams(window.location.search).get('lang');
  if (lang === 'fi') setLanguage('fi');
};

function populateStaticSidebar(p) {
  var sb = p.sidebar || {};
  var txtStyle = 'color:rgba(255,255,255,0.8);font-family:Arial,Helvetica,sans-serif;line-height:1.65;';
  var descStyle = txtStyle + 'font-size:var(--desc-font);margin-bottom:12px;';
  // Hint text
  var hintEl = document.getElementById('sidebar-hint');
  if (hintEl) {
    var hEn = sb.details_hint || 'Click on an item to show description';
    var hFi = sb.details_hint_fi || hEn;
    hintEl.innerHTML = '<span class="en-only">' + hEn + '</span><span class="fi-only">' + hFi + '</span>';
  }
  // About / Intro
  var introEl = document.getElementById('acc-about-body');
  var aboutHtml = p.intro_html ?
    (p.intro_html.en ? '<div class="en-only" style="' + descStyle + '">' + p.intro_html.en + '</div>' : '') +
    (p.intro_html.fi ? '<div class="fi-only" style="' + descStyle + '">' + p.intro_html.fi + '</div>' : '') : '';
  if (introEl) introEl.innerHTML = aboutHtml;
  var mobAbout = document.getElementById('mob-content-about');
  if (mobAbout) mobAbout.innerHTML = aboutHtml;
  // Vote
  var voteEl = document.getElementById('acc-vote-body');
  var voteHtml = p.vote_html ?
    (p.vote_html.en ? '<div id="vote-section"><div class="en-only" style="' + txtStyle + '">' + linkifyHtml(p.vote_html.en) + '</div>' : '') +
    (p.vote_html.fi ? '<div class="fi-only" style="' + txtStyle + '">' + linkifyHtml(p.vote_html.fi) + '</div>' : '') + '</div>' : '';
  if (voteEl) voteEl.innerHTML = voteHtml;
  var mobVote = document.getElementById('mob-content-vote');
  if (mobVote) mobVote.innerHTML = voteHtml;
  // Funding
  var fundEl = document.getElementById('acc-fund-body');
  var fh = p.funding_html || {};
  var fundHtml = '<div class="funding-body">' +
    '<div class="en-only" style="margin-bottom:8px;line-height:1.6;">' + (fh.en_intro || '') + '</div>' +
    '<div class="fi-only" style="margin-bottom:8px;line-height:1.6;">' + (fh.fi_intro || fh.en_intro || '') + '</div>' +
    '<div style="line-height:1.7;">' + (fh.items || '') + '</div></div>';
  if (fundEl) fundEl.innerHTML = fundHtml;
  var mobFund = document.getElementById('mob-content-fund');
  if (mobFund) mobFund.innerHTML = fundHtml;
}

/* ── DOM Ready ───────────────────────────────────────────────────────────── */

document.addEventListener('DOMContentLoaded', function () {
  syncResizeHandle();
  var ga = document.getElementById('graph-area');
  if (ga) {
    var ro = new ResizeObserver(function () { resizeCy(); }); ro.observe(ga);
    // Intercept wheel in capture phase so Cytoscape never sees it.
    // Handle zoom manually with reduced sensitivity (~10% per scroll notch).
    ga.addEventListener('wheel', function (e) {
      e.preventDefault();
      e.stopPropagation();
      if (!cy) return;
      var sensitivity = e.ctrlKey ? 0.003 : 0.001; // pinch/trackpad 3× more aggressive
      var factor = 1 - e.deltaY * sensitivity;
      var newZoom = Math.max(cy.minZoom(), Math.min(cy.maxZoom(), cy.zoom() * factor));
      var rect = ga.getBoundingClientRect();
      cy.zoom({ level: newZoom, renderedPosition: { x: e.clientX - rect.left, y: e.clientY - rect.top } });
    }, { passive: false, capture: true });
  }
  var sideScroll = document.getElementById('sidebar-scroll');
  if (sideScroll) sideScroll.addEventListener('scroll', function() { drawNodeConnector(); });
  mobileMode = useMobileLayout();
  lastMobileState = mobileMode;  // init so first resize event can detect boundary crossing
  lastNarrowState = isNarrow();   // init so the first resize can detect a narrow-breakpoint crossing
  applyMobileLayout();
  resizeCy();
});

/* ── Viewport change: reinit if crossing mobile/desktop boundary ─────────── */

var lastMobileState = null;
var lastNarrowState = null;  // isNarrow() at the last resize — crossing it re-applies the narrow multipliers
var lastMobileW = 0;        // viewport width used in last applyMobileNodeSizes call
var postInitResizeHandler = null; // one-shot handler registered after each initCy
var resizeDebounce = null;
window.addEventListener('resize', function () {
  var nowMobile = useMobileLayout();
  var ga = document.getElementById('graph-area');
  var nowW = ga ? ga.clientWidth : window.innerWidth;
  // Unified UI: the narrow gap/node-width multipliers are baked into the layout at build time
  // (applyNarrowScale), keyed off isNarrow() — which is viewport-width based (the browser width, not
  // the device). Crossing that breakpoint must rebuild so the multipliers apply/unapply live.
  var nowNarrow = isNarrow();
  if (rawPayload && lastNarrowState !== null && nowNarrow !== lastNarrowState) {
    lastNarrowState = nowNarrow;
    clearTimeout(resizeDebounce);
    resizeDebounce = setTimeout(function () {
      var pk = pickData(rawPayload);
      if (cy) { cy.destroy(); cy = null; }
      initCyGraph(pk);
    }, 180);
    return;
  }
  lastNarrowState = nowNarrow;
  var mobileWidthChanged = nowMobile && lastMobileW > 0 && Math.abs(nowW - lastMobileW) > 5;
  if (rawPayload && lastMobileState !== null && nowMobile !== lastMobileState) {
    // Crossed mobile/desktop boundary — full reinit immediately
    lastMobileState = nowMobile;
    clearTimeout(resizeDebounce);
    var picked = pickData(rawPayload);
    if (cy) { cy.destroy(); cy = null; }
    initCyGraph(picked);
  } else if (mobileWidthChanged) {
    // Mobile viewport width changed — debounce to let DOM settle, then reinit
    lastMobileState = nowMobile;
    clearTimeout(resizeDebounce);
    resizeDebounce = setTimeout(function () {
      requestAnimationFrame(function () {
        var picked = pickData(rawPayload);
        if (cy) { cy.destroy(); cy = null; }
        initCyGraph(picked);
      });
    }, 300);
  } else {
    resizeCy();
    refreshLayout();
  }
  lastMobileState = nowMobile;
});

// Re-layout after orientation change — dimensions settle ~300ms after the event
window.addEventListener('orientationchange', function () {
  clearTimeout(resizeDebounce);
  resizeDebounce = setTimeout(function () {
    var nowMobile = useMobileLayout();
    var picked = pickData(rawPayload);
    lastMobileState = nowMobile;
    if (cy) { cy.destroy(); cy = null; }
    initCyGraph(picked);
  }, 350);
});

// Re-layout when page is restored from bfcache (back/forward navigation)
window.addEventListener('pageshow', function (e) {
  if (e.persisted && rawPayload) {
    setTimeout(function () {
      var nowMobile = useMobileLayout();
      var picked = pickData(rawPayload);
      lastMobileState = nowMobile;
      if (cy) { cy.destroy(); cy = null; }
      initCyGraph(picked);
    }, 150);
  }
});