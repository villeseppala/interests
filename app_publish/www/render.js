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
                       nodeId: value, group: desc.group });
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
var nodeBgSameAsGraph = false;
var nodeGradients = {};      // nodeId → {side:'left'|'right'|'both', color:rgbaStr}
var nodeHoverGradients = {}; // same, for hovered node's neighbours
var nodeBaseGradients = {};  // persistent edge-color gradients for Theme/Skill nodes
var projectNodeWidth = 444;
var ptypePct = 10;
var mobileData = null;
var selectedNodeId = null;
var hoveredNodeId = null;
var mobileMode = false;
var forceMobile = false;
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
var descFontSize = 11.5;
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
var currentLang = (new URLSearchParams(window.location.search)).get('lang') === 'fi' ? 'fi' : 'en';
var accTitleData = {};
var langData = {};
var lastDescMsg = null;
document.addEventListener('DOMContentLoaded', function() {
  if (currentLang === 'fi') document.body.classList.add('lang-fi');
});

function useMobileLayout() {
  if (forceMobile) return true;
  if (window.matchMedia) {
    if (window.matchMedia('(max-width:' + MOBILE_BREAKPOINT + 'px)').matches) return true;
    // Landscape phone: narrow height, not a tablet
    if (window.matchMedia('(max-height:500px) and (max-width:1100px)').matches) return true;
    return false;
  }
  return (document.documentElement.clientWidth || window.innerWidth) <= MOBILE_BREAKPOINT;
}

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
      function upd() {
        ov.innerHTML = '';
        var pan = inst.pan(), zoom = inst.zoom();
        inst.nodes().forEach(function (node) {
          var opt = opts[0]; if (!opt) return;
          var html = opt.tpl(node.data()); if (!html) return;
          var pos = node.position(), w = node.data('w') || 160, h = node.data('h') || 44;
          var d = document.createElement('div');
          var grp = node.data('group');
          var nc = grp === 'Theme' ? colTheme : grp === 'Project' ? colProject : colSkill;
          var nr = parseInt(nc.slice(1,3),16)||0, ng = parseInt(nc.slice(3,5),16)||0, nb = parseInt(nc.slice(5,7),16)||0;
          var bevel = '2px 4px 7px rgba(0,0,0,0.65), -2px -2px 3px rgba(' + nr + ',' + ng + ',' + nb + ',0.42)';
          d.style.cssText = 'position:absolute;box-sizing:border-box;pointer-events:none;overflow:hidden;';
          d.style.boxShadow = bevel;
          d.style.left = ((pos.x - w / 2) * zoom + pan.x) + 'px';
          d.style.top = ((pos.y - h / 2) * zoom + pan.y) + 'px';
          d.style.width = w + 'px'; d.style.height = h + 'px';
          d.style.transform = 'scale(' + zoom + ')'; d.style.transformOrigin = 'top left';
          d.innerHTML = html; ov.appendChild(d);
        });
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
/* resize handled by viewport change listener at bottom */

/* ── Cytoscape Styles — border 1.1px ─────────────────────────────────────── */

function buildStyle() {
  var borderColor = function (e) {
    var g = e.data('group');
    if (g === 'Theme') return colTheme; if (g === 'Project') return colProject;
    if (g === 'Funding') return '#78c4e8'; if (g === 'Vote') return '#e8c478';
    return colSkill;
  };
  return [
    { selector: 'node', style: {
        shape: 'rectangle', width: 'data(w)', height: 'data(h)',
        'background-fill': 'flat',
        'background-color': function() { return nodeBgSameAsGraph ? colBg : colNodeBg; },
        'border-width': 2,
        'border-color': borderColor, 'border-style': 'solid', label: '',
        'shadow-opacity': 0, 'outline-width': 0, 'outline-opacity': 0, 'underlay-opacity': 0, 'overlay-opacity': 0,
        cursor: function (e) {
          var g = e.data('group');
          return (g === 'Project' || g === 'Theme' || g === 'Skill') ? 'pointer' : 'default';
        },
    }},
    { selector: 'node.selected', style: {
        'background-color': function() { var b = nodeBgSameAsGraph ? colBg : colNodeBg; return blendWithWhite(b, 0.15); },
        'border-width': mobileMode ? 9 : 6, 'shadow-opacity': 0, 'outline-width': 0, 'outline-opacity': 0,
    }},
    { selector: 'node.hovered', style: {
        'border-width': mobileMode ? 9 : 6, 'outline-width': 0,
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

function hexRgba(hex, a) {
  var r = parseInt(hex.slice(1,3),16)||0, g = parseInt(hex.slice(3,5),16)||0, b = parseInt(hex.slice(5,7),16)||0;
  return 'rgba(' + r + ',' + g + ',' + b + ',' + a + ')';
}
function buildBaseGradients() {
  nodeBaseGradients = {};
  if (!cy) return;
  cy.nodes().forEach(function(n) {
    var grp = n.data('group');
    if (grp !== 'Theme' && grp !== 'Skill') return;
    var side = grp === 'Theme' ? 'right' : 'left';
    n.connectedEdges().forEach(function(edge) {
      if (nodeBaseGradients[n.id()]) return; // first edge color only (consistent with hover behavior)
      var raw = (lightMode ? edge.data('lightColor') : edge.data('color')) || (lightMode ? '#000000' : '#ffffff');
      var edgeCol = hexRgba(raw, 0.45);
      nodeBaseGradients[n.id()] = { side: side, color: edgeCol };
    });
  });
}

function gradientOverlay(id, pct) {
  var sel = nodeGradients[id], hov = nodeHoverGradients[id], base = nodeBaseGradients[id];
  if (!sel && !hov && !base) return '';
  var sty = 'position:absolute;top:0;height:100%;pointer-events:none;z-index:5;';
  var out = '';
  function addDiv(side, col, w) {
    var ws = w + '%';
    if (side === 'left'  || side === 'both')
      out += '<div style="' + sty + 'width:' + ws + ';left:0;background:linear-gradient(to right,' + col + ',transparent);"></div>';
    if (side === 'right' || side === 'both')
      out += '<div style="' + sty + 'width:' + ws + ';right:0;background:linear-gradient(to left,' + col + ',transparent);"></div>';
  }
  // Edge-color gradient: hover replaces base (keeps specific edge color); wider when hovered
  var edgeGrad = hov || base;
  if (edgeGrad) addDiv(edgeGrad.side, edgeGrad.color, hov ? Math.round((pct || 10) * 2) : (pct || 10));
  if (sel) addDiv(sel.side, sel.color, (pct || 10) * (sel.widthMult || 1));
  return out;
}

function nodeHtml(data) {
  var g = data.group;
  var label = dualLabel(data.label, data.label_fi);
  var fn = fontNode + 'px', fs = fontSubs + 'px';
  var wrap = 'word-wrap:break-word;overflow-wrap:break-word;';

  if (g === 'Theme') {
    return '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;' +
      'display:flex;align-items:center;justify-content:flex-end;padding:3px 7px;overflow:hidden;">' +
      '<span style="color:' + colTheme + ';font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'font-weight:bold;text-align:right;line-height:1.25;padding-right:14px;position:relative;z-index:6;' + wrap + '">' + label + '</span>' +
      gradientOverlay(data.id, 20) + '</div>';
  }
  if (g === 'Skill') {
    var html = '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;' +
      'display:flex;flex-direction:column;justify-content:center;padding:4px 7px;overflow:hidden;">' +
      '<div style="color:' + colSkill + ';font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'font-weight:bold;line-height:1.25;position:relative;z-index:6;' + wrap + '">' + label + '</div>';
    var subs = data.subs || '';
    if (subs) {
      var items = subs.split('||');
      for (var i = 0; i < items.length; i++)
        html += '<div style="color:' + colSkill + ';opacity:0.7;font-family:Arial,Helvetica,sans-serif;' +
          'font-size:' + fs + ';line-height:1.3;padding-left:10px;position:relative;z-index:6;' + wrap + '">' + esc(items[i]) + '</div>';
    }
    return html + gradientOverlay(data.id, 20) + '</div>';
  }
  if (g === 'Funding') {
    return '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;' +
      'display:flex;align-items:center;padding:3px 8px;overflow:hidden;">' +
      '<span style="color:#78c4e8;font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'line-height:1.25;' + wrap + '">' + label + '</span></div>';
  }
  if (g === 'Vote') {
    return '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;' +
      'display:flex;align-items:center;padding:3px 8px;overflow:hidden;">' +
      '<span style="color:#e8c478;font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'line-height:1.25;' + wrap + '">' + label + '</span></div>';
  }
  if (g === 'Project') {
    var ptypeRaw = data.ptype || '';
    var ptypeFi = { 'Text': 'Teksti', 'Text, long': 'Pitkä teksti', 'Text, short': 'Lyhyt teksti', 'Website': 'Nettisivu' };
    var ptypeLabel = dualLabel(ptypeRaw, ptypeFi[ptypeRaw] || ptypeRaw);
    var ptypeFontSize = (fontPtype + 2) + 'px';
    var typeCol = (!mobileMode && ptypeRaw)
      ? '<div style="width:' + Math.round((data.w || projectNodeWidth) * ptypePct / 100) + 'px;flex-shrink:0;border-left:1.1px solid ' + colProject + ';' +
        'display:flex;align-items:center;justify-content:center;padding:0 5px;' +
        'color:' + colProject + ';font-family:Arial,Helvetica,sans-serif;font-size:' + ptypeFontSize + ';' +
        'font-weight:bold;text-align:center;line-height:1.25;position:relative;z-index:6;">' + ptypeLabel + '</div>'
      : '';
    return '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;overflow:hidden;' +
      'display:flex;align-items:stretch;">' +
      '<div style="flex:1;display:flex;align-items:center;justify-content:center;padding:4px 9px;' +
      'text-align:center;overflow:hidden;position:relative;z-index:6;">' +
      '<div style="color:' + colProject + ';font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'font-weight:bold;line-height:1.3;' + wrap + '">' + label + '</div></div>' +
      typeCol + gradientOverlay(data.id) + '</div>';
  }
  return '';
}

/* ── Description Panel / Bottom Sheet ────────────────────────────────────── */

function hideDescPanel() {
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
      var selColor = lightMode ? 'rgba(0,0,0,0.28)' : 'rgba(255,255,255,0.28)';
      if (selGrp === 'Project') nodeGradients[String(selectedNodeId)] = { side: 'both', color: selColor };
      sn.connectedEdges().forEach(function(edge) {
        var otherId = edge.data('source') === String(selectedNodeId) ? edge.data('target') : edge.data('source');
        var on = cy.getElementById(otherId); if (!on || on.empty()) return;
        var side = gradSide(selGrp, on.data('group'));
        if (side) nodeGradients[otherId] = { side: side, color: selColor, widthMult: 2 };
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
        var og2 = on2.data('group');
        var side2 = gradSide(hovGrp, og2);
        if (!side2) return;
        var rawHov = (lightMode ? edge.data('lightColor') : edge.data('color')) || (lightMode ? '#000000' : '#ffffff');
        var edgeCol = hexRgba(rawHov, 0.45);
        var selfCol = hovGrp === 'Project' ? hexRgba(colProject, 0.45) : edgeCol;
        mergeHovGrad(nodeHoverGradients, otherId, side2, edgeCol);
        mergeHovGrad(nodeHoverGradients, String(hoveredNodeId), oppSide(side2), selfCol);
      });
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
    var grp = lastDescMsg.group || 'Project';
    var colors = { Theme: colTheme, Project: colProject, Skill: colSkill };
    var c = colors[grp] || colProject;
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

  var edgePaths = [];
  cy.edges().forEach(function (edge) {
    var d = edge.data();
    var src = cy.getElementById(d.source), tgt = cy.getElementById(d.target);
    if (!src || src.empty() || !tgt || tgt.empty()) return;
    var sp = src.position(), tp = tgt.position();
    var sw = src.data('w') || 160, tw = tgt.data('w') || 160;
    var srcEp = d.srcEp || '', tgtEp = d.tgtEp || '';
    var sx, sy, tx, ty;
    if (srcEp.indexOf('px') >= 0) {
      var sp2 = srcEp.split(/\s+/); sx = sp.x + sw / 2; sy = sp.y + parseFloat(sp2[1] || '0');
    } else { sx = sp.x + sw / 2; sy = sp.y; }
    if (tgtEp.indexOf('px') >= 0) {
      var tp2 = tgtEp.split(/\s+/); tx = tp.x - tw / 2; ty = tp.y + parseFloat(tp2[1] || '0');
    } else { tx = tp.x - tw / 2; ty = tp.y; }
    var x1 = sx * zoom + pan.x, y1 = sy * zoom + pan.y;
    var x2 = tx * zoom + pan.x, y2 = ty * zoom + pan.y;
    var cx1 = x1 + (x2 - x1) * 0.45, cx2 = x2 - (x2 - x1) * 0.45;
    edgePaths.push({
      pathD: 'M' + x1 + ',' + y1 + ' C' + cx1 + ',' + y1 + ' ' + cx2 + ',' + y2 + ' ' + x2 + ',' + y2,
      color: d.color || '#ffffff', lightColor: d.lightColor || lightEdgeColor, dashes: d.dashes,
      isSel: edge.hasClass('selected'), isHov: edge.hasClass('hovered')
    });
  });
  var strokeW = baseEdgeWidth * zoom * (mobileMode ? 1.5 : 1);
  // Pass 1: white glow behind SELECTED edges only (not hovered)
  edgePaths.forEach(function (ep) {
    if (!ep.isSel) return;
    var glow = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    glow.setAttribute('d', ep.pathD); glow.setAttribute('fill', 'none');
    glow.setAttribute('stroke', lightMode ? '#000000' : '#ffffff');
    glow.setAttribute('stroke-width', strokeW * 2.5);
    svg.appendChild(glow);
  });
  // Pass 2: all colored edges — hovered edges 3× width, others normal
  edgePaths.forEach(function (ep) {
    var path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    path.setAttribute('d', ep.pathD); path.setAttribute('fill', 'none');
    path.setAttribute('stroke', lightMode ? ep.lightColor : ep.color);
    path.setAttribute('stroke-width', (ep.isHov && !ep.isSel) ? strokeW * 3 : strokeW);
    path.setAttribute('opacity', '0.85');
    if (ep.dashes) path.setAttribute('stroke-dasharray', (5 * zoom) + ',' + (3 * zoom));
    svg.appendChild(path);
  });
}

/* ── Node-to-panel connector line ────────────────────────────────────────── */

function drawNodeConnector() {
  var old = document.getElementById('node-connector'); if (old) old.remove();
  if (mobileMode || !selectedNodeId || !cy) return;
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
      var gd = groupData[grpOrder[i]];
      if (gd.xs.length) h.x = gd.xs.reduce(function(a, b) { return a + b; }, 0) / gd.xs.length;
      if (gd.minY !== Infinity)
        h.y = gd.minY - (fontHdr1 + fontHdr2) * 1.25 - 6 / zoom;
    });
  }
  data.headers.forEach(function (h, i) {
    var sx = h.x * zoom + pan.x, sy = h.y * zoom + pan.y;
    var div = document.createElement('div');
    var hcolor = i === 0 ? colTheme : (i === 1 ? colProject : colSkill);
    div.className = 'col-hdr'; div.id = 'colhdr-' + i; div.style.color = hcolor;
    div.style.transform = 'scale(1)'; div.style.transformOrigin = 'top center';
    div.innerHTML = '<b style="font-size:' + fontHdr1 + 'px;white-space:nowrap">' + dualLabel(h.line1, h.line1_fi) +
      '</b><span style="font-size:' + fontHdr2 + 'px;white-space:nowrap">' + dualLabel(h.line2, h.line2_fi) + '</span>';
    div.style.visibility = 'hidden'; div.style.top = '0'; div.style.left = '0';
    area.appendChild(div);
    var natW = div.offsetWidth;
    div.style.width = natW + 'px';  // lock width before moving left, prevents text-wrap shift
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
  descFontSize = data.fontDesc || 11.5;
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
  fitWithHeaders();
  positionHeaders(lastData);
  drawEdgeOverlay();
  drawNodeConnector();
}

/* ── Auto-fit project node width/height to viewport aspect ratio ─────────── */
/* On wide viewports: widen nodes so titles fit on one row.                   */
/* On narrow viewports: keep base width and double node height so titles wrap  */
/* to two rows — this allows a larger zoom and visually larger font.          */

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
  // 18px exact padding (9px each side) + 18px buffer against canvas/browser discrepancy
  var requiredSingle = Math.ceil((maxTextW + 36) / (1 - typeF));
  if (requiredSingle <= baseW) return; // already fits at base width
  var delta = requiredSingle - baseW;

  // Infer gap between project nodes from layout
  var gapV = 18;
  if (projNodes.length >= 2)
    gapV = Math.max(4, Math.round(projNodes[1].position.y - projNodes[0].position.y -
                                  (projNodes[0].data.h + projNodes[1].data.h) / 2));

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

  if (twoRowNodeW >= requiredSingle) {
    // ── Single-row: balanced width exceeds the one-row requirement; just use single-row ──
    currentLayoutMode = 'single';
    (data.nodes || []).forEach(function(n) {
      if (!n.data || !n.position) return;
      if (n.data.group === 'Project') { n.data.w = requiredSingle; n.position.x += delta / 2; }
      else if (n.data.group === 'Skill') { n.position.x += delta; }
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
      else if (n.data.group === 'Skill') { n.position.x += dtwo; }
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
  // Project inner div: padding:4px 9px → 8px vertical, 18px horizontal (CSS px = cyto units)
  var containerW = Math.max(w - 18, 4);
  var label = String(nodeData.label || '');
  var labelFi = String(nodeData.label_fi || '');
  return measureNodeHeight(
    [{ text: labelFi.length > label.length ? labelFi : label, fontSize: fontNode, fontWeight: 'bold', lineHeight: 1.3 }],
    containerW, 8
  );
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
    else if (grp === 'Theme' || grp === 'Skill') { n.data.w = themeSkillW; }
  });

  // Group nodes by column (sort order preserved across re-stacks)
  var colNodes = { Theme: [], Project: [], Skill: [] };
  (data.nodes || []).forEach(function(n) {
    if (!n.data || !n.position) return;
    var grp = n.data.group;
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
    });
  }

  function restack(gp, gts) {
    var colTotals = {};
    ['Theme', 'Project', 'Skill'].forEach(function(grp) {
      var nodes = colNodes[grp];
      if (!nodes.length) { colTotals[grp] = 0; return; }
      var gap = (grp === 'Project') ? gp : gts;
      var curY = 0;
      nodes.forEach(function(n, i) {
        var h = n.data.h || 46;
        curY = (i === 0) ? h / 2 : curY + (nodes[i - 1].data.h || 46) / 2 + gap + h / 2;
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
  applyDataGlobals(data);
  applyColors();
  applyMobileLayout();
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
  cy.nodeHtmlLabel([{ query: 'node', tpl: function (d) { return nodeHtml(d); } }]);
  fitWithHeaders();
  if (!mobileMode) alignGraphLeft();
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
  cy.on('tap', function (evt) { if (evt.target === cy) hideDescPanel(); });
  cy.on('tap', 'node', function (evt) {
    var d = evt.target.data(), g = d.group;
    if (g === 'Project' || g === 'Theme' || g === 'Skill') {
      var id = parseFloat(d.id);
      if (selectedNodeId === id) { hideDescPanel(); return; }
      selectNode(id);
      if (window.Shiny) Shiny.setInputValue('clicked_node_id', id, { priority: 'event' });
    }
  });
  // Hover only on desktop
  if (!mobileMode) {
    cy.on('mouseover', 'node', function (evt) { hoveredNodeId = evt.target.data('id'); applyHighlightState(); });
    cy.on('mouseout', 'node', function () { hoveredNodeId = null; applyHighlightState(); });
  }
  cy.on('pan zoom', function () { positionHeaders(lastData); drawEdgeOverlay(); drawNodeConnector(); });
  // Dragging on a node pans the graph (nodes are non-grabbable)
  cy.on('vmousedown', 'node', function(evt) {
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
  if (cy) cy.destroy();
  initCyGraph(picked);
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
      if (cy) cy.destroy();
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
  lastData = picked; var prevSel = selectedNodeId;
  applyDataGlobals(picked);
  applyColors();
  applyMobileLayout();
  saveLayoutSnapshot(picked);
  autoFitProjectWidth(picked);
  applyMobileNodeSizes(picked);
  cy.elements().remove(); cy.add(buildElements(picked));
  cy.layout({ name: 'preset' }).run(); positionHeaders(picked);
  if (prevSel) selectNode(prevSel); else drawEdgeOverlay();
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
function processInline(text) {
  var re = /\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)|(https?:\/\/\S+)/g;
  var out = '', last = 0, m;
  while ((m = re.exec(text)) !== null) {
    out += esc(text.slice(last, m.index));
    var linkStyle = 'color:inherit;opacity:0.85;text-decoration:underline;cursor:pointer;';
    if (m[1]) {
      out += '<a href="' + esc(m[2]) + '" target="_blank" rel="noopener" style="' + linkStyle + '">' + esc(m[1]) + '</a>';
    } else {
      out += '<a href="' + esc(m[3]) + '" target="_blank" rel="noopener" style="' + linkStyle + '">' + esc(m[3]) + '</a>';
    }
    last = m.index + m[0].length;
  }
  return out + esc(text.slice(last));
}

/* Simple markdown to HTML for descriptions */
function mdToHtml(text) {
  var lines = String(text).split('\n');
  var out = [];
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    var m = line.match(/^(#{3,6})\s*(.+)$/);
    if (m) {
      var level = Math.min(m[1].length + 1, 6);
      out.push('<h' + level + ' style="margin:12px 0 4px;font-size:' + (descFontSize + 2) + 'px;font-weight:bold;">' + processInline(m[2]) + '</h' + level + '>');
    } else if (line.trim() === '') {
      out.push('<br>');
    } else {
      out.push(processInline(line));
    }
  }
  return out.join('<br>');
}

Shiny.addCustomMessageHandler('showDescPanel', function (msg) {
  lastDescMsg = msg;
  var grp = msg.group || 'Project';
  var colors = { Theme: colTheme, Project: colProject, Skill: colSkill };
  var c = colors[grp] || colProject;
  var dTitle = (currentLang === 'fi' && msg.title_fi) ? msg.title_fi : (msg.title || '');
  var dText  = (currentLang === 'fi' && msg.text_fi)  ? msg.text_fi  : (msg.text  || '');

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
    mdBody.innerHTML = mdToHtml(dText);
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
    body.innerHTML = mdToHtml(dText);
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

var colorPickerIds = ['col_bg','col_sidebar_bg','col_node_bg','col_theme','col_project','col_skill',
                      'light_col_bg','light_col_sidebar_bg','light_col_node_bg','light_col_theme','light_col_project','light_col_skill'];

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
    if (cy) cy.destroy();
    initCyGraph(picked);
  }
});

Shiny.addCustomMessageHandler('setNodeBgSameAsGraph', function (val) {
  nodeBgSameAsGraph = !!val;
  if (cy) cy.style(buildStyle());
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
  applyMobileLayout();
  resizeCy();
});

/* ── Viewport change: reinit if crossing mobile/desktop boundary ─────────── */

var lastMobileState = null;
var lastMobileW = 0;        // viewport width used in last applyMobileNodeSizes call
var postInitResizeHandler = null; // one-shot handler registered after each initCy
window.addEventListener('resize', function () {
  var nowMobile = useMobileLayout();
  if (rawPayload && lastMobileState !== null && nowMobile !== lastMobileState) {
    // Crossed mobile/desktop boundary — full reinit
    lastMobileState = nowMobile;
    var picked = pickData(rawPayload);
    if (cy) cy.destroy();
    initCyGraph(picked);
  } else {
    resizeCy();
    refreshLayout();
  }
  lastMobileState = nowMobile;
});