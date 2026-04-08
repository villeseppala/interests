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
        var desc = window.staticNodeDescs[String(Math.round(value))];
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
var colTheme = '#3be37a';
var colProject = '#ffad33';
var colSkill = '#78e6e7';
var lightMode = false;
var lightColBg = '#f0f4f8';
var lightColSidebarBg = '#e2eaf3';
var lightColTheme = '#1e7c45';
var lightColProject = '#c06000';
var lightColSkill = '#1a7a7b';
var darkColBg = '#0b3552';
var darkColSidebarBg = '#081626';
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

function useMobileLayout() { return forceMobile || window.innerWidth <= MOBILE_BREAKPOINT; }

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
  var dp = document.getElementById('desc-panel');
  if (ga) ga.style.background = colBg;
  if (cy_el) cy_el.style.background = colBg;
  if (sb) sb.style.background = colSidebarBg;
  if (dp) dp.style.background = colSidebarBg;
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
    var aspectH = w * (previewHeight / previewWidth);
    var data = lastData;
    if (data && data.max_h1) {
      var scale = w / ((data.headers && data.headers.length >= 3)
        ? (data.headers[2].x + 130) : 800);
      var contentH = (data.max_h1 + 140) * scale;
      el.style.height = Math.max(contentH, aspectH * 0.6) + 'px';
    } else {
      el.style.height = aspectH + 'px';
    }
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
  var W = container ? container.clientWidth  : window.innerWidth;
  var H = container ? container.clientHeight : window.innerHeight;
  var bb = cy.elements().boundingBox();
  if (!bb || bb.w === 0) { cy.fit(undefined, 20); return; }
  // Compute zoom so content fits with 20px side/bottom margins AND
  // 8px + hm*zoom header clearance at top (hm = headerMargin from R payload).
  // Vertical:   8 + hm*zoom + bb.h*zoom + 20 = H  →  zoom = (H−28)/(bb.h+hm)
  // Horizontal: 20 + bb.w*zoom + 20           = W  →  zoom = (W−40)/bb.w
  var hm = (lastData && lastData.headerMargin) || 70;
  var zoomH = (H - 28) / (bb.h + hm);
  var zoomW = (W - 40) / bb.w;
  var zoom  = Math.min(zoomH, zoomW);
  zoom = Math.max(cy.minZoom(), Math.min(cy.maxZoom(), zoom));
  cy.zoom(zoom);
  // Centre horizontally; top-align so headers have exactly 8px clearance
  cy.pan({
    x: W / 2 - (bb.x1 + bb.w / 2) * zoom,
    y: 8 + hm * zoom - bb.y1 * zoom
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
  var bg = colBg;
  var borderColor = function (e) {
    var g = e.data('group');
    if (g === 'Theme') return colTheme; if (g === 'Project') return colProject;
    if (g === 'Funding') return '#78c4e8'; if (g === 'Vote') return '#e8c478';
    return colSkill;
  };
  return [
    { selector: 'node', style: {
        shape: 'rectangle', width: 'data(w)', height: 'data(h)',
        'background-fill': 'linear-gradient',
        'background-gradient-direction': 'to-bottom',
        'background-gradient-stop-colors': function() { var b = nodeBgSameAsGraph ? colBg : colSidebarBg; return blendWithWhite(b, 0.08) + ' ' + blendWithBlack(b, 0.18); },
        'background-gradient-stop-positions': '0% 100%',
        'border-width': 1.2,
        'border-color': borderColor, 'border-style': 'solid', label: '',
        'shadow-blur': 8, 'shadow-color': '#000000',
        'shadow-offset-x': 2, 'shadow-offset-y': 3, 'shadow-opacity': 0.45,
        cursor: function (e) {
          var g = e.data('group');
          return (g === 'Project' || g === 'Theme' || g === 'Skill') ? 'pointer' : 'default';
        },
    }},
    { selector: 'node.selected', style: {
        'background-fill': 'flat',
        'background-color': function() { var b = nodeBgSameAsGraph ? colBg : colSidebarBg; return blendWithWhite(b, 0.28); },
        'shadow-opacity': 0,
        'outline-width': 3, 'outline-color': lightMode ? '#000000' : '#ffffff', 'outline-style': 'solid', 'outline-offset': 1, 'outline-opacity': 1,
    }},
    { selector: 'node.hovered', style: {
        'border-width': 5,
        'border-color': function(ele) { var g=ele.data('group'); return g==='Theme'?colTheme:g==='Project'?colProject:colSkill; },
        'border-opacity': 0.85,
    }},
    { selector: 'node.selected.hovered', style: {
        'outline-width': 3, 'outline-color': lightMode ? '#000000' : '#ffffff', 'outline-style': 'solid', 'outline-offset': 1, 'outline-opacity': 1,
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
  var fi_esc = esc(fi || '') || en_esc;
  if (!fi || fi === en) return en_esc;
  return '<span class="en-only">' + en_esc + '</span><span class="fi-only">' + fi_esc + '</span>';
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
      'font-weight:bold;text-align:right;line-height:1.25;padding-right:14px;' + wrap + '">' + label + '</span></div>';
  }
  if (g === 'Skill') {
    var html = '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;' +
      'display:flex;flex-direction:column;justify-content:center;padding:4px 7px;overflow:hidden;">' +
      '<div style="color:' + colSkill + ';font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'font-weight:bold;line-height:1.25;' + wrap + '">' + label + '</div>';
    var subs = data.subs || '';
    if (subs) {
      var items = subs.split('||');
      for (var i = 0; i < items.length; i++)
        html += '<div style="color:' + colSkill + ';opacity:0.7;font-family:Arial,Helvetica,sans-serif;' +
          'font-size:' + fs + ';line-height:1.3;padding-left:10px;' + wrap + '">' + esc(items[i]) + '</div>';
    }
    return html + '</div>';
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
    var ptypeLabel = (currentLang === 'fi' && ptypeFi[ptypeRaw]) ? ptypeFi[ptypeRaw] : ptypeRaw;
    var ptypeFontSize = (fontPtype + 2) + 'px';
    var typeCol = ptypeRaw
      ? '<div style="width:' + Math.round((data.w || projectNodeWidth) * ptypePct / 100) + 'px;flex-shrink:0;border-left:1.1px solid ' + colProject + ';' +
        'display:flex;align-items:center;justify-content:center;padding:0 5px;' +
        'color:' + colProject + ';font-family:Arial,Helvetica,sans-serif;font-size:' + ptypeFontSize + ';' +
        'font-weight:bold;text-align:center;line-height:1.25;">' + esc(ptypeLabel) + '</div>'
      : '';
    return '<div style="width:100%;height:100%;box-sizing:border-box;position:relative;overflow:hidden;' +
      'display:flex;align-items:stretch;">' +
      '<div style="flex:1;display:flex;align-items:center;justify-content:center;padding:4px 9px;' +
      'text-align:center;overflow:hidden;">' +
      '<div style="color:' + colProject + ';font-family:Arial,Helvetica,sans-serif;font-size:' + fn + ';' +
      'font-weight:bold;line-height:1.3;' + wrap + '">' + label + '</div></div>' +
      typeCol + '</div>';
  }
  return '';
}

/* ── Description Panel / Bottom Sheet ────────────────────────────────────── */

function hideDescPanel() {
  lastDescMsg = null;
  // Desktop: hide sidebar panel
  var p = document.getElementById('desc-panel'); if (p) p.style.display = 'none';
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
  updateInfoBtnVisibility();
}

function showBottomSheet() {
  var bs = document.getElementById('mobile-bottom-sheet');
  if (bs) bs.classList.add('visible');
  updateInfoBtnVisibility();
}

function updateInfoBtnVisibility() {
  var btn = document.getElementById('mobile-info-btn');
  if (!btn) return;
  // Hide info button when any sheet is showing (close button handles dismissal)
  if (sheetMode) { btn.style.opacity = '0'; btn.style.pointerEvents = 'none'; }
  else { btn.style.opacity = '1'; btn.style.pointerEvents = ''; }
}

function showInfoSheet() {
  // Grab voting + funding content from sidebar (exists in DOM, hidden on mobile via CSS)
  var voteEl = document.getElementById('vote-section');
  var fundEl = document.getElementById('funding-section');
  var bsTitle = document.getElementById('mobile-bs-title');
  var bsBody = document.getElementById('mobile-bs-body');
  var bsClose = document.getElementById('mobile-bs-close');
  var bs = document.getElementById('mobile-bottom-sheet');
  if (!bsTitle || !bsBody || !bs) return;

  var html = '';
  if (voteEl) html += voteEl.innerHTML;
  if (fundEl) html += '<hr style="margin:14px 0 10px;border-color:rgba(255,255,255,0.15);">' + fundEl.innerHTML;
  bsTitle.textContent = 'Info';
  bsTitle.style.color = '#78e6e7';
  bsBody.innerHTML = html || 'No info available.';
  bs.style.borderColor = '#78e6e7';
  bs.style.maxHeight = '50vh'; // reset from any drag
  if (bsClose) { bsClose.style.color = '#78e6e7'; bsClose.style.borderColor = 'rgba(120,230,231,0.4)'; }
  sheetMode = 'info';
  showBottomSheet();
  // Deselect any node
  selectedNodeId = null; applyHighlightState();
}

/* ── Draggable Bottom Sheet ──────────────────────────────────────────────── */

(function () {
  var grabbing = false, startY = 0, startH = 0, sheet = null;
  document.addEventListener('DOMContentLoaded', function () {
    var handle = document.getElementById('mobile-bs-grab');
    sheet = document.getElementById('mobile-bottom-sheet');
    if (!handle || !sheet) return;

    function onStart(y) {
      grabbing = true; startY = y; startH = sheet.offsetHeight;
      document.body.style.userSelect = 'none';
    }
    function onMove(y) {
      if (!grabbing) return;
      var delta = startY - y;
      var newH = Math.max(80, Math.min(window.innerHeight * 0.85, startH + delta));
      sheet.style.maxHeight = newH + 'px';
    }
    function onEnd() {
      if (!grabbing) return;
      grabbing = false;
      document.body.style.userSelect = '';
      // If dragged very small, close
      if (sheet.offsetHeight < 100) hideBottomSheet();
    }
    handle.addEventListener('mousedown', function (e) { e.preventDefault(); onStart(e.clientY); });
    document.addEventListener('mousemove', function (e) { if (grabbing) onMove(e.clientY); });
    document.addEventListener('mouseup', onEnd);
    handle.addEventListener('touchstart', function (e) { e.preventDefault(); onStart(e.touches[0].clientY); }, { passive: false });
    document.addEventListener('touchmove', function (e) { if (grabbing) { e.preventDefault(); onMove(e.touches[0].clientY); } }, { passive: false });
    document.addEventListener('touchend', onEnd);
  });

  // Info button handler
  document.addEventListener('DOMContentLoaded', function () {
    var btn = document.getElementById('mobile-info-btn');
    if (btn) btn.addEventListener('click', function () {
      if (sheetMode === 'info') { hideBottomSheet(); } else { showInfoSheet(); }
    });
  });
})();

/* ── Independent Highlight: selected + hovered coexist ───────────────────── */

function applyHighlightState() {
  if (!cy) return;
  cy.elements('.selected').removeClass('selected');
  cy.elements('.hovered').removeClass('hovered');
  if (selectedNodeId) {
    var sn = cy.getElementById(String(selectedNodeId));
    if (sn && !sn.empty()) { sn.addClass('selected'); sn.connectedEdges().addClass('selected'); }
  }
  if (hoveredNodeId) {
    var hn = cy.getElementById(String(hoveredNodeId));
    if (hn && !hn.empty()) { hn.addClass('hovered'); hn.connectedEdges().addClass('hovered'); }
  }
  drawEdgeOverlay();
  drawNodeConnector();
}

function selectNode(nodeId) { selectedNodeId = nodeId; applyHighlightState(); }
function clearSelection() { selectedNodeId = null; applyHighlightState(); }

function toggleLightMode() {
  lightMode = !lightMode;
  var btn = document.getElementById('mode-btn');
  if (lightMode) {
    colBg = lightColBg; colSidebarBg = lightColSidebarBg;
    colTheme = lightColTheme; colProject = lightColProject; colSkill = lightColSkill;
    if (btn) btn.textContent = '\u263d'; // crescent for "go dark"
  } else {
    colBg = darkColBg; colSidebarBg = darkColSidebarBg;
    colTheme = darkColTheme; colProject = darkColProject; colSkill = darkColSkill;
    if (btn) btn.textContent = '\u2600'; // sun for "go light"
  }
  applyColors();
  if (cy) { cy.style(buildStyle()); }
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
      color: d.color || '#ffffff', dashes: d.dashes,
      isSel: edge.hasClass('selected'), isHov: edge.hasClass('hovered')
    });
  });
  var strokeW = baseEdgeWidth * zoom;
  // Pass 1: white glow behind SELECTED edges only (not hovered)
  edgePaths.forEach(function (ep) {
    if (!ep.isSel) return;
    var glow = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    glow.setAttribute('d', ep.pathD); glow.setAttribute('fill', 'none');
    glow.setAttribute('stroke', lightMode ? '#000000' : '#ffffff');
    glow.setAttribute('stroke-width', strokeW * 2);
    svg.appendChild(glow);
  });
  // Pass 2: all colored edges — hovered edges 3× width, others normal
  edgePaths.forEach(function (ep) {
    var path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    path.setAttribute('d', ep.pathD); path.setAttribute('fill', 'none');
    path.setAttribute('stroke', ep.color);
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
  var px = pr.right, py = pr.top + 8;  // attach to top of panel
  // Project/Skill: start 1/3 down (above center, clear of edge attachment); Theme: center
  var ny = (grp === 'Project' || grp === 'Skill')
    ? (pos.y - nh / 6) * zoom + pan.y + ctr.top
    : pos.y * zoom + pan.y + ctr.top;

  var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.id = 'node-connector';
  svg.style.cssText = 'position:fixed;top:0;left:0;width:100vw;height:100vh;pointer-events:none;z-index:1;overflow:visible;';
  document.body.appendChild(svg);
  var sw = baseEdgeWidth * zoom * 2;
  var path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  var d, nx_start, dx_fb;

  if (grp === 'Theme') {
    // Simple bezier from left edge — no snake
    nx_start = (pos.x - nw / 2) * zoom + pan.x + ctr.left;
    dx_fb = px - nx_start;
    d = 'M' + nx_start.toFixed(1) + ',' + ny.toFixed(1) +
      ' C' + (nx_start + dx_fb * 0.4).toFixed(1) + ',' + ny.toFixed(1) +
      ' ' + (px - dx_fb * 0.4).toFixed(1) + ',' + py.toFixed(1) +
      ' ' + px.toFixed(1) + ',' + py.toFixed(1);
  } else {
    // Routing lane: just below col-hdr labels
    var hdrs = document.querySelectorAll('.col-hdr');
    var route_y = ctr.top + 20;
    for (var i = 0; i < hdrs.length; i++) {
      var hb = hdrs[i].getBoundingClientRect();
      if (hb.bottom > route_y) route_y = hb.bottom;
    }
    route_y += 14;
    // Skill: route above topmost project node (not bounded by panel top)
    // Project: route bounded by panel top so line doesn't overshoot
    if (grp === 'Skill') {
      var projMinTop = Infinity;
      cy.nodes().forEach(function (n) {
        if (n.data('group') !== 'Project') return;
        var nh2 = n.data('h') || 66;
        var top = (n.position().y - nh2 / 2) * zoom + pan.y + ctr.top;
        if (top < projMinTop) projMinTop = top;
      });
      if (projMinTop < Infinity && projMinTop > route_y + 8)
        route_y = projMinTop - 8;
    } else {
      route_y = Math.max(route_y, py); // Project: don't route higher than panel top
    }

    if (ny > route_y + 4) {
      var bend = Math.min(18, (ny - route_y) / 2);
      var t = Math.min((ny - route_y) * 0.3, 45);
      // Final segment: drop to py only if route_y is meaningfully above it
      var d_end = (py > route_y + bend)
        ? ' Q' + px.toFixed(1) + ',' + route_y.toFixed(1) +
          ' ' + px.toFixed(1) + ',' + (route_y + bend).toFixed(1) +
          ' L' + px.toFixed(1) + ',' + py.toFixed(1)
        : ' L' + px.toFixed(1) + ',' + py.toFixed(1);

      nx_start = (pos.x - nw / 2) * zoom + pan.x + ctr.left;
      var p3x = Math.max(nx_start - t - bend, px + bend * 2);
      d = 'M' + nx_start.toFixed(1) + ',' + ny.toFixed(1) +
        ' C' + (nx_start - t).toFixed(1) + ',' + (ny + t * 0.5).toFixed(1) +
        ' ' + (nx_start - t).toFixed(1) + ',' + route_y.toFixed(1) +
        ' ' + p3x.toFixed(1) + ',' + route_y.toFixed(1) +
        ' L' + (px + bend).toFixed(1) + ',' + route_y.toFixed(1) +
        d_end;
    } else {
      // Node near/above routing lane — fallback bezier
      nx_start = (pos.x - nw / 2) * zoom + pan.x + ctr.left;
      dx_fb = px - nx_start;
      d = 'M' + nx_start.toFixed(1) + ',' + ny.toFixed(1) +
        ' C' + (nx_start + dx_fb * 0.4).toFixed(1) + ',' + ny.toFixed(1) +
        ' ' + (px - dx_fb * 0.4).toFixed(1) + ',' + py.toFixed(1) +
        ' ' + px.toFixed(1) + ',' + py.toFixed(1);
    }
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
  data.headers.forEach(function (h, i) {
    var sx = h.x * zoom + pan.x, sy = h.y * zoom + pan.y;
    var div = document.createElement('div');
    var hcolor = i === 0 ? colTheme : (i === 1 ? colProject : colSkill);
    div.className = 'col-hdr'; div.id = 'colhdr-' + i; div.style.color = hcolor;
    div.style.transform = 'scale(1)'; div.style.transformOrigin = 'top center';
    div.innerHTML = '<b style="font-size:' + fontHdr1 + 'px">' + dualLabel(h.line1, h.line1_fi) +
      '</b><span style="font-size:' + fontHdr2 + 'px">' + dualLabel(h.line2, h.line2_fi) + '</span>';
    div.style.visibility = 'hidden'; div.style.top = '0'; div.style.left = '0';
    area.appendChild(div);
    var natW = div.offsetWidth;
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
    return payload.mobile;
  }
  mobileData = payload.mobile || null;
  return payload;
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
  darkColTheme      = data.colTheme      || darkColTheme;
  darkColProject    = data.colProject    || darkColProject;
  darkColSkill      = data.colSkill      || darkColSkill;
  if (data.lightColBg)         lightColBg         = data.lightColBg;
  if (data.lightColSidebarBg)  lightColSidebarBg  = data.lightColSidebarBg;
  if (data.lightColTheme)      lightColTheme       = data.lightColTheme;
  if (data.lightColProject)    lightColProject     = data.lightColProject;
  if (data.lightColSkill)      lightColSkill       = data.lightColSkill;
  // Apply active color set
  if (lightMode) {
    colBg = lightColBg; colSidebarBg = lightColSidebarBg;
    colTheme = lightColTheme; colProject = lightColProject; colSkill = lightColSkill;
  } else {
    colBg = darkColBg; colSidebarBg = darkColSidebarBg;
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
    nodes: (data.nodes).map(function(n) {
      return { id: n.data && n.data.id, w: n.data && n.data.w, h: n.data && n.data.h,
               x: n.position && n.position.x, y: n.position && n.position.y };
    }),
    headers: (data.headers || []).map(function(h) { return { x: h.x, y: h.y }; })
  };
}

function restoreLayoutSnapshot(data) {
  if (!layoutSnapshot || !data || !data.nodes) return;
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
  var prevMode = currentLayoutMode;
  restoreLayoutSnapshot(lastData);
  autoFitProjectWidth(lastData);
  if (currentLayoutMode === prevMode) {
    // Mode unchanged — just refit to new viewport size
    fitWithHeaders();
    positionHeaders(lastData);
    drawEdgeOverlay();
    drawNodeConnector();
    return;
  }
  // Mode changed — rebuild elements with new layout
  cy.elements().remove();
  cy.add(buildElements(lastData));
  cy.layout({ name: 'preset' }).run();
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
    // Move all column headers above the new (taller) project column top
    var newHeaderY = newProjTop - hm;
    (data.headers || []).forEach(function(h) { h.y = newHeaderY; });
  }
}

function initCyGraph(data) {
  lastData = data;
  applyDataGlobals(data);
  applyColors();
  applyMobileLayout();
  saveLayoutSnapshot(data);
  autoFitProjectWidth(data);
  initAccordions();
  resizeCy();
  cy = cytoscape({
    container: document.getElementById('cy'), elements: buildElements(data),
    layout: { name: 'preset' }, style: buildStyle(),
    userZoomingEnabled: true, userPanningEnabled: true,
    boxSelectionEnabled: false, autoungrabify: true,
  });
  cy.nodeHtmlLabel([{ query: 'node', tpl: function (d) { return nodeHtml(d); } }]);
  fitWithHeaders();
  if (!mobileMode) alignGraphLeft();
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
  positionHeaders(data); drawEdgeOverlay();
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
        ga.style.margin = '0 auto';
        ga.style.borderLeft = '2px solid rgba(255,255,255,0.15)';
        ga.style.borderRight = '2px solid rgba(255,255,255,0.15)';
      }
    } else {
      // Real mobile viewport
      body.classList.add('mobile-mode');
      body.classList.remove('mobile-preview');
      if (ga) { ga.style.maxWidth = ''; ga.style.margin = ''; ga.style.borderLeft = ''; ga.style.borderRight = ''; }
    }
  } else {
    body.classList.remove('mobile-mode');
    body.classList.remove('mobile-preview');
    if (ga) { ga.style.maxWidth = ''; ga.style.margin = ''; ga.style.borderLeft = ''; ga.style.borderRight = ''; }
  }
}

/* ── Shiny Message Handlers ──────────────────────────────────────────────── */

var rawPayload = null; // store full payload for resize switching

Shiny.addCustomMessageHandler('initCy', function (data) {
  rawPayload = data;
  var picked = pickData(data);
  if (cy) cy.destroy();
  initCyGraph(picked);
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
  applyAccTitles();
  applyDescPanelLang();
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

/* Inline markdown: [label](url) and bare https?:// URLs become clickable links */
function processInline(text) {
  var re = /\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)|(https?:\/\/\S+)/g;
  var out = '', last = 0, m;
  while ((m = re.exec(text)) !== null) {
    out += esc(text.slice(last, m.index));
    var linkStyle = 'color:inherit;opacity:0.85;text-decoration:underline;cursor:pointer;';
    var linkClick = 'onclick="var u=this.getAttribute(\'href\');window.open(u,\'_blank\');return false;"';
    if (m[1]) {
      out += '<a href="' + esc(m[2]) + '" ' + linkClick + ' rel="noopener" style="' + linkStyle + '">' + esc(m[1]) + '</a>';
    } else {
      out += '<a href="' + esc(m[3]) + '" ' + linkClick + ' rel="noopener" style="' + linkStyle + '">' + esc(m[3]) + '</a>';
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
      out.push('<h' + level + ' style="margin:12px 0 4px;font-size:' + (descFontSize + 2) + 'px;font-weight:bold;color:rgba(255,255,255,0.9);">' + processInline(m[2]) + '</h' + level + '>');
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

  if (mobileMode) {
    // Mobile: show bottom sheet in description mode
    var bs = document.getElementById('mobile-bottom-sheet');
    var bsTitle = document.getElementById('mobile-bs-title');
    var bsBody = document.getElementById('mobile-bs-body');
    var bsClose = document.getElementById('mobile-bs-close');
    if (!bs || !bsTitle || !bsBody) return;
    bsTitle.textContent = dTitle;
    bsBody.innerHTML = mdToHtml(dText);
    bs.style.borderColor = c; bsTitle.style.color = c;
    if (bsClose) { bsClose.style.color = c; bsClose.style.borderColor = c; }
    // Reset sheet height to default when switching content
    bs.style.maxHeight = '50vh';
    sheetMode = 'desc';
    showBottomSheet();
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

var colorPickerIds = ['col_bg','col_sidebar_bg','col_theme','col_project','col_skill',
                      'light_col_bg','light_col_sidebar_bg','light_col_theme','light_col_project','light_col_skill'];

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
  Shiny._handlers['initCy'](payload);
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
    document.title = sb.page_title_en || 'My interests';
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
  if (introEl && p.intro_html) {
    introEl.innerHTML =
      (p.intro_html.en ? '<div class="en-only" style="' + descStyle + '">' + p.intro_html.en + '</div>' : '') +
      (p.intro_html.fi ? '<div class="fi-only" style="' + descStyle + '">' + p.intro_html.fi + '</div>' : '');
  }
  // Vote
  var voteEl = document.getElementById('acc-vote-body');
  if (voteEl && p.vote_html) {
    voteEl.innerHTML =
      (p.vote_html.en ? '<div id="vote-section"><div class="en-only" style="' + txtStyle + '">' + p.vote_html.en + '</div>' : '') +
      (p.vote_html.fi ? '<div class="fi-only" style="' + txtStyle + '">' + p.vote_html.fi + '</div>' : '') +
      '</div>';
  }
  // Funding
  var fundEl = document.getElementById('acc-fund-body');
  if (fundEl && p.funding_html) {
    var fh = p.funding_html;
    fundEl.innerHTML = '<div class="funding-body">' +
      '<div class="en-only" style="margin-bottom:8px;line-height:1.6;">' + (fh.en_intro || '') + '</div>' +
      '<div class="fi-only" style="margin-bottom:8px;line-height:1.6;">' + (fh.fi_intro || fh.en_intro || '') + '</div>' +
      '<div style="line-height:1.7;">' + (fh.items || '') + '</div></div>';
  }
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
  mobileMode = useMobileLayout();
  applyMobileLayout();
  resizeCy();
});

/* ── Viewport change: reinit if crossing mobile/desktop boundary ─────────── */

var lastMobileState = null;
window.addEventListener('resize', function () {
  var nowMobile = useMobileLayout();
  if (lastMobileState !== null && nowMobile !== lastMobileState && rawPayload) {
    // Crossed breakpoint — reinitialize with correct data
    var picked = pickData(rawPayload);
    if (cy) cy.destroy();
    initCyGraph(picked);
  } else {
    resizeCy();
    refreshLayout();
  }
  lastMobileState = nowMobile;
});