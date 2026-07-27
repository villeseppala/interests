# app_author/app.R — Authoring UI for concept map
# Sources shared layout engine; JS in www/render.js

library(shiny)
library(here)

shiny::addResourcePath("app_www", here("app_author", "www"))
source(here("shared", "layout.R"))

# Article manifest for the nav dropdown (nav links resolve to the deployed site)
# ART_SCAN also carries the inline quick-read bodies (articles that opted in via `inline: true`).
ART_SCAN <- scan_articles(here("articles"))
jsonlite::write_json(ART_SCAN$manifest,
                     here("app_author", "www", "articles.json"), auto_unbox = TRUE, null = "null")

# ── Paths ────────────────────────────────────────────────────────────────────
GRAPH_PATH         <- here("app_author", "data", "graph.json")
QMD_PATH           <- here("docs", "descriptions.qmd")
PUBLISH_DESC_JSON  <- here("app_publish", "www", "descriptions.json")
PUBLISH_GRAPH_JSON <- here("app_publish", "www", "graph.json")

# ── Edge color palette ───────────────────────────────────────────────────────
EDGE_COLORS <- c(
  "Pink (existential)"   = "#d17bb7", "Green (harmony)"      = "#97d88b",
  "Yellow (EU)"          = "#f3d24f", "Light blue (climate)" = "#8fb7e8",
  "Orange (markets)"     = "#f08c6b", "Blue (dashboards)"    = "#5a83d6",
  "Green (viz)"          = "#7fbf63", "Orange (video)"       = "#f3a14f",
  "Red (homepage)"       = "#d84b4b", "Purple (AI)"          = "#c084fc",
  "Teal"    = "#2dd4bf", "Cyan"   = "#22d3ee", "Sky"     = "#38bdf8",
  "Indigo"  = "#818cf8", "Violet" = "#a78bfa", "Fuchsia" = "#e879f9",
  "Rose"    = "#fb7185", "Crimson" = "#ef4444", "Amber"   = "#f59e0b",
  "Lime"    = "#a3e635", "Emerald" = "#34d399", "Mint"    = "#6ee7b7",
  "Slate"   = "#94a3b8", "Steel"   = "#64748b", "Navy"    = "#1e3a8a",
  "Forest"  = "#166534", "Olive"   = "#3f6212", "Magenta" = "#db2777",
  "Lavender" = "#b4a7d6", "Sand"  = "#fcd34d", "Peach"   = "#fdba74",
  "Aqua"    = "#67e8f9", "Azure"  = "#60a5fa", "White"   = "#ffffff",
  "Black"   = "#000000"
)

# ── CSS ──────────────────────────────────────────────────────────────────────
# Author-specific styling (authoring panel, tabs, form controls) + any overrides. This is layered
# AFTER the canonical www/style.css (see APP_CSS below) so shared graph/sidebar styling stays in sync
# with the publish app and static site automatically.
AUTHOR_CSS <- "
  :root { --desc-font:11.5px; --desc-heading:13.5px; --desc-title-font:18px; }
  * { scrollbar-color:rgba(120,130,140,0.35) transparent; scrollbar-width:thin; }
  ::-webkit-scrollbar { width:5px; height:5px; }
  ::-webkit-scrollbar-track { background:transparent; }
  ::-webkit-scrollbar-thumb { background:rgba(120,130,140,0.35); border-radius:3px; }
  ::-webkit-scrollbar-thumb:hover { background:rgba(150,160,170,0.55); }
  html, body { height:100%; margin:0; background:#0b3552; overflow:hidden; }
  .container-fluid { padding:0 !important; margin:0 !important; }
  label, .help-block { color:rgba(255,255,255,0.85); font-size:11px; }
  h3  { color:rgba(255,255,255,0.95); margin-top:8px; font-size:14px; }
  .well { background:rgba(255,255,255,0.04); border:1px solid rgba(255,255,255,0.10); padding:8px; }
  .btn  { font-size:11px; }
  .form-control { font-size:11px; }
  #auth-panel textarea.form-control { font-size:14px; }
  select.form-control, input.form-control {
    background:#0d2035; color:#e0eaf4; border:1px solid rgba(255,255,255,0.15); }
  .nav-tabs > li > a { color:rgba(255,255,255,0.7); background:rgba(255,255,255,0.04);
    border-color:rgba(255,255,255,0.1); font-size:11px; padding:3px 7px; }
  .nav-tabs > li.active > a { color:#fff; background:rgba(255,255,255,0.12); }

  #main-row { display:flex; height:100vh; overflow:hidden; }

  #auth-panel {
    width:315px; min-width:315px; flex-shrink:0; height:100%; overflow-y:auto;
    background:rgba(5,20,35,0.95); border-right:1px solid rgba(255,255,255,0.08);
    padding:8px 7px; box-sizing:border-box; }

  /* spacer columns for breathing room */
  .col-spacer {
    width:calc(100vw / 60); flex-shrink:0; }

  #info-sidebar {
    width:20%; min-width:240px; max-width:480px; flex-shrink:0; height:100vh;
    display:flex; flex-direction:column;
    background:rgba(8,22,38,0.97);
    overflow:hidden; }
  #page-title {
    flex-shrink:0; background:#1e1b2e; color:rgba(255,255,255,0.9);
    font-family:Arial,Helvetica,sans-serif; font-weight:bold; font-size:11px;
    padding:10px 14px 8px; letter-spacing:0.01em;
    border-bottom:1px solid rgba(255,255,255,0.08);
    display:flex; flex-direction:column; gap:6px; }
  #page-title-en, #page-title-fi { font-size:20px !important; line-height:1.25; }
  #controls-row { display:flex; gap:6px; align-items:center; }
  #sidebar-scroll {
    flex:1; min-height:0; overflow-y:auto;
    padding:10px 10px; box-sizing:border-box; }
  #sidebar-hint {
    flex:1; min-height:0;
    color:rgba(150,150,150,0.35); font-family:Arial,Helvetica,sans-serif;
    font-size:3vh; font-weight:bold; line-height:1.25;
    display:flex; align-items:center;
    justify-content:center; text-align:center;
    padding:10px; box-sizing:border-box; margin-bottom:0; }
  #desc-panel {
    display:none; flex-direction:column;
    flex:1; min-height:0;
    border:1.5px solid var(--col-project, #ffad33); }
  #acc-desc > .acc-body {
    max-height:none !important; height:0; overflow:hidden !important;
    display:flex !important; flex-direction:column;
    transition:height 0.3s ease,opacity 0.22s ease,padding 0.22s ease; }
  #acc-desc.acc-open > .acc-body { height:13vh; padding:4px 5px 5px !important; }
  #acc-desc.desc-visible.acc-open > .acc-body { height:60vh; }
  #desc-header {
    display:flex; justify-content:space-between; align-items:flex-start;
    padding:9px 11px 5px 11px; flex-shrink:0;
    border-bottom:1px solid rgba(255,173,51,0.25); }
  #desc-title {
    color:var(--col-project, #ffad33); font-family:Arial,Helvetica,sans-serif;
    font-size:var(--desc-title-font); font-weight:bold; line-height:1.3; flex:1; padding-right:8px; }
  #desc-close {
    background:none; border:1px solid var(--col-project-dim, rgba(255,173,51,0.4));
    color:var(--col-project, #ffad33); font-size:12px; cursor:pointer;
    padding:1px 6px; flex-shrink:0; line-height:1.4; }
  #desc-close:hover { background:var(--col-project-hover, rgba(255,173,51,0.12)); }
  #desc-body {
    color:rgba(255,255,255,0.82); font-family:Arial,Helvetica,sans-serif;
    font-size:var(--desc-font); line-height:1.65; padding:9px 11px;
    flex:1; min-height:0; overflow-y:auto; }
  #vote-section { font-size:var(--desc-font); }
  #vote-section a { font-size:var(--desc-font) !important; }

  #sidebar-resize-handle {
    width:5px; flex-shrink:0; height:100vh; cursor:col-resize;
    background:rgba(180,190,200,0.3); transition:background 0.15s; }
  #sidebar-resize-handle:hover,
  #sidebar-resize-handle.dragging { background:rgba(180,190,200,0.55); }

  #graph-area { flex:1; min-width:0; overflow:hidden;
                background:#0b3552; position:relative; }
  #cy { display:block; width:100%; background:#0b3552; pointer-events:auto; }

  .col-hdr { position:absolute; pointer-events:none;
    font-family:Arial,Helvetica,sans-serif; line-height:1.2; z-index:10; text-align:center; }
  .col-hdr b    { display:block; }
  .col-hdr span { display:block; margin-top:2px; }

  .funding-body { color:rgba(255,255,255,0.8); font-family:Arial,Helvetica,sans-serif;
    font-size:var(--desc-font); line-height:1.6; }

  /* ── Language toggle ────────────────────────────────────────────────── */
  #mode-btn {
    background:none; border:1px solid rgba(255,255,255,0.22);
    color:rgba(255,255,255,0.6); font-size:14px;
    cursor:pointer; padding:1px 6px; border-radius:3px;
    font-family:Arial,Helvetica,sans-serif; transition:all 0.15s;
    line-height:1.4; }
  #mode-btn:hover { background:rgba(255,255,255,0.1); color:rgba(255,255,255,0.9); }
  .lang-btn {
    background:none; border:1px solid rgba(255,255,255,0.22);
    color:rgba(255,255,255,0.5); font-size:16px; font-weight:bold;
    cursor:pointer; padding:1px 5px; border-radius:3px;
    font-family:Arial,Helvetica,sans-serif; transition:all 0.15s;
    line-height:1.4; }
  .lang-btn.lang-active { color:rgba(255,255,255,0.9); border-color:rgba(255,255,255,0.6); background:rgba(255,255,255,0.1); }
  .lang-btn:not(.lang-active) { opacity:0.45; }
  #github-btn {
    display:flex; align-items:center; justify-content:center;
    color:rgba(255,255,255,0.45); border:1px solid rgba(255,255,255,0.22);
    border-radius:3px; padding:2px 5px; text-decoration:none;
    transition:all 0.15s; margin-left:auto; }
  #github-btn:hover { color:rgba(255,255,255,0.9); background:rgba(255,255,255,0.1); }
  #github-btn svg { fill:currentColor; }
  .fi-only { display:none !important; }
  body.lang-fi .en-only { display:none !important; }
  body.lang-fi .fi-only { display:block !important; }
  body.lang-fi span.fi-only, body.lang-fi b.fi-only, body.lang-fi a.fi-only { display:inline !important; }

  /* ── Accordions ──────────────────────────────────────────────────────── */
  .acc-section {
    margin-bottom:5px; border-radius:5px; overflow:hidden;
    border:1px solid rgba(255,255,255,0.09);
    background:rgba(0,0,0,0.18); }
  .acc-header {
    display:flex; justify-content:space-between; align-items:center;
    padding:7px 10px; cursor:pointer;
    user-select:none; -webkit-user-select:none;
    background:var(--acc-bg,#0b3552);
    border-left:2.5px solid rgba(255,255,255,0.18);
    transition:background 0.15s,border-color 0.15s; }
  .acc-header:hover {
    background:var(--acc-bg-hover,rgba(255,255,255,0.08));
    border-left-color:rgba(255,255,255,0.35); }
  .acc-section.acc-open > .acc-header {
    border-left-color:rgba(255,255,255,0.5);
    background:var(--acc-bg,#0b3552); }
  .acc-title {
    color:rgba(255,255,255,0.88); font-family:Arial,Helvetica,sans-serif;
    font-size:14.5px; font-weight:bold; letter-spacing:0.2px; flex:1; }
  .acc-arrow {
    color:rgba(255,255,255,0.45); font-size:13px;
    transition:transform 0.22s ease; transform:rotate(-90deg);
    margin-left:6px; display:inline-block; line-height:1; }
  .acc-section.acc-open > .acc-header .acc-arrow {
    transform:rotate(0deg); color:rgba(255,255,255,0.7); }
  .acc-body {
    overflow:hidden; max-height:0; padding:0 10px; opacity:0;
    transition:max-height 0.3s ease,padding 0.22s ease,opacity 0.22s ease; }
  .acc-section.acc-open > .acc-body {
    padding:8px 10px 10px; opacity:1; }

  /* ── Mobile bottom sheet ─────────────────────────────────────────────── */
  #mobile-bottom-sheet {
    display:none; position:fixed; bottom:0; left:0; right:0;
    max-height:50vh; background:rgba(8,22,38,0.97);
    border-top:2px solid var(--col-project, #ffad33);
    flex-direction:column; z-index:100;
    transform:translateY(100%); transition:transform 0.25s ease-out; }
  #mobile-bottom-sheet.visible { transform:translateY(0); }
  #mobile-bs-grab {
    flex-shrink:0; height:20px; cursor:ns-resize;
    display:flex; align-items:center; justify-content:center; }
  #mobile-bs-grab::after {
    content:''; width:36px; height:4px; border-radius:2px;
    background:rgba(255,255,255,0.3); }
  #mobile-bs-header {
    display:flex; justify-content:space-between; align-items:flex-start;
    padding:0 14px 6px; flex-shrink:0;
    border-bottom:1px solid var(--col-project-dim2, rgba(255,173,51,0.25)); }
  #mobile-bs-title {
    color:var(--col-project, #ffad33); font-family:Arial,Helvetica,sans-serif;
    font-size:15px; font-weight:bold; line-height:1.3; flex:1; padding-right:10px; }
  #mobile-bs-close {
    background:none; border:1px solid var(--col-project-dim, rgba(255,173,51,0.4));
    color:var(--col-project, #ffad33); font-size:16px; cursor:pointer;
    padding:2px 8px; flex-shrink:0; line-height:1.4;
    min-width:32px; min-height:32px; text-align:center; }
  #mobile-bs-body {
    flex:1; min-height:0; overflow-y:auto;
    color:rgba(255,255,255,0.82); font-family:Arial,Helvetica,sans-serif;
    font-size:14px; line-height:1.65; padding:10px 14px 16px; }
  #mobile-info-btn {
    display:none; position:fixed; bottom:12px; right:12px; z-index:99;
    background:rgba(8,22,38,0.9); border:1.5px solid #78e6e7;
    color:#78e6e7; font-family:Arial,Helvetica,sans-serif;
    font-size:13px; font-weight:bold; padding:8px 16px;
    border-radius:20px; cursor:pointer;
    min-width:44px; min-height:44px; }

  @media (max-width: 1499px) and (min-width: 769px) {
    .col-spacer { display:none; }
  }

  /* ── Mobile overrides ────────────────────────────────────────────────── */
  @media (max-width: 768px) {
    html, body { overflow:hidden; height:100%; }
    #main-row { flex-direction:column; height:100vh; overflow:hidden; }
    .col-spacer { display:none; }
    #auth-panel { display:none; }
    #info-sidebar { display:none; }
    #sidebar-resize-handle { display:none; }
    #graph-area { width:100%; flex:none; height:55dvh; overflow:hidden; position:relative; }
    #cy { pointer-events:auto; }
    #mobile-bottom-sheet { display:none !important; }
    #mobile-info-btn { display:none !important; }
    #mob-handle { display:flex; }
    #mob-panel { display:flex; }
  }
  #mob-handle {
    display:none; flex:none; height:16px; width:100%;
    cursor:ns-resize; align-items:center; justify-content:center;
    background:rgba(0,0,0,0.25); z-index:10; }
  #mob-handle::after {
    content:''; width:40px; height:4px; border-radius:2px;
    background:rgba(255,255,255,0.3); }
  #mob-panel {
    display:none; flex:1; min-height:0; flex-direction:column;
    background:rgba(8,22,38,0.97); overflow:hidden; position:relative; }
  #mob-tab-bar {
    flex:none; display:flex;
    border-bottom:1px solid rgba(255,255,255,0.1); }
  .mob-tab {
    flex:1; background:none; border:none;
    border-bottom:2px solid transparent;
    color:rgba(255,255,255,0.5);
    font-family:Arial,Helvetica,sans-serif; font-size:13px; font-weight:bold;
    padding:9px 4px 8px; cursor:pointer;
    transition:color 0.15s,border-color 0.15s; }
  .mob-tab.mob-tab-active {
    color:rgba(255,255,255,0.9);
    border-bottom-color:var(--col-skill, #78e6e7); }
  #mob-tab-content { flex:1; min-height:0; overflow-y:auto; }
  .mob-tab-pane {
    display:none; padding:10px 14px 16px;
    color:rgba(255,255,255,0.82); font-family:Arial,Helvetica,sans-serif;
    font-size:14px; line-height:1.65; }
  .mob-tab-pane.mob-tab-pane-active { display:block; }
  #mob-desc-panel {
    display:none; flex-direction:column;
    position:absolute; inset:0;
    background:rgba(8,22,38,0.97); z-index:5; overflow:hidden; }
  #mob-desc-panel.mob-desc-visible { display:flex; }
  #mob-desc-header {
    flex:none; display:flex; justify-content:space-between; align-items:flex-start;
    padding:9px 14px 6px;
    border-bottom:1px solid var(--col-project-dim2, rgba(255,173,51,0.25)); }
  #mob-desc-title {
    color:var(--col-project, #ffad33); font-family:Arial,Helvetica,sans-serif;
    font-size:17px; font-weight:bold; line-height:1.3; flex:1; padding-right:10px; }
  #mob-desc-close {
    background:none; border:1px solid var(--col-project-dim, rgba(255,173,51,0.4));
    color:var(--col-project, #ffad33); font-size:16px; cursor:pointer;
    padding:2px 8px; flex:none; line-height:1.4; min-width:32px; min-height:32px; text-align:center; }
  #mob-desc-body {
    flex:1; min-height:0; overflow-y:auto;
    color:rgba(255,255,255,0.82); font-family:Arial,Helvetica,sans-serif;
    font-size:14px; line-height:1.65; padding:10px 14px 16px; }

  /* ── Mobile preview (author only) ── */
  .mobile-preview #mobile-bottom-sheet { display:none !important; }
  .mobile-preview #mobile-info-btn { display:none !important; }
  .mobile-preview #info-sidebar { display:none !important; }
  .mobile-preview #sidebar-resize-handle { display:none !important; }
  .mobile-preview #main-row { flex-direction:column; }
  .mobile-preview #graph-area { flex:none; }
  .mobile-preview #mob-handle { display:flex; }
  .mobile-preview #mob-panel { display:flex; }

  /* ── Light mode overrides ─────────────────────────────────────────────── */
  body.light-mode #page-title { color:rgba(0,0,0,0.82); }
  body.light-mode .acc-title  { color:rgba(0,0,0,0.82) !important; }
  body.light-mode .acc-arrow  { color:rgba(0,0,0,0.4)  !important; }
  body.light-mode .acc-section { border-color:rgba(0,0,0,0.10); background:rgba(0,0,0,0.04); }
  body.light-mode .acc-header { border-left-color:rgba(0,0,0,0.18); }
  body.light-mode .acc-section.acc-open > .acc-header { border-left-color:rgba(0,0,0,0.4); }
  body.light-mode #sidebar-hint { color:rgba(0,0,0,0.18) !important; }
  body.light-mode #desc-body  { color:rgba(0,0,0,0.82) !important; }
  body.light-mode .funding-body { color:rgba(0,0,0,0.75) !important; }
  body.light-mode #vote-section,
  body.light-mode #vote-section div,
  body.light-mode #vote-section a { color:rgba(0,0,0,0.8) !important; }
  body.light-mode #acc-about .acc-body,
  body.light-mode #acc-about .acc-body div { color:rgba(0,0,0,0.8) !important; }
  body.light-mode .lang-btn { border-color:rgba(0,0,0,0.2) !important; color:rgba(0,0,0,0.5) !important; }
  body.light-mode .lang-btn.lang-active { color:rgba(0,0,0,0.85) !important; border-color:rgba(0,0,0,0.55) !important; background:rgba(0,0,0,0.07) !important; }
  body.light-mode #mode-btn { border-color:rgba(0,0,0,0.2) !important; color:rgba(0,0,0,0.55) !important; }
  body.light-mode #mode-btn:hover { background:rgba(0,0,0,0.07) !important; }
  body.light-mode #github-btn { border-color:rgba(0,0,0,0.2) !important; color:rgba(0,0,0,0.4) !important; }
  body.light-mode #github-btn:hover { background:rgba(0,0,0,0.07) !important; color:rgba(0,0,0,0.7) !important; }
  body.light-mode #sidebar-resize-handle { background:rgba(0,0,0,0.15); }
  body.light-mode #mobile-bottom-sheet { background:rgba(220,232,245,0.97); }
  body.light-mode #mobile-bs-body { color:rgba(0,0,0,0.82) !important; }

  /* ── Export mode (headless screenshot via webshot2) ──────────────────── */
  .export-mode #auth-panel,
  .export-mode #sidebar-resize-handle,
  .export-mode .col-spacer { display: none !important; }
  .export-mode #main-row { height: 100vh !important; overflow: hidden !important; }

  /* ── Inline UI mode (experimental) ──────────────────────────────────── */
  #inline-sidebar-btn { display:none; }
  body.inline-mode #inline-sidebar-btn {
    display:block; position:fixed; top:10px; left:10px; z-index:1000;
    background:rgba(8,22,38,0.92); color:rgba(255,255,255,0.92);
    border:1px solid rgba(255,255,255,0.25); border-radius:6px;
    font-family:Arial,Helvetica,sans-serif; font-size:13px; font-weight:bold;
    padding:6px 12px; cursor:pointer; }
  body.inline-mode #inline-sidebar-btn:hover { background:rgba(20,40,60,0.96); }
  body.inline-mode #info-sidebar {
    position:fixed !important; top:8px !important; right:8px !important; left:auto !important;
    width:auto !important; min-width:0 !important; max-width:min(340px,44vw) !important;
    height:auto !important; max-height:none !important;
    z-index:1000; background:transparent; overflow:visible;
    border-radius:8px; box-shadow:0 4px 16px rgba(0,0,0,0.4); }
  body.inline-mode #info-sidebar #page-title {
    border-radius:8px; border-bottom:none; padding:8px 12px 8px; background:rgba(8,22,38,0.92); }
  body.inline-mode #info-sidebar #page-title-text { font-size:15px; }
  body.inline-mode #inline-header-right {
    position:fixed; top:0; left:10px; right:10px; height:46px; z-index:1001;
    display:flex; align-items:center; gap:16px; }
  body.inline-mode #inline-header-right #inline-zoom { margin-left:auto; }
  body:has(#graph-area) #site-nav .nav-brand,
  body:has(#graph-area) #site-nav .nav-dropdown { display:none !important; }
  body.inline-mode #inline-header-right #inline-allbtns {
    position:static; transform:none; top:auto; left:auto; right:auto; height:auto; gap:2px; }
  body.inline-mode #inline-header-right #info-sidebar {
    position:static !important; top:auto !important; right:auto !important; left:auto !important;
    height:auto !important; max-width:none !important; box-shadow:none !important; border-radius:0 !important; }
  body.inline-mode #inline-header-right #info-sidebar #page-title {
    flex-direction:row; align-items:center; gap:10px;
    padding:0; background:transparent; border-radius:0; white-space:nowrap; }
  body.inline-mode #inline-header-right #info-sidebar #page-title-text { font-size:13px; white-space:nowrap; }
  body.inline-mode #inline-header-right .inline-allbtn { font-size:10.5px; padding:2px 6px; }
  body.inline-mode #inline-header-right .inline-btnlbl { font-size:10px; min-width:42px; }
  body.inline-mode #sidebar-scroll { display:none !important; }
  body.inline-mode #sidebar-resize-handle { display:none; }
  body.inline-mode #graph-area { width:100%; flex:1 1 100%; overflow:hidden; }
  .inline-node-desc a { pointer-events:auto !important; cursor:pointer; }
  .article-link { display:inline-block; margin-top:10px; padding:5px 12px; border-radius:6px;
    font-family:Arial,Helvetica,sans-serif; font-size:13px; font-weight:600; text-decoration:none;
    color:#0b3552; background:#78e6e7; pointer-events:auto !important; cursor:pointer; }
  .article-link:hover { background:#8ff0f1; }
  .inline-article-link { position:relative; z-index:7; }
  body.light-mode .article-link { background:#1a7a7b; color:#eafcff; }
  body:has(#site-nav) #main-row { height:calc(100vh - 46px); }
  #site-nav { display:flex; align-items:center; gap:22px; height:46px; padding:0 20px;
    background:#081626; border-bottom:1px solid rgba(255,255,255,0.12);
    font-family:Arial,Helvetica,sans-serif; position:relative; z-index:1000; box-sizing:border-box; }
  #site-nav a { color:#e8eef4; text-decoration:none; }
  #site-nav .nav-brand { font-weight:700; font-size:16px; letter-spacing:0.02em; }
  #site-nav .nav-dropdown { position:relative; }
  #site-nav .nav-dropdown > a { font-size:14px; opacity:0.92; }
  #site-nav .nav-caret { font-size:10px; margin-left:4px; opacity:0.7; }
  #site-nav .nav-menu { position:absolute; top:100%; left:0; min-width:260px; background:#081626;
    border:1px solid rgba(255,255,255,0.14); border-radius:6px; box-shadow:0 8px 24px rgba(0,0,0,0.35);
    padding:6px 0; display:none; max-height:70vh; overflow-y:auto; }
  #site-nav .nav-dropdown:hover .nav-menu, #site-nav .nav-menu:hover { display:block; }
  #site-nav .nav-menu a { display:block; padding:7px 16px; font-size:13.5px; white-space:nowrap; color:#e8eef4; }
  #site-nav .nav-menu a:hover { background:rgba(255,255,255,0.08); }
  #site-nav .nav-menu .nav-menu-empty { padding:7px 16px; font-size:13px; color:rgba(232,238,244,0.7); font-style:italic; }
  body.light-mode #site-nav { background:#e2eaf3; border-bottom-color:rgba(0,0,0,0.12); }
  body.light-mode #site-nav a { color:#0b2740; }
  body.light-mode #site-nav .nav-menu { background:#eef2f8; border-color:rgba(0,0,0,0.14); }
  body.light-mode #site-nav .nav-menu a:hover { background:rgba(0,0,0,0.06); }
"

# Canonical shared stylesheet (same file the publish app + static site use), with the author-specific
# rules layered on top so they win where they intentionally differ. This keeps future style.css
# changes flowing into the author app without hand-mirroring.
APP_CSS <- paste(
  paste(readLines(here("app_author", "www", "style.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
  AUTHOR_CSS,
  sep = "\n"
)

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=5"),
    tags$style(HTML(APP_CSS)),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/flag-icons/7.2.3/css/flag-icons.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.28.1/cytoscape.min.js"),
    tags$script(src = paste0("app_www/render.js?v=", as.integer(file.mtime(here("app_author", "www", "render.js"))))),
    tags$script(HTML("
      // Bind <input type='color'> elements to Shiny inputs
      // Do NOT send the initial HTML-hardcoded value — setColorInputs will initialise them correctly.
      // Only event listeners here so user interactions still propagate.
      $(document).on('shiny:connected', function() {
        function bindColor(id) {
          var el = document.getElementById(id);
          if (!el) { setTimeout(function(){ bindColor(id); }, 200); return; }
          el.addEventListener('input',  function() { Shiny.setInputValue(id, el.value, {priority:'event'}); });
          el.addEventListener('change', function() { Shiny.setInputValue(id, el.value, {priority:'event'}); });
        }
        ['col_bg','col_sidebar_bg','col_node_bg','col_theme','col_project','col_skill','col_all','source_color_picker',
         'light_col_bg','light_col_sidebar_bg','light_col_node_bg','light_col_theme','light_col_project','light_col_skill','light_col_all','light_edge_color'].forEach(bindColor);
      });

    ")),
    tags$script(HTML("window.SITE_NAV_BASE='https://villeseppala.github.io/interests/'; window.SITE_NAV_MANIFEST='app_www/articles.json';")),
    tags$script(src = paste0("app_www/site-nav.js?v=", as.integer(file.mtime(here("app_author", "www", "site-nav.js")))))
  ),

  # ── Shared site nav bar (links point to the deployed site; populated from app_www/articles.json) ──
  div(id = "site-nav"),

  div(id = "main-row",

      # ── Authoring panel (left) ──
      div(id = "auth-panel",
          h3("Authoring"),
          div(style = "display:flex;gap:4px;flex-wrap:wrap;",
              actionButton("reset_full", "Reset",     class = "btn btn-warning btn-xs"),
              actionButton("save_graph", "Save JSON", class = "btn btn-primary btn-xs"),
              actionButton("stop_app",    "Stop",       class = "btn btn-danger btn-xs"),
              actionButton("restart_app", "Restart",    class = "btn btn-warning btn-xs")
          ),
          tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
          div(style = "display:flex;gap:4px;flex-wrap:wrap;",
              actionButton("sync_to_publish", "\u2192 Publish",   class = "btn btn-default btn-xs"),
              actionButton("write_desc_json", "\u2192 Desc JSON", class = "btn btn-default btn-xs")
          ),
          div(style = "display:flex;gap:4px;flex-wrap:wrap;margin-top:3px;",
              actionButton("load_qmd",  "Load QMD",  class = "btn btn-default btn-xs"),
              actionButton("write_qmd", "Write QMD", class = "btn btn-default btn-xs")
          ),
          tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
          div(style = "display:flex;gap:4px;",
              actionButton("save_svg", "Save SVG", class = "btn btn-default btn-xs", style = "flex:1;"),
              actionButton("save_png", "Save PNG", class = "btn btn-default btn-xs", style = "flex:1;"),
              downloadButton("export_png", "Export PNG", class = "btn btn-default btn-xs", style = "flex:1;padding:1px 4px;")
          ),
          tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
          
          tabsetPanel(id = "auth-tabs", selected = "Node",
                      tabPanel("Node",
                               uiOutput("node_select_ui"),
                               numericInput("edit_id",    "ID",    value = 101, min = 0.01, step = 0.01, width = "100%"),
                               selectInput("edit_group",  "Group", choices = ALL_GROUPS, width = "100%"),
                               textInput("edit_title",    "Title", "", width = "100%"),
                               textInput("edit_title_fi", "Title (FI)", "", width = "100%"),
                               checkboxInput("edit_hidden", "Hidden (exclude node + its edges from graph)", FALSE, width = "100%"),
                               conditionalPanel("input.edit_group == 'Project'",
                                                selectInput("edit_ptype", "Type", choices = c("Text", "Text, long", "Text, short", "Website", "Video"), width = "100%"),
                                                numericInput("edit_pnum", "Num",  value = 201, min = 1, step = 1, width = "100%")
                               ),
                               conditionalPanel("input.edit_group == 'Skill'",
                                                tags$label("Sub-items (one per line)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                                                textAreaInput("edit_subs", NULL, value = "", rows = 12, width = "100%")
                               ),
                               conditionalPanel(
                                 "input.edit_group == 'Theme' || input.edit_group == 'Project' || input.edit_group == 'Skill'",
                                 tags$hr(style = "margin:3px 0;"),
                                 tags$label("Description", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                                 textAreaInput("edit_desc", NULL, value = "", rows = 15, width = "100%"),
                                 tags$label("Description (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                                 textAreaInput("edit_desc_fi", NULL, value = "", rows = 15, width = "100%")
                               ),
                               actionButton("apply_node",  "Apply",  class = "btn btn-primary btn-sm", style = "width:100%;margin-top:3px;"),
                               tags$hr(style = "margin:3px 0;"),
                               actionButton("remove_node", "Remove", class = "btn btn-danger btn-sm",  style = "width:100%;")
                      ),
                      
                      tabPanel("Edges",
                               h3("Add edge"),
                               uiOutput("edge_from_ui"),
                               uiOutput("edge_to_ui"),
                               selectInput("edge_color", "Color", choices = EDGE_COLORS, width = "100%"),
                               textInput("edge_color_custom", "Custom hex", value = "", width = "100%"),
                               tags$span("Custom hex overrides palette if non-empty",
                                         style = "color:rgba(255,255,255,0.4);font-size:13px;"),
                               checkboxInput("edge_dashes", "Dashed", FALSE),
                               actionButton("add_edge", "Add edge", class = "btn btn-primary btn-sm", style = "width:100%;"),
                               
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               h3("Remove edge"),
                               uiOutput("edge_remove_ui"),
                               actionButton("remove_edge", "Remove", class = "btn btn-danger btn-sm", style = "width:100%;"),
                      ),

                      tabPanel("Source Colors",
                               tags$span("Assign a color to all edges from a Theme or to a Skill node.",
                                         style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:4px;"),
                               uiOutput("source_color_node_ui"),
                               selectInput("source_color_palette", "Color", choices = EDGE_COLORS, width = "100%"),
                               tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                        tags$label("Color picker", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                        tags$input(id = "source_color_picker", type = "color", value = "#3be37a",
                                                   style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                               ),
                               textInput("source_color_custom", "Custom hex (overrides above)", value = "", width = "100%"),
                               actionButton("apply_source_color", "Apply color", class = "btn btn-primary btn-sm", style = "width:100%;"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$details(open = NA,
                                 tags$summary("Randomize all edge colors", style = "color:rgba(255,255,255,0.85);font-size:11px;cursor:pointer;padding:2px 0;"),
                                 tags$div(style = "margin-top:6px;",
                                   tags$span("Theme sources", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   actionButton("randomize_edges_theme", "Randomize theme", class = "btn btn-default btn-sm", style = "width:100%;margin-bottom:3px;"),
                                   checkboxInput("rb_theme", "Rainbow (even hue, constant S/V)", value = FALSE),
                                   checkboxInput("rb_random_theme", "Random start hue", value = FALSE),
                                   conditionalPanel("!input.rb_random_theme",
                                     sliderInput("rb_start_theme", "Start hue (°)", 0, 360, 0, step=5, ticks=FALSE, width="100%")),
                                   tags$div(style = "color:rgba(255,255,255,0.6);font-size:10px;line-height:1.35;margin:0 0 4px;", textOutput("ec_hsv_txt_theme", inline = TRUE)),
                                   sliderInput("ec_h_theme", "H",  0, 360, c(0, 360), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_ec_h_theme", "inv H", value = FALSE),
                                   sliderInput("ec_s_theme", "S%", 0, 100, c(55,  90), step=1, ticks=FALSE, width="100%"),
                                   sliderInput("ec_v_theme", "V%", 0, 100, c(70, 100), step=1, ticks=FALSE, width="100%"),
                                   sliderInput("ec_hue_dist_theme", "Min H dist", 0, 120, 30, step=5, ticks=FALSE, width="100%"),
                                   tags$span("Skill sources", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;margin-top:4px;"),
                                   actionButton("randomize_edges_skill", "Randomize skill", class = "btn btn-default btn-sm", style = "width:100%;margin-bottom:3px;"),
                                   checkboxInput("rb_skill", "Rainbow (even hue, constant S/V)", value = FALSE),
                                   checkboxInput("rb_random_skill", "Random start hue", value = FALSE),
                                   conditionalPanel("!input.rb_random_skill",
                                     sliderInput("rb_start_skill", "Start hue (°)", 0, 360, 0, step=5, ticks=FALSE, width="100%")),
                                   tags$div(style = "color:rgba(255,255,255,0.6);font-size:10px;line-height:1.35;margin:0 0 4px;", textOutput("ec_hsv_txt_skill", inline = TRUE)),
                                   sliderInput("ec_h_skill", "H",  0, 360, c(0, 360), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_ec_h_skill", "inv H", value = FALSE),
                                   sliderInput("ec_s_skill", "S%", 0, 100, c(55,  90), step=1, ticks=FALSE, width="100%"),
                                   sliderInput("ec_v_skill", "V%", 0, 100, c(70, 100), step=1, ticks=FALSE, width="100%"),
                                   sliderInput("ec_hue_dist_skill", "Min H dist", 0, 120, 30, step=5, ticks=FALSE, width="100%"),
                                   tags$hr(style = "margin:4px 0;border-color:rgba(255,255,255,0.1);"),
                                   sliderInput("ec_max_s_dist","Max S distance (edge colors)", 0, 100, 30, step=5, ticks=FALSE, width="100%"),
                                   sliderInput("ec_max_v_dist","Max V distance (edge colors)", 0, 100, 20, step=5, ticks=FALSE, width="100%"),
                                   sliderInput("ec_node_dist", "Min H dist from node accents", 0, 120, 20, step=5, ticks=FALSE, width="100%"),
                                   actionButton("save_ec_hsv", "Save HSV settings", class = "btn btn-default btn-xs", style = "width:100%;margin-top:3px;")
                                 )
                               )
                      ),

                      tabPanel("Add",
                               numericInput("new_id",    "New ID", value = 999, min = 0.01, step = 0.01, width = "100%"),
                               selectInput("new_group",  "Group",  choices = ALL_GROUPS, width = "100%"),
                               textInput("new_title",    "Title",  "New node", width = "100%"),
                               conditionalPanel("input.new_group == 'Project'",
                                                selectInput("new_ptype", "Type", choices = c("Text", "Text, long", "Text, short", "Website", "Video"), width = "100%"),
                                                numericInput("new_pnum", "Num",  value = 999, min = 1, step = 1, width = "100%")
                               ),
                               actionButton("add_node", "Add node", class = "btn btn-primary btn-sm", style = "width:100%;margin-top:3px;")
                      ),

                      tabPanel("Layout",
                               sliderInput("gap_v",      "Vertical gap (px)",      min = 4,   max = 80,  value = 18,   step = 2,   width = "100%"),
                               sliderInput("gap_col",    "Column spacing (px)",    min = 200, max = 700, value = 400,  step = 10,  width = "100%"),
                               sliderInput("edge_width", "Edge width (px)",        min = 0.5, max = 8,   value = 2.5,  step = 0.5, width = "100%"),
                               sliderInput("edge_transparency", "Edge transparency (%)", min = 0, max = 100, value = 18, step = 1, width = "100%"),
                               checkboxInput("edge_bands", "Edge bands (ribbons that fill node height)", TRUE),
                               checkboxInput("edge_sankey", "Sankey ribbons (no mid-span pinch)", FALSE),
                               checkboxInput("edge_min_on", "Limit edge width at thinnest point", TRUE),
                               sliderInput("edge_min_width", "Edge width at thinnest point (px)", min = 0, max = 48, value = 2.5, step = 0.5, width = "100%"),
                               sliderInput("edge_curve", "Edge pinch curve", min = 0.2, max = 4, value = 1, step = 0.1, width = "100%"),
                               checkboxInput("edge_pin_header", "Edges fill header height when open (uncheck to fill full border)", FALSE),
                               sliderInput("edge_gap", "Edge band gap (px)", min = 0, max = 20, value = 3, step = 1, width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$div(style = "font-size:11px;opacity:0.7;margin-bottom:2px;", "Extra browser width (inline layout) -> remainder = left/right padding"),
                               sliderInput("fill_nodew", "Extra width -> theme/skill width (%)", min = 0, max = 100, value = 0, step = 1, width = "100%"),
                               sliderInput("fill_projw", "Extra width -> project width (%)", min = 0, max = 100, value = 0, step = 1, width = "100%"),
                               sliderInput("fill_colgap", "Extra width -> column spacing (%)", min = 0, max = 100, value = 0, step = 1, width = "100%"),
                               sliderInput("gradient_extent", "Gradient extent (%)", min = 0, max = 50, value = 20, step = 1, width = "100%"),
                               sliderInput("gradient_transparency", "Gradient transparency (%)", min = 0, max = 100, value = 40, step = 1, width = "100%"),
                               sliderInput("gradient_curve", "Gradient falloff curve", min = 0.3, max = 4, value = 1, step = 0.1, width = "100%"),
                               sliderInput("gradient_hover_mult", "Gradient hover extent (×)", min = 0, max = 5, value = 2, step = 0.1, width = "100%"),
                               sliderInput("node_outline", "Node outline — themes/skills (px)", min = 0, max = 10, value = 3, step = 0.5, width = "100%"),
                               sliderInput("project_outline", "Project outline (px)", min = 0, max = 10, value = 3, step = 0.5, width = "100%"),
                               sliderInput("outline_saturation", "Outline color saturation (×)", min = 0, max = 2, value = 1, step = 0.05, width = "100%"),
                               sliderInput("outline_transparency", "Outline transparency (%)", min = 0, max = 100, value = 0, step = 1, width = "100%"),
                               sliderInput("node_pad", "Node text padding (px)", min = 0, max = 60, value = 0, step = 1, width = "100%"),
                               sliderInput("desc_pad", "Description text padding (px)", min = 0, max = 40, value = 10, step = 1, width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               sliderInput("font_hdr1",  "Header title (px)",      min = 12,  max = 36,  value = 22,   step = 1,   width = "100%"),
                               sliderInput("font_hdr2",  "Header subtitle (px)",   min = 8,   max = 24,  value = 15,   step = 1,   width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               sliderInput("font_node",  "Node text (px)",         min = 8,   max = 24,  value = 12,   step = 1,   width = "100%"),
                               sliderInput("font_ptype", "Project type (px)",      min = 8,   max = 24,  value = 12,   step = 1,   width = "100%"),
                               sliderInput("font_subs",  "Sub-skills (px)",        min = 8,   max = 24,  value = 15,   step = 1,   width = "100%"),
                               sliderInput("font_desc",  "Description text (px)",  min = 8,   max = 26,  value = 18, step = 0.5, width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               sliderInput("h_theme",    "Theme height (px)",      min = 30,  max = 100, value = 46,   step = 2,   width = "100%"),
                               sliderInput("h_project",  "Project height (px)",    min = 40,  max = 120, value = 66,   step = 2,   width = "100%"),
                               sliderInput("h_skill",    "Skill height (px)",      min = 30,  max = 100, value = 46,   step = 2,   width = "100%"),
                               sliderInput("w_project",  "Project node width (px)", min = 200, max = 700, value = 444,  step = 4,   width = "100%"),
                               sliderInput("w_node",     "Theme/skill node width (px)", min = 120, max = 700, value = 444, step = 4, width = "100%"),
                               sliderInput("ptype_pct",  "Type col width (%)",     min = 5,   max = 40,  value = 10,   step = 1,   width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               checkboxInput("show_watermark", "Show watermark", FALSE),
                               conditionalPanel("input.show_watermark",
                                                textInput("watermark_text",   "Text", value = "", width = "100%"),
                                                sliderInput("watermark_size", "Watermark size (px)", min = 6, max = 24, value = 10, step = 1, width = "100%")
                               ),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               h3("Mobile multipliers"),
                               sliderInput("mob_font_mult",    "Font size multiplier",       min = 0.5, max = 3.0, value = 1.5, step = 0.1, width = "100%"),
                               sliderInput("mob_h_theme_mult", "Theme height multiplier",    min = 1.0, max = 5.0, value = 3.0, step = 0.5, width = "100%"),
                               sliderInput("mob_h_proj_mult",  "Project height multiplier",  min = 1.0, max = 5.0, value = 3.0, step = 0.5, width = "100%"),
                               sliderInput("mob_h_skill_mult", "Skill height multiplier",    min = 1.0, max = 5.0, value = 3.0, step = 0.5, width = "100%"),
                               sliderInput("mob_gap_v_mult",   "Vertical gap multiplier",    min = 0.5, max = 3.0, value = 1.0, step = 0.1, width = "100%"),
                               sliderInput("mob_gap_col_mult", "Column spacing multiplier",  min = 0.5, max = 3.0, value = 1.0, step = 0.1, width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               # Inline UI is now the only desktop mode; keep the input (fixed TRUE) but hide it.
                               tags$div(style = "display:none;", checkboxInput("inline_mode", NULL, TRUE)),
                               checkboxInput("edit_inline_text", "Edit node text on nodes (click title / description)", TRUE),
                               checkboxInput("articles_enabled", "Show full-text article links", FALSE),
                               selectInput("accordion_icon", "Openable-node symbol",
                                 choices = c("Triangle ▸ ▾" = "triangle",
                                             "Filled triangle ▶ ▼" = "filled",
                                             "Plus / minus + −" = "plusminus",
                                             "Circled ⊕ ⊖" = "circled",
                                             "Chevron › ⌄" = "chevron",
                                             "None" = "none"),
                                 selected = "triangle", width = "100%"),
                               sliderInput("accordion_icon_size", "Openable-node symbol size (px)", min = 4, max = 48, value = 14, step = 1, width = "100%"),
                               checkboxInput("preview_mobile", "Preview mobile layout", FALSE),
                               conditionalPanel("input.preview_mobile",
                                                selectInput("preview_device", "Device", choices = c(
                                                  "iPhone SE (375x667)" = "375x667",
                                                  "iPhone 14 (390x844)" = "390x844",
                                                  "iPhone 14 Pro Max (430x932)" = "430x932",
                                                  "Samsung Galaxy S21 (360x800)" = "360x800",
                                                  "iPad Mini (768x1024)" = "768x1024",
                                                  "Custom" = "custom"
                                                ), selected = "390x844", width = "100%"),
                                                conditionalPanel("input.preview_device == 'custom'",
                                                                 div(style = "display:flex;gap:4px;",
                                                                     numericInput("preview_w", "W", value = 390, min = 240, max = 1024, step = 1, width = "50%"),
                                                                     numericInput("preview_h", "H", value = 844, min = 400, max = 1400, step = 1, width = "50%")
                                                                 )
                                                )
                               )
                      ),

                      tabPanel("Color",
                               checkboxInput("node_bg_same_as_sidebar", "Node bg = sidebar bg", value = FALSE),
                               tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                        tags$label("Graph bg", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                        tags$input(id = "col_bg", type = "color", value = "#0b3552", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                               ),
                               tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                        tags$label("Sidebar bg", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                        tags$input(id = "col_sidebar_bg", type = "color", value = "#081626", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                               ),
                               tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                        tags$label("Node bg", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                        tags$input(id = "col_node_bg", type = "color", value = "#081626", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                               ),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               conditionalPanel("!input.one_color",
                                 tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                          tags$label("Theme", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                          tags$input(id = "col_theme", type = "color", value = "#3be37a", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                                 ),
                                 tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                          tags$label("Project", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                          tags$input(id = "col_project", type = "color", value = "#ffad33", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                                 ),
                                 tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                          tags$label("Skill", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                          tags$input(id = "col_skill", type = "color", value = "#78e6e7", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                                 )
                               ),
                               conditionalPanel("input.one_color",
                                 tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                          tags$label("All node colors", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                          tags$input(id = "col_all", type = "color", value = "#3be37a", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                                 )
                               ),
                               checkboxInput("one_color", "One color", value = FALSE),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               actionButton("save_colors", "Save colors as default", class = "btn btn-default btn-xs", style = "width:100%;margin-bottom:5px;"),
                               tags$label("Palette presets", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               selectInput("palette_choice", NULL, choices = c(), width = "100%"),
                               actionButton("save_palettes", "Save palettes to file", class = "btn btn-default btn-xs", style = "width:100%;margin-bottom:5px;"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$details(open = NA,
                                 tags$summary("Randomizer", style = "color:rgba(255,255,255,0.85);font-size:11px;cursor:pointer;padding:2px 0;"),
                                 tags$div(style = "margin-top:6px;",
                                   tags$div(style = "display:flex;gap:6px;flex-wrap:wrap;margin-bottom:4px;",
                                     checkboxInput("lock_bg",       "Lock bg",       value = FALSE),
                                     checkboxInput("lock_sidebar",  "Lock sidebar",  value = FALSE),
                                     checkboxInput("lock_node_bg",  "Lock node bg",  value = FALSE),
                                     checkboxInput("lock_theme",    "Lock theme",    value = FALSE),
                                     checkboxInput("lock_project",  "Lock project",  value = FALSE),
                                     checkboxInput("lock_skill",    "Lock skill",    value = FALSE)
                                   ),
                                   actionButton("randomize", "Randomize", class = "btn btn-default btn-sm", style = "width:100%;margin-bottom:3px;"),
                                   tags$div(style = "color:rgba(255,255,255,0.6);font-size:10px;line-height:1.35;margin:0 0 5px;", textOutput("rand_hsv_txt", inline = TRUE)),
                                   actionButton("save_to_presets", "Save to presets", class = "btn btn-default btn-xs", style = "width:100%;margin-bottom:6px;"),
                                   tags$span("Background", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("rnd_h_bg", "H",  0, 360, c(0, 360),  step=5,  ticks=FALSE, width="100%"),
                                   checkboxInput("inv_rnd_h_bg", "inv H", value = FALSE),
                                   sliderInput("rnd_s_bg", "S%", 0, 100, c(5,  25),  step=1,  ticks=FALSE, width="100%"),
                                   sliderInput("rnd_v_bg", "V%", 0, 100, c(5,  22),  step=1,  ticks=FALSE, width="100%"),
                                   tags$span("Sidebar", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("rnd_h_sb", "H",  0, 360, c(0, 360),  step=5,  ticks=FALSE, width="100%"),
                                   checkboxInput("inv_rnd_h_sb", "inv H", value = FALSE),
                                   sliderInput("rnd_s_sb", "S%", 0, 100, c(10, 35),  step=1,  ticks=FALSE, width="100%"),
                                   sliderInput("rnd_v_sb", "V%", 0, 100, c(3,  14),  step=1,  ticks=FALSE, width="100%"),
                                   tags$span("Node bg", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("rnd_h_nb", "H",  0, 360, c(0, 360),  step=5,  ticks=FALSE, width="100%"),
                                   checkboxInput("inv_rnd_h_nb", "inv H", value = FALSE),
                                   sliderInput("rnd_s_nb", "S%", 0, 100, c(10, 35),  step=1,  ticks=FALSE, width="100%"),
                                   sliderInput("rnd_v_nb", "V%", 0, 100, c(3,  14),  step=1,  ticks=FALSE, width="100%"),
                                   tags$span("Theme accent H", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("rnd_h_theme", "H",  0, 360, c(80,  180), step=5,  ticks=FALSE, width="100%"),
                                   checkboxInput("inv_rnd_h_theme", "inv H", value = FALSE),
                                   tags$span("Project accent H", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("rnd_h_proj",  "H",  0, 360, c(20,  60),  step=5,  ticks=FALSE, width="100%"),
                                   checkboxInput("inv_rnd_h_proj", "inv H", value = FALSE),
                                   tags$span("Skill accent H", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("rnd_h_skill", "H",  0, 360, c(180, 280), step=5,  ticks=FALSE, width="100%"),
                                   checkboxInput("inv_rnd_h_skill", "inv H", value = FALSE),
                                   tags$span("Accent S/V (shared)", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("rnd_s_accent", "S%", 0, 100, c(25, 55), step=1, ticks=FALSE, width="100%"),
                                   sliderInput("rnd_v_accent", "V%", 0, 100, c(55, 80), step=1, ticks=FALSE, width="100%"),
                                   tags$hr(style = "margin:4px 0;border-color:rgba(255,255,255,0.1);"),
                                   sliderInput("hue_dist",   "Min hue distance (accent)",  0, 120, 40, step=5,  ticks=FALSE, width="100%"),
                                   sliderInput("max_s_dist", "Max S distance (accent)",    0, 100, 30, step=5,  ticks=FALSE, width="100%"),
                                   sliderInput("max_v_dist", "Max V distance (accent)",    0, 100, 20, step=5,  ticks=FALSE, width="100%"),
                                   actionButton("save_hsv", "Save HSV settings", class = "btn btn-default btn-xs", style = "width:100%;margin-top:3px;")
                                 )
                               )
                      ),

                      tabPanel("Light Color",
                               tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                        tags$label("Graph bg", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                        tags$input(id = "light_col_bg", type = "color", value = "#f0f4f8", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                               ),
                               tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                        tags$label("Sidebar bg", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                        tags$input(id = "light_col_sidebar_bg", type = "color", value = "#e2eaf3", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                               ),
                               tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                        tags$label("Node bg", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                        tags$input(id = "light_col_node_bg", type = "color", value = "#e2eaf3", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                               ),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               conditionalPanel("!input.light_one_color",
                                 tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                          tags$label("Theme", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                          tags$input(id = "light_col_theme", type = "color", value = "#1e7c45", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                                 ),
                                 tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                          tags$label("Project", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                          tags$input(id = "light_col_project", type = "color", value = "#c06000", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                                 ),
                                 tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                          tags$label("Skill", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                          tags$input(id = "light_col_skill", type = "color", value = "#1a7a7b", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                                 )
                               ),
                               conditionalPanel("input.light_one_color",
                                 tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                          tags$label("All node colors", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                          tags$input(id = "light_col_all", type = "color", value = "#1e7c45", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                                 )
                               ),
                               checkboxInput("light_one_color", "One color", value = FALSE),
                               tags$div(style = "display:flex;gap:4px;align-items:center;margin:3px 0;",
                                        tags$label("Edges", style = "color:rgba(255,255,255,0.7);font-size:15px;flex:1;"),
                                        tags$input(id = "light_edge_color", type = "color", value = "#555555", style = "width:32px;height:22px;border:none;padding:0;cursor:pointer;")
                               ),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               actionButton("save_light_colors", "Save light colors as default", class = "btn btn-default btn-xs", style = "width:100%;margin-bottom:5px;"),
                               tags$label("Light palette presets", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               selectInput("light_palette_choice", NULL, choices = c(), width = "100%"),
                               actionButton("save_light_palettes", "Save palettes to file", class = "btn btn-default btn-xs", style = "width:100%;margin-bottom:5px;"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$details(open = NA,
                                 tags$summary("Randomizer", style = "color:rgba(255,255,255,0.85);font-size:11px;cursor:pointer;padding:2px 0;"),
                                 tags$div(style = "margin-top:6px;",
                                   tags$div(style = "display:flex;gap:6px;flex-wrap:wrap;margin-bottom:4px;",
                                     checkboxInput("llock_bg",       "Lock bg",       value = FALSE),
                                     checkboxInput("llock_sidebar",  "Lock sidebar",  value = FALSE),
                                     checkboxInput("llock_node_bg",  "Lock node bg",  value = FALSE),
                                     checkboxInput("llock_theme",    "Lock theme",    value = FALSE),
                                     checkboxInput("llock_project",  "Lock project",  value = FALSE),
                                     checkboxInput("llock_skill",    "Lock skill",    value = FALSE)
                                   ),
                                   actionButton("light_randomize",       "Randomize",       class = "btn btn-default btn-sm", style = "width:100%;margin-bottom:3px;"),
                                   tags$div(style = "color:rgba(255,255,255,0.6);font-size:10px;line-height:1.35;margin:0 0 5px;", textOutput("lrand_hsv_txt", inline = TRUE)),
                                   actionButton("light_save_to_presets", "Save to presets", class = "btn btn-default btn-xs", style = "width:100%;margin-bottom:6px;"),
                                   tags$span("Background", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("lrnd_h_bg", "H",  0, 360, c(0, 360), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_lrnd_h_bg", "inv H", value = FALSE),
                                   sliderInput("lrnd_s_bg", "S%", 0, 100, c(0,  20), step=1, ticks=FALSE, width="100%"),
                                   sliderInput("lrnd_v_bg", "V%", 0, 100, c(82, 97), step=1, ticks=FALSE, width="100%"),
                                   tags$span("Sidebar", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("lrnd_h_sb", "H",  0, 360, c(0, 360), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_lrnd_h_sb", "inv H", value = FALSE),
                                   sliderInput("lrnd_s_sb", "S%", 0, 100, c(5,  25), step=1, ticks=FALSE, width="100%"),
                                   sliderInput("lrnd_v_sb", "V%", 0, 100, c(74, 88), step=1, ticks=FALSE, width="100%"),
                                   tags$span("Node bg", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("lrnd_h_nb", "H",  0, 360, c(0, 360), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_lrnd_h_nb", "inv H", value = FALSE),
                                   sliderInput("lrnd_s_nb", "S%", 0, 100, c(5,  25), step=1, ticks=FALSE, width="100%"),
                                   sliderInput("lrnd_v_nb", "V%", 0, 100, c(74, 88), step=1, ticks=FALSE, width="100%"),
                                   tags$span("Theme accent H", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("lrnd_h_theme", "H",  0, 360, c(80,  180), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_lrnd_h_theme", "inv H", value = FALSE),
                                   tags$span("Project accent H", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("lrnd_h_proj",  "H",  0, 360, c(20,   60), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_lrnd_h_proj", "inv H", value = FALSE),
                                   tags$span("Skill accent H", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("lrnd_h_skill", "H",  0, 360, c(180, 280), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_lrnd_h_skill", "inv H", value = FALSE),
                                   tags$span("Accent S/V (shared)", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   sliderInput("lrnd_s_accent", "S%", 0, 100, c(45, 75), step=1, ticks=FALSE, width="100%"),
                                   sliderInput("lrnd_v_accent", "V%", 0, 100, c(28, 55), step=1, ticks=FALSE, width="100%"),
                                   tags$hr(style = "margin:4px 0;border-color:rgba(255,255,255,0.1);"),
                                   sliderInput("lhue_dist",   "Min hue distance (accent)",  0, 120, 40, step=5, ticks=FALSE, width="100%"),
                                   sliderInput("lmax_s_dist", "Max S distance (accent)",    0, 100, 30, step=5, ticks=FALSE, width="100%"),
                                   sliderInput("lmax_v_dist", "Max V distance (accent)",    0, 100, 20, step=5, ticks=FALSE, width="100%"),
                                   actionButton("save_light_hsv", "Save HSV settings", class = "btn btn-default btn-xs", style = "width:100%;margin-top:3px;")
                                 )
                               ),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$details(open = NA,
                                 tags$summary("Randomize light edge colors", style = "color:rgba(255,255,255,0.85);font-size:11px;cursor:pointer;padding:2px 0;"),
                                 tags$div(style = "margin-top:6px;",
                                   tags$span("Theme sources", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;"),
                                   actionButton("lrandomize_edges_theme", "Randomize theme", class = "btn btn-default btn-sm", style = "width:100%;margin-bottom:3px;"),
                                   checkboxInput("lrb_theme", "Rainbow (even hue, constant S/V)", value = FALSE),
                                   checkboxInput("lrb_random_theme", "Random start hue", value = FALSE),
                                   conditionalPanel("!input.lrb_random_theme",
                                     sliderInput("lrb_start_theme", "Start hue (°)", 0, 360, 0, step=5, ticks=FALSE, width="100%")),
                                   tags$div(style = "color:rgba(255,255,255,0.6);font-size:10px;line-height:1.35;margin:0 0 4px;", textOutput("lec_hsv_txt_theme", inline = TRUE)),
                                   sliderInput("lec_h_theme", "H",  0, 360, c(0, 360), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_lec_h_theme", "inv H", value = FALSE),
                                   sliderInput("lec_s_theme", "S%", 0, 100, c(10, 50),  step=1, ticks=FALSE, width="100%"),
                                   sliderInput("lec_v_theme", "V%", 0, 100, c(15, 45),  step=1, ticks=FALSE, width="100%"),
                                   tags$span("Skill sources", style = "color:rgba(255,255,255,0.5);font-size:15px;display:block;margin-bottom:2px;margin-top:4px;"),
                                   actionButton("lrandomize_edges_skill", "Randomize skill", class = "btn btn-default btn-sm", style = "width:100%;margin-bottom:3px;"),
                                   checkboxInput("lrb_skill", "Rainbow (even hue, constant S/V)", value = FALSE),
                                   checkboxInput("lrb_random_skill", "Random start hue", value = FALSE),
                                   conditionalPanel("!input.lrb_random_skill",
                                     sliderInput("lrb_start_skill", "Start hue (°)", 0, 360, 0, step=5, ticks=FALSE, width="100%")),
                                   tags$div(style = "color:rgba(255,255,255,0.6);font-size:10px;line-height:1.35;margin:0 0 4px;", textOutput("lec_hsv_txt_skill", inline = TRUE)),
                                   sliderInput("lec_h_skill", "H",  0, 360, c(0, 360), step=5, ticks=FALSE, width="100%"),
                                   checkboxInput("inv_lec_h_skill", "inv H", value = FALSE),
                                   sliderInput("lec_s_skill", "S%", 0, 100, c(10, 50),  step=1, ticks=FALSE, width="100%"),
                                   sliderInput("lec_v_skill", "V%", 0, 100, c(15, 45),  step=1, ticks=FALSE, width="100%"),
                                   actionButton("save_lec_hsv", "Save HSV settings", class = "btn btn-default btn-xs", style = "width:100%;margin-top:3px;")
                                 )
                               )
                      ),

                      tabPanel("Column",
                               tags$label("About \u2014 accordion title", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("col_intro_title", NULL, value = "What is this site about", width = "100%"),
                               tags$label("About \u2014 body text", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textAreaInput("col_intro_text", NULL, value = "", rows = 12, width = "100%"),
                               tags$hr(style = "margin:6px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$label("Details \u2014 heading", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("details_title", NULL, value = "Details", width = "100%"),
                               tags$label("Details \u2014 hint text", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("details_hint", NULL, value = "Click on a topic in the graph to see details here", width = "100%"),
                               tags$hr(style = "margin:6px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$label("Vote \u2014 heading", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("vote_title", NULL, value = "Vote", width = "100%"),
                               tags$label("Vote \u2014 body text", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textAreaInput("vote_text", NULL, value = "Vote for themes, projects and skills you\u2019d like me to focus on:", rows = 9, width = "100%"),
                               tags$hr(style = "margin:6px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$label("Funding \u2014 heading", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("funding_title", NULL, value = "Funding", width = "100%"),
                               tags$label("Funding \u2014 intro text", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textAreaInput("funding_intro", NULL, value = "Current preference order to fund work on something related to the themes, projects and skills presented, when not working on my own time on them:", rows = 9, width = "100%"),
                               tags$label("Funding \u2014 items (one per line, indent with spaces)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textAreaInput("funding_items", NULL, value = paste(FUNDING_ITEMS, collapse = "\n"), rows = 30, width = "100%"),
                               tags$hr(style = "margin:8px 0;border-color:rgba(255,255,255,0.2);"),
                               tags$div(style = "color:rgba(255,255,255,0.5);font-size:15px;text-transform:uppercase;letter-spacing:0.08em;margin-bottom:4px;", "Finnish (FI) translations"),
                               tags$label("Page title (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_page_title", NULL, value = "", width = "100%"),
                               tags$label("About \u2014 title (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_col_intro_title", NULL, value = "", width = "100%"),
                               tags$label("About \u2014 text (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textAreaInput("fi_col_intro_text", NULL, value = "", rows = 9, width = "100%"),
                               tags$label("Details \u2014 heading (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_details_title", NULL, value = "", width = "100%"),
                               tags$label("Details \u2014 hint (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_details_hint", NULL, value = "", width = "100%"),
                               tags$label("Vote \u2014 heading (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_vote_title", NULL, value = "", width = "100%"),
                               tags$label("Vote \u2014 body text (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textAreaInput("fi_vote_text", NULL, value = "", rows = 6, width = "100%"),
                               tags$label("Funding \u2014 heading (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_funding_title", NULL, value = "", width = "100%"),
                               tags$label("Funding \u2014 intro text (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textAreaInput("fi_funding_intro", NULL, value = "", rows = 9, width = "100%"),
                               actionButton("apply_column", "Apply", class = "btn btn-primary btn-sm", style = "width:100%;margin-top:4px;")
                      ),

                      tabPanel("Headers",
                               tags$div(style = "color:rgba(255,255,255,0.5);font-size:15px;text-transform:uppercase;letter-spacing:0.08em;margin-bottom:4px;margin-top:2px;", "English"),
                               tags$label("Themes \u2014 row 1", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("hdr_theme_line1", NULL, value = "Themes", width = "100%"),
                               tags$label("Themes \u2014 row 2", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("hdr_theme_line2", NULL, value = "I want to focus on", width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$label("Projects \u2014 row 1", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("hdr_project_line1", NULL, value = "Projects", width = "100%"),
                               tags$label("Projects \u2014 row 2", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("hdr_project_line2", NULL, value = "I\u2019m working on or want to work on", width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$label("Skills \u2014 row 1", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("hdr_skill_line1", NULL, value = "Skills", width = "100%"),
                               tags$label("Skills \u2014 row 2", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("hdr_skill_line2", NULL, value = "I have or want to develop", width = "100%"),
                               tags$hr(style = "margin:8px 0;border-color:rgba(255,255,255,0.2);"),
                               tags$div(style = "color:rgba(255,255,255,0.5);font-size:15px;text-transform:uppercase;letter-spacing:0.08em;margin-bottom:4px;", "Finnish (FI)"),
                               tags$label("Themes \u2014 row 1 (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_hdr_theme_line1", NULL, value = "", width = "100%"),
                               tags$label("Themes \u2014 row 2 (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_hdr_theme_line2", NULL, value = "", width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$label("Projects \u2014 row 1 (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_hdr_project_line1", NULL, value = "", width = "100%"),
                               tags$label("Projects \u2014 row 2 (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_hdr_project_line2", NULL, value = "", width = "100%"),
                               tags$hr(style = "margin:5px 0;border-color:rgba(255,255,255,0.1);"),
                               tags$label("Skills \u2014 row 1 (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_hdr_skill_line1", NULL, value = "", width = "100%"),
                               tags$label("Skills \u2014 row 2 (FI)", style = "color:rgba(255,255,255,0.85);font-size:11px;"),
                               textInput("fi_hdr_skill_line2", NULL, value = "", width = "100%"),
                               actionButton("apply_headers", "Apply", class = "btn btn-primary btn-sm", style = "width:100%;margin-top:4px;")
                      ),

          )
      ),

      # ── Left spacer ──
      div(class = "col-spacer"),
      
      # ── Info sidebar (center) ──
      div(id = "info-sidebar",
          div(id = "page-title",
              tags$button(class = "lang-btn lang-active", id = "lang-btn-en",
                          onclick = "setLanguage('en')", HTML('<span class="fi fi-gb"></span>')),
              tags$button(class = "lang-btn", id = "lang-btn-fi",
                          onclick = "setLanguage('fi')", HTML('<span class="fi fi-fi"></span>')),
              div(id = "page-title-text",
                  span(class = "en-only", id = "page-title-en", "My interests - Ville Sepp\u00e4l\u00e4"),
                  span(class = "fi-only", id = "page-title-fi", "")),
              div(id = "controls-row",
                  tags$button(id = "mode-btn", onclick = "toggleLightMode()", "\u2600"),
                  tags$a(id = "site-url-label", href = "https://villeseppala.github.io/interests", target = "_blank",
                         style = "flex:1;text-align:center;font-size:clamp(6.9px,0.95vw,17.25px);color:rgba(255,255,255,1);text-decoration:none;white-space:nowrap;overflow:hidden;letter-spacing:0.02em;",
                         "villeseppala.github.io/interests"),
                  tags$a(id = "github-btn", href = "https://github.com/villeseppala/interests", target = "_blank", title = "GitHub",
                         HTML('<svg viewBox="0 0 16 16" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"/></svg>'))
              )
          ),
          div(id = "sidebar-scroll",
              # Accordion 1 — Description (open by default)
              div(class = "acc-section acc-open", id = "acc-desc",
                  div(class = "acc-header", onclick = "toggleAcc(this)",
                      span(class = "acc-title", id = "acc-title-desc"),
                      span(class = "acc-arrow", HTML("&#9660;"))
                  ),
                  div(class = "acc-body",
                      uiOutput("sidebar_hint_ui"),
                      div(id = "desc-panel",
                          div(id = "desc-header",
                              div(id = "desc-title", ""),
                              tags$button(id = "desc-close", onclick = "hideDescPanel()", "\u00d7")
                          ),
                          div(id = "desc-body", "")
                      )
                  )
              ),
              # Accordion 2 — About (collapsed)
              div(class = "acc-section acc-open", id = "acc-about",
                  div(class = "acc-header", onclick = "toggleAcc(this)",
                      span(class = "acc-title", id = "acc-title-about"),
                      span(class = "acc-arrow", HTML("&#9660;"))
                  ),
                  div(class = "acc-body",
                      uiOutput("col_intro_ui")
                  )
              ),
              # Accordion 3 — Vote (collapsed)
              div(class = "acc-section", id = "acc-vote",
                  div(class = "acc-header", onclick = "toggleAcc(this)",
                      span(class = "acc-title", id = "acc-title-vote"),
                      span(class = "acc-arrow", HTML("&#9660;"))
                  ),
                  div(class = "acc-body",
                      uiOutput("vote_section_ui")
                  )
              ),
              # Accordion 4 — Funding (collapsed)
              div(class = "acc-section", id = "acc-fund",
                  div(class = "acc-header", onclick = "toggleAcc(this)",
                      span(class = "acc-title", id = "acc-title-fund"),
                      span(class = "acc-arrow", HTML("&#9660;"))
                  ),
                  div(class = "acc-body",
                      uiOutput("funding_ui")
                  )
              )
          )
      ),
      
      # ── Resize handle ──
      div(id = "sidebar-resize-handle"),
      
      # ── Graph area ──
      div(id = "graph-area", div(id = "cy")),

      # ── Mobile drag handle ──
      div(id = "mob-handle"),

      # ── Mobile bottom panel ──
      div(id = "mob-panel",
          div(id = "mob-tab-bar",
              tags$button(class = "mob-tab mob-tab-active", id = "mob-tab-about",
                          onclick = "mobShowTab('about')",
                          span(class = "en-only", "About"), span(class = "fi-only", "Tietoa")),
              tags$button(class = "mob-tab", id = "mob-tab-vote",
                          onclick = "mobShowTab('vote')",
                          span(class = "en-only", "Vote"), span(class = "fi-only", "\u00c4\u00e4nest\u00e4")),
              tags$button(class = "mob-tab", id = "mob-tab-fund",
                          onclick = "mobShowTab('fund')",
                          span(class = "en-only", "Funding"), span(class = "fi-only", "Rahoitus"))
          ),
          div(id = "mob-tab-content",
              div(id = "mob-content-about", class = "mob-tab-pane mob-tab-pane-active"),
              div(id = "mob-content-vote",  class = "mob-tab-pane"),
              div(id = "mob-content-fund",  class = "mob-tab-pane")
          ),
          div(id = "mob-desc-panel",
              div(id = "mob-desc-header",
                  div(id = "mob-desc-title", ""),
                  tags$button(id = "mob-desc-close", onclick = "mobCloseDesc()", "\u00d7")
              ),
              div(id = "mob-desc-body", "")
          )
      )
  ),

  # ── Mobile bottom sheet (outside main-row, fixed position) ──
  div(id = "mobile-bottom-sheet",
      div(id = "mobile-bs-grab"),
      div(id = "mobile-bs-header",
          div(id = "mobile-bs-title", ""),
          tags$button(id = "mobile-bs-close", onclick = "hideDescPanel()", "\u00d7")
      ),
      div(id = "mobile-bs-body", "")
  ),
  
  # ── Mobile info button (fixed, bottom-right) ──
  tags$button(id = "mobile-info-btn", "Info")
)

linkify <- function(txt) {
  txt <- gsub("\n", "<br>", txt, fixed = TRUE)
  txt <- gsub("\\[([^\\]]+)\\]\\((https?://[^)\\s]+)\\)",
    '<a href="\\2" target="_blank" rel="noopener" style="color:inherit;opacity:0.85;text-decoration:underline;">\\1</a>',
    txt, perl = TRUE)
  txt <- gsub('(?<!href=")(https?://[^\\s<>"]+)',
    '<a href="\\1" target="_blank" rel="noopener" style="color:inherit;opacity:0.85;text-decoration:underline;">\\1</a>',
    txt, perl = TRUE)
  HTML(txt)
}

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  output$export_png <- downloadHandler(
    filename = "interests.png",
    content  = function(file) {
      if (!requireNamespace("webshot2", quietly = TRUE))
        stop("Install webshot2: install.packages('webshot2')")

      # Build the current payload from reactive state (same as what the reactive sends to client)
      ly  <- isolate(rv$g$layout)
      cc  <- isolate(rv$current_colors)
      cd  <- isolate(build_dual_cyto_data(
        rv$g,
        gap_v         = input$gap_v         %||% ly$gap_v         %||% 18,
        gap_col       = input$gap_col       %||% ly$gap_col       %||% 400,
        font_node     = input$font_node     %||% ly$font_node     %||% 12,
        font_ptype    = input$font_ptype    %||% ly$font_ptype    %||% 12,
        font_subs     = input$font_subs     %||% ly$font_subs     %||% 15,
        font_desc     = input$font_desc     %||% ly$font_desc     %||% 18,
        font_hdr1     = input$font_hdr1     %||% ly$font_hdr1     %||% 22,
        font_hdr2     = input$font_hdr2     %||% ly$font_hdr2     %||% 15,
        h_theme       = input$h_theme       %||% ly$h_theme       %||% 46,
        h_project     = input$h_project     %||% ly$h_project     %||% 66,
        h_skill       = input$h_skill       %||% ly$h_skill       %||% 46,
        w_project     = input$w_project     %||% ly$w_project     %||% NODE_W$Project,
        w_node        = input$w_node        %||% ly$w_node,
        inline_mode   = isTRUE(input$inline_mode),
        articles_enabled = isTRUE(input$articles_enabled),
        watermark_text  = if (isTRUE(input$show_watermark)) (input$watermark_text %||% "") else "",
        watermark_size  = input$watermark_size %||% ly$watermark_size %||% 10,
        col_bg          = input$col_bg         %||% cc$col_bg         %||% ly$col_bg         %||% "#0b3552",
        col_sidebar_bg  = input$col_sidebar_bg %||% cc$col_sidebar_bg %||% ly$col_sidebar_bg %||% "#081626",
        col_node_bg     = input$col_node_bg    %||% cc$col_node_bg    %||% ly$col_node_bg    %||% "#081626",
        col_theme       = input$col_theme      %||% cc$col_theme      %||% ly$col_theme      %||% "#3be37a",
        col_project     = input$col_project    %||% cc$col_project    %||% ly$col_project    %||% "#ffad33",
        col_skill       = input$col_skill      %||% cc$col_skill      %||% ly$col_skill      %||% "#78e6e7",
        light_col_bg         = input$light_col_bg         %||% ly$light_col_bg         %||% "#f0f4f8",
        light_col_sidebar_bg = input$light_col_sidebar_bg %||% ly$light_col_sidebar_bg %||% "#e2eaf3",
        light_col_node_bg    = input$light_col_node_bg    %||% ly$light_col_node_bg    %||% "#e2eaf3",
        light_col_theme      = input$light_col_theme      %||% ly$light_col_theme      %||% "#1e7c45",
        light_col_project    = input$light_col_project    %||% ly$light_col_project    %||% "#c06000",
        light_col_skill      = input$light_col_skill      %||% ly$light_col_skill      %||% "#1a7a7b",
        light_edge_color     = input$light_edge_color     %||% ly$light_edge_color     %||% "#555555",
        mob_font_mult    = input$mob_font_mult    %||% ly$mob_font_mult    %||% MOBILE_DEFAULTS$mob_font_mult,
        mob_h_theme_mult = input$mob_h_theme_mult %||% ly$mob_h_theme_mult %||% MOBILE_DEFAULTS$mob_h_theme_mult,
        mob_h_proj_mult  = input$mob_h_proj_mult  %||% ly$mob_h_proj_mult  %||% MOBILE_DEFAULTS$mob_h_proj_mult,
        mob_h_skill_mult = input$mob_h_skill_mult %||% ly$mob_h_skill_mult %||% MOBILE_DEFAULTS$mob_h_skill_mult,
        mob_gap_v_mult   = input$mob_gap_v_mult   %||% ly$mob_gap_v_mult   %||% MOBILE_DEFAULTS$mob_gap_v_mult,
        mob_gap_col_mult = input$mob_gap_col_mult %||% ly$mob_gap_col_mult %||% MOBILE_DEFAULTS$mob_gap_col_mult,
        hdr_theme_line1=rv_col$hdr_theme_line1, hdr_theme_line2=rv_col$hdr_theme_line2,
        hdr_project_line1=rv_col$hdr_project_line1, hdr_project_line2=rv_col$hdr_project_line2,
        hdr_skill_line1=rv_col$hdr_skill_line1, hdr_skill_line2=rv_col$hdr_skill_line2,
        fi_hdr_theme_line1=rv_col$fi_hdr_theme_line1, fi_hdr_theme_line2=rv_col$fi_hdr_theme_line2,
        fi_hdr_project_line1=rv_col$fi_hdr_project_line1, fi_hdr_project_line2=rv_col$fi_hdr_project_line2,
        fi_hdr_skill_line1=rv_col$fi_hdr_skill_line1, fi_hdr_skill_line2=rv_col$fi_hdr_skill_line2
      ))

      # Add sidebar content to payload (populateStaticSidebar reads these fields)
      fund_items_html <- if (length(rv_col$fund_items) > 0)
        paste(vapply(rv_col$fund_items, function(l) {
          s <- sub("^( +)", "", l); paste0(strrep("&nbsp;", (nchar(l)-nchar(s))*3), s)
        }, character(1)), collapse = "<br>") else ""
      cd$edge_width <- input$edge_width %||% rv$g$layout$edge_width %||% 2.5
      cd$edge_bands <- isTRUE(input$edge_bands %||% (rv$g$layout$edge_bands %||% TRUE))
      cd$edge_sankey <- isTRUE(input$edge_sankey %||% (rv$g$layout$edge_sankey %||% FALSE))
      cd$edge_gap <- input$edge_gap %||% rv$g$layout$edge_gap %||% 3
      cd$edge_transparency <- input$edge_transparency %||% rv$g$layout$edge_transparency %||% 18
      cd$edge_min_width <- input$edge_min_width %||% rv$g$layout$edge_min_width %||% 2.5
      cd$edge_min_on <- isTRUE(input$edge_min_on %||% (rv$g$layout$edge_min_on %||% TRUE))
      cd$edge_curve <- input$edge_curve %||% rv$g$layout$edge_curve %||% 1
      cd$edge_pin_header <- isTRUE(input$edge_pin_header %||% (rv$g$layout$edge_pin_header %||% FALSE))
      cd$fill_nodew <- input$fill_nodew %||% rv$g$layout$fill_nodew %||% 0
      cd$fill_projw <- input$fill_projw %||% rv$g$layout$fill_projw %||% 0
      cd$fill_colgap <- input$fill_colgap %||% rv$g$layout$fill_colgap %||% 0
      cd$gradient_extent <- input$gradient_extent %||% rv$g$layout$gradient_extent %||% 20
      cd$gradient_transparency <- input$gradient_transparency %||% rv$g$layout$gradient_transparency %||% 40
      cd$gradient_curve <- input$gradient_curve %||% rv$g$layout$gradient_curve %||% 1
      cd$gradient_hover_mult <- input$gradient_hover_mult %||% rv$g$layout$gradient_hover_mult %||% 2
      cd$node_outline <- input$node_outline %||% rv$g$layout$node_outline %||% 3
      cd$project_outline <- input$project_outline %||% rv$g$layout$project_outline %||% cd$node_outline
      cd$outline_saturation <- input$outline_saturation %||% rv$g$layout$outline_saturation %||% 1
      cd$outline_transparency <- input$outline_transparency %||% rv$g$layout$outline_transparency %||% 0
      cd$node_pad <- input$node_pad %||% rv$g$layout$node_pad %||% 0
      cd$desc_pad <- input$desc_pad %||% rv$g$layout$desc_pad %||% 10
      cd$inline_mode <- isTRUE(input$inline_mode)
      cd$articles_enabled <- isTRUE(input$articles_enabled)
      cd$accordion_icon <- input$accordion_icon %||% rv$g$layout$accordion_icon %||% "triangle"
      cd$accordion_icon_size <- input$accordion_icon_size %||% rv$g$layout$accordion_icon_size %||% 14

      cd$sidebar <- list(
        details_title    = rv_col$details_title    %||% "Descriptions",
        intro_title      = rv_col$intro_title      %||% "What is this site about",
        vote_title       = rv_col$vote_title       %||% "Influence my focus",
        fund_title       = rv_col$fund_title       %||% "Funding/work opportunities",
        details_hint     = rv_col$details_hint     %||% "Click on a topic to see details",
        details_hint_fi  = rv_col$fi_details_hint  %||% "",
        details_title_fi = rv_col$fi_details_title %||% "",
        intro_title_fi   = rv_col$fi_intro_title   %||% "",
        vote_title_fi    = rv_col$fi_vote_title    %||% "",
        fund_title_fi    = rv_col$fi_fund_title    %||% "",
        page_title_en    = "My interests - Ville Seppälä",
        page_title_fi    = rv_col$fi_page_title    %||% ""
      )
      cd$intro_html    <- list(en = as.character(linkify(rv_col$intro_text      %||% "")),
                               fi = as.character(linkify(if (nzchar(trimws(rv_col$fi_intro_text %||% ""))) rv_col$fi_intro_text else rv_col$intro_text %||% "")))
      cd$vote_html     <- list(en = as.character(linkify(rv_col$vote_text       %||% "")),
                               fi = as.character(linkify(if (nzchar(trimws(rv_col$fi_vote_text %||% ""))) rv_col$fi_vote_text else rv_col$vote_text %||% "")))
      cd$funding_html  <- list(en_intro = as.character(linkify(rv_col$fund_intro %||% "")),
                               fi_intro = as.character(linkify(rv_col$fi_fund_intro %||% rv_col$fund_intro %||% "")),
                               items    = fund_items_html)

      # Build a fully self-contained HTML using safe line-splicing (no gsub on JS
      # content — render.js has many \\ sequences that gsub would corrupt).
      payload_json <- jsonlite::toJSON(cd, auto_unbox = TRUE)
      # Escape </script> so it can't prematurely close the inline script block
      payload_json <- gsub("</script>", "<\\/script>", payload_json, fixed = TRUE)
      payload_json <- gsub("<!--",      "<\\!--",       payload_json, fixed = TRUE)

      style_lines  <- readLines(here("app_publish", "www", "style.css"),  warn=FALSE, encoding="UTF-8")
      render_lines <- readLines(here("app_publish", "www", "render.js"),  warn=FALSE, encoding="UTF-8")
      html_lines   <- readLines(here("site", "index.html"), warn=FALSE, encoding="UTF-8")
      # Strip cache-busting query params (?v=1234) that build_static.R stamps onto the asset refs,
      # so the exact-line splices below still match style.css / render.js / payload.json.
      html_lines   <- gsub("\\?v=[0-9]+", "", html_lines)

      # A4 landscape ratio (297:210). Keep 1440-wide viewport so it still matches the
      # publish-app look; height = 1440 * 210/297 so the output is A4-proportioned.
      vwidth  <- 1440L
      vheight <- as.integer(round(vwidth * 210 / 297))   # 1018 -> 3744x2647 at zoom 2.6 (A4 landscape)

      # Helper: splice replacement lines in place of the single matching line
      splice <- function(lines, match_str, replacement) {
        i <- which(trimws(lines) == trimws(match_str))
        if (!length(i)) return(lines)
        c(lines[seq_len(i[1]-1)], replacement, lines[(i[1]+1):length(lines)])
      }

      # 1. Replace <link href="style.css"> with inlined CSS + #cy min-height fix
      css_block <- c("<style>", style_lines, "</style>",
                     "<style>#cy { min-height:", paste0(vheight, "px !important; }"), "</style>")
      html_lines <- splice(html_lines, '<link rel="stylesheet" href="style.css">', css_block)

      # 2. Replace <script src="render.js"> with inlined JS
      js_block <- c("<script>", render_lines, "</script>")
      html_lines <- splice(html_lines, '<script src="render.js"></script>', js_block)

      # 3. Replace the fetch('payload.json') block with embedded init
      #    Find the <script> that contains fetch('payload.json') and replace it wholesale
      fetch_line <- which(grepl("fetch('payload.json')", html_lines, fixed=TRUE))[1]
      if (!is.na(fetch_line)) {
        # Walk backwards to opening <script> and forwards to closing </script>
        open_line  <- max(which(grepl("^\\s*<script>\\s*$", html_lines[1:fetch_line])))
        close_line <- fetch_line - 1 + min(which(grepl("^\\s*</script>\\s*$",
                                                        html_lines[fetch_line:length(html_lines)])))
        light_js <- if (isTRUE(isolate(input$light_mode_active)))
          "    if(typeof toggleLightMode==='function') setTimeout(toggleLightMode,200);" else ""
        lang_js <- if (identical(isolate(input$current_lang_active), "fi"))
          "    if(typeof setLanguage==='function') setTimeout(function(){ setLanguage('fi'); },250);" else ""
        # Re-open the nodes that are currently expanded in the live author view (reported by render.js
        # via input$inline_open_ids), so the export matches what's on screen.
        open_ids  <- isolate(input$inline_open_ids)
        open_json <- if (length(open_ids)) jsonlite::toJSON(as.character(open_ids)) else "[]"
        open_js   <- paste0("    var __openIds=", open_json, ";\n",
                            "    setTimeout(function(){ __openIds.forEach(function(id){ ",
                            "if(typeof openNodeById==='function') openNodeById(id); }); }, 1200);")
        init_block <- c(
          "<script>",
          "(function(){",
          paste0("  var __d=", payload_json, ";"),
          "  function doInit(){",
          "    if(typeof window.initStaticApp==='function') window.initStaticApp(__d);",
          light_js,
          lang_js,
          open_js,
          "  }",
          "  if(document.readyState==='loading'){",
          "    document.addEventListener('DOMContentLoaded',function(){ setTimeout(doInit,1000); });",
          "  } else { setTimeout(doInit,1000); }",
          "})();",
          "</script>"
        )
        html_lines <- c(html_lines[seq_len(open_line-1)],
                        init_block,
                        html_lines[(close_line+1):length(html_lines)])
      }

      # Write and screenshot
      tmpfile <- tempfile(fileext = ".html")
      on.exit(unlink(tmpfile), add = TRUE)
      writeLines(html_lines, con = file(tmpfile, "w", encoding="UTF-8"))

      html_path <- normalizePath(tmpfile, winslash = "/", mustWork = TRUE)
      # zoom scales the pixel count by zoom^2; 2.6 ~= 1.5*sqrt(3) -> ~3x the pixels of the old 1.5
      webshot2::webshot(url = paste0("file:///", html_path), file = file,
                        vwidth = vwidth, vheight = vheight, zoom = 2.6, delay = 8)
    }
  )

  observeEvent(input$stop_app, { stopApp() })
  observeEvent(input$restart_app, {
    writeLines("1", here("app_author", ".restart"))
    stopApp()
  })
  observeEvent(input$node_bg_same_as_sidebar, {
    session$sendCustomMessage("setNodeBgSameAsSidebar", isTRUE(input$node_bg_same_as_sidebar))
  }, ignoreInit = TRUE)
  observeEvent(input$inline_mode, {
    session$sendCustomMessage("setInlineMode", list(value = isTRUE(input$inline_mode)))
  }, ignoreInit = TRUE)
  observeEvent(input$edit_inline_text, {
    session$sendCustomMessage("setAuthorEditable", list(value = isTRUE(input$edit_inline_text)))
  }, ignoreInit = TRUE)
  # Inline title edit committed on a node (from render.js). Persist to rv$g; the client already
  # updated the label locally, so skip the disruptive full-graph rebuild that cyto_data() would send.
  observeEvent(input$node_title_edit, {
    info <- input$node_title_edit
    id <- suppressWarnings(as.numeric(info$id)); if (is.na(id)) return()
    txt <- info$text %||% ""
    for (i in seq_along(rv$g$nodes)) {
      if (as.numeric(rv$g$nodes[[i]]$id) == id) {
        if (identical(info$lang, "fi")) rv$g$nodes[[i]]$title_fi <- txt
        else                            rv$g$nodes[[i]]$title    <- txt
        break
      }
    }
    rv$skip_cy_rebuild <- TRUE
    # Keep the sidebar editor in sync if this node is the selected one
    if (identical(rv$selected_id, id)) {
      if (identical(info$lang, "fi")) updateTextInput(session, "edit_title_fi", value = txt)
      else                            updateTextInput(session, "edit_title",    value = txt)
    }
  })
  # Inline description edit committed on a node (raw markdown). Persist to rv$desc_map (not part of
  # cyto_data(), so no graph rebuild); the client already updated the rendered text locally.
  observeEvent(input$node_desc_edit, {
    info <- input$node_desc_edit
    id <- suppressWarnings(as.numeric(info$id)); if (is.na(id)) return()
    grp <- NULL
    for (n in rv$g$nodes) if (as.numeric(n$id) == id) { grp <- n$group %||% "Theme"; break }
    pre <- GROUP_PREFIX[[grp]]; if (is.null(pre)) return()
    key <- if (identical(info$lang, "fi")) paste0("fi_", pre, id) else paste0(pre, id)
    rv$desc_map[[key]] <- info$text %||% ""
    if (identical(rv$selected_id, id)) {
      if (identical(info$lang, "fi")) updateTextAreaInput(session, "edit_desc_fi", value = info$text %||% "")
      else                            updateTextAreaInput(session, "edit_desc",    value = info$text %||% "")
    }
  })

  # Inline article editing (author app): write the edited body back to articles/<id>.qmd, keeping the
  # YAML front matter intact. Only the on-node quick-read + qmd source update here; the external Quarto
  # HTML page refreshes on the next build (tools/build_static.R / quarto render).
  observeEvent(input$article_edit, {
    info <- input$article_edit
    id <- info$id; if (is.null(id)) return()
    id <- as.character(id)
    txt <- info$text %||% ""
    path <- here("articles", paste0(id, ".qmd"))
    if (!file.exists(path)) return()
    old <- readLines(path, warn = FALSE, encoding = "UTF-8")
    fences <- which(grepl("^---\\s*$", old))
    header <- if (length(fences) >= 2) old[1:fences[2]] else character(0)   # keep front matter block
    body   <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    new_lines <- c(header, if (length(header)) "" else character(0), body)
    con <- file(path, open = "wb")
    writeLines(enc2utf8(new_lines), con, useBytes = TRUE)
    close(con)
    tmp <- ART_SCAN; tmp$inline[[id]] <- txt; ART_SCAN <<- tmp   # keep this session's quick-read in sync
  })
  observeEvent(input$articles_enabled, {
    session$sendCustomMessage("setArticlesEnabled", list(value = isTRUE(input$articles_enabled)))
  }, ignoreInit = TRUE)

  observeEvent(input$accordion_icon, {
    session$sendCustomMessage("setAccordionIcon", list(style = input$accordion_icon))
  }, ignoreInit = TRUE)

  observeEvent(input$accordion_icon_size, {
    session$sendCustomMessage("setAccordionIconSize", list(size = input$accordion_icon_size))
  }, ignoreInit = TRUE)

  loaded_g    <- read_graph(GRAPH_PATH)
  initial_desc <- extract_from_qmd(QMD_PATH)
  PALETTES <- tryCatch(
    jsonlite::fromJSON(here("app_author", "data", "palettes.json"), simplifyVector = FALSE),
    error = function(e) list()
  )
  LIGHT_PALETTES <- tryCatch(
    jsonlite::fromJSON(here("app_author", "data", "light_palettes.json"), simplifyVector = FALSE),
    error = function(e) list(
      list(name="Default Light",
           light_col_bg="#f0f4f8", light_col_sidebar_bg="#e2eaf3",
           light_col_theme="#1e7c45", light_col_project="#c06000", light_col_skill="#1a7a7b"),
      list(name="Warm Light",
           light_col_bg="#faf7f2", light_col_sidebar_bg="#ede4d7",
           light_col_theme="#7a3a10", light_col_project="#8b1a1a", light_col_skill="#1a5c7a"),
      list(name="Cool Light",
           light_col_bg="#f2f5fa", light_col_sidebar_bg="#dce6f5",
           light_col_theme="#1a3f8a", light_col_project="#7a1a8a", light_col_skill="#1a7a5a")
    )
  )
  DEFAULTS_PATH <- here("app_author", "data", "defaults.json")
  read_defaults  <- function() tryCatch(jsonlite::fromJSON(DEFAULTS_PATH, simplifyVector = TRUE), error = function(e) list())
  write_defaults <- function(d) tryCatch(
    jsonlite::write_json(d, DEFAULTS_PATH, pretty = TRUE, auto_unbox = TRUE),
    error = function(e) showNotification(paste("Save failed:", e$message), type = "error", duration = 8)
  )
  
  # Helper: send colors to JS pickers AND store in rv so save_colors always has values
  set_colors <- function(cols) {
    rv$current_colors <- cols
    session$sendCustomMessage("setColorInputs", cols)
  }

  set_light_colors <- function(cols) {
    rv$current_light_colors <- cols
    session$sendCustomMessage("setColorInputs", cols)
  }

  padded_desc <- initial_desc
  for (n in loaded_g$nodes) {
    pre <- GROUP_PREFIX[[n$group %||% ""]]
    if (is.null(pre)) next
    key <- paste0(pre, as.numeric(n$id))
    if (is.null(padded_desc[[key]])) padded_desc[[key]] <- ""
  }
  
  # Pre-load saved default colors synchronously so cyto_data() has them before any JS round-trip
  init_defs <- read_defaults()
  init_colors <- if (!is.null(init_defs$colors) && !is.null(init_defs$colors[["col_bg"]]))
    as.list(init_defs$colors) else NULL

  init_light_colors <- if (!is.null(init_defs$light_colors) && !is.null(init_defs$light_colors[["light_col_bg"]]))
    as.list(init_defs$light_colors) else NULL

  rv <- reactiveValues(
    g               = loaded_g,
    desc_map        = padded_desc,
    selected_id     = NA_integer_,
    last_random     = NULL,
    custom_palettes       = list(),
    palette_fire_count    = 0L,
    current_colors        = init_colors,
    last_light_random        = NULL,
    light_custom_palettes    = list(),
    light_palette_fire_count = 0L,
    current_light_colors     = init_light_colors
  )
  
  ensure_desc_keys <- function() {
    for (n in rv$g$nodes) {
      pre <- GROUP_PREFIX[[n$group %||% ""]]
      if (is.null(pre)) next
      key <- paste0(pre, as.numeric(n$id))
      if (is.null(rv$desc_map[[key]])) rv$desc_map[[key]] <- ""
    }
  }
  
  nodes_df <- reactive({
    rows <- lapply(rv$g$nodes, function(n) data.frame(
      id = as.numeric(n$id), group = n$group %||% "Theme", title = n$title %||% "",
      title_fi = n$title_fi %||% "",
      ptype = n$ptype %||% NA_character_,
      pnum = if (!is.null(n$pnum)) as.integer(n$pnum) else NA_integer_,
      stringsAsFactors = FALSE))
    if (!length(rows)) return(data.frame(
      id = integer(0), group = character(0), title = character(0), title_fi = character(0),
      ptype = character(0), pnum = integer(0), stringsAsFactors = FALSE))
    do.call(rbind, rows)
  })
  
  edges_df <- reactive({
    rows <- lapply(rv$g$edges, function(e) data.frame(
      from = as.numeric(e$from), to = as.numeric(e$to),
      color = e$color %||% NA_character_,
      dashes = isTRUE(e$dashes), hidden = isTRUE(e$hidden), stringsAsFactors = FALSE))
    if (!length(rows)) return(data.frame(
      from = integer(0), to = integer(0), color = character(0),
      dashes = logical(0), hidden = logical(0), stringsAsFactors = FALSE))
    do.call(rbind, rows)
  })
  
  cyto_data <- reactive({
    gv <- input$gap_v   %||% (rv$g$layout$gap_v   %||% 18)
    gc <- input$gap_col %||% (rv$g$layout$gap_col %||% 400)
    fn <- input$font_node  %||% (rv$g$layout$font_node  %||% 12)
    fp <- input$font_ptype %||% (rv$g$layout$font_ptype %||% 12)
    fs <- input$font_subs  %||% (rv$g$layout$font_subs  %||% 15)
    fd <- input$font_desc  %||% (rv$g$layout$font_desc  %||% 18)
    fh1 <- input$font_hdr1 %||% (rv$g$layout$font_hdr1  %||% 22)
    fh2 <- input$font_hdr2 %||% (rv$g$layout$font_hdr2  %||% 15)
    ht <- input$h_theme   %||% (rv$g$layout$h_theme   %||% 46)
    hp <- input$h_project %||% (rv$g$layout$h_project %||% 66)
    hs <- input$h_skill   %||% (rv$g$layout$h_skill   %||% 46)
    wp <- input$w_project %||% (rv$g$layout$w_project %||% NODE_W$Project)
    wn <- input$w_node    %||% (rv$g$layout$w_node    %||% wp)   # theme/skill width; defaults to project width
    wt <- if (isTRUE(input$show_watermark)) (input$watermark_text %||% "") else ""
    ws <- input$watermark_size %||% (rv$g$layout$watermark_size %||% 10)
    cc  <- rv$current_colors
    cbg <- input$col_bg         %||% cc$col_bg         %||% (rv$g$layout$col_bg         %||% "#0b3552")
    csb <- input$col_sidebar_bg %||% cc$col_sidebar_bg %||% (rv$g$layout$col_sidebar_bg %||% "#081626")
    cnb <- input$col_node_bg   %||% cc$col_node_bg   %||% (rv$g$layout$col_node_bg   %||% "#081626")
    ct  <- input$col_theme      %||% cc$col_theme      %||% (rv$g$layout$col_theme      %||% "#3be37a")
    cp  <- input$col_project    %||% cc$col_project    %||% (rv$g$layout$col_project    %||% "#ffad33")
    cs  <- input$col_skill      %||% cc$col_skill      %||% (rv$g$layout$col_skill      %||% "#78e6e7")
    lcbg <- input$light_col_bg         %||% (rv$g$layout$light_col_bg         %||% "#f0f4f8")
    lcsb <- input$light_col_sidebar_bg %||% (rv$g$layout$light_col_sidebar_bg %||% "#e2eaf3")
    lcnb <- input$light_col_node_bg   %||% (rv$g$layout$light_col_node_bg   %||% "#e2eaf3")
    lct  <- input$light_col_theme      %||% (rv$g$layout$light_col_theme      %||% "#1e7c45")
    lcp  <- input$light_col_project    %||% (rv$g$layout$light_col_project    %||% "#c06000")
    lcs  <- input$light_col_skill      %||% (rv$g$layout$light_col_skill      %||% "#1a7a7b")
    lec  <- input$light_edge_color     %||% (rv$g$layout$light_edge_color     %||% "#555555")
    mfm  <- input$mob_font_mult    %||% (rv$g$layout$mob_font_mult    %||% MOBILE_DEFAULTS$mob_font_mult)
    mht  <- input$mob_h_theme_mult %||% (rv$g$layout$mob_h_theme_mult %||% MOBILE_DEFAULTS$mob_h_theme_mult)
    mhp  <- input$mob_h_proj_mult  %||% (rv$g$layout$mob_h_proj_mult  %||% MOBILE_DEFAULTS$mob_h_proj_mult)
    mhs  <- input$mob_h_skill_mult %||% (rv$g$layout$mob_h_skill_mult %||% MOBILE_DEFAULTS$mob_h_skill_mult)
    mgv  <- input$mob_gap_v_mult   %||% (rv$g$layout$mob_gap_v_mult   %||% MOBILE_DEFAULTS$mob_gap_v_mult)
    mgc  <- input$mob_gap_col_mult %||% (rv$g$layout$mob_gap_col_mult %||% MOBILE_DEFAULTS$mob_gap_col_mult)
    build_dual_cyto_data(rv$g, gap_v = gv, gap_col = gc,
                         font_node = fn, font_ptype = fp, font_subs = fs, font_desc = fd,
                         font_hdr1 = fh1, font_hdr2 = fh2,
                         h_theme = ht, h_project = hp, h_skill = hs,
                         w_project = wp, w_node = wn, inline_mode = isTRUE(input$inline_mode),
                         articles_enabled = isTRUE(input$articles_enabled),
                         watermark_text = wt, watermark_size = ws,
                         col_bg = cbg, col_sidebar_bg = csb, col_node_bg = cnb, col_theme = ct, col_project = cp, col_skill = cs,
                         light_col_bg = lcbg, light_col_sidebar_bg = lcsb, light_col_node_bg = lcnb,
                         light_col_theme = lct, light_col_project = lcp, light_col_skill = lcs,
                         light_edge_color = lec,
                         mob_font_mult = mfm, mob_h_theme_mult = mht,
                         mob_h_proj_mult = mhp, mob_h_skill_mult = mhs,
                         mob_gap_v_mult = mgv, mob_gap_col_mult = mgc,
                         hdr_theme_line1=rv_col$hdr_theme_line1, hdr_theme_line2=rv_col$hdr_theme_line2,
                         hdr_project_line1=rv_col$hdr_project_line1, hdr_project_line2=rv_col$hdr_project_line2,
                         hdr_skill_line1=rv_col$hdr_skill_line1, hdr_skill_line2=rv_col$hdr_skill_line2,
                         fi_hdr_theme_line1=rv_col$fi_hdr_theme_line1, fi_hdr_theme_line2=rv_col$fi_hdr_theme_line2,
                         fi_hdr_project_line1=rv_col$fi_hdr_project_line1, fi_hdr_project_line2=rv_col$fi_hdr_project_line2,
                         fi_hdr_skill_line1=rv_col$fi_hdr_skill_line1, fi_hdr_skill_line2=rv_col$fi_hdr_skill_line2)
  })
  
  # Initial render: compute from saved layout, not from inputs (which may be NULL)
  observe({
    ly <- isolate(rv$g$layout)
    # Read saved defaults first so initCy uses them directly (no flash of graph.json colors)
    defs <- read_defaults()
    dc <- if (!is.null(defs$colors) && !is.null(defs$colors[["col_bg"]])) defs$colors else list()
    cd <- build_dual_cyto_data(isolate(rv$g),
                               gap_v = ly$gap_v %||% 18, gap_col = ly$gap_col %||% 400,
                               font_node = ly$font_node %||% 12, font_ptype = ly$font_ptype %||% 12,
                               font_subs = ly$font_subs %||% 15, font_desc = ly$font_desc %||% 18,
                               font_hdr1 = ly$font_hdr1 %||% 22, font_hdr2 = ly$font_hdr2 %||% 15,
                               h_theme = ly$h_theme %||% 46, h_project = ly$h_project %||% 66, h_skill = ly$h_skill %||% 46,
                               w_project = ly$w_project %||% NODE_W$Project, w_node = ly$w_node,
                               inline_mode = isTRUE(ly$inline_mode %||% TRUE),
                               articles_enabled = isTRUE(ly$articles_enabled %||% FALSE),
                               watermark_text = ly$watermark_text %||% "", watermark_size = ly$watermark_size %||% 10,
                               col_bg         = dc[["col_bg"]]         %||% ly$col_bg         %||% "#0b3552",
                               col_sidebar_bg = dc[["col_sidebar_bg"]] %||% ly$col_sidebar_bg %||% "#081626",
                               col_node_bg    = dc[["col_node_bg"]]    %||% ly$col_node_bg    %||% "#081626",
                               col_theme      = dc[["col_theme"]]      %||% ly$col_theme      %||% "#3be37a",
                               col_project    = dc[["col_project"]]    %||% ly$col_project    %||% "#ffad33",
                               col_skill      = dc[["col_skill"]]      %||% ly$col_skill      %||% "#78e6e7",
                               light_edge_color = ly$light_edge_color %||% "#555555",
                               mob_font_mult    = ly$mob_font_mult    %||% MOBILE_DEFAULTS$mob_font_mult,
                               mob_h_theme_mult = ly$mob_h_theme_mult %||% MOBILE_DEFAULTS$mob_h_theme_mult,
                               mob_h_proj_mult  = ly$mob_h_proj_mult  %||% MOBILE_DEFAULTS$mob_h_proj_mult,
                               mob_h_skill_mult = ly$mob_h_skill_mult %||% MOBILE_DEFAULTS$mob_h_skill_mult,
                               mob_gap_v_mult   = ly$mob_gap_v_mult   %||% MOBILE_DEFAULTS$mob_gap_v_mult,
                               mob_gap_col_mult = ly$mob_gap_col_mult %||% MOBILE_DEFAULTS$mob_gap_col_mult,
                               hdr_theme_line1=rv_col$hdr_theme_line1, hdr_theme_line2=rv_col$hdr_theme_line2,
                               hdr_project_line1=rv_col$hdr_project_line1, hdr_project_line2=rv_col$hdr_project_line2,
                               hdr_skill_line1=rv_col$hdr_skill_line1, hdr_skill_line2=rv_col$hdr_skill_line2,
                               fi_hdr_theme_line1=rv_col$fi_hdr_theme_line1, fi_hdr_theme_line2=rv_col$fi_hdr_theme_line2,
                               fi_hdr_project_line1=rv_col$fi_hdr_project_line1, fi_hdr_project_line2=rv_col$fi_hdr_project_line2,
                               fi_hdr_skill_line1=rv_col$fi_hdr_skill_line1, fi_hdr_skill_line2=rv_col$fi_hdr_skill_line2)
    session$sendCustomMessage("setInlineMode", list(value = isTRUE(input$inline_mode %||% (ly$inline_mode %||% TRUE))))
    session$sendCustomMessage("setArticlesEnabled", list(value = isTRUE(input$articles_enabled %||% (ly$articles_enabled %||% FALSE))))
    session$sendCustomMessage("setAccordionIcon", list(style = input$accordion_icon %||% (ly$accordion_icon %||% "triangle")))
    session$sendCustomMessage("setAccordionIconSize", list(size = input$accordion_icon_size %||% (ly$accordion_icon_size %||% 14)))
    session$sendCustomMessage("setAuthorEditable", list(value = isTRUE(input$edit_inline_text %||% TRUE)))
    session$sendCustomMessage("initCy", cd)
    session$sendCustomMessage("updateAccTitles", list(
      details_title = rv_col$details_title, intro_title = rv_col$intro_title,
      vote_title = rv_col$vote_title, fund_title = rv_col$fund_title
    ))
    session$sendCustomMessage("setLanguageData", list(
      page_title_en    = "My interests - Ville Sepp\u00e4l\u00e4",
      page_title_fi    = rv_col$fi_page_title    %||% "",
      details_title_fi = rv_col$fi_details_title %||% "",
      intro_title_fi   = rv_col$fi_intro_title   %||% "",
      vote_title_fi    = rv_col$fi_vote_title    %||% "",
      fund_title_fi    = rv_col$fi_fund_title    %||% ""
    ))
    if (length(defs$hsv)) {
      hsv <- defs$hsv
      rng_ids <- c("rnd_h_bg","rnd_s_bg","rnd_v_bg","rnd_h_sb","rnd_s_sb","rnd_v_sb",
                   "rnd_h_nb","rnd_s_nb","rnd_v_nb",
                   "rnd_h_theme","rnd_h_proj","rnd_h_skill","rnd_s_accent","rnd_v_accent")
      for (nm in rng_ids) { v <- hsv[[nm]]; if (length(v) == 2) updateSliderInput(session, nm, value = v) }
      if (!is.null(hsv$hue_dist))   updateSliderInput(session, "hue_dist",   value = hsv$hue_dist)
      if (!is.null(hsv$max_s_dist)) updateSliderInput(session, "max_s_dist", value = hsv$max_s_dist)
      if (!is.null(hsv$max_v_dist)) updateSliderInput(session, "max_v_dist", value = hsv$max_v_dist)
      for (nm in c("inv_rnd_h_bg","inv_rnd_h_sb","inv_rnd_h_nb","inv_rnd_h_theme","inv_rnd_h_proj","inv_rnd_h_skill")) {
        if (!is.null(hsv[[nm]])) updateCheckboxInput(session, nm, value = isTRUE(hsv[[nm]]))
      }
    }
    if (length(defs$light_hsv)) {
      lhsv <- defs$light_hsv
      lrng_ids <- c("lrnd_h_bg","lrnd_s_bg","lrnd_v_bg","lrnd_h_sb","lrnd_s_sb","lrnd_v_sb",
                    "lrnd_h_nb","lrnd_s_nb","lrnd_v_nb",
                    "lrnd_h_theme","lrnd_h_proj","lrnd_h_skill","lrnd_s_accent","lrnd_v_accent")
      for (nm in lrng_ids) { v <- lhsv[[nm]]; if (length(v) == 2) updateSliderInput(session, nm, value = v) }
      if (!is.null(lhsv$lhue_dist))   updateSliderInput(session, "lhue_dist",   value = lhsv$lhue_dist)
      if (!is.null(lhsv$lmax_s_dist)) updateSliderInput(session, "lmax_s_dist", value = lhsv$lmax_s_dist)
      if (!is.null(lhsv$lmax_v_dist)) updateSliderInput(session, "lmax_v_dist", value = lhsv$lmax_v_dist)
      for (nm in c("inv_lrnd_h_bg","inv_lrnd_h_sb","inv_lrnd_h_nb","inv_lrnd_h_theme","inv_lrnd_h_proj","inv_lrnd_h_skill")) {
        if (!is.null(lhsv[[nm]])) updateCheckboxInput(session, nm, value = isTRUE(lhsv[[nm]]))
      }
    }
    if (length(defs$lec_hsv)) {
      lec <- defs$lec_hsv
      for (nm in c("lec_h_theme", "lec_h_skill", "lec_s_theme", "lec_v_theme", "lec_s_skill", "lec_v_skill")) { v <- lec[[nm]]; if (length(v) == 2) updateSliderInput(session, nm, value = v) }
      for (nm in c("inv_lec_h_theme","inv_lec_h_skill")) {
        if (!is.null(lec[[nm]])) updateCheckboxInput(session, nm, value = isTRUE(lec[[nm]]))
      }
    }
    if (length(defs$ec_hsv)) {
      ec <- defs$ec_hsv
      for (nm in c("ec_h_theme","ec_h_skill","ec_s_theme","ec_v_theme","ec_s_skill","ec_v_skill")) {
        v <- ec[[nm]]; if (length(v) == 2) updateSliderInput(session, nm, value = v)
      }
      for (nm in c("ec_hue_dist_theme","ec_hue_dist_skill","ec_max_s_dist","ec_max_v_dist","ec_node_dist")) {
        v <- ec[[nm]]; if (!is.null(v)) updateSliderInput(session, nm, value = v)
      }
      for (nm in c("inv_ec_h_theme","inv_ec_h_skill")) {
        if (!is.null(ec[[nm]])) updateCheckboxInput(session, nm, value = isTRUE(ec[[nm]]))
      }
    }
    # Apply saved colors if present, otherwise randomize for fresh palette
    if (!is.null(defs$colors) && !is.null(defs$colors[["col_bg"]])) {
      dcols <- as.list(defs$colors)
      set_colors(dcols)
      updateCheckboxInput(session, "one_color", value = isTRUE(dcols[["one_color"]]))
    } else {
      do_randomize(if (length(defs$hsv)) defs$hsv else list())
    }
  })

  observe({
    ly  <- isolate(rv$g$layout)
    # Prefer the light colors saved WITH this graph (graph.json / "Save JSON"), then fall back to the
    # author's saved light-color defaults, then hard defaults — so Save JSON is remembered on reload.
    dlc <- as.list(read_defaults()$light_colors)
    lct <- ly$light_col_theme   %||% dlc$light_col_theme   %||% "#1e7c45"
    lcp <- ly$light_col_project %||% dlc$light_col_project %||% "#c06000"
    lcs <- ly$light_col_skill   %||% dlc$light_col_skill   %||% "#1a7a7b"
    lc <- list(
      light_col_bg         = ly$light_col_bg         %||% dlc$light_col_bg         %||% "#f0f4f8",
      light_col_sidebar_bg = ly$light_col_sidebar_bg %||% dlc$light_col_sidebar_bg %||% "#e2eaf3",
      light_col_node_bg    = ly$light_col_node_bg    %||% dlc$light_col_node_bg    %||% "#e2eaf3",
      light_col_theme      = lct, light_col_project = lcp, light_col_skill = lcs,
      light_edge_color     = ly$light_edge_color     %||% dlc$light_edge_color     %||% "#555555"
    )
    # One-color mode is on when the three node colors are identical (derived, so it survives Save JSON).
    one <- identical(tolower(lct), tolower(lcp)) && identical(tolower(lcp), tolower(lcs))
    lc$light_one_color <- one
    lc$light_col_all   <- lct
    set_light_colors(lc)
    updateCheckboxInput(session, "light_one_color", value = one)
  })

  # Subsequent updates driven by slider changes
  observeEvent(cyto_data(), {
    # An inline title edit already updated the label client-side; don't rebuild the whole graph
    # (which would collapse expanded nodes and reset the view).
    if (isTRUE(rv$skip_cy_rebuild)) { rv$skip_cy_rebuild <- FALSE; return() }
    session$sendCustomMessage("updateCy", cyto_data())
  }, ignoreInit = TRUE)
  
  # Edge width (JS-only, no layout recompute)
  observeEvent(input$edge_bands, {
    session$sendCustomMessage("setEdgeBands", list(value = isTRUE(input$edge_bands)))
  }, ignoreInit = TRUE)
  observeEvent(input$edge_sankey, {
    session$sendCustomMessage("setEdgeSankey", list(value = isTRUE(input$edge_sankey)))
  }, ignoreInit = TRUE)
  observeEvent(input$edge_transparency, {
    session$sendCustomMessage("setEdgeTransparency", list(pct = input$edge_transparency))
  }, ignoreInit = TRUE)
  observeEvent(input$edge_min_width, {
    session$sendCustomMessage("setEdgeMinWidth", list(width = input$edge_min_width))
  }, ignoreInit = TRUE)
  observeEvent(input$edge_min_on, {
    session$sendCustomMessage("setEdgeMinOn", list(value = isTRUE(input$edge_min_on)))
  }, ignoreInit = TRUE)
  observeEvent(input$edge_curve, {
    session$sendCustomMessage("setEdgeCurve", list(exp = input$edge_curve))
  }, ignoreInit = TRUE)
  observeEvent(input$edge_pin_header, {
    session$sendCustomMessage("setEdgePinHeader", list(value = isTRUE(input$edge_pin_header)))
  }, ignoreInit = TRUE)
  observeEvent(input$edge_gap, {
    session$sendCustomMessage("setEdgeGap", list(gap = input$edge_gap))
  }, ignoreInit = TRUE)
  observeEvent(input$fill_nodew, {
    session$sendCustomMessage("setFillNodeW", list(pct = input$fill_nodew))
  }, ignoreInit = TRUE)
  observeEvent(input$fill_projw, {
    session$sendCustomMessage("setFillProjW", list(pct = input$fill_projw))
  }, ignoreInit = TRUE)
  observeEvent(input$fill_colgap, {
    session$sendCustomMessage("setFillColGap", list(pct = input$fill_colgap))
  }, ignoreInit = TRUE)
  observeEvent(input$edge_width, {
    session$sendCustomMessage("setEdgeWidth", list(width = input$edge_width))
  }, ignoreInit = TRUE)

  # Gradient extent (JS-only, no layout recompute)
  observeEvent(input$gradient_extent, {
    session$sendCustomMessage("setGradientExtent", list(pct = input$gradient_extent))
  }, ignoreInit = TRUE)

  # Gradient transparency of the inside fill (JS-only, no layout recompute)
  observeEvent(input$gradient_transparency, {
    session$sendCustomMessage("setGradientTransparency", list(pct = input$gradient_transparency))
  }, ignoreInit = TRUE)

  # Gradient falloff curve (JS-only, no layout recompute)
  observeEvent(input$gradient_curve, {
    session$sendCustomMessage("setGradientCurve", list(curve = input$gradient_curve))
  }, ignoreInit = TRUE)

  # Gradient extent on hover (JS-only, no layout recompute)
  observeEvent(input$gradient_hover_mult, {
    session$sendCustomMessage("setGradientHoverMult", list(mult = input$gradient_hover_mult))
  }, ignoreInit = TRUE)

  # Node outline width (JS-only, no layout recompute)
  observeEvent(input$node_outline, {
    session$sendCustomMessage("setNodeOutline", list(width = input$node_outline))
  }, ignoreInit = TRUE)
  observeEvent(input$project_outline, {
    session$sendCustomMessage("setProjectOutline", list(width = input$project_outline))
  }, ignoreInit = TRUE)
  observeEvent(input$outline_saturation, {
    session$sendCustomMessage("setOutlineSaturation", list(value = input$outline_saturation))
  }, ignoreInit = TRUE)
  observeEvent(input$outline_transparency, {
    session$sendCustomMessage("setOutlineTransparency", list(pct = input$outline_transparency))
  }, ignoreInit = TRUE)
  observeEvent(input$node_pad, {
    session$sendCustomMessage("setNodePad", list(px = input$node_pad))
  }, ignoreInit = TRUE)
  observeEvent(input$desc_pad, {
    session$sendCustomMessage("setDescPad", list(px = input$desc_pad))
  }, ignoreInit = TRUE)

  # "One color" mode: drive Theme/Project/Skill from a single picker. Pushing the value into the
  # three actual color inputs keeps the live view, saving and export all consistent.
  observeEvent(list(input$one_color, input$col_all), {
    if (isTRUE(input$one_color)) {
      ca <- input$col_all %||% "#3be37a"
      session$sendCustomMessage("setColorInputs",
                                list(col_theme = ca, col_project = ca, col_skill = ca))
    }
  }, ignoreInit = TRUE)

  # "One color" mode for light theme
  observeEvent(list(input$light_one_color, input$light_col_all), {
    if (isTRUE(input$light_one_color)) {
      ca <- input$light_col_all %||% "#1e7c45"
      session$sendCustomMessage("setColorInputs",
                                list(light_col_theme = ca, light_col_project = ca, light_col_skill = ca))
    }
  }, ignoreInit = TRUE)

  observeEvent(list(input$ptype_pct, input$w_project), {
    session$sendCustomMessage("setPtypeLayout", list(
      ptypePct = input$ptype_pct %||% 21,
      projectNodeWidth = input$w_project %||% NODE_W$Project
    ))
  }, ignoreInit = TRUE)

  # Toggle mobile preview mode
  preview_dims <- reactive({
    dev <- input$preview_device %||% "390x844"
    if (dev == "custom") {
      list(w = input$preview_w %||% 390, h = input$preview_h %||% 844)
    } else {
      parts <- strsplit(dev, "x")[[1]]
      list(w = as.integer(parts[1]), h = as.integer(parts[2]))
    }
  })
  
  send_preview <- function() {
    on <- isTRUE(input$preview_mobile)
    dims <- preview_dims()
    session$sendCustomMessage("setForceMobile", list(
      value = on, width = dims$w, height = dims$h
    ))
  }
  
  observeEvent(input$preview_mobile, { send_preview() }, ignoreInit = TRUE)
  observeEvent(input$preview_device, { if (isTRUE(input$preview_mobile)) send_preview() }, ignoreInit = TRUE)
  observeEvent(input$preview_w,      { if (isTRUE(input$preview_mobile)) send_preview() }, ignoreInit = TRUE)
  observeEvent(input$preview_h,      { if (isTRUE(input$preview_mobile)) send_preview() }, ignoreInit = TRUE)
  
  # Set layout slider values from loaded graph on startup
  observe({
    ly <- isolate(rv$g$layout)
    updateSliderInput(session, "gap_v",      value = ly$gap_v      %||% 18)
    updateSliderInput(session, "gap_col",    value = ly$gap_col    %||% 400)
    updateSliderInput(session, "font_hdr1",  value = ly$font_hdr1  %||% 22)
    updateSliderInput(session, "font_hdr2",  value = ly$font_hdr2  %||% 15)
    updateSliderInput(session, "font_node",  value = ly$font_node  %||% 12)
    updateSliderInput(session, "font_ptype", value = ly$font_ptype %||% 12)
    updateSliderInput(session, "font_subs",  value = ly$font_subs  %||% 15)
    updateSliderInput(session, "font_desc",  value = ly$font_desc  %||% 18)
    updateSliderInput(session, "h_theme",    value = ly$h_theme    %||% 46)
    updateSliderInput(session, "h_project",  value = ly$h_project  %||% 66)
    updateSliderInput(session, "h_skill",    value = ly$h_skill    %||% 46)
    updateSliderInput(session, "w_project",  value = ly$w_project  %||% NODE_W$Project)
    updateSliderInput(session, "w_node",     value = ly$w_node %||% ly$w_project %||% NODE_W$Project)
    updateSliderInput(session, "ptype_pct",  value = ly$ptype_pct  %||% 21)
    updateSliderInput(session, "edge_width", value = ly$edge_width %||% 2.5)
    updateCheckboxInput(session, "edge_bands", value = isTRUE(ly$edge_bands %||% TRUE))
    updateCheckboxInput(session, "edge_sankey", value = isTRUE(ly$edge_sankey %||% FALSE))
    updateSliderInput(session, "edge_gap", value = ly$edge_gap %||% 3)
    updateSliderInput(session, "edge_transparency", value = ly$edge_transparency %||% 18)
    updateSliderInput(session, "edge_min_width", value = ly$edge_min_width %||% 2.5)
    updateCheckboxInput(session, "edge_min_on", value = isTRUE(ly$edge_min_on %||% TRUE))
    updateSliderInput(session, "edge_curve", value = ly$edge_curve %||% 1)
    updateCheckboxInput(session, "edge_pin_header", value = isTRUE(ly$edge_pin_header %||% FALSE))
    updateSliderInput(session, "fill_nodew", value = ly$fill_nodew %||% 0)
    updateSliderInput(session, "fill_projw", value = ly$fill_projw %||% 0)
    updateSliderInput(session, "fill_colgap", value = ly$fill_colgap %||% 0)
    updateSliderInput(session, "gradient_extent", value = ly$gradient_extent %||% 20)
    updateSliderInput(session, "gradient_transparency", value = ly$gradient_transparency %||% 40)
    updateSliderInput(session, "gradient_curve", value = ly$gradient_curve %||% 1)
    updateSliderInput(session, "gradient_hover_mult", value = ly$gradient_hover_mult %||% 2)
    updateSliderInput(session, "node_outline", value = ly$node_outline %||% 3)
    updateSliderInput(session, "project_outline", value = ly$project_outline %||% ly$node_outline %||% 3)
    updateSliderInput(session, "outline_saturation", value = ly$outline_saturation %||% 1)
    updateSliderInput(session, "outline_transparency", value = ly$outline_transparency %||% 0)
    updateSliderInput(session, "node_pad", value = ly$node_pad %||% 0)
    updateSliderInput(session, "desc_pad", value = ly$desc_pad %||% 10)
    updateCheckboxInput(session, "inline_mode", value = isTRUE(ly$inline_mode %||% TRUE))
    updateCheckboxInput(session, "articles_enabled", value = isTRUE(ly$articles_enabled %||% FALSE))
    updateSelectInput(session, "accordion_icon", selected = ly$accordion_icon %||% "triangle")
    updateSliderInput(session, "accordion_icon_size", value = ly$accordion_icon_size %||% 14)
    if (nzchar(ly$watermark_text %||% "")) {
      updateCheckboxInput(session, "show_watermark", value = TRUE)
      updateTextInput(session, "watermark_text", value = ly$watermark_text)
    }
    updateSliderInput(session, "watermark_size", value = ly$watermark_size %||% 10)
    # Mobile multiplier sliders
    updateSliderInput(session, "mob_font_mult",    value = ly$mob_font_mult    %||% MOBILE_DEFAULTS$mob_font_mult)
    updateSliderInput(session, "mob_h_theme_mult", value = ly$mob_h_theme_mult %||% MOBILE_DEFAULTS$mob_h_theme_mult)
    updateSliderInput(session, "mob_h_proj_mult",  value = ly$mob_h_proj_mult  %||% MOBILE_DEFAULTS$mob_h_proj_mult)
    updateSliderInput(session, "mob_h_skill_mult", value = ly$mob_h_skill_mult %||% MOBILE_DEFAULTS$mob_h_skill_mult)
    updateSliderInput(session, "mob_gap_v_mult",   value = ly$mob_gap_v_mult   %||% MOBILE_DEFAULTS$mob_gap_v_mult)
    updateSliderInput(session, "mob_gap_col_mult", value = ly$mob_gap_col_mult %||% MOBILE_DEFAULTS$mob_gap_col_mult)
    # Sync column tab inputs
    updateTextInput(session,    "col_intro_title", value = rv_col$intro_title)
    updateTextAreaInput(session, "col_intro_text", value = rv_col$intro_text)
    updateTextInput(session,    "details_title",   value = rv_col$details_title)
    updateTextInput(session,    "details_hint",    value = rv_col$details_hint)
    updateTextInput(session,    "vote_title",      value = rv_col$vote_title)
    updateTextAreaInput(session, "vote_text",      value = rv_col$vote_text)
    updateTextInput(session,    "funding_title",   value = rv_col$fund_title)
    updateTextAreaInput(session, "funding_intro",  value = rv_col$fund_intro)
    updateTextAreaInput(session, "funding_items",  value = paste(rv_col$fund_items, collapse = "\n"))
    updateTextInput(session,    "fi_page_title",      value = rv_col$fi_page_title)
    updateTextInput(session,    "fi_col_intro_title", value = rv_col$fi_intro_title)
    updateTextAreaInput(session, "fi_col_intro_text", value = rv_col$fi_intro_text)
    updateTextInput(session,    "fi_details_title",   value = rv_col$fi_details_title)
    updateTextInput(session,    "fi_details_hint",    value = rv_col$fi_details_hint)
    updateTextInput(session,    "fi_vote_title",      value = rv_col$fi_vote_title)
    updateTextAreaInput(session, "fi_vote_text",      value = rv_col$fi_vote_text)
    updateTextInput(session,    "fi_funding_title",   value = rv_col$fi_fund_title)
    updateTextAreaInput(session, "fi_funding_intro",  value = rv_col$fi_fund_intro)
    # Sync header tab inputs
    updateTextInput(session, "hdr_theme_line1",      value = rv_col$hdr_theme_line1   %||% "Themes")
    updateTextInput(session, "hdr_theme_line2",      value = rv_col$hdr_theme_line2   %||% "I want to focus on")
    updateTextInput(session, "hdr_project_line1",    value = rv_col$hdr_project_line1 %||% "Projects")
    updateTextInput(session, "hdr_project_line2",    value = rv_col$hdr_project_line2 %||% "I\u2019m working on or want to work on")
    updateTextInput(session, "hdr_skill_line1",      value = rv_col$hdr_skill_line1   %||% "Skills")
    updateTextInput(session, "hdr_skill_line2",      value = rv_col$hdr_skill_line2   %||% "I have or want to develop")
    updateTextInput(session, "fi_hdr_theme_line1",   value = rv_col$fi_hdr_theme_line1   %||% "")
    updateTextInput(session, "fi_hdr_theme_line2",   value = rv_col$fi_hdr_theme_line2   %||% "")
    updateTextInput(session, "fi_hdr_project_line1", value = rv_col$fi_hdr_project_line1 %||% "")
    updateTextInput(session, "fi_hdr_project_line2", value = rv_col$fi_hdr_project_line2 %||% "")
    updateTextInput(session, "fi_hdr_skill_line1",   value = rv_col$fi_hdr_skill_line1   %||% "")
    updateTextInput(session, "fi_hdr_skill_line2",   value = rv_col$fi_hdr_skill_line2   %||% "")
  })
  
  output$node_select_ui <- renderUI({
    nd <- nodes_df()
    if (!nrow(nd)) return(selectInput("node_id", "Select node", choices = character(0), width = "100%"))
    nd <- nd[order(nd$id), , drop = FALSE]; sel <- rv$selected_id %||% nd$id[1]
    selectInput("node_id", "Select node",
                choices = setNames(nd$id, paste0(nd$id, " | ", nd$group, " | ", nd$title)),
                selected = sel, width = "100%")
  })
  
  # Dynamic column sidebar — load from saved layout
  rv_col <- reactiveValues(
    intro_title   = loaded_g$layout$col_intro_title %||% "What is this site about",
    intro_text    = loaded_g$layout$col_intro_text  %||% "",
    details_title = loaded_g$layout$details_title   %||% "Details",
    details_hint  = loaded_g$layout$details_hint    %||% "Click on a topic in the graph to see details here",
    vote_title    = loaded_g$layout$vote_title      %||% "Vote",
    vote_text     = loaded_g$layout$vote_text       %||% "Vote for themes, projects and skills you\u2019d like me to focus on:",
    fund_title    = loaded_g$layout$funding_title   %||% "Funding",
    fund_intro    = loaded_g$layout$funding_intro   %||% "Current preference order to fund work on something related to the themes, projects and skills presented, when not working on my own time on them:",
    fund_items    = as.character(unlist(loaded_g$layout$funding_items %||% FUNDING_ITEMS)),
    # Column headers
    hdr_theme_line1   = loaded_g$layout$hdr_theme_line1   %||% "Themes",
    hdr_theme_line2   = loaded_g$layout$hdr_theme_line2   %||% "I want to focus on",
    hdr_project_line1 = loaded_g$layout$hdr_project_line1 %||% "Projects",
    hdr_project_line2 = loaded_g$layout$hdr_project_line2 %||% "I\u2019m working on or want to work on",
    hdr_skill_line1   = loaded_g$layout$hdr_skill_line1   %||% "Skills",
    hdr_skill_line2   = loaded_g$layout$hdr_skill_line2   %||% "I have or want to develop",
    fi_hdr_theme_line1   = loaded_g$layout$fi_hdr_theme_line1   %||% "",
    fi_hdr_theme_line2   = loaded_g$layout$fi_hdr_theme_line2   %||% "",
    fi_hdr_project_line1 = loaded_g$layout$fi_hdr_project_line1 %||% "",
    fi_hdr_project_line2 = loaded_g$layout$fi_hdr_project_line2 %||% "",
    fi_hdr_skill_line1   = loaded_g$layout$fi_hdr_skill_line1   %||% "",
    fi_hdr_skill_line2   = loaded_g$layout$fi_hdr_skill_line2   %||% "",
    # Finnish translations
    fi_page_title    = loaded_g$layout$fi_page_title    %||% "",
    fi_intro_title   = loaded_g$layout$fi_col_intro_title %||% "",
    fi_intro_text    = loaded_g$layout$fi_col_intro_text  %||% "",
    fi_details_title = loaded_g$layout$fi_details_title  %||% "",
    fi_details_hint  = loaded_g$layout$fi_details_hint   %||% "",
    fi_vote_title    = loaded_g$layout$fi_vote_title     %||% "",
    fi_vote_text     = loaded_g$layout$fi_vote_text      %||% "",
    fi_fund_title    = loaded_g$layout$fi_funding_title  %||% "",
    fi_fund_intro    = loaded_g$layout$fi_funding_intro  %||% ""
  )

  output$col_intro_ui <- renderUI({
    en <- rv_col$intro_text %||% ""
    fi <- rv_col$fi_intro_text %||% ""
    if (!nzchar(trimws(en)) && !nzchar(trimws(fi))) return(NULL)
    txt_fi <- if (nzchar(trimws(fi))) fi else en
    style_str <- "color:rgba(255,255,255,0.8);font-family:Arial,Helvetica,sans-serif;font-size:var(--desc-font);line-height:1.65;margin-bottom:12px;"
    tagList(
      if (nzchar(trimws(en))) div(class = "en-only", style = style_str, linkify(en)),
      div(class = "fi-only", style = style_str, linkify(txt_fi))
    )
  })

  output$sidebar_hint_ui <- renderUI({
    div(id = "sidebar-hint",
        span(class = "en-only", linkify(rv_col$details_hint %||% "")),
        span(class = "fi-only", linkify(rv_col$fi_details_hint %||% rv_col$details_hint %||% ""))
    )
  })

  output$vote_section_ui <- renderUI({
    en <- rv_col$vote_text %||% ""
    vtext_fi <- if (nzchar(trimws(rv_col$fi_vote_text %||% ""))) rv_col$fi_vote_text else en
    div(id = "vote-section",
        tags$div(class = "en-only", style = "color:rgba(255,255,255,0.8);font-family:Arial,Helvetica,sans-serif;line-height:1.65;", linkify(en)),
        tags$div(class = "fi-only", style = "color:rgba(255,255,255,0.8);font-family:Arial,Helvetica,sans-serif;line-height:1.65;", linkify(vtext_fi))
    )
  })

  output$funding_ui <- renderUI({
    html_items <- vapply(rv_col$fund_items, function(line) {
      stripped <- sub("^( +)", "", line)
      n_spaces <- nchar(line) - nchar(stripped)
      paste0(strrep("&nbsp;", n_spaces * 3), stripped)
    }, character(1), USE.NAMES = FALSE)
    fi_intro_eff <- if (nzchar(trimws(rv_col$fi_fund_intro %||% ""))) rv_col$fi_fund_intro else rv_col$fund_intro
    div(id = "funding-section",
        tags$div(class = "funding-body",
                 tags$div(class = "en-only", style = "margin-bottom:8px;line-height:1.6;", linkify(rv_col$fund_intro)),
                 tags$div(class = "fi-only", style = "margin-bottom:8px;line-height:1.6;", linkify(fi_intro_eff)),
                 tags$div(style = "line-height:1.7;", HTML(paste(html_items, collapse = "<br>")))
        )
    )
  })

  observeEvent(input$apply_column, {
    rv_col$intro_title   <- input$col_intro_title %||% "What is this site about"
    rv_col$intro_text    <- input$col_intro_text  %||% ""
    rv_col$details_title <- input$details_title   %||% "Details"
    rv_col$details_hint  <- input$details_hint    %||% "Click on a topic in the graph to see details here"
    rv_col$vote_title    <- input$vote_title      %||% "Vote"
    rv_col$vote_text     <- input$vote_text       %||% ""
    rv_col$fund_title    <- input$funding_title   %||% "Funding"
    rv_col$fund_intro    <- input$funding_intro   %||% ""
    lines <- strsplit(input$funding_items %||% "", "\n", fixed = TRUE)[[1]]
    rv_col$fund_items    <- lines[nzchar(trimws(lines))]
    rv_col$fi_page_title    <- input$fi_page_title       %||% ""
    rv_col$fi_intro_title   <- input$fi_col_intro_title  %||% ""
    rv_col$fi_intro_text    <- input$fi_col_intro_text   %||% ""
    rv_col$fi_details_title <- input$fi_details_title    %||% ""
    rv_col$fi_details_hint  <- input$fi_details_hint     %||% ""
    rv_col$fi_vote_title    <- input$fi_vote_title       %||% ""
    rv_col$fi_vote_text     <- input$fi_vote_text        %||% ""
    rv_col$fi_fund_title    <- input$fi_funding_title    %||% ""
    rv_col$fi_fund_intro    <- input$fi_funding_intro    %||% ""
    session$sendCustomMessage("updateAccTitles", list(
      details_title = rv_col$details_title, intro_title = rv_col$intro_title,
      vote_title = rv_col$vote_title, fund_title = rv_col$fund_title
    ))
    session$sendCustomMessage("setLanguageData", list(
      page_title_en    = "My interests - Ville Sepp\u00e4l\u00e4",
      page_title_fi    = rv_col$fi_page_title    %||% "",
      details_title_fi = rv_col$fi_details_title %||% "",
      intro_title_fi   = rv_col$fi_intro_title   %||% "",
      vote_title_fi    = rv_col$fi_vote_title    %||% "",
      fund_title_fi    = rv_col$fi_fund_title    %||% ""
    ))
    showNotification("Column section updated.", type = "message")
  })

  observeEvent(input$apply_headers, {
    rv_col$hdr_theme_line1   <- input$hdr_theme_line1   %||% "Themes"
    rv_col$hdr_theme_line2   <- input$hdr_theme_line2   %||% "I want to focus on"
    rv_col$hdr_project_line1 <- input$hdr_project_line1 %||% "Projects"
    rv_col$hdr_project_line2 <- input$hdr_project_line2 %||% "I\u2019m working on or want to work on"
    rv_col$hdr_skill_line1   <- input$hdr_skill_line1   %||% "Skills"
    rv_col$hdr_skill_line2   <- input$hdr_skill_line2   %||% "I have or want to develop"
    rv_col$fi_hdr_theme_line1   <- input$fi_hdr_theme_line1   %||% ""
    rv_col$fi_hdr_theme_line2   <- input$fi_hdr_theme_line2   %||% ""
    rv_col$fi_hdr_project_line1 <- input$fi_hdr_project_line1 %||% ""
    rv_col$fi_hdr_project_line2 <- input$fi_hdr_project_line2 %||% ""
    rv_col$fi_hdr_skill_line1   <- input$fi_hdr_skill_line1   %||% ""
    rv_col$fi_hdr_skill_line2   <- input$fi_hdr_skill_line2   %||% ""
    showNotification("Column headers updated.", type = "message")
  })

  output$edge_from_ui <- renderUI({
    nd <- nodes_df(); nd <- nd[order(nd$id), ]
    selectInput("edge_from", "From",
                choices = setNames(nd$id, paste0(nd$id, " | ", nd$group, " | ", nd$title)), width = "100%")
  })
  output$edge_to_ui <- renderUI({
    nd <- nodes_df(); nd <- nd[order(nd$id), ]
    selectInput("edge_to", "To",
                choices = setNames(nd$id, paste0(nd$id, " | ", nd$group, " | ", nd$title)), width = "100%")
  })
  output$edge_remove_ui <- renderUI({
    ed <- edges_df()
    if (!nrow(ed)) return(selectInput("edge_key", "Select edge", choices = character(0), width = "100%"))
    selectInput("edge_key", "Select edge", choices = sort(paste0(ed$from, " -> ", ed$to)), width = "100%")
  })
  
  # ── Source node color selector: Theme and Skill nodes ──
  output$source_color_node_ui <- renderUI({
    nd <- nodes_df()
    ts <- nd[nd$group %in% c("Theme", "Skill"), , drop = FALSE]
    ts <- ts[order(as.numeric(ts$id)), , drop = FALSE]
    if (!nrow(ts)) return(selectInput("source_color_node", "Node", choices = character(0), width = "100%"))
    # Show current edgeColor in label
    labels <- vapply(seq_len(nrow(ts)), function(i) {
      nid <- ts$id[i]
      ec <- ""
      for (n in rv$g$nodes) {
        if (as.numeric(n$id) == nid) { ec <- n$edgeColor %||% ""; break }
      }
      ec_str <- if (nzchar(ec)) paste0(" [", ec, "]") else ""
      paste0(ts$id[i], " | ", ts$group[i], " | ", ts$title[i], ec_str)
    }, character(1))
    selectInput("source_color_node", "Node", choices = setNames(ts$id, labels), width = "100%")
  })
  
  all_palettes <- reactive({
    c(rev(rv$custom_palettes), PALETTES)  # custom newest-first, then originals
  })

  # Update dropdown choices without resetting selection (preserves user's current pick)
  observe({
    pal <- all_palettes()
    nms <- vapply(pal, function(p) p$name, character(1))
    cur <- isolate(input$palette_choice)
    sel <- if (isTruthy(cur) && cur %in% nms) cur else nms[1]
    updateSelectInput(session, "palette_choice", choices = setNames(nms, nms), selected = sel)
  })

  observeEvent(input$palette_choice, {
    # Fire #1 is always the choices-reset (empty value) — skip without incrementing counter
    if (!nzchar(input$palette_choice %||% "")) return()
    cnt <- isolate(rv$palette_fire_count) + 1L
    rv$palette_fire_count <- cnt
    # Fire #2 is the selected= init from updateSelectInput — skip it too
    if (cnt == 1L) return()
    pal <- all_palettes()
    p <- Filter(function(x) x$name == input$palette_choice, pal)
    if (!length(p)) return()
    p <- p[[1]]
    set_colors(list(
      col_bg = p$col_bg, col_sidebar_bg = p$col_sidebar_bg,
      col_theme = p$col_theme, col_project = p$col_project, col_skill = p$col_skill
    ))
  })

  # ── Light color palette system ───────────────────────────────────────────────
  all_light_palettes <- reactive({
    c(rev(rv$light_custom_palettes), LIGHT_PALETTES)
  })
  observe({
    pal <- all_light_palettes()
    nms <- vapply(pal, function(p) p$name, character(1))
    cur <- isolate(input$light_palette_choice)
    sel <- if (isTruthy(cur) && cur %in% nms) cur else nms[1]
    updateSelectInput(session, "light_palette_choice", choices = setNames(nms, nms), selected = sel)
  })
  observeEvent(input$light_palette_choice, {
    if (!nzchar(input$light_palette_choice %||% "")) return()
    cnt <- isolate(rv$light_palette_fire_count) + 1L
    rv$light_palette_fire_count <- cnt
    if (cnt == 1L) return()
    pal <- all_light_palettes()
    p <- Filter(function(x) x$name == input$light_palette_choice, pal)
    if (!length(p)) return()
    p <- p[[1]]
    set_light_colors(list(
      light_col_bg = p$light_col_bg, light_col_sidebar_bg = p$light_col_sidebar_bg,
      light_col_theme = p$light_col_theme, light_col_project = p$light_col_project,
      light_col_skill = p$light_col_skill
    ))
  })

  # Format the H/S/V ranges actually produced by a randomization (h in 0-360, s/v in 0-100).
  fmt_hsv_range <- function(h, s, v) {
    if (!length(h)) return("(nothing randomized)")
    rng <- function(x, unit) {
      lo <- round(min(x)); hi <- round(max(x))
      if (lo == hi) paste0(lo, unit) else paste0(lo, "-", hi, unit)
    }
    paste0("H ", rng(h, ""), "  /  S ", rng(s, "%"), "  /  V ", rng(v, "%"))
  }

  # hsv_vals: optional named list overriding input sliders (used on startup before inputs flush)
  do_randomize <- function(hsv_vals = list()) {
    g <- function(id, default) hsv_vals[[id]] %||% input[[id]] %||% default
    hsv2hex  <- function(h, s, v) grDevices::hsv(h / 360, s / 100, v / 100)
    rnd_in   <- function(range) runif(1, range[1], range[2])
    ang_dist <- function(a, b) min((a - b) %% 360, (b - a) %% 360)
    rnd_h_inv <- function(h_r, inv) {
      if (!isTRUE(inv)) return(rnd_in(h_r) %% 360)
      span <- 360 - (h_r[2] - h_r[1])
      if (span <= 0) return(runif(1, 0, 360))
      (h_r[2] + runif(1, 0, span)) %% 360
    }
    min_h    <- g("hue_dist",    40)
    max_s    <- g("max_s_dist",  30)
    max_v    <- g("max_v_dist",  20)
    s_r      <- g("rnd_s_accent", c(25, 55))
    v_r      <- g("rnd_v_accent", c(55, 80))

    gen_accent <- function(h_r, taken_h = c(), taken_s = c(), taken_v = c(), inv = FALSE) {
      for (i in 1:60) {
        h <- rnd_h_inv(h_r, inv); s <- rnd_in(s_r); v <- rnd_in(v_r)
        h_ok <- all(vapply(taken_h, ang_dist, numeric(1), b = h) >= min_h)
        s_ok <- !length(taken_s) || all(abs(s - taken_s) <= max_s)
        v_ok <- !length(taken_v) || all(abs(v - taken_v) <= max_v)
        if (h_ok && s_ok && v_ok) return(c(h, s, v))
      }
      c(rnd_h_inv(h_r, inv), rnd_in(s_r), rnd_in(v_r))
    }

    a1 <- gen_accent(g("rnd_h_theme", c(80, 180)), inv = isTRUE(input$inv_rnd_h_theme))
    a2 <- gen_accent(g("rnd_h_proj",  c(20,  60)), a1[1],          a1[2],          a1[3], inv = isTRUE(input$inv_rnd_h_proj))
    a3 <- gen_accent(g("rnd_h_skill", c(180,280)), c(a1[1],a2[1]), c(a1[2],a2[2]), c(a1[3],a2[3]), inv = isTRUE(input$inv_rnd_h_skill))

    cur <- rv$current_colors
    cols <- list(
      col_bg         = if (isTRUE(input$lock_bg))       (cur$col_bg         %||% "#0b3552") else
                        hsv2hex(rnd_h_inv(g("rnd_h_bg",c(0,360)), input$inv_rnd_h_bg), rnd_in(g("rnd_s_bg",c(5,25))),  rnd_in(g("rnd_v_bg",c(5,22)))),
      col_sidebar_bg = if (isTRUE(input$lock_sidebar))  (cur$col_sidebar_bg %||% "#081626") else
                        hsv2hex(rnd_h_inv(g("rnd_h_sb",c(0,360)), input$inv_rnd_h_sb), rnd_in(g("rnd_s_sb",c(10,35))), rnd_in(g("rnd_v_sb",c(3,14)))),
      col_node_bg    = if (isTRUE(input$lock_node_bg))  (cur$col_node_bg    %||% "#081626") else
                        hsv2hex(rnd_h_inv(g("rnd_h_nb",c(0,360)), input$inv_rnd_h_nb), rnd_in(g("rnd_s_nb",c(10,35))), rnd_in(g("rnd_v_nb",c(3,14)))),
      col_theme      = if (isTRUE(input$lock_theme))    (cur$col_theme      %||% "#3be37a") else hsv2hex(a1[1], a1[2], a1[3]),
      col_project    = if (isTRUE(input$lock_project))  (cur$col_project    %||% "#ffad33") else hsv2hex(a2[1], a2[2], a2[3]),
      col_skill      = if (isTRUE(input$lock_skill))    (cur$col_skill      %||% "#78e6e7") else hsv2hex(a3[1], a3[2], a3[3])
    )
    rv$last_random <- cols
    # Report the H/S/V ranges of the accent colors that were actually (re)randomized (skip locked ones).
    accs <- list()
    if (!isTRUE(input$lock_theme))   accs <- c(accs, list(a1))
    if (!isTRUE(input$lock_project)) accs <- c(accs, list(a2))
    if (!isTRUE(input$lock_skill))   accs <- c(accs, list(a3))
    rv$rand_hsv_txt <- if (length(accs))
      fmt_hsv_range(vapply(accs, `[`, numeric(1), 1), vapply(accs, `[`, numeric(1), 2), vapply(accs, `[`, numeric(1), 3))
      else "(all accents locked)"
    set_colors(cols)
  }

  observeEvent(input$randomize, { do_randomize() })

  # ── Light color randomizer ────────────────────────────────────────────────
  do_light_randomize <- function(hsv_vals = list()) {
    g <- function(id, default) hsv_vals[[id]] %||% input[[id]] %||% default
    hsv2hex  <- function(h, s, v) grDevices::hsv(h / 360, s / 100, v / 100)
    rnd_in   <- function(range) runif(1, range[1], range[2])
    ang_dist <- function(a, b) min((a - b) %% 360, (b - a) %% 360)
    rnd_h_inv <- function(h_r, inv) {
      if (!isTRUE(inv)) return(rnd_in(h_r) %% 360)
      span <- 360 - (h_r[2] - h_r[1])
      if (span <= 0) return(runif(1, 0, 360))
      (h_r[2] + runif(1, 0, span)) %% 360
    }
    min_h <- g("lhue_dist",    40)
    max_s <- g("lmax_s_dist",  30)
    max_v <- g("lmax_v_dist",  20)
    s_r   <- g("lrnd_s_accent", c(45, 75))
    v_r   <- g("lrnd_v_accent", c(28, 55))
    gen_accent <- function(h_r, taken_h = c(), taken_s = c(), taken_v = c(), inv = FALSE) {
      for (i in 1:60) {
        h <- rnd_h_inv(h_r, inv); s <- rnd_in(s_r); v <- rnd_in(v_r)
        h_ok <- all(vapply(taken_h, ang_dist, numeric(1), b = h) >= min_h)
        s_ok <- !length(taken_s) || all(abs(s - taken_s) <= max_s)
        v_ok <- !length(taken_v) || all(abs(v - taken_v) <= max_v)
        if (h_ok && s_ok && v_ok) return(c(h, s, v))
      }
      c(rnd_h_inv(h_r, inv), rnd_in(s_r), rnd_in(v_r))
    }
    a1 <- gen_accent(g("lrnd_h_theme", c(80, 180)), inv = isTRUE(input$inv_lrnd_h_theme))
    a2 <- gen_accent(g("lrnd_h_proj",  c(20,  60)), a1[1],          a1[2],          a1[3], inv = isTRUE(input$inv_lrnd_h_proj))
    a3 <- gen_accent(g("lrnd_h_skill", c(180,280)), c(a1[1],a2[1]), c(a1[2],a2[2]), c(a1[3],a2[3]), inv = isTRUE(input$inv_lrnd_h_skill))
    cur <- rv$current_light_colors
    cols <- list(
      light_col_bg         = if (isTRUE(input$llock_bg))       (cur$light_col_bg         %||% "#f0f4f8") else
                              hsv2hex(rnd_h_inv(g("lrnd_h_bg",c(0,360)), input$inv_lrnd_h_bg), rnd_in(g("lrnd_s_bg",c(0,20))),  rnd_in(g("lrnd_v_bg",c(82,97)))),
      light_col_sidebar_bg = if (isTRUE(input$llock_sidebar))  (cur$light_col_sidebar_bg %||% "#e2eaf3") else
                              hsv2hex(rnd_h_inv(g("lrnd_h_sb",c(0,360)), input$inv_lrnd_h_sb), rnd_in(g("lrnd_s_sb",c(5,25))),  rnd_in(g("lrnd_v_sb",c(74,88)))),
      light_col_node_bg    = if (isTRUE(input$llock_node_bg))  (cur$light_col_node_bg    %||% "#e2eaf3") else
                              hsv2hex(rnd_h_inv(g("lrnd_h_nb",c(0,360)), input$inv_lrnd_h_nb), rnd_in(g("lrnd_s_nb",c(5,25))),  rnd_in(g("lrnd_v_nb",c(74,88)))),
      light_col_theme      = if (isTRUE(input$llock_theme))    (cur$light_col_theme      %||% "#1e7c45") else hsv2hex(a1[1], a1[2], a1[3]),
      light_col_project    = if (isTRUE(input$llock_project))  (cur$light_col_project    %||% "#c06000") else hsv2hex(a2[1], a2[2], a2[3]),
      light_col_skill      = if (isTRUE(input$llock_skill))    (cur$light_col_skill      %||% "#1a7a7b") else hsv2hex(a3[1], a3[2], a3[3])
    )
    rv$last_light_random <- cols
    accs <- list()
    if (!isTRUE(input$llock_theme))   accs <- c(accs, list(a1))
    if (!isTRUE(input$llock_project)) accs <- c(accs, list(a2))
    if (!isTRUE(input$llock_skill))   accs <- c(accs, list(a3))
    rv$lrand_hsv_txt <- if (length(accs))
      fmt_hsv_range(vapply(accs, `[`, numeric(1), 1), vapply(accs, `[`, numeric(1), 2), vapply(accs, `[`, numeric(1), 3))
      else "(all accents locked)"
    set_light_colors(cols)
  }
  observeEvent(input$light_randomize, { do_light_randomize() })

  # Equidistant hues across the (possibly inverted) hue range, with a random rotation each call.
  # n points spaced span/n apart => exactly equidistant (0% error, well within the 1% margin).
  equidistant_hues <- function(n, h_r, inv, phase = NULL, start = NULL) {
    if (n <= 0) return(numeric(0))
    if (isTRUE(inv)) { lo <- h_r[2]; span <- 360 - (h_r[2] - h_r[1]) }
    else             { lo <- h_r[1]; span <- h_r[2] - h_r[1] }
    if (span <= 0) span <- 360
    if (!is.null(start)) { lo <- start; phase <- 0 }   # fixed start hue overrides the range's low end
    else if (is.null(phase)) phase <- runif(1)         # random rotation
    (lo + (((seq_len(n) - 1) / n + phase) %% 1) * span) %% 360
  }

  light_edge_randomize <- function(group = "both") {
    req(rv$g)
    hsv2hex <- function(h, s, v) grDevices::hsv(h / 360, s / 100, v / 100)
    rnd_in  <- function(range) runif(1, range[1], range[2])
    h_th   <- input$lec_h_theme %||% c(0, 360)
    h_sk   <- input$lec_h_skill %||% c(0, 360)
    s_r_th <- input$lec_s_theme %||% c(10, 50)
    v_r_th <- input$lec_v_theme %||% c(15, 45)
    s_r_sk <- input$lec_s_skill %||% c(10, 50)
    v_r_sk <- input$lec_v_skill %||% c(15, 45)
    groups_now <- vapply(rv$g$nodes, function(n) n$group %||% "Theme", character(1))
    all_ids <- vapply(rv$g$nodes, function(n) as.numeric(n$id), numeric(1))
    th_idx <- which(groups_now == "Theme")
    sk_idx <- which(groups_now == "Skill")
    # Rainbow mode (checkbox): constant S/V (range midpoint) + hues assigned in visual column order
    # (make_ordered_ids = the top-to-bottom stack order) so the sweep runs down the column; else scattered.
    ord_idx <- function(grp) match(make_ordered_ids(rv$g$order %||% list(), rv$g$nodes, grp), all_ids)
    if (group %in% c("both", "Theme")) {
      rb_th <- isTRUE(input$lrb_theme)
      idx <- if (rb_th) ord_idx("Theme") else th_idx
      th_start <- if (rb_th && !isTRUE(input$lrb_random_theme)) (input$lrb_start_theme %||% 0) else NULL
      hues <- equidistant_hues(length(idx), h_th, isTRUE(input$inv_lec_h_theme), start = th_start); ph <- numeric(0); ps <- numeric(0); pv <- numeric(0)
      for (k in seq_along(idx)) {
        s <- if (rb_th) mean(s_r_th) else rnd_in(s_r_th); v <- if (rb_th) mean(v_r_th) else rnd_in(v_r_th)
        rv$g$nodes[[ idx[k] ]]$lightEdgeColor <- hsv2hex(hues[k], s, v)
        ph <- c(ph, hues[k]); ps <- c(ps, s); pv <- c(pv, v)
      }
      rv$lec_hsv_txt_theme <- fmt_hsv_range(ph, ps, pv)
    }
    if (group %in% c("both", "Skill")) {
      rb_sk <- isTRUE(input$lrb_skill)
      idx <- if (rb_sk) ord_idx("Skill") else sk_idx
      sk_start <- if (rb_sk && !isTRUE(input$lrb_random_skill)) (input$lrb_start_skill %||% 0) else NULL
      hues <- equidistant_hues(length(idx), h_sk, isTRUE(input$inv_lec_h_skill), start = sk_start); ph <- numeric(0); ps <- numeric(0); pv <- numeric(0)
      for (k in seq_along(idx)) {
        s <- if (rb_sk) mean(s_r_sk) else rnd_in(s_r_sk); v <- if (rb_sk) mean(v_r_sk) else rnd_in(v_r_sk)
        rv$g$nodes[[ idx[k] ]]$lightEdgeColor <- hsv2hex(hues[k], s, v)
        ph <- c(ph, hues[k]); ps <- c(ps, s); pv <- c(pv, v)
      }
      rv$lec_hsv_txt_skill <- fmt_hsv_range(ph, ps, pv)
    }
  }
  observeEvent(input$lrandomize_edges_theme, { light_edge_randomize("Theme") })
  observeEvent(input$lrandomize_edges_skill,  { light_edge_randomize("Skill")  })
  # Ticking a Rainbow box applies a clean hue rainbow at once; its start-hue / random-start controls
  # re-apply while the box stays on.
  observeEvent(input$lrb_theme, { if (isTRUE(input$lrb_theme)) light_edge_randomize("Theme") }, ignoreInit = TRUE)
  observeEvent(input$lrb_skill,  { if (isTRUE(input$lrb_skill))  light_edge_randomize("Skill")  }, ignoreInit = TRUE)
  observeEvent(list(input$lrb_random_theme, input$lrb_start_theme), { if (isTRUE(input$lrb_theme)) light_edge_randomize("Theme") }, ignoreInit = TRUE)
  observeEvent(list(input$lrb_random_skill,  input$lrb_start_skill),  { if (isTRUE(input$lrb_skill))  light_edge_randomize("Skill")  }, ignoreInit = TRUE)

  # Readouts of the H/S/V ranges produced by the last randomization in each section
  .hsv_hint <- "(randomize to see produced H/S/V ranges)"
  output$rand_hsv_txt  <- renderText({ rv$rand_hsv_txt  %||% .hsv_hint })
  output$lrand_hsv_txt <- renderText({ rv$lrand_hsv_txt %||% .hsv_hint })
  output$ec_hsv_txt_theme  <- renderText({ rv$ec_hsv_txt_theme  %||% .hsv_hint })
  output$ec_hsv_txt_skill  <- renderText({ rv$ec_hsv_txt_skill  %||% .hsv_hint })
  output$lec_hsv_txt_theme <- renderText({ rv$lec_hsv_txt_theme %||% .hsv_hint })
  output$lec_hsv_txt_skill <- renderText({ rv$lec_hsv_txt_skill %||% .hsv_hint })

  observeEvent(input$save_lec_hsv, {
    d <- read_defaults()
    d$lec_hsv <- list(
      lec_h_theme = as.list(input$lec_h_theme %||% c(0, 360)),
      lec_h_skill = as.list(input$lec_h_skill %||% c(0, 360)),
      lec_s_theme = as.list(input$lec_s_theme %||% c(10, 50)),
      lec_v_theme = as.list(input$lec_v_theme %||% c(15, 45)),
      lec_s_skill = as.list(input$lec_s_skill %||% c(10, 50)),
      lec_v_skill = as.list(input$lec_v_skill %||% c(15, 45)),
      inv_lec_h_theme = isTRUE(input$inv_lec_h_theme),
      inv_lec_h_skill = isTRUE(input$inv_lec_h_skill)
    )
    write_defaults(d); showNotification("Light edge HSV settings saved.", type = "message")
  })

  observeEvent(input$light_save_to_presets, {
    cols <- rv$last_light_random
    if (is.null(cols)) { showNotification("Randomize first to get colors.", type = "warning"); return() }
    all_nms <- vapply(all_light_palettes(), function(p) p$name, character(1))
    existing <- sum(grepl("^Light Custom \\d+$", all_nms))
    n <- existing + 1L
    rv$light_custom_palettes <- c(rv$light_custom_palettes,
      list(c(list(name = paste0("Light Custom ", n)), cols)))
  })

  observeEvent(input$save_light_palettes, {
    req(length(rv$light_custom_palettes) > 0)
    jsonlite::write_json(rv$light_custom_palettes,
                         here("app_author", "data", "light_palettes.json"),
                         auto_unbox = TRUE, pretty = TRUE)
    showNotification(paste0(length(rv$light_custom_palettes), " light palette(s) saved to file."), type = "message")
  })

  observeEvent(input$save_light_hsv, {
    d <- read_defaults()
    d$light_hsv <- list(
      lrnd_h_bg = as.list(input$lrnd_h_bg), lrnd_s_bg = as.list(input$lrnd_s_bg), lrnd_v_bg = as.list(input$lrnd_v_bg),
      lrnd_h_sb = as.list(input$lrnd_h_sb), lrnd_s_sb = as.list(input$lrnd_s_sb), lrnd_v_sb = as.list(input$lrnd_v_sb),
      lrnd_h_nb = as.list(input$lrnd_h_nb), lrnd_s_nb = as.list(input$lrnd_s_nb), lrnd_v_nb = as.list(input$lrnd_v_nb),
      lrnd_h_theme  = as.list(input$lrnd_h_theme),
      lrnd_h_proj   = as.list(input$lrnd_h_proj),
      lrnd_h_skill  = as.list(input$lrnd_h_skill),
      lrnd_s_accent = as.list(input$lrnd_s_accent), lrnd_v_accent = as.list(input$lrnd_v_accent),
      lhue_dist   = input$lhue_dist   %||% 40,
      lmax_s_dist = input$lmax_s_dist %||% 30,
      lmax_v_dist = input$lmax_v_dist %||% 20,
      inv_lrnd_h_bg    = isTRUE(input$inv_lrnd_h_bg),
      inv_lrnd_h_sb    = isTRUE(input$inv_lrnd_h_sb),
      inv_lrnd_h_nb    = isTRUE(input$inv_lrnd_h_nb),
      inv_lrnd_h_theme = isTRUE(input$inv_lrnd_h_theme),
      inv_lrnd_h_proj  = isTRUE(input$inv_lrnd_h_proj),
      inv_lrnd_h_skill = isTRUE(input$inv_lrnd_h_skill)
    )
    write_defaults(d); showNotification("Light HSV settings saved.", type = "message")
  })

  observeEvent(input$save_light_colors, {
    bg <- input$light_col_bg         %||% isolate(rv$current_light_colors)$light_col_bg
    sb <- input$light_col_sidebar_bg %||% isolate(rv$current_light_colors)$light_col_sidebar_bg
    nb <- input$light_col_node_bg    %||% isolate(rv$current_light_colors)$light_col_node_bg    %||% "#e2eaf3"
    th <- input$light_col_theme      %||% isolate(rv$current_light_colors)$light_col_theme
    pr <- input$light_col_project    %||% isolate(rv$current_light_colors)$light_col_project
    sk <- input$light_col_skill      %||% isolate(rv$current_light_colors)$light_col_skill
    ec <- input$light_edge_color     %||% isolate(rv$current_light_colors)$light_edge_color %||% "#555555"
    if (is.null(bg)) { showNotification("No light colors to save yet.", type = "warning"); return() }
    cols <- list(light_col_bg=bg, light_col_sidebar_bg=sb, light_col_node_bg=nb, light_col_theme=th, light_col_project=pr, light_col_skill=sk, light_edge_color=ec,
                 light_one_color = isTRUE(input$light_one_color), light_col_all = input$light_col_all %||% th)
    d <- read_defaults(); d$light_colors <- cols
    write_defaults(d); showNotification("Light colors saved as default.", type = "message")
  })

  # Save HSV slider settings permanently
  observeEvent(input$save_hsv, {
    d <- read_defaults()
    d$hsv <- list(
      rnd_h_bg = as.list(input$rnd_h_bg), rnd_s_bg = as.list(input$rnd_s_bg), rnd_v_bg = as.list(input$rnd_v_bg),
      rnd_h_sb = as.list(input$rnd_h_sb), rnd_s_sb = as.list(input$rnd_s_sb), rnd_v_sb = as.list(input$rnd_v_sb),
      rnd_h_nb = as.list(input$rnd_h_nb), rnd_s_nb = as.list(input$rnd_s_nb), rnd_v_nb = as.list(input$rnd_v_nb),
      rnd_h_theme = as.list(input$rnd_h_theme),
      rnd_h_proj  = as.list(input$rnd_h_proj),
      rnd_h_skill = as.list(input$rnd_h_skill),
      rnd_s_accent = as.list(input$rnd_s_accent), rnd_v_accent = as.list(input$rnd_v_accent),
      hue_dist   = input$hue_dist   %||% 40,
      max_s_dist = input$max_s_dist %||% 30,
      max_v_dist = input$max_v_dist %||% 20,
      inv_rnd_h_bg    = isTRUE(input$inv_rnd_h_bg),
      inv_rnd_h_sb    = isTRUE(input$inv_rnd_h_sb),
      inv_rnd_h_nb    = isTRUE(input$inv_rnd_h_nb),
      inv_rnd_h_theme = isTRUE(input$inv_rnd_h_theme),
      inv_rnd_h_proj  = isTRUE(input$inv_rnd_h_proj),
      inv_rnd_h_skill = isTRUE(input$inv_rnd_h_skill)
    )
    write_defaults(d); showNotification("HSV settings saved.", type = "message")
  })

  # Save edge color HSV slider settings permanently
  observeEvent(input$save_ec_hsv, {
    d <- read_defaults()
    d$ec_hsv <- list(
      ec_h_theme   = as.list(input$ec_h_theme   %||% c(0, 360)),
      ec_h_skill   = as.list(input$ec_h_skill   %||% c(0, 360)),
      ec_s_theme   = as.list(input$ec_s_theme   %||% c(55, 90)),
      ec_v_theme   = as.list(input$ec_v_theme   %||% c(70, 100)),
      ec_s_skill   = as.list(input$ec_s_skill   %||% c(55, 90)),
      ec_v_skill   = as.list(input$ec_v_skill   %||% c(70, 100)),
      ec_hue_dist_theme = input$ec_hue_dist_theme %||% 30,
      ec_hue_dist_skill = input$ec_hue_dist_skill %||% 30,
      ec_max_s_dist = input$ec_max_s_dist %||% 30,
      ec_max_v_dist = input$ec_max_v_dist %||% 20,
      ec_node_dist  = input$ec_node_dist  %||% 20,
      inv_ec_h_theme = isTRUE(input$inv_ec_h_theme),
      inv_ec_h_skill = isTRUE(input$inv_ec_h_skill)
    )
    write_defaults(d); showNotification("Edge HSV settings saved.", type = "message")
  })

  # Add current randomized palette to session presets (newest first in dropdown)
  observeEvent(input$save_to_presets, {
    cols <- rv$last_random
    if (is.null(cols)) {
      showNotification("Randomize first to get colors.", type = "warning"); return()
    }
    # Running number across ALL palettes (session custom + file palettes)
    all_nms <- vapply(all_palettes(), function(p) p$name, character(1))
    existing <- sum(grepl("^Custom \\d+$", all_nms))
    n <- existing + 1L
    rv$custom_palettes <- c(rv$custom_palettes,
      list(c(list(name = paste0("Custom ", n)), cols)))
  })

  # Persist custom palettes to palettes.json (prepend to existing)
  observeEvent(input$save_palettes, {
    req(length(rv$custom_palettes) > 0)
    merged <- c(rv$custom_palettes, PALETTES)
    jsonlite::write_json(merged, here("app_author", "data", "palettes.json"),
                         auto_unbox = TRUE, pretty = TRUE)
    showNotification(paste0(length(rv$custom_palettes), " custom palette(s) saved to file."), type = "message")
  })

  # Save current colors as permanent defaults (reads input$col_* which reflect what user sees)
  observeEvent(input$save_colors, {
    bg  <- input$col_bg         %||% isolate(rv$current_colors)$col_bg
    sb  <- input$col_sidebar_bg %||% isolate(rv$current_colors)$col_sidebar_bg
    nb  <- input$col_node_bg    %||% isolate(rv$current_colors)$col_node_bg    %||% "#081626"
    th  <- input$col_theme      %||% isolate(rv$current_colors)$col_theme
    pr  <- input$col_project    %||% isolate(rv$current_colors)$col_project
    sk  <- input$col_skill      %||% isolate(rv$current_colors)$col_skill
    if (is.null(bg)) {
      showNotification("No colors to save yet — apply a palette or randomize first.", type = "warning")
      return()
    }
    cols <- list(col_bg = bg, col_sidebar_bg = sb, col_node_bg = nb, col_theme = th, col_project = pr, col_skill = sk,
                 one_color = isTRUE(input$one_color), col_all = input$col_all %||% th)
    d <- read_defaults(); d$colors <- cols
    write_defaults(d)
    showNotification("Colors saved as permanent default.", type = "message")
  })

  observeEvent(input$apply_source_color, {
    req(input$source_color_node)
    nid <- as.numeric(input$source_color_node)
    color <- trimws(input$source_color_custom %||% "")
    if (!nzchar(color) || !grepl("^#[0-9A-Fa-f]{3,8}$", color)) {
      picker <- trimws(input$source_color_picker %||% "")
      color <- if (nzchar(picker) && grepl("^#[0-9A-Fa-f]{6}$", picker)) picker else input$source_color_palette
    }
    for (i in seq_along(rv$g$nodes)) {
      if (as.numeric(rv$g$nodes[[i]]$id) == nid) {
        rv$g$nodes[[i]]$edgeColor <- color
        break
      }
    }
    # Also update per-edge color fields to stay consistent
    node_grp <- NULL
    for (n in rv$g$nodes) if (as.numeric(n$id) == nid) { node_grp <- n$group; break }
    if (identical(node_grp, "Theme")) {
      for (j in seq_along(rv$g$edges))
        if (as.numeric(rv$g$edges[[j]]$from) == nid) rv$g$edges[[j]]$color <- color
    } else if (identical(node_grp, "Skill")) {
      for (j in seq_along(rv$g$edges))
        if (as.numeric(rv$g$edges[[j]]$to) == nid) rv$g$edges[[j]]$color <- color
    }
    showNotification(paste0("Edge color for node ", nid, " set to ", color), type = "message")
  })

  # ── Edge color randomizer ────────────────────────────────────────────────────
  hex_to_hue <- function(hex) {
    tryCatch({
      v <- col2rgb(hex) / 255
      r <- v[1,1]; g <- v[2,1]; b <- v[3,1]
      mx <- max(r,g,b); mn <- min(r,g,b); d <- mx - mn
      if (d < 1e-6) return(0)
      h <- if (mx == r) 60 * ((g - b) / d %% 6)
           else if (mx == g) 60 * ((b - r) / d + 2)
           else 60 * ((r - g) / d + 4)
      h %% 360
    }, error = function(e) 0)
  }

  do_edge_randomize <- function(group = "both") {
    nd  <- nodes_df()
    th  <- nd[nd$group == "Theme", , drop = FALSE]
    sk  <- nd[nd$group == "Skill", , drop = FALSE]

    rnd_in   <- function(r) runif(1, r[1], r[2])
    hsv2hex  <- function(h, s, v) grDevices::hsv(h / 360, s / 100, v / 100)

    max_s    <- input$ec_max_s_dist %||% 30
    max_v    <- input$ec_max_v_dist %||% 20
    s_r_th   <- input$ec_s_theme %||% c(55, 90)
    v_r_th   <- input$ec_v_theme %||% c(70, 100)
    s_r_sk   <- input$ec_s_skill %||% c(55, 90)
    v_r_sk   <- input$ec_v_skill %||% c(70, 100)
    h_th     <- input$ec_h_theme %||% c(0, 360)
    h_sk     <- input$ec_h_skill %||% c(0, 360)

    # Random S/V kept within max_s/max_v of already-picked colors (hue is fixed/equidistant).
    gen_sv <- function(s_r, v_r, taken_sv) {
      for (i in 1:80) {
        s <- rnd_in(s_r); v <- rnd_in(v_r)
        if (!nrow(taken_sv) || (all(abs(s - taken_sv[,1]) <= max_s) && all(abs(v - taken_sv[,2]) <= max_v)))
          return(c(s, v))
      }
      c(rnd_in(s_r), rnd_in(v_r))
    }

    apply_ec <- function(nid, color, grp) {
      for (i in seq_along(rv$g$nodes))
        if (as.numeric(rv$g$nodes[[i]]$id) == nid) { rv$g$nodes[[i]]$edgeColor <- color; break }
      if (grp == "Theme")
        for (j in seq_along(rv$g$edges))
          if (as.numeric(rv$g$edges[[j]]$from) == nid) rv$g$edges[[j]]$color <- color
      else if (grp == "Skill")
        for (j in seq_along(rv$g$edges))
          if (as.numeric(rv$g$edges[[j]]$to) == nid) rv$g$edges[[j]]$color <- color
    }

    # Rainbow mode (checkbox): constant S/V (range midpoint) + hues assigned in visual column order
    # (make_ordered_ids = the top-to-bottom stack order) so the sweep runs down the column; else scattered.
    ord_ids <- function(grp) make_ordered_ids(rv$g$order %||% list(), rv$g$nodes, grp)
    if (group %in% c("both", "Theme")) {
      rb_th <- isTRUE(input$rb_theme); const_sv <- c(mean(s_r_th), mean(v_r_th))
      th_ids <- if (rb_th) ord_ids("Theme") else th$id
      th_start <- if (rb_th && !isTRUE(input$rb_random_theme)) (input$rb_start_theme %||% 0) else NULL
      hues <- equidistant_hues(length(th_ids), h_th, isTRUE(input$inv_ec_h_theme), start = th_start)
      taken_sv <- matrix(numeric(0), ncol = 2); ph <- numeric(0); ps <- numeric(0); pv <- numeric(0)
      for (i in seq_along(th_ids)) {
        sv <- if (rb_th) const_sv else gen_sv(s_r_th, v_r_th, taken_sv); taken_sv <- rbind(taken_sv, sv)
        apply_ec(th_ids[i], hsv2hex(hues[i], sv[1], sv[2]), "Theme")
        ph <- c(ph, hues[i]); ps <- c(ps, sv[1]); pv <- c(pv, sv[2])
      }
      rv$ec_hsv_txt_theme <- fmt_hsv_range(ph, ps, pv)
    }
    if (group %in% c("both", "Skill")) {
      rb_sk <- isTRUE(input$rb_skill); const_sv <- c(mean(s_r_sk), mean(v_r_sk))
      sk_ids <- if (rb_sk) ord_ids("Skill") else sk$id
      sk_start <- if (rb_sk && !isTRUE(input$rb_random_skill)) (input$rb_start_skill %||% 0) else NULL
      hues <- equidistant_hues(length(sk_ids), h_sk, isTRUE(input$inv_ec_h_skill), start = sk_start)
      taken_sv <- matrix(numeric(0), ncol = 2); ph <- numeric(0); ps <- numeric(0); pv <- numeric(0)
      for (i in seq_along(sk_ids)) {
        sv <- if (rb_sk) const_sv else gen_sv(s_r_sk, v_r_sk, taken_sv); taken_sv <- rbind(taken_sv, sv)
        apply_ec(sk_ids[i], hsv2hex(hues[i], sv[1], sv[2]), "Skill")
        ph <- c(ph, hues[i]); ps <- c(ps, sv[1]); pv <- c(pv, sv[2])
      }
      rv$ec_hsv_txt_skill <- fmt_hsv_range(ph, ps, pv)
    }
  }

  observeEvent(input$randomize_edges_theme, { do_edge_randomize("Theme") })
  observeEvent(input$randomize_edges_skill,  { do_edge_randomize("Skill")  })
  # Ticking a Rainbow box applies a clean hue rainbow at once. Changing its start-hue / random-start
  # controls re-applies while the box stays on (random start re-rolls each time it's toggled).
  observeEvent(input$rb_theme, { if (isTRUE(input$rb_theme)) do_edge_randomize("Theme") }, ignoreInit = TRUE)
  observeEvent(input$rb_skill,  { if (isTRUE(input$rb_skill))  do_edge_randomize("Skill")  }, ignoreInit = TRUE)
  observeEvent(list(input$rb_random_theme, input$rb_start_theme), { if (isTRUE(input$rb_theme)) do_edge_randomize("Theme") }, ignoreInit = TRUE)
  observeEvent(list(input$rb_random_skill,  input$rb_start_skill),  { if (isTRUE(input$rb_skill))  do_edge_randomize("Skill")  }, ignoreInit = TRUE)
  
  get_node_subs <- function(id) {
    for (n in rv$g$nodes)
      if (as.numeric(n$id) == id && identical(n$group, "Skill"))
        return(paste(as.character(unlist(n$subs %||% list())), collapse = "\n"))
    ""
  }
  
  desc_key <- function(id, group) {
    pre <- GROUP_PREFIX[[group]]; if (is.null(pre)) return(NULL); paste0(pre, id)
  }
  fi_desc_key <- function(id, group) {
    pre <- GROUP_PREFIX[[group]]; if (is.null(pre)) return(NULL); paste0("fi_", pre, id)
  }
  node_hidden <- function(id) {
    for (n in rv$g$nodes) if (as.numeric(n$id) == id) return(isTRUE(n$hidden))
    FALSE
  }

  observeEvent(input$node_id, {
    nd <- nodes_df(); id <- as.numeric(input$node_id); rv$selected_id <- id
    row <- nd[nd$id == id, , drop = FALSE]; if (!nrow(row)) return()
    updateNumericInput(session, "edit_id",    value    = row$id)
    updateSelectInput(session,  "edit_group", selected = row$group)
    updateTextInput(session,    "edit_title",    value = row$title)
    updateTextInput(session,    "edit_title_fi", value = row$title_fi)
    updateCheckboxInput(session, "edit_hidden", value = node_hidden(id))
    if (row$group == "Project") {
      updateSelectInput(session,  "edit_ptype", selected = row$ptype %||% "Text")
      updateNumericInput(session, "edit_pnum",  value    = row$pnum %||% row$id)
    }
    if (row$group == "Skill") {
      updateTextAreaInput(session, "edit_subs", value = get_node_subs(row$id))
    } else { updateTextAreaInput(session, "edit_subs", value = "") }
    key <- desc_key(id, row$group); fi_key <- fi_desc_key(id, row$group)
    updateTextAreaInput(session, "edit_desc",    value = if (!is.null(key))    rv$desc_map[[key]]    %||% "" else "")
    updateTextAreaInput(session, "edit_desc_fi", value = if (!is.null(fi_key)) rv$desc_map[[fi_key]] %||% "" else "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$clicked_node_id, {
    id <- as.numeric(input$clicked_node_id); nd <- nodes_df()
    row <- nd[nd$id == id, , drop = FALSE]
    rv$selected_id <- id; updateSelectInput(session, "node_id", selected = id)
    if (nrow(row) == 1) {
      grp <- row$group; key <- desc_key(id, grp)
      if (!is.null(key))
        session$sendCustomMessage("showDescPanel", list(
          title = row$title %||% paste(grp, id), text = rv$desc_map[[key]] %||% "",
          nodeId = id, group = grp,
          hasArticle = file.exists(here("articles", paste0(id, ".qmd"))),
          articleUrl = paste0("articles/", id, ".html"),
          articleInline = ART_SCAN$inline[[as.character(id)]] %||% ""))
      updateNumericInput(session, "edit_id",    value    = id)
      updateSelectInput(session,  "edit_group", selected = grp)
      updateTextInput(session,    "edit_title",    value = row$title)
      updateTextInput(session,    "edit_title_fi", value = row$title_fi)
      updateCheckboxInput(session, "edit_hidden", value = node_hidden(id))
      if (grp == "Project") {
        updateSelectInput(session,  "edit_ptype", selected = row$ptype %||% "Text")
        updateNumericInput(session, "edit_pnum",  value    = row$pnum %||% id)
      }
      if (grp == "Skill") updateTextAreaInput(session, "edit_subs", value = get_node_subs(id))
      fi_key <- fi_desc_key(id, grp)
      updateTextAreaInput(session, "edit_desc",    value = if (!is.null(key))    rv$desc_map[[key]]    %||% "" else "")
      updateTextAreaInput(session, "edit_desc_fi", value = if (!is.null(fi_key)) rv$desc_map[[fi_key]] %||% "" else "")
    }
    if (grp %in% c("Theme", "Skill"))
      updateSelectInput(session, "source_color_node", selected = as.character(id))
  })

  # "Open all" (inline UI): batch every openable node's description to the client to expand at once.
  # Optional group filter (msg$group = "Theme"/"Project"/"Skill", or "" for all).
  observeEvent(input$open_all_nodes, {
    grp_filter <- input$open_all_nodes$group %||% ""
    nodes <- list()
    for (n in rv$g$nodes) {
      grp <- n$group %||% "Theme"
      if (!(grp %in% c("Theme", "Project", "Skill"))) next
      if (nzchar(grp_filter) && grp != grp_filter) next
      id <- as.numeric(n$id); k <- desc_key(id, grp); fk <- fi_desc_key(id, grp)
      nodes[[length(nodes) + 1]] <- list(
        nodeId = id, group = grp,
        text    = if (!is.null(k))  rv$desc_map[[k]]  %||% "" else "",
        text_fi = if (!is.null(fk)) rv$desc_map[[fk]] %||% "" else "",
        hasArticle = file.exists(here("articles", paste0(id, ".qmd"))),
        articleUrl = paste0("articles/", id, ".html"),
        articleInline = ART_SCAN$inline[[as.character(id)]] %||% ""
      )
    }
    session$sendCustomMessage("expandAllInline", list(nodes = nodes))
  })

  observeEvent(input$apply_node, {
    req(input$node_id)
    old_id <- as.numeric(input$node_id); new_id <- as.numeric(input$edit_id); new_grp <- input$edit_group
    all_ids <- vapply(rv$g$nodes, function(n) as.numeric(n$id), numeric(1))
    if (new_id != old_id && new_id %in% all_ids) { showNotification("ID already exists.", type = "error"); return() }
    for (i in seq_along(rv$g$nodes)) {
      if (as.numeric(rv$g$nodes[[i]]$id) != old_id) next
      rv$g$nodes[[i]]$id <- new_id; rv$g$nodes[[i]]$title <- input$edit_title
      rv$g$nodes[[i]]$title_fi <- input$edit_title_fi %||% ""; rv$g$nodes[[i]]$group <- new_grp
      # Keep only `hidden: true` in the JSON (drop the key when unhidden), like ptype/subs below.
      if (isTRUE(input$edit_hidden)) rv$g$nodes[[i]]$hidden <- TRUE else rv$g$nodes[[i]]$hidden <- NULL
      if (new_grp == "Project") {
        rv$g$nodes[[i]]$ptype <- input$edit_ptype; rv$g$nodes[[i]]$pnum <- as.integer(input$edit_pnum)
      } else { rv$g$nodes[[i]]$ptype <- NULL; rv$g$nodes[[i]]$pnum <- NULL }
      if (new_grp == "Skill") {
        subs_lines <- trimws(strsplit(input$edit_subs %||% "", "\n", fixed = TRUE)[[1]])
        rv$g$nodes[[i]]$subs <- as.list(subs_lines[nzchar(subs_lines)])
      } else { rv$g$nodes[[i]]$subs <- NULL }
      break
    }
    old_key <- desc_key(old_id, new_grp); new_key <- desc_key(new_id, new_grp)
    old_fi_key <- fi_desc_key(old_id, new_grp); new_fi_key <- fi_desc_key(new_id, new_grp)
    if (!is.null(new_key)) {
      rv$desc_map[[new_key]] <- input$edit_desc %||% ""
      if (!is.null(old_key) && old_key != new_key) rv$desc_map[[old_key]] <- NULL
    }
    if (!is.null(new_fi_key)) {
      rv$desc_map[[new_fi_key]] <- input$edit_desc_fi %||% ""
      if (!is.null(old_fi_key) && old_fi_key != new_fi_key) rv$desc_map[[old_fi_key]] <- NULL
    }
    if (new_id != old_id) {
      for (j in seq_along(rv$g$edges)) {
        if (as.numeric(rv$g$edges[[j]]$from) == old_id) rv$g$edges[[j]]$from <- new_id
        if (as.numeric(rv$g$edges[[j]]$to)   == old_id) rv$g$edges[[j]]$to   <- new_id
      }
      for (grp in names(rv$g$order)) {
        cur <- as.numeric(unlist(rv$g$order[[grp]]))
        cur[cur == old_id] <- new_id
        # Re-sort so node lands at correct position for its new ID
        rv$g$order[[grp]] <- as.list(sort(cur))
      }
      rv$selected_id <- new_id; updateSelectInput(session, "node_id", selected = new_id)
    }
    ensure_desc_keys(); showNotification("Node updated.", type = "message")
  })
  
  observeEvent(input$add_node, {
    new_id <- as.numeric(input$new_id)
    all_ids <- vapply(rv$g$nodes, function(n) as.numeric(n$id), numeric(1))
    if (new_id %in% all_ids) { showNotification("ID already exists.", type = "error"); return() }
    nn <- list(id = new_id, group = input$new_group, title = input$new_title)
    if (input$new_group == "Project") { nn$ptype <- input$new_ptype; nn$pnum <- as.integer(input$new_pnum) }
    rv$g$nodes <- c(rv$g$nodes, list(nn))
    # Insert at sorted position in order list
    cur_order <- as.numeric(unlist(rv$g$order[[input$new_group]] %||% list()))
    pos <- which(cur_order > new_id)
    if (length(pos) > 0) {
      ins <- pos[1]
      new_order <- c(cur_order[seq_len(ins - 1)], new_id, cur_order[ins:length(cur_order)])
    } else {
      new_order <- c(cur_order, new_id)
    }
    rv$g$order[[input$new_group]] <- as.list(new_order)
    key <- desc_key(new_id, input$new_group)
    if (!is.null(key)) rv$desc_map[[key]] <- ""
    rv$selected_id <- new_id; showNotification("Node added.", type = "message")
  })
  
  observeEvent(input$remove_node, {
    req(input$node_id); id <- as.numeric(input$node_id)
    rv$g$nodes <- Filter(function(n) as.numeric(n$id) != id, rv$g$nodes)
    rv$g$edges <- Filter(function(e) !(as.numeric(e$from) == id || as.numeric(e$to) == id), rv$g$edges)
    for (grp in names(rv$g$order))
      rv$g$order[[grp]] <- Filter(function(x) as.numeric(x) != id, rv$g$order[[grp]])
    for (pre in unlist(GROUP_PREFIX)) rv$desc_map[[paste0(pre, id)]] <- NULL
    rv$selected_id <- NA_integer_; showNotification("Node removed.", type = "message")
  })
  
  observeEvent(input$add_edge, {
    from <- as.numeric(input$edge_from); to <- as.numeric(input$edge_to)
    if (from == to) return()
    exists <- any(vapply(rv$g$edges, function(e) as.numeric(e$from) == from && as.numeric(e$to) == to, logical(1)))
    if (exists) { showNotification("Edge already exists.", type = "warning"); return() }
    color <- trimws(input$edge_color_custom %||% "")
    if (!nzchar(color) || !grepl("^#[0-9A-Fa-f]{3,8}$", color)) color <- input$edge_color
    rv$g$edges <- c(rv$g$edges, list(list(from = from, to = to, color = color,
                                          dashes = isTRUE(input$edge_dashes), hidden = FALSE)))
    showNotification("Edge added.", type = "message")
  })
  
  observeEvent(input$remove_edge, {
    req(input$edge_key)
    parts <- strsplit(input$edge_key, " -> ", fixed = TRUE)[[1]]; if (length(parts) != 2) return()
    from <- as.numeric(parts[1]); to <- as.numeric(parts[2])
    rv$g$edges <- Filter(function(e) !(as.numeric(e$from) == from && as.numeric(e$to) == to), rv$g$edges)
    showNotification("Edge removed.", type = "message")
  })
  
  observeEvent(input$reset_full, {
    rv$g <- read_graph(GRAPH_PATH); rv$desc_map <- extract_from_qmd(QMD_PATH)
    ensure_desc_keys(); rv$selected_id <- NA_integer_
    ly <- rv$g$layout
    updateSliderInput(session, "gap_v",      value = ly$gap_v      %||% 18)
    updateSliderInput(session, "gap_col",    value = ly$gap_col    %||% 400)
    updateSliderInput(session, "font_hdr1",  value = ly$font_hdr1  %||% 22)
    updateSliderInput(session, "font_hdr2",  value = ly$font_hdr2  %||% 15)
    updateSliderInput(session, "font_node",  value = ly$font_node  %||% 12)
    updateSliderInput(session, "font_ptype", value = ly$font_ptype %||% 12)
    updateSliderInput(session, "font_subs",  value = ly$font_subs  %||% 15)
    updateSliderInput(session, "font_desc",  value = ly$font_desc  %||% 18)
    updateSliderInput(session, "h_theme",    value = ly$h_theme    %||% 46)
    updateSliderInput(session, "h_project",  value = ly$h_project  %||% 66)
    updateSliderInput(session, "h_skill",    value = ly$h_skill    %||% 46)
    updateCheckboxInput(session, "show_watermark", value = nzchar(ly$watermark_text %||% ""))
    updateTextInput(session, "watermark_text", value = ly$watermark_text %||% "")
    updateSliderInput(session, "watermark_size", value = ly$watermark_size %||% 10)
    # Reset mobile multipliers
    updateSliderInput(session, "mob_font_mult",    value = ly$mob_font_mult    %||% MOBILE_DEFAULTS$mob_font_mult)
    updateSliderInput(session, "mob_h_theme_mult", value = ly$mob_h_theme_mult %||% MOBILE_DEFAULTS$mob_h_theme_mult)
    updateSliderInput(session, "mob_h_proj_mult",  value = ly$mob_h_proj_mult  %||% MOBILE_DEFAULTS$mob_h_proj_mult)
    updateSliderInput(session, "mob_h_skill_mult", value = ly$mob_h_skill_mult %||% MOBILE_DEFAULTS$mob_h_skill_mult)
    updateSliderInput(session, "mob_gap_v_mult",   value = ly$mob_gap_v_mult   %||% MOBILE_DEFAULTS$mob_gap_v_mult)
    updateSliderInput(session, "mob_gap_col_mult", value = ly$mob_gap_col_mult %||% MOBILE_DEFAULTS$mob_gap_col_mult)
    updateCheckboxInput(session, "preview_mobile", value = FALSE)
    session$sendCustomMessage("setForceMobile", list(value = FALSE))
    set_colors(list(
      col_bg = ly$col_bg %||% "#0b3552", col_sidebar_bg = ly$col_sidebar_bg %||% "#081626",
      col_node_bg = ly$col_node_bg %||% "#081626",
      col_theme = ly$col_theme %||% "#3be37a", col_project = ly$col_project %||% "#ffad33",
      col_skill = ly$col_skill %||% "#78e6e7"))
    set_light_colors(list(
      light_col_bg         = ly$light_col_bg         %||% "#f0f4f8",
      light_col_sidebar_bg = ly$light_col_sidebar_bg %||% "#e2eaf3",
      light_col_node_bg    = ly$light_col_node_bg    %||% "#e2eaf3",
      light_col_theme      = ly$light_col_theme      %||% "#1e7c45",
      light_col_project    = ly$light_col_project    %||% "#c06000",
      light_col_skill      = ly$light_col_skill      %||% "#1a7a7b",
      light_edge_color     = ly$light_edge_color     %||% "#555555"
    ))
    # Restore column section
    rv_col$intro_title   <- ly$col_intro_title %||% "What is this site about"
    rv_col$intro_text    <- ly$col_intro_text  %||% ""
    rv_col$details_title <- ly$details_title   %||% "Details"
    rv_col$details_hint  <- ly$details_hint    %||% "Click on a topic in the graph to see details here"
    rv_col$vote_title    <- ly$vote_title      %||% "Vote"
    rv_col$vote_text     <- ly$vote_text       %||% "Vote for themes, projects and skills you\u2019d like me to focus on:"
    rv_col$fund_title    <- ly$funding_title   %||% "Funding"
    rv_col$fund_intro    <- ly$funding_intro   %||% "Current preference order to fund work on something related to the themes, projects and skills presented, when not working on my own time on them:"
    rv_col$fund_items    <- as.character(unlist(ly$funding_items %||% FUNDING_ITEMS))
    rv_col$hdr_theme_line1   <- ly$hdr_theme_line1   %||% "Themes"
    rv_col$hdr_theme_line2   <- ly$hdr_theme_line2   %||% "I want to focus on"
    rv_col$hdr_project_line1 <- ly$hdr_project_line1 %||% "Projects"
    rv_col$hdr_project_line2 <- ly$hdr_project_line2 %||% "I\u2019m working on or want to work on"
    rv_col$hdr_skill_line1   <- ly$hdr_skill_line1   %||% "Skills"
    rv_col$hdr_skill_line2   <- ly$hdr_skill_line2   %||% "I have or want to develop"
    rv_col$fi_hdr_theme_line1   <- ly$fi_hdr_theme_line1   %||% ""
    rv_col$fi_hdr_theme_line2   <- ly$fi_hdr_theme_line2   %||% ""
    rv_col$fi_hdr_project_line1 <- ly$fi_hdr_project_line1 %||% ""
    rv_col$fi_hdr_project_line2 <- ly$fi_hdr_project_line2 %||% ""
    rv_col$fi_hdr_skill_line1   <- ly$fi_hdr_skill_line1   %||% ""
    rv_col$fi_hdr_skill_line2   <- ly$fi_hdr_skill_line2   %||% ""
    updateTextInput(session, "hdr_theme_line1",      value = rv_col$hdr_theme_line1)
    updateTextInput(session, "hdr_theme_line2",      value = rv_col$hdr_theme_line2)
    updateTextInput(session, "hdr_project_line1",    value = rv_col$hdr_project_line1)
    updateTextInput(session, "hdr_project_line2",    value = rv_col$hdr_project_line2)
    updateTextInput(session, "hdr_skill_line1",      value = rv_col$hdr_skill_line1)
    updateTextInput(session, "hdr_skill_line2",      value = rv_col$hdr_skill_line2)
    updateTextInput(session, "fi_hdr_theme_line1",   value = rv_col$fi_hdr_theme_line1)
    updateTextInput(session, "fi_hdr_theme_line2",   value = rv_col$fi_hdr_theme_line2)
    updateTextInput(session, "fi_hdr_project_line1", value = rv_col$fi_hdr_project_line1)
    updateTextInput(session, "fi_hdr_project_line2", value = rv_col$fi_hdr_project_line2)
    updateTextInput(session, "fi_hdr_skill_line1",   value = rv_col$fi_hdr_skill_line1)
    updateTextInput(session, "fi_hdr_skill_line2",   value = rv_col$fi_hdr_skill_line2)
    rv_col$fi_page_title    <- ly$fi_page_title         %||% ""
    rv_col$fi_intro_title   <- ly$fi_col_intro_title    %||% ""
    rv_col$fi_intro_text    <- ly$fi_col_intro_text     %||% ""
    rv_col$fi_details_title <- ly$fi_details_title      %||% ""
    rv_col$fi_details_hint  <- ly$fi_details_hint       %||% ""
    rv_col$fi_vote_title    <- ly$fi_vote_title         %||% ""
    rv_col$fi_vote_text     <- ly$fi_vote_text          %||% ""
    rv_col$fi_fund_title    <- ly$fi_funding_title      %||% ""
    rv_col$fi_fund_intro    <- ly$fi_funding_intro      %||% ""
    updateTextInput(session,    "col_intro_title", value = rv_col$intro_title)
    updateTextAreaInput(session, "col_intro_text", value = rv_col$intro_text)
    updateTextInput(session,    "details_title",   value = rv_col$details_title)
    updateTextInput(session,    "details_hint",    value = rv_col$details_hint)
    updateTextInput(session,    "vote_title",      value = rv_col$vote_title)
    updateTextAreaInput(session, "vote_text",      value = rv_col$vote_text)
    updateTextInput(session,    "funding_title",   value = rv_col$fund_title)
    updateTextAreaInput(session, "funding_intro",  value = rv_col$fund_intro)
    updateTextAreaInput(session, "funding_items",  value = paste(rv_col$fund_items, collapse = "\n"))
    updateTextInput(session,    "fi_page_title",      value = rv_col$fi_page_title)
    updateTextInput(session,    "fi_col_intro_title", value = rv_col$fi_intro_title)
    updateTextAreaInput(session, "fi_col_intro_text", value = rv_col$fi_intro_text)
    updateTextInput(session,    "fi_details_title",   value = rv_col$fi_details_title)
    updateTextInput(session,    "fi_details_hint",    value = rv_col$fi_details_hint)
    updateTextInput(session,    "fi_vote_title",      value = rv_col$fi_vote_title)
    updateTextAreaInput(session, "fi_vote_text",      value = rv_col$fi_vote_text)
    updateTextInput(session,    "fi_funding_title",   value = rv_col$fi_fund_title)
    updateTextAreaInput(session, "fi_funding_intro",  value = rv_col$fi_fund_intro)
    session$sendCustomMessage("updateAccTitles", list(
      details_title = rv_col$details_title, intro_title = rv_col$intro_title,
      vote_title = rv_col$vote_title, fund_title = rv_col$fund_title
    ))
    session$sendCustomMessage("setLanguageData", list(
      page_title_en    = "My interests - Ville Sepp\u00e4l\u00e4",
      page_title_fi    = rv_col$fi_page_title    %||% "",
      details_title_fi = rv_col$fi_details_title %||% "",
      intro_title_fi   = rv_col$fi_intro_title   %||% "",
      vote_title_fi    = rv_col$fi_vote_title    %||% "",
      fund_title_fi    = rv_col$fi_fund_title    %||% ""
    ))
    showNotification("Reset from saved files.", type = "message")
  })
  
  observeEvent(input$save_graph, {
    ly <- rv$g$layout %||% list()
    rv$g$order <- make_order_from_nodes(rv$g$nodes, rv$g$order)
    rv$g$layout <- list(
      gap_v = input$gap_v %||% 18, gap_col = input$gap_col %||% 400,
      font_node = input$font_node %||% 12, font_ptype = input$font_ptype %||% 12,
      font_subs = input$font_subs %||% 15, font_desc = input$font_desc %||% 18,
      font_hdr1 = input$font_hdr1 %||% 22, font_hdr2 = input$font_hdr2 %||% 15,
      h_theme = input$h_theme %||% 46, h_project = input$h_project %||% 66, h_skill = input$h_skill %||% 46,
      w_project = input$w_project %||% NODE_W$Project,
      w_node = input$w_node %||% input$w_project %||% NODE_W$Project, ptype_pct = input$ptype_pct %||% 21,
      edge_width = input$edge_width %||% 2.5,
      edge_bands = isTRUE(input$edge_bands %||% TRUE),
      edge_sankey = isTRUE(input$edge_sankey %||% FALSE),
      edge_gap = input$edge_gap %||% 3,
      edge_transparency = input$edge_transparency %||% 18,
      edge_min_width = input$edge_min_width %||% 2.5,
      edge_min_on = isTRUE(input$edge_min_on %||% TRUE),
      edge_curve = input$edge_curve %||% 1,
      edge_pin_header = isTRUE(input$edge_pin_header %||% FALSE),
      fill_nodew = input$fill_nodew %||% 0,
      fill_projw = input$fill_projw %||% 0,
      fill_colgap = input$fill_colgap %||% 0,
      gradient_extent = input$gradient_extent %||% 20,
      gradient_transparency = input$gradient_transparency %||% 40,
      gradient_curve = input$gradient_curve %||% 1,
      gradient_hover_mult = input$gradient_hover_mult %||% 2,
      node_outline = input$node_outline %||% 3,
      project_outline = input$project_outline %||% input$node_outline %||% 3,
      outline_saturation = input$outline_saturation %||% 1,
      outline_transparency = input$outline_transparency %||% 0,
      node_pad = input$node_pad %||% 0,
      desc_pad = input$desc_pad %||% 10,
      inline_mode = isTRUE(input$inline_mode),
      articles_enabled = isTRUE(input$articles_enabled),
      accordion_icon = input$accordion_icon %||% "triangle",
      accordion_icon_size = input$accordion_icon_size %||% 14,
      watermark_text = if (isTRUE(input$show_watermark)) (input$watermark_text %||% "") else "",
      watermark_size = input$watermark_size %||% 10,
      col_bg = input$col_bg %||% "#0b3552", col_sidebar_bg = input$col_sidebar_bg %||% "#081626",
      col_node_bg = input$col_node_bg %||% (ly$col_node_bg %||% "#081626"),
      col_theme = input$col_theme %||% "#3be37a", col_project = input$col_project %||% "#ffad33",
      col_skill = input$col_skill %||% "#78e6e7",
      light_col_bg = input$light_col_bg %||% (ly$light_col_bg %||% "#f0f4f8"),
      light_col_sidebar_bg = input$light_col_sidebar_bg %||% (ly$light_col_sidebar_bg %||% "#e2eaf3"),
      light_col_node_bg = input$light_col_node_bg %||% (ly$light_col_node_bg %||% "#e2eaf3"),
      light_col_theme = input$light_col_theme %||% (ly$light_col_theme %||% "#1e7c45"),
      light_col_project = input$light_col_project %||% (ly$light_col_project %||% "#c06000"),
      light_col_skill = input$light_col_skill %||% (ly$light_col_skill %||% "#1a7a7b"),
      light_edge_color = input$light_edge_color %||% (ly$light_edge_color %||% "#555555"),
      mob_font_mult    = input$mob_font_mult    %||% MOBILE_DEFAULTS$mob_font_mult,
      mob_h_theme_mult = input$mob_h_theme_mult %||% MOBILE_DEFAULTS$mob_h_theme_mult,
      mob_h_proj_mult  = input$mob_h_proj_mult  %||% MOBILE_DEFAULTS$mob_h_proj_mult,
      mob_h_skill_mult = input$mob_h_skill_mult %||% MOBILE_DEFAULTS$mob_h_skill_mult,
      mob_gap_v_mult   = input$mob_gap_v_mult   %||% MOBILE_DEFAULTS$mob_gap_v_mult,
      mob_gap_col_mult = input$mob_gap_col_mult %||% MOBILE_DEFAULTS$mob_gap_col_mult,
      col_intro_title = rv_col$intro_title,
      col_intro_text  = rv_col$intro_text,
      details_title   = rv_col$details_title,
      details_hint    = rv_col$details_hint,
      vote_title      = rv_col$vote_title,
      vote_text       = rv_col$vote_text,
      funding_title   = rv_col$fund_title,
      funding_intro   = rv_col$fund_intro,
      funding_items   = as.list(rv_col$fund_items),
      hdr_theme_line1   = rv_col$hdr_theme_line1,   hdr_theme_line2   = rv_col$hdr_theme_line2,
      hdr_project_line1 = rv_col$hdr_project_line1, hdr_project_line2 = rv_col$hdr_project_line2,
      hdr_skill_line1   = rv_col$hdr_skill_line1,   hdr_skill_line2   = rv_col$hdr_skill_line2,
      fi_hdr_theme_line1   = rv_col$fi_hdr_theme_line1,   fi_hdr_theme_line2   = rv_col$fi_hdr_theme_line2,
      fi_hdr_project_line1 = rv_col$fi_hdr_project_line1, fi_hdr_project_line2 = rv_col$fi_hdr_project_line2,
      fi_hdr_skill_line1   = rv_col$fi_hdr_skill_line1,   fi_hdr_skill_line2   = rv_col$fi_hdr_skill_line2,
      fi_page_title       = rv_col$fi_page_title,
      fi_col_intro_title  = rv_col$fi_intro_title,
      fi_col_intro_text   = rv_col$fi_intro_text,
      fi_details_title    = rv_col$fi_details_title,
      fi_details_hint     = rv_col$fi_details_hint,
      fi_vote_title       = rv_col$fi_vote_title,
      fi_vote_text        = rv_col$fi_vote_text,
      fi_funding_title    = rv_col$fi_fund_title,
      fi_funding_intro    = rv_col$fi_fund_intro)
    write_graph(rv$g, GRAPH_PATH); showNotification("Saved graph.json.", type = "message")
  })
  observeEvent(input$sync_to_publish, {
    rv$g$order <- make_order_from_nodes(rv$g$nodes, rv$g$order)
    write_graph(rv$g, PUBLISH_GRAPH_JSON); showNotification("Wrote app_publish/www/graph.json.", type = "message")
  })
  observeEvent(input$load_qmd, {
    rv$desc_map <- extract_from_qmd(QMD_PATH); ensure_desc_keys()
    showNotification("Loaded QMD.", type = "message")
  })
  observeEvent(input$write_qmd, {
    write_qmd_from_map(QMD_PATH, rv$desc_map); showNotification("Wrote QMD.", type = "message")
  })
  observeEvent(input$write_desc_json, {
    write_json_map(PUBLISH_DESC_JSON, rv$desc_map); showNotification("Wrote descriptions.json.", type = "message")
  })
  # SVG/PNG export — write to www/ then trigger download via JS
  export_cyto_data <- function() {
    ly <- rv$g$layout
    build_cyto_data(rv$g,
                    gap_v = input$gap_v %||% (ly$gap_v %||% 18),
                    gap_col = input$gap_col %||% (ly$gap_col %||% 400),
                    font_node = input$font_node %||% (ly$font_node %||% 12),
                    font_ptype = input$font_ptype %||% (ly$font_ptype %||% 12),
                    font_subs = input$font_subs %||% (ly$font_subs %||% 15),
                    font_desc = input$font_desc %||% (ly$font_desc %||% 18),
                    font_hdr1 = input$font_hdr1 %||% (ly$font_hdr1 %||% 22),
                    font_hdr2 = input$font_hdr2 %||% (ly$font_hdr2 %||% 15),
                    h_theme = input$h_theme %||% (ly$h_theme %||% 46),
                    h_project = input$h_project %||% (ly$h_project %||% 66),
                    h_skill = input$h_skill %||% (ly$h_skill %||% 46),
                    w_project = input$w_project %||% (ly$w_project %||% NODE_W$Project),
                    w_node = input$w_node %||% ly$w_node,
                    watermark_text = if (isTRUE(input$show_watermark)) (input$watermark_text %||% "") else "",
                    watermark_size = input$watermark_size %||% (ly$watermark_size %||% 10),
                    col_bg = input$col_bg %||% (ly$col_bg %||% "#0b3552"),
                    col_sidebar_bg = input$col_sidebar_bg %||% (ly$col_sidebar_bg %||% "#081626"),
                    col_node_bg = input$col_node_bg %||% (ly$col_node_bg %||% "#081626"),
                    col_theme = input$col_theme %||% (ly$col_theme %||% "#3be37a"),
                    col_project = input$col_project %||% (ly$col_project %||% "#ffad33"),
                    col_skill = input$col_skill %||% (ly$col_skill %||% "#78e6e7"))
  }
  
  observeEvent(input$save_svg, {
    cd <- export_cyto_data()
    svg_text <- paste0('<?xml version="1.0" encoding="UTF-8"?>\n', generate_svg(cd))
    fname <- paste0("concept_map_", Sys.Date(), ".svg")
    writeBin(charToRaw(svg_text), here("app_author", "www", fname))
    session$sendCustomMessage("triggerDownload", list(url = paste0("app_www/", fname), filename = fname))
    showNotification("SVG saved.", type = "message")
  })
  
  observeEvent(input$save_png, {
    cd <- export_cyto_data()
    svg_text <- generate_svg(cd)
    fname <- paste0("concept_map_", Sys.Date(), ".png")
    rsvg::rsvg_png(charToRaw(svg_text), here("app_author", "www", fname), width = 1080, height = 1080)
    session$sendCustomMessage("triggerDownload", list(url = paste0("app_www/", fname), filename = fname))
    showNotification("PNG saved.", type = "message")
  })
}

shinyApp(ui, server)
