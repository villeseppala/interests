# tools/build_static.R
# Builds the static GitHub Pages site into site/
# Run from repo root: Rscript tools/build_static.R

library(jsonlite)
source(file.path("shared", "layout.R"))

GRAPH_PATH <- file.path("app_publish", "www", "graph.json")
DESC_PATH  <- file.path("app_publish", "www", "descriptions.json")
OUT_DIR    <- "site"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Read data ─────────────────────────────────────────────────────────────────
g        <- read_graph(GRAPH_PATH)
desc_map <- if (file.exists(DESC_PATH)) fromJSON(DESC_PATH, simplifyVector = FALSE) else list()
ly       <- g$layout

# ── Build cyto data ───────────────────────────────────────────────────────────
# Wide/tall values for the aspect-aware settings; av$tall produces the second layout in the payload
# (only when it actually differs from the wide one) — see ASPECT_INPUTS in shared/layout.R.
av <- aspect_values(ly); aw <- av$wide
cd <- build_dual_cyto_data(g,
  gap_v            = aw$gap_v,
  gap_col          = aw$gap_col,
  font_node        = aw$font_node,
  font_project     = aw$font_project,
  font_ptype       = aw$font_ptype,
  font_subs        = aw$font_subs,
  font_desc        = ly$font_desc        %||% 18,
  font_hdr1        = ly$font_hdr1        %||% 22,
  font_hdr2        = ly$font_hdr2        %||% 15,
  h_theme          = aw$h_theme,
  h_project        = aw$h_project,
  h_skill          = aw$h_skill,
  w_project        = aw$w_project,
  w_node           = aw$w_node,
  tall_over        = av$tall,
  inline_mode      = isTRUE(ly$inline_mode %||% TRUE),
  watermark_text   = ly$watermark_text   %||% "",
  watermark_size   = ly$watermark_size   %||% 10,
  qr_enabled       = isTRUE(ly$qr_enabled %||% FALSE),
  qr_url           = ly$qr_url           %||% "",
  qr_size          = ly$qr_size          %||% 110,
  center_cols      = isTRUE(aw$center_cols),
  header_fill_pct  = ly$header_fill_pct  %||% 90,
  header_title_max = ly$header_title_max %||% 1.5,
  frame_line_w     = ly$frame_line_w     %||% 2,
  frame_corner_r   = ly$frame_corner_r   %||% 14,
  frame_fill_pct   = ly$frame_fill_pct   %||% 50,
  frame_fill_opacity = frame_fill_opacity_of(ly),
  headers_on_stack = isTRUE(aw$headers_on_stack),
  col_bg           = ly$col_bg           %||% "#0b3552",
  col_sidebar_bg   = ly$col_sidebar_bg   %||% "#081626",
  col_node_bg      = ly$col_node_bg      %||% "#081626",
  col_column_bg    = ly$col_column_bg    %||% "#000000",
  col_theme        = ly$col_theme        %||% "#3be37a",
  col_project      = ly$col_project      %||% "#ffad33",
  col_skill        = ly$col_skill        %||% "#78e6e7",
  light_col_bg         = ly$light_col_bg         %||% "#f0f4f8",
  light_col_sidebar_bg = ly$light_col_sidebar_bg %||% "#e2eaf3",
  light_col_node_bg    = ly$light_col_node_bg    %||% "#e2eaf3",
  light_col_column_bg  = ly$light_col_column_bg  %||% "#000000",
  light_col_theme      = ly$light_col_theme      %||% "#1e7c45",
  light_col_project    = ly$light_col_project    %||% "#c06000",
  light_col_skill      = ly$light_col_skill      %||% "#1a7a7b",
  light_edge_color     = ly$light_edge_color     %||% "#555555",
  mob_font_mult    = ly$mob_font_mult    %||% MOBILE_DEFAULTS$mob_font_mult,
  mob_h_theme_mult = ly$mob_h_theme_mult %||% MOBILE_DEFAULTS$mob_h_theme_mult,
  mob_h_proj_mult  = ly$mob_h_proj_mult  %||% MOBILE_DEFAULTS$mob_h_proj_mult,
  mob_h_skill_mult = ly$mob_h_skill_mult %||% MOBILE_DEFAULTS$mob_h_skill_mult,
  mob_gap_v_mult   = ly$mob_gap_v_mult   %||% MOBILE_DEFAULTS$mob_gap_v_mult,
  mob_gap_col_mult = ly$mob_gap_col_mult %||% MOBILE_DEFAULTS$mob_gap_col_mult,
  hdr_theme_line1      = ly$hdr_theme_line1      %||% "Themes",
  hdr_theme_line2      = ly$hdr_theme_line2      %||% "I want to focus on",
  hdr_project_line1    = ly$hdr_project_line1    %||% "Projects",
  hdr_project_line2    = ly$hdr_project_line2    %||% "I\u2019m working on or want to work on",
  hdr_skill_line1      = ly$hdr_skill_line1      %||% "Skills",
  hdr_skill_line2      = ly$hdr_skill_line2      %||% "I have or want to develop",
  hdr_about_line1      = ly$hdr_about_line1      %||% "About",
  fi_hdr_about_line1   = ly$fi_hdr_about_line1   %||% "",
  fi_hdr_theme_line1   = ly$fi_hdr_theme_line1   %||% "",
  fi_hdr_theme_line2   = ly$fi_hdr_theme_line2   %||% "",
  fi_hdr_project_line1 = ly$fi_hdr_project_line1 %||% "",
  fi_hdr_project_line2 = ly$fi_hdr_project_line2 %||% "",
  fi_hdr_skill_line1   = ly$fi_hdr_skill_line1   %||% "",
  fi_hdr_skill_line2   = ly$fi_hdr_skill_line2   %||% ""
)

# ── Articles (full texts) ─────────────────────────────────────────────────────
# Source .qmd files live in articles/<id>.qmd; Quarto renders them to site/articles/<id>.html.
# Here we scan the sources to build the manifest (articles.json) and to mark which nodes
# have an article. Rendering the HTML is a separate `quarto render articles` step.
ARTICLES_DIR <- "articles"
# Pages that should live at the site ROOT (e.g. /portfolio.html) rather than under
# /articles/. They still render with the shared article chrome, then get promoted up
# (see below) and are kept out of the numbered-article manifest.
ROOT_PAGES   <- c("portfolio")

# ── Sync the interactive apps into their article's shinylive cells ────────────
# Each app's single source of truth is app_<name>/app.R; inject it verbatim into
# the matching marked shinylive-r cell in the article so the embedded copy never
# drifts. Never hand-edit the cell — edit app_<name>/app.R and rebuild.
# `height` becomes the cell's viewerHeight. Shinylive passes a NUMBER through as px and a STRING
# through verbatim as a CSS length (see asCssLengthUnit in shinylive.js), so a clamp() works here.
# Why clamp and not a fixed px or a bare vh: a fixed height leaves the app scrolling internally on a
# tall screen and cropped on a short one; a bare vh grows without limit, and since the app's width is
# capped, that extra height is dead space. clamp(floor, vh, ceiling) is "at least this tall, grow with
# the window, stop once more height buys nothing". Quoted so YAML always reads it as a string.
inject_shinylive <- function(qmd_path, app_path, marker, height = "clamp(900px, 92vh, 1150px)") {
  if (!file.exists(qmd_path) || !file.exists(app_path)) return(invisible())
  qmd <- readLines(qmd_path, warn = FALSE)
  s   <- grep(sprintf("<!-- %s:START -->", marker), qmd, fixed = TRUE)
  e   <- grep(sprintf("<!-- %s:END -->",   marker), qmd, fixed = TRUE)
  if (length(s) != 1 || length(e) != 1 || e <= s) {
    cat(sprintf("  (%s markers not found in %s; skipped)\n", marker, qmd_path)); return(invisible())
  }
  # Quoting makes YAML read the value as a string; a bare number would then reach CSS as "900"
  # (no unit) and be ignored, so turn numbers into px here rather than relying on shinylive doing it.
  h <- if (is.numeric(height)) paste0(height, "px") else as.character(height)
  cell <- c(sprintf("<!-- %s:START -->", marker),
            "```{shinylive-r}", "#| standalone: true", sprintf('#| viewerHeight: "%s"', h),
            readLines(app_path, warn = FALSE),
            "```", sprintf("<!-- %s:END -->", marker))
  tail <- if (e < length(qmd)) qmd[(e + 1):length(qmd)] else character(0)
  writeLines(c(qmd[seq_len(s - 1)], cell, tail), qmd_path)
  cat(sprintf("  synced %s -> %s (%s)\n", app_path, qmd_path, marker))
}

# ── Apps that are plain web pages (no R runtime): shown in an iframe ─────────
# The x-risk calculator (app_xriskb/xriskb.html) and the threshold curve editor (app_hazard/hazard.html)
# are single self-contained pages: everything runs in JavaScript, so they open instantly, with no R
# runtime to download. (Each folder's app.R is the Shiny version it came from, kept for reference and
# not used here.) The build copies them to site/articles/apps/ (APP_PAGES, after Quarto below) and writes
# the iframe that shows them between the article's markers. Never hand-edit those cells either.
# The frame takes the app's own height: the page posts its height whenever it changes and the listener
# written with the frame applies it, so the app never scrolls inside the frame; the article does.
# `min_width` (px) keeps an app that has no narrow-screen layout at that width on a small screen: the
# frame's wrapper then scrolls sideways, so only the app pans, not the article (the hazard app, 800px,
# as its shinylive embed was). On a bare full-window page (full = TRUE) the frame starts a window tall
# and loads at once instead of when scrolled near.
APP_PAGES <- c("apps/xriskb.html" = file.path("app_xriskb", "xriskb.html"),    # published path -> source
               "apps/hazard.html" = file.path("app_hazard", "hazard.html"))
inject_frame <- function(qmd_path, src, marker, title, full = FALSE, min_width = NULL) {
  if (!file.exists(qmd_path)) return(invisible())
  qmd <- readLines(qmd_path, warn = FALSE)
  s   <- grep(sprintf("<!-- %s:START -->", marker), qmd, fixed = TRUE)
  e   <- grep(sprintf("<!-- %s:END -->",   marker), qmd, fixed = TRUE)
  if (length(s) != 1 || length(e) != 1 || e <= s) {
    cat(sprintf("  (%s markers not found in %s; skipped)\n", marker, qmd_path)); return(invisible())
  }
  style <- paste0("display:block;width:100%;border:0;height:", if (full) "100vh" else "1100px", ";",
                  if (!is.null(min_width)) sprintf("min-width:%dpx;", as.integer(min_width)) else "")
  frame <- c(
    '<div class="app-frame-wrap" style="overflow-x:auto;-webkit-overflow-scrolling:touch;">',
    sprintf('<iframe class="app-frame" src="%s" title="%s"%s style="%s"></iframe>',
            src, title, if (full) "" else ' loading="lazy"', style),
    "</div>",
    "<script>",
    "/* Fit each app frame to its page: the app posts {appHeight} whenever its height changes. */",
    "if (!window.appFrameFit) {",
    "  window.appFrameFit = true;",
    "  window.addEventListener('message', function (e) {",
    "    if (!e.data || typeof e.data.appHeight !== 'number') return;",
    "    document.querySelectorAll('iframe.app-frame').forEach(function (f) {",
    "      if (f.contentWindow === e.source) f.style.height = Math.ceil(e.data.appHeight) + 'px';",
    "    });",
    "  });",
    "}",
    "</script>")
  cell <- c(sprintf("<!-- %s:START -->", marker), "```{=html}", frame, "```", sprintf("<!-- %s:END -->", marker))
  tail <- if (e < length(qmd)) qmd[(e + 1):length(qmd)] else character(0)
  writeLines(c(qmd[seq_len(s - 1)], cell, tail), qmd_path)
  cat(sprintf("  framed %s -> %s (%s)\n", src, qmd_path, marker))
}
# Each app is in article 201 and on its own listed article (xrisk.qmd / hazard.qmd), and has a bare
# full-window page (app-page: true, no nav/article chrome - see articles/app-page.css) that the articles
# link to as "open full-screen".
XRISK  <- "apps/xriskb.html"; XRISK_TITLE  <- "Extinction risk calculator"
HAZARD <- "apps/hazard.html"; HAZARD_TITLE <- "Threshold curve editor"
inject_frame(file.path(ARTICLES_DIR, "201.qmd"),        XRISK,  "XRISK-APP",  XRISK_TITLE)
inject_frame(file.path(ARTICLES_DIR, "xrisk.qmd"),      XRISK,  "XRISK-APP",  XRISK_TITLE)
inject_frame(file.path(ARTICLES_DIR, "xriskb-app.qmd"), XRISK,  "XRISK-APP",  XRISK_TITLE, full = TRUE)
inject_frame(file.path(ARTICLES_DIR, "201.qmd"),        HAZARD, "HAZARD-APP", HAZARD_TITLE, min_width = 800)
inject_frame(file.path(ARTICLES_DIR, "hazard.qmd"),     HAZARD, "HAZARD-APP", HAZARD_TITLE, min_width = 800)
inject_frame(file.path(ARTICLES_DIR, "hazard-app.qmd"), HAZARD, "HAZARD-APP", HAZARD_TITLE, full = TRUE, min_width = 800)

# The only app still running R in the browser (shinylive): the R-rendered original x-risk app, app_xrisk,
# kept as the unlisted, unlinked full-window page xrisk-app.html for comparison.
inject_shinylive(file.path(ARTICLES_DIR, "xrisk-app.qmd"), file.path("app_xrisk", "app.R"), "XRISK-APP", "100vh")

# Render the .qmd sources to site/articles/<id>.html via Quarto (part of this one build).
if (dir.exists(ARTICLES_DIR) && length(list.files(ARTICLES_DIR, pattern = "\\.qmd$"))) {
  if (nzchar(Sys.which("quarto"))) {
    cat("Rendering articles with Quarto...\n")
    rc <- tryCatch(system2("quarto", c("render", shQuote(ARTICLES_DIR))),
                   error = function(e) { cat("  quarto render failed:", conditionMessage(e), "\n"); 1L })
    if (!identical(rc, 0L)) cat("  (quarto render returned non-zero; article HTML may be stale)\n")
  } else {
    cat("Note: 'quarto' not found on PATH — skipping article HTML render (manifest still built).\n")
  }
}

# The plain-page apps go next to the articles that frame them. Their iframe src gets a ?v= stamp at the
# end of the build, so the browser never keeps serving an old copy after a rebuild.
for (nm in names(APP_PAGES)) {
  dir.create(dirname(file.path(OUT_DIR, "articles", nm)), showWarnings = FALSE, recursive = TRUE)
  if (file.copy(APP_PAGES[[nm]], file.path(OUT_DIR, "articles", nm), overwrite = TRUE))
    cat(sprintf("  app page %s -> %s/articles/%s\n", APP_PAGES[[nm]], OUT_DIR, nm))
}

# ── Share one shinylive/webR runtime between every app page ──────────────────
# Quarto (project type "default") gives EVERY page that embeds a shinylive app its own full copy of
# the runtime under <page>_files/libs/quarto-contrib/ — ~80 MB each, byte-identical. Git dedupes the
# blobs, but the checkout and every Pages deploy carry all the copies. So after rendering, the first
# copy found is moved to site/articles/shinylive-libs/quarto-contrib/, the other copies are deleted,
# and each page's <script>/<link> references are rewritten to the shared folder. Safe because
# shinylive locates its assets relative to its own script URL (import.meta.url), and the service
# worker (shinylive-sw.js) is a separate file at site/articles/ that this does not touch.
# Idempotent: with no fresh copies it only (re)writes references, so it also runs when Quarto is absent.
# If a shinylive update changes how the extension embeds itself, check the substitution below first.
SHARED_LIBS <- "shinylive-libs"
dedupe_shinylive <- function() {
  adir   <- file.path(OUT_DIR, "articles")
  shared <- file.path(adir, SHARED_LIBS, "quarto-contrib")
  moved  <- FALSE                                         # has this build already installed a fresh copy?
  for (pg in list.files(adir, pattern = "\\.html$")) {
    stem <- sub("\\.html$", "", pg)
    copy <- file.path(adir, paste0(stem, "_files"), "libs", "quarto-contrib")
    if (dir.exists(copy)) {
      if (!moved) {                                       # first fresh copy REPLACES the shared runtime (so a version bump lands)
        if (dir.exists(shared)) unlink(shared, recursive = TRUE)
        dir.create(dirname(shared), recursive = TRUE, showWarnings = FALSE)
        if (!file.rename(copy, shared)) { cat(sprintf("  (could not move %s; left in place)\n", copy)); next }
        moved <- TRUE
        cat(sprintf("  shinylive runtime: %s -> %s\n", copy, shared))
      } else {                                            # later copies are identical: drop them
        unlink(copy, recursive = TRUE)
        cat(sprintf("  shinylive runtime: dropped duplicate %s\n", copy))
      }
    }
    hp   <- file.path(adir, pg)
    html <- readLines(hp, warn = FALSE, encoding = "UTF-8")
    ref  <- paste0(stem, "_files/libs/quarto-contrib/")
    if (any(grepl(ref, html, fixed = TRUE))) {
      writeLines(gsub(ref, paste0(SHARED_LIBS, "/quarto-contrib/"), html, fixed = TRUE), hp, useBytes = TRUE)
      cat(sprintf("  %s -> shared shinylive libs\n", pg))
    }
  }
}
dedupe_shinylive()

# ── Promote standalone pages to the site root ────────────────────────────────
# Quarto renders every source into site/articles/ with the shared chrome. For the
# ROOT_PAGES we copy the rendered HTML up to site/ and rewrite its relative asset
# paths: ../ refs point to the root, same-dir refs (_files/, article.css, images/)
# stay under articles/. Runs whether or not Quarto is present (uses the committed
# site/articles/<page>.html when it is not), so /<page>.html always tracks it.
for (pg in ROOT_PAGES) {
  src <- file.path(OUT_DIR, "articles", paste0(pg, ".html"))
  if (!file.exists(src)) { cat(sprintf("  (articles/%s.html not rendered yet; root copy skipped)\n", pg)); next }
  html <- paste(readLines(src, warn = FALSE), collapse = "\n")
  html <- gsub("../index.html",  "index.html",  html, fixed = TRUE)   # nav/footer links -> root
  html <- gsub("../site-nav.js", "site-nav.js", html, fixed = TRUE)
  html <- gsub(sprintf('="%s_files/', pg), sprintf('="articles/%s_files/', pg), html, fixed = TRUE)  # libs stay under articles/
  html <- gsub('="article.css', '="articles/article.css', html, fixed = TRUE)
  html <- gsub('="images/',      '="articles/images/',    html, fixed = TRUE)
  html <- gsub(paste0('="', SHARED_LIBS, '/'), paste0('="articles/', SHARED_LIBS, '/'), html, fixed = TRUE)   # shared shinylive runtime
  writeLines(html, file.path(OUT_DIR, paste0(pg, ".html")))
  cat(sprintf("  promoted articles/%s.html -> %s/%s.html\n", pg, OUT_DIR, pg))
}

# Manifest + which node ids have an article (shared helper in layout.R)
.art        <- scan_articles(ARTICLES_DIR)
articles    <- Filter(function(a) !(a$id %in% ROOT_PAGES), .art$manifest)   # root pages aren't numbered articles
article_ids <- setdiff(.art$ids, ROOT_PAGES)

# ── Descriptions lookup (keyed by node id) ────────────────────────────────────
descriptions <- list()
for (n in g$nodes) {
  grp <- n$group %||% "Theme"
  pre <- GROUP_PREFIX[[grp]]
  if (is.null(pre)) next
  key    <- paste0(pre, n$id)
  fi_key <- paste0("fi_", pre, n$id)
  nid    <- as.character(n$id)
  entry <- list(
    title    = n$title    %||% paste(grp, n$id),
    title_fi = n$title_fi %||% "",
    text     = desc_map[[key]]    %||% "",
    text_fi  = desc_map[[fi_key]] %||% "",
    group    = grp
  )
  if (nid %in% article_ids) {
    entry$hasArticle <- TRUE
    entry$articleUrl <- paste0("articles/", nid, ".html")
    # Opt-in inline quick-read: carry the article body markdown so the node can expand it in place.
    if (!is.null(.art$inline[[nid]])) entry$articleInline <- .art$inline[[nid]]
    entry$articleLang <- .art$langs[[nid]] %||% "en"   # "fi" when the article is written in Finnish
  }
  descriptions[[nid]] <- entry
}

# ── Sidebar content ───────────────────────────────────────────────────────────
nl2br <- function(s) gsub("\n", "<br>", s %||% "", fixed = TRUE)

sidebar <- list(
  page_title_en    = ly$page_title_en %||% "My interests",
  page_title_fi    = ly$fi_page_title      %||% "",
  details_title    = ly$details_title      %||% "Details",
  details_hint     = ly$details_hint       %||% "Click on an item to show description",
  details_hint_fi  = ly$fi_details_hint    %||% "",
  details_title_fi = ly$fi_details_title   %||% "",
  intro_title      = ly$col_intro_title    %||% "What is this site about",
  intro_title_fi   = ly$fi_col_intro_title %||% "",
  vote_title       = ly$vote_title         %||% "Vote",
  vote_title_fi    = ly$fi_vote_title      %||% "",
  fund_title       = ly$funding_title      %||% "Funding",
  fund_title_fi    = ly$fi_funding_title   %||% ""
)

vote_text    <- ly$vote_text %||% "Vote for themes, projects and skills."
fi_vote_text <- if (nzchar(ly$fi_vote_text %||% "")) ly$fi_vote_text else vote_text
vote_html <- list(en = nl2br(vote_text), fi = nl2br(fi_vote_text))

col_intro_text <- ly$col_intro_text %||% ""
fi_intro_text  <- if (nzchar(ly$fi_col_intro_text %||% "")) ly$fi_col_intro_text else col_intro_text
intro_html <- list(en = nl2br(col_intro_text), fi = nl2br(fi_intro_text))

fund_items    <- as.character(unlist(ly$funding_items %||% FUNDING_ITEMS))
html_items    <- vapply(fund_items, function(line) {
  stripped <- sub("^( +)", "", line)
  paste0(strrep("&nbsp;", (nchar(line) - nchar(stripped)) * 3), stripped)
}, character(1), USE.NAMES = FALSE)
fund_intro    <- ly$funding_intro    %||% "Current preference order to fund work on something related to the themes, projects and skills presented, when not working on my own time on them:"
fi_fund_intro <- if (nzchar(ly$fi_funding_intro %||% "")) ly$fi_funding_intro else fund_intro
funding_html  <- list(
  en_intro = fund_intro,
  fi_intro = fi_fund_intro,
  items    = paste(html_items, collapse = "<br>")
)

# ── Assemble and write payload ────────────────────────────────────────────────
cd$descriptions  <- descriptions
cd$sidebar       <- sidebar
cd$vote_html     <- vote_html
cd$intro_html    <- intro_html
cd$funding_html  <- funding_html
cd$ptypeLayout   <- list(
  ptypePct         = as.numeric(ly$ptype_pct %||% 10),
  projectNodeWidth = as.numeric(ly$w_project %||% NODE_W$Project)
)
cd$gradient_extent <- as.numeric(ly$gradient_extent %||% 20)
cd$gradient_transparency <- as.numeric(ly$gradient_transparency %||% 40)
cd$gradient_curve <- as.numeric(ly$gradient_curve %||% 1)
cd$gradient_hover_mult <- as.numeric(ly$gradient_hover_mult %||% 2)
cd$gradient_hover_desc <- isTRUE(ly$gradient_hover_desc %||% FALSE)
cd$hover_white_outline <- isTRUE(ly$hover_white_outline %||% FALSE)
cd$node_outline <- as.numeric(ly$node_outline %||% 3)
cd$project_outline <- as.numeric(ly$project_outline %||% ly$node_outline %||% 3)
cd$outline_saturation <- as.numeric(ly$outline_saturation %||% 1)
cd$outline_transparency <- as.numeric(ly$outline_transparency %||% 0)
cd$node_pad <- as.numeric(ly$node_pad %||% 0)
cd$project_max_width <- as.numeric(ly$project_max_width %||% 0)
cd$desc_pad <- as.numeric(ly$desc_pad %||% 10)
cd$edge_width <- as.numeric(ly$edge_width %||% 2.5)
cd$edge_bands <- isTRUE(ly$edge_bands %||% TRUE)
cd$edge_sankey <- isTRUE(ly$edge_sankey %||% FALSE)
cd$edge_gap <- as.numeric(ly$edge_gap %||% 3)
cd$edge_transparency <- as.numeric(ly$edge_transparency %||% 18)
cd$edge_min_width <- as.numeric(ly$edge_min_width %||% 2.5)
cd$edge_min_on <- isTRUE(ly$edge_min_on %||% TRUE)
cd$edge_curve <- as.numeric(ly$edge_curve %||% 1)
cd$edge_pin_header <- isTRUE(ly$edge_pin_header %||% FALSE)
cd$fill_nodew <- as.numeric(ly$fill_nodew %||% 0)
cd$fill_projw <- as.numeric(ly$fill_projw %||% 0)
cd$fill_colgap <- as.numeric(ly$fill_colgap %||% 0)
cd$fill_nodepad <- as.numeric(ly$fill_nodepad %||% 0)
cd$narrow_gap_mult <- as.numeric(ly$narrow_gap_mult %||% 1)
cd$narrow_node_mult <- as.numeric(ly$narrow_node_mult %||% 1)
cd$aspectVars <- aspect_vars_payload(av)
cd$inline_mode <- isTRUE(ly$inline_mode %||% TRUE)
cd$articles_enabled <- isTRUE(ly$articles_enabled %||% FALSE)
cd$auto_fit_open <- isTRUE(ly$auto_fit_open %||% FALSE)
cd$accordion_icon <- ly$accordion_icon %||% "triangle"
cd$accordion_icon_size <- as.numeric(ly$accordion_icon_size %||% 14)
cd$github_url <- ly$github_url %||% "#"

write_json(cd, file.path(OUT_DIR, "payload.json"), auto_unbox = TRUE, null = "null")

# Article manifest for the nav dropdown + landing page (empty array when no articles)
write_json(articles, file.path(OUT_DIR, "articles.json"), auto_unbox = TRUE, null = "null")

# Copy static assets
file.copy(file.path("app_publish", "www", "render.js"), file.path(OUT_DIR, "render.js"), overwrite = TRUE)
file.copy(file.path("app_publish", "www", "style.css"), file.path(OUT_DIR, "style.css"), overwrite = TRUE)

# Cache-bust the asset references in index.html so a rebuild is always picked up by the browser
# (otherwise a stale cached render.js/style.css/payload.json keeps the old behaviour, e.g. inline UI
# not applying after a rebuild). Stamps ?v=<build time>, re-stamping any existing ?v= on each build.
stamp    <- as.integer(Sys.time())
idx_path <- file.path(OUT_DIR, "index.html")
if (file.exists(idx_path)) {
  html  <- paste(readLines(idx_path, warn = FALSE), collapse = "\n")
  html  <- gsub('(href="style\\.css)(\\?v=[0-9]+)?"',      sprintf('\\1?v=%d"', stamp), html)
  html  <- gsub('(src="render\\.js)(\\?v=[0-9]+)?"',       sprintf('\\1?v=%d"', stamp), html)
  html  <- gsub('(src="site-nav\\.js)(\\?v=[0-9]+)?"',     sprintf('\\1?v=%d"', stamp), html)   # render.js relies on its siteNavFillSlot()
  html  <- gsub("(fetch\\('payload\\.json)(\\?v=[0-9]+)?'", sprintf("\\1?v=%d'", stamp), html)
  writeLines(html, idx_path)
  cat(sprintf("  index.html    cache-busted (v=%d)\n", stamp))
}
# ...and the app pages framed in the articles (inject_frame writes a bare src="<page>")
for (hp in list.files(file.path(OUT_DIR, "articles"), pattern = "\\.html$", full.names = TRUE)) {
  html  <- readLines(hp, warn = FALSE, encoding = "UTF-8")
  html2 <- html
  for (nm in names(APP_PAGES))
    html2 <- gsub(sprintf('(src="%s)(\\?v=[0-9]+)?"', nm), sprintf('\\1?v=%d"', stamp), html2)
  if (!identical(html, html2)) {
    writeLines(html2, hp, useBytes = TRUE)
    cat(sprintf("  %s: app frame cache-busted\n", basename(hp)))
  }
}

cat(sprintf("Static site built in %s/\n", OUT_DIR))
cat(sprintf("  payload.json  %s bytes\n", format(file.info(file.path(OUT_DIR, "payload.json"))$size, big.mark = ",")))
cat(sprintf("  render.js     %s bytes\n", format(file.info(file.path(OUT_DIR, "render.js"))$size, big.mark = ",")))
cat(sprintf("  style.css     %s bytes\n", format(file.info(file.path(OUT_DIR, "style.css"))$size, big.mark = ",")))
