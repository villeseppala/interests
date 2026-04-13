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
cd <- build_dual_cyto_data(g,
  gap_v            = ly$gap_v            %||% 18,
  gap_col          = ly$gap_col          %||% 400,
  font_node        = ly$font_node        %||% 12,
  font_ptype       = ly$font_ptype       %||% 12,
  font_subs        = ly$font_subs        %||% 15,
  font_desc        = ly$font_desc        %||% 11.5,
  font_hdr1        = ly$font_hdr1        %||% 22,
  font_hdr2        = ly$font_hdr2        %||% 15,
  h_theme          = ly$h_theme          %||% 46,
  h_project        = ly$h_project        %||% 66,
  h_skill          = ly$h_skill          %||% 46,
  watermark_text   = ly$watermark_text   %||% "",
  watermark_size   = ly$watermark_size   %||% 10,
  col_bg           = ly$col_bg           %||% "#0b3552",
  col_sidebar_bg   = ly$col_sidebar_bg   %||% "#081626",
  col_theme        = ly$col_theme        %||% "#3be37a",
  col_project      = ly$col_project      %||% "#ffad33",
  col_skill        = ly$col_skill        %||% "#78e6e7",
  light_col_bg         = ly$light_col_bg         %||% "#f0f4f8",
  light_col_sidebar_bg = ly$light_col_sidebar_bg %||% "#e2eaf3",
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
  fi_hdr_theme_line1   = ly$fi_hdr_theme_line1   %||% "",
  fi_hdr_theme_line2   = ly$fi_hdr_theme_line2   %||% "",
  fi_hdr_project_line1 = ly$fi_hdr_project_line1 %||% "",
  fi_hdr_project_line2 = ly$fi_hdr_project_line2 %||% "",
  fi_hdr_skill_line1   = ly$fi_hdr_skill_line1   %||% "",
  fi_hdr_skill_line2   = ly$fi_hdr_skill_line2   %||% ""
)

# ── Descriptions lookup (keyed by node id) ────────────────────────────────────
descriptions <- list()
for (n in g$nodes) {
  grp <- n$group %||% "Theme"
  pre <- GROUP_PREFIX[[grp]]
  if (is.null(pre)) next
  key    <- paste0(pre, n$id)
  fi_key <- paste0("fi_", pre, n$id)
  descriptions[[as.character(n$id)]] <- list(
    title    = n$title    %||% paste(grp, n$id),
    title_fi = n$title_fi %||% "",
    text     = desc_map[[key]]    %||% "",
    text_fi  = desc_map[[fi_key]] %||% "",
    group    = grp
  )
}

# ── Sidebar content ───────────────────────────────────────────────────────────
nl2br <- function(s) gsub("\n", "<br>", s %||% "", fixed = TRUE)

sidebar <- list(
  page_title_en    = "My interests",
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
cd$edge_width <- as.numeric(ly$edge_width %||% 2.5)
cd$github_url <- ly$github_url %||% "#"

write_json(cd, file.path(OUT_DIR, "payload.json"), auto_unbox = TRUE, null = "null")

# Copy static assets
file.copy(file.path("app_publish", "www", "render.js"), file.path(OUT_DIR, "render.js"), overwrite = TRUE)
file.copy(file.path("app_publish", "www", "style.css"), file.path(OUT_DIR, "style.css"), overwrite = TRUE)

cat(sprintf("Static site built in %s/\n", OUT_DIR))
cat(sprintf("  payload.json  %s bytes\n", format(file.info(file.path(OUT_DIR, "payload.json"))$size, big.mark = ",")))
cat(sprintf("  render.js     %s bytes\n", format(file.info(file.path(OUT_DIR, "render.js"))$size, big.mark = ",")))
cat(sprintf("  style.css     %s bytes\n", format(file.info(file.path(OUT_DIR, "style.css"))$size, big.mark = ",")))
