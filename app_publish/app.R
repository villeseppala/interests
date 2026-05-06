# app_publish/app.R — Public viewer (no authoring UI)
# Sources shared layout engine; JS in www/render.js

library(shiny)
library(here)

# Ensure CWD is app_publish/ regardless of how Positron launches the app
if (!file.exists("www/graph.json")) setwd(here("app_publish"))

source(here("shared", "layout.R"))

# ── Paths ────────────────────────────────────────────────────────────────────
GRAPH_PATH <- "www/graph.json"
DESC_PATH  <- "www/descriptions.json"

read_desc <- function(path) {
  if (!file.exists(path)) return(list())
  fromJSON(path, simplifyVector = FALSE)
}

# ── CSS ──────────────────────────────────────────────────────────────────────
# Canonical CSS lives in www/style.css (shared with static site build)
APP_CSS <- paste(readLines("www/style.css", warn = FALSE), collapse = "\n")

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=5, viewport-fit=cover"),
    tags$style(HTML(APP_CSS)),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/flag-icons/7.2.3/css/flag-icons.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.28.1/cytoscape.min.js"),
    tags$script(src = "render.js")
  ),
  
  div(id = "main-row",

      # ── Left spacer ──
      div(class = "col-spacer"),
      
      # ── Info sidebar (desktop) ──
      div(id = "info-sidebar",
          div(id = "page-title",
              div(span(class = "en-only", id = "page-title-en", "My interests - Ville Sepp\u00e4l\u00e4"),
                  span(class = "fi-only", id = "page-title-fi", "")),
              div(id = "controls-row",
                  tags$button(id = "mode-btn", onclick = "toggleLightMode()", "\u2600"),
                  tags$button(class = "lang-btn lang-active", id = "lang-btn-en",
                              onclick = "setLanguage('en')", HTML('<span class="fi fi-gb"></span>')),
                  tags$button(class = "lang-btn", id = "lang-btn-fi",
                              onclick = "setLanguage('fi')", HTML('<span class="fi fi-fi"></span>')),
                  tags$a(id = "github-btn", href = "#", target = "_blank", title = "GitHub",
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
                  div(class = "acc-body", id = "acc-about-body",
                      uiOutput("col_intro_ui")
                  )
              ),
              # Accordion 3 — Vote (collapsed)
              div(class = "acc-section", id = "acc-vote",
                  div(class = "acc-header", onclick = "toggleAcc(this)",
                      span(class = "acc-title", id = "acc-title-vote"),
                      span(class = "acc-arrow", HTML("&#9660;"))
                  ),
                  div(class = "acc-body", id = "acc-vote-body",
                      uiOutput("vote_section_ui")
                  )
              ),
              # Accordion 4 — Funding (collapsed)
              div(class = "acc-section", id = "acc-fund",
                  div(class = "acc-header", onclick = "toggleAcc(this)",
                      span(class = "acc-title", id = "acc-title-fund"),
                      span(class = "acc-arrow", HTML("&#9660;"))
                  ),
                  div(class = "acc-body", id = "acc-fund-body",
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
                          span(class = "en-only", "Funding"), span(class = "fi-only", "Rahoitus")),
              tags$button(class = "mob-tab", id = "mob-tab-settings",
                          onclick = "mobShowTab('settings')",
                          span(class = "en-only", "Settings"), span(class = "fi-only", "Asetukset"))
          ),
          div(id = "mob-tab-content",
              div(id = "mob-content-about", class = "mob-tab-pane mob-tab-pane-active"),
              div(id = "mob-content-vote",  class = "mob-tab-pane"),
              div(id = "mob-content-fund",  class = "mob-tab-pane"),
              div(id = "mob-content-settings", class = "mob-tab-pane")
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

# ── Server ───────────────────────────────────────────────────────────────────
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

server <- function(input, output, session) {
  g        <- read_graph(GRAPH_PATH)
  desc_map <- read_desc(DESC_PATH)
  
  ly <- g$layout
  cd <- build_dual_cyto_data(g,
                             gap_v = ly$gap_v %||% 18, gap_col = ly$gap_col %||% 400,
                             font_node = ly$font_node %||% 12, font_ptype = ly$font_ptype %||% 12,
                             font_subs = ly$font_subs %||% 15, font_desc = ly$font_desc %||% 11.5,
                             font_hdr1 = ly$font_hdr1 %||% 22, font_hdr2 = ly$font_hdr2 %||% 15,
                             h_theme = ly$h_theme %||% 46, h_project = ly$h_project %||% 66,
                             h_skill = ly$h_skill %||% 46,
                             watermark_text = ly$watermark_text %||% "",
                             watermark_size = ly$watermark_size %||% 10,
                             col_bg = ly$col_bg %||% "#0b3552",
                             col_sidebar_bg = ly$col_sidebar_bg %||% "#081626",
                             col_node_bg = ly$col_node_bg %||% "#081626",
                             col_theme = ly$col_theme %||% "#3be37a",
                             col_project = ly$col_project %||% "#ffad33",
                             col_skill = ly$col_skill %||% "#78e6e7",
                             light_col_bg = ly$light_col_bg %||% "#f0f4f8",
                             light_col_sidebar_bg = ly$light_col_sidebar_bg %||% "#e2eaf3",
                             light_col_node_bg = ly$light_col_node_bg %||% "#e2eaf3",
                             light_col_theme = ly$light_col_theme %||% "#1e7c45",
                             light_col_project = ly$light_col_project %||% "#c06000",
                             light_col_skill = ly$light_col_skill %||% "#1a7a7b",
                             light_edge_color = ly$light_edge_color %||% "#555555",
                             mob_font_mult    = ly$mob_font_mult    %||% MOBILE_DEFAULTS$mob_font_mult,
                             mob_h_theme_mult = ly$mob_h_theme_mult %||% MOBILE_DEFAULTS$mob_h_theme_mult,
                             mob_h_proj_mult  = ly$mob_h_proj_mult  %||% MOBILE_DEFAULTS$mob_h_proj_mult,
                             mob_h_skill_mult = ly$mob_h_skill_mult %||% MOBILE_DEFAULTS$mob_h_skill_mult,
                             mob_gap_v_mult   = ly$mob_gap_v_mult   %||% MOBILE_DEFAULTS$mob_gap_v_mult,
                             mob_gap_col_mult = ly$mob_gap_col_mult %||% MOBILE_DEFAULTS$mob_gap_col_mult,
                             hdr_theme_line1=ly$hdr_theme_line1 %||% "Themes",
                             hdr_theme_line2=ly$hdr_theme_line2 %||% "I want to focus on",
                             hdr_project_line1=ly$hdr_project_line1 %||% "Projects",
                             hdr_project_line2=ly$hdr_project_line2 %||% "I\u2019m working on or want to work on",
                             hdr_skill_line1=ly$hdr_skill_line1 %||% "Skills",
                             hdr_skill_line2=ly$hdr_skill_line2 %||% "I have or want to develop",
                             fi_hdr_theme_line1=ly$fi_hdr_theme_line1 %||% "",
                             fi_hdr_theme_line2=ly$fi_hdr_theme_line2 %||% "",
                             fi_hdr_project_line1=ly$fi_hdr_project_line1 %||% "",
                             fi_hdr_project_line2=ly$fi_hdr_project_line2 %||% "",
                             fi_hdr_skill_line1=ly$fi_hdr_skill_line1 %||% "",
                             fi_hdr_skill_line2=ly$fi_hdr_skill_line2 %||% ""
  )
  observe({
    session$sendCustomMessage("setPtypeLayout", list(
      ptypePct        = as.numeric(ly$ptype_pct  %||% 10),
      projectNodeWidth = as.numeric(ly$w_project %||% NODE_W$Project)
    ))
    session$sendCustomMessage("setEdgeWidth", list(width = ly$edge_width %||% 2.5))
  })
  observe({ session$sendCustomMessage("initCy", cd) })
  observe({
    session$sendCustomMessage("updateAccTitles", list(
      details_title = details_title, intro_title = col_intro_title,
      vote_title = vote_title, fund_title = fund_title
    ))
    session$sendCustomMessage("setLanguageData", list(
      page_title_en = "My interests - Ville Sepp\u00e4l\u00e4",
      page_title_fi = fi_page_title,
      details_title_fi = fi_details_title,
      intro_title_fi   = fi_intro_title,
      vote_title_fi    = fi_vote_title,
      fund_title_fi    = fi_fund_title
    ))
  })
  
  # Column sidebar content from saved layout
  col_intro_text  <- ly$col_intro_text   %||% ""
  col_intro_title <- ly$col_intro_title  %||% "What is this site about"
  details_title   <- ly$details_title    %||% "Details"
  details_hint    <- ly$details_hint     %||% "Click on a topic in the graph to see details here"
  vote_title      <- ly$vote_title       %||% "Vote"
  vote_text       <- ly$vote_text        %||% "Vote for themes, projects and skills you\u2019d like me to focus on:"
  fund_title      <- ly$funding_title    %||% "Funding"
  fund_intro      <- ly$funding_intro    %||% "Current preference order to fund work on something related to the themes, projects and skills presented, when not working on my own time on them:"
  fund_items      <- as.character(unlist(ly$funding_items %||% FUNDING_ITEMS))
  # Finnish translations
  fi_page_title    <- ly$fi_page_title        %||% ""
  fi_intro_title   <- ly$fi_col_intro_title   %||% ""
  fi_intro_text    <- ly$fi_col_intro_text    %||% ""
  fi_details_title <- ly$fi_details_title     %||% ""
  fi_details_hint  <- ly$fi_details_hint      %||% "Klikkaa aihetta graafissa n\u00e4hd\u00e4ksesi sen kuvauksen t\u00e4ss\u00e4"
  fi_vote_title    <- ly$fi_vote_title        %||% ""
  fi_vote_text     <- ly$fi_vote_text         %||% ""
  fi_fund_title    <- ly$fi_funding_title     %||% ""
  fi_fund_intro    <- ly$fi_funding_intro     %||% ""
  # Column header texts
  hdr_theme_line1      <- ly$hdr_theme_line1      %||% "Themes"
  hdr_theme_line2      <- ly$hdr_theme_line2      %||% "I want to focus on"
  hdr_project_line1    <- ly$hdr_project_line1    %||% "Projects"
  hdr_project_line2    <- ly$hdr_project_line2    %||% "I\u2019m working on or want to work on"
  hdr_skill_line1      <- ly$hdr_skill_line1      %||% "Skills"
  hdr_skill_line2      <- ly$hdr_skill_line2      %||% "I have or want to develop"
  fi_hdr_theme_line1   <- ly$fi_hdr_theme_line1   %||% ""
  fi_hdr_theme_line2   <- ly$fi_hdr_theme_line2   %||% ""
  fi_hdr_project_line1 <- ly$fi_hdr_project_line1 %||% ""
  fi_hdr_project_line2 <- ly$fi_hdr_project_line2 %||% ""
  fi_hdr_skill_line1   <- ly$fi_hdr_skill_line1   %||% ""
  fi_hdr_skill_line2   <- ly$fi_hdr_skill_line2   %||% ""

  output$col_intro_ui <- renderUI({
    if (!nzchar(trimws(col_intro_text)) && !nzchar(trimws(fi_intro_text))) return(NULL)
    txt_fi <- if (nzchar(trimws(fi_intro_text))) fi_intro_text else col_intro_text
    style_str <- "color:rgba(255,255,255,0.8);font-family:Arial,Helvetica,sans-serif;font-size:var(--desc-font);line-height:1.65;margin-bottom:12px;"
    tagList(
      if (nzchar(trimws(col_intro_text)))
        div(class = "en-only", style = style_str, linkify(col_intro_text)),
      div(class = "fi-only", style = style_str, linkify(txt_fi))
    )
  })

  output$sidebar_hint_ui <- renderUI({
    hint_fi <- if (nzchar(fi_details_hint)) fi_details_hint else details_hint
    div(id = "sidebar-hint",
        span(class = "en-only", linkify(details_hint)),
        span(class = "fi-only", linkify(hint_fi))
    )
  })

  output$vote_section_ui <- renderUI({
    vtext_fi <- if (nzchar(fi_vote_text)) fi_vote_text else vote_text
    div(id = "vote-section",
        tags$div(class = "en-only", style = "color:rgba(255,255,255,0.8);font-family:Arial,Helvetica,sans-serif;line-height:1.65;", linkify(vote_text)),
        tags$div(class = "fi-only", style = "color:rgba(255,255,255,0.8);font-family:Arial,Helvetica,sans-serif;line-height:1.65;", linkify(vtext_fi))
    )
  })

  output$funding_ui <- renderUI({
    html_items <- vapply(fund_items, function(line) {
      stripped <- sub("^( +)", "", line)
      n_spaces <- nchar(line) - nchar(stripped)
      paste0(strrep("&nbsp;", n_spaces * 3), stripped)
    }, character(1), USE.NAMES = FALSE)
    fi_intro_eff <- if (nzchar(fi_fund_intro)) fi_fund_intro else fund_intro
    div(id = "funding-section",
        tags$div(class = "funding-body",
                 tags$div(class = "en-only", style = "margin-bottom:8px;line-height:1.6;", linkify(fund_intro)),
                 tags$div(class = "fi-only", style = "margin-bottom:8px;line-height:1.6;", linkify(fi_intro_eff)),
                 tags$div(style = "line-height:1.7;", HTML(paste(html_items, collapse = "<br>")))
        )
    )
  })
  
  outputOptions(output, "col_intro_ui",    suspendWhenHidden = FALSE)
  outputOptions(output, "vote_section_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "funding_ui",      suspendWhenHidden = FALSE)

  observeEvent(input$clicked_node_id, {
    id  <- as.numeric(input$clicked_node_id)
    nd  <- Filter(function(n) as.numeric(n$id) == id, g$nodes)
    if (length(nd) == 0) return()
    n   <- nd[[1]]
    grp <- n$group %||% "Theme"
    pre <- GROUP_PREFIX[[grp]]
    if (is.null(pre)) return()
    key <- paste0(pre, id)
    fi_key <- paste0("fi_", pre, id)
    session$sendCustomMessage("showDescPanel", list(
      title    = n$title %||% paste(grp, id),
      title_fi = n$title_fi %||% "",
      text     = desc_map[[key]] %||% "",
      text_fi  = desc_map[[fi_key]] %||% "",
      nodeId   = id,
      group    = grp
    ))
  })
}

shinyApp(ui, server)
