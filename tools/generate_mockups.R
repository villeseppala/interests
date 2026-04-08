
# tools/generate_mockups.R
# Run from pts/ root: Rscript tools/generate_mockups.R

source(file.path("shared", "layout.R"))
g <- read_graph(file.path("app_author", "data", "graph.json"))

schemes <- list(
  list(
    name    = "1_arctic",
    label   = "Arctic — muted pastels on slate",
    bg      = "#0d1f2d", sbg = "#060f17",
    theme   = "#a8edca", project = "#f7c59f", skill = "#9ad4f5"
  ),
  list(
    name    = "2_cosmic",
    label   = "Cosmic — purple-dark with lavender/amber/emerald",
    bg      = "#0f0b1e", sbg = "#07050f",
    theme   = "#c084fc", project = "#fbbf24", skill = "#34d399"
  ),
  list(
    name    = "3_forest",
    label   = "Forest — green-dark with mint/orange/sky",
    bg      = "#0b1f14", sbg = "#050f09",
    theme   = "#86efac", project = "#fb923c", skill = "#7dd3fc"
  ),
  list(
    name    = "4_ember",
    label   = "Ember — warm dark with green/yellow/pink",
    bg      = "#1a0f08", sbg = "#0d0704",
    theme   = "#4ade80", project = "#fde047", skill = "#f9a8d4"
  ),
  list(
    name    = "5_nordic",
    label   = "Nordic — blue-grey with soft blue/coral/green",
    bg      = "#1c2432", sbg = "#111820",
    theme   = "#93c5fd", project = "#fca5a5", skill = "#86efac"
  ),
  list(
    name    = "6_neon",
    label   = "Neon Night — near-black with high-contrast neon",
    bg      = "#0a0a14", sbg = "#050508",
    theme   = "#00ff9f", project = "#ff8c42", skill = "#00e5ff"
  )
)

out_dir <- file.path("tools", "mockups")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (s in schemes) {
  cd  <- build_cyto_data(g,
           col_bg = s$bg, col_sidebar_bg = s$sbg,
           col_theme = s$theme, col_project = s$project, col_skill = s$skill)
  svg <- generate_svg(cd)
  path <- file.path(out_dir, paste0(s$name, ".svg"))
  writeLines(svg, path)
  message("Saved: ", path, "  (", s$label, ")")
}

message("\nDone — open tools/mockups/*.svg in a browser to compare.")
