# tools/diag_light_colors.R — one-shot diagnostic for the light-colour load path.
# Run from the repo root:  source("tools/diag_light_colors.R")
# Writes tools/diag_light_colors.txt — send/show that file.

source(file.path("shared", "layout.R"))

out <- character(0)
say <- function(...) out <<- c(out, paste0(...))

for (gp in c(file.path("app_author", "data", "graph.json"),
             file.path("app_publish", "www", "graph.json"))) {
  say("==================================================")
  say("GRAPH: ", gp)
  if (!file.exists(gp)) { say("  MISSING"); next }
  g  <- read_graph(gp)
  ly <- g$layout

  say("-- raw layout values as read by the app (ly$...) --")
  for (k in c("col_bg","col_column_bg","col_theme","col_project","col_skill",
              "light_col_bg","light_col_sidebar_bg","light_col_node_bg",
              "light_col_column_bg","light_col_theme","light_col_project","light_col_skill")) {
    v <- ly[[k]]
    say(sprintf("  %-22s %s", k, if (is.null(v)) "<NULL>  (default will be used)" else as.character(v)))
  }

  # Exactly what the author app's cyto_data() passes (input$ is NULL outside the app,
  # so this is the `%||% rv$g$layout$... %||% hard-default` branch).
  cd <- build_dual_cyto_data(g,
    col_bg              = ly$col_bg              %||% "#0b3552",
    col_sidebar_bg      = ly$col_sidebar_bg      %||% "#081626",
    col_node_bg         = ly$col_node_bg         %||% "#081626",
    col_column_bg       = ly$col_column_bg       %||% "#000000",
    col_theme           = ly$col_theme           %||% "#3be37a",
    col_project         = ly$col_project         %||% "#ffad33",
    col_skill           = ly$col_skill           %||% "#78e6e7",
    light_col_bg        = ly$light_col_bg        %||% "#f0f4f8",
    light_col_sidebar_bg= ly$light_col_sidebar_bg%||% "#e2eaf3",
    light_col_node_bg   = ly$light_col_node_bg   %||% "#e2eaf3",
    light_col_column_bg = ly$light_col_column_bg %||% "#000000",
    light_col_theme     = ly$light_col_theme     %||% "#1e7c45",
    light_col_project   = ly$light_col_project   %||% "#c06000",
    light_col_skill     = ly$light_col_skill     %||% "#1a7a7b")

  say("-- what build_dual_cyto_data emits to render.js --")
  for (k in c("colBg","colColumnBg","colTheme","colProject","colSkill",
              "lightColBg","lightColSidebarBg","lightColNodeBg",
              "lightColColumnBg","lightColTheme","lightColProject","lightColSkill")) {
    v <- cd[[k]]
    say(sprintf("  %-20s %s", k, if (is.null(v)) "<<< MISSING FROM PAYLOAD >>>" else as.character(v)))
  }
}

writeLines(out, file.path("tools", "diag_light_colors.txt"))
cat(paste(out, collapse = "\n"), "\n")
cat("\nWrote tools/diag_light_colors.txt\n")
