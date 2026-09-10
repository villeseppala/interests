# tools/check_aspect.R
# Verifies the wide/tall (aspect-aware layout) wiring without launching the apps.
#   1. syntax-checks every R file the feature touched
#   2. shows the wide and tall value of each aspect-aware setting, per graph.json copy
#   3. builds the payload and reports whether a tall layout / aspectVars block came out
# Run from the repo root:  source("tools/check_aspect.R")
# Writes tools/check_aspect.txt (and echoes the same text to the console).
#
# No sink() here on purpose: lines are collected in memory and written once at the end, so a failure
# part-way still produces a readable file and can never leave your console redirected.

local({

  OUT   <- file.path("tools", "check_aspect.txt")
  lines <- character(0)
  say   <- function(...) lines <<- c(lines, paste0(...))
  # Run one step; on error record the message and keep going instead of aborting the whole report.
  step  <- function(label, expr) {
    tryCatch(expr, error = function(e) say("  ERROR in ", label, ": ", conditionMessage(e)))
  }
  flush_out <- function() {
    writeLines(lines, OUT, useBytes = TRUE)
    cat(paste(lines, collapse = "\n"), "\n\nWrote ", OUT, "\n", sep = "")
  }
  on.exit(flush_out(), add = TRUE)   # inside local(), so this really does fire

  say("=== 1. Syntax check ===")
  say("  (parses each file; a FAIL here means the app will not launch)")
  files <- c("shared/layout.R", "app_author/app.R", "app_publish/app.R", "tools/build_static.R")
  for (f in files) {
    if (!file.exists(f)) { say("  MISSING ", f); next }
    msg <- tryCatch({ parse(f); NULL }, error = function(e) conditionMessage(e))
    if (is.null(msg)) say("  ok    ", f)
    else              say("  FAIL  ", f, "\n          ", gsub("\n", "\n          ", msg))
  }

  say("", "=== 2. Aspect values per graph.json ===")
  ok_src <- tryCatch({ source(file.path("shared", "layout.R")); TRUE },
                     error = function(e) { say("  could not source shared/layout.R: ",
                                               conditionMessage(e)); FALSE })
  if (!ok_src) return(invisible())

  copies <- c(publish = "app_publish/www/graph.json", author = "app_author/data/graph.json")
  for (nm in names(copies)) {
    p <- copies[[nm]]
    say("", paste0("-- ", nm, " -- ", p))
    if (!file.exists(p)) { say("  (missing)"); next }

    step(paste0(nm, " / read+resolve"), {
      g  <- read_graph(p)
      ly <- g$layout
      av <- aspect_values(ly)

      say(sprintf("  %-18s %-12s %-12s %s", "setting", "wide", "tall", "saved tall_*?"))
      for (k in ASPECT_INPUTS) {
        w <- av$wide[[k]]; t <- av$tall[[k]]
        fmt <- function(x) if (is.null(x)) "NULL" else paste(format(x), collapse = ",")
        say(sprintf("  %-18s %-12s %-12s %s%s", k, fmt(w), fmt(t),
                    if (is.null(ly[[paste0("tall_", k)]])) "no (follows wide)" else "yes",
                    if (!identical(w, t)) "   <- DIFFERS" else ""))
      }

      say("", "  Payload build:")
      aw <- av$wide
      cd <- build_dual_cyto_data(
        g,
        gap_v = aw$gap_v, gap_col = aw$gap_col,
        font_node = aw$font_node, font_project = aw$font_project,
        font_ptype = aw$font_ptype, font_subs = aw$font_subs,
        font_desc = ly$font_desc %||% 18,
        h_theme = aw$h_theme, h_project = aw$h_project, h_skill = aw$h_skill,
        w_project = aw$w_project, w_node = aw$w_node,
        center_cols = isTRUE(aw$center_cols), headers_on_stack = isTRUE(aw$headers_on_stack),
        tall_over = av$tall
      )
      cd$aspectVars <- aspect_vars_payload(av)

      say("    payload$tall present: ", !is.null(cd$tall),
          if (is.null(cd$tall)) "   (expected while no tall value differs)" else "")
      if (!is.null(cd$tall))
        say("    tall nodes: ", length(cd$tall$nodes), "   wide nodes: ", length(cd$nodes))
      kv <- function(l) if (!length(l)) "(none)" else
        paste(names(l), unlist(l), sep = "=", collapse = ", ")
      say("    aspectVars$wide: ", kv(cd$aspectVars$wide))
      say("    aspectVars$tall: ", kv(cd$aspectVars$tall))
    })
  }

  say("", "=== 3. Registry ===")
  say("  ASPECT_BUILD_INPUTS : ", paste(ASPECT_BUILD_INPUTS,  collapse = ", "))
  say("  ASPECT_CLIENT_INPUTS: ", paste(ASPECT_CLIENT_INPUTS, collapse = ", "))
  say("", "Done.")
})
