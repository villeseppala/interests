# ────────────────────────────────────────────────────────────────────────────
# Where does one edit's R time go in the x-risk app? Native-R timing of the per-edit work
# (the cascade maths vs re-rendering the table), plus an Rprof breakdown of the renders.
# The webR (shinylive) build runs the same code roughly an order of magnitude slower, so the
# split here is what matters.
#
# Run from the project root:   source("app_xrisk/profile_panel.R")
# Writes:                      app_xrisk/profile_out.txt  (and app_xrisk/profile_rprof.out)
# ────────────────────────────────────────────────────────────────────────────

library(shiny)

app <- new.env()
sys.source("app_xrisk/app.R", envir = app)      # defines ui / server / helpers; the app is not launched

out_txt   <- "app_xrisk/profile_out.txt"
rprof_out <- "app_xrisk/profile_rprof.out"
N_RENDER  <- 20                                 # panel renders per horizon
N_COMPUTE <- 200                                # bare cascade computations per horizon

sink(out_txt)
cat("x-risk app: per-edit R time (native)  —  ", format(Sys.time()), "\n\n")

for (hs in c(3, 5)) {                           # 1 000 y and 100 000 y horizons
  M     <- app$model_for(hs)
  rates <- M$def_rates; pops <- M$def_pops

  # 1 · the maths alone (compute() runs twice per render: current + baseline)
  t_comp <- system.time(for (i in seq_len(N_COMPUTE)) app$compute(rates * (1 + i * 1e-6), pops, M))[["elapsed"]]

  # 2 · a full panel re-render per edit, driven through the real server function
  t_panel <- NA_real_; t_all <- NA_real_
  testServer(app$server, {
    session$setInputs(hscale = hs, vars = unname(app$VAR_ROWS), showref = FALSE)
    session$setInputs(state = list(rates = rates, pops = pops))
    invisible(output$panel)                                     # warm-up render

    if (hs == 5) Rprof(rprof_out, interval = 0.005)             # profile the heavier horizon
    t_panel <<- system.time(for (i in seq_len(N_RENDER)) {
      session$setInputs(state = list(rates = rates * (1 + i * 1e-4), pops = pops))
      invisible(output$panel)
    })[["elapsed"]]
    if (hs == 5) Rprof(NULL)

    t_all <<- system.time(for (i in seq_len(N_RENDER)) {       # panel + summary + footer, as one edit does
      session$setInputs(state = list(rates = rates * (1 - i * 1e-4), pops = pops))
      invisible(output$panel); invisible(output$summary); invisible(output$foot)
    })[["elapsed"]]
  })

  cat(sprintf("Horizon 10^%d years (%d checkpoints), all rows visible\n", hs, M$n))
  cat(sprintf("  compute() once            : %8.2f ms\n", 1000 * t_comp  / N_COMPUTE))
  cat(sprintf("  panel re-render per edit  : %8.2f ms\n", 1000 * t_panel / N_RENDER))
  cat(sprintf("  panel+summary+foot / edit : %8.2f ms\n\n", 1000 * t_all / N_RENDER))
}

cat("Rprof, 10^5-year horizon panel renders — top functions by total time\n")
print(head(summaryRprof(rprof_out)$by.total, 30))
cat("\nRprof — top functions by self time\n")
print(head(summaryRprof(rprof_out)$by.self, 30))
sink()

message("Done: see ", out_txt)
