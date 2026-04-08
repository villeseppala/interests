# tools/run_static.R
# Build the static site and serve it locally for preview.
# Source this file from the project root (or run: Rscript tools/run_static.R)
#
# Requires the 'servr' package:  install.packages("servr")

library(here)

# 1 — Build payload.json + copy assets into site/
message("Building static site...")
source(here("tools", "build_static.R"))

# 2 — Serve site/ on localhost and open in browser
message("Serving site/ at http://127.0.0.1:4321  (Ctrl+C to stop)")
servr::httd(here("site"), port = 4321, browser = TRUE)
