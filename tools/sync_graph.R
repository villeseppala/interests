# tools/sync_graph.R
# Usage:
#   Rscript tools/sync_graph.R app_author/data/graph.json app_publish/www/graph.json

args <- commandArgs(trailingOnly = TRUE)
src <- if (length(args) >= 1) args[[1]] else "app_author/data/graph.json"
dst <- if (length(args) >= 2) args[[2]] else "app_publish/www/graph.json"

if (!file.exists(src)) stop("Missing source graph.json: ", src)
dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
file.copy(src, dst, overwrite = TRUE)
cat("Copied graph.json to:", dst, "\n")