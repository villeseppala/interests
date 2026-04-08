# tools/extract_descriptions.R
# Parses docs/descriptions.qmd and writes descriptions.json
#
# Supports section anchors:
#   {#p201}  — Project 201
#   {#t101}  — Theme 101
#   {#s301}  — Skill 301
#
# Sections tagged with .draft are SKIPPED (not exported):
#   ## My rough idea {#p999 .draft}
#
# Sections stop at ANY heading line (with or without anchor),
# so ### subheadings and # group headings act as boundaries.
#
# Usage:
#   Rscript tools/extract_descriptions.R [input.qmd] [output.json]

args     <- commandArgs(trailingOnly = TRUE)
in_path  <- if (length(args) >= 1) args[[1]] else "docs/descriptions.qmd"
out_path <- if (length(args) >= 2) args[[2]] else "app_publish/www/descriptions.json"

if (!file.exists(in_path)) stop("Input file not found: ", in_path)

lines <- readLines(in_path, warn = FALSE)

# Match anchored headings: ## Title {#id} or ## Title {#id .draft}
anchor_re <- "^(#{1,6})\\s+.*\\{#([A-Za-z0-9_-]+)(\\s+[^}]*)?\\}\\s*$"
# Section boundaries: level 1-2 headings only (# and ##)
# Level 3+ (###, ####) are kept as content within descriptions
boundary_re <- "^#{1,2}\\s+"

ids    <- character(0)
starts <- integer(0)
drafts <- logical(0)

for (i in seq_along(lines)) {
  m <- regexec(anchor_re, lines[[i]], perl = TRUE)
  r <- regmatches(lines[[i]], m)[[1]]
  if (length(r) > 0) {
    ids    <- c(ids, r[[3]])
    starts <- c(starts, i)
    attrs  <- trimws(r[[4]])
    drafts <- c(drafts, grepl("\\.draft", attrs))
  }
}

if (length(ids) == 0) {
  cat("No headings with {#id} found in:", in_path, "\n")
  cat("Writing empty JSON.\n")
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  writeLines("{}", out_path)
  quit(status = 0)
}

# Build list of boundary heading line numbers (level 1-2 only)
all_heading_lines <- integer(0)
for (i in seq_along(lines)) {
  if (grepl(boundary_re, lines[[i]])) all_heading_lines <- c(all_heading_lines, i)
}

extract_section <- function(idx) {
  start_line <- starts[[idx]] + 1
  # Find the next heading of any kind after this section's start
  later <- all_heading_lines[all_heading_lines > starts[[idx]]]
  end_line <- if (length(later) > 0) later[1] - 1 else length(lines)
  if (start_line > end_line) return("")
  txt <- paste(lines[start_line:end_line], collapse = "\n")
  txt <- sub("^\\s+", "", txt)
  txt <- sub("\\s+$", "", txt)
  txt
}

# Only export non-draft sections
keep <- !drafts
desc <- setNames(lapply(which(keep), extract_section), ids[keep])

n_total    <- length(ids)
n_draft    <- sum(drafts)
n_exported <- sum(keep)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

writeLines(
  jsonlite::toJSON(desc, auto_unbox = TRUE, pretty = TRUE),
  out_path,
  useBytes = TRUE
)

cat(sprintf("Wrote: %s (%d sections exported, %d drafts skipped, %d total)\n",
            out_path, n_exported, n_draft, n_total))