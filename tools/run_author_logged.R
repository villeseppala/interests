# tools/run_author_logged.R
# Launch the author app with R's errors / warnings ALSO written to tools/author_console.txt, so a
# startup failure can be read from a file. Browser-side (JavaScript) errors are logged by the app itself
# to tools/author_js_errors.txt.
# Run from the repo root:  source("tools/run_author_logged.R")
# Stop the app as usual (Esc / the stop button); the log is closed then.

local({
  log_path <- file.path("tools", "author_console.txt")
  con <- file(log_path, open = "wt")
  cat("=== author app started", format(Sys.time()), "===\n", file = con)
  old_opts <- options(shiny.fullstacktrace = TRUE, warn = 1)   # full traces; warnings printed as they happen
  # Messages (Shiny prints observer errors and warnings this way) go to the file; ordinary output still
  # shows in the console, and is copied to the file too (split = TRUE).
  sink(con, split = TRUE)
  sink(con, type = "message")
  tryCatch(
    shiny::runApp("app_author"),
    error = function(e) message("runApp error: ", conditionMessage(e)),
    finally = {
      sink(type = "message"); sink(); close(con); options(old_opts)
      cat("Log written to ", log_path, "\n", sep = "")
    }
  )
})
