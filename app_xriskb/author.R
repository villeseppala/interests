# ────────────────────────────────────────────────────────────────────────────
# Author mode for xriskb.html in any browser, Positron's own included: serves app_xriskb/ on this machine only
# and lets the page's Save settings write app_xriskb/xriskb_settings.js. (A page can't write files by itself:
# Chrome and Edge can ask for one, Positron's browser and viewer can't. The role app_author's server has for
# graph.json, for this one file.)
# Run from the project root:
#   source("app_xriskb/author.R")
# It opens http://127.0.0.1:7788/xriskb.html?author (in Positron, in its Viewer; the address works in any
# browser while this runs). Press Esc in the console to stop it. Needs httpuv, which Shiny already installs.
# ────────────────────────────────────────────────────────────────────────────

local({
  DIR  <- normalizePath("app_xriskb", winslash = "/", mustWork = TRUE)
  SAVE <- file.path(DIR, "xriskb_settings.js")
  TYPES <- c(html = "text/html; charset=utf-8", js = "text/javascript; charset=utf-8", css = "text/css; charset=utf-8",
             json = "application/json", svg = "image/svg+xml", png = "image/png")
  reply <- function(status, body, type = "text/plain; charset=utf-8")
    list(status = status, headers = list("Content-Type" = type, "Cache-Control" = "no-store"), body = body)

  app <- list(call = function(req) {
    path <- utils::URLdecode(req$PATH_INFO)
    if (path == "/save-settings") {
      # The custom header can't be sent by another site's page without a preflight, which is refused below
      if (req$REQUEST_METHOD != "POST" || is.null(req$HTTP_X_XRISKB_SAVE)) return(reply(405L, "not allowed"))
      body <- req$rook.input$read()
      txt  <- rawToChar(body); Encoding(txt) <- "UTF-8"
      if (!grepl("window.XRISKB_SETTINGS", txt, fixed = TRUE)) return(reply(400L, "not a settings file"))
      writeBin(body, SAVE)                         # the page's bytes as they are (UTF-8, LF)
      message(format(Sys.time(), "%H:%M:%S"), "  saved ", SAVE)
      return(reply(200L, "saved"))
    }
    if (req$REQUEST_METHOD != "GET") return(reply(405L, "not allowed"))
    if (path == "/") path <- "/xriskb.html"
    f <- normalizePath(file.path(DIR, sub("^/+", "", path)), winslash = "/", mustWork = FALSE)
    if (!startsWith(f, paste0(DIR, "/")) || !file.exists(f) || dir.exists(f)) return(reply(404L, "not found"))
    type <- TYPES[tolower(tools::file_ext(f))]
    reply(200L, readBin(f, "raw", file.info(f)$size), if (is.na(type)) "application/octet-stream" else unname(type))
  })

  port <- 7788L
  srv  <- tryCatch(httpuv::startServer("127.0.0.1", port, app), error = function(e) NULL)
  if (is.null(srv)) { port <- httpuv::randomPort(); srv <- httpuv::startServer("127.0.0.1", port, app) }   # 7788 taken
  url <- sprintf("http://127.0.0.1:%d/xriskb.html?author", port)
  message("Author mode: ", url, "\nSave settings writes ", SAVE, "\nPress Esc to stop.")
  utils::browseURL(url)
  tryCatch(repeat httpuv::service(100),
           interrupt = function(e) message("Stopped."),
           finally = httpuv::stopServer(srv))
})
