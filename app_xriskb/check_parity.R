# ────────────────────────────────────────────────────────────────────────────
# Parity check: app_xriskb's JavaScript (xriskb.html: model, number formatting, cell HTML, arrow list) against
# app_xrisk's R on the same inputs. The JavaScript runs in V8, so no browser is involved.
#   - model: compute() at the checkpoints must agree to rounding error
#   - formatting, cell HTML and the arrow list must be identical strings
# The baseline display differs by design since 2026-09-25: xriskb shows the baseline on its own editable
# line under the delta, app_xrisk beside the value. So cells are compared without the baseline shown.
# Padding zeros too (2026-09-26): xriskb draws them fainter (FAINT), app_xrisk in the arrows' grey (ZERO).
# So the JavaScript is run with FAINT = ZERO.
#
# Needs the V8 package (install.packages("V8")). Run from the project root:
#   source("app_xriskb/check_parity.R")
# Writes app_xriskb/parity_out.txt
# ────────────────────────────────────────────────────────────────────────────

if (!requireNamespace("V8", quietly = TRUE)) stop('This check needs the V8 package: install.packages("V8")')
library(shiny)

app <- new.env()
sys.source("app_xrisk/app.R", envir = app)      # the R reference; the app is not launched

# The page's script up to its "state" section is pure (model, formatting, cells, arrows): load that into V8.
src <- readLines("app_xriskb/xriskb.html", encoding = "UTF-8")
s   <- which(src == "<script>")[1]
e   <- which(src == "</script>"); e <- e[e > s][1]
js  <- src[(s + 1):(e - 1)]
cut <- grep("^// ── state, baselines and history", js)
ctx <- V8::v8()
ctx$eval(paste(js[seq_len(cut - 1)], collapse = "\n"))
ctx$eval("FAINT = ZERO;")                          # padding zeros in app_xrisk's grey (see the header)

# R values → JavaScript literals, numbers at full precision
js_val <- function(x) {
  if (is.null(x)) return("null")
  if (is.logical(x)) return(if (length(x) == 1) tolower(x) else paste0("[", paste(tolower(x), collapse = ","), "]"))
  if (is.numeric(x)) { v <- sprintf("%.17g", x); return(if (length(x) == 1) v else paste0("[", paste(v, collapse = ","), "]")) }
  q <- vapply(x, function(s) as.character(jsonlite::toJSON(s, auto_unbox = TRUE)), "")
  if (length(x) == 1) q else paste0("[", paste(q, collapse = ","), "]")
}
js_obj <- function(l) paste0("{", paste(sprintf("%s: %s", names(l), vapply(l, js_val, "")), collapse = ", "), "}")
js_str <- function(expr) enc2utf8(as.character(ctx$eval(expr)))

out <- "app_xriskb/parity_out.txt"
sink(out)
cat("app_xriskb (JavaScript) vs app_xrisk (R)  —  ", format(Sys.time()), "\n\n")
n_bad <- 0; n_all <- 0
report <- function(label, ok, detail = NULL) {
  n_all <<- n_all + 1; ok <- isTRUE(ok)                   # NA (e.g. a NaN difference) counts as a mismatch
  cat(sprintf("  %-66s %s\n", label, if (ok) "ok" else "MISMATCH"))
  if (!ok) { n_bad <<- n_bad + 1; if (length(detail)) cat(paste0("      ", detail), sep = "\n") }
}
same_str <- function(label, r, j) {                   # identical strings; else show where they part ways
  r <- enc2utf8(as.character(r)); j <- enc2utf8(j)
  if (identical(r, j)) return(report(label, TRUE))
  a <- strsplit(r, "")[[1]]; b <- strsplit(j, "")[[1]]; n <- min(length(a), length(b))
  i <- which(a[seq_len(n)] != b[seq_len(n)])[1]; if (is.na(i)) i <- n + 1
  ctxt <- function(v) paste(v[max(1, i - 50):min(length(v), i + 50)], collapse = "")
  report(label, FALSE, c(paste("first difference at character", i), paste("R : ", ctxt(a)), paste("JS: ", ctxt(b))))
}

# ── 1 · the model ──
cat("Model: compute() at the checkpoints (tolerance: relative 1e-10)\n")
cases <- list(
  list(e = 3, rates = c(0.0005, 0.0005, 0.0001),                   pops = rep(8.3e9, 4)),
  list(e = 5, rates = c(0.0005, 0.0005, 0.0001, 0.00005, 0.00001), pops = c(8.3e9, 8.5e9, 9e9, 7e9, 1e10, 1.2e10)),
  list(e = 2, rates = c(0.01, 0.002),                              pops = c(8.3e9, 9e9, 1e10)),
  list(e = 4, rates = c(0, 0.001, 0.0003, 0.00002),                pops = c(8.3e9, 8.3e9, 5e9, 5e9, 2e10)))
fields <- c(surv = "surv", pop_pot = "popPot", pop_exp = "popExp", cum_years = "cumYears", cum_ly = "cumLy",
            cum_ly_pot = "cumLyPot", lives = "lives", gens = "gens", lives_pot = "livesPot", growth = "growth")
for (cs in cases) {
  r <- app$compute(cs$rates, cs$pops, app$model_for(cs$e))
  j <- jsonlite::fromJSON(js_str(sprintf("JSON.stringify(compute(%s, %s, modelFor(%d)))",
                                         js_val(cs$rates), js_val(cs$pops), cs$e)))
  rel <- vapply(names(fields), function(f) {
    a <- r[[f]]; b <- as.numeric(j[[fields[[f]]]])
    if (f == "growth") { a <- a[-1]; b <- b[-1] }                     # growth[1] is NA / NaN in both
    if (length(a) != length(b)) return(Inf)
    max(c(0, abs(a - b) / pmax(abs(a), 1e-300)))
  }, 0)
  report(sprintf("horizon 10^%d, rates %s", cs$e, paste(cs$rates, collapse = " / ")), max(rel) < 1e-10,
         sprintf("worst: %s, relative difference %.3g", names(which.max(rel)), max(rel)))
}

# ── 2 · number formatting ──
cat("\nFormatting (identical strings)\n")
for (x in c(0, 999, 1000, 1234567, 8.3e9, 7604252997099.4, 1e15 + 0.3))
  same_str(sprintf("f_int(%s)", format(x, scientific = FALSE)), app$f_int(x), js_str(sprintf("fInt(%s)", js_val(x))))
for (y in c(2026, 12026, 102026))
  same_str(sprintf("fmt_year(%d)", y), app$fmt_year(y), js_str(sprintf("fmtYear(%d)", y)))

# ── 3 · value cells ──
cat("\nValue cells (identical HTML)\n")
edit_cases <- list(
  "rate, lowered"                 = list(col = 3, cur = 0.00049, ref = 0.0005, kind = "rate", vi = 1, to_disp = 100, decimals = 4,
                                         suffix = "%", good = "more_bad", color = app$COL_RATE,
                                         top = "<span class='ratio'>1 in 2 041</span>"),
  "rate_c (annual survival)"      = list(col = 5, cur = 0.99951, ref = 0.9995, kind = "rate_c", vi = 2, to_disp = 100, decimals = 4,
                                         suffix = "%", good = "more_good", color = app$COL_RATE),
  "surv, padded"                  = list(col = 4, cur = 0.995011, ref = 0.9950113, kind = "surv", vi = 1, to_disp = 100, decimals = 4,
                                         suffix = "%", pad = 2, good = "more_good", color = app$COL_SURV),
  "surv at 100%"                  = list(col = 6, cur = 1, ref = 0.99, kind = "surv", vi = 2, to_disp = 100, decimals = 4,
                                         suffix = "%", pad = 2, good = "more_good", color = app$COL_SURV),
  "pop, raised"                   = list(col = 4, cur = 8.3e9, ref = 8.25e9, kind = "pop", vi = 2, to_disp = 1 / 1e6, decimals = 0,
                                         suffix = "m", pad = 5, good = "more_good", color = app$COL_POP),
  "pop, 5+ digits"                = list(col = 6, cur = 12345e6, ref = 8.3e9, kind = "pop", vi = 3, to_disp = 1 / 1e6, decimals = 0,
                                         suffix = "m", pad = 5, good = "more_good", color = app$COL_POP))
js_names <- c(to_disp = "toDisp")
to_js <- function(l) { n <- names(l); hit <- n %in% names(js_names); n[hit] <- js_names[n[hit]]; names(l) <- n; l }
for (nm in names(edit_cases)) {
  a <- edit_cases[[nm]]
  same_str(paste("gcell_edit:", nm), do.call(app$gcell_edit, a), js_str(sprintf("gcellEdit(%s)", js_obj(to_js(a)))))
}
ro_cases <- list(
  "years"                  = list(col = 6, cur = 916.1751234, ref = 916.2, to_disp = 1, decimals = 4, suffix = "y", kind = "more_good",
                                  sub = "of 1 000y potential", color = app$COL_SURV),
  "population, no change"  = list(col = 4, cur = 8258593251.3, ref = 8258593251.3, to_disp = 1, decimals = 0, suffix = "",
                                  kind = "more_good", sub = "of 8 300 000 000 potential", color = app$COL_POP),
  "life-years, lowered"    = list(col = 8, cur = 7604252997099.4, ref = 7604260602009.1, to_disp = 1, decimals = 0, suffix = "",
                                  kind = "more_good", color = app$COL_POP),
  "first checkpoint"       = list(col = 2, cur = 0, ref = 0, to_disp = 1, decimals = 0, suffix = "", delta = FALSE, color = app$COL_POP))
for (nm in names(ro_cases)) {
  a <- ro_cases[[nm]]
  same_str(paste("gcell_ro:", nm), do.call(app$gcell_ro, a), js_str(sprintf("gcellRo(%s)", js_obj(to_js(a)))))
}
same_str("gcell_fixed: 2026 survival", app$gcell_fixed(2, 1, 100, 4, "%", app$COL_SURV, pad = 2),
         js_str(sprintf("gcellFixed(2, 1, 100, 4, '%%', %s, 2)", js_val(app$COL_SURV))))

# ── 4 · the arrow list ──
cat("\nArrow edges (identical strings)\n")
vis_cases <- list(unname(app$VAR_ROWS), unname(app$VAR_ON), c("rate", "years", "popexp", "lives"),
                  c("pop", "popexp", "lives"), c("surv", "gens", "ly"))
for (v in vis_cases) for (n in c(4, 6))
  same_str(sprintf("arrow_edges(%s, n = %d)", paste(v, collapse = ","), n), app$arrow_edges(v, n),
           js_str(sprintf("arrowEdges(%s, %d)", js_val(v), n)))

cat(sprintf("\n%d of %d checks passed%s\n", n_all - n_bad, n_all, if (n_bad) "  —  see MISMATCH lines above" else ""))
sink()
message("Done: see ", out, if (n_bad) sprintf("  (%d mismatch%s)", n_bad, if (n_bad > 1) "es" else "") else "  (all passed)")
