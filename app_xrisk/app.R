# ────────────────────────────────────────────────────────────────────────────
# Extinction-risk visualiser — replica of the right-hand data panel from the
# concept slide. Recomputes the whole cascade in R from the per-period annual
# extinction rates + population checkpoints (2-4 periods, set by the time-scale
# slider: 100 / 1 000 / 10 000 / 100 000 years), so it also serves as a clean
# recomputation to diff against the slide.
#
# Interaction (JavaScript): click an "Annual extinction probability" cell to
# select a period, then:
#   ← / →  move selection between periods
#   ↑ / ↓  raise / lower that period's annual rate (Shift = fine step)
# The whole table + comparison deltas update live. "Set baseline" freezes the
# current state as the comparison reference; "Reset" restores defaults.
# The "Variables" multi-select chooses which metric rows are shown (and so editable).
# Values are boxed, with SVG arrows from each value to the values it is computed from ("Show arrows").
#
# Run:  shiny::runApp("app_xrisk")
# ────────────────────────────────────────────────────────────────────────────

library(shiny)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── model config ────────────────────────────────────────────────────────────
# The horizon is a power of ten (100 / 1 000 / 10 000 / 100 000 years) chosen by the reader; the checkpoints
# are START plus every power of ten up to it, so the periods are always 10, 90, 900, 9000 years.
# Everything that depends on the horizon lives in the list model_for() returns — nothing below
# assumes a fixed number of columns.
START     <- 2026
SCALE_EXP <- 2:5                                 # slider positions: 10^2 .. 10^5 years
DEF_EXP   <- 3                                   # default horizon 1 000 years (lighter than 10 000)
ALL_RATES <- c(0.0005, 0.0005, 0.0001, 0.00005, 0.00001)  # 0.05%, 0.05%, 0.01%, 0.005%, 0.001% per year, by period
POP0      <- 8.3e9                               # constant 8300 M default at every checkpoint
LIFE_EXP  <- 75

model_for <- function(scale_exp) {
  cps       <- START + c(0, 10^seq_len(scale_exp))       # 2026, 2036, 2126, (3026, (12026, (102026)))
  n         <- length(cps)
  seg_start <- cps[-n] + 1                                # first year of each period
  seg_end   <- cps[-1]                                    # last  year of each period
  list(n = n, cps = cps, seg_start = seg_start, seg_end = seg_end,
       seg_len   = seg_end - seg_start + 1,               # 10, 90, 900, ...
       yrs_all   = START:max(cps),
       def_rates = ALL_RATES[seq_len(n - 1)],
       def_pops  = rep(POP0, n))
}

# Coerce a {rates, pops} state to a model's checkpoint count: keep whatever the reader already set
# for the periods both horizons share, and fill the rest with defaults. Used when the horizon
# changes (edits to the first periods survive) and to keep rendering coherent in the moment
# between the slider moving and the client sending back a resized state.
fit_state <- function(s, M) {
  fill <- function(x, def) { k <- min(length(x), length(def)); def[seq_len(k)] <- x[seq_len(k)]; def }
  list(rates = fill(as.numeric(s$rates), M$def_rates),
       pops  = fill(as.numeric(s$pops),  M$def_pops))
}

# potential population per year: geometric interpolation between the (editable)
# checkpoint values, so the potential-population column stays internally
# consistent — it hits the checkpoint values exactly at the checkpoint years.
build_pop <- function(pop_cps, M) {
  yrs <- M$yrs_all; cps <- M$cps
  pop <- numeric(length(yrs))
  for (k in seq_len(length(cps) - 1)) {
    y0 <- cps[k]; y1 <- cps[k + 1]; v0 <- pop_cps[k]; v1 <- pop_cps[k + 1]
    ix <- which(yrs >= y0 & yrs <= y1)
    pop[ix] <- v0 * (v1 / v0) ^ ((yrs[ix] - y0) / (y1 - y0))
  }
  pop
}

# ── the cascade ─────────────────────────────────────────────────────────────
compute <- function(rates, pop_cps, M) {
  yrs <- M$yrs_all
  POP_ALL <- build_pop(pop_cps, M)
  seg  <- findInterval(yrs, M$seg_start)             # 0 for 2026, else 1..n-1
  r    <- ifelse(seg >= 1, rates[pmax(seg, 1)], 0)
  surv <- cumprod(1 - r)                             # survival to end of year
  pop_exp <- POP_ALL * surv
  lived   <- seg >= 1                                # years that actually count
  cum_years  <- cumsum(ifelse(lived, surv,    0))
  cum_ly     <- cumsum(ifelse(lived, pop_exp, 0))
  cum_ly_pot <- cumsum(ifelse(lived, POP_ALL, 0))
  ix <- match(M$cps, yrs)
  growth <- c(NA, (pop_cps[-1] / pop_cps[-length(pop_cps)]) ^ (1 / diff(M$cps)) - 1)
  list(
    rates      = rates,
    pop_cps    = pop_cps,
    growth     = growth,
    surv       = surv[ix],
    pop_pot    = POP_ALL[ix],
    pop_exp    = pop_exp[ix],
    cum_years  = cum_years[ix],
    cum_ly     = cum_ly[ix],
    cum_ly_pot = cum_ly_pot[ix],
    lives      = cum_ly[ix]     / LIFE_EXP,
    gens       = cum_years[ix]  / LIFE_EXP,
    lives_pot  = cum_ly_pot[ix] / LIFE_EXP
  )
}

# ── formatting ──────────────────────────────────────────────────────────────
f_rate  <- function(x) paste0(formatC(x * 100, digits = 4, format = "f"), "%")
f_pct   <- function(x) paste0(formatC(x * 100, digits = 4, format = "f"), "%")
f_int   <- function(x) formatC(round(x), format = "f", digits = 0, big.mark = " ")
f_years <- function(x) paste0(formatC(x, digits = 4, format = "f", big.mark = " "), "y")
f_pop_m <- function(x) paste0(f_int(x / 1e6), "m")
f_ly    <- function(x) paste0(f_int(x), " ly")

# metric rows, in display order: data-row id -> label in the "Variables" dropdown
VAR_ROWS <- c("Annual extinction probability"   = "rate",
              "Cumulative survival probability" = "surv",
              "Cumulative survived years"       = "years",
              "Cumulative survived generations" = "gens",
              "Annual population potential"     = "pop",
              "Annual population"               = "popexp",
              "Cumulative life-years"           = "ly",
              "Cumulative lives"                = "lives")
# rows shown on first load (the rest start unticked in the Variables menu)
VAR_ON <- setdiff(VAR_ROWS, c("gens", "ly"))

# ── dependency arrows ───────────────────────────────────────────────────────
# Nodes are "row:j" (checkpoint j; for the rate row, period j). For checkpoints j >= 2, each value is
# computed from its row's previous value ("h", across) and from values in rows above it (cross-row):
cross_src <- function(row, j) switch(row,
  surv   = paste0("rate:", j - 1),                  # cs_t  <- ae over the period ending at t
  years  = paste0("surv:", j),                      # Y_t   <- cs_t
  gens   = paste0("surv:", j),                      # G_t   <- cs_t  (G_t = G_{t-1} + sum of cs over the period / 75)
  popexp = paste0(c("pop:", "surv:"), j),           # E_t   <- P_t, cs_t
  ly     = paste0("popexp:", j),                    # H_t   <- E_t
  lives  = paste0("popexp:", j),                    # N_t   <- E_t  (N_t = N_{t-1} + sum of E over the period / 75)
  NULL)
HORIZ_ROWS <- c("surv", "years", "gens", "ly", "lives")   # running totals: each value builds on its previous value

# Edge list for the visible rows, as "from,to,type;..." — type h (across), v (from the row directly
# above) or d (skips over visible rows, so the JS routes it round the side instead of through them).
# A source in a hidden row is replaced by what that row was itself derived from (recursively, cross-row
# only); if nothing visible remains, the arrow is dropped.
arrow_edges <- function(vis, n) {
  expand <- function(id) {
    row <- sub(":.*", "", id)
    if (row %in% vis) return(id)
    unique(unlist(lapply(cross_src(row, as.integer(sub(".*:", "", id))), expand)))
  }
  ord <- VAR_ROWS[VAR_ROWS %in% vis]
  out <- character(0)
  for (row in vis) for (j in seq_len(n)[-1]) {
    if (row %in% HORIZ_ROWS) out <- c(out, paste(paste0(row, ":", j - 1), paste0(row, ":", j), "h", sep = ","))
    for (s in unique(unlist(lapply(cross_src(row, j), expand)))) {
      srow  <- sub(":.*", "", s)
      skips <- match(row, ord) - match(srow, ord) > 1
      out <- c(out, paste(s, paste0(row, ":", j), if (skips) "d" else "v", sep = ","))
    }
  }
  paste(unique(out), collapse = ";")
}

# colours
COL_RATE  <- "#e8836f"; COL_SURV <- "#4fb8d6"; COL_POP <- "#e46ba6"
GOOD <- "#7fd18f"; BAD <- "#ee4d4d"; ZERO <- "#8a97a3"

# ── value cells, built as HTML text ─────────────────────────────────────────
# The panel re-renders on every edit and holds thousands of per-digit elements. Building them as
# htmltools tag objects cost ~0.7-1 s per edit natively (several times that under webR/shinylive),
# almost all in tag construction and serialisation, so the cells are pasted as HTML strings instead
# and handed to the tag tree as HTML(). Same elements and classes as before.

# calendar year: thin-space separator only for 5+ digit years (12026 → "12 026")
fmt_year <- function(y) { y <- as.integer(y); if (y >= 10000) group3(as.character(y)) else as.character(y) }

# insert thin-space thousands separators into a plain digit string
group3 <- function(s) {
  ch <- rev(strsplit(s, "")[[1]]); out <- character(0)
  for (i in seq_along(ch)) { if (i > 1 && (i - 1) %% 3 == 0) out <- c(out, " "); out <- c(out, ch[i]) }
  paste(rev(out), collapse = "")
}

sty_col <- function(col) if (is.null(col)) "" else paste0(' style="color:', col, ';"')

# build digit slots for a number — used for both the value and its (editable) delta. Returns HTML text.
#   native      : native value; abs() is shown, sign handled via sign_char
#   to_disp     : multiplier to display units (rate 100 → %, pop 1/1e6 → m)
#   decimals    : decimal places in display units
#   pad         : if set, always show this many integer digits (leading zeros greyed
#                 but still adjustable — lets the user ramp big places fast)
#   digit_color : function(is_zero, is_lead) -> CSS colour or NULL (inherit)
#   sign_char/sign_color : optional leading sign slot (for deltas)
# ref_txt: baseline value shown in parentheses as one more slot after the suffix, level with the digit
# row (it is a column like the digits, with the same arrow spacers). ref_ghost = TRUE renders it invisible:
# the delta row carries that ghost so its digits still sit exactly under the value's digits.
build_slots <- function(native, to_disp, decimals, suffix, pad = NULL, editable = TRUE,
                        digit_color = NULL, sign_char = NULL, sign_color = NULL,
                        ref_txt = NULL, ref_ghost = FALSE) {
  a <- abs(native)
  whole <- formatC(a * to_disp, format = "f", digits = decimals)   # non-scientific; rounds
  parts <- strsplit(whole, ".", fixed = TRUE)[[1]]
  ip    <- parts[1]
  if (!is.null(pad) && nchar(ip) < pad) ip <- paste0(strrep("0", pad - nchar(ip)), ip)
  ip    <- group3(ip)                                              # thin-space thousands on integer part
  s     <- if (length(parts) > 1) paste0(ip, ".", parts[2]) else ip
  chars <- strsplit(s, "")[[1]]
  n     <- length(chars)
  isdig <- grepl("[0-9]", chars)
  dot   <- match(".", chars); if (is.na(dot)) dot <- n + 1
  place <- rep(NA_real_, n)                         # place value of each digit, in native units
  ii <- which(isdig & seq_len(n) < dot)             # integer digits: 10^0 at the right end
  place[ii] <- 10^(rev(seq_along(ii)) - 1) / to_disp
  di <- which(isdig & seq_len(n) > dot)             # decimal digits: 10^-1, 10^-2, ... left → right
  place[di] <- 10^(-seq_along(di)) / to_disp
  lead <- isdig & cumsum(isdig & chars != "0") == 0 # zeros before the first significant digit
  # editable digits carry ▲/▼ arrows (with matching spacers on separators so rows line up);
  # read-only digits carry neither, so those rows have no wasted vertical space.
  # decimal points / thousands separators / the suffix follow the digits' colour
  # (e.g. a grey delta's "." and "%" render in the same grey as its zeros)
  sep_col <- if (!is.null(digit_color)) digit_color(FALSE, FALSE) else NULL
  up <- if (editable) '<span class="ar up">&#9650;</span>' else ""
  dn <- if (editable) '<span class="ar dn">&#9660;</span>' else ""
  sp <- if (editable) '<span class="arsp"></span>' else ""
  sep_slot <- function(txt, cls = "dch", col = sep_col)
    paste0('<span class="sep">', sp, '<b class="', cls, '"', sty_col(col), '>', txt, '</b>', sp, '</span>')
  dlt  <- formatC(place, format = "e", digits = 6)
  slot <- vapply(seq_len(n), function(p) {
    ch <- chars[p]
    if (isdig[p]) {
      col <- if (!is.null(digit_color)) digit_color(ch == "0", lead[p])
             else if (!is.null(pad) && lead[p]) ZERO else NULL   # leading zeros: same grey as the ▲/▼ arrows
      paste0('<span class="dig" data-delta="', dlt[p], '">', up, '<b class="dch"', sty_col(col), '>', ch, '</b>', dn, '</span>')
    } else sep_slot(if (ch == " ") "&nbsp;" else ch)
  }, "")
  # split positions into thousands-groups (a space starts a new group; the dot + decimals stay with
  # their group). Each group is one inline-flex unit; the sign stays with the leading group and the
  # suffix with the trailing one.
  gid    <- cumsum(chars == " ")
  groups <- unname(vapply(split(slot, gid), paste, "", collapse = ""))
  if (!is.null(sign_char))
    groups[1] <- paste0(sep_slot(sign_char, "dch sgn", sign_color), groups[1])
  if (nzchar(suffix))
    groups[length(groups)] <- paste0(groups[length(groups)], sep_slot(suffix, "dch suf"))
  out <- paste0('<span class="dgroup">', groups, '</span>', collapse = "")
  if (!is.null(ref_txt))                            # baseline: its own group
    out <- paste0(out, '<span class="dgroup">',
                  sep_slot(ref_txt, if (ref_ghost) "dch vref ghost" else "dch vref", NULL), '</span>')
  out
}

# a value cell on the grid (spans 2 half-columns); attrs = extra attribute text
vcell_html <- function(col, inner, cls = "vcell", attrs = "")
  paste0('<div class="', cls, '" style="grid-column:', col, '/span 2;"', attrs, '>', inner, '</div>')

# fixed (non-editable) value in a row of editable cells: same digit slots as gcell_edit, with an empty
# spacer where the ▲ arrows would be, so its number sits level with its editable neighbours'.
gcell_fixed <- function(col, v, to_disp, decimals, suffix, color, pad = NULL)
  HTML(vcell_html(col, paste0('<div class="vstack"><div class="arsp"></div>',
    '<div class="vmain digits"', sty_col(color), '>',
    build_slots(v, to_disp, decimals, suffix, pad = pad, editable = FALSE), '</div></div>')))

# editable value cell: per-digit spinner
# ref_txt (both cell builders): when given, the baseline value is shown in parentheses right after the
# value, level with its digits (see build_slots); the delta row gets an invisible copy so it stays aligned.
gcell_edit <- function(col, cur, ref, kind, vi, to_disp, decimals, suffix,
                       good = "more_good", sub = NULL, color, pad = NULL, top = NULL, ref_txt = NULL) {
  dv   <- cur - ref
  cpos <- if (good == "more_good") GOOD else BAD    # colour when delta positive
  cneg <- if (good == "more_good") BAD  else GOOD
  dshow <- formatC(abs(dv) * to_disp, format = "f", digits = decimals)   # decimals kept: a padded % cell can move by < 1
  disp_zero <- abs(dv) < 1e-12 || !grepl("[1-9]", dshow)   # exact, or rounds to 0 in display → grey
  dcol <- if (disp_zero) ZERO else if (dv > 0) cpos else cneg
  sgn  <- if (disp_zero) "±" else if (dv > 0) "+" else "−"
  digit_color <- function(is0, lead) if (lead) ZERO else dcol
  HTML(vcell_html(col, cls = "vcell xedit", attrs = paste0(' data-kind="', kind, '" data-i="', vi - 1, '"'), paste0(
    if (!is.null(top)) paste0('<div class="vtop">', top, '</div>'),
    '<div class="vstack">',
      '<div class="vmain digits"', sty_col(color), '>',
        build_slots(cur, to_disp, decimals, suffix, pad, TRUE, ref_txt = ref_txt), '</div>',
      '<div class="vdelta digits">',
        build_slots(dv, to_disp, decimals, suffix, pad, FALSE,   # no ▲/▼ on deltas — number-key entry only
                    digit_color = digit_color, sign_char = sgn, sign_color = dcol,
                    ref_txt = ref_txt, ref_ghost = TRUE), '</div>',
    '</div>',
    if (!is.null(sub)) paste0('<div class="vsub">', sub, '</div>'))))
}
# baseline text for the ref_txt slot: same display scale/decimals as the value it sits next to
f_ref <- function(v, to_disp, decimals, suffix)
  paste0("(", formatC(v * to_disp, format = "f", digits = decimals, big.mark = " "), suffix, ")")

# read-only value cell: value + a digit-aligned (non-editable) delta below it.
# The delta shows one grey zero under each value digit when there is no change,
# and the signed change (leading zeros greyed) when there is.
gcell_ro <- function(col, cur, ref, to_disp, decimals, suffix, kind = "more_good",
                     sub = NULL, color, delta = TRUE, ref_txt = NULL) {
  dv   <- cur - ref
  cpos <- if (kind == "more_good") GOOD else BAD
  cneg <- if (kind == "more_good") BAD  else GOOD
  vwhole <- formatC(abs(cur) * to_disp, format = "f", digits = decimals)
  pad    <- nchar(strsplit(vwhole, ".", fixed = TRUE)[[1]][1])       # value's integer-digit width
  dshow  <- formatC(abs(dv) * to_disp, format = "f", digits = decimals)
  disp_zero <- abs(dv) < 1e-9 || !grepl("[1-9]", dshow)             # exact, or rounds to 0 in display
  dcol <- if (disp_zero) ZERO else if (dv > 0) cpos else cneg
  sgn  <- if (disp_zero) "±" else if (dv > 0) "+" else "−"
  digit_color <- function(is0, lead) if (lead) ZERO else dcol
  HTML(vcell_html(col, paste0(
    '<div class="vstack">',
      # value carries an invisible sign slot when a delta follows, so value and delta are the same width
      '<div class="vmain digits"', sty_col(color), '>',
        build_slots(cur, to_disp, decimals, suffix, pad, FALSE,
                    sign_char = if (delta) "±" else NULL, sign_color = "transparent", ref_txt = ref_txt), '</div>',
      if (delta) paste0('<div class="vdelta digits">',
        build_slots(dv, to_disp, decimals, suffix, pad, FALSE,
                    digit_color = digit_color, sign_char = sgn, sign_color = dcol,
                    ref_txt = ref_txt, ref_ghost = TRUE), '</div>'),
    '</div>',
    if (!is.null(sub)) paste0('<div class="vsub">', sub, '</div>'))))
}

# big-operator (∏ / ∑) with limits stacked over/under the symbol
bigop <- function(sym, lo, up)
  sprintf('<span class="bigop"><span class="lim up">%s</span><span class="op">%s</span><span class="lim lo">%s</span></span>',
          up, sym, lo)

# colour a variable symbol (with its subscript) to match its metric's colour
mv <- function(txt, col) sprintf('<span style="color:%s;">%s</span>', col, txt)

# a full metric row (label in column 1, then its value cells).
# cls = "erow" tints the whole row (label included) to flag it as editable.
# row = the row id (data-row), matching its entry in the Variables dropdown.
# toggle = optional control under the formula (see rtoggle).
metric_grow <- function(label, color, cells, cls = NULL, row = NULL, formula = NULL, toggle = NULL)
  do.call(tags$div, c(list(class = trimws(paste("grow", cls %||% "")), `data-row` = row,
    tags$div(class = "rlabel", style = paste0("color:", color, ";"),
      div(class = "rlblwrap",
        span(class = "rlbl", HTML(label)),
        if (!is.null(formula)) div(class = "rformula", HTML(formula)),
        toggle))),
    cells))

# Per-row "switch to the complement" button. Its text names the view a click switches TO, so it flips
# with the mode (label_off while the default view shows, label_on while the complement shows). The
# panel re-renders on every edit, so this is a plain button (not a Shiny input binding); JS flips
# input$<id> (see the .rtog handler).
rtoggle <- function(id, on, label_off, label_on)
  tags$button(type = "button", class = if (on) "rtog on" else "rtog",
    `data-input` = id, `data-on` = if (on) "1" else "0",
    HTML(paste0("&#8644;&nbsp;", if (on) label_on else label_off)))

# ── UI ──────────────────────────────────────────────────────────────────────
css <- r"(
body{background:#0b3552;color:#e8eef4;font-family:Arial,Helvetica,sans-serif;margin:0;}
.wrap{max-width:1240px;margin:0 auto;padding:18px 11px 70px;}
h1{font-size:21px;font-weight:700;margin:0 0 4px;}
.lead{font-size:13px;color:#b7c6d3;margin:0 0 14px;line-height:1.5;}
.controls{display:flex;gap:10px;align-items:center;margin:12px 0 20px;flex-wrap:wrap;}
.controls:has(+ .controls){margin-bottom:6px;}          /* two stacked control rows sit close together */
.controls + .controls{margin-top:0;}
.controls .btn{background:#12405f;color:#e8eef4;border:1px solid #2a6a8f;border-radius:6px;
  padding:6px 13px;font-size:13px;cursor:pointer;}
.controls .btn:hover{background:#175981;}
.controls .btn:disabled{opacity:0.35;cursor:default;background:#12405f;box-shadow:none;}
.controls .btn.active{background:#1f77a8;border-color:#8fd6f2;color:#fff;box-shadow:0 0 0 1px #8fd6f2 inset;}
.khint{font-size:12.5px;color:#9fb0bf;}
.lead2{margin:-8px 0 4px;line-height:1.7;}   /* editing hint, directly under the lead paragraph */
.kbd{display:inline-block;background:#0a2438;border:1px solid #2a6a8f;border-radius:4px;
  padding:0 7px;font-size:12px;margin:0 1px;line-height:18px;}
.grid-wrap{margin-top:4px;position:relative;}
/* each value is a box sized to its content; "Show arrows" adds an SVG overlay (drawArrows) linking
   each box to the values it is computed from */
.grid-wrap .vcell{justify-self:center;margin:7px 3px;padding:5px calc(4px + .5ch);   /* sides: + half a digit each */
  border:1px solid rgba(143,214,242,.132);border-radius:6px;background:rgba(6,30,48,.21);}   /* 40% more transparent than .22 / .35 */
/* captions under the number (vsub) wrap to the number's width instead of widening the box: inline-size containment
   makes them contribute no width of their own, and stretch fills whatever the number sets */
.grid-wrap .vcell > .vsub{contain:inline-size;align-self:stretch;}
.grid-wrap .xedit:hover{background:rgba(255,255,255,.07);}
.grid-wrap .xedit.sel{background:rgba(255,210,127,.08);}
svg.arrows{position:absolute;left:0;top:0;pointer-events:none;overflow:visible;z-index:2;}
svg.arrows path.e{fill:none;stroke:#7fb0cc;stroke-width:1.3;opacity:.75;}
svg.arrows marker path{fill:#7fb0cc;opacity:.9;}
/* two half-columns per checkpoint (values sit on the years, rates in the gaps between them);
   --ncols is set on .grid-wrap per render from the number of checkpoints */
/* Cap the column pitch so a short horizon doesn't spread three checkpoints across the whole width.
   Every cell is two half-columns wide (the rate cells straddle the year columns), so one cap governs
   both the gaps and the value cells. The cap is set per render (--halfcol on .grid-wrap) and grows
   with the horizon, because the numbers do: each 10x of horizon adds a digit and a separator to the
   cumulative totals. 1.05 x (46 + 7n) px per half-column (n = checkpoints) is sized to the widest bold NUMBER
   at each horizon (a cap for air only: the floor that keeps boxes whole is --halfmin, below); the italic
   captions beneath are allowed to wrap onto a second line. Spare width sits after the table. */
/* Columns never squeeze a box: boxes keep their content on one line (baseline included), fitColumns()
   measures the widest box after each render and sets --halfmin, and every row's grid uses it as its
   half-column minimum, so rows stay aligned. When the minimums don't fit, .grid-scroll scrolls sideways
   (only the table; the page never does) and the row labels stay pinned on the left. */
.grid-wrap{--halfcol:74px;--labw:258px;}
@media (max-width:760px){ .grid-wrap{--labw:180px;} }   /* small screens: narrower labels, more room for values */
.grid-scroll{overflow-x:auto;overflow-y:hidden;}
.grow{display:grid;grid-template-columns:var(--labw) repeat(var(--ncols,8),minmax(var(--halfmin,0px),1fr));align-items:start;
  max-width:calc(var(--labw) + var(--ncols,8) * max(var(--halfcol), var(--halfmin,0px)));
  min-width:calc(var(--labw) + var(--ncols,8) * var(--halfmin,0px));
  border-bottom:1px solid rgba(255,255,255,.05);}
/* horizon slider: compact, dark-themed, four stops (100 / 1 000 / 10 000 / 100 000 years) */
.scalectl{display:flex;align-items:center;gap:10px;}
.scalectl .form-group{margin:0;}
.scalectl .irs{font-family:inherit;}
.scalectl .irs--shiny .irs-line{background:#12405f;border-color:#2a6a8f;}
.scalectl .irs--shiny .irs-bar{background:#1f77a8;border-color:#1f77a8;}
.scalectl .irs--shiny .irs-handle{background:#8fd6f2;border-color:#8fd6f2;}
.scalectl .irs--shiny .irs-single{background:#1f77a8;color:#fff;font-size:11px;}
.scalectl .irs--shiny .irs-min,.scalectl .irs--shiny .irs-max{color:#9fb0bf;background:transparent;font-size:10px;}
.grow.ghead{border-bottom:1px solid rgba(255,255,255,.18);}
.grow.erow{background:rgba(255,255,255,.05);border-radius:7px;margin:3px 0;}
.rlabel{grid-column:1;grid-row:1;display:flex;align-items:flex-start;gap:5px;font-size:12.5px;line-height:1.35;font-weight:600;
  align-self:stretch;padding:9px 8px 9px 0;border-right:1px solid rgba(255,255,255,.10);}
/* pinned while the table scrolls sideways: opaque (page colour, or the tinted-row colour) so values and
   arrows pass underneath */
.rlabel{position:sticky;left:0;z-index:3;background:#0b3552;}
.grow.erow .rlabel{background:#173f5b;border-radius:7px 0 0 7px;}
.rlblwrap{flex:1;min-width:0;text-align:right;}
.rformula{font-size:14.4px;color:#cbb68e;margin-top:4px;padding-left:0;font-weight:400;
  line-height:1.6;font-family:Cambria,Georgia,"Times New Roman",serif;font-style:italic;}
.rtog{display:inline-block;margin:6px 0 0;padding:3px 9px;font:inherit;font-size:11.5px;font-weight:400;
  color:#cdd9e3;background:#12405f;border:1px solid #2a6a8f;border-radius:5px;cursor:pointer;user-select:none;}
.rtog:hover{background:#175981;color:#fff;}
.rtog.on{border-color:#8fd6f2;}
body.nofx .rformula{display:none;}          /* "Show formulas" unticked */
.rformula sub{font-size:10.2px;font-style:normal;}
.rformula sup{font-size:10.2px;font-style:normal;}
.rformula .bigop{display:inline-flex;flex-direction:column;align-items:center;
  vertical-align:middle;font-style:normal;line-height:1;margin:0 3px;}
.rformula .bigop .op{font-size:24px;line-height:1;}
.rformula .bigop .lim{font-size:9.6px;line-height:1;font-style:normal;white-space:nowrap;}
.rformula .bigop .lim.up{margin-bottom:1px;}
.rformula .bigop .lim.lo{margin-top:3px;}
.ycol{text-align:center;padding:2px 6px 7px;justify-self:center;}
.ycol .yr{font-size:21px;font-weight:700;}
.ivlcell{text-align:center;font-size:11px;color:#9fb0bf;padding:3px 4px;align-self:center;justify-self:center;
  white-space:nowrap;}
/* on the annual-probability row: period labels on a first grid line, the boxes on a second, the row label spanning both */
.grow.rrow .ivlcell{grid-row:1;}
.grow.rrow .vcell{grid-row:2;}
.grow.rrow .rlabel{grid-row:1 / span 2;}
.ivlrng{font-size:11px;font-style:italic;color:#8496a4;margin-top:3px;white-space:nowrap;}
.vcell{grid-row:1;padding:8px 7px;display:flex;flex-direction:column;align-items:center;text-align:center;min-width:0;}
.vstack{display:inline-flex;flex-direction:column;align-items:stretch;max-width:100%;}
.vmain{font-size:13px;line-height:1.2;font-weight:700;word-break:break-word;font-variant-numeric:tabular-nums;text-align:right;}
.vmain .per{font-size:10px;color:#9fb0bf;font-weight:400;}
.vdelta{font-size:13px;margin-top:1px;font-weight:600;font-variant-numeric:tabular-nums;text-align:right;line-height:1.15;}
.vdelta .d0{color:#8a97a3;}
.vsub{font-size:11px;color:#8496a4;margin-top:3px;font-style:italic;line-height:1.35;}
/* baseline value ("Show baseline values"): same colour as the value (inherited), regular weight, a little
   smaller; .ghost is the delta row's invisible copy that keeps its digits under the value's */
.dch.vref{font-weight:400;font-size:12px;padding-left:4px;white-space:nowrap;}
.ratio.vref{font-weight:400;font-size:12px;}
.dch.vref.ghost{visibility:hidden;}
.controls .checkbox{margin:0;} .controls .checkbox label{color:#cdd9e3;font-size:13px;font-weight:400;cursor:pointer;}
.controls .checkbox input{margin-right:5px;}
.vgrowth{grid-row:1;align-self:start;justify-self:center;margin-top:22px;text-align:center;font-size:12px;color:#8496a4;
  font-style:italic;white-space:nowrap;padding:0 3px;pointer-events:none;}

.ratio{color:#e8836f;font-size:13px;font-weight:600;font-style:normal;}
.rng{font-size:12px;font-style:italic;color:#8496a4;}
.vtop{line-height:1.1;margin-bottom:1px;white-space:nowrap;}
.ital{font-style:italic;font-weight:400;opacity:.85;}
.digits{display:flex;justify-content:flex-end;align-items:flex-end;flex-wrap:nowrap;}   /* one line: the table scrolls instead */
.dgroup{display:inline-flex;align-items:flex-end;}   /* a thousands-group: never breaks internally */
.dig,.sep{display:flex;flex-direction:column;align-items:center;line-height:1;}
.dig{width:1ch;}                 /* digit width fixed; larger arrows overflow it */
.dch{font-size:13px;font-weight:700;font-variant-numeric:tabular-nums;padding:0;}
.dch.suf{color:#9fb0bf;}
.dig.lead .dch{color:#8a97a3;}
.ar{cursor:pointer;font-size:11px;color:#8a97a3;user-select:none;height:13px;line-height:13px;padding:0;}
.ar:hover{color:#ffd27f;}
.arsp{height:13px;}
.dig.digsel .dch{color:#ffd27f !important;}
.dig.digsel .ar{color:#e8b96a;}
.xedit{cursor:pointer;border-radius:6px;}
.xedit:hover{background:rgba(255,255,255,.045);}
.xedit.sel{outline:2px solid rgba(255,210,127,.55);outline-offset:-2px;background:rgba(255,210,127,.06);}
/* "Variables" dropdown: a <details> button that opens a checkbox list of the metric rows */
.vdrop{position:relative;}
.vdrop > summary{list-style:none;display:inline-block;user-select:none;}
.vdrop > summary::-webkit-details-marker{display:none;}
.vdrop[open] > summary{background:#1f77a8;border-color:#8fd6f2;color:#fff;}
.vdrop .vcount{color:#9fb0bf;font-size:12px;}
.vdrop[open] .vcount{color:#dff3fb;}
.vmenu{position:absolute;z-index:20;top:calc(100% + 4px);left:0;min-width:250px;background:#0a2438;
  border:1px solid #2a6a8f;border-radius:6px;padding:8px 12px 4px;box-shadow:0 6px 18px rgba(0,0,0,.45);}
.vmenu .form-group{margin:0;}
.vmenu .checkbox{margin:0 0 5px;}
.vmenu .checkbox label{color:#cdd9e3;font-size:13px;font-weight:400;cursor:pointer;white-space:nowrap;}
.vmenu .checkbox input{margin-right:6px;}
.vall{font-size:12px;color:#5f7484;margin-bottom:6px;padding-bottom:5px;border-bottom:1px solid rgba(255,255,255,.08);}
.vall a{color:#8fd6f2;text-decoration:none;}
.vall a:hover{color:#ffd27f;}
.novars{font-size:13px;color:#9fb0bf;font-style:italic;padding:14px 0;}
.summary{margin:18px 0 0;font-size:13px;color:#cdd9e3;line-height:1.55;
  border-top:1px solid rgba(255,255,255,.10);padding-top:12px;}
.summary b{color:#ffd27f;}
.foot{margin-top:22px;font-size:11.5px;color:#7d8fa0;line-height:1.5;}
)"

js <- r"(
// Initial arrays match the server's default horizon (1 000 years = 4 checkpoints, 3 periods).
// A horizon change arrives as a 'setScale' message that replaces all three.
var rates = [0.0005, 0.0005, 0.0001];
var pops  = [8.3e9, 8.3e9, 8.3e9, 8.3e9];
var LEN   = [10, 90, 900];                       // interval lengths (years)
var baseRates = rates.slice(), basePops = pops.slice();   // comparison baseline (from server)
var sel   = {kind:'rate', i:0, place:0.00001, delta:false};  // first period, second-to-last decimal (0.001%); value/delta + which digit

// ── undo / redo history: client-side snapshots of {rates, pops} ──
var histStack = [{rates: rates.slice(), pops: pops.slice()}];
var histIdx = 0, histSuppress = false;
function histEq(a,b){ return a.rates.every(function(v,i){return v===b.rates[i];}) && a.pops.every(function(v,i){return v===b.pops[i];}); }
function updateHistBtns(){
  var u = document.getElementById('undoBtn'), r = document.getElementById('redoBtn');
  if(u) u.disabled = (histIdx <= 0);
  if(r) r.disabled = (histIdx >= histStack.length - 1);
}
function recordHistory(){
  if(histSuppress) return;
  var cur = {rates: rates.slice(), pops: pops.slice()};
  if(histEq(cur, histStack[histIdx])) return;        // no actual change
  histStack = histStack.slice(0, histIdx + 1);         // drop any redo tail
  histStack.push(cur); histIdx = histStack.length - 1;
  updateHistBtns();
}
function restoreHist(){
  var s = histStack[histIdx];
  histSuppress = true; rates = s.rates.slice(); pops = s.pops.slice(); pushState(); histSuppress = false;
  applyHighlight(); updateHistBtns();
}
function undo(){ if(histIdx > 0){ histIdx--; restoreHist(); } }
function redo(){ if(histIdx < histStack.length - 1){ histIdx++; restoreHist(); } }

// Sending edits to R. A re-render takes a noticeable moment (much longer under shinylive/webR), so while
// R is still busy with one edit, further edits only update the local state; the latest state is sent
// once R reports idle. A single edit is sent at once; a held key or wheel spin costs one extra render,
// not one per step. The fallback timer un-sticks the gate if an idle event never arrives.
var serverBusy = false, statePending = false, busyTimer = null;
function sendState(){
  if(!window.Shiny) return;
  Shiny.setInputValue('state', {rates:rates, pops:pops}, {priority:'event'});
  serverBusy = true;
  clearTimeout(busyTimer); busyTimer = setTimeout(function(){ serverBusy = false; flushState(); }, 5000);
}
function flushState(){ if(statePending && !serverBusy){ statePending = false; sendState(); } }
$(document).on('shiny:idle', function(){ serverBusy = false; clearTimeout(busyTimer); flushState(); });
function pushState(){ if(serverBusy) statePending = true; else sendState(); recordHistory(); }
// Complementary display kinds: the "show annual survival" / "show extinction probability" checkboxes
// re-render rows 1-2 with data-kind rate_c / surv_c, whose value is 1 - the underlying one. Editing them
// writes back through the same rate / survival machinery. A selection survives the toggle by following
// its cell to the other kind name.
var ALT_KIND = { rate:'rate_c', rate_c:'rate', surv:'surv_c', surv_c:'surv' };
function cellOf(s){
  var c = document.querySelector('.xedit[data-kind="'+s.kind+'"][data-i="'+s.i+'"]');
  if(!c && ALT_KIND[s.kind]){ c = document.querySelector('.xedit[data-kind="'+ALT_KIND[s.kind]+'"][data-i="'+s.i+'"]'); if(c) s.kind = ALT_KIND[s.kind]; }
  return c;
}
function digsOf(c){ if(!c) return []; return Array.prototype.slice.call(c.querySelectorAll((sel.delta ? '.vdelta' : '.vmain') + ' .dig')); }
function allCells(){ return Array.prototype.slice.call(document.querySelectorAll('.xedit')); }
function digDelta(el){ return parseFloat(el.getAttribute('data-delta')); }
function nearestDig(digs, place){                // match a digit by its place value
  var best = 0, bd = Infinity;
  for(var k = 0; k < digs.length; k++){
    var d = Math.abs(Math.log(digDelta(digs[k])) - Math.log(place));
    if(d < bd){ bd = d; best = k; }
  }
  return best;
}
function applyHighlight(){
  allCells().forEach(function(c){
    var on = c.getAttribute('data-kind') === sel.kind && (+c.getAttribute('data-i')) === sel.i;
    c.classList.toggle('sel', on);
    c.querySelectorAll('.dig').forEach(function(d){ d.classList.remove('digsel'); });
    if(on){
      var digs = digsOf(c);
      if(digs.length){
        var idx = nearestDig(digs, sel.place);
        digs[idx].classList.add('digsel');
        sel.place = digDelta(digs[idx]);         // snap to an existing digit
      }
    }
  });
}
function survOf(arr, i){                          // cumulative survival through interval i
  var p = 1; for(var k = 0; k <= i; k++) p *= Math.pow(1 - arr[k], LEN[k]); return p;
}
// set survival at checkpoint i to target T by solving the interval-i extinction
// rate, backtracing into earlier intervals when T exceeds the prior survival.
function setSurv(i, T){
  T = Math.min(1, Math.max(0, T));               // 100% ceiling, 0 floor
  for(var m = i; m >= 0; m--){
    var Pprev = 1; for(var k = 0; k < m; k++) Pprev *= Math.pow(1 - rates[k], LEN[k]);
    var needed = T / Pprev;                       // desired (1-r_m)^len_m
    if(needed <= 1){ rates[m] = 1 - Math.pow(needed, 1 / LEN[m]); break; }
    rates[m] = 0;                                 // maxed this interval, keep backtracing
  }
  pushState();
}
var MINPLACE = { rate:1e-6, surv:1e-6, pop:1e6, rate_c:1e-6, surv_c:1e-6 };  // smallest editable digit per kind
function valOf(kind, i, rArr, pArr){
  if(kind === 'surv')   return survOf(rArr, i);
  if(kind === 'surv_c') return 1 - survOf(rArr, i);      // cumulative extinction probability
  if(kind === 'rate_c') return 1 - rArr[i];              // annual survival probability
  return (kind === 'rate' ? rArr : pArr)[i];
}
function curVal(){  return valOf(sel.kind, sel.i, rates, pops); }
function baseVal(){ return valOf(sel.kind, sel.i, baseRates, basePops); }
function setValAbs(v){                            // write the actual value (backtrace for surv)
  if(sel.kind === 'surv'){ setSurv(sel.i, v); }
  else if(sel.kind === 'surv_c'){ setSurv(sel.i, 1 - v); }
  else if(sel.kind === 'rate_c'){ rates[sel.i] = Math.min(1, Math.max(0, +(1 - v).toFixed(12))); pushState(); }
  else { var arr = (sel.kind === 'rate') ? rates : pops; arr[sel.i] = Math.max(0, +v.toFixed(12)); pushState(); }
}
// when sel.delta, we edit the change (value − baseline); otherwise the value itself
function editVal(){ return sel.delta ? (curVal() - baseVal()) : curVal(); }
function applyEdit(nv){ setValAbs(sel.delta ? baseVal() + nv : nv); }
function change(dir){                             // ▼ always lowers, ▲ always raises (monotonic)
  applyEdit(editVal() + dir * sel.place);         // crosses zero once, never flip-flops
}
// type a digit into the selected place (selection stays put)
function typeDigit(d){
  var place = sel.place, v = editVal(), av = Math.abs(v), sign = v < 0 ? -1 : 1;
  var cd = Math.floor(av / place + 1e-6) % 10;      // current digit at this place
  applyEdit(sign * (av + (d - cd) * place));
  moveDigit(1, false);                              // advance like a text field; stops at the last digit
}
// Move the selection one digit left (-1) / right (+1). cross = may step into the neighbouring cell
// (arrow keys do; typing does not, so a number entered in the last place just stays there).
function moveDigit(dir, cross){
  var cells = allCells(), cell = cellOf(sel); if(!cell) return;
  var digs = digsOf(cell), idx = nearestDig(digs, sel.place), ci = cells.indexOf(cell);
  if(dir > 0){
    if(idx < digs.length - 1) sel.place = digDelta(digs[idx + 1]);
    else if(cross && ci < cells.length - 1){ var nc = cells[ci + 1], nd = digsOf(nc);
      sel = {kind:nc.getAttribute('data-kind'), i:+nc.getAttribute('data-i'), place:digDelta(nd[0]), delta:sel.delta}; }
  } else {
    if(idx > 0) sel.place = digDelta(digs[idx - 1]);
    else if(cross && ci > 0){ var pc = cells[ci - 1], pd = digsOf(pc);
      sel = {kind:pc.getAttribute('data-kind'), i:+pc.getAttribute('data-i'), place:digDelta(pd[pd.length - 1]), delta:sel.delta}; }
  }
  applyHighlight();
}
document.addEventListener('keydown', function(e){
  var k = e.key;
  if(e.target.closest && e.target.closest('input, textarea, select, .vdrop')) return;  // keys aimed at a control (e.g. the Variables menu)
  if(/^[0-9]$|^Arrow/.test(k) && !cellOf(sel)){      // selected cell's row is hidden: jump to the first visible one, don't edit blind
    var c0 = allCells()[0]; if(!c0) return;
    e.preventDefault();
    var d0 = digsOf(c0); sel = {kind:c0.getAttribute('data-kind'), i:+c0.getAttribute('data-i'), place:d0.length ? digDelta(d0[0]) : sel.place, delta:false};
    applyHighlight(); return;
  }
  if(/^[0-9]$/.test(k)){ e.preventDefault(); typeDigit(+k); return; }
  if(['ArrowUp','ArrowDown','ArrowLeft','ArrowRight'].indexOf(k) < 0) return;
  e.preventDefault();
  if(k === 'ArrowUp')   { if(!sel.delta) change(1);  return; }   // ▲/▼ change values only, not deltas
  if(k === 'ArrowDown') { if(!sel.delta) change(-1); return; }
  moveDigit(k === 'ArrowRight' ? 1 : -1, true);   // arrows may walk on into the next/previous cell
});
document.addEventListener('click', function(e){
  var cell = e.target.closest('.xedit'); if(!cell) return;
  var dig  = e.target.closest('.dig');
  if(dig){
    sel = {kind:cell.getAttribute('data-kind'), i:+cell.getAttribute('data-i'),
           place:digDelta(dig), delta: !!dig.closest('.vdelta')};
    var up = e.target.closest('.ar.up'), dn = e.target.closest('.ar.dn');
    if(up || dn){ change(up ? 1 : -1); return; }
    applyHighlight();
  } else {
    sel.kind = cell.getAttribute('data-kind'); sel.i = +cell.getAttribute('data-i'); sel.delta = false;
    applyHighlight();
  }
});
// mouse wheel over a digit nudges that digit (up = raise, down = lower).
// Accumulate delta and step per threshold so a touchpad gesture (many events +
// inertia) doesn't run the digit away — ~one step per notch / firm swipe.
var wheelAcc = 0;
document.addEventListener('wheel', function(e){
  var dig = e.target.closest('.dig'); if(!dig){ wheelAcc = 0; return; }
  var cell = dig.closest('.xedit'); if(!cell){ wheelAcc = 0; return; }
  if(dig.closest('.vdelta')){ wheelAcc = 0; return; }   // deltas: no wheel change (number keys only)
  e.preventDefault();
  var d = e.deltaY;
  if(e.deltaMode === 1) d *= 33; else if(e.deltaMode === 2) d *= 400;   // lines/pages → px
  if(wheelAcc !== 0 && (d < 0) !== (wheelAcc < 0)) wheelAcc = 0;         // direction flip → reset
  wheelAcc += d;
  var STEP = 90;
  if(Math.abs(wheelAcc) < STEP) return;
  sel = {kind:cell.getAttribute('data-kind'), i:+cell.getAttribute('data-i'),
         place:digDelta(dig), delta: !!dig.closest('.vdelta')};
  var n = 0;
  while(wheelAcc <= -STEP && n < 8){ wheelAcc += STEP; change(-1); n++; }
  while(wheelAcc >=  STEP && n < 8){ wheelAcc -= STEP; change(1);  n++; }
  if(n >= 8) wheelAcc = 0;   // guard against a pathological single event
}, {passive:false});
$(document).on('shiny:value', function(ev){ if(ev.name === 'panel') setTimeout(function(){ applyHighlight(); watchArrows(); }, 0); });

// ── dependency arrows: SVG overlay on .grid-wrap, drawn from its data-edges ("from,to,type;...") ──
// type h: across a row (previous value → this one, at the value's line); v: from the row directly above;
// d: skips visible rows, so it loops round the boxes' left side.
// A value with an h arrow is its previous value *acted on* by the rows above (cs_2036 = cs_2026 · (1 − ae)^10),
// so its inputs from above don't enter the box: they join the h arrow with an arrowhead (see step 2).
var SVGNS = 'http://www.w3.org/2000/svg';
function nodeBox(gw, base, id){
  var el = gw.querySelector('[data-node="' + id + '"]'); if(!el) return null;
  var r = el.getBoundingClientRect(), vm = el.querySelector('.vmain'), m = vm ? vm.getBoundingClientRect() : r;
  return {l:r.left - base.left, r:r.right - base.left, t:r.top - base.top, b:r.bottom - base.top,
          cx:(r.left + r.right) / 2 - base.left, my:(m.top + m.bottom) / 2 - base.top};
}
function drawArrows(){
  var gw = document.querySelector('.grid-wrap'); if(!gw) return;
  var old = gw.querySelector('svg.arrows'); if(old) old.remove();
  if(document.body.classList.contains('noarrows')) return;
  var spec = gw.getAttribute('data-edges'); if(!spec) return;
  var base = gw.getBoundingClientRect();
  var svg = document.createElementNS(SVGNS, 'svg');
  svg.setAttribute('class', 'arrows');
  svg.setAttribute('width', gw.scrollWidth); svg.setAttribute('height', gw.scrollHeight);
  svg.innerHTML = '<defs><marker id="ah" viewBox="0 0 8 8" refX="7.5" refY="4" markerUnits="userSpaceOnUse" ' +
                  'markerWidth="7" markerHeight="7" orient="auto"><path d="M0,0 L8,4 L0,8 z"/></marker></defs>';
  gw.appendChild(svg);                                            // attached first: paths get sampled while drawing
  function add(d, head){
    var p = document.createElementNS(SVGNS, 'path');
    p.setAttribute('class', 'e'); p.setAttribute('d', d); if(head) p.setAttribute('marker-end', 'url(#ah)');
    svg.appendChild(p); return p;
  }
  var edges = spec.split(';').map(function(e){
    var p = e.split(','); return {from:p[0], to:p[1], type:p[2], S:nodeBox(gw, base, p[0]), T:nodeBox(gw, base, p[1])};
  }).filter(function(e){ return e.S && e.T; });
  // 1 · across arrows; remember each one's segment so arrows from above can join it
  var hseg = {};
  edges.forEach(function(e){
    if(e.type !== 'h') return;
    var x0 = e.S.r + 1, x1 = e.T.l - 1;
    if(x1 - x0 < 6) return;                                       // boxes touching: no room for an arrow
    add('M' + x0 + ',' + e.S.my + ' L' + x1 + ',' + e.T.my, true);
    hseg[e.to] = {x0:x0, y0:e.S.my, x1:x1, y1:e.T.my};
  });
  // 2 · arrows from above. A box directly above feeds from its bottom-left corner (its output side); a
  // box sitting over the gap between years (the ae row) drops straight down from its centre.
  //   target has an across arrow  → every input joins that arrow just before the target
  //   otherwise                   → one input is the "trunk": it swings out left of the boxes, runs down,
  //                                 and enters the target's left edge; any other inputs join the trunk
  //                                 (E_t = P_t · cs_t: P's arrow meets the cs arrow on its way into E)
  var byT = {};
  edges.forEach(function(e){ if(e.type !== 'h') (byT[e.to] = byT[e.to] || []).push(e); });
  Object.keys(byT).forEach(function(to){
    var list = byT[to], T = list[0].T, h = hseg[to];
    if(h && h.x1 - h.x0 >= 22){
      list.forEach(function(e){
        var S = e.S, sy = S.b + 1, overGap = S.cx <= h.x1 - 12;
        if(e.type === 'd'){                                         // skips rows: go round their left side, then
          var xo = Math.min(S.l, T.l) - 16, xd = S.l + 5;           // drop onto the across arrow before the box
          if(xo >= h.x0 + 6){
            var jyd = h.y0 + (h.y1 - h.y0) * (xo - h.x0) / (h.x1 - h.x0);
            if((jyd - 1) - (sy + 12) >= 4){
              add('M' + xd + ',' + sy + ' Q' + xo + ',' + sy + ' ' + xo + ',' + (sy + 12) + ' L' + xo + ',' + (jyd - 1), true);
              return;
            }
          }
        }
        var sx = overGap ? S.cx : S.l + 5;
        var jx = overGap ? Math.max(S.cx, h.x0 + 8) : Math.max(h.x1 - 12, h.x0 + 8);
        var jy = h.y0 + (h.y1 - h.y0) * (jx - h.x0) / (h.x1 - h.x0);
        var cy = (sy + Math.min(T.t, jy)) / 2;
        add('M' + sx + ',' + sy + ' C' + sx + ',' + cy + ' ' + jx + ',' + cy + ' ' + jx + ',' + (jy - 1), true);
      });
      return;
    }
    // the trunk is the input from directly above (E_t: P_t is primary); inputs that skip rows join it
    var trunk = list.filter(function(e){ return e.type === 'v'; })[0] || list[0], S = trunk.S;
    var xo = Math.min.apply(null, [S.l, T.l].concat(list.map(function(e){ return e.S.l; }))) - 12;
    var sx = S.l + 5, sy = S.b + 1, tx = T.l - 1, ty = T.my;
    var straight = (ty - 10) - (sy + 12) >= 8;                      // room for a straight vertical run?
    var tp = add(straight
      ? 'M' + sx + ',' + sy + ' Q' + xo + ',' + sy + ' ' + xo + ',' + (sy + 12) + ' L' + xo + ',' + (ty - 10) +
        ' Q' + xo + ',' + ty + ' ' + (xo + 10) + ',' + ty + ' L' + tx + ',' + ty
      : 'M' + sx + ',' + sy + ' C' + xo + ',' + (sy + 16) + ' ' + xo + ',' + ty + ' ' + tx + ',' + ty, true);
    // Where the trunk turns down: its leftmost point (topmost, if it runs straight for a while). Joins land
    // exactly there, whichever shape the trunk took. The path is in the SVG, so it can be sampled.
    var px = Infinity, py = sy, L = tp.getTotalLength ? tp.getTotalLength() : 0;
    for(var k = 0; k <= 60; k++){
      var pt = tp.getPointAtLength(L * k / 60);
      if(pt.x < px - 0.5){ px = pt.x; py = pt.y; }
    }
    if(!isFinite(px)){ px = xo; py = sy + 12; }
    var y1 = straight ? ty - 10 : py;                               // bottom of the straight run (if any)
    list.forEach(function(e){
      if(e === trunk) return;
      var jsx = e.S.l + 5, jsy = e.S.b + 1;
      if(e.type === 'd'){
        // from further up: swing out to the trunk's line and run straight down it, landing where it turns down
        add((py - 1) - (jsy + 12) >= 4
          ? 'M' + jsx + ',' + jsy + ' Q' + px + ',' + jsy + ' ' + px + ',' + (jsy + 12) + ' L' + px + ',' + (py - 1)
          : 'M' + jsx + ',' + jsy + ' C' + px + ',' + jsy + ' ' + px + ',' + (py - 10) + ' ' + px + ',' + (py - 1), true);
        return;
      }
      var jy = y1 - py >= 8 ? Math.min(Math.max(jsy + 14, py + 4), y1 - 4) : py;   // on the trunk's straight run
      add(jsx - 10 > px ? 'M' + jsx + ',' + jsy + ' Q' + jsx + ',' + jy + ' ' + (jsx - 10) + ',' + jy + ' L' + (px + 1) + ',' + jy
                        : 'M' + jsx + ',' + jsy + ' L' + (px + 1) + ',' + jy, true);
    });
  });
}
// Column floor: every value box, year, period label and growth note sits centred at its natural
// (one-line) width, so the widest one is the smallest pitch at which nothing overlaps. Each cell spans
// two half-columns, so half of that (plus a little air) becomes --halfmin for every row's grid. Box
// widths don't depend on the column width, so this can't feed back on itself; it runs once per render.
function fitColumns(){
  var gw = document.querySelector('.grid-wrap'); if(!gw) return;
  var w = 0;
  gw.querySelectorAll('.vcell, .ycol, .ivlcell, .vgrowth').forEach(function(el){
    var cs = getComputedStyle(el);
    var tot = el.getBoundingClientRect().width + parseFloat(cs.marginLeft) + parseFloat(cs.marginRight);
    if(tot > w) w = tot;
  });
  gw.style.setProperty('--halfmin', Math.ceil(w / 2 + 3) + 'px');
}
// Redraw whenever the table's box changes size (window resize, wrapping, formulas toggled, fonts
// loading). The panel's re-render replaces .grid-wrap, so re-attach the observer each time.
var arrowRO = window.ResizeObserver ? new ResizeObserver(function(){ requestAnimationFrame(drawArrows); }) : null;
function watchArrows(){
  var gw = document.querySelector('.grid-wrap');
  if(arrowRO){ arrowRO.disconnect(); if(gw) arrowRO.observe(gw); }
  fitColumns();
  drawArrows();
}
window.addEventListener('resize', function(){ requestAnimationFrame(drawArrows); });
$(document).on('change', '#showarrows', function(){ document.body.classList.toggle('noarrows', !this.checked); drawArrows(); });
$(document).on('shiny:connected', function(){ pushState(); setTimeout(function(){ applyHighlight(); updateHistBtns(); }, 60); });
if(document.fonts && document.fonts.ready) document.fonts.ready.then(function(){ fitColumns(); drawArrows(); });
document.addEventListener('click', function(e){
  if(e.target.closest('#undoBtn')){ undo(); }
  else if(e.target.closest('#redoBtn')){ redo(); }
});
document.addEventListener('keydown', function(e){
  var z = (e.key === 'z' || e.key === 'Z'), y = (e.key === 'y' || e.key === 'Y');
  if((e.ctrlKey || e.metaKey) && z && !e.shiftKey){ e.preventDefault(); undo(); }
  else if((e.ctrlKey || e.metaKey) && (y || (z && e.shiftKey))){ e.preventDefault(); redo(); }
});
if(window.Shiny){
  Shiny.addCustomMessageHandler('setState', function(v){
    rates = v.rates.slice(); pops = v.pops.slice(); pushState(); applyHighlight();
  });
  Shiny.addCustomMessageHandler('baseline', function(v){
    baseRates = v.rates.slice(); basePops = v.pops.slice();
  });
  // Horizon changed: the server sends the resized arrays (edits to shared periods preserved).
  // The undo history restarts — its snapshots have the old length and can't be restored into
  // the new structure — and the selection is clamped so it can't point past the last column.
  Shiny.addCustomMessageHandler('setScale', function(v){
    LEN = v.len.slice(); rates = v.rates.slice(); pops = v.pops.slice();
    baseRates = rates.slice(); basePops = pops.slice();
    histStack = [{rates: rates.slice(), pops: pops.slice()}]; histIdx = 0;
    var last = (sel.kind === 'pop' ? pops.length : rates.length) - 1;   // rate/rate_c/surv/surv_c all index periods
    if(sel.i > last) sel.i = last;
    pushState(); applyHighlight(); updateHistBtns();
  });
}
// "Show formulas": a body class, so it survives every panel re-render with no server round trip
$(document).on('change', '#showfx', function(){ document.body.classList.toggle('nofx', !this.checked); });
// Per-row complement toggles (rendered inside the panel): forward to input$rate_comp / input$surv_comp
$(document).on('click', '.rtog', function(){
  if(window.Shiny) Shiny.setInputValue(this.getAttribute('data-input'), this.getAttribute('data-on') !== '1');
});
// Variables dropdown: live "k / n" count on the button, All / None shortcuts, close on an outside click.
function varBoxes(){ return Array.prototype.slice.call(document.querySelectorAll('#vars input[type=checkbox]')); }
function updateVarCount(){
  var b = varBoxes(), c = document.querySelector('.vdrop .vcount');
  if(c) c.textContent = b.filter(function(x){ return x.checked; }).length + ' / ' + b.length;
}
$(document).on('change', '#vars input[type=checkbox]', updateVarCount);
document.addEventListener('click', function(e){
  var a = e.target.closest('.vall a');
  if(a){
    e.preventDefault();
    var on = a.getAttribute('data-vars') === 'all';
    varBoxes().forEach(function(x){ x.checked = on; });
    $('#vars input[type=checkbox]').first().trigger('change');   // one change event → Shiny reads the whole group
    return;
  }
  var d = document.querySelector('.vdrop[open]');
  if(d && !d.contains(e.target)) d.removeAttribute('open');
});
document.addEventListener('keydown', function(e){
  if(e.key === 'Escape'){ var d = document.querySelector('.vdrop[open]'); if(d) d.removeAttribute('open'); }
});
// Horizon slider labels: it runs 2..5 (exponents); show the reader "100 y / 1 000 y / 10 000 y / 100 000 y".
function fmtScale(n){ return String(Math.pow(10, n)).replace(/\B(?=(\d{3})+(?!\d))/g, ' ') + ' y'; }
$(document).on('shiny:connected', function(){
  var s = $('#hscale').data('ionRangeSlider');
  if(s) s.update({ prettify: fmtScale });
});
)"

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  div(class = "wrap",
    h1("Expected longevity of humanity under extinction risk"),
    p(class = "lead",
      "Each period's annual extinction probability cascades into survival ",
      "probability, population, and cumulative human life lived. ",
      "Deltas compare against the frozen baseline."),
    p(class = "khint lead2", HTML(
      "Edit any <b>extinction probability</b>, <b>survival probability</b>, or ",
      "<b>population potential</b> digit &mdash; click its ",
      "<span class='kbd'>&#9650;</span>/<span class='kbd'>&#9660;</span>, or select and use ",
      "<span class='kbd'>&larr;</span><span class='kbd'>&rarr;</span><span class='kbd'>&uarr;</span><span class='kbd'>&darr;</span>, ",
      "or type <span class='kbd'>0</span>&ndash;<span class='kbd'>9</span> to set the digit. ",
      "Survival edits backtrace into the earlier extinction rates.")),
    div(class = "controls",
      tags$button(id = "undoBtn", type = "button", class = "btn", title = "Undo (Ctrl+Z)",
                  disabled = NA, HTML("&#8630;&nbsp;Undo")),
      tags$button(id = "redoBtn", type = "button", class = "btn", title = "Redo (Ctrl+Y)",
                  disabled = NA, HTML("Redo&nbsp;&#8631;")),
      actionButton("surv100", "100% survival", class = "btn"),
      actionButton("reset", "Reset to defaults", class = "btn"),
      checkboxInput("showfx",    "Show formulas", TRUE, width = "auto"),
      checkboxInput("showarrows", "Show arrows", TRUE, width = "auto")),
    # second row: which variables are shown, the baseline choice, and the time scale
    div(class = "controls",
      # Variables: which metric rows are rendered. A hidden row isn't built at all, so its cells
      # can't be clicked, arrowed into or wheel-edited.
      # A <details> dropdown holding a checkbox per row; the count on the button is kept live by JS.
      tags$details(class = "vdrop",
        tags$summary(class = "btn", "Variables ",
          span(class = "vcount", sprintf("%d / %d", length(VAR_ON), length(VAR_ROWS))),
          HTML(" &#9662;")),
        div(class = "vmenu",
          div(class = "vall",
            tags$a(href = "#", `data-vars` = "all", "All"), " · ",
            tags$a(href = "#", `data-vars` = "none", "None")),
          checkboxGroupInput("vars", NULL, choices = VAR_ROWS, selected = VAR_ON))),
      uiOutput("basebtns", inline = TRUE),
      checkboxInput("showref",   "Show baseline values", FALSE, width = "auto"),
      # Horizon: 100 / 1 000 / 10 000 / 100 000 years. The slider holds the exponent (2..5); a JS prettify
      # hook shows the reader the year count instead. Default 1 000 keeps the first load light.
      div(class = "scalectl",
        span(class = "khint", "Time scale"),
        sliderInput("hscale", NULL, min = min(SCALE_EXP), max = max(SCALE_EXP), value = DEF_EXP,
                    step = 1, ticks = FALSE, width = "170px"))),
    uiOutput("panel"),
    uiOutput("summary"),
    uiOutput("foot"),
    tags$script(HTML(js)))
)

# ── server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # The model for the chosen horizon. Everything downstream — state, baseline, history, the
  # panel — is fitted to it with fit_state(), so a horizon change can never leave a stale
  # 5-column state being rendered into a 3-column model (or vice versa).
  M <- reactive(model_for(as.integer(input$hscale %||% DEF_EXP)))
  def_state <- reactive(list(rates = M()$def_rates, pops = M()$def_pops))

  state_r <- reactive({
    s <- input$state
    if (is.null(s)) def_state() else fit_state(s, M())
  })

  DEF0       <- list(rates = model_for(DEF_EXP)$def_rates, pops = model_for(DEF_EXP)$def_pops)
  mode       <- reactiveVal("previous")          # "current" | "previous"
  ref_static <- reactiveVal(DEF0)                 # frozen baseline (current mode)
  hist       <- reactiveValues(prev = DEF0, last = DEF0)

  # remember the state before the most recent change
  observeEvent(input$state, {
    hist$prev <- hist$last
    hist$last <- state_r()
  })

  ref_state <- reactive(fit_state(if (mode() == "previous") hist$prev else ref_static(), M()))

  # the cascade, computed once per change and shared by the table and the summary
  cur_r <- reactive({ s <- state_r(); compute(s$rates, s$pops, M()) })
  ref_r <- reactive({ s <- ref_state(); compute(s$rates, s$pops, M()) })

  # Horizon changed: resize the current state (keeping edits to the periods both horizons share),
  # hand it to the client, and rebase deltas/history on it — a structural change, not an edit.
  observeEvent(input$hscale, {
    new_state <- fit_state(isolate(state_r()), M())
    session$sendCustomMessage("setScale",
      list(len = as.numeric(M()$seg_len), rates = as.numeric(new_state$rates), pops = as.numeric(new_state$pops)))
    ref_static(new_state); hist$prev <- new_state; hist$last <- new_state
  }, ignoreInit = TRUE)

  observe({                                        # keep JS informed of the baseline (for delta editing)
    b <- ref_state()
    session$sendCustomMessage("baseline",
      list(rates = as.numeric(b$rates), pops = as.numeric(b$pops)))
  })

  observeEvent(input$setref,  { ref_static(state_r()); mode("current") })
  observeEvent(input$setprev, mode("previous"))
  observeEvent(input$surv100, {                   # all survival → 100%, all x-risk → 0; rebase deltas to 0
    new_state <- list(rates = as.numeric(rep(0, M()$n - 1)), pops = as.numeric(state_r()$pops))
    session$sendCustomMessage("setState", new_state)
    ref_static(new_state); mode("current")
  })
  observeEvent(input$reset, {
    d <- def_state()
    session$sendCustomMessage("setState", list(rates = as.numeric(d$rates), pops = as.numeric(d$pops)))
    ref_static(d); hist$prev <- d; hist$last <- d
    mode("previous")
  })

  output$basebtns <- renderUI({
    m <- mode()
    tagList(
      actionButton("setref",  "Set baseline = current",
                   class = if (m == "current")  "btn active" else "btn"),
      actionButton("setprev", "Set baseline = previous value",
                   class = if (m == "previous") "btn active" else "btn"))
  })

  output$panel <- renderUI({
    m   <- M(); n <- m$n; np <- n - 1                 # checkpoints / periods for this horizon
    st  <- state_r(); rf <- ref_state()
    cur <- cur_r()
    ref <- ref_r()
    elapsed <- c(0, cumsum(m$seg_len))               # 0,10,100,(1000,(10000))
    ycol <- function(j) 2 * j                         # grid start for year j
    gcol <- function(i) 2 * i + 1                     # grid start for gap i (between year i & i+1)
    J <- seq_len(n); I <- seq_len(np)                 # column / gap indices
    # "Show baseline values": the baseline in parentheses beside the value, first three variables only
    sr <- isTRUE(input$showref)
    rtxt <- function(v, to_disp, decimals, suffix) if (sr) f_ref(v, to_disp, decimals, suffix) else NULL

    # header: years centered over their columns
    header <- do.call(tags$div, c(list(class = "grow ghead",
      tags$div(class = "rlabel", "")),
      lapply(J, function(j)
        tags$div(class = "ycol", style = paste0("grid-column:", ycol(j), "/span 2;"),
          div(class = "yr", fmt_year(m$cps[j]))))))

    # period labels: "← N years →" over each gap, with the calendar year-range below it. They sit on the
    # annual-probability row, above that period's box; if that row is hidden they get a row of their own.
    ivl_cells <- lapply(I, function(i)
      tags$div(class = "ivlcell", style = paste0("grid-column:", gcol(i), "/span 2;"),
        div(HTML(paste0("&larr; ", m$seg_len[i], " years &rarr;"))),
        div(class = "ivlrng", HTML(paste0(fmt_year(m$seg_start[i]), "&ndash;", fmt_year(m$seg_end[i]))))))
    intervals <- do.call(tags$div, c(list(class = "grow", tags$div(class = "rlabel", "")), ivl_cells))

    # Complementary views: row 1 as annual SURVIVAL (1 - e_t), row 2 as cumulative EXTINCTION (1 - s_t).
    # Same editable cells; the JS maps the rate_c / surv_c kinds back onto the rates (see ALT_KIND).
    rc <- isTRUE(input$rate_comp); sc <- isTRUE(input$surv_comp)

    # 1 · annual extinction (or survival) probability — editable, sits in the gaps between years
    rate_cells <- lapply(I, function(i) {
      cv <- if (rc) 1 - cur$rates[i] else cur$rates[i]
      rv <- if (rc) 1 - ref$rates[i] else ref$rates[i]
      gcell_edit(gcol(i), cur = cv, ref = rv, kind = if (rc) "rate_c" else "rate", vi = i,
        to_disp = 100, decimals = 4, suffix = "%", good = if (rc) "more_good" else "more_bad",
        top   = paste0("<span class='ratio'>1 in ", f_int(1 / cur$rates[i]), "</span>",   # odds of extinction either way
                       if (sr) paste0(" <span class='ratio vref'>(1 in ", f_int(1 / ref$rates[i]), ")</span>")),
        color = COL_RATE, ref_txt = rtxt(rv, 100, 4, "%"))
    })

    # 2 · survival (or extinction) probability given past extinction — editable (backtraces to rates)
    surv_cells <- lapply(J, function(j) {
      if (j == 1)                                     # 2026 is fixed at 100% survival / 0% extinction
        return(gcell_fixed(ycol(1), if (sc) 0 else cur$surv[1], 100, 4, "%", pad = 2, color = COL_SURV))
      cv <- if (sc) 1 - cur$surv[j] else cur$surv[j]
      rv <- if (sc) 1 - ref$surv[j] else ref$surv[j]
      gcell_edit(ycol(j), cur = cv, ref = rv, kind = if (sc) "surv_c" else "surv", vi = j - 1,
        to_disp = 100, decimals = 4, suffix = "%", pad = 2, good = if (sc) "more_bad" else "more_good",
        color = COL_SURV, ref_txt = rtxt(rv, 100, 4, "%"))
    })

    # 3 · cumulative survived years of humanity
    years_cells <- lapply(J, function(j)
      if (j == 1) gcell_ro(ycol(j), cur$cum_years[j], cur$cum_years[j], 1, 4, "y", delta = FALSE, color = COL_SURV)
      else gcell_ro(ycol(j), cur$cum_years[j], ref$cum_years[j], 1, 4, "y", "more_good",
             sub = paste0("of ", f_int(elapsed[j]), "y potential"), color = COL_SURV,
             ref_txt = rtxt(ref$cum_years[j], 1, 4, "y")))

    # 3b · cumulative survived generations (sum of cumulative survival / 75)
    gens_cells <- lapply(J, function(j)
      if (j == 1) gcell_ro(ycol(j), cur$gens[j], cur$gens[j], 1, 4, "", delta = FALSE, color = COL_SURV)
      else gcell_ro(ycol(j), cur$gens[j], ref$gens[j], 1, 4, "", "more_good",
             sub = paste0("of ", formatC(elapsed[j] / LIFE_EXP, format = "f", digits = 2), " potential"), color = COL_SURV,
             ref_txt = rtxt(ref$gens[j], 1, 4, "")))

    # 4 · potential population if survived — editable checkpoints
    pop_pot_cells <- lapply(J, function(j)
      gcell_edit(ycol(j), cur = cur$pop_pot[j], ref = ref$pop_pot[j], kind = "pop", vi = j,
        to_disp = 1 / 1e6, decimals = 0, suffix = "m", pad = 5, good = "more_good",
        color = COL_POP))
    # per-period growth rate sits in the gap between the two checkpoints it spans
    growth_cells <- lapply(I, function(i) {
      g <- cur$growth[i + 1] * 100
      tags$div(class = "vgrowth", style = paste0("grid-column:", gcol(i), "/span 2;"),
        HTML(paste0(if (g >= 0) "+" else "", formatC(g, digits = 3, format = "f"), "%/yr")))
    })

    # 5 · expected population with survival probabilities
    pop_exp_cells <- lapply(J, function(j)
      if (j == 1) gcell_ro(ycol(j), cur$pop_exp[j], cur$pop_exp[j], 1, 0, "", delta = FALSE, color = COL_POP)
      else gcell_ro(ycol(j), cur$pop_exp[j], ref$pop_exp[j], 1, 0, "", "more_good",
             sub = paste0("of ", f_int(cur$pop_pot[j]), " potential"), color = COL_POP))

    # 6 · expected cumulative lived human life-years
    ly_cells <- lapply(J, function(j)
      if (j == 1) gcell_ro(ycol(j), cur$cum_ly[j], cur$cum_ly[j], 1, 0, "", delta = FALSE, color = COL_POP)
      else gcell_ro(ycol(j), cur$cum_ly[j], ref$cum_ly[j], 1, 0, "", "more_good",
             sub = paste0("of ", f_int(cur$cum_ly_pot[j]), " potential"), color = COL_POP))

    # 7 · expected cumulative lived human lives (75y)
    lives_cells <- lapply(J, function(j)
      if (j == 1) gcell_ro(ycol(j), cur$lives[j], cur$lives[j], 1, 0, "", delta = FALSE, color = COL_POP)
      else gcell_ro(ycol(j), cur$lives[j], ref$lives[j], 1, 0, "", "more_good",
             sub = paste0(f_int(cur$lives_pot[j] - cur$lives[j]), " less than potential"), color = COL_POP))

    # arrow endpoints: each value cell carries its node id ("row:j"; the rate row is indexed by period)
    tag_nodes <- function(cells, row, idx)
      Map(function(cl, k) HTML(sub("^<div ", paste0("<div data-node=\"", row, ":", k, "\" "), cl)), cells, idx)
    rate_cells    <- tag_nodes(rate_cells,    "rate",   I)
    surv_cells    <- tag_nodes(surv_cells,    "surv",   J)
    years_cells   <- tag_nodes(years_cells,   "years",  J)
    gens_cells    <- tag_nodes(gens_cells,    "gens",   J)
    pop_pot_cells <- tag_nodes(pop_pot_cells, "pop",    J)
    pop_exp_cells <- tag_nodes(pop_exp_cells, "popexp", J)
    ly_cells      <- tag_nodes(ly_cells,      "ly",     J)
    lives_cells   <- tag_nodes(lives_cells,   "lives",  J)

    # Formula symbols: ae / as = annual extinction / survival, ce / cs = cumulative extinction / survival.
    # Each formula is written in whichever variable the rows above currently show (rc / sc toggles), e.g.
    # with row 1 switched to annual survival, row 2 reads cs_t = prod as_i rather than prod (1 - ae_i).
    sym  <- function(nm, k, col) mv(paste0(nm, "<sub>", k, "</sub>"), col)
    asv  <- function(k) if (rc) sym("as", k, COL_RATE)                        # annual survival, in shown terms
                        else paste0("(1 &minus; ", sym("ae", k, COL_RATE), ")")
    csv  <- function(k) if (sc) paste0("(1 &minus; ", sym("ce", k, COL_SURV), ")")  # cumulative survival, in shown terms
                        else sym("cs", k, COL_SURV)
    fx_rate <- if (rc) paste0(sym("as", "t", COL_RATE), " = 1 &minus; ", sym("ae", "t", COL_RATE))
               else sym("ae", "t", COL_RATE)
    fx_surv <- paste0(if (sc) sym("ce", "t", COL_SURV) else sym("cs", "t", COL_SURV), " = ",
                      if (sc) "1 &minus; " else "", bigop("&prod;", "i=2027", "t"), " ", asv("i"))

    rows <- list(
      rate = metric_grow(if (rc) "Annual survival probability<br><span class='ital'>if survived until then</span>"
                  else    "Annual extinction probability<br><span class='ital'>if survived until then</span>",
                  COL_RATE, c(ivl_cells, rate_cells), cls = "erow rrow", row = "rate", toggle = rtoggle("rate_comp", rc, "Show annual survival", "Show annual extinction"),
                  formula = fx_rate),
      surv = metric_grow(if (sc) paste0("Cumulative extinction probability<br><span class='ital'>given past ", if (rc) "survival" else "extinction", " probabilities</span>")
                  else    paste0("Cumulative survival probability<br><span class='ital'>given past ", if (rc) "survival" else "extinction", " probabilities</span>"),
                  COL_SURV, surv_cells, cls = "erow", row = "surv", toggle = rtoggle("surv_comp", sc, "Show cumulative extinction", "Show cumulative survival"),
                  formula = fx_surv),
      years = metric_grow("Cumulative survived years of humanity",                                 COL_SURV, years_cells,   row = "years",
                  formula = paste0(mv("Y<sub>t</sub>", COL_SURV), " = ", bigop("&sum;", "i=2027", "t"),
                                   " ", csv("i"))),
      gens = metric_grow("Cumulative survived generations <span class='ital'>(75y)</span>",       COL_SURV, gens_cells,    row = "gens",
                  formula = paste0(mv("G<sub>t</sub>", COL_SURV), " = ", bigop("&sum;", "i=2027", "t"), " ", csv("i"), " / 75")),
      pop = metric_grow("Annual population potential<br><span class='ital'>if survived</span>",            COL_POP,  c(pop_pot_cells, growth_cells), cls = "erow", row = "pop",
                  formula = mv("P<sub>t</sub>", COL_POP)),
      popexp = metric_grow("Annual population<br><span class='ital'>given survival probability</span>",     COL_POP,  pop_exp_cells, row = "popexp",
                  formula = paste0(mv("E<sub>t</sub>", COL_POP), " = ", mv("P<sub>t</sub>", COL_POP),
                                   " &middot; ", csv("t"))),
      ly = metric_grow("Cumulative lived human life-years",                            COL_POP,  ly_cells,      row = "ly",
                  formula = paste0(mv("H<sub>t</sub>", COL_POP), " = ", bigop("&sum;", "i=2027", "t"),
                                   " ", mv("E<sub>i</sub>", COL_POP))),
      lives = metric_grow("Cumulative lived human lives <span class='ital'>(75y)</span>", COL_POP,  lives_cells,   row = "lives",
                  formula = paste0(mv("N<sub>t</sub>", COL_POP), " = ", bigop("&sum;", "i=2027", "t"), " ", mv("E<sub>i</sub>", COL_POP), " / 75")))

    # "Variables" dropdown: keep only the chosen rows (always in the fixed display order)
    vis <- intersect(names(rows), input$vars %||% character(0))
    # --halfcol: column pitch cap for this horizon (see the .grow CSS comment) — one number to tune.
    # data-edges: the dependency arrows for the visible rows; the JS draws them as an SVG overlay.
    div(class = "grid-scroll", do.call(div, c(list(class = "grid-wrap", style = paste0("--ncols:", 2 * n, ";--halfcol:", round(1.05 * (46 + 7 * n), 1), "px;"),
      `data-edges` = arrow_edges(vis, n),
      header, if (!"rate" %in% vis) intervals),
      unname(rows[vis]),
      list(if (!length(vis)) div(class = "novars", "No variables selected — pick some from the Variables menu.")))))
  })

  output$foot <- renderUI({
    p   <- state_r()$pops
    cps <- paste(formatC(p / 1e9, format = "f", digits = 1), collapse = " / ")
    div(class = "foot", HTML(paste0(
      "Population potential is interpolated geometrically between the checkpoints ",
      cps, "&nbsp;bn, so it stays internally consistent. ",
      "Lives = cumulative life-years &divide; 75.")))
  })

  output$summary <- renderUI({
    st  <- state_r()
    m   <- M(); n <- m$n
    cur <- cur_r()
    hl  <- function(txt, col) sprintf("<b style='color:%s;'>%s</b>", col, txt)   # value in its row's colour
    div(class = "summary", HTML(paste0(
      "With these rates, humanity survives to ", hl(fmt_year(max(m$cps)), "#e8eef4"),
      " with ", hl(f_pct(cur$surv[n]), COL_SURV), " probability, is expected to live ",
      hl(f_years(cur$cum_years[n]), COL_SURV), " of the next ",
      f_int(sum(m$seg_len)), " years, and to accrue ", hl(f_int(cur$lives[n]), COL_POP),
      " human lives (", hl(f_int(cur$cum_ly[n]), COL_POP), " life-years).")))
  })
}

shinyApp(ui, server)
