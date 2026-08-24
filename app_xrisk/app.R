# ────────────────────────────────────────────────────────────────────────────
# Extinction-risk visualiser — replica of the right-hand data panel from the
# concept slide. Recomputes the whole cascade in R from the four annual
# extinction rates + population checkpoints, so it also serves as a clean
# recomputation to diff against the slide.
#
# Interaction (JavaScript): click an "Annual extinction probability" cell to
# select a period, then:
#   ← / →  move selection between periods
#   ↑ / ↓  raise / lower that period's annual rate (Shift = fine step)
# The whole table + comparison deltas update live. "Set baseline" freezes the
# current state as the comparison reference; "Reset" restores defaults.
#
# Run:  shiny::runApp("app_xrisk")
# ────────────────────────────────────────────────────────────────────────────

library(shiny)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── model config ────────────────────────────────────────────────────────────
START     <- 2026
CPS       <- c(2026, 2036, 2126, 3026, 12026)   # checkpoint (column) years
SEG_START <- c(2027, 2037, 2127, 3027)          # first year of each period
SEG_END   <- c(2036, 2126, 3026, 12026)         # last  year of each period
SEG_LEN   <- SEG_END - SEG_START + 1            # 10, 90, 900, 9000
DEF_RATES <- c(0.0005, 0.0005, 0.0001, 0.00005)   # 0.05%, 0.05%, 0.01%, 0.005% per year
POP_CPS   <- rep(8.3e9, 5)                       # constant 8300 M default at every checkpoint
LIFE_EXP  <- 75

YRS_ALL <- START:max(CPS)                        # 2026 .. 12026

# potential population per year: geometric interpolation between the (editable)
# checkpoint values, so the potential-population column stays internally
# consistent — it hits the checkpoint values exactly at the checkpoint years.
build_pop <- function(pop_cps) {
  pop <- numeric(length(YRS_ALL))
  for (k in seq_len(length(CPS) - 1)) {
    y0 <- CPS[k]; y1 <- CPS[k + 1]; v0 <- pop_cps[k]; v1 <- pop_cps[k + 1]
    ix <- which(YRS_ALL >= y0 & YRS_ALL <= y1)
    pop[ix] <- v0 * (v1 / v0) ^ ((YRS_ALL[ix] - y0) / (y1 - y0))
  }
  pop
}

# ── the cascade ─────────────────────────────────────────────────────────────
compute <- function(rates, pop_cps) {
  POP_ALL <- build_pop(pop_cps)
  seg  <- findInterval(YRS_ALL, SEG_START)          # 0 for 2026, else 1..4
  r    <- ifelse(seg >= 1, rates[pmax(seg, 1)], 0)
  surv <- cumprod(1 - r)                             # survival to end of year
  pop_exp <- POP_ALL * surv
  lived   <- seg >= 1                                # years that actually count
  cum_years  <- cumsum(ifelse(lived, surv,    0))
  cum_ly     <- cumsum(ifelse(lived, pop_exp, 0))
  cum_ly_pot <- cumsum(ifelse(lived, POP_ALL, 0))
  ix <- match(CPS, YRS_ALL)
  growth <- c(NA, (pop_cps[-1] / pop_cps[-length(pop_cps)]) ^ (1 / diff(CPS)) - 1)
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

# colours
COL_RATE  <- "#e8836f"; COL_SURV <- "#4fb8d6"; COL_POP <- "#e46ba6"
GOOD <- "#7fd18f"; BAD <- "#ee4d4d"; ZERO <- "#8a97a3"

# delta pill: kind = "more_good" (up is green) or "more_bad" (up is red)
fmt_delta <- function(cur, ref, kind, fmt) {
  d  <- cur - ref
  ds <- fmt(abs(d))
  if (abs(d) < 1e-9 || !grepl("[1-9]", ds))            # no change, or rounds to 0 in display → grey ±0
    return(span(class = "d0", "±0"))                    # ±0
  good <- (d > 0) == (kind == "more_good")
  sgn  <- if (d > 0) "+" else "−"                       # − for minus
  span(style = paste0("color:", if (good) GOOD else BAD, ";"),
       paste0(sgn, ds))
}

# plain (non-editable) value cell on the grid (spans 2 half-columns)
gcell <- function(col, main, delta = NULL, sub = NULL, color = "#e8eef4") {
  tags$div(class = "vcell", style = paste0("grid-column:", col, "/span 2;"),
    div(class = "vstack",
      div(class = "vmain", style = paste0("color:", color, ";"), HTML(main)),
      if (!is.null(delta)) div(class = "vdelta", delta)),
    if (!is.null(sub)) div(class = "vsub", HTML(sub)))
}

# calendar year: thin-space separator only for 5+ digit years (12026 → "12 026")
fmt_year <- function(y) { y <- as.integer(y); if (y >= 10000) group3(as.character(y)) else as.character(y) }

# insert thin-space thousands separators into a plain digit string
group3 <- function(s) {
  ch <- rev(strsplit(s, "")[[1]]); out <- character(0)
  for (i in seq_along(ch)) { if (i > 1 && (i - 1) %% 3 == 0) out <- c(out, " "); out <- c(out, ch[i]) }
  paste(rev(out), collapse = "")
}

# build digit slots for a number — used for both the value and its (editable) delta.
#   native      : native value; abs() is shown, sign handled via sign_char
#   to_disp     : multiplier to display units (rate 100 → %, pop 1/1e6 → m)
#   decimals    : decimal places in display units
#   pad         : if set, always show this many integer digits (leading zeros greyed
#                 but still adjustable — lets the user ramp big places fast)
#   digit_color : function(is_zero, is_lead) -> CSS colour or NULL (inherit)
#   sign_char/sign_color : optional leading sign slot (for deltas)
build_slots <- function(native, to_disp, decimals, suffix, pad = NULL, editable = TRUE,
                        digit_color = NULL, sign_char = NULL, sign_color = NULL) {
  a <- abs(native)
  whole <- formatC(a * to_disp, format = "f", digits = decimals)   # non-scientific; rounds
  parts <- strsplit(whole, ".", fixed = TRUE)[[1]]
  ip    <- parts[1]
  if (!is.null(pad) && nchar(ip) < pad) ip <- paste0(strrep("0", pad - nchar(ip)), ip)
  ip    <- group3(ip)                                              # thin-space thousands on integer part
  s     <- if (length(parts) > 1) paste0(ip, ".", parts[2]) else ip
  chars <- strsplit(s, "")[[1]]
  n     <- length(chars)
  dot   <- match(".", chars); if (is.na(dot)) dot <- n + 1
  place <- rep(NA_real_, n)
  k <- 0                                            # integer digits, right → left
  for (p in seq(dot - 1, 1)) {
    if (grepl("[0-9]", chars[p])) { place[p] <- (10^k) / to_disp; k <- k + 1 }
  }
  if (dot < n) {                                    # decimal digits, left → right
    kk <- 1
    for (p in seq(dot + 1, n)) {
      if (grepl("[0-9]", chars[p])) { place[p] <- (10^(-kk)) / to_disp; kk <- kk + 1 }
    }
  }
  lead <- rep(FALSE, n)                             # zeros before the first significant digit
  seen <- FALSE
  for (p in seq_len(n)) if (grepl("[0-9]", chars[p])) {
    if (!seen && chars[p] == "0") lead[p] <- TRUE else seen <- TRUE
  }
  # editable digits carry ▲/▼ arrows (with matching spacers on separators so rows line up);
  # read-only digits carry neither, so those rows have no wasted vertical space.
  mkslot <- function(p) {
    ch <- chars[p]
    if (!is.na(place[p])) {
      col <- if (!is.null(digit_color)) digit_color(ch == "0", lead[p])
             else if (!is.null(pad) && lead[p]) "#5f7484" else NULL
      tags$span(class = "dig", `data-delta` = formatC(place[p], format = "e", digits = 6),
        if (editable) span(class = "ar up", HTML("&#9650;")),
        tags$b(class = "dch", style = if (!is.null(col)) paste0("color:", col, ";"), ch),
        if (editable) span(class = "ar dn", HTML("&#9660;")))
    } else
      tags$span(class = "sep",
        if (editable) span(class = "arsp"),
        tags$b(class = "dch", if (ch == " ") HTML("&nbsp;") else ch),
        if (editable) span(class = "arsp"))
  }
  # split positions into thousands-groups (a space starts a new group; the dot +
  # decimals stay with their group). Each group is one non-breaking wrap unit, so a
  # number wraps only between groups — never mid-group — on narrow widths.
  groups <- list(); cur_g <- integer(0)
  for (p in seq_len(n)) {
    if (chars[p] == " ") { if (length(cur_g)) groups <- c(groups, list(cur_g)); cur_g <- p }
    else cur_g <- c(cur_g, p)
  }
  if (length(cur_g)) groups <- c(groups, list(cur_g))

  sign_slot <- if (!is.null(sign_char))
    tags$span(class = "sep", if (editable) span(class = "arsp"),
      tags$b(class = "dch sgn", style = if (!is.null(sign_color)) paste0("color:", sign_color, ";"),
             HTML(sign_char)), if (editable) span(class = "arsp")) else NULL
  suffix_slot <- if (nzchar(suffix))
    tags$span(class = "sep", if (editable) span(class = "arsp"),
      tags$b(class = "dch suf", HTML(suffix)), if (editable) span(class = "arsp")) else NULL

  ng <- length(groups)
  lapply(seq_len(ng), function(gi) {
    items <- lapply(groups[[gi]], mkslot)
    if (gi == 1  && !is.null(sign_slot))   items <- c(list(sign_slot), items)     # sign stays with the leading digit
    if (gi == ng && !is.null(suffix_slot)) items <- c(items, list(suffix_slot))   # suffix stays with the trailing digit
    do.call(tags$span, c(list(class = "dgroup"), items))
  })
}

# editable value cell: per-digit spinner
gcell_edit <- function(col, cur, ref, kind, vi, to_disp, decimals, suffix,
                       good = "more_good", sub = NULL, color, pad = NULL, top = NULL) {
  dv   <- cur - ref
  cpos <- if (good == "more_good") GOOD else BAD    # colour when delta positive
  cneg <- if (good == "more_good") BAD  else GOOD
  dshow <- if (!is.null(pad)) formatC(round(abs(dv) * to_disp), format = "d", width = pad, flag = "0")
           else formatC(abs(dv) * to_disp, format = "f", digits = decimals)
  disp_zero <- abs(dv) < 1e-12 || !grepl("[1-9]", dshow)   # exact, or rounds to 0 in display → grey
  dcol <- if (disp_zero) ZERO else if (dv > 0) cpos else cneg
  sgn  <- if (disp_zero) "±" else if (dv > 0) "+" else "−"
  digit_color <- function(is0, lead) if (lead) ZERO else dcol
  tags$div(class = "vcell xedit", style = paste0("grid-column:", col, "/span 2;"),
    `data-kind` = kind, `data-i` = vi - 1,
    if (!is.null(top)) div(class = "vtop", HTML(top)),
    div(class = "vstack",
      div(class = "vmain digits", style = paste0("color:", color, ";"),
          build_slots(cur, to_disp, decimals, suffix, pad, TRUE)),
      div(class = "vdelta digits",
          build_slots(dv, to_disp, decimals, suffix, pad, TRUE,
                      digit_color = digit_color, sign_char = sgn, sign_color = dcol))),
    if (!is.null(sub)) div(class = "vsub", HTML(sub)))
}

# read-only value cell: value + a digit-aligned (non-editable) delta below it.
# The delta shows one grey zero under each value digit when there is no change,
# and the signed change (leading zeros greyed) when there is.
gcell_ro <- function(col, cur, ref, to_disp, decimals, suffix, kind = "more_good",
                     sub = NULL, color, delta = TRUE) {
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
  tags$div(class = "vcell", style = paste0("grid-column:", col, "/span 2;"),
    div(class = "vstack",
      # value carries an invisible sign slot when a delta follows, so value and delta
      # are the same width and wrap in lockstep (no ragged extra line).
      div(class = "vmain digits", style = paste0("color:", color, ";"),
          build_slots(cur, to_disp, decimals, suffix, pad, FALSE,
                      sign_char = if (delta) "±" else NULL, sign_color = "transparent")),
      if (delta) div(class = "vdelta digits",
          build_slots(dv, to_disp, decimals, suffix, pad, FALSE,
                      digit_color = digit_color, sign_char = sgn, sign_color = dcol))),
    if (!is.null(sub)) div(class = "vsub", HTML(sub)))
}

# big-operator (∏ / ∑) with limits stacked over/under the symbol
bigop <- function(sym, lo, up)
  sprintf('<span class="bigop"><span class="lim up">%s</span><span class="op">%s</span><span class="lim lo">%s</span></span>',
          up, sym, lo)

# colour a variable symbol (with its subscript) to match its metric's colour
mv <- function(txt, col) sprintf('<span style="color:%s;">%s</span>', col, txt)

# a full metric row (label in column 1, then its value cells).
# cls = "erow" tints the whole row (label included) to flag it as editable.
# row = id used by the accordion toggle to collapse/expand this row.
metric_grow <- function(label, color, cells, cls = NULL, row = NULL, formula = NULL)
  do.call(tags$div, c(list(class = trimws(paste("grow", cls %||% "")), `data-row` = row,
    tags$div(class = "rlabel", style = paste0("color:", color, ";"),
      span(class = "acc", HTML("&#9662;")), span(class = "rlbl", HTML(label)),
      if (!is.null(formula)) div(class = "rformula", HTML(formula)))),
    cells))

# ── UI ──────────────────────────────────────────────────────────────────────
css <- r"(
body{background:#0b3552;color:#e8eef4;font-family:Arial,Helvetica,sans-serif;margin:0;}
.wrap{max-width:1240px;margin:0 auto;padding:18px 22px 70px;}
h1{font-size:21px;font-weight:700;margin:0 0 4px;}
.lead{font-size:13px;color:#b7c6d3;margin:0 0 14px;line-height:1.5;}
.controls{display:flex;gap:10px;align-items:center;margin:12px 0 20px;flex-wrap:wrap;}
.controls .btn{background:#12405f;color:#e8eef4;border:1px solid #2a6a8f;border-radius:6px;
  padding:6px 13px;font-size:13px;cursor:pointer;}
.controls .btn:hover{background:#175981;}
.controls .btn:disabled{opacity:0.35;cursor:default;background:#12405f;box-shadow:none;}
.controls .btn.active{background:#1f77a8;border-color:#8fd6f2;color:#fff;box-shadow:0 0 0 1px #8fd6f2 inset;}
.khint{font-size:12.5px;color:#9fb0bf;}
.kbd{display:inline-block;background:#0a2438;border:1px solid #2a6a8f;border-radius:4px;
  padding:0 7px;font-size:12px;margin:0 1px;line-height:18px;}
.grid-wrap{margin-top:4px;}
.grow{display:grid;grid-template-columns:200px repeat(10,1fr);align-items:start;
  border-bottom:1px solid rgba(255,255,255,.05);}
.grow.ghead{border-bottom:1px solid rgba(255,255,255,.18);}
.grow.erow{background:rgba(255,255,255,.05);border-radius:7px;margin:3px 0;}
.rlabel{grid-column:1;grid-row:1;text-align:right;font-size:12.5px;line-height:1.35;font-weight:600;
  align-self:stretch;padding:9px 12px 9px 0;border-right:1px solid rgba(255,255,255,.10);}
.acc{cursor:pointer;color:#8fa3b3;font-size:10px;margin-right:6px;display:inline-block;user-select:none;}
.acc:hover{color:#ffd27f;}
.grow.collapsed .acc{transform:rotate(-90deg);}
.grow.collapsed .vcell{display:none;}
.grow.collapsed .rformula{display:none;}
.grow.collapsed{margin:1px 0;}
.rformula{font-size:12px;color:#cbb68e;margin-top:4px;padding-left:0;font-weight:400;
  line-height:1.6;font-family:Cambria,Georgia,"Times New Roman",serif;font-style:italic;}
.rformula sub{font-size:8.5px;font-style:normal;}
.rformula sup{font-size:8.5px;font-style:normal;}
.rformula .bigop{display:inline-flex;flex-direction:column;align-items:center;
  vertical-align:middle;font-style:normal;line-height:1;margin:0 3px;}
.rformula .bigop .op{font-size:20px;line-height:1;}
.rformula .bigop .lim{font-size:8px;line-height:1;font-style:normal;white-space:nowrap;}
.rformula .bigop .lim.up{margin-bottom:1px;}
.rformula .bigop .lim.lo{margin-top:3px;}
.ycol{text-align:center;padding:2px 6px 7px;}
.ycol .yr{font-size:21px;font-weight:700;}
.ivlcell{text-align:center;font-size:11px;color:#9fb0bf;padding:3px 4px;align-self:center;
  white-space:nowrap;}
.ivlrng{font-size:11px;font-style:italic;color:#8496a4;margin-top:3px;white-space:nowrap;}
.vcell{grid-row:1;padding:8px 7px;display:flex;flex-direction:column;align-items:center;text-align:center;min-width:0;}
.vstack{display:inline-flex;flex-direction:column;align-items:stretch;max-width:100%;}
.vmain{font-size:13px;line-height:1.2;font-weight:700;word-break:break-word;font-variant-numeric:tabular-nums;text-align:right;}
.vmain .per{font-size:10px;color:#9fb0bf;font-weight:400;}
.vdelta{font-size:13px;margin-top:1px;font-weight:600;font-variant-numeric:tabular-nums;text-align:right;line-height:1.15;}
.vdelta .d0{color:#8a97a3;}
.vsub{font-size:11px;color:#8496a4;margin-top:3px;font-style:italic;line-height:1.35;}
.vgrowth{grid-row:1;align-self:start;margin-top:22px;text-align:center;font-size:12px;color:#8496a4;
  font-style:italic;white-space:nowrap;padding:0 3px;pointer-events:none;}
@media (max-width:860px){ .vgrowth{grid-row:auto;margin-top:3px;} }  /* narrow: drop below, no overlap */
.grow.collapsed .vgrowth{display:none;}
.ratio{color:#e8836f;font-size:13px;font-weight:600;font-style:normal;}
.rng{font-size:12px;font-style:italic;color:#8496a4;}
.vtop{line-height:1.1;margin-bottom:1px;}
.ital{font-style:italic;font-weight:400;opacity:.85;}
.digits{display:flex;justify-content:flex-end;align-items:flex-end;flex-wrap:wrap;}
.dgroup{display:inline-flex;align-items:flex-end;}   /* a thousands-group: never breaks internally */
.dig,.sep{display:flex;flex-direction:column;align-items:center;line-height:1;}
.dig{width:1ch;}                 /* digit width fixed; larger arrows overflow it */
.dch{font-size:13px;font-weight:700;font-variant-numeric:tabular-nums;padding:0;}
.dch.suf{color:#9fb0bf;}
.dig.lead .dch{color:#5f7484;}
.ar{cursor:pointer;font-size:11px;color:#5f7484;user-select:none;height:13px;line-height:13px;padding:0;}
.ar:hover{color:#ffd27f;}
.arsp{height:13px;}
.dig.digsel .dch{color:#ffd27f !important;}
.dig.digsel .ar{color:#e8b96a;}
.xedit{cursor:pointer;border-radius:6px;}
.xedit:hover{background:rgba(255,255,255,.045);}
.xedit.sel{outline:2px solid rgba(255,210,127,.55);outline-offset:-2px;background:rgba(255,210,127,.06);}
.summary{margin:18px 0 0;font-size:13px;color:#cdd9e3;line-height:1.55;
  border-top:1px solid rgba(255,255,255,.10);padding-top:12px;}
.summary b{color:#ffd27f;}
.foot{margin-top:22px;font-size:11.5px;color:#7d8fa0;line-height:1.5;}
)"

js <- r"(
var rates = [0.0005, 0.0005, 0.0001, 0.00005];
var pops  = [8.3e9, 8.3e9, 8.3e9, 8.3e9, 8.3e9];
var LEN   = [10, 90, 900, 9000];                 // interval lengths (years)
var baseRates = rates.slice(), basePops = pops.slice();   // comparison baseline (from server)
var sel   = {kind:'rate', i:1, place:0.00001, delta:false};  // value/delta + which digit

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

function pushState(){ if(window.Shiny) Shiny.setInputValue('state', {rates:rates, pops:pops}, {priority:'event'}); recordHistory(); }
function cellOf(s){ return document.querySelector('.xedit[data-kind="'+s.kind+'"][data-i="'+s.i+'"]'); }
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
var MINPLACE = { rate:1e-6, surv:1e-6, pop:1e6 };  // smallest editable digit per kind
function valOf(kind, i, rArr, pArr){ return kind === 'surv' ? survOf(rArr, i) : (kind === 'rate' ? rArr : pArr)[i]; }
function curVal(){  return valOf(sel.kind, sel.i, rates, pops); }
function baseVal(){ return valOf(sel.kind, sel.i, baseRates, basePops); }
function setValAbs(v){                            // write the actual value (backtrace for surv)
  if(sel.kind === 'surv'){ setSurv(sel.i, v); }
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
}
document.addEventListener('keydown', function(e){
  var k = e.key;
  if(/^[0-9]$/.test(k)){ e.preventDefault(); typeDigit(+k); return; }
  if(['ArrowUp','ArrowDown','ArrowLeft','ArrowRight'].indexOf(k) < 0) return;
  e.preventDefault();
  if(k === 'ArrowUp')   { change(1);  return; }
  if(k === 'ArrowDown') { change(-1); return; }
  var cells = allCells(), cell = cellOf(sel); if(!cell) return;
  var digs = digsOf(cell), idx = nearestDig(digs, sel.place), ci = cells.indexOf(cell);
  if(k === 'ArrowRight'){
    if(idx < digs.length - 1) sel.place = digDelta(digs[idx + 1]);
    else if(ci < cells.length - 1){ var nc = cells[ci + 1], nd = digsOf(nc);
      sel = {kind:nc.getAttribute('data-kind'), i:+nc.getAttribute('data-i'), place:digDelta(nd[0]), delta:sel.delta}; }
  } else {                                         // ArrowLeft
    if(idx > 0) sel.place = digDelta(digs[idx - 1]);
    else if(ci > 0){ var pc = cells[ci - 1], pd = digsOf(pc);
      sel = {kind:pc.getAttribute('data-kind'), i:+pc.getAttribute('data-i'), place:digDelta(pd[pd.length - 1]), delta:sel.delta}; }
  }
  applyHighlight();
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
var collapsed = {};                              // row id -> hidden?
function applyCollapsed(){
  document.querySelectorAll('.grow[data-row]').forEach(function(g){
    g.classList.toggle('collapsed', !!collapsed[g.getAttribute('data-row')]);
  });
}
document.addEventListener('click', function(e){
  var acc = e.target.closest('.acc'); if(!acc) return;
  var g = acc.closest('.grow'); if(!g) return;
  var r = g.getAttribute('data-row'); collapsed[r] = !collapsed[r]; applyCollapsed();
});
$(document).on('shiny:value', function(ev){ if(ev.name === 'panel') setTimeout(function(){ applyHighlight(); applyCollapsed(); }, 0); });
$(document).on('shiny:connected', function(){ pushState(); setTimeout(function(){ applyHighlight(); applyCollapsed(); updateHistBtns(); }, 60); });
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
}
)"

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  div(class = "wrap",
    h1("Expected longevity of humanity under extinction risk"),
    p(class = "lead",
      "Each period's annual extinction probability cascades into survival ",
      "probability, expected population, and cumulative human life lived. ",
      "Deltas compare against the frozen baseline."),
    div(class = "controls",
      tags$button(id = "undoBtn", type = "button", class = "btn", title = "Undo (Ctrl+Z)",
                  disabled = NA, HTML("&#8630;&nbsp;Undo")),
      tags$button(id = "redoBtn", type = "button", class = "btn", title = "Redo (Ctrl+Y)",
                  disabled = NA, HTML("Redo&nbsp;&#8631;")),
      actionButton("surv100", "100% survival", class = "btn"),
      uiOutput("basebtns", inline = TRUE),
      actionButton("reset", "Reset to defaults", class = "btn"),
      span(class = "khint", HTML(
        "Edit any <b>extinction probability</b>, <b>survival probability</b>, or ",
        "<b>potential population</b> digit &mdash; click its ",
        "<span class='kbd'>&#9650;</span>/<span class='kbd'>&#9660;</span>, or select and use ",
        "<span class='kbd'>&larr;</span><span class='kbd'>&rarr;</span><span class='kbd'>&uarr;</span><span class='kbd'>&darr;</span>, ",
        "or type <span class='kbd'>0</span>&ndash;<span class='kbd'>9</span> to set the digit. ",
        "Survival edits backtrace into the earlier extinction rates."))),
    uiOutput("panel"),
    uiOutput("summary"),
    uiOutput("foot"),
    tags$script(HTML(js)))
)

# ── server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  DEF_STATE <- list(rates = DEF_RATES, pops = POP_CPS)
  state_r <- reactive({
    s <- input$state
    if (is.null(s)) DEF_STATE
    else            list(rates = as.numeric(s$rates), pops = as.numeric(s$pops))
  })

  mode       <- reactiveVal("previous")          # "current" | "previous"
  ref_static <- reactiveVal(DEF_STATE)            # frozen baseline (current mode)
  hist       <- reactiveValues(prev = DEF_STATE, last = DEF_STATE)

  # remember the state before the most recent change
  observeEvent(input$state, {
    hist$prev <- hist$last
    hist$last <- state_r()
  })

  ref_state <- reactive(if (mode() == "previous") hist$prev else ref_static())

  observe({                                        # keep JS informed of the baseline (for delta editing)
    b <- ref_state()
    session$sendCustomMessage("baseline",
      list(rates = as.numeric(b$rates), pops = as.numeric(b$pops)))
  })

  observeEvent(input$setref,  { ref_static(state_r()); mode("current") })
  observeEvent(input$setprev, mode("previous"))
  observeEvent(input$surv100, {                   # all survival → 100%, all x-risk → 0; rebase deltas to 0
    new_state <- list(rates = as.numeric(c(0, 0, 0, 0)), pops = as.numeric(state_r()$pops))
    session$sendCustomMessage("setState", new_state)
    ref_static(new_state); mode("current")
  })
  observeEvent(input$reset, {
    session$sendCustomMessage("setState",
      list(rates = as.numeric(DEF_RATES), pops = as.numeric(POP_CPS)))
    ref_static(DEF_STATE); hist$prev <- DEF_STATE; hist$last <- DEF_STATE
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
    st  <- state_r(); rf <- ref_state()
    cur <- compute(st$rates, st$pops)
    ref <- compute(rf$rates, rf$pops)
    elapsed <- c(0, cumsum(SEG_LEN))                 # 0,10,100,1000,10000
    ycol <- function(j) 2 * j                         # grid start for year j
    gcol <- function(i) 2 * i + 1                     # grid start for gap i (between year i & i+1)

    # header: years centered over their columns
    header <- do.call(tags$div, c(list(class = "grow ghead",
      tags$div(class = "rlabel", "")),
      lapply(1:5, function(j)
        tags$div(class = "ycol", style = paste0("grid-column:", ycol(j), "/span 2;"),
          div(class = "yr", fmt_year(CPS[j]))))))

    # interval row: "← N years →" over each gap, with the calendar year-range below it
    intervals <- do.call(tags$div, c(list(class = "grow",
      tags$div(class = "rlabel", "")),
      lapply(1:4, function(i)
        tags$div(class = "ivlcell", style = paste0("grid-column:", gcol(i), "/span 2;"),
          div(HTML(paste0("&larr; ", SEG_LEN[i], " years &rarr;"))),
          div(class = "ivlrng", HTML(paste0(fmt_year(SEG_START[i]), "&ndash;", fmt_year(SEG_END[i]))))))))

    # 1 · annual extinction probability — editable, sits in the gaps between years
    rate_cells <- lapply(1:4, function(i)
      gcell_edit(gcol(i), cur = cur$rates[i], ref = ref$rates[i], kind = "rate", vi = i,
        to_disp = 100, decimals = 4, suffix = "%", good = "more_bad",
        top   = paste0("<span class='ratio'>1 in ", f_int(1 / cur$rates[i]), "</span>"),
        color = COL_RATE))

    # 2 · survival probability given past extinction — editable (backtraces to rates)
    surv_cells <- lapply(1:5, function(j) {
      if (j == 1)                                     # 2026 is fixed at 100%
        return(gcell(ycol(1), f_pct(cur$surv[1]), color = COL_SURV))
      gcell_edit(ycol(j), cur = cur$surv[j], ref = ref$surv[j], kind = "surv", vi = j - 1,
        to_disp = 100, decimals = 4, suffix = "%", good = "more_good",
        color = COL_SURV)
    })

    # 3 · cumulative survived years of humanity
    years_cells <- lapply(1:5, function(j)
      if (j == 1) gcell_ro(ycol(j), cur$cum_years[j], cur$cum_years[j], 1, 4, "y", delta = FALSE, color = COL_SURV)
      else gcell_ro(ycol(j), cur$cum_years[j], ref$cum_years[j], 1, 4, "y", "more_good",
             sub = paste0("of ", f_int(elapsed[j]), "y potential"), color = COL_SURV))

    # 4 · potential population if survived — editable checkpoints
    pop_pot_cells <- lapply(1:5, function(j)
      gcell_edit(ycol(j), cur = cur$pop_pot[j], ref = ref$pop_pot[j], kind = "pop", vi = j,
        to_disp = 1 / 1e6, decimals = 0, suffix = "m", pad = 5, good = "more_good",
        color = COL_POP))
    # per-period growth rate sits in the gap between the two checkpoints it spans
    growth_cells <- lapply(1:4, function(i) {
      g <- cur$growth[i + 1] * 100
      tags$div(class = "vgrowth", style = paste0("grid-column:", gcol(i), "/span 2;"),
        HTML(paste0(if (g >= 0) "+" else "", formatC(g, digits = 3, format = "f"), "%/yr")))
    })

    # 5 · expected population with survival probabilities
    pop_exp_cells <- lapply(1:5, function(j)
      if (j == 1) gcell_ro(ycol(j), cur$pop_exp[j], cur$pop_exp[j], 1, 0, "", delta = FALSE, color = COL_POP)
      else gcell_ro(ycol(j), cur$pop_exp[j], ref$pop_exp[j], 1, 0, "", "more_good",
             sub = paste0("of ", f_int(cur$pop_pot[j]), " potential"), color = COL_POP))

    # 6 · expected cumulative lived human life-years
    ly_cells <- lapply(1:5, function(j)
      if (j == 1) gcell_ro(ycol(j), cur$cum_ly[j], cur$cum_ly[j], 1, 0, "", delta = FALSE, color = COL_POP)
      else gcell_ro(ycol(j), cur$cum_ly[j], ref$cum_ly[j], 1, 0, "", "more_good",
             sub = paste0("of ", f_int(cur$cum_ly_pot[j]), " potential"), color = COL_POP))

    # 7 · expected cumulative lived human lives (75y)
    lives_cells <- lapply(1:5, function(j)
      if (j == 1) gcell_ro(ycol(j), cur$lives[j], cur$lives[j], 1, 0, "", delta = FALSE, color = COL_POP)
      else gcell_ro(ycol(j), cur$lives[j], ref$lives[j], 1, 0, "", "more_good",
             sub = paste0(f_int(cur$lives_pot[j] - cur$lives[j]), " less than potential"), color = COL_POP))

    div(class = "grid-wrap",
      header,
      intervals,
      metric_grow("Annual extinction<br>probability <span class='ital'>if survived</span>",  COL_RATE, rate_cells,    cls = "erow", row = "rate",
                  formula = mv("e<sub>t</sub>", COL_RATE)),
      metric_grow("Survival probability<br><span class='ital'>given past extinction</span>",  COL_SURV, surv_cells,    cls = "erow", row = "surv",
                  formula = paste0(mv("s<sub>t</sub>", COL_SURV), " = ", bigop("&prod;", "i=2027", "t"),
                                   " (1 &minus; ", mv("e<sub>i</sub>", COL_RATE), ")")),
      metric_grow("Cumulative survived<br>years of humanity",                                 COL_SURV, years_cells,   row = "years",
                  formula = paste0(mv("L<sub>t</sub>", COL_SURV), " = ", bigop("&sum;", "i=2027", "t"),
                                   " ", mv("s<sub>i</sub>", COL_SURV))),
      metric_grow("Potential population<br><span class='ital'>if survived</span>",            COL_POP,  c(pop_pot_cells, growth_cells), cls = "erow", row = "pop",
                  formula = mv("P<sub>t</sub>", COL_POP)),
      metric_grow("Expected population<br><span class='ital'>with survival prob.</span>",     COL_POP,  pop_exp_cells, row = "popexp",
                  formula = paste0(mv("E<sub>t</sub>", COL_POP), " = ", mv("P<sub>t</sub>", COL_POP),
                                   " &middot; ", mv("s<sub>t</sub>", COL_SURV))),
      metric_grow("Expected cumulative<br>lived human life-years",                            COL_POP,  ly_cells,      row = "ly",
                  formula = paste0(mv("H<sub>t</sub>", COL_POP), " = ", bigop("&sum;", "i=2027", "t"),
                                   " ", mv("E<sub>i</sub>", COL_POP))),
      metric_grow("Expected cumulative<br>lived human lives <span class='ital'>(75y)</span>", COL_POP,  lives_cells,   row = "lives",
                  formula = paste0(mv("N<sub>t</sub>", COL_POP), " = ", mv("H<sub>t</sub>", COL_POP), " / 75")))
  })

  output$foot <- renderUI({
    p   <- state_r()$pops
    cps <- paste(formatC(p / 1e9, format = "f", digits = 1), collapse = " / ")
    div(class = "foot", HTML(paste0(
      "Potential population is interpolated geometrically between the checkpoints ",
      cps, "&nbsp;bn, so it stays internally consistent. ",
      "Lives = cumulative life-years &divide; 75.")))
  })

  output$summary <- renderUI({
    st  <- state_r()
    cur <- compute(st$rates, st$pops)
    div(class = "summary", HTML(paste0(
      "With these rates, humanity survives to <b>", fmt_year(max(CPS)),
      "</b> with <b>", f_pct(cur$surv[5]), "</b> probability, is expected to live ",
      "<b>", f_years(cur$cum_years[5]), "</b> of the next ",
      f_int(sum(SEG_LEN)), " years, and to accrue <b>", f_int(cur$lives[5]),
      "</b> human lives (", f_int(cur$cum_ly[5]), " life-years).")))
  })
}

shinyApp(ui, server)
