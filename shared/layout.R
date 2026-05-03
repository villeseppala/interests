# shared/layout.R — Shared layout engine, graph I/O, and rendering utilities
# Sourced by both app_author/app.R and app_publish/app.R

library(jsonlite)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── Constants ────────────────────────────────────────────────────────────────

ALL_GROUPS   <- c("Theme", "Project", "Skill", "Funding", "Vote")
GROUP_PREFIX <- list(Theme = "t", Project = "p", Skill = "s")

NODE_H <- list(Theme = 46, Project = 66, Funding = 44, Vote = 44)
NODE_W <- list(Theme = 195, Project = 444, Skill = 225, Funding = 255, Vote = 255)

HEADER_MARGIN <- 70L

# ── Mobile layout defaults ───────────────────────────────────────────────────

MOBILE_DEFAULTS <- list(
  mob_font_mult    = 1.5,
  mob_h_theme_mult = 3.0,
  mob_h_proj_mult  = 3.0,
  mob_h_skill_mult = 3.0,
  mob_gap_v_mult   = 1.0,
  mob_gap_col_mult = 1.0
)

GROUP_COLORS <- list(
  Theme   = "#3be37a",
  Project = "#ffad33",
  Skill   = "#78e6e7",
  Funding = "#78c4e8",
  Vote    = "#e8c478"
)

# ── Funding preference text (rendered in sidebar) ────────────────────────────

FUNDING_ITEMS <- c(
  "\u2013 Crowdfunding",
  "\u2013 Grants",
  "\u2013 Freelance work (toiminimi)",
  "\u2013 Fixed term employment",
  "    \u2013 Part time",
  "    \u2013 Full time",
  "        \u2013 Ongoing employment",
  "            \u2013 Part time",
  "            \u2013 Full time"
)

build_funding_html <- function() {
  html_items <- vapply(FUNDING_ITEMS, function(line) {
    stripped <- sub("^( +)", "", line)
    n_spaces <- nchar(line) - nchar(stripped)
    n_nbsp   <- n_spaces * 3
    paste0(strrep("&nbsp;", n_nbsp), stripped)
  }, character(1), USE.NAMES = FALSE)
  paste0(
    '<div style="margin-bottom:8px;line-height:1.6;">',
    'Current preference order to fund work on something related to the themes, ',
    'projects and skills presented, when not working on my own time on them:',
    '</div>',
    '<div style="line-height:1.7;">',
    paste(html_items, collapse = "<br>"),
    '</div>'
  )
}

# ── Graph I/O ────────────────────────────────────────────────────────────────

read_graph <- function(path) {
  if (!file.exists(path)) stop("Graph file not found: ", path)
  g <- fromJSON(path, simplifyVector = FALSE)
  if (!is.null(g$layout$nodesep)) {
    g$layout <- list(gap_v = 18, gap_col = 400)
    g$order  <- make_order_from_nodes(g$nodes, list())
  }
  for (grp in c("Funding", "Vote"))
    if (is.null(g$order[[grp]])) g$order[[grp]] <- list()
  g <- migrate_subskills(g)
  g
}

write_graph <- function(g, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(toJSON(g, auto_unbox = TRUE, pretty = TRUE), path, useBytes = TRUE)
}

migrate_subskills <- function(g) {
  sub_nodes <- Filter(function(n) identical(n$group, "Subskill"), g$nodes)
  if (length(sub_nodes) == 0) return(g)
  parent_subs <- list()
  for (sn in sub_nodes) {
    pid <- as.character(as.numeric(sn$parent %||% 0))
    parent_subs[[pid]] <- c(parent_subs[[pid]], list(sn$title %||% ""))
  }
  for (i in seq_along(g$nodes)) {
    n <- g$nodes[[i]]
    if (identical(n$group, "Skill")) {
      sid <- as.character(as.numeric(n$id))
      if (!is.null(parent_subs[[sid]])) {
        existing <- as.character(unlist(n$subs %||% list()))
        new_t    <- as.character(unlist(parent_subs[[sid]]))
        g$nodes[[i]]$subs <- as.list(c(existing, new_t[!new_t %in% existing]))
      }
    }
  }
  sub_ids <- vapply(sub_nodes, function(n) as.numeric(n$id), numeric(1))
  g$nodes <- Filter(function(n) !identical(n$group, "Subskill"), g$nodes)
  g$edges <- Filter(function(e) {
    !(as.numeric(e$from) %in% sub_ids || as.numeric(e$to) %in% sub_ids)
  }, g$edges)
  g$order[["Subskill"]] <- NULL
  g
}

# ── QMD extraction / writing ────────────────────────────────────────────────

extract_from_qmd <- function(in_path) {
  if (!file.exists(in_path)) return(list())
  lines <- readLines(in_path, warn = FALSE)
  
  anchor_re <- "^(#{1,6})\\s+.*\\{#([A-Za-z0-9_.-]+)(\\s+\\.draft)?\\}\\s*$"
  boundary_re <- "^#{1,2}\\s+"
  
  ids <- character(0); starts <- integer(0); is_draft <- logical(0)
  for (i in seq_along(lines)) {
    m <- regexec(anchor_re, lines[[i]], perl = TRUE)
    r <- regmatches(lines[[i]], m)[[1]]
    if (length(r) > 0) {
      ids <- c(ids, r[[3]])
      starts <- c(starts, i)
      is_draft <- c(is_draft, nzchar(r[[4]]))
    }
  }
  if (length(ids) == 0) return(list())
  
  all_heading_lines <- integer(0)
  for (i in seq_along(lines)) {
    if (grepl(boundary_re, lines[[i]])) all_heading_lines <- c(all_heading_lines, i)
  }
  
  extract_section <- function(idx) {
    sl <- starts[[idx]] + 1
    later <- all_heading_lines[all_heading_lines > starts[[idx]]]
    el <- if (length(later) > 0) later[1] - 1 else length(lines)
    if (sl > el) return("")
    sub("^\\s+", "", sub("\\s+$", "", paste(lines[sl:el], collapse = "\n")))
  }
  
  # Only return non-draft sections — drafts stay in .qmd untouched
  keep <- !is_draft
  if (!any(keep)) return(list())
  setNames(lapply(which(keep), extract_section), ids[keep])
}

# Read raw draft blocks from an existing QMD file (heading line + body)
read_draft_blocks <- function(in_path) {
  if (!file.exists(in_path)) return(character(0))
  lines <- readLines(in_path, warn = FALSE)
  anchor_re <- "^(#{1,6})\\s+.*\\{#([A-Za-z0-9_.-]+)(\\s+\\.draft)\\}\\s*$"
  boundary_re <- "^#{1,2}\\s+"
  
  draft_starts <- integer(0)
  for (i in seq_along(lines)) {
    m <- regexec(anchor_re, lines[[i]], perl = TRUE)
    r <- regmatches(lines[[i]], m)[[1]]
    if (length(r) > 0 && nzchar(r[[4]])) draft_starts <- c(draft_starts, i)
  }
  if (length(draft_starts) == 0) return(character(0))
  
  all_heading_lines <- integer(0)
  for (i in seq_along(lines)) {
    if (grepl(boundary_re, lines[[i]])) all_heading_lines <- c(all_heading_lines, i)
  }
  
  blocks <- character(0)
  for (ds in draft_starts) {
    later <- all_heading_lines[all_heading_lines > ds]
    el <- if (length(later) > 0) later[1] - 1 else length(lines)
    blocks <- c(blocks, paste(lines[ds:el], collapse = "\n"), "")
  }
  blocks
}

write_qmd_from_map <- function(out_path, desc_map, title = "Descriptions") {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  # Preserve draft blocks from the existing file before overwriting
  draft_blocks <- read_draft_blocks(out_path)
  keys <- names(desc_map)
  prefix_order <- function(k) {
    pre <- substr(k, 1, 1)
    num <- suppressWarnings(as.numeric(sub("^[tps]", "", k)))
    if (is.na(num)) num <- 9999L
    ord <- switch(pre, t = 1, p = 2, s = 3, 9)
    ord * 10000 + num
  }
  keys <- keys[order(sapply(keys, prefix_order))]
  header <- c("---", paste0('title: "', title, '"'), "---", "")
  body <- character(0)
  for (k in keys) {
    pre   <- substr(k, 1, 1)
    label <- switch(pre, t = "Theme", p = "Project", s = "Skill", "Item")
    body  <- c(body, sprintf("## %s %s {#%s}", label, sub("^[tps]", "", k), k),
               desc_map[[k]] %||% "", "")
  }
  # Append preserved draft blocks at the end
  if (length(draft_blocks) > 0)
    body <- c(body, "", "## Drafts", "", draft_blocks)
  writeLines(c(header, body), out_path, useBytes = TRUE)
}

write_json_map <- function(out_path, desc_map) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(toJSON(desc_map, auto_unbox = TRUE, pretty = TRUE), out_path, useBytes = TRUE)
}

# ── Layout engine ────────────────────────────────────────────────────────────

skill_node_height <- function(n, base_h = 46L) {
  nsubs <- length(n$subs %||% list())
  if (nsubs == 0) return(base_h)
  as.integer(max(base_h, 20 + 16 + nsubs * 14))
}

make_order_from_nodes <- function(nodes_list, existing_order = list()) {
  out <- list()
  for (grp in ALL_GROUPS) {
    existing_ids <- as.numeric(unlist(existing_order[[grp]]))
    all_ids <- vapply(nodes_list, function(n)
      if (identical(n$group, grp)) as.numeric(n$id) else NA_real_, numeric(1))
    all_ids <- all_ids[!is.na(all_ids)]
    kept    <- existing_ids[existing_ids %in% all_ids]
    new_ids <- sort(all_ids[!all_ids %in% kept])
    out[[grp]] <- as.list(c(kept, new_ids))
  }
  out
}

make_ordered_ids <- function(order_map, nodes_list, grp) {
  existing <- vapply(nodes_list, function(n)
    if (identical(n$group, grp)) as.numeric(n$id) else NA_real_, numeric(1))
  existing <- existing[!is.na(existing)]
  raw <- as.numeric(unlist(order_map[[grp]]))
  if (length(raw) == 0) return(sort(existing))
  ordered <- raw[raw %in% existing]
  extras  <- sort(existing[!existing %in% ordered])
  c(ordered, extras)
}

y_positions <- function(ids_groups, gap_v = 18) {
  y <- list(); cur_y <- 0
  for (i in seq_along(ids_groups)) {
    grp <- ids_groups[[i]]$group; id <- ids_groups[[i]]$id
    h   <- ids_groups[[i]]$h %||% (NODE_H[[grp]] %||% 46)
    if (i == 1) { cur_y <- h / 2
    } else {
      prev_h <- ids_groups[[i - 1]]$h %||% (NODE_H[[ids_groups[[i - 1]]$group]] %||% 46)
      cur_y <- cur_y + prev_h / 2 + gap_v + h / 2
    }
    y[[as.character(id)]] <- cur_y
  }
  y
}

stack_extent <- function(y_map, seq_list) {
  if (!length(seq_list)) return(c(0, 0))
  tops <- sapply(seq_list, function(s) {
    y <- y_map[[as.character(s$id)]]; if (is.null(y)) return(Inf)
    h <- s$h %||% (NODE_H[[s$group]] %||% 46); y - h / 2
  })
  bots <- sapply(seq_list, function(s) {
    y <- y_map[[as.character(s$id)]]; if (is.null(y)) return(-Inf)
    h <- s$h %||% (NODE_H[[s$group]] %||% 46); y + h / 2
  })
  c(min(tops[is.finite(tops)]), max(bots[is.finite(bots)]))
}

shift_y <- function(y_map, delta) lapply(y_map, function(v) v + delta)

build_cyto_data <- function(g, gap_v = 18, gap_col = 400,
                            font_node = 12, font_ptype = 12, font_subs = 15, font_desc = 11.5,
                            font_hdr1 = 22, font_hdr2 = 15,
                            h_theme = 46, h_project = 66, h_skill = 46,
                            w_project = NODE_W$Project,
                            watermark_text = "", watermark_size = 10,
                            col_bg = "#0b3552", col_sidebar_bg = "#081626", col_node_bg = "#081626",
                            col_theme = "#3be37a", col_project = "#ffad33", col_skill = "#78e6e7",
                            light_col_bg = "#f0f4f8", light_col_sidebar_bg = "#e2eaf3", light_col_node_bg = "#e2eaf3",
                            light_col_theme = "#1e7c45", light_col_project = "#c06000", light_col_skill = "#1a7a7b",
                            light_edge_color = "#555555",
                            hdr_theme_line1   = "Themes",   hdr_theme_line2   = "I want to focus on",
                            hdr_project_line1 = "Projects", hdr_project_line2 = "I\u2019m working on or want to work on",
                            hdr_skill_line1   = "Skills",   hdr_skill_line2   = "I have or want to develop",
                            fi_hdr_theme_line1   = "", fi_hdr_theme_line2   = "",
                            fi_hdr_project_line1 = "", fi_hdr_project_line2 = "",
                            fi_hdr_skill_line1   = "", fi_hdr_skill_line2   = "") {
  nodes_list <- g$nodes; edges_list <- g$edges; order_map <- g$order %||% list()
  nmap <- list(); for (n in nodes_list) nmap[[as.character(as.numeric(n$id))]] <- n
  col_x <- list(Theme = 0, Project = gap_col, Skill = gap_col * 2)
  
  # Use custom node heights
  node_h_map <- list(Theme = h_theme, Project = h_project)
  
  theme_ids <- make_ordered_ids(order_map, nodes_list, "Theme")
  proj_ids  <- make_ordered_ids(order_map, nodes_list, "Project")
  skill_ids <- make_ordered_ids(order_map, nodes_list, "Skill")
  theme_seq <- lapply(theme_ids, function(id) list(id = id, group = "Theme", h = h_theme))
  proj_seq  <- lapply(proj_ids,  function(id) list(id = id, group = "Project", h = h_project))
  skill_seq <- lapply(skill_ids, function(id) {
    nd <- nmap[[as.character(id)]]; h <- if (!is.null(nd)) skill_node_height(nd, h_skill) else h_skill
    list(id = id, group = "Skill", h = h)
  })
  
  theme_y_raw <- y_positions(theme_seq, gap_v)
  proj_y_raw  <- y_positions(proj_seq,  gap_v)
  skill_y_raw <- y_positions(skill_seq, gap_v)
  theme_ext <- stack_extent(theme_y_raw, theme_seq)
  proj_ext  <- stack_extent(proj_y_raw,  proj_seq)
  skill_ext <- stack_extent(skill_y_raw, skill_seq)
  max_h1 <- max(theme_ext[2]-theme_ext[1], proj_ext[2]-proj_ext[1], skill_ext[2]-skill_ext[1])
  
  theme_y <- shift_y(theme_y_raw, (max_h1-(theme_ext[2]-theme_ext[1]))/2 - theme_ext[1])
  proj_y  <- shift_y(proj_y_raw,  (max_h1-(proj_ext[2]-proj_ext[1]))/2  - proj_ext[1])
  skill_y <- shift_y(skill_y_raw, (max_h1-(skill_ext[2]-skill_ext[1]))/2 - skill_ext[1])
  
  header_margin_total <- HEADER_MARGIN + round((h_project + gap_v) / 2)
  proj_top  <- (max_h1 - (proj_ext[2]  - proj_ext[1]))  / 2
  theme_top <- (max_h1 - (theme_ext[2] - theme_ext[1])) / 2
  skill_top <- (max_h1 - (skill_ext[2] - skill_ext[1])) / 2
  gap_to_top <- proj_top + header_margin_total
  headers <- list(
    list(x=col_x[["Theme"]],   y=round(theme_top - gap_to_top), color=col_theme,   line1=hdr_theme_line1,   line1_fi=fi_hdr_theme_line1,   line2=hdr_theme_line2,   line2_fi=fi_hdr_theme_line2),
    list(x=col_x[["Project"]], y=round(proj_top  - gap_to_top), color=col_project, line1=hdr_project_line1, line1_fi=fi_hdr_project_line1, line2=hdr_project_line2, line2_fi=fi_hdr_project_line2),
    list(x=col_x[["Skill"]],   y=round(skill_top - gap_to_top), color=col_skill,   line1=hdr_skill_line1,   line1_fi=fi_hdr_skill_line1,   line2=hdr_skill_line2,   line2_fi=fi_hdr_skill_line2)
  )
  
  node_pos <- list()
  for (n in nodes_list) {
    id <- as.numeric(n$id); sid <- as.character(id); grp <- n$group %||% "Theme"
    if (!(grp %in% c("Theme","Project","Skill"))) next
    node_h <- if (grp=="Skill") skill_node_height(n, h_skill) else (node_h_map[[grp]] %||% 46)
    y_val <- switch(grp, Theme=theme_y[[sid]], Project=proj_y[[sid]], Skill=skill_y[[sid]], NULL) %||% 0
    node_pos[[sid]] <- list(x=col_x[[grp]]%||%0, y=y_val, w=(if(grp=="Project") w_project else NODE_W[[grp]])%||%200, h=node_h, group=grp)
  }
  
  cy_nodes <- list()
  for (n in nodes_list) {
    grp <- n$group %||% "Theme"; if (!(grp %in% c("Theme","Project","Skill"))) next
    sid <- as.character(as.numeric(n$id)); np <- node_pos[[sid]]; if (is.null(np)) next
    subs_str <- paste(as.character(unlist(n$subs %||% list())), collapse="||")
    cy_nodes <- c(cy_nodes, list(list(
      data=list(id=sid, label=n$title%||%"", label_fi=n$title_fi%||%"", group=np$group, ptype=n$ptype%||%"", w=np$w, h=np$h, subs=subs_str),
      position=list(x=np$x, y=np$y))))
  }
  
  vis_edges <- Filter(function(e) !isTRUE(e$hidden), edges_list)
  STAGGER <- 4
  is_cross_edge <- function(fr, to) {
    fnp <- node_pos[[fr]]; tnp <- node_pos[[to]]
    if (is.null(fnp)||is.null(tnp)) return(FALSE)
    (fnp$group=="Theme"&&tnp$group=="Project")||(fnp$group=="Project"&&tnp$group=="Skill")
  }
  ekey <- function(e) paste0(as.character(as.numeric(e$from)),"_",as.character(as.numeric(e$to)))
  # Pre-compute stagger offsets sorted by opposite-end y-position to avoid crossings
  cross_edges <- Filter(function(e) {
    fr <- as.character(as.numeric(e$from)); to <- as.character(as.numeric(e$to))
    is_cross_edge(fr, to)
  }, vis_edges)
  src_dy_map <- list(); tgt_dy_map <- list()
  # Source stagger: group by source, sort outgoing edges by target y
  src_grp <- list()
  for (e in cross_edges) { fr <- as.character(as.numeric(e$from)); src_grp[[fr]] <- c(src_grp[[fr]], list(e)) }
  for (fr in names(src_grp)) {
    grp <- src_grp[[fr]]; n <- length(grp)
    tgt_ys <- sapply(grp, function(e) { to <- as.character(as.numeric(e$to)); node_pos[[to]]$y %||% 0 })
    ord <- order(tgt_ys)
    for (i in seq_along(ord)) src_dy_map[[ekey(grp[[ord[i]]])]] <- round((i-(n+1)/2)*STAGGER)
  }
  # Target stagger: group by target, sort incoming edges by source y
  tgt_grp <- list()
  for (e in cross_edges) { to <- as.character(as.numeric(e$to)); tgt_grp[[to]] <- c(tgt_grp[[to]], list(e)) }
  for (to in names(tgt_grp)) {
    grp <- tgt_grp[[to]]; n <- length(grp)
    src_ys <- sapply(grp, function(e) { fr <- as.character(as.numeric(e$from)); node_pos[[fr]]$y %||% 0 })
    ord <- order(src_ys)
    for (i in seq_along(ord)) tgt_dy_map[[ekey(grp[[ord[i]]])]] <- round((i-(n+1)/2)*STAGGER)
  }
  cy_edges <- list()
  for (e in vis_edges) {
    fr <- as.character(as.numeric(e$from)); to <- as.character(as.numeric(e$to))
    fnp <- node_pos[[fr]]; tnp <- node_pos[[to]]; if (is.null(fnp)||is.null(tnp)) next
    src_node <- nmap[[fr]]; tgt_node <- nmap[[to]]
    if (!is.null(src_node) && identical(src_node$group,"Theme")) {
      edge_color      <- src_node$edgeColor      %||% e$color %||% "#ffffff"
      light_edge_col  <- src_node$lightEdgeColor %||% light_edge_color
    } else if (!is.null(tgt_node) && identical(tgt_node$group,"Skill")) {
      edge_color      <- tgt_node$edgeColor      %||% e$color %||% "#ffffff"
      light_edge_col  <- tgt_node$lightEdgeColor %||% light_edge_color
    } else {
      edge_color     <- e$color %||% "#ffffff"
      light_edge_col <- light_edge_color
    }
    ek <- ekey(e)
    if (!is.null(src_dy_map[[ek]])) {
      src_ep <- paste0(round(fnp$w/2),"px ",src_dy_map[[ek]],"px")
      tgt_ep <- paste0(-round(tnp$w/2),"px ",(tgt_dy_map[[ek]]%||%0),"px")
    } else { src_ep <- "outside-to-node"; tgt_ep <- "outside-to-node" }
    cy_edges <- c(cy_edges, list(list(data=list(
      id=paste0("e",fr,"_",to), source=fr, target=to,
      color=edge_color, lightColor=light_edge_col, dashes=isTRUE(e$dashes), srcEp=src_ep, tgtEp=tgt_ep))))
  }
  list(nodes=cy_nodes, edges=cy_edges, headers=headers, max_h1=max_h1,
       headerMargin=header_margin_total,
       fontNode=font_node, fontPtype=font_ptype, fontSubs=font_subs, fontDesc=font_desc,
       fontHdr1=font_hdr1, fontHdr2=font_hdr2,
       watermarkText=watermark_text, watermarkSize=watermark_size,
       colBg=col_bg, colSidebarBg=col_sidebar_bg, colNodeBg=col_node_bg,
       colTheme=col_theme, colProject=col_project, colSkill=col_skill,
       lightColBg=light_col_bg, lightColSidebarBg=light_col_sidebar_bg, lightColNodeBg=light_col_node_bg,
       lightColTheme=light_col_theme, lightColProject=light_col_project, lightColSkill=light_col_skill,
       lightEdgeColor=light_edge_color)
}

# ── Dual layout builder (desktop + mobile) ───────────────────────────────────

build_dual_cyto_data <- function(g, gap_v = 18, gap_col = 400,
                                 font_node = 12, font_ptype = 12, font_subs = 15, font_desc = 11.5,
                                 font_hdr1 = 22, font_hdr2 = 15,
                                 h_theme = 46, h_project = 66, h_skill = 46,
                                 w_project = NODE_W$Project,
                                 watermark_text = "", watermark_size = 10,
                                 col_bg = "#0b3552", col_sidebar_bg = "#081626", col_node_bg = "#081626",
                                 col_theme = "#3be37a", col_project = "#ffad33", col_skill = "#78e6e7",
                                 light_col_bg = "#f0f4f8", light_col_sidebar_bg = "#e2eaf3", light_col_node_bg = "#e2eaf3",
                                 light_col_theme = "#1e7c45", light_col_project = "#c06000", light_col_skill = "#1a7a7b",
                                 light_edge_color = "#555555",
                                 mob_font_mult = 1.5, mob_h_theme_mult = 3.0,
                                 mob_h_proj_mult = 3.0, mob_h_skill_mult = 3.0,
                                 mob_gap_v_mult = 1.0, mob_gap_col_mult = 1.0,
                                 hdr_theme_line1   = "Themes",   hdr_theme_line2   = "I want to focus on",
                                 hdr_project_line1 = "Projects", hdr_project_line2 = "I\u2019m working on or want to work on",
                                 hdr_skill_line1   = "Skills",   hdr_skill_line2   = "I have or want to develop",
                                 fi_hdr_theme_line1   = "", fi_hdr_theme_line2   = "",
                                 fi_hdr_project_line1 = "", fi_hdr_project_line2 = "",
                                 fi_hdr_skill_line1   = "", fi_hdr_skill_line2   = "") {
  hdr_args <- list(hdr_theme_line1=hdr_theme_line1, hdr_theme_line2=hdr_theme_line2,
                   hdr_project_line1=hdr_project_line1, hdr_project_line2=hdr_project_line2,
                   hdr_skill_line1=hdr_skill_line1, hdr_skill_line2=hdr_skill_line2,
                   fi_hdr_theme_line1=fi_hdr_theme_line1, fi_hdr_theme_line2=fi_hdr_theme_line2,
                   fi_hdr_project_line1=fi_hdr_project_line1, fi_hdr_project_line2=fi_hdr_project_line2,
                   fi_hdr_skill_line1=fi_hdr_skill_line1, fi_hdr_skill_line2=fi_hdr_skill_line2)
  # Desktop build (unchanged)
  desktop <- do.call(build_cyto_data, c(list(g=g, gap_v=gap_v, gap_col=gap_col,
                             font_node=font_node, font_ptype=font_ptype,
                             font_subs=font_subs, font_desc=font_desc,
                             font_hdr1=font_hdr1, font_hdr2=font_hdr2,
                             h_theme=h_theme, h_project=h_project, h_skill=h_skill,
                             w_project=w_project,
                             watermark_text=watermark_text, watermark_size=watermark_size,
                             col_bg=col_bg, col_sidebar_bg=col_sidebar_bg, col_node_bg=col_node_bg,
                             col_theme=col_theme, col_project=col_project, col_skill=col_skill,
                             light_col_bg=light_col_bg, light_col_sidebar_bg=light_col_sidebar_bg, light_col_node_bg=light_col_node_bg,
                             light_col_theme=light_col_theme, light_col_project=light_col_project, light_col_skill=light_col_skill,
                             light_edge_color=light_edge_color), hdr_args))
  # Mobile build (multiplied fonts, heights, gaps)
  mobile <- do.call(build_cyto_data, c(list(g=g, gap_v=gap_v*mob_gap_v_mult,
                            gap_col=gap_col*mob_gap_col_mult,
                            font_node =round(font_node *mob_font_mult,1),
                            font_ptype=round(font_ptype*mob_font_mult,1),
                            font_subs =round(font_subs *mob_font_mult,1),
                            font_desc =round(font_desc *mob_font_mult,1),
                            font_hdr1 =round(font_hdr1 *mob_font_mult,1),
                            font_hdr2 =round(font_hdr2 *mob_font_mult,1),
                            h_theme  =round(h_theme  *mob_h_theme_mult),
                            h_project=round(h_project*mob_h_proj_mult),
                            h_skill  =round(h_skill  *mob_h_skill_mult),
                            w_project=w_project,
                            watermark_text=watermark_text, watermark_size=watermark_size,
                            col_bg=col_bg, col_sidebar_bg=col_sidebar_bg, col_node_bg=col_node_bg,
                            col_theme=col_theme, col_project=col_project, col_skill=col_skill,
                            light_col_bg=light_col_bg, light_col_sidebar_bg=light_col_sidebar_bg, light_col_node_bg=light_col_node_bg,
                            light_col_theme=light_col_theme, light_col_project=light_col_project, light_col_skill=light_col_skill,
                            light_edge_color=light_edge_color), hdr_args))
  # Attach mobile as nested field (backward-compatible: top-level = desktop)
  desktop$mobile <- mobile
  desktop
}

# ── SVG generation ───────────────────────────────────────────────────────────

svg_esc <- function(x) {
  x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE)
  x <- gsub(">","&gt;",x,fixed=TRUE); x <- gsub("\"","&quot;",x,fixed=TRUE); x
}

svg_wrap_text <- function(text, max_chars) {
  words <- strsplit(text," ")[[1]]; lines <- character(0); cur <- ""
  for (w in words) {
    cand <- if (nchar(cur)==0) w else paste(cur,w)
    if (nchar(cand) <= max_chars) cur <- cand
    else { if (nchar(cur)>0) lines <- c(lines,cur); cur <- w }
  }
  if (nchar(cur)>0) lines <- c(lines,cur); if (!length(lines)) lines <- ""; lines
}

parse_ep <- function(ep, cx, cy) {
  if (is.null(ep)||grepl("outside",ep)) return(c(cx,cy))
  p <- strsplit(trimws(ep),"\\s+")[[1]]
  c(cx+as.numeric(sub("px","",p[1],fixed=TRUE)), cy+as.numeric(sub("px","",p[2],fixed=TRUE)))
}

generate_svg <- function(cd) {
  nodes <- cd$nodes; edges <- cd$edges; headers <- cd$headers; pad <- 40
  fsize_node <- cd$fontNode %||% 12; fsize_ptype <- cd$fontPtype %||% 12; fsize_subs <- cd$fontSubs %||% 15
  fsize_hdr1 <- cd$fontHdr1 %||% 22; fsize_hdr2 <- cd$fontHdr2 %||% 15
  wm_text <- cd$watermarkText %||% ""; wm_size <- cd$watermarkSize %||% 10
  svg_bg <- cd$colBg %||% "#0b3552"
  svg_col_theme <- cd$colTheme %||% "#3be37a"
  svg_col_project <- cd$colProject %||% "#ffad33"
  svg_col_skill <- cd$colSkill %||% "#78e6e7"
  xs <- sapply(nodes, function(n) n$position$x); ys <- sapply(nodes, function(n) n$position$y)
  ws <- sapply(nodes, function(n) n$data$w); hs <- sapply(nodes, function(n) n$data$h)
  hdr_ys <- sapply(headers, function(h) h$y)
  mn_x <- min(xs-ws/2)-pad; mx_x <- max(xs+ws/2)+pad
  mn_y <- min(c(ys-hs/2, hdr_ys))-6; mx_y <- max(ys+hs/2)+pad
  cw <- mx_x-mn_x; ch <- mx_y-mn_y
  if (cw > ch) ch <- cw else cw <- ch
  vx <- mn_x-(cw-(mx_x-mn_x))/2; vy <- mn_y-(ch-(mx_y-mn_y))/2
  nmap <- setNames(nodes, sapply(nodes, function(n) n$data$id))
  out <- c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="%g %g %g %g" width="1080" height="1080">',vx,vy,cw,ch),
    sprintf('<rect x="%g" y="%g" width="%g" height="%g" fill="%s"/>',vx,vy,cw,ch,svg_bg))
  for (h in headers) {
    out <- c(out,
             sprintf('<text x="%g" y="%g" fill="%s" font-family="Arial,Helvetica,sans-serif" font-size="%d" font-weight="bold" text-anchor="middle">%s</text>',
                     h$x, h$y+18, h$color, fsize_hdr1, svg_esc(h$line1)),
             sprintf('<text x="%g" y="%g" fill="%s" font-family="Arial,Helvetica,sans-serif" font-size="%d" text-anchor="middle" opacity="0.55">%s</text>',
                     h$x, h$y+34, h$color, fsize_hdr2, svg_esc(h$line2)))
  }
  for (e in edges) {
    fn <- nmap[[e$data$source]]; tn <- nmap[[e$data$target]]; if (is.null(fn)||is.null(tn)) next
    sp <- parse_ep(e$data$srcEp, fn$position$x, fn$position$y)
    tp <- parse_ep(e$data$tgtEp, tn$position$x, tn$position$y)
    dx <- tp[1]-sp[1]; cx1 <- sp[1]+dx*0.45; cx2 <- tp[1]-dx*0.45
    col <- svg_esc(e$data$color%||%"#fff"); dash <- if (isTRUE(e$data$dashes)) ' stroke-dasharray="5,3"' else ""
    out <- c(out, sprintf(
      '<path d="M%g,%g C%g,%g %g,%g %g,%g" fill="none" stroke="%s" stroke-width="1.8" opacity="0.75"%s/>',
      sp[1],sp[2],cx1,sp[2],cx2,tp[2],tp[1],tp[2],col,dash))
  }
  svg_sidebar_bg <- cd$colSidebarBg %||% "#081626"
  
  grp_stroke <- list(Theme=svg_col_theme,Project=svg_col_project,Skill=svg_col_skill)
  for (n in nodes) {
    x <- n$position$x; y <- n$position$y; w <- n$data$w; h <- n$data$h; g <- n$data$group
    tcol <- grp_stroke[[g]]%||%svg_col_skill; fill <- svg_sidebar_bg; stroke <- grp_stroke[[g]]%||%svg_col_skill
    out <- c(out, sprintf('<rect x="%g" y="%g" width="%g" height="%g" fill="%s" stroke="%s" stroke-width="1.1" rx="2"/>',
                          x-w/2,y-h/2,w,h,fill,stroke))
    label <- n$data$label%||%""
    if (g=="Project") {
      lines <- svg_wrap_text(label,38)
      # Center project text vertically in SVG
      base_y <- y - (length(lines)-1)*8.5 + 4
      for (li in seq_along(lines))
        out <- c(out, sprintf('<text x="%g" y="%g" fill="%s" font-family="Arial,Helvetica,sans-serif" font-size="%d" font-weight="bold" text-anchor="middle">%s</text>',
                              x,base_y+(li-1)*17,tcol,fsize_node,svg_esc(lines[li])))
    } else if (g=="Theme") {
      lines <- svg_wrap_text(label,18); base_y <- y-(length(lines)-1)*8+4
      for (li in seq_along(lines))
        out <- c(out, sprintf('<text x="%g" y="%g" fill="%s" font-family="Arial,Helvetica,sans-serif" font-size="%d" font-weight="bold" text-anchor="end">%s</text>',
                              x+w/2-7,base_y+(li-1)*17,tcol,fsize_node,svg_esc(lines[li])))
    } else if (g=="Skill") {
      # Vertically center title + subs block
      lines <- svg_wrap_text(label, 26)
      subs_str <- n$data$subs%||%""
      sub_items <- if (nzchar(subs_str)) strsplit(subs_str,"||",fixed=TRUE)[[1]] else character(0)
      total_h <- length(lines)*16 + length(sub_items)*18
      block_top <- y - total_h/2 + 8
      for (li in seq_along(lines))
        out <- c(out, sprintf('<text x="%g" y="%g" fill="%s" font-family="Arial,Helvetica,sans-serif" font-size="%d" font-weight="bold">%s</text>',
                              x-w/2+8, block_top+(li-1)*16, tcol, fsize_node, svg_esc(lines[li])))
      if (length(sub_items) > 0) {
        subs_top <- block_top + length(lines)*16
        for (si in seq_along(sub_items))
          out <- c(out, sprintf('<text x="%g" y="%g" fill="%s" font-family="Arial,Helvetica,sans-serif" font-size="%d" opacity="0.7">%s</text>',
                                x-w/2+18, subs_top+(si-1)*18, tcol, fsize_subs, svg_esc(sub_items[si])))
      }
    }
  }
  
  # Watermark text bottom-left
  if (nzchar(wm_text)) {
    out <- c(out, sprintf(
      '<text x="%g" y="%g" fill="rgba(255,255,255,0.8)" font-family="Arial,Helvetica,sans-serif" font-size="%d">%s</text>',
      vx + 15, vy + ch - 15, wm_size, svg_esc(wm_text)))
  }
  
  paste(c(out,"</svg>"),collapse="\n")
}