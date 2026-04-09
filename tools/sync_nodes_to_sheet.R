library(jsonlite)
library(googlesheets4)

SHEET_ID <- "1LDCpAadX-mAdTs8e--XERkaa-i0Wmt6v4O-2RTGekVs"  # replace with your actual Sheet ID

graph <- fromJSON("app_publish/www/graph.json")
nodes_df <- graph$nodes[order(as.numeric(graph$nodes$id)), ]
titles <- nodes_df$title

gs4_auth()

ss <- gs4_get(SHEET_ID)

# Create sheet if it doesn't exist yet
tryCatch(sheet_add(ss, sheet = "nodes"), error = function(e) NULL)

# Clear then write — avoids column-resize bug in sheet_write()
range_clear(ss, sheet = "nodes", reformat = FALSE)
range_write(ss, data = data.frame(title = titles), sheet = "nodes",
            col_names = TRUE, reformat = FALSE)

cat("Written", length(titles), "node titles to sheet 'nodes'\n")
