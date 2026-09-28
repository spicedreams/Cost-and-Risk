# ==========================================
# impexp.R
# ==========================================

import_csv_to_db <- function(csv_file_path, db_file_path) {
  if (!file.exists(csv_file_path)) stop("CSV file does not exist.")
  
  csv_df <- read.csv(csv_file_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(csv_df) == 0) stop("CSV file is empty.")
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE;")
  DBI::dbExecute(con, "PRAGMA busy_timeout=5000;")
  
  DBI::dbExecute(con, "DROP TABLE IF EXISTS financial_elements;")
  DBI::dbExecute(con, "
    CREATE TABLE financial_elements (
      id TEXT PRIMARY KEY, 
      parent_id TEXT, 
      element_type TEXT NOT NULL,
      title TEXT NOT NULL, 
      opt_val REAL, 
      likely_val REAL, 
      pess_val REAL,
      chance REAL DEFAULT 100, 
      is_leaf INTEGER DEFAULT 0,
      is_active INTEGER DEFAULT 1, 
      owner TEXT, 
      updated_at TEXT
    )")
  
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parent ON financial_elements(parent_id);")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_id ON financial_elements(id);")
  
  DBI::dbBegin(con)
  
  parent_stack <- vector("character", 8)
  
  for (i in seq_len(nrow(csv_df))) {
    rec_id <- as.character(i)
    
    elem_type <- trimws(as.character(csv_df[i, 1]))
    if (is.na(elem_type) || elem_type == "") elem_type <- "Cost"
    
    active_str <- tolower(trimws(as.character(csv_df[i, 2])))
    is_active_val <- if (active_str %in% c("yes", "true", "1")) 1 else 0
    
    # Check title nesting across L1..L8 (columns 3 to 10)
    title_val <- ""
    depth <- 0
    for (col_idx in 3:10) {
      val <- csv_df[i, col_idx]
      if (!is.na(val) && trimws(as.character(val)) != "") {
        title_val <- trimws(as.character(val))
        depth <- col_idx - 2
        break
      }
    }
    
    if (depth == 0 || title_val == "") next
    
    parent_id_val <- if (depth > 1) parent_stack[depth - 1] else NA_character_
    if (is.na(parent_id_val) || parent_id_val == "") parent_id_val <- NA_character_
    
    parent_stack[depth] <- rec_id
    if (depth < 8) {
      parent_stack[(depth + 1):8] <- NA_character_
    }
    
    parse_num <- function(x) {
      if (is.na(x) || trimws(as.character(x)) == "") return(NA_real_)
      suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
    }
    
    opt_val <- parse_num(csv_df[i, 11])
    likely_val <- parse_num(csv_df[i, 12])
    pess_val <- parse_num(csv_df[i, 13])
    chance_val <- parse_num(csv_df[i, 14])
    
    is_leaf_val <- if (!is.na(opt_val) || !is.na(likely_val) || !is.na(pess_val) || !is.na(chance_val)) 1 else 0
    if (is.na(chance_val)) chance_val <- 100
    
    updated_at_val <- if (ncol(csv_df) >= 18) trimws(as.character(csv_df[i, 18])) else ""
    if (is.na(updated_at_val)) updated_at_val <- ""
    
    DBI::dbExecute(con, 
      "INSERT INTO financial_elements (id, parent_id, element_type, title, opt_val, likely_val, pess_val, chance, is_leaf, is_active, updated_at) 
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        rec_id,
        parent_id_val,
        elem_type,
        title_val,
        opt_val,
        likely_val,
        pess_val,
        chance_val,
        is_leaf_val,
        is_active_val,
        updated_at_val
      )
    )
  }
  
  DBI::dbCommit(con)
  return(TRUE)
}