# ==========================================
# mod_report_io.R
# ==========================================

reportIOUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          class = "btn-group me-2",
          role = "group",
          downloadButton(ns("btn_export_csv"), "Export CSV", class = "btn-outline-secondary btn-sm"),
          downloadButton(ns("btn_export_excel"), "Export Excel", class = "btn-outline-secondary btn-sm")
        ),
        div(
          class = "btn-group",
          role = "group",
          fileInput(
            ns("file_import_csv"), 
            label = NULL, 
            buttonLabel = "Import CSV", 
            accept = c(".csv"),
            width = "auto"
          )
        )
      )
    )
  )
}

reportIOServer <- function(id, rv, get_db, trigger_refresh, iterations_input, seed_input, use_seed_input) {
  moduleServer(id, function(input, output, session) {
    
    # ----------------------------------------------------
    # Helper: Robust CSV Reader
    # ----------------------------------------------------
    read_csv_robust <- function(filepath) {
      lines <- readLines(filepath, n = 5, warn = FALSE)
      if (length(lines) == 0) return(data.frame())
      
      # Clean UTF-8 Byte Order Mark (BOM) without mixing \u and \x escapes
      bom_char <- intToUtf8(0xFEFF)
      first_line <- sub(paste0("^", bom_char), "", lines[1])
      
      n_comma <- length(gregexpr(",", first_line)[[1]])
      if (n_comma == 1 && gregexpr(",", first_line)[[1]][1] == -1) n_comma <- 0
      
      n_semi <- length(gregexpr(";", first_line)[[1]])
      if (n_semi == 1 && gregexpr(";", first_line)[[1]][1] == -1) n_semi <- 0
      
      sep <- if (n_semi > n_comma) ";" else ","
      
      df <- tryCatch({
        read.csv(filepath, sep = sep, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
      }, error = function(e) {
        read.csv(filepath, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)
      })
      
      return(df)
    }

    # ----------------------------------------------------
    # Helper: Convert WBS Level Format (L1..L8) to Relational Nodes
    # ----------------------------------------------------
    convert_wbs_report_to_nodes <- function(df) {
      clean_names <- trimws(tolower(names(df)))
      clean_names <- gsub("[^a-z0-9_]", "_", clean_names)
      names(df) <- clean_names
      
      lvl_cols <- grep("^l[0-9]+$", names(df), value = TRUE)
      if (length(lvl_cols) == 0) return(df) # Not a WBS report format
      
      lvl_cols <- lvl_cols[order(as.numeric(sub("^l", "", lvl_cols)))]
      
      stack <- list()
      records <- list()
      
      col_type    <- intersect(c("element_type", "type", "element"), names(df))[1]
      col_active  <- intersect(c("active", "is_active"), names(df))[1]
      col_opt     <- intersect(c("leaf_opt", "opt_val", "opt", "optimistic"), names(df))[1]
      col_likely  <- intersect(c("leaf_likely", "likely_val", "likely", "most_likely"), names(df))[1]
      col_pess    <- intersect(c("leaf_pess", "pess_val", "pess", "pessimistic"), names(df))[1]
      col_prob    <- intersect(c("leaf_prob", "chance", "probability"), names(df))[1]
      col_updated <- intersect(c("updated", "updated_at"), names(df))[1]

      for (i in seq_len(nrow(df))) {
        row <- df[i, , drop = FALSE]
        
        row_lvl <- NULL
        node_title <- NULL
        
        for (col in lvl_cols) {
          val <- row[[col]]
          if (!is.na(val) && trimws(as.character(val)) != "") {
            row_lvl <- as.numeric(sub("^l", "", col))
            node_title <- trimws(as.character(val))
            break
          }
        }
        
        if (is.null(node_title)) next
        
        node_id <- sprintf("node_%d", i)
        parent_id <- if (row_lvl > 1 && !is.null(stack[[as.character(row_lvl - 1)]])) {
          stack[[as.character(row_lvl - 1)]]
        } else {
          NA_character_
        }
        
        stack[[as.character(row_lvl)]] <- node_id
        
        # Clear deeper stack levels
        deep_keys <- names(stack)[as.numeric(names(stack)) > row_lvl]
        for (k in deep_keys) stack[[k]] <- NULL
        
        elem_type <- if (!is.null(col_type) && !is.na(row[[col_type]])) trimws(as.character(row[[col_type]])) else "Cost"
        
        act_raw <- if (!is.null(col_active) && !is.na(row[[col_active]])) row[[col_active]] else "Yes"
        is_act <- if (tolower(trimws(as.character(act_raw))) %in% c("yes", "true", "1")) 1 else 0
        
        opt_val <- if (!is.null(col_opt) && !is.na(row[[col_opt]])) as.numeric(row[[col_opt]]) else NA_real_
        likely_val <- if (!is.null(col_likely) && !is.na(row[[col_likely]])) as.numeric(row[[col_likely]]) else NA_real_
        pess_val <- if (!is.null(col_pess) && !is.na(row[[col_pess]])) as.numeric(row[[col_pess]]) else NA_real_
        chance_val <- if (!is.null(col_prob) && !is.na(row[[col_prob]])) as.numeric(row[[col_prob]]) else 100
        
        is_leaf_node <- if (!is.na(likely_val)) 1 else 0
        updated_str <- if (!is.null(col_updated) && !is.na(row[[col_updated]])) as.character(row[[col_updated]]) else format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        records[[length(records) + 1]] <- data.frame(
          id = node_id,
          parent_id = parent_id,
          element_type = elem_type,
          title = node_title,
          opt_val = opt_val,
          likely_val = likely_val,
          pess_val = pess_val,
          chance = chance_val,
          is_leaf = is_leaf_node,
          is_active = is_act,
          owner = NA_character_,
          updated_at = updated_str,
          stringsAsFactors = FALSE
        )
      }
      
      do.call(rbind, records)
    }

    # ----------------------------------------------------
    # Helper: Map Standard Headers
    # ----------------------------------------------------
    normalize_headers <- function(df) {
      clean_names <- trimws(tolower(names(df)))
      clean_names <- gsub("[^a-z0-9_]", "_", clean_names)
      clean_names <- gsub("_+", "_", clean_names)
      clean_names <- gsub("^_|_$", "", clean_names)
      
      alias_map <- c(
        "wbs_id"            = "id",
        "node_id"           = "id",
        "parent"            = "parent_id",
        "parent_wbs_id"     = "parent_id",
        "type"              = "element_type",
        "type_element"      = "element_type",
        "element"           = "element_type",
        "name"              = "title",
        "node_title"        = "title",
        "opt"               = "opt_val",
        "optimistic"        = "opt_val",
        "leaf_opt"          = "opt_val",
        "likely"            = "likely_val",
        "most_likely"       = "likely_val",
        "leaf_likely"       = "likely_val",
        "pess"              = "pess_val",
        "pessimistic"       = "pess_val",
        "leaf_pess"         = "pess_val",
        "probability"       = "chance",
        "leaf_prob"         = "chance",
        "leaf"              = "is_leaf",
        "active"            = "is_active",
        "updated"           = "updated_at"
      )
      
      mapped_names <- sapply(clean_names, function(col) {
        if (col %in% names(alias_map)) alias_map[[col]] else col
      }, USE.NAMES = FALSE)
      
      names(df) <- mapped_names
      return(df)
    }

    # ----------------------------------------------------
    # Export Handlers
    # ----------------------------------------------------
    output$btn_export_csv <- downloadHandler(
      filename = function() {
        proj_name <- if (!is.null(rv$db_path)) gsub("\\.sqlite$|\\.db$", "", basename(rv$db_path)) else "project"
        paste0(proj_name, "_export_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(rv$db_path)
        con <- get_db()
        df <- dbGetQuery(con, "SELECT id, parent_id, element_type, title, opt_val, likely_val, pess_val, chance, is_leaf, is_active, owner, updated_at FROM financial_elements")
        dbDisconnect(con)
        
        write.csv(df, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      }
    )

    output$btn_export_excel <- downloadHandler(
      filename = function() {
        proj_name <- if (!is.null(rv$db_path)) gsub("\\.sqlite$|\\.db$", "", basename(rv$db_path)) else "project"
        paste0(proj_name, "_report_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        req(rv$db_path)
        con <- get_db()
        elements_df <- dbGetQuery(con, "SELECT id, parent_id, element_type, title, opt_val, likely_val, pess_val, chance, is_leaf, is_active, owner, updated_at FROM financial_elements")
        dbDisconnect(con)
        
        wb <- openxlsx::createWorkbook()
        
        openxlsx::addWorksheet(wb, "Financial Elements")
        openxlsx::writeData(wb, "Financial Elements", elements_df)
        
        if (!is.null(rv$mc_results)) {
          res <- rv$mc_results
          summary_df <- data.frame(
            Metric = c("Mean Exposure", "P10 (Favorable)", "P50 (Median)", "P90 (Unfavorable)"),
            Value = c(
              mean(res),
              quantile(res, 0.10, names = FALSE),
              quantile(res, 0.50, names = FALSE),
              quantile(res, 0.90, names = FALSE)
            )
          )
          openxlsx::addWorksheet(wb, "Simulation Summary")
          openxlsx::writeData(wb, "Simulation Summary", summary_df)
        }
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    # ----------------------------------------------------
    # CSV Import Observer
    # ----------------------------------------------------
    observeEvent(input$file_import_csv, {
      req(input$file_import_csv, rv$db_path)
      
      tryCatch({
        raw_df <- read_csv_robust(input$file_import_csv$datapath)
        
        if (nrow(raw_df) == 0) {
          showNotification("Import failed: The selected CSV file is empty.", type = "error")
          return()
        }
        
        # Check if file is WBS report format and convert to relational nodes
        if (any(grepl("^L[0-9]+$", names(raw_df), ignore.case = TRUE))) {
          df <- convert_wbs_report_to_nodes(raw_df)
        } else {
          df <- normalize_headers(raw_df)
        }
        
        required_cols <- c("id", "element_type", "title")
        missing_cols <- setdiff(required_cols, names(df))
        
        if (length(missing_cols) > 0) {
          showNotification(
            paste("Import failed: CSV missing required columns:", paste(missing_cols, collapse = ", ")),
            type = "error",
            duration = 10
          )
          return()
        }
        
        # Defaults for optional fields
        if (!"parent_id" %in% names(df)) df$parent_id <- NA
        if (!"opt_val" %in% names(df)) df$opt_val <- NA
        if (!"likely_val" %in% names(df)) df$likely_val <- NA
        if (!"pess_val" %in% names(df)) df$pess_val <- NA
        if (!"chance" %in% names(df)) df$chance <- 100
        if (!"is_leaf" %in% names(df)) df$is_leaf <- 0
        if (!"is_active" %in% names(df)) df$is_active <- 1
        if (!"owner" %in% names(df)) df$owner <- NA
        if (!"updated_at" %in% names(df)) df$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        # Sanitize data types
        df$id <- as.character(df$id)
        df$parent_id <- ifelse(is.na(df$parent_id) | df$parent_id == "", NA_character_, as.character(df$parent_id))
        df$element_type <- as.character(df$element_type)
        df$title <- as.character(df$title)
        df$opt_val <- suppressWarnings(as.numeric(df$opt_val))
        df$likely_val <- suppressWarnings(as.numeric(df$likely_val))
        df$pess_val <- suppressWarnings(as.numeric(df$pess_val))
        df$chance <- ifelse(is.na(suppressWarnings(as.numeric(df$chance))), 100, suppressWarnings(as.numeric(df$chance)))
        df$is_leaf <- ifelse(is.na(suppressWarnings(as.integer(df$is_leaf))), 0, suppressWarnings(as.integer(df$is_leaf)))
        df$is_active <- ifelse(is.na(suppressWarnings(as.integer(df$is_active))), 1, suppressWarnings(as.integer(df$is_active)))
        df$owner <- as.character(df$owner)
        df$updated_at <- as.character(df$updated_at)
        
        # Database replace transaction
        con <- get_db()
        dbBegin(con)
        
        dbExecute(con, "DELETE FROM financial_elements")
        
        insert_query <- "
          INSERT INTO financial_elements (
            id, parent_id, element_type, title, opt_val, likely_val, 
            pess_val, chance, is_leaf, is_active, owner, updated_at
          ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        "
        
        for (i in seq_len(nrow(df))) {
          dbExecute(con, insert_query, params = list(
            df$id[i],
            df$parent_id[i],
            df$element_type[i],
            df$title[i],
            df$opt_val[i],
            df$likely_val[i],
            df$pess_val[i],
            df$chance[i],
            df$is_leaf[i],
            df$is_active[i],
            df$owner[i],
            df$updated_at[i]
          ))
        }
        
        dbCommit(con)
        dbDisconnect(con)
        
        trigger_refresh(trigger_refresh() + 1)
        showNotification("Database successfully restored from CSV.", type = "message")
        
      }, error = function(e) {
        if (exists("con") && dbIsValid(con)) {
          try(dbRollback(con), silent = TRUE)
          dbDisconnect(con)
        }
        showNotification(paste("Import failed:", e$message), type = "error", duration = 10)
      })
    })
  })
}