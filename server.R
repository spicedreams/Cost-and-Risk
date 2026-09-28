# ==========================================
# server.R
# ==========================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    mc_results = NULL, 
    db_path = NULL,
    network_db_path = NULL,
    lock_dir = NULL, 
    current_leaf_state = FALSE, 
    focus_node_id = NULL,
    force_open_all = TRUE 
  )

  # Robust lock cleanup on session termination
  session$onSessionEnded(function() {
    isolate({
      if (!is.null(rv$db_path) && !is.null(rv$network_db_path)) {
        try(sync_to_network(rv), silent = TRUE)
        try(release_db_lock(rv), silent = TRUE)
        if (!is.null(rv$lock_dir) && dir.exists(rv$lock_dir)) {
          unlink(rv$lock_dir, recursive = TRUE, force = TRUE)
        }
      }
    })
  })

  get_db <- function() { 
    if (is.null(rv$db_path) || rv$db_path == "") stop("No active database specified.")
    con <- dbConnect(RSQLite::SQLite(), rv$db_path)
    dbExecute(con, "PRAGMA journal_mode=DELETE;")
    dbExecute(con, "PRAGMA busy_timeout=5000;")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parent ON financial_elements(parent_id);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_id ON financial_elements(id);")
    con
  }
  
  # Type constraint rule enforcement
  get_allowed_child_types <- function(parent_type) {
    if (parent_type == "Risk") return(c("Risk", "Treatment"))
    if (parent_type == "Treatment") return(c("Cost", "Risk"))
    return(c("Cost", "Risk", "Issue", "Benefit", "Treatment", "Residual"))
  }

  show_add_modal <- function(default_type = "Cost", default_title = "", allowed_types = c("Cost", "Risk", "Issue", "Benefit", "Treatment", "Residual")) {
    if (!(default_type %in% allowed_types)) default_type <- allowed_types[1]
    
    showModal(modalDialog(
      title = "Add Child Node",
      textInput("new_title", "Title (Defaults to Parent's Title)", value = default_title),
      selectInput("new_type", "Element Type", choices = allowed_types, selected = default_type),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_child", "Save to DB", class = "btn-success")
      )
    ))
  }

  trigger_refresh <- reactiveVal(0)
  selected_node_id <- reactiveVal(NULL)
  browse_dir <- reactiveVal(getwd())
  
  output$db_controls_ui <- renderUI({
    if (is.null(rv$db_path) || rv$db_path == "") {
      fluidRow(
        column(6, actionButton("project_db_select", "Browse DB", icon = icon("folder-open"), class = "btn-outline-primary", width = "100%", style = "padding: 5px;")),
        column(6, actionButton("btn_new_project", "New DB", icon = icon("plus"), class = "btn-success", width = "100%", style = "padding: 5px;"))
      )
    } else {
      actionButton("btn_close_db", "Close DB", icon = icon("lock"), class = "btn-danger", width = "100%", style = "padding: 5px;")
    }
  })

  observeEvent(input$btn_close_db, {
    req(rv$db_path, rv$network_db_path)
    
    tryCatch({ sync_to_network(rv) }, error = function(e) warning(e))
    tryCatch({ release_db_lock(rv) }, error = function(e) warning(e))
    
    if (!is.null(rv$lock_dir) && dir.exists(rv$lock_dir)) {
      unlink(rv$lock_dir, recursive = TRUE, force = TRUE)
    }
    
    rv$db_path <- NULL
    rv$network_db_path <- NULL
    rv$focus_node_id <- NULL
    rv$lock_dir <- NULL
    selected_node_id(NULL)
    rv$current_leaf_state <- FALSE
    
    updateCheckboxInput(session, "is_leaf_check", value = FALSE)
    shinyjs::disable("is_leaf_check")
    shinyjs::disable("est_fieldset")
    shinyjs::hide("treatment_active_container")
    rv$mc_results <- NULL
    
    trigger_refresh(trigger_refresh() + 1)
    showNotification("Database successfully saved to network share and closed.", type = "message")
  })

  output$current_db_display <- renderText({
    if (is.null(rv$db_path)) return("No project database selected.")
    paste("Active DB:", basename(rv$db_path))
  })

  observeEvent(input$project_db_select, {
    showModal(modalDialog(
      title = "Select Project Database",
      h5(textOutput("current_browse_dir"), style = "margin-bottom: 15px; color: #2c3e50; font-weight: bold;"),
      DT::dataTableOutput("file_browser_table"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_confirm_db", "OK", class = "btn-primary")
      ),
      size = "l"
    ))
  })
  
  output$current_browse_dir <- renderText({
    paste("Directory:", browse_dir())
  })
  
  file_list_df <- reactive({
    d <- browse_dir()
    parent_dir <- dirname(d)
    
    dirs <- list.dirs(d, recursive = FALSE, full.names = FALSE)
    files <- list.files(d, pattern = "\\.(sqlite|db)$", ignore.case = TRUE, full.names = FALSE)
    
    paths <- c()
    names_col <- c()
    types <- c()
    
    if (d != parent_dir) {
      paths <- c(parent_dir)
      names_col <- c(".. (Up)")
      types <- c("Directory")
    }
    
    if (length(dirs) > 0) {
      paths <- c(paths, file.path(d, dirs))
      names_col <- c(names_col, dirs)
      types <- c(types, rep("Directory", length(dirs)))
    }
    
    if (length(files) > 0) {
      paths <- c(paths, file.path(d, files))
      names_col <- c(names_col, files)
      types <- c(types, rep("Database", length(files)))
    }
    
    if (length(paths) > 0) {
      info <- file.info(paths)
      df <- data.frame(
        Name = names_col, Type = types, Modified = format(info$mtime, "%Y-%m-%d %H:%M"),
        Size = ifelse(!is.na(info$size) & types == "Database", paste(round(info$size / 1024), "KB"), ""),
        Path = paths, stringsAsFactors = FALSE
      )
    } else {
      df <- data.frame(Name = character(), Type = character(), Modified = character(), Size = character(), Path = character())
    }
    df
  })
  
  output$file_browser_table <- DT::renderDataTable({
    DT::datatable(file_list_df()[, c("Name", "Type", "Modified", "Size")],
                  selection = "single", rownames = FALSE,
                  options = list(pageLength = 15, dom = 't', scrollY = "400px", paging = FALSE, ordering = FALSE))
  })
  
  observeEvent(input$file_browser_table_rows_selected, {
    idx <- input$file_browser_table_rows_selected
    req(idx)
    df <- file_list_df()
    row <- df[idx, ]
    if (row$Type == "Directory") { browse_dir(row$Path) }
  })
  
  observeEvent(input$btn_confirm_db, {
    idx <- input$file_browser_table_rows_selected
    if (length(idx) != 1) { showNotification("Please select a file.", type = "warning"); return() }
    
    df <- file_list_df()
    row <- df[idx, ]
    if (row$Type == "Directory") { browse_dir(row$Path); return() }
    
    new_network_path <- row$Path
    if (!is.null(rv$network_db_path) && new_network_path == rv$network_db_path) { removeModal(); return() }
    
    if (!acquire_db_lock(new_network_path, rv)) { return() }
    
    if (!is.null(rv$network_db_path) && rv$network_db_path != new_network_path) {
      sync_to_network(rv)
      release_db_lock(rv)
    }
    
    local_temp_path <- file.path(tempdir(), basename(new_network_path))
    file.copy(from = new_network_path, to = local_temp_path, overwrite = TRUE)
    
    rv$network_db_path <- new_network_path
    rv$db_path <- local_temp_path
    proj_name <- gsub("\\.sqlite$|\\.db$", "", basename(rv$db_path))
    
    con <- dbConnect(RSQLite::SQLite(), rv$db_path)
    dbExecute(con, "PRAGMA journal_mode=DELETE;")
    dbExecute(con, "PRAGMA busy_timeout=5000;")
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS financial_elements (
        id TEXT PRIMARY KEY, parent_id TEXT, element_type TEXT NOT NULL,
        title TEXT NOT NULL, opt_val REAL, likely_val REAL, pess_val REAL
      )")
    
    cols <- dbListFields(con, "financial_elements")
    if (!"chance" %in% cols) dbExecute(con, "ALTER TABLE financial_elements ADD COLUMN chance REAL DEFAULT 100")
    if (!"is_leaf" %in% cols) dbExecute(con, "ALTER TABLE financial_elements ADD COLUMN is_leaf INTEGER DEFAULT 0")
    if (!"is_active" %in% cols) dbExecute(con, "ALTER TABLE financial_elements ADD COLUMN is_active INTEGER DEFAULT 1")
    if (!"owner" %in% cols) dbExecute(con, "ALTER TABLE financial_elements ADD COLUMN owner TEXT")
    if (!"updated_at" %in% cols) dbExecute(con, "ALTER TABLE financial_elements ADD COLUMN updated_at TEXT")
    
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parent ON financial_elements(parent_id);")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_id ON financial_elements(id);")

    has_root <- dbGetQuery(con, "SELECT COUNT(*) as n FROM financial_elements WHERE id = 'root'")$n
    if (has_root == 0) {
      update_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      dbExecute(con, "INSERT INTO financial_elements (id, parent_id, element_type, title, chance, is_leaf, is_active, updated_at) VALUES ('root', NULL, 'Cost', ?, 100, 0, 1, ?)", params = list(proj_name, update_time))
      dbExecute(con, "UPDATE financial_elements SET parent_id = 'root' WHERE (parent_id IS NULL OR parent_id = '') AND id != 'root'")
    }
    dbDisconnect(con)
    
    rv$focus_node_id <- NULL
    selected_node_id(NULL)
    rv$current_leaf_state <- FALSE
    updateCheckboxInput(session, "is_leaf_check", value = FALSE)
    shinyjs::disable("is_leaf_check")
    shinyjs::disable("est_fieldset")
    shinyjs::hide("treatment_active_container")
    rv$mc_results <- NULL
    
    trigger_refresh(trigger_refresh() + 1)
    removeModal()
  })

  selected_create_path <- reactiveVal(NULL)
  
  observeEvent(input$btn_new_project, {
    volumes <- get_safe_volumes()
    shinyDirChoose(input, "new_proj_dir", roots = volumes, session = session)
    
    showModal(modalDialog(
      title = "Create New Project Database",
      textInput("new_proj_name", "Project Name", placeholder = "e.g., Enterprise ERP"),
      textInput("new_proj_file", "Database File Name", value = paste0("project_", format(Sys.Date(), "%Y%m%d"), ".sqlite")),
      
      tags$label("Destination Folder:"),
      br(),
      shinyDirButton("new_proj_dir", "Select Folder", "Please select a destination folder", class = "btn-info"),
      verbatimTextOutput("selected_dir_path"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_execute_create", "Create & Load DB", class = "btn-primary")
      )
    ))
  })
  
  observe({
    req(input$new_proj_dir)
    if (!is.integer(input$new_proj_dir)) {
      volumes <- get_safe_volumes()
      path <- parseDirPath(volumes, input$new_proj_dir)
      selected_create_path(path)
    }
  })
  
  output$selected_dir_path <- renderPrint({
    path <- selected_create_path()
    if (is.null(path) || length(path) == 0) cat("No folder selected.") else cat(path)
  })

  observeEvent(input$btn_execute_create, {
    req(input$new_proj_name, input$new_proj_file, selected_create_path())
    full_network_path <- file.path(selected_create_path(), input$new_proj_file)
    
    if (file.exists(full_network_path)) {
      showNotification("A file with this name already exists in the selected folder.", type = "error")
      return()
    }
    if (!acquire_db_lock(full_network_path, rv)) { return() }
    if (!is.null(rv$network_db_path) && rv$network_db_path != full_network_path) {
      sync_to_network(rv)
      release_db_lock(rv)
    }

    local_temp_path <- file.path(tempdir(), input$new_proj_file)

    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), local_temp_path)
      dbExecute(con, "PRAGMA journal_mode=DELETE;")
      dbExecute(con, "PRAGMA busy_timeout=5000;")
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS financial_elements (
          id TEXT PRIMARY KEY, parent_id TEXT, element_type TEXT NOT NULL,
          title TEXT NOT NULL, opt_val REAL, likely_val REAL, pess_val REAL,
          chance REAL DEFAULT 100, is_leaf INTEGER DEFAULT 0,
          is_active INTEGER DEFAULT 1, owner TEXT, updated_at TEXT
        )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parent ON financial_elements(parent_id);")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_id ON financial_elements(id);")

      update_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      dbExecute(con, "INSERT INTO financial_elements (id, parent_id, element_type, title, chance, is_leaf, is_active, updated_at) VALUES ('root', NULL, 'Cost', ?, 100, 0, 1, ?)", 
                params = list(trimws(input$new_proj_name), update_time))
      dbDisconnect(con)
      
      file.copy(from = local_temp_path, to = full_network_path, overwrite = TRUE)
      
      rv$network_db_path <- full_network_path
      rv$db_path <- local_temp_path
      rv$focus_node_id <- NULL
      selected_node_id(NULL)
      rv$current_leaf_state <- FALSE
      updateCheckboxInput(session, "is_leaf_check", value = FALSE)
      shinyjs::disable("is_leaf_check")
      shinyjs::disable("est_fieldset")
      shinyjs::hide("treatment_active_container")
      rv$mc_results <- NULL
      
      trigger_refresh(trigger_refresh() + 1)
      removeModal()
      showNotification(paste("Database created locally and synced to:", basename(full_network_path)), type = "message")
      
    }, error = function(e) {
      release_db_lock(rv)
      showNotification(paste("Error creating database:", e$message), type = "error")
    })
  })

  observeEvent(input$pasted_estimates, {
    txt <- input$pasted_estimates
    parts <- strsplit(txt, "\t")[[1]]
    if (length(parts) < 3) parts <- strsplit(txt, " {2,}")[[1]]
    if (length(parts) < 3) parts <- strsplit(txt, "\\s+")[[1]]
    
    if (length(parts) >= 3) {
      clean_val <- function(x) {
        val <- as.numeric(gsub("[^0-9.-]", "", x))
        if (is.na(val)) return(NULL) else return(val)
      }
      
      v_opt <- clean_val(parts[1])
      v_lik <- clean_val(parts[2])
      v_pes <- clean_val(parts[3])
      v_chn <- if(length(parts) >= 4) clean_val(parts[4]) else NULL
      
      if (!is.null(v_opt)) updateAutonumericInput(session, "est_opt", value = v_opt)
      if (!is.null(v_lik)) updateAutonumericInput(session, "est_likely", value = v_lik)
      if (!is.null(v_pes)) updateAutonumericInput(session, "est_pess", value = v_pes)
      
      if (!is.null(v_chn)) {
        if (v_chn > 0 && v_chn <= 1) v_chn <- v_chn * 100
        updateNumericInput(session, "est_chance", value = v_chn)
      }
    }
  })
  
  observeEvent(input$pasted_owner_estimates, {
    txt <- input$pasted_owner_estimates
    parts <- strsplit(txt, "\t")[[1]]
    if (length(parts) < 2) parts <- strsplit(txt, " {2,}")[[1]]
    
    if (length(parts) > 1) { 
      updateTextInput(session, "est_owner", value = trimws(parts[1]))
      clean_val <- function(x) {
        val <- as.numeric(gsub("[^0-9.-]", "", x))
        if (is.na(val)) return(NULL) else return(val)
      }
      
      if (length(parts) >= 2) {
        v_opt <- clean_val(parts[2])
        if (!is.null(v_opt)) updateAutonumericInput(session, "est_opt", value = v_opt)
      }
      if (length(parts) >= 3) {
        v_lik <- clean_val(parts[3])
        if (!is.null(v_lik)) updateAutonumericInput(session, "est_likely", value = v_lik)
      }
      if (length(parts) >= 4) {
        v_pes <- clean_val(parts[4])
        if (!is.null(v_pes)) updateAutonumericInput(session, "est_pess", value = v_pes)
      }
      if (length(parts) >= 5) {
        v_chn <- clean_val(parts[5])
        if (!is.null(v_chn)) {
          if (v_chn > 0 && v_chn <= 1) v_chn <- v_chn * 100
          updateNumericInput(session, "est_chance", value = v_chn)
        }
      }
    } else {
      updateTextInput(session, "est_owner", value = txt)
    }
  })
  
  output$wbs_tree <- renderTree({
    trigger_refresh() 
    if (is.null(rv$db_path)) return(list())
    
    con <- get_db()
    df <- dbGetQuery(con, "SELECT id, parent_id, element_type, title, is_active FROM financial_elements")
    dbDisconnect(con)
    
    if (nrow(df) == 0) return(list())
    
    isolate({
      opened_ids <- if (rv$force_open_all) df$id else extract_opened_ids(input$wbs_tree)
      if (rv$force_open_all) rv$force_open_all <- FALSE 
      
      if (!is.null(rv$focus_node_id) && !(rv$focus_node_id %in% opened_ids)) {
        opened_ids <- c(opened_ids, rv$focus_node_id)
      }
      
      ancestors_to_open <- c()
      if (!is.null(rv$focus_node_id)) {
        curr <- rv$focus_node_id
        while (!is.null(curr) && curr != "" && !is.na(curr)) {
          p_idx <- match(curr, df$id)
          if (is.na(p_idx)) break
          p_id <- df$parent_id[p_idx]
          if (!is.na(p_id) && p_id != "") ancestors_to_open <- c(ancestors_to_open, p_id)
          curr <- p_id
        }
      }
      tree_data <- build_nested_tree(df, parent = NA, focus_id = rv$focus_node_id, opened_ids = opened_ids, ancestor_ids = ancestors_to_open)
    })
    return(tree_data)
  })
  
  observeEvent(input$wbs_tree, {
    req(input$wbs_tree, rv$db_path)
    sel_id <- find_selected_node(input$wbs_tree)
    
    if (!is.null(sel_id) && length(sel_id) > 0) {
      if (is.null(selected_node_id()) || sel_id != selected_node_id()) {
        selected_node_id(sel_id)
        rv$focus_node_id <- sel_id
        
        con <- get_db()
        node_data <- dbGetQuery(con, "SELECT * FROM financial_elements WHERE id = ?", params = list(sel_id))
        dbDisconnect(con)
        
        if (nrow(node_data) > 0) {
          is_leaf_val <- ifelse(is.na(node_data$is_leaf[1]), 0, node_data$is_leaf[1]) == 1
          rv$current_leaf_state <- is_leaf_val
          updateCheckboxInput(session, "is_leaf_check", value = is_leaf_val)
          
          shinyjs::enable("is_leaf_check")
          if (is_leaf_val) shinyjs::enable("est_fieldset") else shinyjs::disable("est_fieldset")
          
          if (node_data$element_type[1] == "Treatment") {
            shinyjs::show("treatment_active_container")
            updateCheckboxInput(session, "chk_active_treatment", value = ifelse(is.na(node_data$is_active[1]), 0, node_data$is_active[1]) == 1)
          } else {
            shinyjs::hide("treatment_active_container")
          }
          
          updateAutonumericInput(session, "est_opt", value = node_data$opt_val[1])
          updateAutonumericInput(session, "est_likely", value = node_data$likely_val[1])
          updateAutonumericInput(session, "est_pess", value = node_data$pess_val[1])
          updateNumericInput(session, "est_chance", value = ifelse(is.na(node_data$chance[1]), 100, node_data$chance[1]))
          updateTextInput(session, "est_owner", value = ifelse(is.na(node_data$owner[1]), "", node_data$owner[1]))
        }
      }
    } else {
      selected_node_id(NULL)
      shinyjs::disable("is_leaf_check")
      shinyjs::disable("est_fieldset")
      shinyjs::hide("treatment_active_container")
    }
  }, ignoreInit = TRUE)
  
  output$header_title <- renderText({
    if (is.null(rv$db_path) || is.null(selected_node_id())) return("Node Estimate: None Selected")
    con <- get_db()
    title <- dbGetQuery(con, "SELECT title FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$title
    dbDisconnect(con)
    paste("Node Estimate:", title)
  })
  
  output$meta_id <- renderText({ selected_node_id() })
  output$meta_type <- renderText({
    if (is.null(rv$db_path) || is.null(selected_node_id())) return("")
    con <- get_db()
    type <- dbGetQuery(con, "SELECT element_type FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$element_type
    dbDisconnect(con)
    type
  })
  
  output$meta_updated <- renderText({
    if (is.null(rv$db_path) || is.null(selected_node_id())) return("")
    con <- get_db()
    upd <- dbGetQuery(con, "SELECT updated_at FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$updated_at
    dbDisconnect(con)
    if (is.na(upd) || upd == "") return("Last Updated: Never")
    paste("Last Updated:", upd)
  })
  
  observeEvent(input$is_leaf_check, {
    req(selected_node_id())
    if (input$is_leaf_check != rv$current_leaf_state) {
      if (!input$is_leaf_check) {
        showModal(modalDialog(
          title = "Remove Leaf Status?",
          "Unchecking this will remove leaf status and clear all estimates. Continue?",
          footer = tagList(
            actionButton("confirm_uncheck", "Yes, clear estimates", class = "btn-danger"),
            actionButton("cancel_uncheck", "Cancel")
          )
        ))
      } else {
        rv$current_leaf_state <- TRUE
        con <- get_db()
        dbExecute(con, "UPDATE financial_elements SET is_leaf = 1 WHERE id = ?", params = list(selected_node_id()))
        dbDisconnect(con)
        shinyjs::enable("est_fieldset")
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_uncheck, {
    removeModal()
    rv$current_leaf_state <- TRUE 
    updateCheckboxInput(session, "is_leaf_check", value = TRUE)
  })
  
  observeEvent(input$confirm_uncheck, {
    removeModal()
    con <- get_db()
    update_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, "UPDATE financial_elements SET is_leaf = 0, opt_val = NULL, likely_val = NULL, pess_val = NULL, chance = 100, owner = NULL, updated_at = ? WHERE id = ?", params = list(update_time, selected_node_id()))
    dbDisconnect(con)
    
    rv$current_leaf_state <- FALSE
    updateTextInput(session, "est_owner", value = "")
    updateAutonumericInput(session, "est_opt", value = "")
    updateAutonumericInput(session, "est_likely", value = "")
    updateAutonumericInput(session, "est_pess", value = "")
    updateNumericInput(session, "est_chance", value = 100)
    shinyjs::disable("est_fieldset")
    trigger_refresh(trigger_refresh() + 1)
  })

  observeEvent(input$chk_active_treatment, {
    req(selected_node_id())
    con <- get_db()
    el_type <- dbGetQuery(con, "SELECT element_type FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$element_type
    
    if (length(el_type) > 0 && el_type == "Treatment") {
      new_val <- ifelse(input$chk_active_treatment, 1, 0)
      dbExecute(con, "UPDATE financial_elements SET is_active = ? WHERE id = ?", params = list(new_val, selected_node_id()))
      
      if (new_val == 0) {
        ids_to_update <- c()
        current_parents <- c(selected_node_id())
        while(length(current_parents) > 0) {
          placeholders <- paste(rep("?", length(current_parents)), collapse=",")
          q <- sprintf("SELECT id, element_type FROM financial_elements WHERE parent_id IN (%s)", placeholders)
          kids <- dbGetQuery(con, q, params = as.list(current_parents))
          if (nrow(kids) > 0) {
            trt_kids <- kids$id[kids$element_type == "Treatment"]
            ids_to_update <- c(ids_to_update, trt_kids)
            current_parents <- kids$id
          } else {
            current_parents <- c()
          }
        }
        if (length(ids_to_update) > 0) {
          placeholders <- paste(rep("?", length(ids_to_update)), collapse=",")
          dbExecute(con, sprintf("UPDATE financial_elements SET is_active = 0 WHERE id IN (%s)", placeholders), params = as.list(ids_to_update))
        }
      }
      trigger_refresh(trigger_refresh() + 1)
    }
    dbDisconnect(con)
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_save_est, {
    req(selected_node_id())
    
    if (trimws(input$est_owner) == "") {
      showNotification("Owner Name is required.", type = "error")
      return()
    }
    
    con <- get_db()
    node_state <- dbGetQuery(con, "SELECT element_type, chance FROM financial_elements WHERE id = ?", params = list(selected_node_id()))
    current_type <- if (nrow(node_state) > 0) node_state$element_type[1] else "Cost"
    update_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    new_type <- current_type
    if (current_type == "Risk" && !is.na(input$est_chance) && as.numeric(input$est_chance) >= 100) {
      new_type <- "Issue"
    }
    dbExecute(con, "UPDATE financial_elements SET opt_val = ?, likely_val = ?, pess_val = ?, chance = ?, owner = ?, element_type = ?, updated_at = ? WHERE id = ?", 
              params = list(input$est_opt, input$est_likely, input$est_pess, input$est_chance, trimws(input$est_owner), new_type, update_time, selected_node_id()))
    
    parent_id <- dbGetQuery(con, "SELECT parent_id FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$parent_id
    dbDisconnect(con)
    
    if (current_type == "Risk" && !is.na(input$est_chance) && as.numeric(input$est_chance) >= 100) {
      showNotification("This Risk reached 100% probability and has been converted to an Issue. Use Edit Node to move it to the Issue tree.", type = "message", duration = 10)
    } else {
      showNotification("Estimates saved successfully", type = "message")
    }
    output$meta_updated <- renderText({ paste("Last Updated:", update_time) })
    
    if (!is.na(parent_id) && parent_id != "") {
      rv$focus_node_id <- parent_id
      trigger_refresh(trigger_refresh() + 1)
    }
  })

  observeEvent(input$btn_edit_node, {
    if (is.null(selected_node_id())) { showNotification("Please select a node to edit.", type = "warning"); return() }
    con <- get_db()
    node_info <- dbGetQuery(con, "SELECT title, element_type, parent_id FROM financial_elements WHERE id = ?", params = list(selected_node_id()))
    
    parent_type <- "root"
    if (!is.null(node_info$parent_id[1]) && !is.na(node_info$parent_id[1]) && node_info$parent_id[1] != "" && node_info$parent_id[1] != "root") {
      p_info <- dbGetQuery(con, "SELECT element_type FROM financial_elements WHERE id = ?", params = list(node_info$parent_id[1]))
      if (nrow(p_info) > 0) parent_type <- p_info$element_type[1]
    }
    
    df <- dbGetQuery(con, "SELECT id, parent_id, element_type, title FROM financial_elements")
    dbDisconnect(con)

    allowed_types <- if (parent_type == "root") c("Cost", "Risk", "Issue", "Benefit", "Treatment", "Residual") else get_allowed_child_types(parent_type)

    issue_move_ui <- NULL
    if (node_is_issue_in_risk_tree(df, selected_node_id())) {
      issue_move_ui <- tagList(
        hr(),
        div(style = "margin-top: 8px; margin-bottom: 8px; font-weight: 600;", "Move to an existing Issue-tree parent"),
        shinyTree("issue_move_tree")
      )
    }
    
    showModal(modalDialog(
      title = ifelse(selected_node_id() == "root", "Edit Project Name", "Edit Node"),
      textInput("node_name_input", "Name", value = node_info$title[1]),
      if (selected_node_id() != "root") {
        selectInput("node_type_input", "Element Type", choices = allowed_types, selected = node_info$element_type[1])
      },
      issue_move_ui,
      footer = tagList(
        if (!is.null(issue_move_ui)) actionButton("move_node_to_issue_tree", "Move to Issue Tree", class = "btn-warning"),
        modalButton("Cancel"),
        actionButton("save_node_name", "Save", class = "btn-success")
      )
    ))
  }, ignoreInit = TRUE)

  output$issue_move_tree <- renderTree({
    req(selected_node_id(), rv$db_path)
    con <- get_db()
    df <- dbGetQuery(con, "SELECT id, parent_id, element_type, title FROM financial_elements")
    dbDisconnect(con)

    if (!node_is_issue_in_risk_tree(df, selected_node_id())) return(list())
    excluded_ids <- c(selected_node_id(), collect_descendants(df, selected_node_id()))
    tree_data <- build_issue_candidate_tree(df, parent = NA, excluded_ids = excluded_ids)
    if (length(tree_data) == 0 || identical(tree_data, "")) return(list())
    tree_data
  })

  observeEvent(input$move_node_to_issue_tree, {
    req(selected_node_id())
    con <- get_db()
    df <- dbGetQuery(con, "SELECT id, parent_id, element_type, title FROM financial_elements")
    target_id <- find_selected_node(input$issue_move_tree)
    dbDisconnect(con)

    if (is.null(target_id) || length(target_id) == 0 || target_id == "") {
      showNotification("Please select a target parent in the Issue tree first.", type = "warning")
      return()
    }
    if (target_id %in% c(selected_node_id(), collect_descendants(df, selected_node_id()))) {
      showNotification("A node cannot be moved under its own descendant.", type = "error")
      return()
    }

    con <- get_db()
    dbExecute(con, "UPDATE financial_elements SET parent_id = ?, element_type = 'Issue' WHERE id = ?", params = list(target_id, selected_node_id()))
    dbDisconnect(con)
    removeModal()
    trigger_refresh(trigger_refresh() + 1)
    showNotification("Node moved into the Issue tree.", type = "message")
  }, ignoreInit = TRUE)
  
  observeEvent(input$save_node_name, {
    new_title <- trimws(input$node_name_input)
    if (new_title == "") { showNotification("Name cannot be blank.", type = "error"); return() }
    
    new_type <- if (!is.null(input$node_type_input)) input$node_type_input else "Cost"

    con <- get_db()
    node_state <- dbGetQuery(con, "SELECT element_type, chance FROM financial_elements WHERE id = ?", params = list(selected_node_id()))
    if (nrow(node_state) > 0) {
      chance_val <- if (is.na(node_state$chance[1])) 100 else as.numeric(node_state$chance[1])
      if (node_state$element_type[1] == "Risk" && chance_val >= 100) {
        new_type <- "Issue"
      }
    }
    
    if (grepl("(?i)treatment", new_title) && new_type != "Treatment") {
      showNotification("Warning: Node name contains 'Treatment' but element type is not 'Treatment'.", type = "warning", duration = 10)
    }
    
    dbExecute(con, "UPDATE financial_elements SET title = ?, element_type = ? WHERE id = ?", params = list(new_title, new_type, selected_node_id()))
    dbDisconnect(con)
    
    if (selected_node_id() == "root") {
      safe_filename <- gsub("[/\\\\:*?\"<>|]", "_", new_title)
      new_db_path <- paste0(safe_filename, ".sqlite")
      if (new_db_path != rv$db_path && file.exists(new_db_path)) {
        showNotification("A project file with this name already exists.", type = "error")
        removeModal()
        return()
      }
      if (new_db_path != rv$db_path) {
        old_path <- rv$db_path
        release_db_lock(rv)
        file.rename(old_path, new_db_path)
        acquire_db_lock(new_db_path, rv)
        rv$db_path <- new_db_path
      }
    }
    removeModal()
    trigger_refresh(trigger_refresh() + 1)
    showNotification("Node updated.", type = "message")
  })
  
  observeEvent(input$btn_delete_node, {
    if (is.null(selected_node_id())) { showNotification("Please select a node to delete.", type = "warning"); return() }
    if (selected_node_id() == "root") { showNotification("The Master Root Node cannot be deleted.", type = "error"); return() }
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to delete this node AND all of its children? This cannot be undone.",
      footer = tagList(
        actionButton("confirm_delete", "Yes, Delete", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$confirm_delete, {
    removeModal()
    con <- get_db()
    ids_to_delete <- c(selected_node_id())
    current_parents <- c(selected_node_id())
    
    while(length(current_parents) > 0) {
      placeholders <- paste(rep("?", length(current_parents)), collapse=",")
      q <- sprintf("SELECT id FROM financial_elements WHERE parent_id IN (%s)", placeholders)
      kids <- dbGetQuery(con, q, params = as.list(current_parents))$id
      if (length(kids) > 0) { ids_to_delete <- c(ids_to_delete, kids); current_parents <- kids
      } else { current_parents <- c() }
    }
    
    placeholders <- paste(rep("?", length(ids_to_delete)), collapse=",")
    dbExecute(con, sprintf("DELETE FROM financial_elements WHERE id IN (%s)", placeholders), params = as.list(ids_to_delete))
    dbDisconnect(con)
    
    selected_node_id(NULL)
    trigger_refresh(trigger_refresh() + 1)
    showNotification("Node and children deleted.", type = "message")
  })
  
  observeEvent(input$btn_add_node, { 
    if (is.null(selected_node_id())) {
      showNotification("Please select a parent node first.", type = "error")
    } else {
      con <- get_db()
      node_data <- dbGetQuery(con, "SELECT element_type, is_leaf, title FROM financial_elements WHERE id = ?", params = list(selected_node_id()))
      dbDisconnect(con)
      parent_title <- node_data$title[1]
      parent_type <- node_data$element_type[1]
      allowed_types <- get_allowed_child_types(parent_type)
      
      if (ifelse(is.na(node_data$is_leaf[1]), 0, node_data$is_leaf[1]) == 1) {
        showModal(modalDialog(
          title = "Convert to Parent Node?",
          "This node is marked as a Leaf Node. Adding a child will remove Leaf status and clear estimates. Continue?",
          footer = tagList(
            actionButton("confirm_add_child_clear", "Yes, add child & clear estimates", class = "btn-danger"),
            actionButton("cancel_add_child", "Cancel")
          )
        ))
      } else { 
        show_add_modal(default_type = parent_type, default_title = parent_title, allowed_types = allowed_types) 
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$cancel_add_child, { removeModal() })
  
  observeEvent(input$confirm_add_child_clear, {
    removeModal()
    con <- get_db()
    update_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, "UPDATE financial_elements SET is_leaf = 0, opt_val = NULL, likely_val = NULL, pess_val = NULL, chance = 100, owner = NULL, updated_at = ? WHERE id = ?", params = list(update_time, selected_node_id()))
    parent_info <- dbGetQuery(con, "SELECT element_type, title FROM financial_elements WHERE id = ?", params = list(selected_node_id()))
    dbDisconnect(con)
    
    rv$current_leaf_state <- FALSE
    updateCheckboxInput(session, "is_leaf_check", value = FALSE)
    shinyjs::disable("est_fieldset")
    updateTextInput(session, "est_owner", value = "")
    updateAutonumericInput(session, "est_opt", value = "")
    updateAutonumericInput(session, "est_likely", value = "")
    updateAutonumericInput(session, "est_pess", value = "")
    updateNumericInput(session, "est_chance", value = 100)
    
    p_type <- parent_info$element_type[1]
    show_add_modal(default_type = p_type, default_title = parent_info$title[1], allowed_types = get_allowed_child_types(p_type))
  })
  
  observeEvent(input$save_child, {
    if (trimws(input$new_title) == "") { showNotification("Title cannot be blank.", type = "error"); return() }
    
    if (grepl("(?i)treatment", input$new_title) && input$new_type != "Treatment") {
      showNotification("Warning: Node name contains 'Treatment' but element type is not 'Treatment'.", type = "warning", duration = 10)
    }
    
    con <- get_db()
    new_id <- generate_id()
    update_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    init_active <- ifelse(input$new_type == "Treatment", 0, 1)
    
    dbExecute(con, "INSERT INTO financial_elements (id, parent_id, element_type, title, chance, is_leaf, is_active, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
              params = list(new_id, selected_node_id(), input$new_type, trimws(input$new_title), 100, 0, init_active, update_time))
    
    dbDisconnect(con)
    removeModal()
    rv$focus_node_id <- new_id
    trigger_refresh(trigger_refresh() + 1)
  })

  output$node_mini_plot <- renderPlot({
    trigger_refresh() 
    if (is.null(rv$db_path) || is.null(selected_node_id())) return(NULL)
    
    con <- get_db()
    node_data <- dbGetQuery(con, "SELECT element_type, opt_val, likely_val, pess_val, chance FROM financial_elements WHERE id = ?", params = list(selected_node_id()))
    dbDisconnect(con)
    
    if (nrow(node_data) == 0 || is.na(node_data$opt_val[1]) || is.na(node_data$likely_val[1]) || is.na(node_data$pess_val[1])) {
      return(NULL)
    }
    
    n_iter <- 10000 
    calc_min <- min(c(node_data$opt_val[1], node_data$likely_val[1], node_data$pess_val[1]), na.rm = TRUE)
    calc_max <- max(c(node_data$opt_val[1], node_data$likely_val[1], node_data$pess_val[1]), na.rm = TRUE)
    calc_likely <- median(c(node_data$opt_val[1], node_data$likely_val[1], node_data$pess_val[1]), na.rm = TRUE)
    prob_decimal <- ifelse(is.na(node_data$chance[1]), 1, node_data$chance[1] / 100)
    
    sev <- rpert(n_iter, calc_min, calc_likely, calc_max)
    occ <- rbinom(n_iter, size = 1, prob = prob_decimal)
    
    val_mult <- ifelse(node_data$element_type[1] == "Benefit", -1, 1)
    mc_array <- sev * occ * val_mult
    plot_df <- data.frame(Value = mc_array)
    
    if (node_data$element_type[1] %in% c("Risk", "Issue", "Residual") && prob_decimal < 1) {
      plot_df <- plot_df[plot_df$Value != 0, , drop = FALSE]
    }
    if (nrow(plot_df) == 0) return(NULL)
    
    ggplot(plot_df, aes(x = Value)) +
      geom_histogram(fill = "#18bc9c", color = "white", bins = 40) +
      scale_x_continuous(labels = scales::dollar_format(scale_cut = scales::cut_short_scale())) +
      theme_minimal() + 
      theme(axis.title = element_blank(), axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(), plot.margin = margin(0, 0, 0, 0, "pt"))
  })

  observeEvent(input$use_seed, {
    if (input$use_seed) shinyjs::enable("seed_val") else shinyjs::disable("seed_val")
  })
  
  observeEvent(input$run_mc, {
    req(rv$db_path)
    con <- get_db()
    df <- dbGetQuery(con, "SELECT * FROM financial_elements")
    dbDisconnect(con)
    
    if (nrow(df[!is.na(df$opt_val), ]) == 0) {
      showNotification("No nodes with estimates found.", type = "warning")
      return()
    }
    
    seed_num <- suppressWarnings(as.numeric(input$seed_val))
    if (input$use_seed && !is.na(seed_num)) set.seed(seed_num) else set.seed(NULL)
    
    n_iter <- input$iterations
    
    calc_node <- function(node_id) {
      node <- df[df$id == node_id, ]
      is_active <- ifelse(is.na(node$is_active), 1, node$is_active)
      if (node$element_type == "Treatment" && is_active == 0) {
        return(rep(0, n_iter))
      }
      
      children <- df[!is.na(df$parent_id) & df$parent_id == node_id, ]
      val_mult <- ifelse(node$element_type == "Benefit", -1, 1)
      is_leaf <- ifelse(is.na(node$is_leaf), 0, node$is_leaf) == 1
      
      has_active_treatment <- FALSE
      if (node$element_type %in% c("Risk", "Issue") && nrow(children) > 0) {
        has_active_treatment <- any(children$element_type == "Treatment" & 
                                    ifelse(is.na(children$is_active), 1, children$is_active) == 1)
      }
      
      node_mc <- rep(0, n_iter)
      
      if (is_leaf && !is.na(node$opt_val) && !is.na(node$likely_val) && !is.na(node$pess_val)) {
        if (!has_active_treatment) {
          calc_min <- min(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
          calc_max <- max(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
          calc_likely <- median(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
          prob_decimal <- ifelse(is.na(node$chance), 1, node$chance / 100)
          
          sev <- rpert(n_iter, calc_min, calc_likely, calc_max)
          occ <- rbinom(n_iter, size = 1, prob = prob_decimal)
          node_mc <- sev * occ * val_mult
        }
      }
      
      if (nrow(children) > 0) {
        leaf_arrays <- list()
        parent_arrays <- list()
        
        for (i in 1:nrow(children)) {
          arr <- calc_node(children$id[i])
          child_is_leaf <- ifelse(is.na(children$is_leaf[i]), 0, children$is_leaf[i]) == 1
          if (child_is_leaf) {
            leaf_arrays <- c(leaf_arrays, list(arr))
          } else {
            parent_arrays <- c(parent_arrays, list(arr))
          }
        }
        if (length(leaf_arrays) > 0) node_mc <- node_mc + (Reduce("+", leaf_arrays) / length(leaf_arrays))
        if (length(parent_arrays) > 0) node_mc <- node_mc + Reduce("+", parent_arrays)
      }
      return(node_mc)
    }
    
    roots <- df[is.na(df$parent_id) | df$parent_id == "", ]
    total_exposure <- rep(0, n_iter)
    for (i in 1:nrow(roots)) {
      total_exposure <- total_exposure + calc_node(roots$id[i])
    }
    rv$mc_results <- total_exposure
  })
  
  output$mc_plot <- renderPlot({
    req(rv$mc_results)
    ggplot(data.frame(TotalExposure = rv$mc_results), aes(x = TotalExposure)) +
      geom_histogram(fill = "#2c3e50", color = "white", bins = 50) +
      geom_vline(aes(xintercept = median(TotalExposure)), color = "#e74c3c", linetype = "dashed", linewidth = 1) +
      scale_x_continuous(labels = scales::dollar_format(scale_cut = scales::cut_short_scale())) +
      theme_minimal() + labs(x = "Total Project Exposure ($)", y = "Frequency")
  })
  
  output$summary_stats <- renderTable({
    req(rv$mc_results)
    res <- rv$mc_results
    data.frame(
      Metric = c("Mean Exposure", "P10 (Favorable)", "P50 (Median)", "P90 (Unfavorable)"),
      Value = c(fmt_dollar(mean(res)), fmt_dollar(quantile(res, 0.10, names = FALSE)),
                fmt_dollar(quantile(res, 0.50, names = FALSE)), fmt_dollar(quantile(res, 0.90, names = FALSE)))
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  observeEvent(input$btn_export, {
    req(rv$db_path)
    con <- get_db()
    df <- dbGetQuery(con, "SELECT * FROM financial_elements")
    dbDisconnect(con)
    seed_num <- suppressWarnings(as.numeric(input$seed_val))
    seed_to_use <- if (input$use_seed && !is.na(seed_num)) seed_num else NULL
    out_df <- generate_report(df, n_iter = input$iterations, seed_val = seed_to_use)
    
    filename <- paste0("wbs-risk-report-", format(Sys.time(), "%Y%m%d-%H%M"), ".csv")
    filepath <- file.path(getwd(), filename)
    write.csv(out_df, filepath, row.names = FALSE, na = "")
    
    showNotification(paste("Report saved locally to:", filepath), type = "message", duration = 8)
  })
}