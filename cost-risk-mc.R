# ==========================================
# 0. SMART PACKAGE INSTALLATION & LOADING
# ==========================================
req_pkgs <- c("shiny", "bslib", "shinyTree", "DBI", "RSQLite", "ggplot2", "shinyjs", "shinyWidgets", "DT")
missing_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[,"Package"])]

if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org/")
}

invisible(lapply(req_pkgs, library, character.only = TRUE))

# ==========================================
# 1. HELPER FUNCTIONS & MATH ENGINE
# ==========================================
rpert <- function(n, x.min, x.mode, x.max, lambda = 4) {
  if (is.na(x.min) || is.na(x.max) || is.na(x.mode)) return(rep(0, n))
  if (x.min == x.max) return(rep(x.min, n))
  x.range <- x.max - x.min
  alpha <- 1 + lambda * (x.mode - x.min) / x.range
  beta <- 1 + lambda * (x.max - x.mode) / x.range
  return(x.min + x.range * rbeta(n, alpha, beta))
}

fmt_dollar <- function(x) {
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}

generate_id <- function() {
  paste0(sample(c(letters, 0:9), 16, replace = TRUE), collapse = "")
}

extract_opened_ids <- function(tree) {
  opened <- c()
  if (is.list(tree) && length(tree) > 0) {
    for (name in names(tree)) {
      node <- tree[[name]]
      
      ststate <- attr(node, "ststate")
      stclass <- attr(node, "stclass")
      
      is_open <- FALSE
      if (!is.null(ststate) && isTRUE(ststate$opened)) {
        is_open <- TRUE
      } else if (isTRUE(attr(node, "stopened"))) {
        is_open <- TRUE
      }
      
      if (is_open && !is.null(stclass)) {
        node_id <- sub(" inactive-node", "", stclass)
        node_id <- sub("^node_", "", node_id)
        opened <- c(opened, node_id)
      }
      
      opened <- c(opened, extract_opened_ids(node))
    }
  }
  return(opened)
}

build_nested_tree <- function(df, parent = NA, focus_id = NULL, opened_ids = NULL, ancestor_ids = NULL) {
  if (is.na(parent)) {
    children <- df[is.na(df$parent_id) | df$parent_id == "", ]
  } else {
    children <- df[!is.na(df$parent_id) & df$parent_id == parent, ]
  }
  
  if (nrow(children) == 0) return("")
  
  res <- list()
  for (i in 1:nrow(children)) {
    node_name <- children$title[i]
    node_id <- children$id[i]
    el_type <- children$element_type[i]
    
    child_node <- build_nested_tree(df, node_id, focus_id, opened_ids, ancestor_ids)
    
    if (is.null(opened_ids)) {
      attr(child_node, "stopened") <- TRUE 
    } else {
      attr(child_node, "stopened") <- (node_id %in% opened_ids) || (node_id %in% ancestor_ids)
    }
    
    is_active <- ifelse(is.na(children$is_active[i]), 1, children$is_active[i])
    if (el_type == "Treatment" && is_active == 0) {
      attr(child_node, "stclass") <- paste0("node_", node_id, " inactive-node")
    } else {
      attr(child_node, "stclass") <- paste0("node_", node_id) 
    }
    
    # Assign icon based on Element Type
    icon_class <- switch(el_type,
                         "Cost" = "fa fa-tags",
                         "Risk" = "fa fa-exclamation-triangle",
                         "Issue" = "fa fa-fire",
                         "Benefit" = "fa fa-gift",
                         "Treatment" = "fa fa-medkit",
                         "Residual" = "fa fa-filter",
                         "fa fa-folder") # fallback
    
    attr(child_node, "sticon") <- icon_class
    
    if (!is.null(focus_id) && node_id == focus_id) {
      attr(child_node, "stselected") <- TRUE
    }
    
    res[[node_name]] <- child_node
  }
  return(res)
}

find_selected_node <- function(tree) {
  if (isTRUE(attr(tree, "stselected"))) {
    # 2026-08-14T1039+12:00 - Strip 'node_' prefix to return correct DB ID so node details load and edit dialogs populate
    val <- sub(" inactive-node", "", attr(tree, "stclass"))
    return(sub("^node_", "", val)) 
  }
  if (is.list(tree)) {
    for (i in seq_along(tree)) {
      res <- find_selected_node(tree[[i]])
      if (!is.null(res)) {
        # 2026-08-14T1039+12:00 - Strip 'node_' prefix to return correct DB ID so node details load and edit dialogs populate
        val <- sub(" inactive-node", "", res)
        return(sub("^node_", "", val))
      }
    }
  }
  return(NULL)
}

collect_descendants <- function(df, node_id) {
  if (is.null(node_id) || is.na(node_id) || node_id == "" || node_id == "root") return(character())
  ids <- character()
  current <- c(node_id)
  while (length(current) > 0) {
    next_ids <- df$id[!is.na(df$parent_id) & df$parent_id %in% current]
    next_ids <- next_ids[!(next_ids %in% ids)]
    if (length(next_ids) == 0) break
    ids <- c(ids, next_ids)
    current <- next_ids
  }
  unique(ids)
}

node_is_issue_in_risk_tree <- function(df, node_id) {
  if (is.null(node_id) || is.na(node_id) || node_id == "" || node_id == "root") return(FALSE)
  node_row <- df[df$id == node_id, , drop = FALSE]
  if (nrow(node_row) == 0 || node_row$element_type[1] != "Issue") return(FALSE)

  current <- node_id
  seen <- character()
  has_issue_ancestor <- FALSE
  while (!is.na(current) && current != "" && !(current %in% seen)) {
    seen <- c(seen, current)
    row <- df[df$id == current, , drop = FALSE]
    if (nrow(row) > 0) {
      if (row$element_type[1] == "Issue" && current != node_id) {
        has_issue_ancestor <- TRUE
      }
      current <- row$parent_id[1]
    } else {
      current <- NA
    }
  }

  !has_issue_ancestor
}

build_issue_candidate_tree <- function(df, parent = NA, excluded_ids = character()) {
  if (is.na(parent)) {
    children <- df[df$element_type == "Issue" & (is.na(df$parent_id) | df$parent_id == ""), , drop = FALSE]
  } else {
    children <- df[df$element_type == "Issue" & !is.na(df$parent_id) & df$parent_id == parent, , drop = FALSE]
  }

  if (nrow(children) == 0) return("")

  res <- list()
  for (i in seq_len(nrow(children))) {
    node_id <- children$id[i]
    if (node_id %in% excluded_ids) next

    child_node <- build_issue_candidate_tree(df, node_id, excluded_ids)
    attr(child_node, "stclass") <- paste0("node_", node_id)
    attr(child_node, "sticon") <- "fa fa-fire"
    attr(child_node, "stopened") <- TRUE
    res[[children$title[i]]] <- child_node
  }

  if (length(res) == 0) return("")
  res
}

generate_report <- function(df, n_iter = 10000, seed_val = NULL) {
  if (nrow(df) == 0) return(data.frame())
  if (!is.null(seed_val) && !is.na(seed_val)) set.seed(seed_val) else set.seed(NULL)
  
  traverse <- function(node_id, level) {
    node <- df[df$id == node_id, ]
    children <- df[!is.na(df$parent_id) & df$parent_id == node_id, ]
    
    val_mult <- ifelse(node$element_type == "Benefit", -1, 1)
    is_leaf <- ifelse(is.na(node$is_leaf), 0, node$is_leaf) == 1
    is_active <- ifelse(is.na(node$is_active), 1, node$is_active)
    
    mc_array <- rep(0, n_iter)
    child_rows <- list()
    
    if (nrow(children) == 0) {
      if (is_leaf && !is.na(node$opt_val) && !is.na(node$likely_val) && !is.na(node$pess_val)) {
        calc_min <- min(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
        calc_max <- max(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
        calc_likely <- median(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
        prob_decimal <- ifelse(is.na(node$chance), 1, node$chance / 100)
        
        sev <- rpert(n_iter, calc_min, calc_likely, calc_max)
        occ <- rbinom(n_iter, size = 1, prob = prob_decimal)
        mc_array <- sev * occ * val_mult
      }
    } else {
      leaf_arrays <- list()
      parent_arrays <- list()
      
      for (i in 1:nrow(children)) {
        c_res <- traverse(children$id[i], level + 1)
        child_rows <- c(child_rows, c_res$rows)
        
        child_is_leaf <- ifelse(is.na(children$is_leaf[i]), 0, children$is_leaf[i]) == 1
        if (child_is_leaf) {
          leaf_arrays <- c(leaf_arrays, list(c_res$mc_array))
        } else {
          parent_arrays <- c(parent_arrays, list(c_res$mc_array))
        }
      }
      
      if (length(leaf_arrays) > 0) mc_array <- mc_array + (Reduce("+", leaf_arrays) / length(leaf_arrays))
      if (length(parent_arrays) > 0) mc_array <- mc_array + Reduce("+", parent_arrays)
    }
    
    row_data <- list(
      Element_Type = node$element_type,
      Active = ifelse(is_active == 1, "Yes", "No"),
      Leaf_Opt = if(is_leaf && !is.na(node$opt_val)) round(node$opt_val) else NA, 
      Leaf_Likely = if(is_leaf && !is.na(node$likely_val)) round(node$likely_val) else NA,
      Leaf_Pess = if(is_leaf && !is.na(node$pess_val)) round(node$pess_val) else NA, 
      Leaf_Prob = if(is_leaf && !is.na(node$chance)) round(node$chance) else NA,
      Rollup_MC_P10 = round(quantile(mc_array, 0.10, names = FALSE)),
      Rollup_MC_P50 = round(quantile(mc_array, 0.50, names = FALSE)),
      Rollup_MC_P90 = round(quantile(mc_array, 0.90, names = FALSE)),
      Updated = if(!is.na(node$updated_at)) node$updated_at else "",
      Level = level, Title = node$title
    )
    
    # 2026-08-14T11:38+12:00 - Stop aggregation only for Treatment nodes with Active unchecked, and for no other reason
    return_mc_array <- if (node$element_type == "Treatment" && is_active == 0) rep(0, n_iter) else mc_array
    return(list(rows = c(list(row_data), child_rows), mc_array = return_mc_array))
  }
  
  roots <- df[is.na(df$parent_id) | df$parent_id == "", ]
  all_rows <- list()
  for (i in 1:nrow(roots)) {
    all_rows <- c(all_rows, traverse(roots$id[i], 1)$rows)
  }
  
  out_df <- do.call(rbind, lapply(all_rows, as.data.frame, stringsAsFactors = FALSE))
  
  for (lvl in 1:8) {
    col_name <- paste0("L", lvl)
    out_df[[col_name]] <- ifelse(out_df$Level == lvl, out_df$Title, "")
  }
  
  col_order <- c("Element_Type", "Active", paste0("L", 1:8), "Leaf_Opt", "Leaf_Likely", "Leaf_Pess", "Leaf_Prob", "Rollup_MC_P10", "Rollup_MC_P50", "Rollup_MC_P90", "Updated")
  out_df <- out_df[, col_order]
  
  return(out_df)
}

# ==========================================
# 2. UI DEFINITION
# ==========================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      fieldset[disabled] label { color: #a0a0a0; }
      fieldset[disabled] input { color: #a0a0a0; background-color: #e9ecef; border-color: #dee2e6; }
      fieldset[disabled] .btn { pointer-events: none; opacity: 0.65; }
      .inactive-node { color: #a0a0a0 !important; font-style: italic; }
      #file_browser_table { cursor: pointer; } 
    ")),
    tags$script(HTML("
      $(document).on('paste', '#est_opt', function(e) {
        let pastedText = (e.originalEvent || e).clipboardData.getData('text');
        if (pastedText) {
          Shiny.setInputValue('pasted_estimates', pastedText, {priority: 'event'});
          e.preventDefault(); 
        }
      });
      $(document).on('paste', '#est_owner', function(e) {
        let pastedText = (e.originalEvent || e).clipboardData.getData('text');
        if (pastedText) {
          Shiny.setInputValue('pasted_owner_estimates', pastedText, {priority: 'event'});
          e.preventDefault(); 
        }
      });
      $(document).on('paste', '#new_title', function(e) {
        let pastedText = (e.originalEvent || e).clipboardData.getData('text');
        if (pastedText && pastedText.indexOf('\\t') !== -1) {
          e.preventDefault();
          let firstVal = pastedText.split('\\t')[0].trim();
          let el = $(this);
          el.val(firstVal);
          el.trigger('input'); 
        }
      });
    "))
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Integrated Cost & Risk Forecaster"),
  
  sidebarLayout(
    sidebarPanel(
      width = 5,
      h4("Project Explorer"),
      actionButton("project_db_select", "Browse for Project DB...", icon = icon("folder-open"), class = "btn-outline-primary", width = "100%"),
      div(style = "margin-top: 10px; margin-bottom: 15px; font-weight: bold; word-wrap: break-word;", textOutput("current_db_display")),
      actionButton("btn_edit_node", "Edit Node", icon = icon("edit"), class = "btn-secondary btn-sm mb-2"),
      actionButton("btn_add_node", "Add Child Node", icon = icon("plus"), class = "btn-primary btn-sm mb-2"),
      actionButton("btn_delete_node", "Delete Node", icon = icon("trash"), class = "btn-danger btn-sm mb-2"),
      hr(),
      shinyTree("wbs_tree")
    ),
    
    mainPanel(
      width = 7,
      card(
        card_header(class = "bg-dark text-white", textOutput("header_title", inline = TRUE)),
        card_body(
          fluidRow(
            column(6, strong("ID: "), textOutput("meta_id", inline = TRUE)),
            column(6, strong("Type: "), textOutput("meta_type", inline = TRUE))
          ),
          hr(),
          
          fluidRow(
            column(6, checkboxInput("is_leaf_check", "This is a Leaf Node (Enable Estimates)", value = FALSE)),
            # 2026-08-14T11:38+12:00 - Default Active checkbox to unchecked for Treatment nodes
            column(6, div(id = "treatment_active_container", checkboxInput("chk_active_treatment", "Treatment is Active (Included in Rollup)", value = FALSE)))
          ),
          
          tags$fieldset(id = "est_fieldset",
            fluidRow(
              column(6, textInput("est_owner", "Owner Name (Required)", value = "")),
              column(6, tags$div(style = "margin-top: 32px; color: #666; font-style: italic;", textOutput("meta_updated")))
            ),
            fluidRow(
              column(3, autonumericInput("est_opt", "Optimistic ($)", value = "", decimalPlaces = 0, digitGroupSeparator = ",")),
              column(3, autonumericInput("est_likely", "Likely ($)", value = "", decimalPlaces = 0, digitGroupSeparator = ",")),
              column(3, autonumericInput("est_pess", "Pessimistic ($)", value = "", decimalPlaces = 0, digitGroupSeparator = ",")),
              column(3, div(id = "prob_container", numericInput("est_chance", "Probability (%)", value = 100, min = 0, max = 100)))
            ),
            fluidRow(
              column(3, actionButton("btn_save_est", "Save Estimate", class = "btn-success", style = "margin-top: 20px; width: 100%;")),
              column(9, plotOutput("node_mini_plot", height = "120px"))
            )
          )
        )
      ),
      
      card(
        card_header("Simulation & Roll-up (Total Project Exposure)", class = "bg-primary text-white"),
        card_body(
          fluidRow(
            column(6, numericInput("iterations", "Iterations:", value = 10000, step = 1000, width = "100%"))
          ),
          fluidRow(
            column(6, textInput("seed_val", "Seed (Paste arbitrary number):", value = "12345678", width = "100%")),
            column(6, div(style = "margin-top: 32px;", checkboxInput("use_seed", "Set Seed", value = FALSE)))
          ),
          fluidRow(
            column(6, actionButton("run_mc", "Update Dashboard", class = "btn-warning", style = "width:100%;")),
            # 2026-08-14T11:38+12:00 - Change downloadButton to actionButton to enable saving directly to the R working directory
            column(6, actionButton("btn_export", "Export Detailed Report", class = "btn-info", style = "width:100%;"))
          ),
          hr(),
          fluidRow(
            column(8, plotOutput("mc_plot", height = "300px")),
            column(4, tableOutput("summary_stats"))
          )
        )
      )
    )
  )
)

# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    mc_results = NULL, 
    db_path = NULL,
    current_leaf_state = FALSE, 
    focus_node_id = NULL,
    force_open_all = TRUE 
  )

  get_db <- function() { dbConnect(RSQLite::SQLite(), rv$db_path) }
  
  trigger_refresh <- reactiveVal(0)
  selected_node_id <- reactiveVal(NULL)
  browse_dir <- reactiveVal(getwd())
  
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
        Name = names_col,
        Type = types,
        Modified = format(info$mtime, "%Y-%m-%d %H:%M"),
        Size = ifelse(!is.na(info$size) & types == "Database", paste(round(info$size / 1024), "KB"), ""),
        Path = paths,
        stringsAsFactors = FALSE
      )
    } else {
      df <- data.frame(Name = character(), Type = character(), Modified = character(), Size = character(), Path = character())
    }
    df
  })
  
  output$file_browser_table <- DT::renderDataTable({
    DT::datatable(file_list_df()[, c("Name", "Type", "Modified", "Size")],
                  selection = "single",
                  rownames = FALSE,
                  options = list(pageLength = 15, dom = 't', scrollY = "400px", paging = FALSE, ordering = FALSE))
  })
  
  observeEvent(input$file_browser_table_rows_selected, {
    idx <- input$file_browser_table_rows_selected
    req(idx)
    df <- file_list_df()
    row <- df[idx, ]
    if (row$Type == "Directory") {
      browse_dir(row$Path)
    }
  })
  
  observeEvent(input$btn_confirm_db, {
    idx <- input$file_browser_table_rows_selected
    if (length(idx) != 1) {
       showNotification("Please select a file.", type = "warning")
       return()
    }
    
    df <- file_list_df()
    row <- df[idx, ]
    
    if (row$Type == "Directory") {
       browse_dir(row$Path)
       return()
    }
    
    new_path <- row$Path
    if (!is.null(rv$db_path) && new_path == rv$db_path) {
      removeModal()
      return()
    }
    
    proj_name <- gsub("\\.sqlite$|\\.db$", "", basename(new_path))
    
    con <- dbConnect(RSQLite::SQLite(), new_path)
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
    
    has_root <- dbGetQuery(con, "SELECT COUNT(*) as n FROM financial_elements WHERE id = 'root'")$n
    if (has_root == 0) {
      update_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      dbExecute(con, "INSERT INTO financial_elements (id, parent_id, element_type, title, chance, is_leaf, is_active, updated_at) VALUES ('root', NULL, 'Cost', ?, 100, 0, 1, ?)", params = list(proj_name, update_time))
      dbExecute(con, "UPDATE financial_elements SET parent_id = 'root' WHERE (parent_id IS NULL OR parent_id = '') AND id != 'root'")
    }
    dbDisconnect(con)
    
    rv$db_path <- new_path
    
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
  
  # Tree Rendering
  output$wbs_tree <- renderTree({
    trigger_refresh()
    req(rv$db_path)
    con <- get_db()
    df <- dbGetQuery(con, "SELECT id, parent_id, element_type, title, is_active FROM financial_elements")
    dbDisconnect(con)
    
    if (nrow(df) == 0) return(list())
    
    # 2026-08-14T11:05+12:00 - Isolate reactive dependencies to prevent the tree from rebuilding on every click, which breaks subsequent selections.
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
    req(input$wbs_tree)
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
          if (is_leaf_val) {
            shinyjs::enable("est_fieldset")
          } else {
            shinyjs::disable("est_fieldset")
          }
          
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
    if (is.null(selected_node_id())) return("Node Estimate: None Selected")
    con <- get_db()
    title <- dbGetQuery(con, "SELECT title FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$title
    dbDisconnect(con)
    paste("Node Estimate:", title)
  })
  
  output$meta_id <- renderText({ selected_node_id() })
  
  output$meta_type <- renderText({
    if (is.null(selected_node_id())) return("")
    con <- get_db()
    type <- dbGetQuery(con, "SELECT element_type FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$element_type
    dbDisconnect(con)
    type
  })
  
  output$meta_updated <- renderText({
    if (is.null(selected_node_id())) return("")
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
    df <- dbGetQuery(con, "SELECT id, parent_id, element_type, title FROM financial_elements")
    dbDisconnect(con)

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
        selectInput("node_type_input", "Element Type", 
                    choices = c("Cost", "Risk", "Issue", "Benefit", "Treatment", "Residual"), 
                    selected = node_info$element_type[1])
      },
      issue_move_ui,
      footer = tagList(
        if (!is.null(issue_move_ui)) actionButton("move_node_to_issue_tree", "Move to Issue Tree", class = "btn-warning"),
        modalButton("Cancel"),
        actionButton("save_node_name", "Save", class = "btn-success")
      )
    ))
    shinyjs::runjs("setTimeout(function() { $('#node_name_input').focus().select(); }, 500);")
  })

  output$issue_move_tree <- renderTree({
    req(selected_node_id())
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
    
    # 2026-08-14T11:38+12:00 - Warn user on edit if name contains 'treatment' but element type is not 'Treatment'
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
        file.rename(rv$db_path, new_db_path)
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
  })
  
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
  
  show_add_modal <- function(default_type = "Cost", default_title = "") {
    showModal(modalDialog(
      title = "Add Child Node",
      textInput("new_title", "Title (Defaults to Parent's Title)", value = default_title),
      selectInput("new_type", "Element Type", choices = c("Cost", "Risk", "Issue", "Benefit", "Treatment", "Residual"), selected = default_type),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_child", "Save to DB", class = "btn-success")
      )
    ))
    shinyjs::runjs("setTimeout(function() { $('#new_title').focus().select(); }, 500);")
  }
  
  observeEvent(input$btn_add_node, { 
    if (is.null(selected_node_id())) {
      showNotification("Please select a parent node first.", type = "error")
    } else {
      con <- get_db()
      node_data <- dbGetQuery(con, "SELECT element_type, is_leaf, title FROM financial_elements WHERE id = ?", params = list(selected_node_id()))
      dbDisconnect(con)
      parent_title <- node_data$title[1]
      
      if (ifelse(is.na(node_data$is_leaf[1]), 0, node_data$is_leaf[1]) == 1) {
        showModal(modalDialog(
          title = "Convert to Parent Node?",
          "This node is marked as a Leaf Node. Adding a child will remove Leaf status and clear estimates. Continue?",
          footer = tagList(
            actionButton("confirm_add_child_clear", "Yes, add child & clear estimates", class = "btn-danger"),
            actionButton("cancel_add_child", "Cancel")
          )
        ))
      } else { show_add_modal(default_type = node_data$element_type[1], default_title = parent_title) }
    }
  })
  
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
    
    show_add_modal(default_type = parent_info$element_type[1], default_title = parent_info$title[1])
  })
  
  observeEvent(input$save_child, {
    if (trimws(input$new_title) == "") { showNotification("Title cannot be blank.", type = "error"); return() }
    
    # 2026-08-14T11:38+12:00 - Warn user if name contains 'treatment' but element type is not 'Treatment'
    if (grepl("(?i)treatment", input$new_title) && input$new_type != "Treatment") {
      showNotification("Warning: Node name contains 'Treatment' but element type is not 'Treatment'.", type = "warning", duration = 10)
    }
    
    con <- get_db()
    new_id <- generate_id()
    update_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # 2026-08-14T11:38+12:00 - Default Treatment nodes to inactive (0) in the database upon creation
    init_active <- ifelse(input$new_type == "Treatment", 0, 1)
    
    dbExecute(con, "INSERT INTO financial_elements (id, parent_id, element_type, title, chance, is_leaf, is_active, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
              params = list(new_id, selected_node_id(), input$new_type, trimws(input$new_title), 100, 0, init_active, update_time))
    
    dbDisconnect(con)
    removeModal()
    
    rv$focus_node_id <- new_id
    trigger_refresh(trigger_refresh() + 1)
    
    shinyjs::runjs("setTimeout(function() { $('#is_leaf_check').focus(); }, 800);")
  })

  output$node_mini_plot <- renderPlot({
    trigger_refresh() 
    req(selected_node_id())
    
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
    
if (node_data$element_type[1] %in% c("Risk", "Issue", "Residual") && prob_decimal < 1) {          plot_df <- plot_df[plot_df$Value != 0, , drop = FALSE]
    }
    
    if (nrow(plot_df) == 0) return(NULL)
    
    ggplot(plot_df, aes(x = Value)) +
      geom_histogram(fill = "#18bc9c", color = "white", bins = 40) +
      scale_x_continuous(labels = scales::dollar_format(scale_cut = scales::cut_short_scale())) +
      theme_minimal() + 
      theme(
        axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt")
      )
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
      
      if (nrow(children) == 0) {
        mc_array <- rep(0, n_iter)
        if (is_leaf && !is.na(node$opt_val) && !is.na(node$likely_val) && !is.na(node$pess_val)) {
          calc_min <- min(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
          calc_max <- max(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
          calc_likely <- median(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
          prob_decimal <- ifelse(is.na(node$chance), 1, node$chance / 100)
          
          sev <- rpert(n_iter, calc_min, calc_likely, calc_max)
          occ <- rbinom(n_iter, size = 1, prob = prob_decimal)
          mc_array <- sev * occ * val_mult
        }
        return(mc_array)
      } else {
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
        
        node_mc <- rep(0, n_iter)
        if (length(leaf_arrays) > 0) node_mc <- node_mc + (Reduce("+", leaf_arrays) / length(leaf_arrays))
        if (length(parent_arrays) > 0) node_mc <- node_mc + Reduce("+", parent_arrays)
        
        return(node_mc)
      }
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
  
  # 2026-08-14T11:38+12:00 - Save the report directly to the directory from which the R programme was run
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

# 2026-08-14T11:38+12:00 - Fix the listening port to 3296 so it does not change between builds or runs
shinyApp(ui, server, options = list(port = 3296))