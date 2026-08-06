# ==========================================
# 0. SMART PACKAGE INSTALLATION & LOADING
# ==========================================
req_pkgs <- c("shiny", "bslib", "shinyTree", "DBI", "RSQLite", "ggplot2", "shinyjs", "shinyWidgets")
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

build_nested_tree <- function(df, parent = NA, focus_id = NULL) {
  if (is.na(parent)) {
    children <- df[is.na(df$parent_id) | df$parent_id == "", ]
  } else {
    children <- df[!is.na(df$parent_id) & df$parent_id == parent, ]
  }
  
  if (nrow(children) == 0) return("")
  
  res <- list()
  for (i in 1:nrow(children)) {
    node_name <- children$title[i]
    child_node <- build_nested_tree(df, children$id[i], focus_id)
    attr(child_node, "stopened") <- TRUE 
    
    # Auto-focus the newly created node
    if (!is.null(focus_id) && children$id[i] == focus_id) {
      attr(child_node, "stselected") <- TRUE
    }
    
    res[[node_name]] <- child_node
  }
  return(res)
}

# --- RECURSIVE REPORT GENERATOR ---
generate_report <- function(df, n_iter = 10000, seed_val = NULL) {
  if (nrow(df) == 0) return(data.frame())
  
  if (!is.null(seed_val) && !is.na(seed_val)) set.seed(seed_val) else set.seed(NULL)
  
  traverse <- function(node_id, level) {
    node <- df[df$id == node_id, ]
    children <- df[!is.na(df$parent_id) & df$parent_id == node_id, ]
    
    val_mult <- ifelse(node$element_type == "Benefit", -1, 1)
    is_leaf <- ifelse(is.na(node$is_leaf), 0, node$is_leaf) == 1
    
    if (nrow(children) == 0) {
      opt <- node$opt_val; lik <- node$likely_val; pess <- node$pess_val; prob <- node$chance
      
      mc_array <- rep(0, n_iter)
      if (is_leaf && !is.na(opt) && !is.na(lik) && !is.na(pess)) {
        calc_min <- min(c(opt, lik, pess), na.rm = TRUE)
        calc_max <- max(c(opt, lik, pess), na.rm = TRUE)
        calc_likely <- median(c(opt, lik, pess), na.rm = TRUE)
        prob_decimal <- ifelse(is.na(prob), 1, prob / 100)
        
        sev <- rpert(n_iter, calc_min, calc_likely, calc_max)
        occ <- rbinom(n_iter, size = 1, prob = prob_decimal)
        mc_array <- sev * occ * val_mult
      }
      
      row_data <- list(
        Element_Type = node$element_type,
        Leaf_Opt = if(is_leaf && !is.na(opt)) round(opt) else NA, 
        Leaf_Likely = if(is_leaf && !is.na(lik)) round(lik) else NA,
        Leaf_Pess = if(is_leaf && !is.na(pess)) round(pess) else NA, 
        Leaf_Prob = if(is_leaf && !is.na(prob)) round(prob) else NA,
        Rollup_MC_P10 = round(quantile(mc_array, 0.10, names = FALSE)),
        Rollup_MC_P50 = round(quantile(mc_array, 0.50, names = FALSE)),
        Rollup_MC_P90 = round(quantile(mc_array, 0.90, names = FALSE)),
        Level = level, Title = node$title
      )
      
      return(list(rows = list(row_data), mc_array = mc_array))
      
    } else {
      child_rows <- list()
      sum_mc_array <- rep(0, n_iter) 
      
      for (i in 1:nrow(children)) {
        c_res <- traverse(children$id[i], level + 1)
        child_rows <- c(child_rows, c_res$rows)
        sum_mc_array <- sum_mc_array + c_res$mc_array
      }
      
      row_data <- list(
        Element_Type = node$element_type,
        Leaf_Opt = NA, Leaf_Likely = NA, Leaf_Pess = NA, Leaf_Prob = NA,
        Rollup_MC_P10 = round(quantile(sum_mc_array, 0.10, names = FALSE)),
        Rollup_MC_P50 = round(quantile(sum_mc_array, 0.50, names = FALSE)),
        Rollup_MC_P90 = round(quantile(sum_mc_array, 0.90, names = FALSE)),
        Level = level, Title = node$title
      )
      
      return(list(rows = c(list(row_data), child_rows), mc_array = sum_mc_array))
    }
  }
  
  roots <- df[is.na(df$parent_id) | df$parent_id == "", ]
  all_rows <- list()
  for (i in 1:nrow(roots)) {
    all_rows <- c(all_rows, traverse(roots$id[i], 1)$rows)
  }
  
  out_df <- do.call(rbind, lapply(all_rows, as.data.frame, stringsAsFactors = FALSE))
  
  max_lvl <- max(out_df$Level)
  for (lvl in 1:max_lvl) {
    col_name <- paste0("L", lvl)
    out_df[[col_name]] <- ifelse(out_df$Level == lvl, out_df$Title, "")
  }
  
  out_df$Level <- NULL
  out_df$Title <- NULL
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
    "))
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Integrated Cost & Risk Forecaster"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Project Explorer"),
      actionButton("btn_edit_root", "Edit Root Node", icon = icon("edit"), class = "btn-secondary btn-sm mb-2"),
      actionButton("btn_add_node", "Add Child Node", icon = icon("plus"), class = "btn-primary btn-sm mb-2"),
      actionButton("btn_delete_node", "Delete Node", icon = icon("trash"), class = "btn-danger btn-sm mb-2"),
      hr(),
      shinyTree("wbs_tree")
    ),
    
    mainPanel(
      width = 8,
      card(
        card_header(class = "bg-dark text-white", textOutput("header_title", inline = TRUE)),
        card_body(
          fluidRow(
            column(6, strong("ID: "), textOutput("meta_id", inline = TRUE)),
            column(6, strong("Type: "), textOutput("meta_type", inline = TRUE))
          ),
          hr(),
          
          checkboxInput("is_leaf_check", "This is a Leaf Node (Enable Estimates)", value = FALSE),
          
          tags$fieldset(id = "est_fieldset",
            fluidRow(
              column(3, autonumericInput("est_opt", "Optimistic ($)", value = "", decimalPlaces = 0, digitGroupSeparator = ",")),
              column(3, autonumericInput("est_likely", "Likely ($)", value = "", decimalPlaces = 0, digitGroupSeparator = ",")),
              column(3, autonumericInput("est_pess", "Pessimistic ($)", value = "", decimalPlaces = 0, digitGroupSeparator = ",")),
              column(3, div(id = "prob_container", numericInput("est_chance", "Probability (%)", value = 100, min = 0, max = 100)))
            ),
            actionButton("btn_save_est", "Save Estimate", class = "btn-success mt-2")
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
            column(6, numericInput("seed_val", "Seed:", value = 12345678, width = "100%")),
            column(6, div(style = "margin-top: 32px;", checkboxInput("use_seed", "Set Seed", value = FALSE)))
          ),
          fluidRow(
            column(6, actionButton("run_mc", "Calculate", class = "btn-warning", style = "width:100%;")),
            column(6, downloadButton("btn_export", "Export CSV", class = "btn-info", style = "width:100%;"))
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
    focus_node_id = NULL
  )
  
  # --- DB Initialization ---
  sqlite_files <- list.files(pattern = "\\.sqlite$")
  if (length(sqlite_files) > 0) {
    init_path <- sqlite_files[1]
    proj_name <- gsub("\\.sqlite$", "", init_path)
  } else {
    proj_name <- "New Project"
    init_path <- paste0(proj_name, ".sqlite")
  }
  
  con <- dbConnect(RSQLite::SQLite(), init_path)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS financial_elements (
      id TEXT PRIMARY KEY, parent_id TEXT, element_type TEXT NOT NULL,
      title TEXT NOT NULL, opt_val REAL, likely_val REAL, pess_val REAL
    )")
  
  cols <- dbListFields(con, "financial_elements")
  if (!"chance" %in% cols) dbExecute(con, "ALTER TABLE financial_elements ADD COLUMN chance REAL DEFAULT 100")
  if (!"is_leaf" %in% cols) dbExecute(con, "ALTER TABLE financial_elements ADD COLUMN is_leaf INTEGER DEFAULT 0")
  
  has_root <- dbGetQuery(con, "SELECT COUNT(*) as n FROM financial_elements WHERE id = 'root'")$n
  if (has_root == 0) {
    dbExecute(con, "INSERT INTO financial_elements (id, parent_id, element_type, title, chance, is_leaf) VALUES ('root', NULL, 'Cost', ?, 100, 0)", params = list(proj_name))
    dbExecute(con, "UPDATE financial_elements SET parent_id = 'root' WHERE (parent_id IS NULL OR parent_id = '') AND id != 'root'")
  }
  dbDisconnect(con)
  
  rv$db_path <- init_path
  get_db <- function() { dbConnect(RSQLite::SQLite(), rv$db_path) }
  
  trigger_refresh <- reactiveVal(0)
  selected_node_id <- reactiveVal(NULL)
  
  observeEvent(input$use_seed, {
    if (input$use_seed) shinyjs::enable("seed_val") else shinyjs::disable("seed_val")
  })
  
  output$wbs_tree <- renderTree({
    trigger_refresh()
    con <- get_db()
    df <- dbGetQuery(con, "SELECT * FROM financial_elements")
    dbDisconnect(con)
    
    if (nrow(df) == 0) return(list("Error: Master node missing" = ""))
    build_nested_tree(df, focus_id = rv$focus_node_id)
  })
  
  # --- TREE SELECTION LOGIC ---
  observeEvent(input$wbs_tree, {
    sel <- get_selected(input$wbs_tree, format = "names")
    
    if (length(sel) > 0) {
      path <- sel[[1]]
      node_name <- path[length(path)]
      
      if (!is.null(node_name)) {
        con <- get_db()
        node_data <- dbGetQuery(con, "SELECT * FROM financial_elements WHERE title = ?", params = list(node_name))
        
        if (nrow(node_data) > 0) {
          selected_node_id(node_data$id[1])
          
          if (!is.null(rv$focus_node_id) && node_data$id[1] == rv$focus_node_id) {
             rv$focus_node_id <- NULL # Clear focus trigger after successful selection
          }
          
          if (node_data$element_type[1] %in% c("Cost", "Benefit")) {
            shinyjs::hide("prob_container")
          } else {
            shinyjs::show("prob_container")
          }
          
          child_count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM financial_elements WHERE parent_id = ?", params = list(node_data$id[1]))$n
          is_leaf_flag <- ifelse(is.na(node_data$is_leaf[1]), 0, node_data$is_leaf[1])
          
          # Sync UI and track internal state to prevent false-positive observers
          rv$current_leaf_state <- as.logical(is_leaf_flag)
          
          if (child_count > 0) {
            updateCheckboxInput(session, "is_leaf_check", value = FALSE)
            shinyjs::disable("is_leaf_check")
            shinyjs::disable("est_fieldset")
          } else {
            shinyjs::enable("is_leaf_check")
            updateCheckboxInput(session, "is_leaf_check", value = as.logical(is_leaf_flag))
            if (is_leaf_flag == 1) shinyjs::enable("est_fieldset") else shinyjs::disable("est_fieldset")
          }
          
          updateAutonumericInput(session, "est_opt", value = ifelse(is.na(node_data$opt_val[1]), "", node_data$opt_val[1]))
          updateAutonumericInput(session, "est_likely", value = ifelse(is.na(node_data$likely_val[1]), "", node_data$likely_val[1]))
          updateAutonumericInput(session, "est_pess", value = ifelse(is.na(node_data$pess_val[1]), "", node_data$pess_val[1]))
          updateNumericInput(session, "est_chance", value = ifelse(is.na(node_data$chance[1]), 100, node_data$chance[1]))
        }
        dbDisconnect(con)
      }
    } else {
      selected_node_id(NULL)
      rv$current_leaf_state <- FALSE
      updateCheckboxInput(session, "is_leaf_check", value = FALSE)
      shinyjs::disable("is_leaf_check")
      shinyjs::disable("est_fieldset")
    }
  })
  
  observeEvent(input$is_leaf_check, {
    req(selected_node_id())
    ui_is_leaf <- as.logical(input$is_leaf_check)
    
    # Guards against programmatic updates triggering the modal
    if (ui_is_leaf == rv$current_leaf_state) return() 
    rv$current_leaf_state <- ui_is_leaf 
    
    if (ui_is_leaf) {
      con <- get_db()
      dbExecute(con, "UPDATE financial_elements SET is_leaf = 1 WHERE id = ?", params = list(selected_node_id()))
      dbDisconnect(con)
      shinyjs::enable("est_fieldset")
    } else {
      showModal(modalDialog(
        title = "Remove Leaf Status?",
        "Unchecking this will permanently clear all estimates for this node. Are you sure?",
        footer = tagList(
          actionButton("confirm_uncheck", "Yes, clear estimates", class = "btn-danger"),
          actionButton("cancel_uncheck", "Cancel")
        )
      ))
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
    dbExecute(con, "UPDATE financial_elements SET is_leaf = 0, opt_val = NULL, likely_val = NULL, pess_val = NULL, chance = 100 WHERE id = ?", params = list(selected_node_id()))
    dbDisconnect(con)
    
    updateAutonumericInput(session, "est_opt", value = "")
    updateAutonumericInput(session, "est_likely", value = "")
    updateAutonumericInput(session, "est_pess", value = "")
    updateNumericInput(session, "est_chance", value = 100)
    shinyjs::disable("est_fieldset")
  })
  
  output$header_title <- renderText({
    if (is.null(selected_node_id())) return("Node Estimate")
    con <- get_db()
    title <- dbGetQuery(con, "SELECT title FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$title
    dbDisconnect(con)
    paste("Node Estimate:", ifelse(title == "", "(Blank Node)", title))
  })
  
  output$meta_id <- renderText({ if (is.null(selected_node_id())) "N/A" else selected_node_id() })
  output$meta_type <- renderText({ 
    if (is.null(selected_node_id())) return("N/A")
    con <- get_db()
    type <- dbGetQuery(con, "SELECT element_type FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$element_type
    dbDisconnect(con)
    type
  })
  
  observeEvent(input$btn_save_est, {
    req(selected_node_id())
    con <- get_db()
    
    val_opt <- if (is.null(input$est_opt) || input$est_opt == "") NA else as.numeric(input$est_opt)
    val_lik <- if (is.null(input$est_likely) || input$est_likely == "") NA else as.numeric(input$est_likely)
    val_pes <- if (is.null(input$est_pess) || input$est_pess == "") NA else as.numeric(input$est_pess)
    
    dbExecute(con, "UPDATE financial_elements SET opt_val=?, likely_val=?, pess_val=?, chance=? WHERE id=?", 
              params = list(val_opt, val_lik, val_pes, input$est_chance, selected_node_id()))
    dbDisconnect(con)
    showNotification("Estimates Saved to Database", type = "message")
  })
  
  observeEvent(input$btn_edit_root, {
    con <- get_db()
    root_title <- dbGetQuery(con, "SELECT title FROM financial_elements WHERE id = 'root'")$title
    dbDisconnect(con)
    
    showModal(modalDialog(
      title = "Edit Project Name",
      textInput("root_name_input", "Project Name (Database Name)", value = root_title),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_root_name", "Save", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$save_root_name, {
    new_title <- trimws(input$root_name_input)
    if (new_title == "") { showNotification("Project name cannot be blank.", type = "error"); return() }
    
    safe_filename <- gsub("[/\\\\:*?\"<>|]", "_", new_title)
    new_db_path <- paste0(safe_filename, ".sqlite")
    
    if (new_db_path != rv$db_path && file.exists(new_db_path)) {
      showNotification("A project file with this name already exists.", type = "error")
      return()
    }
    
    con <- get_db()
    dbExecute(con, "UPDATE financial_elements SET title = ? WHERE id = 'root'", params = list(new_title))
    dbDisconnect(con)
    
    if (new_db_path != rv$db_path) {
      file.rename(rv$db_path, new_db_path)
      rv$db_path <- new_db_path
    }
    removeModal()
    trigger_refresh(trigger_refresh() + 1)
    showNotification("Project name updated.", type = "message")
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
  
  show_add_modal <- function(default_type = "Cost") {
    showModal(modalDialog(
      title = "Add Child Node",
      textInput("new_title", "Title (Must be unique and not blank)"),
      selectInput("new_type", "Element Type", choices = c("Cost", "Risk", "Issue", "Benefit"), selected = default_type),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_child", "Save to DB", class = "btn-success")
      )
    ))
  }
  
  observeEvent(input$btn_add_node, { 
    if (is.null(selected_node_id())) {
      showNotification("Please select a parent node first.", type = "error")
    } else {
      con <- get_db()
      node_data <- dbGetQuery(con, "SELECT element_type, is_leaf FROM financial_elements WHERE id = ?", params = list(selected_node_id()))
      dbDisconnect(con)
      
      if (ifelse(is.na(node_data$is_leaf[1]), 0, node_data$is_leaf[1]) == 1) {
        showModal(modalDialog(
          title = "Convert to Parent Node?",
          "This node is marked as a Leaf Node. Adding a child will remove Leaf status and clear estimates. Continue?",
          footer = tagList(
            actionButton("confirm_add_child_clear", "Yes, add child & clear estimates", class = "btn-danger"),
            actionButton("cancel_add_child", "Cancel")
          )
        ))
      } else { show_add_modal(default_type = node_data$element_type[1]) }
    }
  })
  
  observeEvent(input$cancel_add_child, { removeModal() })
  
  observeEvent(input$confirm_add_child_clear, {
    removeModal()
    con <- get_db()
    dbExecute(con, "UPDATE financial_elements SET is_leaf = 0, opt_val = NULL, likely_val = NULL, pess_val = NULL, chance = 100 WHERE id = ?", params = list(selected_node_id()))
    parent_type <- dbGetQuery(con, "SELECT element_type FROM financial_elements WHERE id = ?", params = list(selected_node_id()))$element_type
    dbDisconnect(con)
    
    rv$current_leaf_state <- FALSE
    updateCheckboxInput(session, "is_leaf_check", value = FALSE)
    shinyjs::disable("est_fieldset")
    updateAutonumericInput(session, "est_opt", value = "")
    updateAutonumericInput(session, "est_likely", value = "")
    updateAutonumericInput(session, "est_pess", value = "")
    updateNumericInput(session, "est_chance", value = 100)
    
    show_add_modal(default_type = parent_type)
  })
  
  observeEvent(input$save_child, {
    if (trimws(input$new_title) == "") { showNotification("Title cannot be blank.", type = "error"); return() }
    con <- get_db()
    new_id <- generate_id()
    dbExecute(con, "INSERT INTO financial_elements (id, parent_id, element_type, title, chance, is_leaf) VALUES (?, ?, ?, ?, ?, ?)",
              params = list(new_id, selected_node_id(), input$new_type, trimws(input$new_title), 100, 0))
    dbDisconnect(con)
    removeModal()
    
    rv$focus_node_id <- new_id
    trigger_refresh(trigger_refresh() + 1)
  })
  
  # --- MONTE CARLO ENGINE ---
  observeEvent(input$run_mc, {
    con <- get_db()
    df <- dbGetQuery(con, "SELECT * FROM financial_elements WHERE opt_val IS NOT NULL")
    dbDisconnect(con)
    
    if(nrow(df) == 0) { showNotification("No nodes with estimates found.", type = "warning"); return() }
    if (input$use_seed && !is.na(input$seed_val)) set.seed(input$seed_val) else set.seed(NULL)
    
    n_iter <- input$iterations
    total_exposure <- numeric(n_iter)
    
    for (i in 1:nrow(df)) {
      raw_vals <- c(df$opt_val[i], df$likely_val[i], df$pess_val[i])
      calc_min <- min(raw_vals, na.rm = TRUE)
      calc_max <- max(raw_vals, na.rm = TRUE)
      calc_likely <- median(raw_vals, na.rm = TRUE) 
      prob_decimal <- ifelse(is.na(df$chance[i]), 1, df$chance[i] / 100)
      multiplier <- ifelse(df$element_type[i] == "Benefit", -1, 1)
      
      severity_sim <- rpert(n_iter, calc_min, calc_likely, calc_max)
      occurrence_sim <- rbinom(n_iter, size = 1, prob = prob_decimal)
      total_exposure <- total_exposure + (severity_sim * occurrence_sim * multiplier)
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
  
  # --- CSV EXPORT HANDLER ---
  output$btn_export <- downloadHandler(
    filename = function() { paste("wbs-risk-report-", format(Sys.time(), "%Y%m%d-%H%M"), ".csv", sep="") },
    content = function(file) {
      con <- get_db()
      df <- dbGetQuery(con, "SELECT * FROM financial_elements")
      dbDisconnect(con)
      
      seed_to_use <- if (input$use_seed && !is.na(input$seed_val)) input$seed_val else NULL
      out_df <- generate_report(df, n_iter = input$iterations, seed_val = seed_to_use)
      write.csv(out_df, file, row.names = FALSE, na = "")
    }
  )
}

runApp(shinyApp(ui = ui, server = server), port = 3296, launch.browser = FALSE)