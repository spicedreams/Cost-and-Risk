# ==========================================
# ui.R
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
      uiOutput("db_controls_ui"),
      div(style = "margin-top: 10px; margin-bottom: 15px; font-weight: bold; word-wrap: break-word;", textOutput("current_db_display")),
      actionButton("btn_import_csv", "Import CSV", icon = icon("file-csv"), class = "btn-outline-success btn-sm mb-2", style = "width: 100%;"),
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
            column(6, div(id = "treatment_active_container", checkboxInput("chk_active_treatment", "Treatment is Selected (Rolls up cost & Residual replaces Risk)", value = FALSE)))
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