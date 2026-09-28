
# ==========================================
# global.R
# ==========================================
req_pkgs <- c("shiny", "bslib", "shinyTree", "DBI", "RSQLite", "ggplot2", "shinyjs", "shinyWidgets", "DT", "shinyFiles")
missing_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[,"Package"])]

if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org/")
}

invisible(lapply(req_pkgs, library, character.only = TRUE))

# Load helper functions and business logic
source("utils.R")
source("impexp.R")