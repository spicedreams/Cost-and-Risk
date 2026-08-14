# --- standalone_migration.R ---
library(DBI)
library(RSQLite)

# 1. Define the path to your existing SQLite database
db_file <- "cost-risk-2026-08-14.sqlite" # Update this to your actual file path

# 2. Connect to the database
conn <- dbConnect(RSQLite::SQLite(), db_file)

# 3. Perform the migration
tryCatch({
  # Update all 'Residual' types to 'Risk'
  rows_affected <- dbExecute(conn, "UPDATE financial_elements SET element_type = 'Risk' WHERE element_type = 'Residual'")
  
  message(sprintf("Migration successful: %d 'Residual' node(s) converted to 'Risk'.", rows_affected))
  
}, error = function(e) {
  message("An error occurred during migration: ", e$message)
})

# 4. Disconnect
dbDisconnect(conn)