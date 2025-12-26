# cron_job.R
library(DBI)
library(RMariaDB)

# ... (Include your db_connect function here or source('app.R')) ...
source("app.R") # Assuming app.R has your db_connect and backend functions

conn <- db_connect()
print("Starting Cron Job: Checking Overdue Tasks...")

tryCatch({
  backend_check_overdue(conn)
  print("Success: Database updated.")
}, error = function(e) {
  print(paste("Error:", e$message))
})

dbDisconnect(conn)