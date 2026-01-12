library(DBI)
library(RPostgres)

# Connect using Environment Variables
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = as.numeric(Sys.getenv("DB_PORT")),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode  = "require"
)

print("Running Overdue Check...")

tryCatch({
  # 1. Update statuses to OVERDUE
  dbExecute(conn, "
    UPDATE tasks 
    SET status = 'OVERDUE' 
    WHERE due_datetime < NOW() AT TIME ZONE 'UTC' 
    AND status IN ('TODO', 'IN_PROGRESS')
  ")

  # 2. Insert notifications for newly overdue tasks
  dbExecute(conn, "
    INSERT INTO notifications (task_id, message)
    SELECT id, CONCAT('⚠️ Task \"', title, '\" is now overdue')
    FROM tasks
    WHERE status = 'OVERDUE'
    AND id NOT IN (SELECT task_id FROM notifications WHERE message LIKE '%now overdue%')
  ")
  
  print("Database successfully updated.")
}, error = function(e) {
  print(paste("Error during update:", e$message))
})

dbDisconnect(conn)