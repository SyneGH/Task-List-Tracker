library(DBI)
library(RMariaDB)

# 1. Connect to Database
# (Ensure these match your actual credentials!)
conn <- dbConnect(
  MariaDB(),
  dbname = "task_list_tracker_system",
  host = "127.0.0.1",
  port = 3306,
  user = "root",
  password = "" 
)

print("Connected to database...")

# 2. Drop existing tables (Clean Slate)
tryCatch({
  dbExecute(conn, "DROP TABLE IF EXISTS tasks")
  print("Dropped 'tasks' table.")
}, error = function(e) {
  print(paste("Error dropping table:", e$message))
})

# 3. Create the table with the CORRECT columns (including updated_at)
sql <- "
CREATE TABLE tasks (
  id SERIAL PRIMARY KEY,
  title VARCHAR(255) NOT NULL,
  description TEXT,
  priority ENUM('LOW','MEDIUM','HIGH','CRITICAL') NOT NULL DEFAULT 'LOW',
  status ENUM('TODO','IN_PROGRESS','OVERDUE','COMPLETED') NOT NULL DEFAULT 'TODO',
  start_datetime TIMESTAMP NULL,
  due_datetime TIMESTAMP NULL,
  completed_at TIMESTAMP NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
)"

tryCatch({
  dbExecute(conn, sql)
  print("SUCCESS: 'tasks' table created with new schema!")
}, error = function(e) {
  print(paste("FAILED to create table:", e$message))
})

# 4. Add a dummy task (Optional, just to verify)
tryCatch({
  dbExecute(conn, "
    INSERT INTO tasks (title, description, priority, status) 
    VALUES ('Test Task', 'This confirms DB is working', 'MEDIUM', 'TODO')
  ")
  print("Inserted test task.")
}, error = function(e) {
  print(paste("Error inserting test task:", e$message))
})

dbDisconnect(conn)