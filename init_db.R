library(DBI)
library(RPostgres)

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = "postgres", # Or your specific DB name
  host = "db.ifujwshgwkodxrqzgyhg.supabase.co",
  user = "postgres",
  password = "Vb2Tu6M8H$z?wUh",
  port = 5432
)

# Create tables using PostgreSQL syntax (already used in your app.R)
dbExecute(conn, "CREATE TABLE IF NOT EXISTS tasks (...)") 
dbDisconnect(conn)