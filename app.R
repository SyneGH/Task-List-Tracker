# ============================================================================
# Task List Tracker - Frontend UI Only with Drag & Drop and Dark Mode
# ============================================================================
# Save this file as: app.R
# 
# Dependencies (install before running):
# install.packages(c("shiny", "bslib", "shinyWidgets", "ggplot2", "dplyr",
#                    "htmltools", "shinyjs", "sortable", "DBI", "RMariaDB"))
# ============================================================================

library(shiny)
library(bslib)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(htmltools)
library(shinyjs)
library(sortable)
library(DBI)
# library(RMariaDB)
library(RPostgres)
library(sodium)

DEFAULT_CREDENTIALS <- list(
  username = "admin",
  password_hash = sodium::password_store("admin123")
)

# =============================================================================
# DATABASE HELPERS
# =============================================================================

db_connect <- function() {
  # Check if we are running locally or on Render
  # Render always sets the 'RENDER' env var to 'true'
  is_local <- Sys.getenv("RENDER") == ""
  
  db_name <- Sys.getenv("DB_NAME", "task_tracker")
  db_host <- Sys.getenv("DB_HOST", "127.0.0.1") # Default to localhost
  db_port <- as.numeric(Sys.getenv("DB_PORT", "5432"))
  db_user <- Sys.getenv("DB_USER", "postgres")   # Default local superuser
  db_pass <- Sys.getenv("DB_PASSWORD", "admin123") # Default local password
  
  # Local Postgres usually doesn't have SSL set up, but Render REQUIRES it.
  # "prefer" tries SSL if available, but falls back to plain text (perfect for hybrid).
  ssl_mode <- if (is_local) "prefer" else "require"

  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname = db_name,
    host = db_host,
    port = db_port,
    user = db_user,
    password = db_pass,
    sslmode = ssl_mode 
  )
  
  # Postgres handles timezones differently.
  dbExecute(conn, "SET TIME ZONE 'Asia/Manila'") 
  conn
}

initialize_db <- function(conn) {
  # Drop tables if you need a clean slate (Be careful in production!)
  # dbExecute(conn, "DROP TABLE IF EXISTS notifications")
  # dbExecute(conn, "DROP TABLE IF EXISTS tasks")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS tasks (
      id SERIAL PRIMARY KEY,
      title VARCHAR(255) NOT NULL,
      description TEXT,
      priority TEXT NOT NULL DEFAULT 'LOW' CHECK (priority IN ('LOW','MEDIUM','HIGH','CRITICAL')),
      status TEXT NOT NULL DEFAULT 'TODO' CHECK (status IN ('TODO','IN_PROGRESS','OVERDUE','COMPLETED')),
      start_datetime TIMESTAMP NULL,
      due_datetime TIMESTAMP NULL,
      completed_at TIMESTAMP NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS notifications (
      id SERIAL PRIMARY KEY,
      task_id INT,
      message TEXT NOT NULL,
      is_read BOOLEAN DEFAULT FALSE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (task_id) REFERENCES tasks(id) ON DELETE CASCADE
    )
  ")
}

format_db_time <- function(value) {
  if (is.null(value) || is.na(value)) return(NA)
  format(as.POSIXct(value, tz = "UTC"), "%Y-%m-%d %H:%M:%S")
}

backend_fetch_kanban <- function(conn) {
  tasks <- dbGetQuery(conn, "SELECT id, title, description, priority, status, start_datetime, due_datetime, completed_at FROM tasks ORDER BY id")

  if (nrow(tasks) == 0) {
    tasks <- data.frame(
      id = integer(),
      title = character(),
      description = character(),
      priority = character(),
      status = character(),
      start_datetime = as.POSIXct(character()),
      due_datetime = as.POSIXct(character()),
      completed_at = as.POSIXct(character()),
      stringsAsFactors = FALSE
    )
  } else {
    tasks$start_datetime <- lubridate::with_tz(as.POSIXct(tasks$start_datetime, tz = "UTC"), "Asia/Manila")
    tasks$due_datetime   <- lubridate::with_tz(as.POSIXct(tasks$due_datetime, tz = "UTC"), "Asia/Manila")
    tasks$completed_at   <- lubridate::with_tz(as.POSIXct(tasks$completed_at, tz = "UTC"), "Asia/Manila")
  }

  tasks$overdue_days <- ifelse(
    !is.na(tasks$due_datetime),
    pmax(0, round(as.numeric(difftime(Sys.time(), tasks$due_datetime, units = "days")))),
    0
  )

  tasks$ui_column <- ifelse(tasks$status == "TODO", "TODO",
                            ifelse(tasks$status == "IN_PROGRESS", "IN_PROGRESS",
                                   ifelse(tasks$status == "OVERDUE", "OVERDUE", "COMPLETED")))

  tasks$display_text <- ifelse(
    is.na(tasks$due_datetime),
    "No due date",
    paste0(
      format(tasks$due_datetime, "%b %d, %I:%M %p"),
      ifelse(tasks$overdue_days > 0, paste0(" (", tasks$overdue_days, "d overdue)"), "")
    )
  )

  return(tasks)
}

backend_get_task <- function(conn, task_id) {
  task <- dbGetQuery(
    conn,
    "SELECT id, title, description, priority, status, start_datetime, due_datetime, completed_at FROM tasks WHERE id = $1 LIMIT 1",
    params = list(task_id)
  )

  # ... rest of function remains the same ...
  if (nrow(task) > 0) {
    task$start_datetime <- as.POSIXct(task$start_datetime, tz = "UTC")
    task$due_datetime <- as.POSIXct(task$due_datetime, tz = "UTC")
    task$completed_at <- as.POSIXct(task$completed_at, tz = "UTC")
  }
  task
}

backend_create_task <- function(conn, task) {
  # 1. Prepare SQL with $n placeholders and RETURNING id
  sql <- "
    INSERT INTO tasks (title, description, priority, status, start_datetime, due_datetime) 
    VALUES ($1, $2, $3, $4, $5, $6)
    RETURNING id
  "
  
  # 2. Execute and get the ID in one step
  # Note: dbGetQuery is used here because we expect a return value (the ID)
  result <- dbGetQuery(conn, sql, params = list(
    task$title,
    task$description,
    task$priority,
    task$status,
    format_db_time(task$start_datetime),
    format_db_time(task$due_datetime)
  ))
  
  new_id <- result$id
  
  # 3. Return the full task
  backend_get_task(conn, new_id)
}

backend_update_task <- function(conn, task_id, updates) {
  if (length(updates) == 0) return(NULL)

  updates$updated_at <- Sys.time()
  update_names <- names(updates)
  
  # FIX: Create $1, $2, $3 placeholders dynamically
  # example: "title = $1, status = $2"
  placeholders <- paste0(update_names, " = $", seq_along(update_names))
  set_clause <- paste(placeholders, collapse = ", ")
  
  # The WHERE clause needs the next available number (e.g., $3)
  id_placeholder <- paste0("$", length(updates) + 1)

  params <- lapply(seq_along(updates), function(i) {
    name <- update_names[i]
    value <- updates[[i]]
    if (name %in% c("start_datetime", "due_datetime", "completed_at", "updated_at")) {
      return(format_db_time(value))
    }
    value
  })

  params <- c(params, list(task_id))

  dbExecute(
    conn,
    paste0("UPDATE tasks SET ", set_clause, " WHERE id = ", id_placeholder),
    params = params
  )

  backend_get_task(conn, task_id)
}

backend_move_task <- function(conn, task_id, new_status) {
  backend_update_task(conn, task_id, list(status = new_status))
}

backend_mark_completed <- function(conn, task_id) {
  backend_update_task(conn, task_id, list(
    status = "COMPLETED",
    completed_at = Sys.time()
  ))
}

backend_fetch_notifications <- function(conn) {
  res <- dbGetQuery(conn, "
    SELECT id, message, created_at 
    FROM notifications 
    WHERE is_read = FALSE 
    ORDER BY created_at DESC
  ")
  
  if (nrow(res) > 0) {
    return(res$message)
  } else {
    return(character(0))
  }
}

backend_clear_notifications <- function(conn) {
  dbExecute(conn, "UPDATE notifications SET is_read = TRUE WHERE is_read = FALSE")
}

backend_check_overdue <- function(conn) {
  
  # 1. WARNING: 24 HOURS BEFORE (Range: 6h to 24h)
  # Prevents triggering if the task is already super urgent (e.g. due in 1 hour)
  dbExecute(conn, "
    INSERT INTO notifications (task_id, message)
    SELECT id, CONCAT('⏳ Task \"', title, '\" is due in 24 hours')
    FROM tasks
    WHERE status IN ('TODO', 'IN_PROGRESS')
    AND due_datetime > NOW() AT TIME ZONE 'UTC' + INTERVAL '6 hours'
    AND due_datetime <= NOW() AT TIME ZONE 'UTC' + INTERVAL '1 day'
    AND id NOT IN (SELECT task_id FROM notifications WHERE message LIKE '%due in 24 hours%')
  ")

  # 2. WARNING: 6 HOURS BEFORE (Range: 30m to 6h)
  dbExecute(conn, "
    INSERT INTO notifications (task_id, message)
    SELECT id, CONCAT('⏰ Task \"', title, '\" is due in 6 hours')
    FROM tasks
    WHERE status IN ('TODO', 'IN_PROGRESS')
    AND due_datetime > NOW() AT TIME ZONE 'UTC' + INTERVAL '30 minutes'
    AND due_datetime <= NOW() AT TIME ZONE 'UTC' + INTERVAL '6 hours'
    AND id NOT IN (SELECT task_id FROM notifications WHERE message LIKE '%due in 6 hours%')
  ")

  # 3. WARNING: 30 MINUTES BEFORE (Range: Now to 30m)
  dbExecute(conn, "
    INSERT INTO notifications (task_id, message)
    SELECT id, CONCAT('🔥 Task \"', title, '\" is due in < 30 minutes!')
    FROM tasks
    WHERE status IN ('TODO', 'IN_PROGRESS')
    AND due_datetime > NOW() AT TIME ZONE 'UTC'
    AND due_datetime <= NOW() AT TIME ZONE 'UTC' + INTERVAL '30 minutes'
    AND id NOT IN (SELECT task_id FROM notifications WHERE message LIKE '%due in < 30 minutes%')
  ")

  # 4. ACTION: MARK OVERDUE (0 Seconds)
  # First, update the status
  dbExecute(conn, "
    UPDATE tasks 
    SET status = 'OVERDUE' 
    WHERE due_datetime < NOW() AT TIME ZONE 'UTC' 
    AND status IN ('TODO', 'IN_PROGRESS')
  ")

  # Second, send the notification for the status change
  dbExecute(conn, "
    INSERT INTO notifications (task_id, message)
    SELECT id, CONCAT('⚠️ Task \"', title, '\" is now overdue')
    FROM tasks
    WHERE status = 'OVERDUE'
    AND id NOT IN (SELECT task_id FROM notifications WHERE message LIKE '%now overdue%')
  ")
}

backend_delete_task <- function(conn, task_id) {
  # Cascading delete handles notifications automatically via SQL schema
  dbExecute(conn, "DELETE FROM tasks WHERE id = $1", params = list(task_id))
}

# =============================================================================
# UI COMPONENTS
# =============================================================================

create_task_card <- function(task, ns) {
  priority_colors <- list(
    LOW = list(bg = "#e3fcef", text = "#006644", border = "#00875a"),
    MEDIUM = list(bg = "#fff4e6", text = "#974f0c", border = "#ff991f"),
    HIGH = list(bg = "#ffebe6", text = "#bf2600", border = "#ff5630"),
    CRITICAL = list(bg = "#ffebe6", text = "#de350b", border = "#de350b")
  )
  
  p_style <- priority_colors[[task$priority]]
  if (is.null(p_style)) p_style <- priority_colors$LOW

  div(
    class = "task-card sortable-item",
    `data-task-id` = task$id,
    id = paste0("task_", task$id),
    
    # Drag Handle
    div(class = "drag-handle", icon("grip-vertical")),
    
    div(
      style = "margin-bottom: 8px; padding-right: 20px;",
      div(class = "task-title", task$title),
      span(
        class = "priority-badge",
        style = sprintf("background: %s; color: %s; border-color: %s;",
                        p_style$bg, p_style$text, p_style$border),
        task$priority
      )
    ),
    
    if (!is.na(task$description) && task$description != "") {
      div(class = "task-description", task$description)
    },
    
    div(
      class = "task-meta",
      icon("clock"),
      span(task$display_text)
    ),
    
    div(
      class = "task-actions",
      # 1. Edit Button
      tags$button(
        class = "btn btn-task-action",
        type = "button",
        onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_edit_task"), task$id),
        "Edit"
      ),
      # 2. Complete Button (Only if not completed)
      if (task$status != "COMPLETED") {
        tags$button(
          class = "btn btn-task-complete",
          type = "button",
          onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_complete_task"), task$id),
          HTML('<i class="fa fa-check"></i>')
        )
      },
      # 3. Delete Button
      tags$button(
        class = "btn btn-task-delete",
        type = "button",
        title = "Delete Task",
        onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('%s', %d, {priority: 'event'});", ns("trigger_delete_task"), task$id),
        icon("trash")
      )
    )
  )
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- page_fillable(
  useShinyjs(),
  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#0052cc",
    secondary = "#5e6c84",
    success = "#00875a",
    info = "#0065ff",
    warning = "#ff991f",
    danger = "#de350b",
    base_font = font_google("Inter"),
    bg = "#f4f5f7",
    fg = "#172b4d"
  ),
  
  tags$head(
    tags$style(HTML("
      /* === THEME VARIABLES === */
      :root {
        --bg-primary: #f4f5f7;
        --bg-secondary: #ffffff;
        --bg-tertiary: #fafbfc;
        --text-primary: #172b4d;
        --text-secondary: #5e6c84;
        --text-tertiary: #8993a4;
        --border-color: #dfe1e6;
        --shadow-sm: 0 1px 2px rgba(0,0,0,0.08);
        --shadow-md: 0 2px 6px rgba(0,0,0,0.08);
        --shadow-lg: 0 4px 12px rgba(0,0,0,0.12);
        --accent: #0052cc;
        --accent-hover: #0747a6;
      }
      
      [data-theme='dark'] {
        --bg-primary: #1a1d23;
        --bg-secondary: #22272b;
        --bg-tertiary: #2c333a;
        --text-primary: #e4e6eb;
        --text-secondary: #b6c2cf;
        --text-tertiary: #8993a4;
        --border-color: #38414a;
        --shadow-sm: 0 1px 2px rgba(0,0,0,0.3);
        --shadow-md: 0 2px 6px rgba(0,0,0,0.4);
        --shadow-lg: 0 4px 12px rgba(0,0,0,0.5);
        --accent: #4c9aff;
        --accent-hover: #2684ff;
      }
      
      /* === GLOBAL STYLES === */
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      
      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        background: var(--bg-primary);
        color: var(--text-primary);
        transition: background-color 0.3s, color 0.3s;
      }
      
      /* === ANIMATIONS === */
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .fade-in {
        animation: fadeIn 0.4s ease-out;
      }
      
      /* === LOGIN PAGE === */
      .login-container {
        max-width: 440px;
        margin: 80px auto;
        animation: fadeIn 0.6s ease-out;
      }
      
      .login-card {
        background: var(--bg-secondary);
        border-radius: 16px;
        box-shadow: var(--shadow-lg);
        padding: 48px 40px;
        border: 1px solid var(--border-color);
      }
      
      .login-header {
        text-align: center;
        margin-bottom: 32px;
      }
      
      .login-icon {
        font-size: 56px;
        color: var(--accent);
        margin-bottom: 16px;
      }
      
      .login-title {
        font-size: 28px;
        font-weight: 700;
        color: var(--text-primary);
        margin: 0;
      }
      
      .login-subtitle {
        font-size: 15px;
        color: var(--text-secondary);
        margin-top: 8px;
      }
      
      .form-control, .form-select {
        border: 2px solid var(--border-color);
        border-radius: 8px;
        padding: 12px 16px;
        font-size: 15px;
        transition: all 0.2s;
        background: var(--bg-secondary);
        color: var(--text-primary);
      }
      
      .form-control:focus, .form-select:focus {
        border-color: var(--accent);
        box-shadow: 0 0 0 3px rgba(76, 154, 255, 0.1);
        outline: none;
        background: var(--bg-secondary);
      }
      
      .form-label {
        font-weight: 600;
        color: var(--text-primary);
        margin-bottom: 8px;
        font-size: 14px;
      }
      
      .btn-primary {
        background: var(--accent);
        border: none;
        border-radius: 8px;
        padding: 12px 24px;
        font-size: 15px;
        font-weight: 600;
        transition: all 0.2s;
        box-shadow: 0 2px 4px rgba(0,82,204,0.2);
        color: white;
      }
      
      .btn-primary:hover {
        background: var(--accent-hover);
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(0,82,204,0.3);
        color: white;
      }
      
      /* === NAVBAR === */
      .app-navbar {
        background: var(--bg-secondary);
        border-bottom: 1px solid var(--border-color);
        box-shadow: var(--shadow-sm);
        padding: 16px 24px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      .navbar-left {
        display: flex;
        align-items: center;
        gap: 24px;
      }
      
      .navbar-brand {
        font-size: 22px;
        font-weight: 700;
        color: var(--text-primary);
        display: flex;
        align-items: center;
        gap: 12px;
      }
      
      .navbar-brand i {
        color: var(--accent);
        font-size: 24px;
      }
      
      .nav-pills {
        display: flex;
        gap: 8px;
        background: var(--bg-tertiary);
        padding: 4px;
        border-radius: 10px;
      }
      
      .nav-link {
        color: var(--text-secondary);
        font-weight: 500;
        border-radius: 8px;
        padding: 10px 20px;
        transition: all 0.2s;
        border: none;
        background: transparent;
      }
      
      .nav-link:hover {
        background: var(--bg-secondary);
        color: var(--text-primary);
      }
      
      .nav-link.active {
        background: var(--accent);
        color: white !important;
        box-shadow: 0 2px 6px rgba(76, 154, 255, 0.3);
      }
      
      .hamburger-btn {
        background: transparent;
        border: none;
        color: var(--text-primary);
        font-size: 24px;
        cursor: pointer;
        padding: 8px;
        border-radius: 8px;
        transition: all 0.2s;
      }
      
      .hamburger-btn:hover {
        background: var(--bg-tertiary);
      }
      
      /* === NAVBAR RIGHT === */
      .navbar-right {
        display: flex;
        align-items: center;
        gap: 16px;
      }

      .nav-icon-btn {
        background: transparent;
        border: none;
        color: var(--text-secondary);
        font-size: 20px;
        padding: 8px;
        border-radius: 50%;
        cursor: pointer;
        transition: all 0.2s;
        position: relative; /* For badge positioning */
      }

      .nav-icon-btn:hover {
        background: var(--bg-tertiary);
        color: var(--text-primary);
      }

      /* Notification Badge (Red Dot) */
      .notif-badge {
        position: absolute;
        top: 4px;
        right: 4px;
        width: 10px;
        height: 10px;
        background: #de350b;
        border-radius: 50%;
        border: 2px solid var(--bg-secondary);
      }

      /* Popover/Dropdown Styling */
      .popover {
        border: 1px solid var(--border-color);
        box-shadow: var(--shadow-lg);
        background: var(--bg-secondary);
        max-width: 350px;
        z-index: 1060;
      }
      
      .popover-header {
        background: var(--bg-secondary);
        border-bottom: 1px solid var(--border-color);
        color: var(--text-primary);
        font-weight: 700;
      }
      
      .popover-body {
        padding: 0;
        background: var(--bg-secondary);
        color: var(--text-primary);
      }

      /* === SIDEBAR MENU === */
      .sidebar-overlay {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: rgba(0,0,0,0.5);
        z-index: 999;
        opacity: 0;
        pointer-events: none;
        transition: opacity 0.3s;
      }
      
      .sidebar-overlay.active {
        opacity: 1;
        pointer-events: all;
      }
      
      .sidebar-menu {
        position: fixed;
        top: 0;
        right: -400px;
        width: 400px;
        height: 100vh;
        background: var(--bg-secondary);
        box-shadow: -4px 0 20px rgba(0,0,0,0.2);
        z-index: 1000;
        transition: right 0.3s ease;
        overflow-y: auto;
      }
      
      .sidebar-menu.active {
        right: 0;
      }
      
      .sidebar-header {
        padding: 24px;
        border-bottom: 1px solid var(--border-color);
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      .sidebar-title {
        font-size: 20px;
        font-weight: 700;
        color: var(--text-primary);
      }
      
      .sidebar-close {
        background: transparent;
        border: none;
        color: var(--text-secondary);
        font-size: 24px;
        cursor: pointer;
        padding: 8px;
        border-radius: 8px;
        transition: all 0.2s;
      }
      
      .sidebar-close:hover {
        background: var(--bg-tertiary);
        color: var(--text-primary);
      }
      
      .sidebar-section {
        padding: 24px;
        border-bottom: 1px solid var(--border-color);
      }
      
      .section-title {
        font-size: 14px;
        font-weight: 700;
        color: var(--text-secondary);
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 16px;
      }
      
      .theme-toggle {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 12px 16px;
        background: var(--bg-tertiary);
        border-radius: 8px;
        cursor: pointer;
        transition: all 0.2s;
      }
      
      .theme-toggle:hover {
        background: var(--bg-primary);
      }
      
      .theme-toggle-label {
        display: flex;
        align-items: center;
        gap: 12px;
        color: var(--text-primary);
        font-weight: 500;
      }
      
      .notification-list {
        max-height: 300px;
        overflow-y: auto;
      }
      
      .notification-item {
        padding: 14px 16px;
        margin-bottom: 10px;
        background: var(--bg-tertiary);
        border-radius: 8px;
        font-size: 13px;
        border-left: 4px solid #ff991f;
        line-height: 1.5;
        transition: all 0.2s;
      }
      
      .notification-item:hover {
        transform: translateX(-4px);
      }
      
      .notification-item.critical {
        border-left-color: #de350b;
      }
      
      .btn-logout {
        width: 100%;
        padding: 14px 20px;
        background: var(--bg-tertiary);
        border: 1px solid var(--border-color);
        border-radius: 8px;
        color: var(--text-primary);
        font-weight: 600;
        font-size: 15px;
        cursor: pointer;
        transition: all 0.2s;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 8px;
      }
      
      .btn-logout:hover {
        background: #de350b;
        border-color: #de350b;
        color: white;
      }
      
      /* === CARDS === */
      .card {
        background: var(--bg-secondary);
        border: 1px solid var(--border-color);
        border-radius: 12px;
        box-shadow: var(--shadow-sm);
        transition: all 0.2s;
        margin-bottom: 20px;
      }
      
      .card:hover {
        box-shadow: var(--shadow-md);
      }
      
      .card-header {
        background: transparent;
        border-bottom: 1px solid var(--border-color);
        padding: 20px 24px;
        font-weight: 600;
        font-size: 16px;
        color: var(--text-primary);
      }
      
      .card-body {
        padding: 24px;
      }
      
      /* === VALUE BOXES === */
      .bslib-value-box {
        background: var(--bg-secondary);
        border: 1px solid var(--border-color);
        border-radius: 12px;
        box-shadow: var(--shadow-sm);
        transition: all 0.2s;
      }
      
      .bslib-value-box:hover {
        transform: translateY(-2px);
        box-shadow: var(--shadow-md);
      }
      
      /* === KANBAN BOARD === */
      .kanban-container {
        padding: 24px;
        height: calc(100vh - 180px);
      }
      
      .kanban-header-bar {
        background: var(--bg-secondary);
        padding: 20px 24px;
        border-radius: 12px;
        margin-bottom: 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        box-shadow: var(--shadow-sm);
        border: 1px solid var(--border-color);
      }
      
      .kanban-title {
        font-size: 24px;
        font-weight: 700;
        color: var(--text-primary);
        margin: 0;
        display: flex;
        align-items: center;
        gap: 12px;
      }
      
      .btn-create-task {
        background: var(--accent);
        color: white;
        border: none;
        padding: 12px 24px;
        border-radius: 8px;
        font-weight: 600;
        font-size: 15px;
        transition: all 0.2s;
        box-shadow: 0 2px 4px rgba(76, 154, 255, 0.2);
      }
      
      .btn-create-task:hover {
        background: var(--accent-hover);
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(76, 154, 255, 0.3);
      }
      
      .kanban-board {
        display: flex;
        gap: 20px;
        overflow-x: auto;
        padding: 4px;
        height: calc(100vh - 280px);
        align-items: flex-start;
      }
      
      .kanban-column {
        flex: 1 1 0;
        min-width: 0;

        background: var(--bg-secondary);
        border-radius: 12px;
        padding: 16px;
        display: flex;
        flex-direction: column;
        border: 1px solid var(--border-color);
        box-shadow: var(--shadow-sm);
        height: 100%;
      }
      
      .kanban-column-header {
        font-weight: 600;
        font-size: 14px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 16px;
        padding-bottom: 12px;
        border-bottom: 2px solid var(--border-color);
        display: flex;
        justify-content: space-between;
        align-items: center;
        color: var(--text-secondary);
      }
      
      .column-count-badge {
        background: var(--bg-tertiary);
        color: var(--text-secondary);
        padding: 4px 10px;
        border-radius: 12px;
        font-size: 13px;
        font-weight: 700;
      }
      
      .kanban-cards {
        flex: 1;
        overflow-y: auto;
        padding-right: 4px;
        min-height: 100px;
      }
      
      .kanban-cards::-webkit-scrollbar {
        width: 6px;
      }
      
      .kanban-cards::-webkit-scrollbar-track {
        background: var(--bg-tertiary);
        border-radius: 3px;
      }
      
      .kanban-cards::-webkit-scrollbar-thumb {
        background: var(--border-color);
        border-radius: 3px;
      }
      
      .empty-column {
        text-align: center;
        color: var(--text-tertiary);
        padding: 40px 20px;
        font-size: 14px;
        font-weight: 500;
      }
      
      /* === TASK CARDS === */
      .task-card {
        background: var(--bg-secondary);
        border-radius: 8px;
        padding: 12px 16px; /* Adjusted padding */
        margin-bottom: 12px;
        box-shadow: var(--shadow-sm);
        border: 1px solid var(--border-color);
        transition: all 0.2s;
        position: relative; /* Needed for handle positioning */
        max-width: 100%;
      }
      
      .task-card:hover {
        box-shadow: var(--shadow-md);
        border-color: var(--accent);
      }
      
      .task-card:active {
        cursor: grabbing;
      }
      
      .task-card.sortable-chosen {
        opacity: 0.5;
        cursor: grabbing;
      }
      
      .task-card.sortable-ghost {
        opacity: 0.3;
        background: var(--accent);
      }
      
      .task-title {
        font-weight: 600;
        font-size: 15px;
        color: var(--text-primary);
        margin-bottom: 8px;
        line-height: 1.4;

        /* Truncation Logic */
        display: -webkit-box;
        -webkit-line-clamp: 2;        /* Show max 2 lines */
        -webkit-box-orient: vertical;
        overflow: hidden;
        text-overflow: ellipsis;
        word-break: break-all;       /* FORCE break long words like 'aaaaa' */
        overflow-wrap: anywhere;
      }

      .task-description {
        font-size: 13px;
        color: var(--text-secondary);
        margin-bottom: 12px;

        display: -webkit-box;
        -webkit-line-clamp: 2; /* Show max 2 lines */
        -webkit-box-orient: vertical;
        overflow: hidden;
        text-overflow: ellipsis;
        word-break: break-all;       /* FORCE break long words like 'aaaaa' */
        overflow-wrap: anywhere;
        line-height: 1.4;
      }
      
      .priority-badge {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 4px;
        font-size: 11px;
        font-weight: 700;
        border: 1px solid transparent;
        text-transform: uppercase;
        margin-top: 4px;
      }
      
      .task-meta {
        font-size: 13px;
        color: var(--text-secondary);
        margin-bottom: 12px;
        display: flex;
        align-items: center;
        gap: 6px;
      }
      
      .task-actions {
        display: flex;
        gap: 8px;
        align-items: center;
      }
      
      .btn-task-action {
        display: inline-block;
        padding: 6px 14px;
        font-size: 13px;
        border-radius: 6px;
        background: var(--bg-tertiary);
        border: 1px solid var(--border-color);
        color: var(--text-primary);
        font-weight: 500;
        transition: all 0.2s;
        cursor: pointer;
      }
      
      .btn-task-action:hover {
        background: var(--bg-primary);
        border-color: var(--text-tertiary);
      }
      
      .btn-task-complete {
        padding: 6px 12px;
        font-size: 13px;
        border-radius: 6px;
        background: #00875a;
        border: none;
        color: white;
        font-weight: 500;
        transition: all 0.2s;
        cursor: pointer;
      }
      
      .btn-task-complete:hover {
        background: #006644;
        transform: translateY(-1px);
      }

      .btn-task-delete {
        padding: 6px 14px;
        font-size: 13px;
        border-radius: 6px;
        background: var(--bg-tertiary);
        border: 1px solid var(--border-color);
        color: var(--text-tertiary); /* Muted color by default */
        font-weight: 500;
        transition: all 0.2s;
        cursor: pointer;
      }
      
      .btn-task-delete:hover {
        background: #ffebe6;
        border-color: #ffbdad;
        color: #de350b; /* Red on hover */
      }
      
      /* === DRAG HANDLE === */
      .drag-handle {
        cursor: grab;
        color: var(--text-tertiary);
        padding: 4px 8px;
        position: absolute;
        top: 12px;
        right: 12px;
        font-size: 14px;
        border-radius: 4px;
      }
      
      .drag-handle:hover {
        background: var(--bg-tertiary);
        color: var(--text-primary);
      }
      
      .drag-handle:active {
        cursor: grabbing;
      }
      
      /* === MODAL === */
      .modal {
        z-index: 1050 !important; 
      }

      .modal-backdrop {
        z-index: 1040 !important;
      }

      .modal-content {
        border-radius: 12px;
        border: 1px solid var(--border-color);
        box-shadow: 0 20px 50px rgba(0,0,0,0.3) !important; /* Stronger, cleaner shadow */
        background: var(--bg-secondary);
        background-clip: padding-box; /* Ensures background doesn't bleed */
      }
      
      .modal-header {
        border-bottom: 1px solid var(--border-color);
        padding: 24px 28px;
        background: var(--bg-secondary);
      }
      
      .modal-title {
        font-size: 20px;
        font-weight: 700;
        color: var(--text-primary);
      }
      
      .modal-body {
        padding: 28px;
        background: var(--bg-secondary);
      }
      
      .modal-footer {
        border-top: 1px solid var(--border-color);
        padding: 20px 28px;
        background: var(--bg-secondary);
      }

      .modal-dialog {
        /* Fixes the ghosting artifact */
        box-shadow: none !important; 
      }
      
      .btn-secondary {
        background: var(--bg-tertiary);
        border: 1px solid var(--border-color);
        color: var(--text-primary);
        border-radius: 8px;
        padding: 10px 20px;
        font-weight: 600;
        transition: all 0.2s;
      }
      
      .btn-secondary:hover {
        background: var(--bg-primary);
        border-color: var(--text-tertiary);
        color: var(--text-primary);
      }

      /* === MODAL DELETE BUTTON === */
      .btn-modal-delete {
        background-color: #de350b; /* Red */
        border-color: #de350b;
        color: white;
        font-weight: 600;
        transition: all 0.2s; /* This enables the animation */
      }
      
      .btn-modal-delete:hover {
        background-color: #bf2600 !important; /* Darker Red on hover */
        border-color: #bf2600 !important;
        transform: translateY(-1px);          /* Slight lift effect */
        box-shadow: 0 4px 8px rgba(222, 53, 11, 0.3); /* Red glow */
        color: white;
      }
      
      /* === ALERTS === */
      .alert {
        border-radius: 8px;
        border: none;
        padding: 16px 20px;
        font-size: 14px;
        font-weight: 500;
      }
      
      .alert-danger {
        background: #ffebe6;
        color: #bf2600;
        border-left: 4px solid #de350b;
      }
      
      /* === SWITCH === */
      .form-switch .form-check-input {
        width: 48px;
        height: 24px;
        cursor: pointer;
      }
      
      .form-switch .form-check-input:checked {
        background-color: var(--accent);
        border-color: var(--accent);
      }

      .air-datepicker-global-container {
        z-index: 1060 !important; /* Higher than modal (1050) */
      }
      
      /* Ensure the input wrapper doesn't clip the popup */
      .air-datepicker-cell.-selected- {
        background: var(--accent) !important;
      }

      /* === MODERN SCROLLBAR (Global) === */
      /* Works on Chrome, Edge, Safari */
      ::-webkit-scrollbar {
        width: 6px;               /* Vertical scrollbar width */
        height: 6px;              /* Horizontal scrollbar height */
      }

      ::-webkit-scrollbar-track {
        background: transparent;  /* Transparent track */
      }

      ::-webkit-scrollbar-thumb {
        background-color: #dfe1e6; /* Light gray handle */
        border-radius: 20px;       /* Fully rounded corners */
        border: 2px solid transparent; /* padding around scroll thumb */
        background-clip: content-box;  /* makes the scrollbar look thinner */
      }

      ::-webkit-scrollbar-thumb:hover {
        background-color: #c1c7d0; /* Darker on hover */
      }

      /* Works on Firefox */
      * {
        scrollbar-width: thin;
        scrollbar-color: #dfe1e6 transparent;
      }
    "))
  ),
  
  # Login Page
  div(
    id = "login_page",
    class = "login-container",
    div(
      class = "login-card fade-in",
      div(
        class = "login-header",
        icon("clipboard-check", class = "login-icon"),
        h1(class = "login-title", "Task Tracker"),
        p(class = "login-subtitle", "Sign in to manage your tasks")
      ),
      div(
        style = "margin-bottom: 20px;",
        tags$label(class = "form-label", "Username"),
        textInput("username", NULL, placeholder = "admin", width = "100%")
      ),
      div(
        style = "margin-bottom: 24px;",
        tags$label(class = "form-label", "Password"),
        passwordInput("password", NULL, placeholder = "admin123", width = "100%")
      ),
      actionButton("login_btn", "Sign In", class = "btn-primary w-100"),
      uiOutput("login_error")
    )
  ),
  
  # Main App (hidden initially)
  div(
    id = "main_app",
    style = "display: none;",
    
# Navbar
    div(
      class = "app-navbar",
      
      # LEFT: Logo and Tabs
      div(
        class = "navbar-left",
        div(
          class = "navbar-brand",
          icon("clipboard-check"),
          span("Task Tracker")
        ),
        div(
          class = "nav-pills",
          actionButton("nav_dashboard", "Dashboard", class = "nav-link active"),
          actionButton("nav_tasks", "Tasks", class = "nav-link")
        )
      ),
      
      # RIGHT: Icons (Dark Mode, Notifs, Logout)
      div(
        class = "navbar-right",
        
        # 1. Dark Mode Toggle
        tags$button(
          id = "theme_toggle_btn",
          class = "nav-icon-btn",
          onclick = "toggleTheme()", # Reusing your existing JS function
          icon("moon")
        ),
        
        # 2. Notifications (Uses bslib popover)
        popover(
          trigger = tags$button(
            class = "nav-icon-btn",
            icon("bell"),
            # Dynamic UI for the red dot badge
            uiOutput("notif_badge_indicator", inline = TRUE)
          ),
          title = "Notifications",
          placement = "bottom",
          
          # Content inside the popover
          div(
            class = "notification-list",
            style = "max-height: 300px; overflow-y: auto; padding: 12px;",
            uiOutput("navbar_notifications") # Renamed from sidebar_notifications
          ),
          div(
            style = "padding: 12px; border-top: 1px solid var(--border-color); text-align: center;",
            actionButton("clear_notifications", "Mark all read", 
                         class = "btn-task-action w-100")
          )
        ),
        
        # 3. Logout
        tags$button(
          id = "logout_btn",
          class = "nav-icon-btn",
          onclick = "Shiny.setInputValue('logout', Math.random());",
          icon("sign-out-alt"),
          title = "Logout"
        )
      )
    ),
    
    # Sidebar Menu
    div(
      class = "sidebar-overlay",
      id = "sidebar_overlay",
      onclick = "toggleSidebar()"
    ),
    div(
      class = "sidebar-menu",
      id = "sidebar_menu",
      div(
        class = "sidebar-header",
        span(class = "sidebar-title", "Menu"),
        tags$button(
          class = "sidebar-close",
          onclick = "toggleSidebar()",
          icon("times")
        )
      ),
      div(
        class = "sidebar-section",
        div(class = "section-title", "Appearance"),
        div(
          class = "theme-toggle",
          tags$label(
            class = "theme-toggle-label",
            `for` = "theme_switch",
            icon("moon"),
            span("Dark Mode")
          ),
          div(
            class = "form-check form-switch",
            tags$input(
              type = "checkbox",
              class = "form-check-input",
              id = "theme_switch"
            )
          )
        )
      ),
      div(
        class = "sidebar-section",
        div(class = "section-title", "Notifications"),
        div(
          class = "notification-list",
          uiOutput("sidebar_notifications")
        ),
        actionButton("clear_notifications", "Clear All", 
                     class = "btn-task-action w-100",
                     style = "margin-top: 12px;")
      ),
      div(
        class = "sidebar-section",
        tags$button(
          class = "btn-logout",
          onclick = "Shiny.setInputValue('logout', Math.random());",
          icon("sign-out-alt"),
          span("Logout")
        )
      )
    ),
    
    # Content Area
    uiOutput("main_content"),
    
    # JavaScript for sidebar and theme
    tags$script(HTML("
      function toggleSidebar() {
        document.getElementById('sidebar_overlay').classList.toggle('active');
        document.getElementById('sidebar_menu').classList.toggle('active');
      }

      function toggleTheme(targetTheme) {
        const html = document.documentElement;
        const switchEl = document.getElementById('theme_switch');
        const currentTheme = html.getAttribute('data-theme') || 'light';
        const newTheme = targetTheme || (currentTheme === 'dark' ? 'light' : 'dark');
        html.setAttribute('data-theme', newTheme);
        if (switchEl) {
          switchEl.checked = newTheme === 'dark';
        }
        localStorage.setItem('theme', newTheme);

        // Notify Shiny
        Shiny.setInputValue('theme_changed', newTheme);
      }

      // Load theme on page load
      document.addEventListener('DOMContentLoaded', function() {
        const switchEl = document.getElementById('theme_switch');
        const themeToggle = document.querySelector('.theme-toggle');
        const savedTheme = localStorage.getItem('theme') || 'light';
        document.documentElement.setAttribute('data-theme', savedTheme);
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('theme_changed', savedTheme, {priority: 'event'});
        }
        if (switchEl) {
          switchEl.checked = savedTheme === 'dark';
          switchEl.addEventListener('change', function(e) {
            toggleTheme(e.target.checked ? 'dark' : 'light');
          });
        }

        if (themeToggle) {
          themeToggle.addEventListener('click', function(e) {
            if (e.target.id !== 'theme_switch') {
              if (switchEl) {
                switchEl.checked = !switchEl.checked;
                toggleTheme(switchEl.checked ? 'dark' : 'light');
              } else {
                toggleTheme();
              }
            }
          });
        }
      });

      /* === NEW: SESSION COOKIE MANAGEMENT === */
      
      // 1. Function to Set a Cookie (Login)
      function setCookie(name, value, days) {
        var expires = '';
        if (days) {
          var date = new Date();
          date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
          expires = '; expires=' + date.toUTCString();
        }
        document.cookie = name + '=' + (value || '') + expires + '; path=/';
      }

      // 2. Function to Get a Cookie (Auto-Login)
      function getCookie(name) {
        var nameEQ = name + '=';
        var ca = document.cookie.split(';');
        for(var i=0;i < ca.length;i++) {
          var c = ca[i];
          while (c.charAt(0)==' ') c = c.substring(1,c.length);
          if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
        }
        return null;
      }

      // 3. Function to Delete Cookie (Logout)
      function deleteCookie(name) {
        document.cookie = name + '=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
      }

      // 4. Send Cookie to Shiny on Connection
      $(document).on('shiny:connected', function() {
        var savedUser = getCookie('task_tracker_user');
        // Send this value to R as input$saved_user
        Shiny.setInputValue('saved_user', savedUser);
      });
    "))
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

  # --- AUTO-LOGIN LOGIC ---
  observeEvent(input$saved_user, {
    # Only run if we aren't already logged in
    req(!logged_in())
    
    # Check if the cookie contains our valid user (In production, use a secure token!)
    if (!is.null(input$saved_user) && input$saved_user == "admin") {
      logged_in(TRUE)
      shinyjs::hide("login_page")
      shinyjs::show("main_app")
      showNotification("Welcome back!", type = "message")
    }
  })

  # Initialize app state
  logged_in <- reactiveVal(FALSE)
  db_conn <- tryCatch(
    {
      db_connect()
    },
    error = function(e) {
      showNotification("Database connection failed. Please check credentials and network.", type = "error", duration = NULL)
      stop(e)
    }
  )
  initialize_db(db_conn)
  selected_task <- reactiveVal(NULL)
  tasks_data <- reactiveVal(NULL)
  current_view <- reactiveVal("dashboard")
  theme_mode <- reactiveVal("light")

  onStop(function() {
    if (dbIsValid(db_conn)) {
      dbDisconnect(db_conn)
    }
  })

  # Login logic
  observeEvent(input$login_btn, {
    valid_username <- identical(input$username, DEFAULT_CREDENTIALS$username)
    valid_password <- sodium::password_verify(DEFAULT_CREDENTIALS$password_hash, input$password)

    if (isTRUE(valid_username) && isTRUE(valid_password)) {
      logged_in(TRUE)
      
      # Save session cookie for 7 days
      shinyjs::runjs(sprintf("setCookie('task_tracker_user', '%s', 7);", input$username))

      shinyjs::hide("login_page")
      shinyjs::show("main_app")
      output$login_error <- renderUI(NULL)
    } else {
      output$login_error <- renderUI({
        div(
          class = "alert alert-danger",
          style = "margin-top: 20px;",
          icon("exclamation-circle"),
          " Invalid credentials. Use admin / admin123"
        )
      })
    }
  })
  
  # Logout
  observeEvent(input$logout, {
    logged_in(FALSE)

    # Delete the session cookie
    shinyjs::runjs("deleteCookie('task_tracker_user');")

    shinyjs::show("login_page")
    shinyjs::hide("main_app")
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
  })
  
  # Navigation
  observeEvent(input$nav_dashboard, {
    current_view("dashboard")
    shinyjs::addClass(id = "nav_dashboard", class = "active")
    shinyjs::removeClass(id = "nav_tasks", class = "active")
  })
  
  observeEvent(input$nav_tasks, {
    current_view("tasks")
    shinyjs::addClass(id = "nav_tasks", class = "active")
    shinyjs::removeClass(id = "nav_dashboard", class = "active")
  })
  
  # ===========================================================================
  # 1. POLLING TIMERS (The "Heartbeat" of the App)
  # ===========================================================================
  
  # Fast Timer (Every 2 seconds): Checks for new notifications
  fast_poll <- reactiveTimer(2000)
  
  # Slow Timer (Every 30 seconds): Checks for overdue tasks & updates board
  # We do this less often so cards don't "jump" while you are dragging them
  slow_poll <- reactiveTimer(30000)

  # ===========================================================================
  # 2. AUTO-REFRESH LOGIC
  # ===========================================================================

  # MAIN DATA LOOP (Runs every 60s OR when manually refreshed)
  observe({
    req(logged_in())
    
    # Trigger 1: The 60-second timer
    slow_poll()
    
    # Trigger 2: Manual refresh (via the helper function)
    # Note: We don't need to explicitly list it here because 'refresh_data'
    # updates 'tasks_data' directly, but we do need to run the checks.
    
    # A. Check for overdue tasks (Background Worker Simulation)
    # This ensures statuses flip to "OVERDUE" even if you just sit there.
    backend_check_overdue(db_conn)
    
    # B. Fetch the fresh data for the Kanban board
    tasks_data(backend_fetch_kanban(db_conn))
  })

  observeEvent(input$theme_changed, {
    if (!is.null(input$theme_changed) && nzchar(input$theme_changed)) {
      theme_mode(input$theme_changed)
    }
  }, ignoreInit = FALSE)

  # Refresh data helper
  refresh_data <- function() {
    tasks_data(backend_fetch_kanban(db_conn))
  }
  
  # Main content renderer
  output$main_content <- renderUI({
    req(logged_in())
    view <- current_view()
    
    if (view == "dashboard") {
      # Dashboard Content
      div(
        class = "fade-in",
        style = "padding: 24px;",
        layout_columns(
          col_widths = c(6, 6, 4, 4, 4, 12),
          
          card(
            card_header("Status Distribution"),
            card_body(plotOutput("status_pie", height = "200px"))
          ),
          
          card(
            card_header("Priority Breakdown"),
            card_body(plotOutput("priority_bar", height = "200px"))
          ),
          
          value_box(
            title = "Total Tasks",
            value = textOutput("total_tasks"),
            showcase = icon("tasks"),
            theme = "primary"
          ),
          
          value_box(
            title = "In Progress",
            value = textOutput("in_progress_count"),
            showcase = icon("spinner"),
            theme = "info"
          ),
          
          value_box(
            title = "Overdue",
            value = textOutput("overdue_count"),
            showcase = icon("exclamation-triangle"),
            theme = "danger"
          ),
          
          card(
            card_header("Upcoming Deadlines"),
            card_body(plotOutput("deadline_timeline", height = "250px"))
          )
        )
      )
    } else {
      # Tasks (Kanban) Content
      div(
        class = "kanban-container fade-in",
        div(
          class = "kanban-header-bar",
          h2(class = "kanban-title", 
             icon("columns"),
             "Kanban Board"
          ),
          actionButton("create_task_btn", 
                       HTML('<i class="fa fa-plus"></i> Create Task'),
                       class = "btn-create-task")
        ),
        div(
          class = "kanban-board",
          uiOutput("kanban_columns")
        )
      )
    }
  })
  
  # Kanban board with drag & drop
  output$kanban_columns <- renderUI({
    req(logged_in())
    tasks <- tasks_data()
    req(!is.null(tasks))
    
    columns <- list(
      list(id = "TODO", name = "TO DO"),
      list(id = "IN_PROGRESS", name = "IN PROGRESS"),
      list(id = "OVERDUE", name = "OVERDUE"),
      list(id = "COMPLETED", name = "COMPLETED")
    )
    
    column_uis <- lapply(columns, function(col) {
      col_tasks <- tasks[tasks$ui_column == col$id, ]
      
      card_list <- if (nrow(col_tasks) > 0) {
        lapply(1:nrow(col_tasks), function(i) {
          create_task_card(col_tasks[i, ], session$ns)
        })
      } else {
        list(div(
          class = "empty-placeholder",
          style = "height: 50px; border: 1px dashed #ccc; border-radius: 8px; margin-bottom: 8px; display: flex; align-items: center; justify-content: center; color: #999;",
          "Drop here"
        ))
      }
      
      div(
        class = "kanban-column",
        id = paste0("col_", col$id),
        `data-status` = col$id,
        
        div(
          class = "kanban-column-header",
          span(col$name),
          span(class = "column-count-badge", nrow(col_tasks))
        ),
        
        rank_list(
          text = NULL,
          labels = card_list,
          input_id = paste0("rank_", col$id),
          class = "kanban-cards",
          options = sortable_options(
            group = "kanban",
            handle = ".drag-handle",
            onEnd = htmlwidgets::JS("
              function(evt) {
                var itemEl = evt.item;
                
                // --- ROBUST ID LOOKUP ---
                // 1. Try to get ID from the dragged element itself
                var taskId = itemEl.getAttribute('data-task-id');
                
                // 2. If not found, sortable might have wrapped it. 
                //    Look for the ID inside the children.
                if (!taskId) {
                   var innerCard = itemEl.querySelector('.task-card');
                   if (innerCard) {
                     taskId = innerCard.getAttribute('data-task-id');
                   }
                }
                // ------------------------

                // Find the new status from the column wrapper
                var dropTarget = evt.to; 
                var columnWrapper = dropTarget.closest('.kanban-column');
                var newStatus = columnWrapper ? columnWrapper.getAttribute('data-status') : null;
                
                console.log('Moved Task:', taskId, 'To Status:', newStatus);

                if (taskId && newStatus) {
                  Shiny.setInputValue('task_moved', {
                    task_id: taskId,
                    new_status: newStatus,
                    timestamp: new Date().getTime()
                  }, {priority: 'event'});
                }
              }
            ")
          )
        )
      )
    })
    
    tagList(column_uis)
  })
  
  # Handle drag & drop
  observeEvent(input$task_moved, {
    req(input$task_moved)
    task_id <- as.numeric(input$task_moved$task_id)
    new_status <- input$task_moved$new_status

    req(!is.na(task_id), nzchar(new_status))

    # If moved to COMPLETED, mark it
    if (new_status == "COMPLETED") {
      backend_mark_completed(db_conn, task_id)
    } else {
      backend_move_task(db_conn, task_id, new_status)
    }

    refresh_data()
  })
  
  # Task selection and actions
  # --- NEW: Single Observer for Edit Actions ---
  observeEvent(input$trigger_edit_task, {
    task_id <- input$trigger_edit_task
    tasks <- tasks_data()
    
    # Find the specific task from the current data
    task <- tasks[tasks$id == task_id, ]
    
    if (nrow(task) > 0) {
      selected_task(task)
      showModal(task_modal(task, edit = TRUE))
    }
  })

  # --- NEW: Single Observer for Complete Actions ---
  observeEvent(input$trigger_complete_task, {
    task_id <- input$trigger_complete_task
    
    # Run the database update
    backend_mark_completed(db_conn, task_id)
    
    # Refresh the UI
    refresh_data()
  })
  
  # Task modal
  task_modal <- function(task = NULL, edit = FALSE) {
    modalDialog(
      title = if (edit) "Edit Task" else "Create New Task",
      size = "m",
      
      div(
        style = "margin-bottom: 20px;",
        tags$label(class = "form-label", "Title"),
        textInput("modal_title", NULL, 
                  value = if (edit) task$title else "",
                  width = "100%")
      ),
      div(
        style = "margin-bottom: 20px;",
        tags$label(class = "form-label", "Description"),
        textAreaInput("modal_description", NULL,
                      value = if (edit) task$description else "",
                      rows = 3,
                      width = "100%")
      ),
      div(
        style = "margin-bottom: 20px;",
        tags$label(class = "form-label", "Priority"),
        selectInput("modal_priority", NULL,
                    choices = c("LOW", "MEDIUM", "HIGH", "CRITICAL"),
                    selected = if (edit) task$priority else "MEDIUM",
                    width = "100%")
      ),
      div(
        style = "margin-bottom: 20px;",
        tags$label(class = "form-label", "Status"),
        selectInput("modal_status", NULL,
                    choices = c("TODO", "IN_PROGRESS", "OVERDUE", "COMPLETED"),
                    selected = if (edit) task$status else "TODO",
                    width = "100%")
      ),
      div(
        style = "margin-bottom: 20px;",
        tags$label(class = "form-label", "Start Date/Time"),
        airDatepickerInput(
          "modal_start", NULL,
          value = if (edit) task$start_datetime else Sys.time(),
          timepicker = TRUE,
          width = "100%",
          position = "top right"
        )
      ),
      div(
        tags$label(class = "form-label", "Due Date/Time"),
        airDatepickerInput(
          "modal_due", NULL,
          value = if (edit) task$due_datetime else Sys.time() + 86400,
          timepicker = TRUE,
          width = "100%",
          position = "top right"
        )
      ),
      
      footer = tagList(
        if (edit) {
          div(
            style = "float: left;",
            # CHANGE: Removed inline style, added 'btn-modal-delete' class
            actionButton("delete_task_modal", "Delete", icon = icon("trash"), 
                         class = "btn-danger btn-modal-delete")
          )
        },
        actionButton("modal_cancel", "Cancel", class = "btn-secondary"),
        actionButton("save_task", "Save Task", class = "btn-primary")
      )
    )
  }
  
  observeEvent(input$modal_cancel, {
    removeModal()
  })
  
  # Create task button
  observeEvent(input$create_task_btn, {
    showModal(task_modal())
  })
  
  # Save task
  observeEvent(input$save_task, {
    parse_datetime <- function(value) {
      if (inherits(value, "POSIXt")) return(value)
      if (is.null(value) || is.na(value)) return(NA)
      as.POSIXct(value)
    }

    task <- list(
      title = input$modal_title,
      description = input$modal_description,
      priority = input$modal_priority,
      status = input$modal_status,
      start_datetime = parse_datetime(input$modal_start),
      due_datetime = parse_datetime(input$modal_due)
    )
    
    sel_task <- selected_task()

    if (!is.null(sel_task)) {
      backend_update_task(db_conn, sel_task$id, task)
      selected_task(NULL)
    } else {
      backend_create_task(db_conn, task)
    }
    
    refresh_data()
    removeModal()
  })
  
  # Dashboard outputs
  output$status_pie <- renderPlot({
    req(logged_in())
    tasks <- tasks_data()
    req(!is.null(tasks))

    if (nrow(tasks) == 0) {
      return(ggplot() + 
               annotate("text", x = 1, y = 1, label = "No tasks yet", size = 6, color = "#8993a4") + 
               theme_void())
    }

    status_data <- as.data.frame(table(tasks$status))
    if (ncol(status_data) < 2) colnames(status_data) <- c("Var1", "Freq")
    colnames(status_data) <- c("Status", "Count")

    # Dark Mode Logic
    is_dark <- identical(theme_mode(), "dark")
    text_color <- if (is_dark) "#e4e6eb" else "#172b4d"

    status_colors <- c(
      "TODO" = "#dfe1e6",       # Gray
      "IN_PROGRESS" = "#0065ff", # Blue
      "OVERDUE" = "#de350b",    # Red
      "COMPLETED" = "#00875a"   # Green
    )

    ggplot(status_data, aes(x = 2, y = Count, fill = Status)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      xlim(c(0.5, 2.5)) +
      theme_void(base_size = 14) + # Increase base text size
      theme(
        text = element_text(family = "sans"), # Cleaner font
        legend.text = element_text(color = text_color),
        legend.title = element_text(color = text_color, face = "bold"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA)
      ) +
      scale_fill_manual(values = status_colors)
  }, bg = "transparent", res = 96)

  output$priority_bar <- renderPlot({
    req(logged_in())
    tasks <- tasks_data()
    req(!is.null(tasks))
    
    if (nrow(tasks) == 0) {
      return(ggplot() + 
               annotate("text", x = 1, y = 1, label = "No data available", size = 6, color = "#8993a4") + 
               theme_void())
    }
    
    priority_data <- as.data.frame(table(tasks$priority))
    if (ncol(priority_data) < 2) { 
        priority_data <- data.frame(Priority = character(0), Count = integer(0))
    } else {
        colnames(priority_data) <- c("Priority", "Count")
    }

    priority_data$Priority <- factor(priority_data$Priority, levels = c("LOW", "MEDIUM", "HIGH", "CRITICAL"))

    is_dark <- identical(theme_mode(), "dark")
    text_color <- if (is_dark) "#e4e6eb" else "#172b4d"
    grid_color <- if (is_dark) "#38414a" else "#dfe1e6"

    ggplot(priority_data, aes(x = Priority, y = Count, fill = Priority)) +
      geom_bar(stat = "identity") +
      theme_minimal(base_size = 14) + # Larger text base
      theme(
        text = element_text(family = "sans", color = text_color),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(color = text_color, face = "bold"),
        axis.text = element_text(color = text_color),
        panel.grid.major.y = element_line(color = grid_color),
        panel.grid.minor.y = element_blank()
      ) +
      scale_fill_manual(values = c("LOW" = "#28a745", "MEDIUM" = "#ffc107",
                                   "HIGH" = "#fd7e14", "CRITICAL" = "#dc3545"))
  }, bg = "transparent", res = 96)
  
  output$deadline_timeline <- renderPlot({
    req(logged_in())
    tasks <- tasks_data()
    req(!is.null(tasks))
    
    upcoming <- tasks[!is.na(tasks$due_datetime) & tasks$due_datetime > Sys.time(), ]
    
    if (nrow(upcoming) == 0) {
      return(ggplot() + 
               annotate("text", x = 1, y = 1, label = "No upcoming deadlines", size = 5, color = "#8993a4") + 
               theme_void() +
               theme(plot.background = element_rect(fill = "transparent", color = NA)))
    }

    upcoming <- upcoming[order(upcoming$due_datetime), ]
    upcoming <- head(upcoming, 10)

    is_dark <- identical(theme_mode(), "dark")
    text_color <- if (is_dark) "#e4e6eb" else "#172b4d"
    grid_color <- if (is_dark) "#38414a" else "#dfe1e6"

    ggplot(upcoming, aes(x = due_datetime, y = reorder(title, due_datetime))) +
      geom_segment(aes(x = Sys.time(), xend = due_datetime,
                       y = reorder(title, due_datetime),
                       yend = reorder(title, due_datetime),
                       color = priority),
                   linewidth = 1, alpha = 0.6) +
      geom_point(aes(color = priority, shape = status), size = 4) +
      theme_minimal(base_size = 14) + # Larger text base
      theme(
        text = element_text(family = "sans", color = text_color),
        legend.position = "bottom",
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(color = text_color),
        axis.text = element_text(color = text_color),
        legend.text = element_text(color = text_color),
        legend.title = element_text(color = text_color),
        panel.grid.major = element_line(color = grid_color),
        panel.grid.minor = element_blank()
      ) +
      labs(x = "Due Date", y = NULL, shape = "Status") +
      scale_shape_manual(values = c("TODO" = 16, "IN_PROGRESS" = 17, "OVERDUE" = 15, "COMPLETED" = 13)) +
      scale_color_manual(values = c("LOW" = "#28a745", "MEDIUM" = "#ffc107",
                                    "HIGH" = "#fd7e14", "CRITICAL" = "#dc3545"))
  }, bg = "transparent", res = 96)
  
  output$total_tasks <- renderText({
    req(logged_in())
    tasks <- tasks_data()
    req(!is.null(tasks))
    nrow(tasks)
  })
  
  output$in_progress_count <- renderText({
    req(logged_in())
    tasks <- tasks_data()
    req(!is.null(tasks))
    sum(tasks$status == "IN_PROGRESS")
  })
  
  output$overdue_count <- renderText({
    req(logged_in())
    tasks <- tasks_data()
    req(!is.null(tasks))
    sum(tasks$status == "OVERDUE")
  })
  
  # 1. Render the Notification List (Inside Popover)
  output$navbar_notifications <- renderUI({
    req(logged_in())
    
    fast_poll()

    # Fetch from DB table (using the new function we discussed previously)
    notifs <- backend_fetch_notifications(db_conn) 
    
    if (length(notifs) > 0) {
      lapply(notifs, function(msg) {
        # Check for keywords to style critical vs normal
        is_critical <- grepl("🔴|Critical", msg)
        div(
          class = if (is_critical) "notification-item critical" else "notification-item",
          msg
        )
      })
    } else {
      div(
        style = "text-align: center; color: var(--text-tertiary); padding: 20px;",
        "No new notifications"
      )
    }
  })

  # 2. Render the Red Dot Badge (Only if notifications exist)
  output$notif_badge_indicator <- renderUI({
    req(logged_in())

    fast_poll()
    notifs <- backend_fetch_notifications(db_conn)
    
    if (length(notifs) > 0) {
      span(class = "notif-badge")
    } else {
      NULL
    }
  })
  
  observeEvent(input$clear_notifications, {
    req(logged_in())

    tryCatch({
      backend_clear_notifications(db_conn)
      showNotification("All notifications marked as read", type = "message")
    }, error = function(e) {
      showNotification("Failed to clear notifications", type = "error")
    })
    
    refresh_data()
  })

  # --- DELETE LOGIC ---
  
  # A. Store the ID of the task we want to delete (from card or modal)
  task_to_delete <- reactiveVal(NULL)

  # 1. Trigger from CARD
  observeEvent(input$trigger_delete_task, {
    task_to_delete(input$trigger_delete_task)
    show_delete_confirmation()
  })

  # 2. Trigger from MODAL
  observeEvent(input$delete_task_modal, {
    # We are currently in the edit modal, so we use the selected task ID
    req(selected_task())
    task_to_delete(selected_task()$id)
    show_delete_confirmation()
  })

  # 3. Helper to show confirmation dialog
  show_delete_confirmation <- function() {
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to permanently delete this task?",
      footer = tagList(
        actionButton("cancel_delete", "Cancel", class = "btn-secondary"),
        # FIX: Added 'btn-modal-delete' class here to enable the hover animation
        actionButton("confirm_delete", "Delete Task", class = "btn-danger btn-modal-delete")
      ),
      size = "s" 
    ))
  }

  # 4. Handle Confirmation Click
  observeEvent(input$confirm_delete, {
    req(task_to_delete())
    
    tryCatch({
      backend_delete_task(db_conn, task_to_delete())
      showNotification("Task deleted", type = "message")
    }, error = function(e) {
      showNotification("Failed to delete task", type = "error")
    })
    
    # Cleanup
    task_to_delete(NULL)
    selected_task(NULL)
    removeModal() # Closes confirmation
    refresh_data()
  })

  # 5. Handle Cancel Click
  observeEvent(input$cancel_delete, {
    task_to_delete(NULL)
    removeModal() # Just close the confirmation
  })
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)