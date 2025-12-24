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
library(RMariaDB)
library(sodium)

DEFAULT_CREDENTIALS <- list(
  username = "admin",
  password_hash = sodium::password_store("admin123")
)

# =============================================================================
# DATABASE HELPERS
# =============================================================================

db_config <- function() {
  list(
    host = Sys.getenv("DB_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("DB_PORT", "3306")),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASSWORD", ""),
    dbname = "task_list_tracker_system"
  )
}

db_connect <- function() {
  cfg <- db_config()
  conn <- dbConnect(
    MariaDB(),
    dbname = cfg$dbname,
    host = cfg$host,
    port = cfg$port,
    user = cfg$user,
    password = cfg$password,
    timezone = "+00:00",
    encoding = "UTF-8"
  )

  dbExecute(conn, "SET time_zone = '+00:00'")
  conn
}

initialize_db <- function(conn) {
  dbExecute(conn, "DROP TABLE IF EXISTS users")
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS tasks (
      id INT AUTO_INCREMENT PRIMARY KEY,
      title VARCHAR(255) NOT NULL,
      description TEXT,
      priority ENUM('LOW','MEDIUM','HIGH','CRITICAL') NOT NULL DEFAULT 'LOW',
      status ENUM('TODO','IN_PROGRESS','OVERDUE','COMPLETED') NOT NULL DEFAULT 'TODO',
      start_datetime DATETIME NULL,
      due_datetime DATETIME NULL,
      completed_at DATETIME NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
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
    tasks$start_datetime <- as.POSIXct(tasks$start_datetime, tz = "UTC")
    tasks$due_datetime <- as.POSIXct(tasks$due_datetime, tz = "UTC")
    tasks$completed_at <- as.POSIXct(tasks$completed_at, tz = "UTC")
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
    "SELECT id, title, description, priority, status, start_datetime, due_datetime, completed_at FROM tasks WHERE id = ? LIMIT 1",
    params = list(task_id)
  )

  if (nrow(task) > 0) {
    task$start_datetime <- as.POSIXct(task$start_datetime, tz = "UTC")
    task$due_datetime <- as.POSIXct(task$due_datetime, tz = "UTC")
    task$completed_at <- as.POSIXct(task$completed_at, tz = "UTC")
  }

  task
}

backend_create_task <- function(conn, task) {
  dbExecute(
    conn,
    "INSERT INTO tasks (title, description, priority, status, start_datetime, due_datetime) VALUES (?, ?, ?, ?, ?, ?)",
    params = list(
      task$title,
      task$description,
      task$priority,
      task$status,
      format_db_time(task$start_datetime),
      format_db_time(task$due_datetime)
    )
  )

  new_id <- dbGetQuery(conn, "SELECT LAST_INSERT_ID() AS id")$id[1]
  backend_get_task(conn, new_id)
}

backend_update_task <- function(conn, task_id, updates) {
  if (length(updates) == 0) return(NULL)

  updates$updated_at <- Sys.time()
  update_names <- names(updates)
  set_clause <- paste(paste0(update_names, " = ?"), collapse = ", ")

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
    paste0("UPDATE tasks SET ", set_clause, " WHERE id = ?"),
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
  tasks <- backend_fetch_kanban(conn)
  notifications <- c()

  overdue <- tasks[tasks$status == "OVERDUE", ]
  overdue <- overdue[!is.na(overdue$due_datetime), ]
  if (nrow(overdue) > 0) {
    notifications <- c(notifications,
                       paste0("⚠️ Task '", overdue$title, "' is ",
                              round(as.numeric(difftime(Sys.time(), overdue$due_datetime, units = "days"))),
                              " days overdue"))
  }

  critical <- tasks[tasks$priority == "CRITICAL" & tasks$status != "COMPLETED", ]
  if (nrow(critical) > 0) {
    notifications <- c(notifications,
                       paste0("🔴 Critical task: '", critical$title, "'"))
  }

  return(notifications)
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
  
  priority_style <- priority_colors[[task$priority]]
  
  div(
    class = "task-card sortable-item",
    `data-task-id` = task$id,
    id = paste0("task_", task$id),
    
    div(
      style = "margin-bottom: 12px;",
      div(
        class = "task-title",
        task$title
      ),
      span(
        class = "priority-badge",
        style = sprintf(
          "background: %s; color: %s; border-color: %s;",
          priority_style$bg, priority_style$text, priority_style$border
        ),
        task$priority
      )
    ),
    
    div(
      class = "task-meta",
      icon("clock"),
      span(task$display_text)
    ),
    
    div(
      class = "task-actions",
      actionButton(
        ns(paste0("edit_", task$id)),
        "Edit",
        class = "btn-task-action",
        onclick = "event.stopPropagation();"
      ),
      if (task$status != "COMPLETED") {
        actionButton(
          ns(paste0("complete_", task$id)),
          HTML('<i class="fa fa-check"></i>'),
          class = "btn-task-complete",
          onclick = "event.stopPropagation();"
        )
      }
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
      }
      
      .kanban-column {
        flex: 0 0 320px;
        background: var(--bg-secondary);
        border-radius: 12px;
        padding: 16px;
        display: flex;
        flex-direction: column;
        border: 1px solid var(--border-color);
        box-shadow: var(--shadow-sm);
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
        padding: 16px;
        margin-bottom: 12px;
        box-shadow: var(--shadow-sm);
        cursor: grab;
        border: 1px solid var(--border-color);
        transition: all 0.2s;
      }
      
      .task-card:hover {
        box-shadow: var(--shadow-md);
        transform: translateY(-2px);
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
      }
      
      .priority-badge {
        display: inline-block;
        padding: 4px 10px;
        border-radius: 12px;
        font-size: 11px;
        font-weight: 600;
        border: 1px solid;
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
      
      /* === MODAL === */
      .modal-content {
        border-radius: 12px;
        border: none;
        box-shadow: 0 8px 32px rgba(0,0,0,0.3);
        background: var(--bg-secondary);
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
      tags$button(
        class = "hamburger-btn",
        onclick = "toggleSidebar()",
        icon("bars")
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
    "))
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

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
  
  # Load task data
  observe({
    req(logged_in())
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
          class = "empty-column",
          icon("inbox", style = "font-size: 32px; margin-bottom: 8px; display: block;"),
          "Drop tasks here"
        ))
      }
      
      div(
        class = "kanban-column",
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
            onEnd = htmlwidgets::JS(
              " 
              function(evt) {
                var taskId = evt.item.getAttribute('data-task-id');
                var targetId = (evt.to && evt.to.getAttribute('id')) || '';
                var newStatus = targetId.replace('rank_', '') || (evt.to && evt.to.dataset.status) || '';
                Shiny.setInputValue('task_moved', {
                  task_id: taskId,
                  new_status: newStatus,
                  timestamp: new Date().getTime()
                }, {priority: 'event'});
              }"
            )
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
  observe({
    tasks <- tasks_data()
    req(!is.null(tasks))
    
    lapply(tasks$id, function(task_id) {
      observeEvent(input[[paste0("edit_", task_id)]], {
        selected_task(tasks[tasks$id == task_id, ])
        showModal(task_modal(tasks[tasks$id == task_id, ], edit = TRUE))
      })

      observeEvent(input[[paste0("complete_", task_id)]], {
        backend_mark_completed(db_conn, task_id)
        refresh_data()
      })
    })
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
          width = "100%"
        )
      ),
      div(
        tags$label(class = "form-label", "Due Date/Time"),
        airDatepickerInput(
          "modal_due", NULL,
          value = if (edit) task$due_datetime else Sys.time() + 86400,
          timepicker = TRUE,
          width = "100%"
        )
      ),
      
      footer = tagList(
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

    status_data <- as.data.frame(table(tasks$status))
    colnames(status_data) <- c("Status", "Count")

    text_color <- if (identical(theme_mode(), "dark")) "#e4e6eb" else "#172b4d"

    ggplot(status_data, aes(x = 2, y = Count, fill = Status)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      xlim(c(0.5, 2.5)) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.text = element_text(color = text_color),
        legend.title = element_text(color = text_color),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      ) +
      scale_fill_brewer(palette = "Set2")
  })

  output$priority_bar <- renderPlot({
    req(logged_in())
    tasks <- tasks_data()
    req(!is.null(tasks))
    
    priority_data <- as.data.frame(table(tasks$priority))
    colnames(priority_data) <- c("Priority", "Count")
    priority_data$Priority <- factor(priority_data$Priority,
                                     levels = c("LOW", "MEDIUM", "HIGH", "CRITICAL"))

    text_color <- if (identical(theme_mode(), "dark")) "#e4e6eb" else "#172b4d"

    ggplot(priority_data, aes(x = Priority, y = Count, fill = Priority)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major.x = element_blank(),
        text = element_text(color = text_color),
        axis.title = element_text(color = text_color),
        axis.text = element_text(color = text_color)
      ) +
      scale_fill_manual(values = c("LOW" = "#28a745", "MEDIUM" = "#ffc107",
                                   "HIGH" = "#fd7e14", "CRITICAL" = "#dc3545"))
  })
  
  output$deadline_timeline <- renderPlot({
    req(logged_in())
    tasks <- tasks_data()
    req(!is.null(tasks))
    
    upcoming <- tasks[tasks$status != "COMPLETED" & tasks$due_datetime > Sys.time(), ]
    upcoming <- upcoming[order(upcoming$due_datetime), ]

    if (nrow(upcoming) > 0) {
      upcoming <- head(upcoming, 10)

      text_color <- if (identical(theme_mode(), "dark")) "#e4e6eb" else "#172b4d"

      ggplot(upcoming, aes(x = due_datetime, y = reorder(title, due_datetime))) +
        geom_point(aes(color = priority), size = 4) +
        geom_segment(aes(x = Sys.time(), xend = due_datetime,
                         y = reorder(title, due_datetime),
                         yend = reorder(title, due_datetime),
                         color = priority)) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          text = element_text(color = text_color),
          axis.title = element_text(color = text_color),
          axis.text = element_text(color = text_color),
          legend.text = element_text(color = text_color),
          legend.title = element_text(color = text_color)
        ) +
        labs(x = "Due Date", y = NULL) +
        scale_color_manual(values = c("LOW" = "#28a745", "MEDIUM" = "#ffc107",
                                      "HIGH" = "#fd7e14", "CRITICAL" = "#dc3545"))
    }
  })
  
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
  
  # Sidebar notifications
  output$sidebar_notifications <- renderUI({
    req(logged_in())
    notifications <- backend_fetch_notifications(db_conn)
    
    if (length(notifications) > 0) {
      lapply(notifications, function(notif) {
        is_critical <- grepl("🔴|Critical", notif)
        div(
          class = if (is_critical) "notification-item critical" else "notification-item",
          notif
        )
      })
    } else {
      div(
        class = "empty-column",
        style = "padding: 20px;",
        icon("bell-slash", style = "font-size: 24px; margin-bottom: 8px; display: block;"),
        "No notifications"
      )
    }
  })
  
  observeEvent(input$clear_notifications, {
    # REPLACE WITH REAL BACKEND: DELETE FROM notifications
    showNotification("Notifications cleared", type = "message")
  })
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)