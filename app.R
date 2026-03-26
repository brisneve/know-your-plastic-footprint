# ---------------------------------
# Plastic Footprint Calculator App
# ---------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

# -------------------------------
# Utility helpers
# -------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_trim <- function(x) {
  trimws(as.character(x %||% ""))
}

safe_numeric <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  x
}

fmt_num <- function(x, digits = 1) {
  format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x) {
  percent(x, accuracy = 0.1)
}

# -------------------------------
# Data and configuration
# -------------------------------

plastic_factors <- data.frame(
  key = c(
    "bags", "bottles", "sachets", "cups", "containers",
    "straws", "cutlery", "wrappers", "toiletry", "delivery"
  ),
  item = c(
    "Plastic bags",
    "PET bottles",
    "Sachets / single-use packets",
    "Takeaway cups",
    "Food containers",
    "Plastic straws",
    "Plastic cutlery sets",
    "Snack wrappers",
    "Shampoo / detergent bottles",
    "Online delivery packaging"
  ),
  unit = c(
    "pieces/week",
    "pieces/week",
    "pieces/week",
    "pieces/week",
    "pieces/week",
    "pieces/week",
    "sets/week",
    "pieces/week",
    "pieces/month",
    "packages/month"
  ),
  weight_g = c(5, 20, 3, 12, 25, 0.5, 8, 2, 35, 40),
  stringsAsFactors = FALSE
)

required_columns <- c(
  "id",
  "submitted_at",
  "name",
  "email",
  "role",
  "bags",
  "bottles",
  "sachets",
  "cups",
  "containers",
  "straws",
  "cutlery",
  "wrappers",
  "toiletry",
  "delivery",
  "reduction_goal_pct"
)

admin_password <- "w@ypl@stik@y"

# -------------------------------
# Local CSV database configuration
# -------------------------------

app_data_dir <- Sys.getenv("APP_DATA_DIR", unset = "data")
submissions_csv <- Sys.getenv(
  "PLASTIC_DB_CSV",
  unset = file.path(app_data_dir, "plastic_footprint_submissions.csv")
)

# -------------------------------
# Empty data / schema helpers
# -------------------------------

empty_submission_df <- function() {
  data.frame(
    id = numeric(),
    submitted_at = character(),
    name = character(),
    email = character(),
    role = character(),
    bags = numeric(),
    bottles = numeric(),
    sachets = numeric(),
    cups = numeric(),
    containers = numeric(),
    straws = numeric(),
    cutlery = numeric(),
    wrappers = numeric(),
    toiletry = numeric(),
    delivery = numeric(),
    reduction_goal_pct = numeric(),
    stringsAsFactors = FALSE
  )
}

coerce_submission_schema <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    df <- empty_submission_df()
  }
  
  missing_cols <- setdiff(required_columns, names(df))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) df[[col]] <- NA
  }
  
  df <- as.data.frame(df[, required_columns, drop = FALSE], stringsAsFactors = FALSE)
  
  numeric_cols <- c(
    "id", "bags", "bottles", "sachets", "cups", "containers",
    "straws", "cutlery", "wrappers", "toiletry", "delivery", "reduction_goal_pct"
  )
  
  char_cols <- c("submitted_at", "name", "email", "role")
  
  for (col in numeric_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  
  for (col in char_cols) {
    df[[col]] <- as.character(df[[col]])
  }
  
  df
}

# -------------------------------
# Local CSV helpers
# -------------------------------

ensure_data_dir <- function() {
  dir_path <- dirname(submissions_csv)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(TRUE)
}

ensure_csv_ready <- function() {
  ensure_data_dir()
  
  if (!file.exists(submissions_csv)) {
    utils::write.csv(
      empty_submission_df(),
      submissions_csv,
      row.names = FALSE,
      na = ""
    )
  }
  
  invisible(TRUE)
}

read_submissions_csv <- function() {
  ensure_csv_ready()
  
  df <- tryCatch(
    utils::read.csv(
      submissions_csv,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA")
    ),
    error = function(e) empty_submission_df()
  )
  
  coerce_submission_schema(df)
}

write_submissions_csv <- function(df) {
  ensure_csv_ready()
  df <- coerce_submission_schema(df)
  
  utils::write.csv(
    df[, required_columns, drop = FALSE],
    submissions_csv,
    row.names = FALSE,
    na = ""
  )
  
  invisible(TRUE)
}

append_submission_csv <- function(new_entry) {
  ensure_csv_ready()
  
  df <- read_submissions_csv()
  next_id <- if (nrow(df) == 0 || all(is.na(df$id))) 1 else max(df$id, na.rm = TRUE) + 1
  
  new_entry$id <- next_id
  new_entry <- coerce_submission_schema(new_entry)
  
  updated_df <- dplyr::bind_rows(df, new_entry[, required_columns, drop = FALSE])
  write_submissions_csv(updated_df)
  
  invisible(next_id)
}

# -------------------------------
# Theme and CSS
# -------------------------------

forest_theme <- bs_theme(
  version = 5,
  bg = "#F4F7F4",
  fg = "#1F2A1F",
  primary = "#2F6B3B",
  secondary = "#6B7280",
  success = "#2F6B3B",
  info = "#3F7D47",
  warning = "#D4A017",
  danger = "#A63D40"
)

loading_gif_src <- "data:image/gif;base64,R0lGODlhQABAAIMAAP///y9rOy9rOy9rOy9rOy9rOy9rOy9rOy9rOwAAAAAAAAAAAAAAAAAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh+QQJCAAAACwAAAAAQABAAAAI/wABCBxIsKDBgwgTKlzIsKHDhxAjSpxIsaLFixgzatzIsaPHjyBDihxJsiTIAyhNVkTJMqXKhQRiDmzJcmCAmy8HxtwZk2bLm0BxvuS50yfLoECHEu1pFGlQlUuL+nSa1GRUpjSpCrW6dKZLAFRJ7hQYNWFYsFs1lgUwlmHVsxnXVoR78SqBuVrjXsVLt6Jdvk7VdrXY16JcioUrGjDQNiNQBAgyLp68eCPky5AvUp5sGXNmxZs5Y/SMGXRoAxlJXzYdOrXqyKxFu/asubVG1ZIXDxiwUYDvzxl3C9990bdx3xuHCy9+HHlw5cspNj/+HDpv6dOdX7ROHHt2jdaZN11PDl1g9IXGBX7nGD08wvXpR7oHUKD+wPUmuQ+oz79+du0k6dcff/8JoJKAA/qHX0nWJUjgdDk16GABCx7YHX0J3gdgTgY5yGFE/H0o4ogklmjiiSimqOKKLLYYUkAAOw=="

custom_css <- "
:root {
  --forest: #2F6B3B;
  --forest-dark: #1F4D2A;
  --forest-light: #EAF4EC;
  --page-bg: #F4F7F4;
  --panel-bg: #FFFFFF;
  --border: #D7E3D8;
  --text-main: #1F2A1F;
  --text-muted: #667085;
  --row-bg: #F9FBF9;
}

html, body {
  margin: 0;
  padding: 0;
  background-color: var(--page-bg);
  color: var(--text-main);
  font-family: Calibri, Carlito, 'Segoe UI', Arial, sans-serif !important;
  overflow-x: hidden;
  overflow-y: auto !important;
  min-height: 100%;
  width: 100%;
}

body {
  font-size: 15px;
  line-height: 1.45;
}

.navbar {
  background: linear-gradient(90deg, var(--forest-dark), var(--forest)) !important;
  border-bottom: 1px solid rgba(255,255,255,0.08);
  box-shadow: 0 2px 10px rgba(0,0,0,0.08);
}

.navbar-brand,
.navbar-brand:visited,
.navbar-brand:hover,
.navbar-brand:focus {
  color: #FFFFFF !important;
  font-weight: 700 !important;
  margin-right: 28px !important;
}

.navbar-nav {
  margin-left: 20px !important;
}

.navbar-nav .nav-item {
  margin-left: 10px !important;
}

.navbar-nav .nav-link,
.navbar-nav .nav-link:visited,
.navbar-nav .nav-link:hover,
.navbar-nav .nav-link:focus,
.navbar-nav .nav-link.active,
.navbar-nav .active > .nav-link {
  color: #FFFFFF !important;
  font-weight: 600 !important;
  padding-left: 12px !important;
  padding-right: 12px !important;
}

.container-fluid {
  padding-top: 18px;
  padding-bottom: 24px;
  padding-left: 14px;
  padding-right: 14px;
  max-width: 100%;
}

.form-shell {
  width: 100%;
  max-width: 1100px;
  margin: 0 auto 22px auto;
}

.form-panel,
.chart-panel {
  background: var(--panel-bg);
  border: 1px solid var(--border);
  border-radius: 20px;
  padding: 22px 24px;
  box-shadow: 0 4px 16px rgba(15, 23, 42, 0.05);
  margin-bottom: 18px;
}

.section-title {
  font-size: 1.15rem;
  font-weight: 700;
  color: var(--forest-dark);
  margin-bottom: 8px;
}

.section-subtitle {
  color: var(--text-muted);
  font-size: 0.96rem;
  margin-bottom: 16px;
}

.form-control, .form-select, .shiny-input-container input, .shiny-input-container select {
  border-radius: 10px !important;
  border: 1px solid #CBD5E1 !important;
  box-shadow: none !important;
  width: 100%;
  max-width: 100%;
}

.form-control:focus, .form-select:focus {
  border-color: var(--forest) !important;
  box-shadow: 0 0 0 0.2rem rgba(47, 107, 59, 0.10) !important;
}

.shiny-input-container {
  width: 100% !important;
  max-width: 100%;
  margin-bottom: 0 !important;
}

.btn {
  border-radius: 999px !important;
  font-weight: 700 !important;
  padding: 0.65rem 1.1rem !important;
  white-space: normal;
}

.btn-primary,
.btn-default {
  background-color: var(--forest) !important;
  border-color: var(--forest) !important;
  color: #FFFFFF !important;
}

.btn-primary:hover,
.btn-default:hover {
  background-color: var(--forest-dark) !important;
  border-color: var(--forest-dark) !important;
  color: #FFFFFF !important;
}

.action-center {
  display: flex;
  justify-content: center;
  margin-top: 24px;
  gap: 12px;
  flex-wrap: wrap;
}

.action-center .btn {
  min-width: 220px;
}

.table-responsive {
  width: 100%;
  overflow-x: auto;
  -webkit-overflow-scrolling: touch;
}

.input-table {
  width: 100%;
  border-collapse: collapse;
  margin-bottom: 1rem;
}

.input-table thead th {
  background: var(--forest-light);
  color: var(--forest-dark);
  font-weight: 700;
  border: 1px solid var(--border);
  padding: 12px 10px;
  font-size: 0.96rem;
}

.input-table tbody td {
  padding: 10px;
  border: 1px solid var(--border);
  vertical-align: middle;
}

.input-table tbody tr:nth-child(odd) {
  background: var(--row-bg);
}

.calc-card-shell {
  width: 100%;
  display: flex;
  justify-content: center;
}

.calc-card-band {
  width: 75%;
  min-width: 320px;
  max-width: 1000px;
}

.calc-table-shell {
  width: 100%;
  display: flex;
  justify-content: center;
}

.calc-table-band {
  width: 100%;
}

.input-table .item-unit-col {
  font-weight: 600;
  color: var(--text-main);
  min-width: 260px;
}

.input-table .input-col {
  min-width: 130px;
}

.input-cell .form-group,
.input-cell .shiny-input-container {
  margin-bottom: 0 !important;
}

.slider-note {
  margin-top: 10px;
  margin-bottom: 0;
  color: var(--text-muted);
  font-size: 0.92rem;
  text-align: center;
}

.slider-shell {
  width: 100%;
  max-width: 520px;
  margin: 0 auto;
}

.slider-block {
  margin-top: 10px;
  margin-bottom: 20px;
}

.result-kpi-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 12px;
  margin-bottom: 14px;
}

.result-kpi {
  background: var(--forest-light);
  border: 1px solid #D7EAD9;
  border-radius: 14px;
  padding: 14px;
  text-align: center;
}

.result-kpi-label {
  font-size: 0.92rem;
  color: var(--text-muted);
  margin-bottom: 4px;
}

.result-kpi-value {
  font-size: 1.35rem;
  font-weight: 700;
  color: var(--forest-dark);
}

.dashboard-grid {
  display: grid;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 14px;
  margin-bottom: 18px;
}

.dashboard-card {
  background: linear-gradient(180deg, #FFFFFF 0%, #F8FAF8 100%);
  border: 1px solid var(--border);
  border-radius: 18px;
  padding: 18px;
  box-shadow: 0 4px 16px rgba(15, 23, 42, 0.04);
}

.dashboard-label {
  color: var(--text-muted);
  font-size: 0.9rem;
  margin-bottom: 6px;
}

.dashboard-value {
  color: var(--forest-dark);
  font-size: 1.5rem;
  font-weight: 700;
  line-height: 1.15;
  word-break: break-word;
}

.dashboard-sub {
  color: var(--text-muted);
  font-size: 0.85rem;
  margin-top: 6px;
}

.note-box {
  background: #F8FAF8;
  border: 1px solid var(--border);
  border-radius: 12px;
  padding: 12px 14px;
  margin-bottom: 14px;
  color: var(--text-main);
}

.filter-row {
  display: grid;
  grid-template-columns: 300px 1fr;
  gap: 16px;
  align-items: end;
  margin-bottom: 14px;
}

.admin-shell {
  max-width: 700px;
  margin: 0 auto;
}

.admin-status {
  margin-top: 12px;
}

.loading-wrapper {
  position: relative;
  min-height: 160px;
}

.loading-wrapper.loading-wrapper-plot {
  min-height: 420px;
}

.loading-wrapper.loading-wrapper-table {
  min-height: 240px;
}

.loading-output {
  min-height: inherit;
}

.loading-overlay {
  display: none;
  position: absolute;
  inset: 0;
  z-index: 20;
  background: rgba(244, 247, 244, 0.88);
  border-radius: 14px;
  align-items: center;
  justify-content: center;
  text-align: center;
  padding: 18px;
}

.loading-overlay-box {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 10px;
}

.loading-overlay-box img {
  width: 48px;
  height: 48px;
}

.loading-overlay-text {
  color: var(--text-muted);
  font-size: 0.95rem;
  font-weight: 600;
}

.loading-wrapper:has(.recalculating) .loading-overlay,
.loading-wrapper:has(.shiny-output-error) .loading-overlay,
.loading-wrapper:has(.html-widget-output.recalculating) .loading-overlay,
.loading-wrapper:has(.shiny-plot-output.recalculating) .loading-overlay,
.loading-wrapper:has(.shiny-bound-output.recalculating) .loading-overlay {
  display: flex;
}

.admin-login-shell {
  width: 100%;
  display: flex;
  justify-content: center;
}

.admin-login-band {
  width: 60%;
  min-width: 320px;
  max-width: 520px;
}

@media (max-width: 991.98px) {
  .dashboard-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }

  .filter-row {
    grid-template-columns: 1fr;
  }

  .calc-card-band {
    width: 85%;
  }

  .admin-login-band {
    width: 80%;
  }
}

@media (max-width: 767.98px) {
  body {
    font-size: 14px;
  }

  .container-fluid {
    padding-left: 10px;
    padding-right: 10px;
  }

  .form-panel, .chart-panel {
    padding: 14px;
    border-radius: 16px;
  }

  .action-center .btn,
  .download-btn {
    width: 100%;
    min-width: unset;
  }

  .calc-card-band {
    width: 100%;
    min-width: 0;
  }

  .dashboard-grid,
  .result-kpi-grid {
    grid-template-columns: 1fr;
  }

  .dashboard-value {
    font-size: 1.25rem;
  }

  .admin-login-band {
    width: 100%;
  }
}
"

loading_block <- function(output_ui, text = "Loading results...", min_height = "160px", extra_class = "") {
  div(
    class = paste("loading-wrapper", extra_class),
    style = paste0("min-height:", min_height, ";"),
    div(class = "loading-output", output_ui),
    div(
      class = "loading-overlay",
      div(
        class = "loading-overlay-box",
        tags$img(src = loading_gif_src, alt = "Loading"),
        div(class = "loading-overlay-text", text)
      )
    )
  )
}

# -------------------------------
# UI
# -------------------------------

ui <- page_navbar(
  id = "main_nav",
  title = div(style = "font-weight:700;", "Know Your Plastic Footprint"),
  theme = forest_theme,
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, shrink-to-fit=no"),
    tags$style(HTML(custom_css))
  ),
  
  nav_panel(
    "Calculator",
    div(
      class = "form-shell",
      div(
        class = "calc-card-shell",
        div(
          class = "calc-card-band",
          div(
            class = "form-panel",
            div(class = "section-title", "Plastic Use Table"),
            p(class = "section-subtitle", "Enter the number of plastic items you typically use."),
            
            div(
              class = "table-responsive",
              tags$table(
                class = "input-table",
                tags$thead(
                  tags$tr(
                    tags$th("Plastic Item / Unit"),
                    tags$th("Input")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("Plastic bags<br><span style='font-weight:400; color:#667085;'>pieces/week</span>")),
                    tags$td(class = "input-col input-cell", numericInput("bags", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  ),
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("PET bottles<br><span style='font-weight:400; color:#667085;'>pieces/week</span>")),
                    tags$td(class = "input-col input-cell", numericInput("bottles", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  ),
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("Sachets / single-use packets<br><span style='font-weight:400; color:#667085;'>pieces/week</span>")),
                    tags$td(class = "input-col input-cell", numericInput("sachets", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  ),
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("Takeaway cups<br><span style='font-weight:400; color:#667085;'>pieces/week</span>")),
                    tags$td(class = "input-col input-cell", numericInput("cups", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  ),
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("Food containers<br><span style='font-weight:400; color:#667085;'>pieces/week</span>")),
                    tags$td(class = "input-col input-cell", numericInput("containers", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  ),
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("Plastic straws<br><span style='font-weight:400; color:#667085;'>pieces/week</span>")),
                    tags$td(class = "input-col input-cell", numericInput("straws", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  ),
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("Plastic cutlery sets<br><span style='font-weight:400; color:#667085;'>sets/week</span>")),
                    tags$td(class = "input-col input-cell", numericInput("cutlery", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  ),
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("Snack wrappers<br><span style='font-weight:400; color:#667085;'>pieces/week</span>")),
                    tags$td(class = "input-col input-cell", numericInput("wrappers", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  ),
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("Shampoo / detergent bottles<br><span style='font-weight:400; color:#667085;'>pieces/month</span>")),
                    tags$td(class = "input-col input-cell", numericInput("toiletry", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  ),
                  tags$tr(
                    tags$td(class = "item-unit-col", HTML("Online delivery packaging<br><span style='font-weight:400; color:#667085;'>packages/month</span>")),
                    tags$td(class = "input-col input-cell", numericInput("delivery", NULL, value = 0, min = 0, step = 1, width = "100%"))
                  )
                )
              )
            ),
            
            hr(),
            
            div(
              class = "slider-shell",
              h5(
                style = "font-weight:700; color:#1F4D2A; text-align:center; margin-bottom:12px;",
                "Reduction Goal"
              ),
              div(
                class = "slider-block",
                sliderInput(
                  "reduction_goal",
                  NULL,
                  min = 0,
                  max = 100,
                  value = 50,
                  step = 5,
                  post = "%"
                ),
                uiOutput("reduction_goal_label")
              )
            ),
            
            div(
              class = "action-center",
              actionButton("view_results", "Calculate Plastic Footprint", class = "btn-primary")
            ),
            br(),
            uiOutput("save_status")
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Overall Results",
    div(
      class = "form-shell",
      div(
        class = "form-panel",
        div(class = "section-title", "Overall Results Dashboard"),
        div(
          class = "filter-row",
          selectInput(
            "role_filter",
            "Respondent Category",
            choices = c("All", "Student", "Faculty", "Staff", "Others"),
            selected = "All"
          ),
          div()
        ),
        loading_block(
          uiOutput("overall_dashboard"),
          text = "Loading dashboard...",
          min_height = "180px"
        )
      ),
      div(
        class = "chart-panel",
        div(class = "section-title", "Overall Distributions"),
        p(
          class = "section-subtitle",
          "View the percentage distribution of plastic use by estimated weight and the percentage distribution of reduction goals."
        ),
        tabsetPanel(
          id = "overall_results_tabs",
          tabPanel(
            "Plastic Use by Weight (%)",
            br(),
            loading_block(
              plotlyOutput("overall_weight_pct_plot", width = "100%", height = "460px"),
              text = "Loading chart...",
              min_height = "460px",
              extra_class = "loading-wrapper-plot"
            )
          ),
          tabPanel(
            "Reduction Goal Distribution",
            br(),
            loading_block(
              plotlyOutput("overall_goal_plot", width = "100%", height = "460px"),
              text = "Loading chart...",
              min_height = "460px",
              extra_class = "loading-wrapper-plot"
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Recommendations",
    div(
      class = "form-shell",
      div(
        class = "form-panel",
        div(class = "section-title", "How to Reduce Your Plastic Footprint"),
        p(class = "section-subtitle", "Practical ways to reduce your plastic footprint."),
        tags$ol(
          class = "plain-list",
          tags$li(tags$strong("Bring reusables"), ": Carry a tumbler, utensils, food container, and eco-bag."),
          tags$li(tags$strong("Avoid sachets"), ": Choose refill or bulk options when possible."),
          tags$li(tags$strong("Refuse extras"), ": Decline straws, disposable cutlery, and unnecessary packaging."),
          tags$li(tags$strong("Choose refillable packaging"), ": Prefer reusable or refill systems."),
          tags$li(tags$strong("Track your habits"), ": Set small weekly goals and monitor progress."),
          tags$li(tags$strong("Segregate properly"), ": Clean and sort recyclable plastics.")
        )
      )
    )
  ),
  
  nav_panel(
    "Method",
    div(
      class = "form-shell",
      div(
        class = "form-panel",
        div(class = "section-title", "How the Calculation Works"),
        p("Each item is assigned an average weight in grams. Monthly entries are converted to weekly equivalents before totals are calculated."),
        withMathJax(
          div(
            p("$$\\text{Plastic Footprint (g/week)} = \\sum (\\text{Number of Items} \\times \\text{Average Grams per Item})$$"),
            p("$$\\text{Monthly Footprint} \\approx \\text{Weekly Footprint} \\times 4.345$$"),
            p("$$\\text{Annual Footprint} = \\text{Weekly Footprint} \\times 52$$"),
            p("$$\\text{Reduced Annual Footprint} = \\text{Annual Footprint} \\times (1 - \\text{Reduction Goal})$$")
          )
        ),
        br(),
        div(class = "section-title", style = "font-size:1rem; margin-top:8px;", "Default Item Weights"),
        div(
          style = "display:flex; justify-content:center;",
          div(
            class = "table-responsive",
            style = "max-width: 800px; width: 100%;",
            loading_block(
              tableOutput("factor_table"),
              text = "Loading table...",
              min_height = "220px",
              extra_class = "loading-wrapper-table"
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Admin",
    div(
      class = "form-shell admin-shell",
      div(
        class = "form-panel",
        div(
          class = "admin-login-shell",
          div(
            class = "admin-login-band",
            div(class = "section-title", "Admin Access"),
            p(class = "section-subtitle", "Enter the admin password to view and manage submissions."),
            passwordInput("admin_password_input", "Password"),
            div(
              class = "action-center",
              actionButton("admin_login", "Unlock Admin Panel", class = "btn-primary")
            ),
            div(class = "admin-status", uiOutput("admin_status")),
            br(),
            uiOutput("admin_controls")
          )
        )
      )
    )
  )
)

# -------------------------------
# Server
# -------------------------------

server <- function(input, output, session) {
  save_msg <- reactiveVal("")
  save_msg_type <- reactiveVal("secondary")
  preview_entry <- reactiveVal(NULL)
  data_refresh <- reactiveVal(0)
  admin_unlocked <- reactiveVal(FALSE)
  admin_msg <- reactiveVal("")
  admin_msg_type <- reactiveVal("secondary")
  
  reset_form <- function() {
    updateNumericInput(session, "bags", value = 0)
    updateNumericInput(session, "bottles", value = 0)
    updateNumericInput(session, "sachets", value = 0)
    updateNumericInput(session, "cups", value = 0)
    updateNumericInput(session, "containers", value = 0)
    updateNumericInput(session, "straws", value = 0)
    updateNumericInput(session, "cutlery", value = 0)
    updateNumericInput(session, "wrappers", value = 0)
    updateNumericInput(session, "toiletry", value = 0)
    updateNumericInput(session, "delivery", value = 0)
    updateSliderInput(session, "reduction_goal", value = 50)
  }
  
  output$reduction_goal_label <- renderUI({
    req(input$reduction_goal)
    
    goal_text <- dplyr::case_when(
      input$reduction_goal == 0 ~ "No planned reduction",
      input$reduction_goal > 0 & input$reduction_goal <= 25 ~ "Small reduction target",
      input$reduction_goal > 25 & input$reduction_goal <= 50 ~ "Moderate reduction target",
      input$reduction_goal > 50 & input$reduction_goal <= 75 ~ "High reduction target",
      input$reduction_goal > 75 ~ "Very high reduction target",
      TRUE ~ "Reduction target"
    )
    
    div(
      class = "slider-note",
      paste0("Selected goal: ", input$reduction_goal, "% reduction — ", goal_text)
    )
  })
  
  usage_data <- reactive({
    weekly_counts <- c(
      safe_numeric(input$bags),
      safe_numeric(input$bottles),
      safe_numeric(input$sachets),
      safe_numeric(input$cups),
      safe_numeric(input$containers),
      safe_numeric(input$straws),
      safe_numeric(input$cutlery),
      safe_numeric(input$wrappers),
      safe_numeric(input$toiletry) / 4.345,
      safe_numeric(input$delivery) / 4.345
    )
    
    df <- plastic_factors %>%
      mutate(
        count_per_week = weekly_counts,
        grams_per_week = count_per_week * weight_g
      )
    
    total_grams <- sum(df$grams_per_week, na.rm = TRUE)
    
    df %>%
      mutate(
        share = if (total_grams > 0) grams_per_week / total_grams else 0
      )
  })
  
  totals <- reactive({
    total_week_g <- sum(usage_data()$grams_per_week, na.rm = TRUE)
    reduction_fraction <- safe_numeric(input$reduction_goal) / 100
    
    list(
      weekly_g = total_week_g,
      monthly_g = total_week_g * 4.345,
      annual_g = total_week_g * 52,
      reduced_annual_g = total_week_g * 52 * (1 - reduction_fraction),
      avoided_annual_g = total_week_g * 52 * reduction_fraction
    )
  })
  
  output$factor_table <- renderTable({
    plastic_factors %>%
      transmute(
        `Plastic Item` = item,
        Unit = unit,
        `Average grams per item` = weight_g
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  observeEvent(input$view_results, {
    preview_entry(
      data.frame(
        submitted_at = as.character(Sys.time()),
        name = NA_character_,
        email = NA_character_,
        role = NA_character_,
        bags = safe_numeric(input$bags),
        bottles = safe_numeric(input$bottles),
        sachets = safe_numeric(input$sachets),
        cups = safe_numeric(input$cups),
        containers = safe_numeric(input$containers),
        straws = safe_numeric(input$straws),
        cutlery = safe_numeric(input$cutlery),
        wrappers = safe_numeric(input$wrappers),
        toiletry = safe_numeric(input$toiletry),
        delivery = safe_numeric(input$delivery),
        reduction_goal_pct = safe_numeric(input$reduction_goal),
        stringsAsFactors = FALSE
      )
    )
    
    showModal(
      modalDialog(
        title = "Your Plastic Footprint Results",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close"),
          actionButton("open_save_modal", "Submit Results", class = "btn-primary")
        ),
        
        div(
          class = "result-kpi-grid",
          div(
            class = "result-kpi",
            div(class = "result-kpi-label", "Weekly Footprint"),
            div(class = "result-kpi-value", paste0(fmt_num(totals()$weekly_g), " g"))
          ),
          div(
            class = "result-kpi",
            div(class = "result-kpi-label", "Monthly Footprint"),
            div(class = "result-kpi-value", paste0(fmt_num(totals()$monthly_g), " g"))
          ),
          div(
            class = "result-kpi",
            div(class = "result-kpi-label", "Annual Footprint"),
            div(class = "result-kpi-value", paste0(fmt_num(totals()$annual_g), " g"))
          )
        ),
        
        div(
          class = "result-kpi-grid",
          div(
            class = "result-kpi",
            div(class = "result-kpi-label", "Reduction Goal"),
            div(class = "result-kpi-value", paste0(safe_numeric(input$reduction_goal), "%"))
          ),
          div(
            class = "result-kpi",
            div(class = "result-kpi-label", "Reduced Annual Footprint"),
            div(class = "result-kpi-value", paste0(fmt_num(totals()$reduced_annual_g), " g"))
          ),
          div(
            class = "result-kpi",
            div(class = "result-kpi-label", "Potential Annual Plastic Avoided"),
            div(class = "result-kpi-value", paste0(fmt_num(totals()$avoided_annual_g), " g"))
          )
        ),
        
        loading_block(
          plotlyOutput("individual_weight_plot", height = "420px"),
          text = "Loading results...",
          min_height = "420px",
          extra_class = "loading-wrapper-plot"
        )
      )
    )
  })
  
  output$individual_weight_plot <- renderPlotly({
    df <- usage_data() %>%
      mutate(
        label = paste0(
          "<b>", item, "</b><br>",
          "Estimated weight: ", fmt_num(grams_per_week), " g/week<br>",
          "Share: ", fmt_pct(share)
        )
      )
    
    p <- ggplot(df, aes(
      x = reorder(item, grams_per_week),
      y = grams_per_week,
      text = label
    )) +
      geom_col(fill = "#A7D7A9", width = 0.75) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Estimated plastic use (g/week)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text = element_text(color = "#374151"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 120, r = 20, t = 20, b = 60),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  observeEvent(input$open_save_modal, {
    removeModal()
    
    showModal(
      modalDialog(
        title = "Confirm Submission",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_save", "Confirm and Submit", class = "btn-primary")
        ),
        textInput("respondent_name", "Name"),
        textInput("respondent_email", "Email"),
        selectInput(
          "respondent_role",
          "Role",
          choices = c("Student", "Faculty", "Staff", "Others"),
          selected = "Student"
        )
      )
    )
  })
  
  observeEvent(input$confirm_save, {
    req(preview_entry())
    
    new_entry <- preview_entry()
    new_entry$name <- safe_trim(input$respondent_name)
    new_entry$email <- safe_trim(input$respondent_email)
    new_entry$role <- safe_trim(input$respondent_role)
    
    tryCatch(
      {
        append_submission_csv(new_entry)
        data_refresh(data_refresh() + 1)
        save_msg(paste("Entry saved successfully to local CSV:", submissions_csv))
        save_msg_type("success")
        removeModal()
        reset_form()
        
        bslib::nav_select(id = "main_nav", selected = "Overall Results", session = session)
      },
      error = function(e) {
        save_msg(paste("Unable to save entry:", conditionMessage(e)))
        save_msg_type("danger")
      }
    )
  })
  
  output$save_status <- renderUI({
    req(nzchar(save_msg()))
    div(
      class = paste0("alert alert-", save_msg_type()),
      style = "padding:10px; margin-top:5px;",
      save_msg()
    )
  })
  
  all_submissions <- reactive({
    data_refresh()
    
    tryCatch(
      {
        df <- read_submissions_csv()
        if ("id" %in% names(df) && nrow(df) > 0) {
          df <- df %>% arrange(desc(id))
        }
        df
      },
      error = function(e) {
        admin_msg(paste("Local CSV error:", conditionMessage(e)))
        admin_msg_type("danger")
        empty_submission_df()
      }
    )
  })
  
  filtered_submissions <- reactive({
    df <- all_submissions()
    req(input$role_filter)
    
    if (nrow(df) == 0) return(df)
    if (input$role_filter == "All") return(df)
    
    df %>% filter(role == input$role_filter)
  })
  
  weight_summary <- reactive({
    df <- filtered_submissions()
    
    if (nrow(df) == 0) {
      return(data.frame(
        key = plastic_factors$key,
        item = plastic_factors$item,
        total_g = 0,
        pct_weight = 0
      ))
    }
    
    item_totals <- sapply(plastic_factors$key, function(k) {
      vals <- if (k %in% names(df)) df[[k]] else rep(0, nrow(df))
      vals <- safe_numeric(vals)
      
      if (k %in% c("toiletry", "delivery")) {
        vals <- vals / 4.345
      }
      
      vals
    })
    
    if (is.null(dim(item_totals))) {
      item_totals <- matrix(item_totals, nrow = nrow(df))
      colnames(item_totals) <- plastic_factors$key
    }
    
    total_g <- colSums(
      sweep(item_totals, 2, plastic_factors$weight_g, `*`),
      na.rm = TRUE
    )
    
    total_sum <- sum(total_g, na.rm = TRUE)
    
    data.frame(
      key = plastic_factors$key,
      item = plastic_factors$item,
      total_g = as.numeric(total_g),
      pct_weight = if (total_sum > 0) as.numeric(total_g) / total_sum else 0
    )
  })
  
  output$overall_dashboard <- renderUI({
    df <- filtered_submissions()
    ws <- weight_summary()
    
    total_submissions <- nrow(df)
    total_weekly_g <- sum(ws$total_g, na.rm = TRUE)
    avg_weekly_g <- if (total_submissions > 0) total_weekly_g / total_submissions else 0
    avg_annual_g <- avg_weekly_g * 52
    avg_goal <- if (total_submissions > 0) mean(safe_numeric(df$reduction_goal_pct), na.rm = TRUE) else 0
    
    top_item <- if (nrow(ws) > 0) ws$item[which.max(ws$total_g)] else "N/A"
    top_share <- if (nrow(ws) > 0 && sum(ws$total_g) > 0) max(ws$pct_weight, na.rm = TRUE) else 0
    
    div(
      class = "dashboard-grid",
      div(
        class = "dashboard-card",
        div(class = "dashboard-label", "Total submissions"),
        div(class = "dashboard-value", fmt_num(total_submissions, 0)),
        div(class = "dashboard-sub", "Saved responses in local CSV")
      ),
      div(
        class = "dashboard-card",
        div(class = "dashboard-label", "Average annual footprint"),
        div(class = "dashboard-value", paste0(fmt_num(avg_annual_g), " g")),
        div(class = "dashboard-sub", "Weekly estimate multiplied by 52")
      ),
      div(
        class = "dashboard-card",
        div(class = "dashboard-label", "Average reduction goal"),
        div(class = "dashboard-value", paste0(fmt_num(avg_goal), "%")),
        div(class = "dashboard-sub", "Mean reported target")
      ),
      div(
        class = "dashboard-card",
        div(class = "dashboard-label", "Highest plastic contributor"),
        div(class = "dashboard-value", top_item),
        div(class = "dashboard-sub", if (top_item == "N/A") "No data yet" else paste0("Share of weight: ", fmt_pct(top_share)))
      )
    )
  })
  
  output$overall_weight_pct_plot <- renderPlotly({
    ws <- weight_summary()
    
    if (sum(ws$total_g, na.rm = TRUE) <= 0) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "No submissions available yet.",
                x = 0.5, y = 0.5,
                xref = "paper", yref = "paper",
                showarrow = FALSE,
                font = list(size = 16)
              )
            )
          )
      )
    }
    
    ws <- ws %>%
      mutate(
        text = paste0(
          "<b>", item, "</b><br>",
          "Estimated weight: ", fmt_num(total_g), " g<br>",
          "Share: ", fmt_pct(pct_weight)
        )
      )
    
    p <- ggplot(ws, aes(
      x = reorder(item, pct_weight),
      y = pct_weight,
      text = text
    )) +
      geom_col(fill = "#A7D7A9", width = 0.75) +
      coord_flip() +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        x = NULL,
        y = "Percentage of total plastic weight"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text = element_text(color = "#374151"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 140, r = 20, t = 20, b = 55),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$overall_goal_plot <- renderPlotly({
    df <- filtered_submissions()
    
    if (nrow(df) == 0 || !"reduction_goal_pct" %in% names(df)) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "No submissions available yet.",
                x = 0.5, y = 0.5,
                xref = "paper", yref = "paper",
                showarrow = FALSE,
                font = list(size = 16)
              )
            )
          )
      )
    }
    
    goal_df <- df %>%
      mutate(reduction_goal_pct = safe_numeric(reduction_goal_pct)) %>%
      count(reduction_goal_pct, name = "n") %>%
      mutate(pct = n / sum(n)) %>%
      arrange(reduction_goal_pct)
    
    if (nrow(goal_df) == 0) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "No submissions available yet.",
                x = 0.5, y = 0.5,
                xref = "paper", yref = "paper",
                showarrow = FALSE,
                font = list(size = 16)
              )
            )
          )
      )
    }
    
    p <- ggplot(goal_df, aes(
      x = factor(reduction_goal_pct),
      y = pct,
      text = paste0(
        "<b>", reduction_goal_pct, "%</b><br>",
        "Submissions: ", n, "<br>",
        "Share: ", percent(pct, accuracy = 0.1)
      )
    )) +
      geom_col(fill = "#A7D7A9", width = 0.75) +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        x = "Reduction goal (%)",
        y = "Percentage of submissions"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text = element_text(color = "#374151"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 60, r = 20, t = 20, b = 55),
        dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$admin_status <- renderUI({
    msg <- admin_msg()
    if (!nzchar(msg)) return(NULL)
    
    div(
      class = paste0("alert alert-", admin_msg_type()),
      style = "padding:10px;",
      msg
    )
  })
  
  output$admin_controls <- renderUI({
    if (!isTRUE(admin_unlocked())) {
      return(
        div(
          class = "note-box",
          "The admin panel is locked. Enter the correct password to enable admin tools."
        )
      )
    }
    
    df <- all_submissions()
    
    tagList(
      div(
        class = "note-box",
        paste0("Admin access granted. Current number of submissions: ", nrow(df), ".")
      ),
      div(
        class = "action-center",
        downloadButton("download_csv", "Download CSV File", class = "download-btn"),
        actionButton("admin_logout", "Sign Out", class = "btn btn-outline-secondary")
      )
    )
  })
  
  observeEvent(input$admin_login, {
    if (identical(input$admin_password_input, admin_password)) {
      admin_unlocked(TRUE)
      admin_msg("Admin access granted.")
      admin_msg_type("success")
      updateTextInput(session, "admin_password_input", value = "")
    } else {
      admin_unlocked(FALSE)
      admin_msg("Incorrect password.")
      admin_msg_type("danger")
    }
  })
  
  observeEvent(input$admin_logout, {
    admin_unlocked(FALSE)
    admin_msg("You have signed out of the admin panel.")
    admin_msg_type("secondary")
    updateTextInput(session, "admin_password_input", value = "")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("plastic_footprint_submissions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- tryCatch(
        read_submissions_csv(),
        error = function(e) empty_submission_df()
      )
      write.csv(df, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui = ui, server = server)