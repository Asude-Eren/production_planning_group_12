library(shiny)
library(readxl)

moving_average_new_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ---- Styling (CSS) ----
    tags$head(tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap');

      .mav-module {
        font-family: 'Poppins', 'Segoe UI', sans-serif; color: #26324A;
        font-size: 13px; padding-top: 28px; padding-bottom: 80px;
      }

      /* Left settings box */
      .mav-module .well {
        background: #F1F4F9; border: 1px solid #CBD5E3; border-top: 4px solid #2C3E63;
        border-radius: 10px; padding: 14px; box-shadow: none;
      }
      .mav-module .control-label { font-weight: 600; color: #2C3E63; font-size: 13px; }
      .mav-module .form-group { margin-bottom: 12px; }
      .mav-module .form-control {
        height: 32px; padding: 4px 10px; font-size: 13px;
        border-radius: 6px; border: 1px solid #CBD5E3; box-shadow: none;
      }
      .mav-module .form-control:focus { border-color: #4A6FA5; }
      .mav-module .btn-default {
        background: #2C3E63; color: #fff; border: none;
        height: 32px; padding: 4px 12px; font-size: 13px; border-radius: 6px 0 0 6px;
      }
      .mav-module .progress { height: 18px; margin-bottom: 0; }
      .mav-module .progress-bar { background-color: #E76F51; font-size: 11px; line-height: 18px; }

      /* Headings */
      .mav-module h3 {
        font-family: 'Poppins', sans-serif; font-weight: 600; font-size: 18px;
        color: #2C3E63; background: #F1F4F9; border-left: 4px solid #E76F51;
        padding: 8px 14px; border-radius: 6px; margin: 0 0 10px 0;
      }
      .mav-module h4 {
        font-family: 'Poppins', sans-serif; font-weight: 600; font-size: 15px;
        color: #2C3E63; margin: 18px 0 8px 0;
        display: table; border-bottom: 2px solid #E76F51; padding-bottom: 2px;
      }

      /* Tables */
      .mav-module table {
        width: auto; min-width: 360px; background: #fff; font-size: 12.5px;
        border-collapse: separate; border-spacing: 0;
      }
      .mav-module table th {
        background: #2C3E63; color: #fff; font-weight: 600; padding: 5px 12px;
        text-align: center; position: sticky; top: 0;
      }
      .mav-module table th:first-child { border-top-left-radius: 6px; }
      .mav-module table th:last-child  { border-top-right-radius: 6px; }
      .mav-module table td {
        padding: 4px 12px; text-align: center; border-top: 1px solid #E1E8F1;
      }
      .mav-module table tr:nth-child(even) td { background: #F6F8FB; }
      .mav-module table tr:hover td { background: #FCEDE8; }

      /* Scrollable table box */
      .mav-scroll {
        max-height: 280px; overflow-y: auto; display: block; width: fit-content;
        border: 1px solid #CBD5E3; border-radius: 8px;
      }

      /* Plot legend */
      .mav-legend { font-size: 12px; margin: 2px 0 4px 0; color: #26324A; }
      .mav-dot {
        display: inline-block; width: 10px; height: 10px; border-radius: 50%;
        margin: 0 5px 0 16px; vertical-align: middle;
      }
      .mav-legend .mav-dot:first-child { margin-left: 0; }
      .mav-dot-square  { border-radius: 2px; }
      .mav-dot-diamond { border-radius: 2px; transform: rotate(45deg); }

      /* Small note */
      .mav-note { font-size: 11.5px; color: #6B7A94; margin-top: 4px; }
    "))),
    
    # ---- Page layout ----
    div(class = "mav-module",
        sidebarLayout(
          sidebarPanel(width = 3,
                       fileInput(ns("file"), "Upload an Excel or CSV file (.xlsx, .csv)",
                                 accept = c(".xlsx", ".csv")),
                       
                       selectInput(ns("column"), "Demand column", choices = NULL),
                       
                       radioButtons(
                         ns("mode"), "How should N be chosen?",
                         choices = c("I will choose N" = "manual",
                                     "Find the best N" = "auto")
                       ),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'manual'", ns("mode")),
                         numericInput(ns("N"), "N (number of past periods)",
                                      value = 3, min = 1, step = 1)
                       ),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'auto'", ns("mode")),
                         numericInput(ns("maxN"), "Maximum N to try",
                                      value = 6, min = 2, step = 1),
                         radioButtons(ns("criterion"), "Best N means lowest:",
                                      choices = c("MAD", "MSE", "MAPE"))
                       )
          ),
          
          mainPanel(width = 9,
                    h3(textOutput(ns("title"))),
                    
                    # Plot legend sits ABOVE the plot
                    div(class = "mav-legend",
                        span(class = "mav-dot", style = "background:#2C3E63;"), "Actual demand",
                        span(class = "mav-dot mav-dot-square", style = "background:#E76F51;"), "Forecast",
                        span(class = "mav-dot mav-dot-diamond", style = "background:#E9B44C;"), "Next-period forecast"
                    ),
                    plotOutput(ns("forecast_plot"), height = "320px"),
                    
                    h4("Error measures"),
                    tableOutput(ns("error_table")),
                    
                    # Only shown in "Find the best N" mode
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'auto'", ns("mode")),
                      div(style = "margin-top: 12px;", tableOutput(ns("best_table"))),
                      div(class = "mav-note", textOutput(ns("best_note"))),
                      h4("Comparison of all N values"),
                      tableOutput(ns("n_table")),
                      plotOutput(ns("bar_plot"), height = "260px")
                    ),
                    
                    h4("Forecasts by period"),
                    div(class = "mav-scroll", tableOutput(ns("period_table")))
          )
        )
    )
  )
}

moving_average_new_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ---- Read the uploaded file (.xlsx or .csv) ----
    data <- reactive({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      validate(need(ext %in% c("xlsx", "csv"),
                    "Please upload an .xlsx or .csv file."))
      if (ext == "csv") {
        df <- read.csv(input$file$datapath, check.names = FALSE)
        # If only one column was found, the file probably uses ";" as separator
        if (ncol(df) == 1) {
          df <- read.csv2(input$file$datapath, check.names = FALSE)
        }
        df
      } else {
        read_excel(input$file$datapath)
      }
    })
    
    # ---- Fill the demand column list ----
    observeEvent(data(), {
      numeric_cols <- names(data())[sapply(data(), is.numeric)]
      updateSelectInput(session, "column", choices = numeric_cols)
    })
    
    # ---- Demand vector ----
    demand <- reactive({
      req(input$column)
      d <- data()[[input$column]]
      d[!is.na(d)]
    })
    
    # ---- MA(N) forecasts (last element = next-period forecast) ----
    compute_forecast <- function(d, N) {
      n <- length(d)
      forecast <- rep(NA_real_, n + 1)
      for (t in (N + 1):(n + 1)) {
        forecast[t] <- mean(d[(t - N):(t - 1)])
      }
      forecast
    }
    
    # ---- Error measures: MAD, MSE, MAPE ----
    # start = first period in which the error is evaluated
    # MAPE ignores periods where demand is 0
    # Error is defined as e = F - D
    error_measures <- function(d, forecast, start = 1) {
      idx <- start:length(d)
      idx <- idx[!is.na(forecast[idx])]
      e   <- forecast[idx] - d[idx]
      nonzero <- d[idx] != 0
      c(MAD  = mean(abs(e)),
        MSE  = mean(e^2),
        MAPE = mean(abs(e[nonzero] / d[idx][nonzero])) * 100)
    }
    
    # ---- Compare N = 1..upper on the SAME periods (upper+1 ... n) ----
    scan_n <- function(d, upper) {
      result_list <- lapply(1:upper, function(k) {
        h <- error_measures(d, compute_forecast(d, k), start = upper + 1)
        data.frame(N = k,
                   MAD = unname(h["MAD"]),
                   MSE = unname(h["MSE"]),
                   MAPE = unname(h["MAPE"]))
      })
      do.call(rbind, result_list)
    }
    
    # ---- Auto mode: largest N to try ----
    max_n <- reactive({
      req(input$maxN)
      upper <- floor(min(input$maxN, length(demand()) - 1))
      validate(need(upper >= 1, "Not enough data."))
      upper
    })
    
    # ---- Auto mode: comparison table ----
    search_table <- reactive({
      scan_n(demand(), max_n())
    })
    
    # ---- N to use (manual or best) ----
    chosen_n <- reactive({
      if (input$mode == "manual") {
        req(input$N)
        validate(need(input$N >= 1 && input$N == round(input$N),
                      "N must be a positive integer."),
                 need(input$N < length(demand()),
                      "N must be smaller than the number of periods."))
        input$N
      } else {
        tbl <- search_table()
        tbl$N[which.min(tbl[[input$criterion]])]
      }
    })
    
    # ---- First period in which the error is evaluated ----
    # Manual: N+1 | Auto: maxN+1 (all N values use the same periods)
    start_period <- reactive({
      if (input$mode == "manual") {
        chosen_n() + 1
      } else {
        max_n() + 1
      }
    })
    
    # ---- Title at the top ----
    output$title <- renderText({
      d <- demand()
      N <- chosen_n()
      f <- compute_forecast(d, N)
      paste0("MA(", N, ") — next-period forecast: ",
             sprintf("%.2f", f[length(d) + 1]))
    })
    
    # ---- Line plot of actual demand and forecast (legend is above the plot) ----
    output$forecast_plot <- renderPlot({
      d <- demand()
      N <- chosen_n()
      n <- length(d)
      f <- compute_forecast(d, N)
      
      par(bg = "#FFFFFF", mar = c(4, 4, 2.5, 1), cex.main = 1, cex.lab = 0.9,
          cex.axis = 0.85)
      plot(1:n, d, type = "o", pch = 16, cex = 0.7, col = "#2C3E63", lwd = 1.5,
           xlim = c(1, n + 1), ylim = range(c(d, f), na.rm = TRUE),
           xlab = "Period", ylab = "Demand",
           main = paste0("Actual demand and MA(", N, ") forecast"))
      grid(col = "#E1E8F1")
      lines(1:(n + 1), f, type = "o", pch = 15, cex = 0.6, lty = 1, lwd = 1.5,
            col = "#E76F51")
      points(n + 1, f[n + 1], pch = 18, cex = 2.2, col = "#E9B44C")
    })
    
    # ---- Error measures table ----
    output$error_table <- renderTable({
      d     <- demand()
      N     <- chosen_n()
      n     <- length(d)
      start <- start_period()
      h     <- error_measures(d, compute_forecast(d, N), start = start)
      data.frame(
        "Measure" = c("MAD", "MSE", "MAPE (%)"),
        "Value"   = round(unname(h), 2),
        "Periods evaluated" = paste0(start, "–", n),
        check.names = FALSE
      )
    }, digits = 2)
    
    # ---- Auto mode: best N for each measure ----
    output$best_table <- renderTable({
      req(input$mode == "auto")
      tbl <- search_table()
      data.frame(
        "Best by"     = c("MAD", "MSE", "MAPE (%)"),
        "Best N"      = as.integer(c(tbl$N[which.min(tbl$MAD)],
                                     tbl$N[which.min(tbl$MSE)],
                                     tbl$N[which.min(tbl$MAPE)])),
        "Error value" = round(c(min(tbl$MAD, na.rm = TRUE),
                                min(tbl$MSE, na.rm = TRUE),
                                min(tbl$MAPE, na.rm = TRUE)), 2),
        check.names = FALSE
      )
    }, digits = 2)
    
    output$best_note <- renderText({
      req(input$mode == "auto")
      upper <- max_n()
      paste0("All N values from 1 to ", upper, " are compared on the same periods (",
             upper + 1, "–", length(demand()), "), so the comparison is fair.")
    })
    
    # ---- Auto mode: comparison table of all N values ----
    output$n_table <- renderTable({
      req(input$mode == "auto")
      tbl <- search_table()
      tbl$MAD  <- round(tbl$MAD, 3)
      tbl$MSE  <- round(tbl$MSE, 3)
      tbl$MAPE <- round(tbl$MAPE, 3)
      tbl$N    <- as.integer(tbl$N)
      tbl
    })
    
    # ---- Auto mode: bar plot (best N in coral) ----
    output$bar_plot <- renderPlot({
      req(input$mode == "auto")
      tbl <- search_table()
      crit <- input$criterion
      colors <- ifelse(tbl$N == chosen_n(), "#E76F51", "#B7C4D9")
      barplot(tbl[[crit]], names.arg = tbl$N, col = colors, border = NA,
              xlab = "N", ylab = crit,
              main = paste0("Best N by ", crit, " = ", chosen_n()))
    })
    
    # ---- Period-by-period forecast table (Error = F - D) ----
    output$period_table <- renderTable({
      d <- demand()
      n <- length(d)
      f <- compute_forecast(d, chosen_n())
      data.frame(
        "Period"            = 1:(n + 1),
        "Demand (D)"        = c(d, NA),
        "Forecast (F)"      = f,
        "Error (e = F - D)" = f - c(d, NA),
        check.names = FALSE
      )
    }, na = "", digits = 2)
    
  })
}
