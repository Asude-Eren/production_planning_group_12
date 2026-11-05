library(shiny)
library(readxl)

weighted_moving_average_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ---- Styling (CSS) ----
    tags$head(tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap');

      .wma-module {
        font-family: 'Poppins', 'Segoe UI', sans-serif; color: #26324A;
        font-size: 13px; padding-top: 28px; padding-bottom: 80px;
      }

      .wma-module .well {
        background: #F1F4F9; border: 1px solid #CBD5E3; border-top: 4px solid #2C3E63;
        border-radius: 10px; padding: 14px; box-shadow: none;
      }
      .wma-module .control-label { font-weight: 600; color: #2C3E63; font-size: 13px; }
      .wma-module .form-group { margin-bottom: 12px; }
      .wma-module .form-control {
        height: 32px; padding: 4px 10px; font-size: 13px;
        border-radius: 6px; border: 1px solid #CBD5E3; box-shadow: none;
      }
      .wma-module .form-control:focus { border-color: #4A6FA5; }
      .wma-module .btn-default {
        background: #2C3E63; color: #fff; border: none;
        height: 32px; padding: 4px 12px; font-size: 13px; border-radius: 6px 0 0 6px;
      }
      .wma-module .progress { height: 18px; margin-bottom: 0; }
      .wma-module .progress-bar { background-color: #E76F51; font-size: 11px; line-height: 18px; }

      .wma-module h3 {
        font-family: 'Poppins', sans-serif; font-weight: 600; font-size: 18px;
        color: #2C3E63; background: #F1F4F9; border-left: 4px solid #E76F51;
        padding: 8px 14px; border-radius: 6px; margin: 0 0 10px 0;
      }
      .wma-module h4 {
        font-family: 'Poppins', sans-serif; font-weight: 600; font-size: 15px;
        color: #2C3E63; margin: 18px 0 8px 0;
        display: table; border-bottom: 2px solid #E76F51; padding-bottom: 2px;
      }

      .wma-module table {
        width: auto; min-width: 360px; background: #fff; font-size: 12.5px;
        border-collapse: separate; border-spacing: 0;
      }
      .wma-module table th {
        background: #2C3E63; color: #fff; font-weight: 600; padding: 5px 12px;
        text-align: center; position: sticky; top: 0;
      }
      .wma-module table th:first-child { border-top-left-radius: 6px; }
      .wma-module table th:last-child  { border-top-right-radius: 6px; }
      .wma-module table td {
        padding: 4px 12px; text-align: center; border-top: 1px solid #E1E8F1;
      }
      .wma-module table tr:nth-child(even) td { background: #F6F8FB; }
      .wma-module table tr:hover td { background: #FCEDE8; }

      .wma-scroll {
        max-height: 280px; overflow-y: auto; display: block; width: fit-content;
        border: 1px solid #CBD5E3; border-radius: 8px;
      }

      .wma-legend { font-size: 12px; margin: 2px 0 4px 0; color: #26324A; }
      .wma-dot {
        display: inline-block; width: 10px; height: 10px; border-radius: 50%;
        margin: 0 5px 0 16px; vertical-align: middle;
      }
      .wma-legend .wma-dot:first-child { margin-left: 0; }
      .wma-dot-square  { border-radius: 2px; }
      .wma-dot-diamond { border-radius: 2px; transform: rotate(45deg); }

      .wma-note { font-size: 11.5px; color: #6B7A94; margin: 4px 0 8px 0; }
    "))),
    
    # ---- Page layout ----
    div(class = "wma-module",
        sidebarLayout(
          sidebarPanel(width = 3,
                       fileInput(ns("file"), "Upload an Excel or CSV file (.xlsx, .csv)",
                                 accept = c(".xlsx", ".csv")),
                       
                       selectInput(ns("column"), "Demand column", choices = NULL),
                       
                       radioButtons(
                         ns("n_mode"), "How should N be chosen?",
                         choices = c("I will choose N" = "manual",
                                     "Find the best N" = "auto")
                       ),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'manual'", ns("n_mode")),
                         numericInput(ns("N"), "N (number of past periods)",
                                      value = 3, min = 2, max = 20, step = 1)
                       ),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'auto'", ns("n_mode")),
                         numericInput(ns("maxN"), "Maximum N to try",
                                      value = 6, min = 3, max = 20, step = 1)
                       ),
                       
                       radioButtons(
                         ns("weight_mode"), "How should the weights be set?",
                         choices = c("Linearly decreasing (recent weighs most)" = "decreasing",
                                     "I will enter the weights" = "manual",
                                     "Find the best weights" = "auto")
                       ),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'manual'", ns("weight_mode")),
                         uiOutput(ns("weight_ui"))
                       ),
                       
                       # Criterion is needed whenever something is searched automatically
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'auto' || input['%s'] == 'auto'",
                                             ns("n_mode"), ns("weight_mode")),
                         radioButtons(ns("criterion"), "Best means lowest:",
                                      choices = c("MAD", "MSE", "MAPE"))
                       )
          ),
          
          mainPanel(width = 9,
                    h3(textOutput(ns("title"))),
                    
                    div(class = "wma-legend",
                        span(class = "wma-dot", style = "background:#2C3E63;"), "Actual demand",
                        span(class = "wma-dot wma-dot-square", style = "background:#E76F51;"), "Forecast",
                        span(class = "wma-dot wma-dot-diamond", style = "background:#E9B44C;"), "Next-period forecast"
                    ),
                    plotOutput(ns("forecast_plot"), height = "320px"),
                    
                    h4("Error measures"),
                    tableOutput(ns("error_table")),
                    div(class = "wma-note", textOutput(ns("search_note"))),
                    
                    # Only shown in "Find the best N" mode
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'auto'", ns("n_mode")),
                      h4("Comparison of all N values"),
                      tableOutput(ns("n_table")),
                      plotOutput(ns("bar_plot"), height = "260px")
                    ),
                    
                    h4("Weights used"),
                    tableOutput(ns("weight_table")),
                    div(class = "wma-note",
                        "t-1 is the most recent period. Weights are divided by their sum, so they do not need to add up to 1."),
                    
                    h4("Forecasts by period"),
                    div(class = "wma-scroll", tableOutput(ns("period_table")))
          )
        )
    )
  )
}

weighted_moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ---- Read the uploaded file (.xlsx or .csv) ----
    raw_data <- reactive({
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
    observeEvent(raw_data(), {
      numeric_cols <- names(raw_data())[sapply(raw_data(), is.numeric)]
      updateSelectInput(session, "column", choices = numeric_cols)
    })
    
    # ---- Demand vector ----
    demand <- reactive({
      req(input$column)
      d <- raw_data()[[input$column]]
      d[!is.na(d)]
    })
    
    # ---- WMA forecasts (last element = next-period forecast) ----
    # w[1] is the weight of the most recent period (t-1)
    compute_forecast <- function(d, w) {
      n <- length(d)
      N <- length(w)
      forecast <- rep(NA_real_, n + 1)
      for (t in (N + 1):(n + 1)) {
        forecast[t] <- sum(w * d[(t - 1):(t - N)]) / sum(w)
      }
      forecast
    }
    
    # ---- Error measures: MAD, MSE, MAPE (periods with zero demand are skipped in MAPE) ----
    # start = first period in which the error is evaluated
    # Error is defined as e = F - D
    error_measures <- function(d, forecast, start) {
      idx <- start:length(d)
      idx <- idx[!is.na(forecast[idx])]
      e   <- forecast[idx] - d[idx]
      nonzero <- d[idx] != 0
      c(MAD  = mean(abs(e)),
        MSE  = mean(e^2),
        MAPE = mean(abs(e[nonzero] / d[idx][nonzero])) * 100)
    }
    
    # ---- Search for the best weights for a given N ----
    # Weights are non-negative and normalized to sum to 1.
    # Several starting points are tried (equal, linear, geometric decay) and
    # each one is refined with Nelder-Mead; the best result is returned.
    optimize_weights <- function(d, N, start, crit) {
      objective <- function(par) {
        w <- abs(par)
        if (sum(w) == 0) return(1e12)
        v <- error_measures(d, compute_forecast(d, w), start)[[crit]]
        if (!is.finite(v)) 1e12 else v
      }
      starts <- list(rep(1, N), N:1, 0.5^(0:(N - 1)), 0.8^(0:(N - 1)))
      best_par <- starts[[1]]
      best_val <- objective(best_par)
      for (s in starts) {
        v0 <- objective(s)
        if (v0 < best_val) { best_val <- v0; best_par <- s }
        fit <- optim(s, objective, method = "Nelder-Mead",
                     control = list(maxit = 300))
        if (fit$value < best_val) { best_val <- fit$value; best_par <- fit$par }
      }
      w <- abs(best_par)
      w / sum(w)
    }
    
    # ---- Number of manual weight boxes ----
    # Fixed N: N boxes | Auto N: one box for every N up to "Maximum N to try"
    # (when N = k is tried, the first k weights w1..wk are used)
    n_boxes <- reactive({
      if (input$n_mode == "auto") {
        req(input$maxN)
        floor(min(input$maxN, 20))
      } else {
        req(input$N)
        input$N
      }
    })
    
    # ---- Manual weights: one input box per weight (w1 = most recent period) ----
    output$weight_ui <- renderUI({
      m <- n_boxes()
      req(m >= 2, m <= 20, m == round(m))
      note <- if (input$n_mode == "auto") {
        paste0("w1 = most recent period (t-1), w2 = t-2, ... When N = k is tried, ",
               "the first k weights (w1 ... wk) are used.")
      } else {
        "w1 = most recent period (t-1), w2 = t-2, ..."
      }
      tagList(
        div(class = "wma-note", note),
        lapply(1:m, function(i) {
          numericInput(session$ns(paste0("w", i)),
                       paste0("w", i, "  (t-", i, ")"),
                       value = m - i + 1, min = 0, step = 0.1)
        })
      )
    })
    
    # ---- Manual weights entered by the user (validated) ----
    manual_weights <- reactive({
      m <- n_boxes()
      w <- sapply(1:m, function(i) {
        v <- input[[paste0("w", i)]]
        if (is.null(v)) NA_real_ else v
      })
      validate(
        need(!any(is.na(w)), "Please enter all the weights."),
        need(all(w >= 0), "Weights cannot be negative."),
        need(sum(w[1:2]) > 0, "The sum of the first weights must be greater than zero.")
      )
      w
    })
    
    # ---- Auto-N mode: largest N to try ----
    max_n <- reactive({
      req(input$maxN)
      upper <- floor(min(input$maxN, 20, length(demand()) - 1))
      validate(need(upper >= 2, "Not enough data to try N values of at least 2."))
      upper
    })
    
    # ---- Auto-N mode: compare N = 2..upper on the SAME periods (upper+1 ... n) ----
    # Weights for each N: linearly decreasing, entered by the user, or optimized
    # (depending on the weight mode).
    search_result <- reactive({
      req(input$n_mode == "auto")
      d     <- demand()
      upper <- max_n()
      crit  <- input$criterion
      ks    <- 2:upper
      weight_list <- vector("list", length(ks))
      
      manual_w <- NULL
      if (input$weight_mode == "manual") {
        manual_w <- manual_weights()
        validate(need(length(manual_w) >= upper,
                      "Please enter a weight for every N up to the maximum N."))
      }
      
      withProgress(message = "Searching for the best N...", value = 0, {
        for (i in seq_along(ks)) {
          k <- ks[i]
          weight_list[[i]] <- switch(input$weight_mode,
                                     "auto"       = optimize_weights(d, k, upper + 1, crit),
                                     "manual"     = manual_w[1:k],
                                     "decreasing" = k:1)
          incProgress(1 / length(ks))
        }
      })
      
      tbl <- do.call(rbind, lapply(seq_along(ks), function(i) {
        h <- error_measures(d, compute_forecast(d, weight_list[[i]]), upper + 1)
        data.frame(N = ks[i],
                   MAD = unname(h["MAD"]),
                   MSE = unname(h["MSE"]),
                   MAPE = unname(h["MAPE"]))
      }))
      list(table = tbl, weights = setNames(weight_list, ks))
    })
    
    # ---- N to use (manual or best) ----
    chosen_n <- reactive({
      if (input$n_mode == "manual") {
        req(input$N)
        validate(
          need(input$N >= 2 && input$N == round(input$N),
               "N must be an integer of at least 2."),
          need(input$N <= 20, "N can be at most 20."),
          need(input$N < length(demand()),
               "N must be smaller than the number of periods.")
        )
        input$N
      } else {
        tbl <- search_result()$table
        tbl$N[which.min(tbl[[input$criterion]])]
      }
    })
    
    # ---- First period in which the error is evaluated ----
    # Manual N: N+1 | Auto N: maxN+1 (all N values use the same periods)
    start_period <- reactive({
      if (input$n_mode == "manual") chosen_n() + 1 else max_n() + 1
    })
    
    # ---- Weights to use ----
    wma_weights <- reactive({
      N <- chosen_n()
      if (input$n_mode == "auto") {
        # Weights come from the N search (linear, manual or optimized)
        search_result()$weights[[as.character(N)]]
      } else if (input$weight_mode == "decreasing") {
        N:1
      } else if (input$weight_mode == "manual") {
        manual_weights()
      } else {
        # Find the best weights for the fixed N
        withProgress(message = "Searching for the best weights...", value = 0.5, {
          optimize_weights(demand(), N, N + 1, input$criterion)
        })
      }
    })
    
    # ---- Title at the top ----
    output$title <- renderText({
      d <- demand()
      w <- wma_weights()
      f <- compute_forecast(d, w)
      paste0("WMA(", length(w), ") — next-period forecast: ",
             sprintf("%.2f", f[length(d) + 1]))
    })
    
    # ---- Plot of actual demand and forecast ----
    output$forecast_plot <- renderPlot({
      d <- demand()
      w <- wma_weights()
      n <- length(d)
      f <- compute_forecast(d, w)
      
      par(bg = "#FFFFFF", mar = c(4, 4, 2.5, 1), cex.main = 1, cex.lab = 0.9,
          cex.axis = 0.85)
      plot(1:n, d, type = "o", pch = 16, cex = 0.7, col = "#2C3E63", lwd = 1.5,
           xlim = c(1, n + 1), ylim = range(c(d, f), na.rm = TRUE),
           xlab = "Period", ylab = "Demand",
           main = paste0("Actual demand and WMA(", length(w), ") forecast"))
      grid(col = "#E1E8F1")
      lines(1:(n + 1), f, type = "o", pch = 15, cex = 0.6, lty = 1, lwd = 1.5,
            col = "#E76F51")
      points(n + 1, f[n + 1], pch = 18, cex = 2.2, col = "#E9B44C")
    })
    
    # ---- Error measures table ----
    output$error_table <- renderTable({
      d     <- demand()
      w     <- wma_weights()
      n     <- length(d)
      start <- start_period()
      h     <- error_measures(d, compute_forecast(d, w), start = start)
      data.frame(
        "Measure" = c("MAD", "MSE", "MAPE (%)"),
        "Value"   = round(unname(h), 2),
        "Periods evaluated" = paste0(start, "–", n),
        check.names = FALSE
      )
    }, digits = 2)
    
    # ---- Note describing what was searched automatically ----
    output$search_note <- renderText({
      n     <- length(demand())
      start <- start_period()
      parts <- character(0)
      if (input$n_mode == "auto") {
        parts <- c(parts, paste0("All N values from 2 to ", max_n(),
                                 " are compared on the same periods (", start, "–", n, ")."))
        if (input$weight_mode == "manual") {
          parts <- c(parts, "For each N, the first N of your weights (w1 ... wN) were used.")
        }
      }
      if (input$weight_mode == "auto") {
        parts <- c(parts, paste0("Weights were optimized to minimize ", input$criterion,
                                 " on periods ", start, "–", n,
                                 " (in-sample), so the error shown may be optimistic for future forecasts."))
      }
      paste(parts, collapse = " ")
    })
    
    # ---- Auto-N mode: comparison table of all N values ----
    output$n_table <- renderTable({
      req(input$n_mode == "auto")
      tbl <- search_result()$table
      tbl$MAD  <- round(tbl$MAD, 3)
      tbl$MSE  <- round(tbl$MSE, 3)
      tbl$MAPE <- round(tbl$MAPE, 3)
      tbl$N    <- as.integer(tbl$N)
      tbl
    })
    
    # ---- Auto-N mode: bar plot (best N in coral) ----
    output$bar_plot <- renderPlot({
      req(input$n_mode == "auto")
      tbl  <- search_result()$table
      crit <- input$criterion
      colors <- ifelse(tbl$N == chosen_n(), "#E76F51", "#B7C4D9")
      barplot(tbl[[crit]], names.arg = tbl$N, col = colors, border = NA,
              xlab = "N", ylab = crit,
              main = paste0("Best N by ", crit, " = ", chosen_n()))
    })
    
    # ---- Weights used ----
    output$weight_table <- renderTable({
      w <- wma_weights()
      data.frame(
        "Period"              = paste0("t-", seq_along(w)),
        "Weight (w)"          = w,
        "Share of total (%)"  = w / sum(w) * 100,
        check.names = FALSE
      )
    }, digits = 2)
    
    # ---- Period-by-period forecast table (Error = F - D) ----
    output$period_table <- renderTable({
      d <- demand()
      n <- length(d)
      f <- compute_forecast(d, wma_weights())
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
