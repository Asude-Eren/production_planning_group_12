library(shiny)
library(readxl)
library(ggplot2)

# ==========================================
# 1. CORE CALCULATION FUNCTION (Nahmias Method)
# ==========================================
calculate_holts_module <- function(D, alpha, beta, S0, G0, horizon = 1, eval_start = 1) {
  n <- length(D)
  S <- numeric(n)
  G <- numeric(n)
  F_one_step <- numeric(n)
  
  # Step 1 (t = 1) using S0 and G0
  F_one_step[1] <- S0 + G0
  S[1] <- alpha * D[1] + (1 - alpha) * (S0 + G0)
  G[1] <- beta * (S[1] - S0) + (1 - beta) * G0
  
  # Steps t = 2 to n
  if (n > 1) {
    for (t in 2:n) {
      F_one_step[t] <- S[t - 1] + G[t - 1]
      S[t] <- alpha * D[t] + (1 - alpha) * (S[t - 1] + G[t - 1])
      G[t] <- beta * (S[t] - S[t - 1]) + (1 - beta) * G[t - 1]
    }
  }
  
  # Errors (e_t = D_t - F_t)
  errors <- D - F_one_step
  abs_errors <- abs(errors)
  sq_errors <- errors^2
  pct_errors <- ifelse(D == 0, NA, abs(errors / D) * 100)
  
  # Evaluation subset (e.g., Periods 4 to 8 in Example 2.5)
  eval_idx <- max(1, min(eval_start, n)):n
  MAD  <- mean(abs_errors[eval_idx], na.rm = TRUE)
  MSE  <- mean(sq_errors[eval_idx], na.rm = TRUE)
  MAPE <- mean(pct_errors[eval_idx], na.rm = TRUE)
  
  # Historical Data Frame
  hist_df <- data.frame(
    Period = 1:n,
    Actual = D,
    Intercept_S = round(S, 4),
    Slope_G = round(G, 4),
    Forecast = round(F_one_step, 4),
    Error = round(errors, 4),
    Abs_Error = round(abs_errors, 4),
    Sq_Error = round(sq_errors, 4),
    APE_Pct = round(pct_errors, 4)
  )
  
  # Future Forecasts: F_{n, n + tau} = S_n + tau * G_n
  future_periods <- (n + 1):(n + horizon)
  tau <- 1:horizon
  future_forecasts <- S[n] + tau * G[n]
  
  future_df <- data.frame(
    Period = future_periods,
    Actual = NA,
    Intercept_S = NA,
    Slope_G = NA,
    Forecast = round(future_forecasts, 4),
    Error = NA,
    Abs_Error = NA,
    Sq_Error = NA,
    APE_Pct = NA
  )
  
  full_df <- rbind(hist_df, future_df)
  
  list(
    data = full_df,
    metrics = data.frame(
      Metric = c("MAD", "MSE", "MAPE (%)"),
      Value = round(c(MAD, MSE, MAPE), 4)
    ),
    params = c(alpha = alpha, beta = beta, S0 = S0, G0 = G0)
  )
}

# ==========================================
# 2. SHINY MODULE UI
# ==========================================
holts_method_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Data Input"),
      fileInput(ns("file"), "Upload Excel or CSV File", accept = c(".csv", ".xlsx", ".xls")),
      uiOutput(ns("col_select_ui")),
      actionButton(ns("load_ex"), "Load Textbook Example 2.5", class = "btn-info", style = "width: 100%; margin-bottom: 15px;"),
      hr(),
      
      h4("Initial Values (S0 & G0)"),
      radioButtons(ns("init_mode"), "Initialization Method:",
                   choices = c("Linear Regression" = "reg",
                               "Manual Entry" = "manual"),
                   selected = "manual"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'reg'", ns("init_mode")),
        numericInput(ns("reg_n"), "Number of Baseline Periods for Regression:", value = 6, min = 2, step = 1)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("init_mode")),
        numericInput(ns("S0_val"), "Initial Intercept (S0):", value = 200, step = 1),
        numericInput(ns("G0_val"), "Initial Slope (G0):", value = 10, step = 0.5)
      ),
      hr(),
      
      h4("Smoothing Parameters (Alpha & Beta)"),
      radioButtons(ns("param_mode"), "Parameter Selection:",
                   choices = c("Manual Entry" = "manual",
                               "Auto-Search Best (Optimize)" = "opt"),
                   selected = "manual"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("param_mode")),
        sliderInput(ns("alpha"), "Alpha (Level Weight):", min = 0.01, max = 0.99, value = 0.10, step = 0.01),
        sliderInput(ns("beta"), "Beta (Trend Weight):", min = 0.01, max = 0.99, value = 0.10, step = 0.01)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'opt'", ns("param_mode")),
        selectInput(ns("opt_target"), "Optimization Criterion:", choices = c("MSE", "MAD", "MAPE"))
      ),
      hr(),
      
      h4("Forecast & Evaluation Settings"),
      numericInput(ns("horizon"), "Forecast Horizon (tau steps ahead):", value = 4, min = 1, max = 50, step = 1),
      numericInput(ns("eval_start"), "Start Error Evaluation from Period:", value = 4, min = 1, step = 1),
      helpText("Note: In Example 2.5, Nahmias evaluates errors from Period 4 to 8 (set to 1 for all periods).")
    ),
    
    mainPanel(
      h3("Holt's Trend-Adjusted Exponential Smoothing"),
      fluidRow(
        column(6, h4("Performance Metrics"), tableOutput(ns("metrics_tbl"))),
        column(6, h4("Model Parameters"), verbatimTextOutput(ns("param_summary")))
      ),
      hr(),
      h4("Actual vs. Forecast Plot"),
      plotOutput(ns("forecast_plot"), height = "380px"),
      hr(),
      h4("Detailed Forecast & Smoothing Table"),
      tableOutput(ns("results_tbl"))
    )
  )
}

# ==========================================
# 3. SHINY MODULE SERVER
# ==========================================
holts_method_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive storage for dataset (Pre-loaded with Example 2.5 by default)
    raw_data <- reactiveVal(data.frame(
      Period = 1:8,
      Engine_Failures = c(200, 250, 175, 186, 225, 285, 305, 190)
    ))
    
    # Reload Example 2.5 Data when button is clicked
    observeEvent(input$load_ex, {
      ex_df <- data.frame(
        Period = 1:8,
        Engine_Failures = c(200, 250, 175, 186, 225, 285, 305, 190)
      )
      raw_data(ex_df)
      updateRadioButtons(session, "init_mode", selected = "manual")
      updateNumericInput(session, "S0_val", value = 200)
      updateNumericInput(session, "G0_val", value = 10)
      updateRadioButtons(session, "param_mode", selected = "manual")
      updateSliderInput(session, "alpha", value = 0.10)
      updateSliderInput(session, "beta", value = 0.10)
      updateNumericInput(session, "eval_start", value = 4)
    })
    
    # Load Uploaded File (CSV or Excel)
    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      df <- if (tolower(ext) == "csv") {
        read.csv(input$file$datapath, stringsAsFactors = FALSE)
      } else if (tolower(ext) %in% c("xls", "xlsx")) {
        as.data.frame(read_excel(input$file$datapath))
      } else {
        validate("Invalid file format. Please upload a .csv, .xls, or .xlsx file.")
      }
      raw_data(df)
      updateNumericInput(session, "eval_start", value = 1)
    })
    
    # Dynamic Series Selector
    output$col_select_ui <- renderUI({
      df <- raw_data()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      selectInput(ns("series_col"), "Select Target Series:", 
                  choices = num_cols, 
                  selected = tail(num_cols, 1))
    })
    
    # Extract Clean Numeric Vector
    selected_series <- reactive({
      df <- raw_data()
      req(df, input$series_col)
      vals <- as.numeric(na.omit(df[[input$series_col]]))
      req(length(vals) >= 2)
      vals
    })
    
    # Main Model Computation
    model_results <- reactive({
      D <- selected_series()
      n <- length(D)
      
      # 1. Determine Initial Values (S0 and G0)
      if (input$init_mode == "reg") {
        k <- min(max(2, as.numeric(input$reg_n)), n)
        t_sub <- 1:k
        D_sub <- D[1:k]
        fit <- lm(D_sub ~ t_sub)
        S0 <- as.numeric(coef(fit)[1]) # Intercept at t = 0
        G0 <- as.numeric(coef(fit)[2]) # Slope
      } else {
        req(!is.na(input$S0_val), !is.na(input$G0_val))
        S0 <- as.numeric(input$S0_val)
        G0 <- as.numeric(input$G0_val)
      }
      
      eval_start <- ifelse(is.na(input$eval_start), 1, max(1, min(as.numeric(input$eval_start), n)))
      horizon <- ifelse(is.na(input$horizon), 1, max(1, as.numeric(input$horizon)))
      
      # 2. Determine Smoothing Constants (Alpha and Beta)
      if (input$param_mode == "manual") {
        alpha <- as.numeric(input$alpha)
        beta <- as.numeric(input$beta)
      } else {
        obj_fn <- function(par) {
          a <- par[1]
          b <- par[2]
          if (a <= 0 || a >= 1 || b <= 0 || b >= 1) return(Inf)
          res <- calculate_holts_module(D, a, b, S0, G0, horizon = 1, eval_start = eval_start)
          m_df <- res$metrics
          if (input$opt_target == "MSE") return(m_df$Value[m_df$Metric == "MSE"])
          if (input$opt_target == "MAD") return(m_df$Value[m_df$Metric == "MAD"])
          return(m_df$Value[m_df$Metric == "MAPE (%)"])
        }
        opt <- optim(c(0.2, 0.1), obj_fn, method = "L-BFGS-B", lower = c(0.001, 0.001), upper = c(0.999, 0.999))
        alpha <- round(opt$par[1], 4)
        beta <- round(opt$par[2], 4)
      }
      
      calculate_holts_module(D, alpha, beta, S0, G0, horizon = horizon, eval_start = eval_start)
    })
    
    # Outputs
    output$metrics_tbl <- renderTable({
      req(model_results())
      model_results()$metrics
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$param_summary <- renderPrint({
      req(model_results())
      p <- model_results()$params
      cat(sprintf("Alpha (Level)   : %.4f\n", p["alpha"]))
      cat(sprintf("Beta  (Trend)   : %.4f\n", p["beta"]))
      cat(sprintf("Initial S0      : %.4f\n", p["S0"]))
      cat(sprintf("Initial G0      : %.4f\n", p["G0"]))
    })
    
    output$forecast_plot <- renderPlot({
      req(model_results())
      df <- model_results()$data
      
      ggplot(df, aes(x = Period)) +
        geom_line(aes(y = Actual, color = "Actual"), linewidth = 1, na.rm = TRUE) +
        geom_point(aes(y = Actual, color = "Actual"), size = 2.5, na.rm = TRUE) +
        geom_line(aes(y = Forecast, color = "Forecast"), linetype = "dashed", linewidth = 1) +
        geom_point(aes(y = Forecast, color = "Forecast"), size = 2.5) +
        scale_color_manual(values = c("Actual" = "#2c3e50", "Forecast" = "#e74c3c")) +
        labs(title = "Holt's Method: Actual vs. Forecast",
             x = "Period (t)", y = "Value", color = "Series") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom")
    })
    
    output$results_tbl <- renderTable({
      req(model_results())
      model_results()$data
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
