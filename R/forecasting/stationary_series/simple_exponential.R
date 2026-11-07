# R/simple_exponential.R
# Simple Exponential Smoothing (SES) Shiny Module
# Based on Nahmias Production and Operations Analysis & Assignment 2 Requirements

library(shiny)
library(readxl)
library(ggplot2)
library(DT)

# --- CORE CALCULATION FUNCTION ---
calculate_ses_module <- function(demand, alpha, init_method = "regression", manual_init = NULL, horizon = 1) {
  n <- length(demand)
  forecasts <- numeric(n)
  errors <- numeric(n)
  
  # 1. Determine Initial Forecast (F_1)
  if (init_method == "regression") {
    t_idx <- 1:n
    reg_model <- lm(demand ~ t_idx)
    init_val <- as.numeric(predict(reg_model, newdata = data.frame(t_idx = 1)))
  } else if (init_method == "first_obs") {
    init_val <- demand[1]
  } else if (init_method == "average") {
    k <- min(n, 10)
    init_val <- mean(demand[1:k], na.rm = TRUE)
  } else {
    init_val <- ifelse(is.null(manual_init) || is.na(manual_init), demand[1], manual_init)
  }
  
  forecasts[1] <- init_val
  errors[1] <- demand[1] - forecasts[1]
  
  # 2. One-step-ahead forecasts
  if (n >= 2) {
    for (t in 2:n) {
      forecasts[t] <- alpha * demand[t - 1] + (1 - alpha) * forecasts[t - 1]
      errors[t] <- demand[t] - forecasts[t]
    }
  }
  
  # 3. Next period forecast
  next_forecast <- alpha * demand[n] + (1 - alpha) * forecasts[n]
  future_forecasts <- rep(next_forecast, horizon)
  
  # 4. Accuracy Metrics (MAD, MSE, MAPE)
  mad_val <- mean(abs(errors), na.rm = TRUE)
  mse_val <- mean(errors^2, na.rm = TRUE)
  
  non_zero <- demand != 0
  if (any(non_zero)) {
    mape_val <- mean(abs(errors[non_zero] / demand[non_zero]), na.rm = TRUE) * 100
  } else {
    mape_val <- NA
  }
  
  equiv_N <- (2 - alpha) / alpha
  
  list(
    alpha = alpha,
    initial_forecast = init_val,
    fitted = forecasts,
    errors = errors,
    abs_errors = abs(errors),
    sq_errors = errors^2,
    ape = ifelse(demand != 0, abs(errors / demand) * 100, NA),
    future = future_forecasts,
    MAD = mad_val,
    MSE = mse_val,
    MAPE = mape_val,
    equivalent_MA_N = equiv_N
  )
}

# --- MODULE UI ---
simple_exponential_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Simple Exponential Smoothing (Stationary Series)"),
    sidebarLayout(
      sidebarPanel(
        h4("1. Data Upload & Selection"),
        fileInput(ns("file"), "Upload Excel (.xlsx, .xls) or CSV (.csv) File:",
                  accept = c(".csv", ".xlsx", ".xls")),
        uiOutput(ns("col_selector")),
        helpText("Note: If no file is uploaded, sample data from Nahmias Example 2.3 (Aircraft Engine Failures) is used."),
        hr(),
        
        h4("2. Initial Forecast Value (F1)"),
        radioButtons(ns("init_type"), "Select Initialization Method:",
                     choices = c("Fit Regression (Intercept/Level)" = "regression",
                                 "Enter Initial Level Manually" = "manual",
                                 "Use First Observation (F1 = D1)" = "first_obs",
                                 "Arithmetic Average of Data" = "average"),
                     selected = "regression"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("init_type")),
          numericInput(ns("manual_init"), "Enter Initial Forecast (F1):", value = 200, step = 1)
        ),
        hr(),
        
        h4("3. Smoothing Constant (Alpha)"),
        radioButtons(ns("alpha_mode"), "Parameter Selection Mode:",
                     choices = c("Enter Alpha Manually" = "manual",
                                 "Search for Best Alpha (Auto-Optimize)" = "auto"),
                     selected = "manual"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("alpha_mode")),
          sliderInput(ns("alpha_val"), "Smoothing Constant (Alpha):",
                      min = 0.01, max = 0.99, value = 0.10, step = 0.01)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("alpha_mode")),
          selectInput(ns("opt_metric"), "Optimization Target Metric:",
                      choices = c("Minimize MSE" = "MSE",
                                  "Minimize MAD" = "MAD",
                                  "Minimize MAPE" = "MAPE"),
                      selected = "MSE")
        ),
        hr(),
        
        h4("4. Forecast Horizon"),
        numericInput(ns("horizon"), "Number of Future Periods to Forecast:",
                     value = 4, min = 1, max = 50, step = 1)
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Forecast Plot & Accuracy",
                   br(),
                   h4("Performance & Accuracy Summary"),
                   tableOutput(ns("metrics_table")),
                   hr(),
                   h4("Actual Demand vs. Exponential Smoothing Forecast"),
                   plotOutput(ns("forecast_plot"), height = "420px"),
                   hr(),
                   h4("Future Period Forecasts (Multiple-Step-Ahead)"),
                   tableOutput(ns("future_table"))
          ),
          tabPanel("Detailed Forecast Table",
                   br(),
                   h4("Period-by-Period Calculations and Error Analysis"),
                   DTOutput(ns("detailed_table"))
          ),
          tabPanel("Method & Formula Guide",
                   br(),
                   h4("Simple Exponential Smoothing Overview"),
                   p("Simple Exponential Smoothing is used for forecasting stationary time series."),
                   p(strong("Forecast Equation: "), "F(t) = alpha * D(t-1) + (1 - alpha) * F(t-1)"),
                   p(strong("Multiple-Step-Ahead Forecast: "), "All future forecasts beyond period n equal F(n+1)."),
                   p(strong("Equivalent Moving Average Period (N): "), "alpha = 2 / (N + 1) => N = (2 - alpha) / alpha.")
          )
        )
      )
    )
  )
}

# --- MODULE SERVER ---
simple_exponential_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    raw_data <- reactive({
      if (is.null(input$file)) {
        return(data.frame(
          Quarter = 1:8,
          Failures = c(200, 250, 175, 186, 225, 285, 305, 190)
        ))
      }
      
      ext <- tools::file_ext(input$file$name)
      if (tolower(ext) == "csv") {
        df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      } else if (tolower(ext) %in% c("xlsx", "xls")) {
        df <- as.data.frame(read_excel(input$file$datapath))
      } else {
        validate(need(FALSE, "Unsupported file format! Please upload a .csv, .xlsx, or .xls file."))
      }
      return(df)
    })
    
    output$col_selector <- renderUI({
      df <- raw_data()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      validate(need(length(num_cols) > 0, "No numeric columns found in the uploaded dataset."))
      
      default_col <- ifelse(length(num_cols) >= 2, num_cols[2], num_cols[1])
      selectInput(ns("series_col"), "Select Demand Series Column:",
                  choices = num_cols, selected = default_col)
    })
    
    selected_demand <- reactive({
      df <- raw_data()
      req(df, input$series_col)
      vec <- as.numeric(df[[input$series_col]])
      vec <- vec[!is.na(vec)]
      validate(need(length(vec) >= 2, "The selected series must contain at least 2 numeric observations."))
      return(vec)
    })
    
    ses_results <- reactive({
      demand <- selected_demand()
      init_method <- input$init_type
      manual_init <- input$manual_init
      horizon <- max(1, as.integer(input$horizon))
      
      if (input$alpha_mode == "manual") {
        alpha_use <- input$alpha_val
      } else {
        obj_fn <- function(a) {
          res <- calculate_ses_module(demand, a, init_method, manual_init, horizon = 1)
          val <- res[[input$opt_metric]]
          if (is.na(val) || is.infinite(val)) return(1e12)
          return(val)
        }
        opt <- optimize(obj_fn, interval = c(0.001, 0.999))
        alpha_use <- round(opt$minimum, 4)
      }
      
      calculate_ses_module(demand, alpha_use, init_method, manual_init, horizon)
    })
    
    output$metrics_table <- renderTable({
      res <- ses_results()
      data.frame(
        `Optimal / Selected Alpha` = round(res$alpha, 4),
        `Initial Forecast (F1)` = round(res$initial_forecast, 2),
        `MAD` = round(res$MAD, 3),
        `MSE` = round(res$MSE, 3),
        `MAPE (%)` = round(res$MAPE, 3),
        `Equivalent MA Period (N)` = round(res$equivalent_MA_N, 2),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, align = "c")
    
    output$future_table <- renderTable({
      res <- ses_results()
      n <- length(selected_demand())
      h <- length(res$future)
      data.frame(
        `Future Period (t)` = (n + 1):(n + h),
        `Forecasted Value` = round(res$future, 2),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, align = "c")
    
    output$forecast_plot <- renderPlot({
      demand <- selected_demand()
      res <- ses_results()
      n <- length(demand)
      h <- length(res$future)
      
      df_actual <- data.frame(Period = 1:n, Value = demand, Type = "Actual Demand")
      df_fitted <- data.frame(Period = 1:n, Value = res$fitted, Type = "In-Sample Forecast")
      df_future <- data.frame(Period = n:(n + h),
                              Value = c(res$fitted[n], res$future),
                              Type = "Future Forecast")
      
      plot_df <- rbind(df_actual, df_fitted, df_future)
      
      ggplot(plot_df, aes(x = Period, y = Value, color = Type, linetype = Type)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2.5) +
        scale_color_manual(values = c(
          "Actual Demand" = "#1f77b4",
          "In-Sample Forecast" = "#ff7f0e",
          "Future Forecast" = "#2ca02c"
        )) +
        scale_linetype_manual(values = c(
          "Actual Demand" = "solid",
          "In-Sample Forecast" = "dashed",
          "Future Forecast" = "dotted"
        )) +
        labs(
          title = paste0("Simple Exponential Smoothing (Alpha = ", round(res$alpha, 4), ")"),
          subtitle = paste0("MAD = ", round(res$MAD, 2),
                            " | MSE = ", round(res$MSE, 2),
                            " | MAPE = ", round(res$MAPE, 2), "%"),
          x = "Time Period (t)",
          y = "Demand / Forecast",
          color = "Series",
          linetype = "Series"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom")
    })
    
    output$detailed_table <- renderDT({
      demand <- selected_demand()
      res <- ses_results()
      n <- length(demand)
      
      df_out <- data.frame(
        Period = 1:n,
        Actual_Demand = round(demand, 2),
        Forecast = round(res$fitted, 2),
        Error = round(res$errors, 2),
        Absolute_Error = round(res$abs_errors, 2),
        Squared_Error = round(res$sq_errors, 2),
        APE_Percent = round(res$ape, 2)
      )
      
      datatable(df_out, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
    })
  })
}