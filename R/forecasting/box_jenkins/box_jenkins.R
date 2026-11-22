# Box-Jenkins (AR, MA, ARMA, ARIMA) Forecasting Module

box_jenkins_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      fileInput(
        ns("file"),
        "Upload data file (.xlsx or .csv)",
        accept = c(".xlsx", ".xls", ".csv")
      ),
      
      selectInput(
        ns("col"),
        "Select demand / series column",
        choices = NULL
      ),
      
      hr(),
      
      selectInput(
        ns("model_type"),
        "Box-Jenkins model family",
        choices = c(
          "AR(p)" = "AR",
          "MA(q)" = "MA",
          "ARMA(p, q)" = "ARMA",
          "ARIMA(p, d, q)" = "ARIMA"
        ),
        selected = "ARIMA"
      ),
      
      radioButtons(
        ns("param_mode"),
        "How should parameters be chosen?",
        choices = c(
          "I will enter parameters manually" = "manual",
          "Search for the best parameters" = "auto"
        ),
        selected = "manual"
      ),
      
      # Manual parameter inputs
      conditionalPanel(
        condition = "input.param_mode == 'manual' && (input.model_type == 'AR' || input.model_type == 'ARMA' || input.model_type == 'ARIMA')",
        ns = ns,
        numericInput(ns("p"), "AR order (p)", value = 1, min = 0, max = 6, step = 1)
      ),
      conditionalPanel(
        condition = "input.param_mode == 'manual' && input.model_type == 'ARIMA'",
        ns = ns,
        numericInput(ns("d"), "Differencing order (d)", value = 1, min = 0, max = 2, step = 1)
      ),
      conditionalPanel(
        condition = "input.param_mode == 'manual' && (input.model_type == 'MA' || input.model_type == 'ARMA' || input.model_type == 'ARIMA')",
        ns = ns,
        numericInput(ns("q"), "MA order (q)", value = 1, min = 0, max = 6, step = 1)
      ),
      
      # Automatic search inputs
      conditionalPanel(
        condition = "input.param_mode == 'auto'",
        ns = ns,
        selectInput(
          ns("criterion"),
          "Selection criterion",
          choices = c(
            "AICc (recommended)" = "AICc",
            "AIC" = "AIC",
            "BIC" = "BIC",
            "MSE" = "MSE",
            "MAD" = "MAD",
            "MAPE (%)" = "MAPE"
          ),
          selected = "AICc"
        ),
        conditionalPanel(
          condition = "input.model_type == 'AR' || input.model_type == 'ARMA' || input.model_type == 'ARIMA'",
          ns = ns,
          numericInput(ns("max_p"), "Maximum p to search", value = 3, min = 1, max = 5, step = 1)
        ),
        conditionalPanel(
          condition = "input.model_type == 'ARIMA'",
          ns = ns,
          numericInput(ns("max_d"), "Maximum d to search", value = 2, min = 0, max = 2, step = 1)
        ),
        conditionalPanel(
          condition = "input.model_type == 'MA' || input.model_type == 'ARMA' || input.model_type == 'ARIMA'",
          ns = ns,
          numericInput(ns("max_q"), "Maximum q to search", value = 3, min = 1, max = 5, step = 1)
        )
      ),
      
      checkboxInput(ns("include_const"), "Include mean / drift term", value = TRUE),
      
      hr(),
      
      numericInput(
        ns("h"),
        "Forecast horizon (periods ahead)",
        value = 6,
        min = 1,
        max = 36,
        step = 1
      ),
      
      checkboxInput(ns("show_pi"), "Show 80% and 95% prediction intervals", value = TRUE)
    ),
    
    mainPanel(
      width = 8,
      h4(textOutput(ns("model_header"))),
      
      tabsetPanel(
        id = ns("tabs"),
        
        tabPanel(
          "Forecast & Accuracy",
          br(),
          plotOutput(ns("forecast_plot"), height = "420px"),
          h4("Error measures"),
          tableOutput(ns("error_table")),
          h4("Future forecasts"),
          tableOutput(ns("future_table")),
          h4("Fitted values by period"),
          tableOutput(ns("period_table"))
        ),
        
        tabPanel(
          "Identification (ACF / PACF)",
          br(),
          radioButtons(
            ns("acf_diff"),
            "Series to inspect for stationarity and ACF/PACF:",
            choices = c(
              "Match fitted model (d)" = "auto",
              "Original series (d = 0)" = "0",
              "1st difference (d = 1)" = "1",
              "2nd difference (d = 2)" = "2"
            ),
            selected = "auto",
            inline = TRUE
          ),
          p(strong(textOutput(ns("stationarity_info")))),
          plotOutput(ns("series_plot"), height = "280px"),
          plotOutput(ns("acf_pacf_plot"), height = "340px")
        ),
        
        tabPanel(
          "Parameter Estimates",
          br(),
          h4("Estimated model parameters"),
          tableOutput(ns("coef_table")),
          h4("Model fit statistics"),
          tableOutput(ns("fit_stats_table")),
          conditionalPanel(
            condition = "input.param_mode == 'auto'",
            ns = ns,
            h4("Top candidate models evaluated"),
            tableOutput(ns("search_table"))
          )
        ),
        
        tabPanel(
          "Diagnostics (Residuals)",
          br(),
          plotOutput(ns("diag_plot"), height = "520px"),
          h4("Ljung-Box white noise test"),
          tableOutput(ns("lb_table"))
        )
      )
    )
  )
}

box_jenkins_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Helper: robustly convert vector to numeric (supports decimal comma or dot)
    to_numeric <- function(x) {
      if (is.numeric(x)) return(as.numeric(x))
      cleaned <- gsub(",", ".", trimws(as.character(x)))
      suppressWarnings(as.numeric(cleaned))
    }
    
    # Helper: safe ARIMA fitting with fallback methods
    safe_arima <- function(y, order, include_const = TRUE) {
      tryCatch(
        forecast::Arima(y, order = order, include.constant = include_const),
        error = function(e) {
          tryCatch(
            forecast::Arima(y, order = order, include.constant = include_const, method = "ML"),
            error = function(e2) {
              tryCatch(
                forecast::Arima(y, order = order, include.constant = include_const, method = "CSS"),
                error = function(e3) NULL
              )
            }
          )
        }
      )
    }
    
    # Helper: compute MAD, MSE, MAPE on original scale
    calc_errors <- function(actual, fitted_vals, eval_start = 1) {
      n <- length(actual)
      eval_start <- max(1L, min(as.integer(eval_start), n))
      idx <- eval_start:n
      a <- actual[idx]
      f <- fitted_vals[idx]
      valid <- is.finite(a) & is.finite(f)
      if (!any(valid)) {
        return(list(MAD = NA_real_, MSE = NA_real_, MAPE = NA_real_, start = eval_start, end = n))
      }
      e <- f[valid] - a[valid]
      mad_val <- mean(abs(e))
      mse_val <- mean(e^2)
      nz <- a[valid] != 0
      mape_val <- if (any(nz)) mean(abs(e[nz] / a[valid][nz])) * 100 else NA_real_
      list(MAD = mad_val, MSE = mse_val, MAPE = mape_val, start = eval_start, end = n)
    }
    
    # Helper: format model label according to selected family
    format_model_name <- function(model_type, p, d, q) {
      switch(
        model_type,
        "AR"    = paste0("AR(", p, ")"),
        "MA"    = paste0("MA(", q, ")"),
        "ARMA"  = paste0("ARMA(", p, ", ", q, ")"),
        "ARIMA" = paste0("ARIMA(", p, ", ", d, ", ", q, ")")
      )
    }
    
    # 1. Read uploaded file (.xlsx, .xls, .csv)
    raw_data <- reactive({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      
      if (ext %in% c("xlsx", "xls")) {
        df <- as.data.frame(readxl::read_excel(input$file$datapath))
      } else if (ext == "csv") {
        first_line <- readLines(input$file$datapath, n = 1, warn = FALSE)
        sep_char <- if (grepl(";", first_line) && !grepl(",", first_line)) ";" else ","
        df <- utils::read.csv(
          input$file$datapath,
          sep = sep_char,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        validate("Please upload a valid .xlsx, .xls, or .csv file.")
      }
      
      validate(need(nrow(df) >= 6, "The dataset must contain at least 6 observations."))
      df
    })
    
    # Populate numeric series column choices
    observeEvent(raw_data(), {
      df <- raw_data()
      is_num_col <- sapply(df, function(col) sum(is.finite(to_numeric(col))) >= 6)
      num_cols <- names(df)[is_num_col]
      updateSelectInput(
        session,
        "col",
        choices = num_cols,
        selected = if (length(num_cols) > 0) num_cols[1] else NULL
      )
    })
    
    # Extract selected numeric series
    series_vec <- reactive({
      df <- raw_data()
      req(input$col, input$col %in% names(df))
      y <- to_numeric(df[[input$col]])
      y <- y[is.finite(y)]
      validate(need(length(y) >= 6, "Selected column must have at least 6 numeric values."))
      y
    })
    
    # 2. Fit model (manual or automatic search within chosen model family)
    model_result <- reactive({
      y <- series_vec()
      n <- length(y)
      mtype <- input$model_type
      inc_const <- isTRUE(input$include_const)
      
      if (input$param_mode == "manual") {
        p <- if (mtype == "MA") 0L else as.integer(req(input$p))
        d <- if (mtype == "ARIMA") as.integer(req(input$d)) else 0L
        q <- if (mtype == "AR") 0L else as.integer(req(input$q))
        
        validate(
          need(!is.na(p) && p >= 0, "AR order (p) must be a non-negative integer."),
          need(!is.na(d) && d >= 0 && d <= 2, "Differencing order (d) must be 0, 1, or 2."),
          need(!is.na(q) && q >= 0, "MA order (q) must be a non-negative integer.")
        )
        
        if (mtype == "AR") {
          validate(need(p >= 1, "For an AR(p) model, p must be at least 1."))
        } else if (mtype == "MA") {
          validate(need(q >= 1, "For an MA(q) model, q must be at least 1."))
        } else if (mtype == "ARMA") {
          validate(need(p + q >= 1, "For an ARMA(p, q) model, at least one of p or q must be >= 1."))
        }
        
        fit <- safe_arima(y, order = c(p, d, q), include_const = inc_const)
        validate(need(!is.null(fit), "Model could not be fitted with these parameters. Try smaller orders or a different d."))
        
        return(list(
          fit = fit,
          order = c(p = p, d = d, q = q),
          label = format_model_name(mtype, p, d, q),
          search_df = NULL
        ))
      }
      
      # Automatic grid search constrained to the selected model family
      crit <- input$criterion
      max_p <- if (mtype == "MA") 0L else max(1L, min(5L, as.integer(req(input$max_p))))
      max_q <- if (mtype == "AR") 0L else max(1L, min(5L, as.integer(req(input$max_q))))
      max_d <- if (mtype == "ARIMA") max(0L, min(2L, as.integer(req(input$max_d)))) else 0L
      
      p_grid <- if (mtype == "AR") 1:max_p else if (mtype == "MA") 0L else 0:max_p
      q_grid <- if (mtype == "MA") 1:max_q else if (mtype == "AR") 0L else 0:max_q
      
      # For information criteria, fix d via KPSS unit-root test so likelihoods are comparable
      if (mtype == "ARIMA") {
        if (crit %in% c("AICc", "AIC", "BIC")) {
          d_auto <- forecast::ndiffs(y, max.d = max_d)
          d_grid <- as.integer(d_auto)
        } else {
          d_grid <- 0:max_d
        }
      } else {
        d_grid <- 0L
      }
      
      grid <- expand.grid(p = p_grid, d = d_grid, q = q_grid)
      if (mtype == "ARMA") {
        grid <- grid[grid$p + grid$q >= 1, , drop = FALSE]
      }
      if (mtype == "ARIMA") {
        grid <- grid[grid$p + grid$d + grid$q >= 1, , drop = FALSE]
      }
      
      # Align evaluation start across candidates when comparing error metrics
      common_eval_start <- max(d_grid) + 1L
      
      results_list <- vector("list", nrow(grid))
      fits_list <- vector("list", nrow(grid))
      
      for (i in seq_len(nrow(grid))) {
        pi <- grid$p[i]
        di <- grid$d[i]
        qi <- grid$q[i]
        
        fit_i <- safe_arima(y, order = c(pi, di, qi), include_const = inc_const)
        if (is.null(fit_i)) next
        
        f_vals <- as.numeric(stats::fitted(fit_i))
        errs <- calc_errors(y, f_vals, eval_start = common_eval_start)
        
        aic_v  <- if (!is.null(fit_i$aic)  && is.finite(fit_i$aic))  fit_i$aic  else NA_real_
        aicc_v <- if (!is.null(fit_i$aicc) && is.finite(fit_i$aicc)) fit_i$aicc else NA_real_
        bic_v  <- if (!is.null(fit_i$bic)  && is.finite(fit_i$bic))  fit_i$bic  else NA_real_
        
        fits_list[[i]] <- fit_i
        results_list[[i]] <- data.frame(
          Model = format_model_name(mtype, pi, di, qi),
          p = pi,
          d = di,
          q = qi,
          AICc = aicc_v,
          AIC = aic_v,
          BIC = bic_v,
          MAD = errs$MAD,
          MSE = errs$MSE,
          MAPE = errs$MAPE,
          idx = i,
          stringsAsFactors = FALSE
        )
      }
      
      res_df <- do.call(rbind, results_list)
      validate(need(!is.null(res_df) && nrow(res_df) > 0, "No valid model could be fitted for the given search range."))
      
      score_col <- res_df[[crit]]
      valid_rows <- which(is.finite(score_col))
      validate(need(length(valid_rows) > 0, paste("Could not compute", crit, "for candidate models.")))
      
      res_df <- res_df[valid_rows, , drop = FALSE]
      res_df <- res_df[order(res_df[[crit]]), , drop = FALSE]
      
      best_row <- res_df[1, ]
      best_fit <- fits_list[[best_row$idx]]
      
      list(
        fit = best_fit,
        order = c(p = best_row$p, d = best_row$d, q = best_row$q),
        label = best_row$Model,
        search_df = res_df[, c("Model", "AICc", "AIC", "BIC", "MAD", "MSE", "MAPE"), drop = FALSE]
      )
    })
    
    # Forecast object
    fc_obj <- reactive({
      res <- model_result()
      h <- as.integer(req(input$h))
      validate(need(!is.na(h) && h >= 1 && h <= 60, "Forecast horizon must be between 1 and 60."))
      forecast::forecast(res$fit, h = h, level = c(80, 95))
    })
    
    # Header text
    output$model_header <- renderText({
      res <- model_result()
      fc <- fc_obj()
      next_val <- round(as.numeric(fc$mean)[1], 2)
      paste0(res$label, " ??? Next-period forecast: ", next_val)
    })
    
    # 3. Forecast & Accuracy Plot
    output$forecast_plot <- renderPlot({
      y <- series_vec()
      res <- model_result()
      fc <- fc_obj()
      
      n <- length(y)
      h <- length(fc$mean)
      d <- res$order["d"]
      eval_start <- if (d > 0) min(d + 1L, n) else 1L
      
      fitted_vals <- as.numeric(stats::fitted(res$fit))
      fc_mean <- as.numeric(fc$mean)
      lo80 <- as.numeric(fc$lower[, 1])
      hi80 <- as.numeric(fc$upper[, 1])
      lo95 <- as.numeric(fc$lower[, 2])
      hi95 <- as.numeric(fc$upper[, 2])
      
      t_hist <- seq_len(n)
      t_fit <- eval_start:n
      t_fc <- (n + 1):(n + h)
      
      all_y <- c(y, fitted_vals[t_fit], fc_mean)
      if (isTRUE(input$show_pi)) {
        all_y <- c(all_y, lo95, hi95)
      }
      y_rng <- range(all_y, na.rm = TRUE)
      pad <- diff(y_rng) * 0.08
      if (!is.finite(pad) || pad == 0) pad <- 1
      y_lim <- c(y_rng[1] - pad, y_rng[2] + pad)
      
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5))
      graphics::plot(
        t_hist, y,
        type = "n",
        xlim = c(1, n + h),
        ylim = y_lim,
        xlab = "Period",
        ylab = input$col,
        main = paste("Actual Series, Fitted Values, and", res$label, "Forecast")
      )
      graphics::grid(col = "gray88", lty = "dotted")
      
      # Prediction interval bands
      if (isTRUE(input$show_pi)) {
        if (h == 1) {
          graphics::segments(t_fc, lo95, t_fc, hi95, col = "#F4C7A9", lwd = 6)
          graphics::segments(t_fc, lo80, t_fc, hi80, col = "#E59866", lwd = 6)
        } else {
          graphics::polygon(
            c(t_fc, rev(t_fc)), c(lo95, rev(hi95)),
            col = grDevices::adjustcolor("#B4501E", alpha.f = 0.14), border = NA
          )
          graphics::polygon(
            c(t_fc, rev(t_fc)), c(lo80, rev(hi80)),
            col = grDevices::adjustcolor("#B4501E", alpha.f = 0.25), border = NA
          )
        }
      }
      
      # Divider between history and forecast
      graphics::abline(v = n + 0.5, col = "gray60", lty = 3, lwd = 1.2)
      
      # Actual demand
      graphics::lines(t_hist, y, col = "#333333", lwd = 1.8)
      graphics::points(t_hist, y, col = "#333333", pch = 19, cex = 0.75)
      
      # In-sample fitted values
      graphics::lines(t_fit, fitted_vals[t_fit], col = "#2E74B5", lty = 2, lwd = 1.6)
      graphics::points(t_fit, fitted_vals[t_fit], col = "#2E74B5", pch = 17, cex = 0.7)
      
      # Connector + future forecasts
      graphics::lines(c(n, t_fc[1]), c(y[n], fc_mean[1]), col = "#B4501E", lty = 3, lwd = 1.4)
      graphics::lines(t_fc, fc_mean, col = "#B4501E", lwd = 2.2)
      graphics::points(t_fc, fc_mean, col = "#B4501E", pch = 17, cex = 0.95)
      
      leg_labels <- c("Actual series", "Fitted forecast", "Future forecast")
      leg_cols <- c("#333333", "#2E74B5", "#B4501E")
      leg_lty <- c(1, 2, 1)
      leg_pch <- c(19, 17, 17)
      if (isTRUE(input$show_pi)) {
        leg_labels <- c(leg_labels, "80% / 95% PI")
        leg_cols <- c(leg_cols, grDevices::adjustcolor("#B4501E", alpha.f = 0.35))
        leg_lty <- c(leg_lty, 1)
        leg_pch <- c(leg_pch, 15)
      }
      
      graphics::legend(
        "topleft",
        legend = leg_labels,
        col = leg_cols,
        lty = leg_lty,
        pch = leg_pch,
        lwd = c(1.8, 1.6, 2.2, 6)[seq_along(leg_labels)],
        bty = "o",
        bg = grDevices::adjustcolor("white", alpha.f = 0.85),
        box.col = "gray80",
        cex = 0.85
      )
    })
    
    # Error measures table (MAD, MSE, MAPE)
    output$error_table <- renderTable({
      y <- series_vec()
      res <- model_result()
      d <- res$order["d"]
      eval_start <- if (d > 0) min(d + 1L, length(y)) else 1L
      fitted_vals <- as.numeric(stats::fitted(res$fit))
      errs <- calc_errors(y, fitted_vals, eval_start = eval_start)
      period_range <- paste0(errs$start, "???", errs$end)
      
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = round(c(errs$MAD, errs$MSE, errs$MAPE), 3),
        `Periods evaluated` = rep(period_range, 3),
        check.names = FALSE
      )
    }, digits = 3)
    
    # Future forecasts table
    output$future_table <- renderTable({
      y <- series_vec()
      fc <- fc_obj()
      n <- length(y)
      h <- length(fc$mean)
      
      data.frame(
        Period = (n + 1):(n + h),
        `Point Forecast` = round(as.numeric(fc$mean), 2),
        `Lo 80` = round(as.numeric(fc$lower[, 1]), 2),
        `Hi 80` = round(as.numeric(fc$upper[, 1]), 2),
        `Lo 95` = round(as.numeric(fc$lower[, 2]), 2),
        `Hi 95` = round(as.numeric(fc$upper[, 2]), 2),
        check.names = FALSE
      )
    }, digits = 2)
    
    # Historical fitted & error table
    output$period_table <- renderTable({
      y <- series_vec()
      res <- model_result()
      n <- length(y)
      d <- res$order["d"]
      fitted_vals <- as.numeric(stats::fitted(res$fit))
      
      if (d > 0) {
        fitted_vals[seq_len(min(d, n))] <- NA_real_
      }
      err_vals <- fitted_vals - y
      
      data.frame(
        Period = seq_len(n),
        `Demand (D)` = round(y, 2),
        `Forecast (F)` = round(fitted_vals, 2),
        `Error (e = F - D)` = round(err_vals, 2),
        check.names = FALSE
      )
    }, digits = 2, na = "")
    
    # 4. Identification Tab: Series + ACF & PACF (starting cleanly at Lag 1)
    acf_working_data <- reactive({
      y <- series_vec()
      d_view <- if (input$acf_diff == "auto") {
        tryCatch(as.integer(model_result()$order["d"]), error = function(e) 0L)
      } else {
        as.integer(input$acf_diff)
      }
      
      work_y <- if (d_view > 0) diff(y, differences = d_view) else y
      validate(need(length(work_y) >= 4, "Not enough observations left after differencing."))
      
      lbl <- if (d_view == 0) {
        "Original Series (d = 0)"
      } else if (d_view == 1) {
        "1st Differenced Series (d = 1)"
      } else {
        paste0(d_view, "nd Differenced Series (d = ", d_view, ")")
      }
      
      list(orig = y, work = work_y, d_view = d_view, label = lbl)
    })
    
    output$stationarity_info <- renderText({
      y <- series_vec()
      d_rec <- forecast::ndiffs(y, max.d = 2)
      paste0(
        "Stationarity check (KPSS unit-root test): recommended differencing order is d = ",
        d_rec,
        if (d_rec == 0) " (series appears stationary)." else " (differencing recommended to achieve stationarity)."
      )
    })
    
    output$series_plot <- renderPlot({
      wd <- acf_working_data()
      if (wd$d_view == 0) {
        graphics::par(mfrow = c(1, 1), mar = c(4, 4.5, 2.8, 1.5))
        graphics::plot(
          seq_along(wd$orig), wd$orig,
          type = "o", pch = 19, cex = 0.7, col = "#333333", lwd = 1.6,
          xlab = "Period", ylab = input$col,
          main = "Original Series (d = 0)"
        )
        graphics::grid(col = "gray88", lty = "dotted")
      } else {
        graphics::par(mfrow = c(1, 2), mar = c(4, 4.2, 2.8, 1.2))
        graphics::plot(
          seq_along(wd$orig), wd$orig,
          type = "o", pch = 19, cex = 0.65, col = "#333333", lwd = 1.5,
          xlab = "Period", ylab = input$col,
          main = "Original Series (d = 0)"
        )
        graphics::grid(col = "gray88", lty = "dotted")
        
        graphics::plot(
          seq_along(wd$work), wd$work,
          type = "o", pch = 19, cex = 0.65, col = "#2E74B5", lwd = 1.5,
          xlab = "Observation", ylab = paste0("Diff(d=", wd$d_view, ")"),
          main = wd$label
        )
        graphics::abline(h = 0, col = "#B4501E", lty = 2)
        graphics::grid(col = "gray88", lty = "dotted")
      }
    })
    
    output$acf_pacf_plot <- renderPlot({
      wd <- acf_working_data()
      max_lag <- min(20L, max(1L, length(wd$work) - 2L))
      
      graphics::par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3.0, 1.2))
      forecast::Acf(
        wd$work,
        lag.max = max_lag,
        main = paste("ACF ???", wd$label),
        col = "#2E74B5",
        lwd = 2
      )
      forecast::Pacf(
        wd$work,
        lag.max = max_lag,
        main = paste("PACF ???", wd$label),
        col = "#B4501E",
        lwd = 2
      )
    })
    
    # 5. Parameter Estimates Tab
    output$coef_table <- renderTable({
      fit <- model_result()$fit
      coefs <- stats::coef(fit)
      
      if (length(coefs) == 0) {
        return(data.frame(
          Parameter = "None",
          Note = "No AR, MA, or constant parameters estimated (pure random walk)."
        ))
      }
      
      vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
      ses <- if (!is.null(vc) && nrow(vc) == length(coefs)) {
        sqrt(pmax(0, diag(vc)))
      } else {
        rep(NA_real_, length(coefs))
      }
      
      z_vals <- ifelse(is.finite(ses) & ses > 0, coefs / ses, NA_real_)
      p_vals <- ifelse(is.finite(z_vals), 2 * (1 - stats::pnorm(abs(z_vals))), NA_real_)
      sig <- ifelse(
        is.na(p_vals), "",
        ifelse(p_vals < 0.001, "***",
               ifelse(p_vals < 0.01, "**",
                      ifelse(p_vals < 0.05, "*",
                             ifelse(p_vals < 0.1, ".", ""))))
      )
      
      data.frame(
        Parameter = names(coefs),
        Estimate = round(as.numeric(coefs), 4),
        `Std. Error` = round(ses, 4),
        `z-value` = round(z_vals, 3),
        `p-value` = round(p_vals, 4),
        Sig. = sig,
        check.names = FALSE
      )
    }, digits = 4, na = "-")
    
    output$fit_stats_table <- renderTable({
      res <- model_result()
      fit <- res$fit
      data.frame(
        Metric = c("Selected Model", "Sigma^2 (Residual Variance)", "Log-Likelihood", "AIC", "AICc", "BIC"),
        Value = c(
          res$label,
          round(fit$sigma2, 4),
          if (!is.null(fit$loglik) && is.finite(fit$loglik)) round(fit$loglik, 3) else "-",
          if (!is.null(fit$aic)    && is.finite(fit$aic))    round(fit$aic, 3)    else "-",
          if (!is.null(fit$aicc)   && is.finite(fit$aicc))   round(fit$aicc, 3)   else "-",
          if (!is.null(fit$bic)    && is.finite(fit$bic))    round(fit$bic, 3)    else "-"
        ),
        check.names = FALSE
      )
    })
    
    output$search_table <- renderTable({
      df <- model_result()$search_df
      req(df)
      utils::head(df, 10)
    }, digits = 3, na = "-")
    
    # 6. Diagnostics Tab: Residual Plots & Ljung-Box Test
    output$diag_plot <- renderPlot({
      fit <- model_result()$fit
      res_vals <- as.numeric(stats::residuals(fit))
      res_clean <- res_vals[is.finite(res_vals)]
      validate(need(length(res_clean) >= 4, "Not enough residuals for diagnostics."))
      
      max_lag <- min(20L, max(1L, length(res_clean) - 2L))
      
      graphics::par(mfrow = c(2, 2), mar = c(4, 4.2, 2.8, 1.2))
      
      # (1) Residuals over time
      graphics::plot(
        seq_along(res_vals), res_vals,
        type = "o", pch = 19, cex = 0.65, col = "#333333", lwd = 1.4,
        xlab = "Period", ylab = "Residual",
        main = "Residuals over Time"
      )
      graphics::abline(h = 0, col = "#B4501E", lty = 2, lwd = 1.4)
      graphics::grid(col = "gray88", lty = "dotted")
      
      # (2) Histogram + Normal density curve
      h_obj <- graphics::hist(res_clean, plot = FALSE)
      sd_r <- stats::sd(res_clean)
      mean_r <- mean(res_clean)
      max_dens <- max(h_obj$density, if (is.finite(sd_r) && sd_r > 0) stats::dnorm(mean_r, mean_r, sd_r) else 0)
      
      graphics::hist(
        res_clean,
        prob = TRUE,
        ylim = c(0, max_dens * 1.15),
        col = "#9DB4CF",
        border = "white",
        xlab = "Residual",
        main = "Residual Distribution"
      )
      if (is.finite(sd_r) && sd_r > 0) {
        x_seq <- seq(min(res_clean), max(res_clean), length.out = 100)
        graphics::lines(x_seq, stats::dnorm(x_seq, mean = mean_r, sd = sd_r), col = "#B4501E", lwd = 2)
      }
      
      # (3) Residual ACF (starts at Lag 1)
      forecast::Acf(
        res_clean,
        lag.max = max_lag,
        main = "Residual ACF",
        col = "#2E74B5",
        lwd = 2
      )
      
      # (4) Residual PACF (starts at Lag 1)
      forecast::Pacf(
        res_clean,
        lag.max = max_lag,
        main = "Residual PACF",
        col = "#B4501E",
        lwd = 2
      )
    })
    
    output$lb_table <- renderTable({
      res_obj <- model_result()
      res_clean <- as.numeric(stats::residuals(res_obj$fit))
      res_clean <- res_clean[is.finite(res_clean)]
      
      fitdf <- as.integer(res_obj$order["p"] + res_obj$order["q"])
      lb_lag <- min(length(res_clean) - 1L, max(10L, fitdf + 2L))
      
      if (lb_lag <= fitdf || length(res_clean) <= fitdf + 1L) {
        return(data.frame(Result = "Not enough observations for the Ljung-Box test."))
      }
      
      lb <- stats::Box.test(res_clean, lag = lb_lag, type = "Ljung-Box", fitdf = fitdf)
      p_val <- as.numeric(lb$p.value)
      conclusion <- if (is.finite(p_val) && p_val > 0.05) {
        "Residuals resemble white noise (p > 0.05) ??? model is adequate."
      } else {
        "Significant autocorrelation remains (p <= 0.05) ??? consider adjusting p, d, or q."
      }
      
      data.frame(
        `Lags Tested` = lb_lag,
        `Q-Statistic` = round(as.numeric(lb$statistic), 3),
        df = as.integer(lb$parameter),
        `p-value` = round(p_val, 4),
        Interpretation = conclusion,
        check.names = FALSE
      )
    })
  })
}