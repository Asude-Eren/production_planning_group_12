library(shiny)
library(readxl)

weighted_moving_average_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ---- Görünüm (CSS) ----
    tags$head(tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap');

      .wma-modul {
        font-family: 'Poppins', 'Segoe UI', sans-serif; color: #26324A;
        font-size: 13px; padding-top: 28px; padding-bottom: 80px;
      }

      .wma-modul .well {
        background: #F1F4F9; border: 1px solid #CBD5E3; border-top: 4px solid #2C3E63;
        border-radius: 10px; padding: 14px; box-shadow: none;
      }
      .wma-modul .control-label { font-weight: 600; color: #2C3E63; font-size: 13px; }
      .wma-modul .form-group { margin-bottom: 12px; }
      .wma-modul .form-control {
        height: 32px; padding: 4px 10px; font-size: 13px;
        border-radius: 6px; border: 1px solid #CBD5E3; box-shadow: none;
      }
      .wma-modul .form-control:focus { border-color: #4A6FA5; }
      .wma-modul .btn-default {
        background: #2C3E63; color: #fff; border: none;
        height: 32px; padding: 4px 12px; font-size: 13px; border-radius: 6px 0 0 6px;
      }
      .wma-modul .progress { height: 18px; margin-bottom: 0; }
      .wma-modul .progress-bar { background-color: #E76F51; font-size: 11px; line-height: 18px; }

      .wma-modul h3 {
        font-family: 'Poppins', sans-serif; font-weight: 600; font-size: 18px;
        color: #2C3E63; background: #F1F4F9; border-left: 4px solid #E76F51;
        padding: 8px 14px; border-radius: 6px; margin: 0 0 10px 0;
      }
      .wma-modul h4 {
        font-family: 'Poppins', sans-serif; font-weight: 600; font-size: 15px;
        color: #2C3E63; margin: 18px 0 8px 0;
        display: table; border-bottom: 2px solid #E76F51; padding-bottom: 2px;
      }

      .wma-modul table {
        width: auto; min-width: 360px; background: #fff; font-size: 12.5px;
        border-collapse: separate; border-spacing: 0;
      }
      .wma-modul table th {
        background: #2C3E63; color: #fff; font-weight: 600; padding: 5px 12px;
        text-align: center; position: sticky; top: 0;
      }
      .wma-modul table th:first-child { border-top-left-radius: 6px; }
      .wma-modul table th:last-child  { border-top-right-radius: 6px; }
      .wma-modul table td {
        padding: 4px 12px; text-align: center; border-top: 1px solid #E1E8F1;
      }
      .wma-modul table tr:nth-child(even) td { background: #F6F8FB; }
      .wma-modul table tr:hover td { background: #FCEDE8; }

      .wma-scroll {
        max-height: 280px; overflow-y: auto; display: block; width: fit-content;
        border: 1px solid #CBD5E3; border-radius: 8px;
      }

      .wma-lejant { font-size: 12px; margin: 2px 0 4px 0; color: #26324A; }
      .wma-k {
        display: inline-block; width: 10px; height: 10px; border-radius: 50%;
        margin: 0 5px 0 16px; vertical-align: middle;
      }
      .wma-lejant .wma-k:first-child { margin-left: 0; }
      .wma-k-kare  { border-radius: 2px; }
      .wma-k-elmas { border-radius: 2px; transform: rotate(45deg); }

      .wma-not { font-size: 11.5px; color: #6B7A94; margin: 4px 0 8px 0; }
    "))),
    
    # ---- Sayfa yapısı ----
    div(class = "wma-modul",
        sidebarLayout(
          sidebarPanel(width = 3,
                       fileInput(ns("dosya"), "Upload an Excel file (.xlsx)", accept = ".xlsx"),
                       
                       selectInput(ns("kolon"), "Demand column", choices = NULL),
                       
                       numericInput(ns("N"), "N (number of past periods)",
                                    value = 3, min = 2, max = 20, step = 1),
                       
                       radioButtons(
                         ns("agirlik_mod"), "How should the weights be set?",
                         choices = c("Linearly decreasing (recent weighs most)" = "azalan",
                                     "I will enter the weights" = "manuel")
                       ),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'manuel'", ns("agirlik_mod")),
                         uiOutput(ns("agirlik_ui"))
                       )
          ),
          
          mainPanel(width = 9,
                    h3(textOutput(ns("baslik"))),
                    
                    div(class = "wma-lejant",
                        span(class = "wma-k", style = "background:#2C3E63;"), "Actual demand",
                        span(class = "wma-k wma-k-kare", style = "background:#E76F51;"), "Forecast",
                        span(class = "wma-k wma-k-elmas", style = "background:#E9B44C;"), "Next-period forecast"
                    ),
                    plotOutput(ns("tahmin_grafik"), height = "320px"),
                    
                    h4("Error measures"),
                    tableOutput(ns("hata_tablo")),
                    
                    h4("Weights used"),
                    tableOutput(ns("agirlik_tablo")),
                    div(class = "wma-not",
                        "t-1 is the most recent period. Weights are divided by their sum, so they do not need to add up to 1."),
                    
                    h4("Forecasts by period"),
                    div(class = "wma-scroll", tableOutput(ns("donem_tablo")))
          )
        )
    )
  )
}

weighted_moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ---- Excel'i oku ----
    veri <- reactive({
      req(input$dosya)
      read_excel(input$dosya$datapath)
    })
    
    # ---- Demand column listesini doldur ----
    observeEvent(veri(), {
      sayisal <- names(veri())[sapply(veri(), is.numeric)]
      updateSelectInput(session, "kolon", choices = sayisal)
    })
    
    # ---- Talep vektörü ----
    talep <- reactive({
      req(input$kolon)
      d <- veri()[[input$kolon]]
      d[!is.na(d)]
    })
    
    # ---- Geçerli N ----
    N_sec <- reactive({
      req(input$N)
      validate(
        need(input$N >= 2 && input$N == round(input$N),
             "N must be an integer of at least 2."),
        need(input$N <= 20, "N can be at most 20."),
        need(input$N < length(talep()),
             "N must be smaller than the number of periods.")
      )
      input$N
    })
    
    # ---- Manuel mod: her ağırlık için bir kutu (w1 = en yeni dönem) ----
    output$agirlik_ui <- renderUI({
      req(input$N)
      N <- input$N
      req(N >= 2, N <= 20, N == round(N))
      tagList(
        div(class = "wma-not", "w1 = most recent period (t-1), w2 = t-2, ..."),
        lapply(1:N, function(i) {
          numericInput(session$ns(paste0("w", i)),
                       paste0("w", i, "  (t-", i, ")"),
                       value = N - i + 1, min = 0, step = 0.1)
        })
      )
    })
    
    # ---- Kullanılacak ağırlıklar ----
    agirliklar <- reactive({
      N <- N_sec()
      if (input$agirlik_mod == "azalan") {
        w <- N:1
      } else {
        w <- sapply(1:N, function(i) {
          v <- input[[paste0("w", i)]]
          if (is.null(v)) NA_real_ else v
        })
        validate(
          need(!any(is.na(w)), "Please enter all the weights."),
          need(all(w >= 0), "Weights cannot be negative."),
          need(sum(w) > 0, "The sum of the weights must be greater than zero.")
        )
      }
      w
    })
    
    # ---- WMA tahminleri (son eleman = gelecek dönem tahmini) ----
    # w[1] en yeni dönemin (t-1) ağırlığı
    hesapla <- function(d, w) {
      n <- length(d)
      N <- length(w)
      tahmin <- rep(NA_real_, n + 1)
      for (t in (N + 1):(n + 1)) {
        tahmin[t] <- sum(w * d[(t - 1):(t - N)]) / sum(w)
      }
      tahmin
    }
    
    # ---- Hata ölçüleri: MAD, MSE, MAPE (talebi 0 olan dönemler MAPE'ye girmez) ----
    hata_olc <- function(d, tahmin, bas) {
      idx <- bas:length(d)
      idx <- idx[!is.na(tahmin[idx])]
      e   <- tahmin[idx] - d[idx]
      sifir_degil <- d[idx] != 0
      c(MAD  = mean(abs(e)),
        MSE  = mean(e^2),
        MAPE = mean(abs(e[sifir_degil] / d[idx][sifir_degil])) * 100)
    }
    
    # ---- Üstteki başlık ----
    output$baslik <- renderText({
      d <- talep()
      w <- agirliklar()
      f <- hesapla(d, w)
      paste0("WMA(", length(w), ") — next-period forecast: ",
             sprintf("%.2f", f[length(d) + 1]))
    })
    
    # ---- Gerçek talep ve tahmin grafiği ----
    output$tahmin_grafik <- renderPlot({
      d <- talep()
      w <- agirliklar()
      n <- length(d)
      f <- hesapla(d, w)
      
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
    
    # ---- Error measures tablosu ----
    output$hata_tablo <- renderTable({
      d   <- talep()
      w   <- agirliklar()
      n   <- length(d)
      bas <- length(w) + 1
      h   <- hata_olc(d, hesapla(d, w), bas = bas)
      data.frame(
        "Measure" = c("MAD", "MSE", "MAPE (%)"),
        "Value"   = round(unname(h), 2),
        "Periods evaluated" = paste0(bas, "–", n),
        check.names = FALSE
      )
    }, digits = 2)
    
    # ---- Kullanılan ağırlıklar ----
    output$agirlik_tablo <- renderTable({
      w <- agirliklar()
      data.frame(
        "Period"              = paste0("t-", seq_along(w)),
        "Weight (w)"          = w,
        "Share of total (%)"  = w / sum(w) * 100,
        check.names = FALSE
      )
    }, digits = 2)
    
    # ---- Dönem bazlı tahmin tablosu (Error = F - D) ----
    output$donem_tablo <- renderTable({
      d <- talep()
      n <- length(d)
      f <- hesapla(d, agirliklar())
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
