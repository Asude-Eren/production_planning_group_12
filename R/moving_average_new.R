library(shiny)
library(readxl)

moving_average_yeni_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ---- Görünüm (CSS) ----
    tags$head(tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap');

      .mav-modul {
        font-family: 'Poppins', 'Segoe UI', sans-serif; color: #26324A;
        font-size: 13px; padding-top: 28px; padding-bottom: 80px;
      }

      /* Sol ayar kutusu */
      .mav-modul .well {
        background: #F1F4F9; border: 1px solid #CBD5E3; border-top: 4px solid #2C3E63;
        border-radius: 10px; padding: 14px; box-shadow: none;
      }
      .mav-modul .control-label { font-weight: 600; color: #2C3E63; font-size: 13px; }
      .mav-modul .form-group { margin-bottom: 12px; }
      .mav-modul .form-control {
        height: 32px; padding: 4px 10px; font-size: 13px;
        border-radius: 6px; border: 1px solid #CBD5E3; box-shadow: none;
      }
      .mav-modul .form-control:focus { border-color: #4A6FA5; }
      .mav-modul .btn-default {
        background: #2C3E63; color: #fff; border: none;
        height: 32px; padding: 4px 12px; font-size: 13px; border-radius: 6px 0 0 6px;
      }
      .mav-modul .progress { height: 18px; margin-bottom: 0; }
      .mav-modul .progress-bar { background-color: #E76F51; font-size: 11px; line-height: 18px; }

      /* Basliklar */
      .mav-modul h3 {
        font-family: 'Poppins', sans-serif; font-weight: 600; font-size: 18px;
        color: #2C3E63; background: #F1F4F9; border-left: 4px solid #E76F51;
        padding: 8px 14px; border-radius: 6px; margin: 0 0 10px 0;
      }
      .mav-modul h4 {
        font-family: 'Poppins', sans-serif; font-weight: 600; font-size: 15px;
        color: #2C3E63; margin: 18px 0 8px 0;
        display: table; border-bottom: 2px solid #E76F51; padding-bottom: 2px;
      }

      /* Tablolar */
      .mav-modul table {
        width: auto; min-width: 360px; background: #fff; font-size: 12.5px;
        border-collapse: separate; border-spacing: 0;
      }
      .mav-modul table th {
        background: #2C3E63; color: #fff; font-weight: 600; padding: 5px 12px;
        text-align: center; position: sticky; top: 0;
      }
      .mav-modul table th:first-child { border-top-left-radius: 6px; }
      .mav-modul table th:last-child  { border-top-right-radius: 6px; }
      .mav-modul table td {
        padding: 4px 12px; text-align: center; border-top: 1px solid #E1E8F1;
      }
      .mav-modul table tr:nth-child(even) td { background: #F6F8FB; }
      .mav-modul table tr:hover td { background: #FCEDE8; }

      /* Kaydirilabilir tablo kutusu */
      .mav-scroll {
        max-height: 280px; overflow-y: auto; display: block; width: fit-content;
        border: 1px solid #CBD5E3; border-radius: 8px;
      }

      /* Grafik aciklamasi (legend) */
      .mav-lejant { font-size: 12px; margin: 2px 0 4px 0; color: #26324A; }
      .mav-k {
        display: inline-block; width: 10px; height: 10px; border-radius: 50%;
        margin: 0 5px 0 16px; vertical-align: middle;
      }
      .mav-lejant .mav-k:first-child { margin-left: 0; }
      .mav-k-kare  { border-radius: 2px; }
      .mav-k-elmas { border-radius: 2px; transform: rotate(45deg); }

      /* Kucuk not */
      .mav-not { font-size: 11.5px; color: #6B7A94; margin-top: 4px; }
    "))),
    
    # ---- Sayfa yapısı ----
    div(class = "mav-modul",
        sidebarLayout(
          sidebarPanel(width = 3,
                       fileInput(ns("dosya"), "Upload an Excel file (.xlsx)", accept = ".xlsx"),
                       
                       selectInput(ns("kolon"), "Demand column", choices = NULL),
                       
                       radioButtons(
                         ns("mod"), "How should N be chosen?",
                         choices = c("I will choose N" = "manuel",
                                     "Find the best N" = "otomatik")
                       ),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'manuel'", ns("mod")),
                         numericInput(ns("N"), "N (number of past periods)",
                                      value = 3, min = 1, step = 1)
                       ),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'otomatik'", ns("mod")),
                         numericInput(ns("maxN"), "Maximum N to try",
                                      value = 6, min = 2, step = 1),
                         radioButtons(ns("kriter"), "Best N means lowest:",
                                      choices = c("MAD", "MSE", "MAPE"))
                       )
          ),
          
          mainPanel(width = 9,
                    h3(textOutput(ns("baslik"))),
                    
                    # Grafik açıklaması grafiğin ÜSTÜNDE
                    div(class = "mav-lejant",
                        span(class = "mav-k", style = "background:#2C3E63;"), "Actual demand",
                        span(class = "mav-k mav-k-kare", style = "background:#E76F51;"), "Forecast",
                        span(class = "mav-k mav-k-elmas", style = "background:#E9B44C;"), "Next-period forecast"
                    ),
                    plotOutput(ns("tahmin_grafik"), height = "320px"),
                    
                    h4("Error measures"),
                    tableOutput(ns("hata_tablo")),
                    div(style = "margin-top: 12px;", tableOutput(ns("en_iyi_tablo"))),
                    div(class = "mav-not", textOutput(ns("en_iyi_not"))),
                    
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'otomatik'", ns("mod")),
                      h4("Comparison of N values"),
                      tableOutput(ns("n_tablo")),
                      plotOutput(ns("grafik"), height = "260px")
                    ),
                    
                    h4("Forecasts by period"),
                    div(class = "mav-scroll", tableOutput(ns("donem_tablo")))
          )
        )
    )
  )
}

moving_average_yeni_server <- function(id) {
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
    
    # ---- MA(N) tahminleri (son eleman = gelecek dönem tahmini) ----
    hesapla <- function(d, N) {
      n <- length(d)
      tahmin <- rep(NA_real_, n + 1)
      for (t in (N + 1):(n + 1)) {
        tahmin[t] <- mean(d[(t - N):(t - 1)])
      }
      tahmin
    }
    
    # ---- Hata ölçüleri: MAD, MSE, MAPE ----
    hata_olc <- function(d, tahmin) {
      n  <- length(d)
      ok <- !is.na(tahmin[1:n])
      e  <- d[ok] - tahmin[1:n][ok]
      c(MAD  = mean(abs(e)),
        MSE  = mean(e^2),
        MAPE = mean(abs(e / d[ok])) * 100)
    }
    
    # ---- N = 1..ust için hata tablosu ----
    tara <- function(d, ust) {
      sonuc_liste <- lapply(1:ust, function(k) {
        h <- hata_olc(d, hesapla(d, k))
        data.frame(N = k,
                   MAD = unname(h["MAD"]),
                   MSE = unname(h["MSE"]),
                   MAPE = unname(h["MAPE"]))
      })
      do.call(rbind, sonuc_liste)
    }
    
    # ---- Otomatik mod: N = 1..maxN hepsini dene ----
    arama <- reactive({
      req(input$maxN)
      d    <- talep()
      maxN <- min(input$maxN, length(d) - 1)
      validate(need(maxN >= 1, "Not enough data."))
      tara(d, maxN)
    })
    
    # ---- Kullanılacak N (manuel veya en iyi) ----
    secilen_N <- reactive({
      if (input$mod == "manuel") {
        req(input$N)
        validate(need(input$N >= 1 && input$N == round(input$N),
                      "N must be a positive integer."),
                 need(input$N < length(talep()),
                      "N must be smaller than the number of periods."))
        input$N
      } else {
        tablo <- arama()
        tablo$N[which.min(tablo[[input$kriter]])]
      }
    })
    
    # ---- Üstteki başlık ----
    output$baslik <- renderText({
      d <- talep()
      N <- secilen_N()
      f <- hesapla(d, N)
      paste0("MA(", N, ") — next-period forecast: ",
             sprintf("%.2f", f[length(d) + 1]))
    })
    
    # ---- Gerçek talep ve tahmin çizgi grafiği (yazı yok, açıklama üstte) ----
    output$tahmin_grafik <- renderPlot({
      d <- talep()
      N <- secilen_N()
      n <- length(d)
      f <- hesapla(d, N)
      
      par(bg = "#FFFFFF", mar = c(4, 4, 2.5, 1), cex.main = 1, cex.lab = 0.9,
          cex.axis = 0.85)
      plot(1:n, d, type = "o", pch = 16, cex = 0.7, col = "#2C3E63", lwd = 1.5,
           xlim = c(1, n + 1), ylim = range(c(d, f), na.rm = TRUE),
           xlab = "Period", ylab = "Demand",
           main = paste0("Actual demand and MA(", N, ") forecast"))
      grid(col = "#E1E8F1")
      lines(1:n, f[1:n], type = "o", pch = 15, cex = 0.6, lty = 1, lwd = 1.5,
            col = "#E76F51")
      points(n + 1, f[n + 1], pch = 18, cex = 2.2, col = "#E9B44C")
    })
    
    # ---- Error measures tablosu ----
    output$hata_tablo <- renderTable({
      d <- talep()
      N <- secilen_N()
      n <- length(d)
      h <- hata_olc(d, hesapla(d, N))
      data.frame(
        "Measure" = c("MAD", "MSE", "MAPE (%)"),
        "Value"   = round(unname(h), 2),
        "Periods evaluated" = paste0(N + 1, "–", n),
        check.names = FALSE
      )
    }, digits = 2)
    
    # ---- Her ölçü için en iyi N ----
    output$en_iyi_tablo <- renderTable({
      d <- talep()
      validate(need(length(d) >= 2, "Not enough data."))
      if (input$mod == "otomatik") {
        tablo <- arama()
      } else {
        tablo <- tara(d, length(d) - 1)
      }
      data.frame(
        "Best by"     = c("MAD", "MSE", "MAPE (%)"),
        "Best N"      = as.integer(c(tablo$N[which.min(tablo$MAD)],
                                     tablo$N[which.min(tablo$MSE)],
                                     tablo$N[which.min(tablo$MAPE)])),
        "Error value" = round(c(min(tablo$MAD, na.rm = TRUE),
                                min(tablo$MSE, na.rm = TRUE),
                                min(tablo$MAPE, na.rm = TRUE)), 2),
        check.names = FALSE
      )
    }, digits = 2)
    
    output$en_iyi_not <- renderText({
      d <- talep()
      if (input$mod == "otomatik") {
        req(input$maxN)
        ust <- min(input$maxN, length(d) - 1)
      } else {
        ust <- length(d) - 1
      }
      paste0("Best N is searched among N = 1–", ust, ".")
    })
    
    # ---- Otomatik mod: tüm N'lerin karşılaştırma tablosu ----
    output$n_tablo <- renderTable({
      req(input$mod == "otomatik")
      tablo <- arama()
      tablo$MAD  <- round(tablo$MAD, 3)
      tablo$MSE  <- round(tablo$MSE, 3)
      tablo$MAPE <- round(tablo$MAPE, 3)
      tablo$N    <- as.integer(tablo$N)
      tablo
    })
    
    # ---- Otomatik mod: çubuk grafik (en iyi N mercan) ----
    output$grafik <- renderPlot({
      req(input$mod == "otomatik")
      tablo <- arama()
      k <- input$kriter
      renk <- ifelse(tablo$N == secilen_N(), "#E76F51", "#B7C4D9")
      barplot(tablo[[k]], names.arg = tablo$N, col = renk, border = NA,
              xlab = "N", ylab = k,
              main = paste0("Best N by ", k, " = ", secilen_N()))
    })
    
    # ---- Dönem bazlı tahmin tablosu (Error = F - D) ----
    output$donem_tablo <- renderTable({
      d <- talep()
      n <- length(d)
      f <- hesapla(d, secilen_N())
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

