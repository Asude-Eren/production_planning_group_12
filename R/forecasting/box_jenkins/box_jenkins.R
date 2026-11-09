library(shiny)

# Box-Jenkins User Interface (ArayC<z)
box_jenkins_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Data & Parameters"),
        fileInput(ns("data_file"), "Upload Data (Excel or CSV)", accept = c(".xlsx", ".csv")),
        selectInput(ns("series_col"), "Select Series", choices = NULL),
        
        radioButtons(ns("param_choice"), "Parameter Selection:",
                     choices = c("Manual Entry" = "manual", "Auto Search (Auto.ARIMA)" = "auto")),
        
        conditionalPanel(
          condition = paste0("input['", ns("param_choice"), "'] == 'manual'"),
          numericInput(ns("p_val"), "AR order (p):", value = 0, min = 0),
          numericInput(ns("d_val"), "Differencing (d):", value = 0, min = 0),
          numericInput(ns("q_val"), "MA order (q):", value = 0, min = 0)
        ),
        
        actionButton(ns("run_model"), "Run Model")
      ),
      
      mainPanel(
        h3("Forecast Plot"),
        plotOutput(ns("forecast_plot")),
        
        h3("ACF / PACF Plots"),
        plotOutput(ns("acf_pacf_plot")),
        
        h3("Parameter Estimates"),
        verbatimTextOutput(ns("param_estimates")),
        
        h3("Error Measures (MAD, MSE, MAPE)"),
        tableOutput(ns("error_metrics")),
        
        h3("Forecasts"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

# Box-Jenkins Server (Arka Plan D0Elemleri)
box_jenkins_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 1. Y??klenen dosyay?? okuma
    data_reactive <- reactive({
      req(input$data_file)
      ext <- tools::file_ext(input$data_file$name)
      
      if (ext %in% c("xls", "xlsx")) {
        readxl::read_excel(input$data_file$datapath)
      } else if (ext == "csv") {
        read.csv(input$data_file$datapath)
      } else {
        NULL
      }
    })
    
    # 2. Dosya y??klendi??inde s??tun isimlerini a????l??r listeye g??nderme
    observe({
      df <- data_reactive()
      req(df)
      updateSelectInput(session, "series_col", choices = names(df))
    })
    # 3. Model ??al????t??rma ve Grafikleri ??izdirme
    observeEvent(input$run_model, {
      req(input$series_col)
      df <- data_reactive()
      
      # Veriyi zaman serisine (ts) d??n????t??r
      ts_veri <- ts(df[[input$series_col]])
      
      # Auto.ARIMA modelini hesapla (12 periyot ileri tahmin)
      model <- forecast::auto.arima(ts_veri)
      tahmin <- forecast::forecast(model, h = 12)
      
      # Forecast Grafi??ini ??izdir
      output$forecast_plot <- renderPlot({
        plot(tahmin, main = paste(input$series_col, "????in Box-Jenkins Tahmini"), 
             xlab = "D??nem", ylab = input$series_col, 
             col = "blue", fcol = "red")
      })
      
      # ACF / PACF Grafiklerini ??izdir
      output$acf_pacf_plot <- renderPlot({
        par(mfrow = c(1, 2))
        forecast::Acf(ts_veri, main = "ACF")
        forecast::Pacf(ts_veri, main = "PACF")
      })
    })
  })
}