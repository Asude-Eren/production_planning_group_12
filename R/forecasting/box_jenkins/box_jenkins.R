library(shiny)

# Box-Jenkins User Interface (Arayüz)
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

# Box-Jenkins Server (Arka Plan İşlemleri)
box_jenkins_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Not: Arka plan hesaplamaları (veri okuma, model çalıştırma) 
    # projenin ilerleyen aşamalarında buraya eklenecek.
    
  })
}
