library(shiny)
library(forecast)

seasonal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Dataset (CSV/Excel)", accept = c(".csv", ".xlsx")),
        selectInput(ns("method"), "Select Seasonal Forecasting Method:",
                    choices = c(
                      "Seasonal Factors (Quick & Dirty)" = "quick",
                      "Seasonal Decomposition (Moving Averages)" = "decomp",
                      "Winters' Method (Holt-Winters)" = "winters"
                    )),
        numericInput(ns("frequency"), "Seasonal Frequency (e.g., 12 for Monthly, 4 for Quarterly):", value = 12, min = 1),
        actionButton(ns("run"), "Run Forecast", class = "btn-primary")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput(ns("plot"))),
          tabPanel("Summary & Results", verbatimTextOutput(ns("summary")))
        )
      )
    )
  )
}

seasonal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data_reactive <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        read.csv(input$file$datapath)
      } else {
        readxl::read_excel(input$file$datapath)
      }
    })
    
    results <- eventReactive(input$run, {
      df <- data_reactive()
      ts_data <- ts(df[, 2], frequency = input$frequency)
      
      if (input$method == "quick") {
        # Quick & Dirty Seasonal Factor calculation
        decomp_res <- decompose(ts_data, type = "additive")
        list(plot = function() plot(decomp_res), text = capture.output(print(decomp_res$figure)))
      } else if (input$method == "decomp") {
        # Seasonal Decomposition with Moving Averages
        decomp_res <- decompose(ts_data)
        list(plot = function() plot(decomp_res), text = capture.output(print(decomp_res)))
      } else {
        # Winters' Method (Holt-Winters)
        hw_res <- HoltWinters(ts_data)
        list(plot = function() plot(hw_res), text = capture.output(print(hw_res)))
      }
    })
    
    output$plot <- renderPlot({
      res <- results()
      res$plot()
    })
    
    output$summary <- renderPrint({
      res <- results()
      cat(res$text, sep = "\n")
    })
  })
}