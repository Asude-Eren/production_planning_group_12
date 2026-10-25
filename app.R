library(shiny)

for (f in list.files("R", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)) {
  source(f, local = TRUE)
}

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
                 tabPanel("Moving Average", moving_average_ui("moving_average")),
                 tabPanel("Moving Average (New)", moving_average_yeni_ui("moving_average_yeni")),
                 tabPanel("Weighted Moving Average", weighted_moving_average_ui("weighted_moving_average"))
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  moving_average_yeni_server("moving_average_yeni")
  weighted_moving_average_server("weighted_moving_average")
}

shinyApp(ui, server)