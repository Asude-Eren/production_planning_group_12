library(shiny)

for (f in list.files("R", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)) {
  source(f, local = TRUE)
}

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
                 tabPanel("Moving Average", moving_average_ui("moving_average")),
                 tabPanel("Moving Average (New)", moving_average_new_ui("moving_average_new")),
                 tabPanel("Weighted Moving Average", weighted_moving_average_ui("weighted_moving_average")),
                 tabPanel("Simple Exponential Smoothing", SimpleExponential_ui("ses_mod"))
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  moving_average_new_server("moving_average_new")
  weighted_moving_average_server("weighted_moving_average")
  SimpleExponential_server("ses_mod")
}

shinyApp(ui, server)
