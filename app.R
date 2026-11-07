library(shiny)

for (f in list.files("R", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)) {
  source(f, local = TRUE)
}

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
                 tabPanel("Moving Average", moving_average_ui("moving_average")),
                 tabPanel("Moving Average (New)", moving_average_new_ui("moving_average_new")),
                 tabPanel("Weighted Moving Average", weighted_moving_average_ui("weighted_moving_average")),
                 tabPanel("Simple Exponential Smoothing", simple_exponential_ui("ses_mod")),
                 tabPanel("Box-Jenkins", box_jenkins_ui("box_jenkins"))
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  moving_average_new_server("moving_average_new")
  weighted_moving_average_server("weighted_moving_average")
  simple_exponential_server("ses_mod")
  box_jenkins_server("box_jenkins")
}

shinyApp(ui, server)
