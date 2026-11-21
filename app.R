library(shiny)
library(bslib)
library(forecast)

for (f in list.files("R", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)) {
  source(f, local = TRUE)
}

navy  <- "#17284B"
page  <- "#EEF1F6"
edge  <- "#C9D1E0"
focus <- "#2F6FED"

app_theme <- bs_theme(
  version = 5,
  bg = page, fg = "#1B2433",
  primary = navy, secondary = "#5B6780",
  base_font    = font_google("IBM Plex Sans"),
  heading_font = font_google("IBM Plex Serif", wght = c(500, 600)),
  "border-radius" = "2px"
)

app_css <- sprintf("
  /* Top bar: solid navy, active item becomes a folder tab */
  .navbar { background: %1$s !important; box-shadow: none;
            padding: 8px 16px 0 16px; }
  .navbar > .container-fluid { flex-wrap: wrap; align-items: flex-end; }
  .navbar-brand { font-family: 'IBM Plex Serif', serif; font-weight: 600;
                  color: #FFFFFF !important; padding-bottom: 8px; margin-right: 28px; }

  /* Menu always in one row, never collapsed */
  .navbar-toggler { display: none !important; }
  .navbar .navbar-nav { flex-direction: row !important; flex-wrap: wrap;
                        align-items: flex-end; }
  .navbar .navbar-nav .dropdown-menu { position: absolute !important; }

  .navbar .nav-link { color: rgba(255,255,255,.85) !important; font-weight: 500;
                      padding: .6rem 1.2rem; border-radius: 3px 3px 0 0; }
  .navbar .nav-link:hover { background: rgba(255,255,255,.12); color: #FFFFFF !important; }
  .navbar .nav-link.active,
  .navbar .nav-link.show { background: %2$s; color: %1$s !important; font-weight: 600; }

  .dropdown-menu { background: #FFFFFF; border: 1px solid %3$s; border-top: 3px solid %1$s;
                   border-radius: 0 0 3px 3px; box-shadow: none; padding: 6px 0;
                   min-width: 270px; margin-top: 0; }
  .dropdown-header { color: #5B6780; font-size: .85rem; font-weight: 600; padding-top: 10px; }
  .dropdown-divider { border-color: %3$s; }
  .dropdown-item { color: %1$s; padding: 6px 18px; }
  .dropdown-item:hover { background: %2$s; color: %1$s; }
  .dropdown-item.active { background: %1$s; color: #FFFFFF; }

  /* Working area */
  .container-fluid { padding-top: 24px; padding-bottom: 32px; }
  .navbar > .container-fluid { padding-top: 0; padding-bottom: 0; }
  h1, h2, h3, h4 { color: %1$s; }
  .well { background: #FFFFFF; border: 1px solid %3$s; border-radius: 2px; box-shadow: none; }
  .shiny-plot-output { background: #FFFFFF; border: 1px solid %3$s;
                       border-radius: 2px; padding: 6px; }
  .btn { border-radius: 2px; font-weight: 500; }
  .btn-primary { background: %1$s; border-color: %1$s; }
  .btn-primary:hover { background: #0F1B36; border-color: #0F1B36; }
  .form-control, .selectize-input { border-radius: 2px !important; border-color: #AAB4C8; }
  a:focus-visible, button:focus-visible, .form-control:focus {
    outline: 3px solid %4$s; outline-offset: 2px; }
  table.dataTable thead th, .table thead th { border-bottom: 2px solid %1$s; }
", navy, page, edge, focus)

ui <- navbarPage(
  title = "Production Planning",
  id = "nav",
  theme = app_theme,
  header = tags$head(tags$style(HTML(app_css))),
  collapsible = FALSE,
  
  navbarMenu("Basic Tools",
             tabPanel("Learning Curve", learning_curve_ui("learning_curve"))
  ),
  
  navbarMenu("Forecasting",
             
             "Stationary series",
             tabPanel("Moving Average",
                      moving_average_ui("moving_average")),
             tabPanel("Weighted Moving Average",
                      weighted_moving_average_ui("weighted_moving_average")),
             tabPanel("Simple Exponential Smoothing",
                      simple_exponential_ui("ses_mod")),
             
             "----",
             
             "Trend series",
             tabPanel("Holt's Method",
                      holts_method_ui("holts1")),
             
             "----",
             
             "Seasonal series",
             tabPanel("Seasonal Analysis",
                      seasonal_ui("seasonal")),
             
             "----",
             
             "Box-Jenkins",
             tabPanel("Box-Jenkins",
                      box_jenkins_ui("box_jenkins"))
  )
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  
  moving_average_server("moving_average")
  weighted_moving_average_server("weighted_moving_average")
  simple_exponential_server("ses_mod")
  holts_method_server("holts1")
  seasonal_server("seasonal")
  box_jenkins_server("box_jenkins")
}

shinyApp(ui, server)