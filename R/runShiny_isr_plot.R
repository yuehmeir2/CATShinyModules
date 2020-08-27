#' A shiny app for testing ISR plot module
#'
#' @return a shiny app "runShiny_isr_plot"
#'
#' @examples \dontrun{ runShiny_isr_plot() }
#' @export

runShiny_isr_plot <- function() {

  require(shiny)
  require(plotly)
  require(DT)

  ui <- fluidPage(

    h2("Hey"),
    isr_plot_ui("plots"),
    isr_plot_ui("plots2")
  )

  server <- function(input, output, session) {

    callModule(isr_plot_server,
                      "plots",
                      dataset = iris,
                      # n_plots = 1,
                      plot1vars = "Sepal.Length",
                      plot2vars = "Sepal.Width")
    callModule(isr_plot_server,
               "plots2",
               dataset = mtcars,
               # n_plots = 1,
               plot1vars = "mpg",
               plot2vars = "cyl")
  }

  shinyApp(ui, server)

}


