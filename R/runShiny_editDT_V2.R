#' A shiny app for testing the editDT2 module module
#'
#' @return a shiny app "runShiny_editDT2"
#'
#' @examples \dontrun{ runShiny_editDT2() }
#'
#' @export

runShiny_editDT2 <- function() {

  require(shiny)
  require(DT)

  ui <- fluidPage(

    h2("module--edit the table"),
    editDT2Input("editTable2"),
    h2("the edit to the first table can be see in the table below"),
    DTOutput("table"),
    h2("the names of the data sets"),
    textOutput("testOutput")
  )

  server <- function(input, output, session) {

    updatedData <- callModule(editDT2, "editTable2", data_sets = list(iris = iris, mtcars = mtcars), filter = 'top')

    output$table <- renderDT({

      updatedData()[[1]]

    })

    output$testOutput <- renderText(names(updatedData()))
  }

  shinyApp(ui, server)

}
