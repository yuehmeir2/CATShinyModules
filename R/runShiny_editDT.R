#' A shiny app for testing the editDT module module
#'
#' @return a shiny app "runShiny_editDT"
#'
#' @examples \dontrun{ runShiny_editDT() }
#'
#' @export

runShiny_editDT <- function() {

  require(shiny)
  require(DT)

  ui <- fluidPage(

    h2("module--edit the table"),
    editDTInput("editTable1"),
    h2("the edit to the first table can be see in the table below"),
    DTOutput("table"),
    h2("the names of the data sets"),
    textOutput("testOutput")
  )

  server <- function(input, output, session) {

    updatedData <- callModule(editDT, "editTable1", data_sets = list(iris = iris, mtcars = mtcars), filter = 'top')

    output$table <- renderDT({

      updatedData()[[1]]

    })

    output$testOutput <- renderText(names(updatedData()))
  }

  shinyApp(ui, server)

}
