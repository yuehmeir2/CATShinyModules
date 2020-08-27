#' A shiny app for testing IRT plot module
#'
#' @return a shiny app "runShiny_IRTPlot"
#'
#' @examples \dontrun{ runShiny_IRTPlot() }
#' @export

runShiny_IRTPlot <- function() {

  require(shiny)
  require(plotly)
  require(readr)

  ui <- fluidPage(

    IRTPlot_mod_ui("plot1"),
    # IRTPlot_mod_ui("plot2")
  )


  csvFile = "../NCME_2019_workshop/data/pool_3PL.csv"
  itemPool = read_csv(file = csvFile)
  IRTParCols = c("IRT_PAR_A", "IRT_PAR_B", "IRT_PAR_C")
  IRTModelCol = "IRT_MODEL"
  # groupByCol = "STANDARD"

  # itemPool = readr::read_rds("../CATSimulator/inst/data/passage/pool.rds")
  # IRTParCols = c("PAR_1", "PAR_2", "PAR_3")
  # IRTModelCol = "MODEL"
  # groupByCol = "PSG_ID"

  server <- function(input, output, session) {

    callModule(IRTPlot_mod_server, "plot1", #plotInOne = FALSE,
               dataset = reactive(itemPool),
               IRTDataConf = reactive(list(IRTParCols = IRTParCols,
                                  IRTModelCol = IRTModelCol,
                                  IRTScale = 1.0,
                                  theta = c(-4,4))),
               # groupByCol = groupByCol,
               groupByCols = reactive(c("TASK_TYPE", "STANDARD"))
               )

  }

  shinyApp(ui, server)

}


