#' A shiny app for testing IRT plot module
#'
#' @return a shiny app "runShiny_formAnalysis"
#'
#' @examples \dontrun{ runShiny_formAnalysis() }
#' @export

runShiny_formAnalysis <- function() {

  require(shiny)
  require(plotly)
  require(tidyverse)

  ui <- fluidPage(

    formAnalysis_mod_ui("plot1"),
  )

  lp_obj <- readRDS(system.file("rdsdata", "Forms_tidy_1_solved.rds", package = "CATShinyModules"))
  lp_obj$output = NULL
  timeout = lp_obj$options$timeout
  solver = lp_obj$options$solver
  gap = lp_obj$options$gap
  system.time(
  lp_obj <- to_solve_cont(lp_obj, maxOverlapRate = 0.5, timeout = timeout,
                         solver = solver, maxIter = 100, gap = gap))

  items = lp_obj$items
  forms = lp_obj$formList %>% bind_rows(.id = "form_id") %>% select(form_ind = "form_id", item_id)
  colMap = lp_obj$input_obj$alias

  server <- function(input, output, session) {

    callModule(formAnalysis_mod_server, "plot1",
               items = reactive(items),
               forms = reactive(forms),
               colMap = reactive(colMap))
  }

  shinyApp(ui, server)

}


