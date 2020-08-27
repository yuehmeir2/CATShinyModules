#' IRTPlot module user interface
#'
#' @param id a namespace id
#'
#' @return a plot_ly output
#' @export
#'

IRTPlot_mod_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      checkboxGroupInput(inputId = ns("IRTFunCheckBox"), label = "IRT Functions: ",
                         choices = c("Information" = "Information", "CSEM" = "CSEM",
                                     "Expected Percent Score" = "Expected Percent Score" ,
                                     "Expected Raw Score" = "Expected Raw Score" ),
                         selected = "Information", inline = T),

      radioButtons(inputId = ns("groupByCheckBox"), label = "Group by: ",
                   choices = character(0), selected = character(0), inline = T),
      uiOutput(ns("dynamicIRTPlotUI"))
    )
  )
}

#' IRTPlot module server-side processing
#'
#' This module produces an IRT plot.
#'
#' @param input,output,session are standard parameters necessary for Shiny modules.
#' @param dataset the item metadata
#' @param IRTDataConf a list of IRT related column information. An example:
#' IRTDataConf = list(IRTParCols = c("IRT_PAR_A", "IRT_PAR_B", "IRT_PAR_C"), IRTModelCol = "IRT_MODEL",
#' IRTScale = 1.0, theta = c(-4,4))
#' @param groupByCols a vector of column names that can be used as group in plotly plot.
#' @export

IRTPlot_mod_server <- function(input, output, session, dataset, IRTDataConf, groupByCols) {

  # data = reactive({
  #
  #   require(purrr)
  #
  #   data0 = dataset()
  #
  #   # find list columns
  #
  #   listCols = imap(data0, ~{if (class(.x) == "list") (.y)}) %>% compact() %>% unlist()
  #
  #   for (theCol in listCols) {
  #
  #     data0 <- suppressMessages(data0 %>% tidyr::unnest_wider(theCol)) %>%
  #       rename_at(vars(starts_with("...")), ~sub("...", theCol, .))
  #   }
  #
  #   data0
  #
  # })


  observeEvent(groupByCols(), {

    require(purrr)

    IRTDataConf = isolate(IRTDataConf())

    # remove columns found in the IRTDataConf() from groupByCols()

    theCols = setdiff(groupByCols(), c(IRTDataConf$IRTParCols, IRTDataConf$IRTModelCol))

    theCols = intersect(names(dataset()), theCols)

    updateRadioButtons(session, inputId = "groupByCheckBox",
                             choices = theCols, inline = T)

  })

  output$dynamicIRTPlotUI <- renderUI({

    # require(myFormAssembler)
    require(purrr)
    require(htmltools)


    req(dataset())

    # Make sure at elast one IRT function is checked

    if (length(input$IRTFunCheckBox) <= 0) return()

    toPlotNames <- reactive({
      input$IRTFunCheckBox
    })

    n_plots <- length(toPlotNames())
    ns <- session$ns

    nsNames = ns(paste0("dynamicIRTPlot_", 1:n_plots))
    # plot_output_list <- lapply(nsNames, function(i) {
    #   htmltools::tagList(plotlyOutput(ns(i)), br())
    # })
    #
    # tagList(plot_output_list)


    imap(nsNames, ~{

        output[[.x]] <<- renderPlotly(

          gen_IRT_plot (data = dataset(), IRT_fun = toPlotNames()[.y], theta = IRTDataConf()$theta,
                        IRTParCols = IRTDataConf()$IRTParCols,
                        IRTModelCol = IRTDataConf()$IRTModelCol,
                        IRTScale = IRTDataConf()$IRTScale,
                        groupByCol = input$groupByCheckBox,
                        showLegend = T)[[1]]

        )})

  })
}

