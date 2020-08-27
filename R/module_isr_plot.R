#' ISR plot module UI function
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export

isr_plot_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        plotlyOutput(ns("plot1"))
      ),
      column(
        width = 6,
        plotlyOutput(ns("plot2"))
      )
    ),
    fluidRow(
      numericInput(ns("nPlotSel"), "Number of plots", value = 2, min = 0, max = 5),
      uiOutput(ns("dynamicPlotUIs"))
    )
  )
}

#' ISR plot module server side function
#'
#' This module produces a scatterplot with the sales price against a variable selected by the user.
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param dataset data frame (non-reactive) with variables necessary for scatterplot
#' @param plot1_vars list containing reactive x-variable name (called `xvar`) and y-variable name (called `yvar`) for plot 1
#' @param plot2_vars list containing reactive x-variable name (called `xvar`) and y-variable name (called `yvar`) for plot 2
#'
#' @import plotly
#' @export

isr_plot_server <- function(input,
                                   output,
                                   session,
                                   dataset,
                                   # n_plots,
                                   plot1vars,
                                   plot2vars) {
  require(plotly)

  plot1_obj <- reactive({
    p <- plot_ly(data = dataset,
                 x = as.formula(paste0("~", plot1vars)),
                 y = as.formula(paste0("~", plot2vars)))
    return(p)
  })

  plot2_obj <- reactive({
    p <- plot_ly(data = dataset,
                 x = as.formula(paste0("~", plot1vars)),
                 y = as.formula(paste0("~", plot2vars)))
    return(p)
  })


  output$plot1 <- renderPlotly({
    plot1_obj()
  })

  output$plot2 <- renderPlotly({
    plot2_obj()
  })

  # dynamic plot UIs

  output$dynamicPlotUIs <- renderUI({

    # if (n_plots <= 0)
    #   return()

    req(input$nPlotSel > 0)

    n_plots = input$nPlotSel
    ns <- session$ns

    nsNames = paste0("dynamic_p_", 1:n_plots)
    plot_output_list <- lapply(nsNames, function(i) {
      plotlyOutput(ns(i))
    })

    do.call(tagList, plot_output_list)

    # ns <- session$ns
    # validate(need(data_sets, message = FALSE))
    # nTabs = length(data_sets)
    # shNames = names(data_sets)
    # nsNames = paste0("dataset_", 1:nTabs)
    #
    # myTabs = lapply(seq_len(nTabs), function(i) {
    #   tabPanel(title = shNames[i],
    #            # DT::renderDT(dataTableOutput(ns(nsNames[i])), options = list(scrollX = TRUE))
    #            DT::DTOutput(ns(nsNames[i]))
    #   )
    # })
    #
    # do.call(tabBox, c(myTabs, width = 12, id = ns('tabBox1')))

  })

  # data2 <- reactiveValues(data = dataset )

  observeEvent(input$nPlotSel, {

    cat("### ", format(Sys.time(), "%X"), "  observeEvent(input$nPlotSel\n")

    req(input$nPlotSel > 0)
    n_plots = input$nPlotSel

    lapply(paste0("dynamic_p_", 1:n_plots), function(i) {
      output[[i]] <<- renderPlotly(
        plot_ly(data = dataset,
                x = as.formula(paste0("~", plot1vars)),
                y = as.formula(paste0("~", plot2vars)))

        )})
  })

}
