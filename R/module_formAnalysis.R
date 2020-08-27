#' formAnalysis module user interface
#'
#' @param id a namespace id
#'
#' @return a tagList output
#' @export
#'

formAnalysis_mod_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      checkboxGroupInput(inputId = ns("IRTFunCheckBox"), label = "IRT Functions: ",
                         choices = c("Information" = "Information", "CSEM" = "CSEM",
                                     "Expected Percent Score" = "Expected Percent Score" ,
                                     "Expected Raw Score" = "Expected Raw Score" ),
                         selected = "Information", inline = T),

      # radioButtons(inputId = ns("groupByCheckBox"), label = "Group by: ",
      #              choices = character(0), selected = character(0), inline = T),
      # uiOutput(ns("dynamicformAnalysisUI")),
      uiOutput(ns("formIRTUI"))
    )
  )
}

#' formAnalysis module server-side processing
#'
#' @param input,output,session are standard parameters necessary for Shiny modules.
#' @param items the item pool
#' @param forms the forms
#' @param colMap the col
#'  a list of IRT related column information. An example:
#' IRTDataConf = list(IRTParCols = c("IRT_PAR_A", "IRT_PAR_B", "IRT_PAR_C"), IRTModelCol = "IRT_MODEL",
#' IRTScale = 1.0, theta = c(-4,4))
#' @export

formAnalysis_mod_server <- function(input, output, session, items, forms, colMap) {

  # formIRTUI
  output$formIRTUI <- renderUI({

    require(purrr)
    require(htmltools)

    req(items())
    req(forms())

    # Make sure at elast one IRT function is checked

    if (length(input$IRTFunCheckBox) <= 0) return()

    toPlotNames <- reactive({
      input$IRTFunCheckBox
    })

    n_plots <- length(toPlotNames())
    ns <- session$ns

    nsNames = ns(paste0("formIRT_", 1:n_plots))

    imap(nsNames, ~{

      formInf = tibble(form_id = str_c("F", forms()$form_ind),
                       item_id = forms()$item_id)

      IRTParCols = colMap() %>% filter(grepl("irt_par_", .$attribute)) %>% drop_na() %>% pull("alias")
      IRTModelCol = colMap() %>% filter(attribute == "irt_model") %>% pull("alias")
      output[[.x]] <<- renderPlotly(

        gen_formIRTPlot (metadata = items(),
                         formInf = formInf,
                         joinBy = c("item_id" = colMap()$alias[which(colMap()$attribute == "item_id")]),
                         IRT_fun = toPlotNames()[.y],
                         IRTParCols = IRTParCols,
                         IRTModelCol = IRTModelCol,
                         # IRTParCols = IRTDataConf()$IRTParCols,
                         # IRTModelCol = IRTDataConf()$IRTModelCol,
                         # IRTScale = IRTDataConf()$IRTScale,
                         # theta = IRTDataConf()$theta
                        )[[1]]

      )})
  })
}

