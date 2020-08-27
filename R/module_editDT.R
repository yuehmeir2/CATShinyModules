#' editDT module UI function
#'
#' @export
#'
editDTInput <- function(id, label = "") {

  ns <- NS(id)
  uiOutput(ns("DT_table_UI"))
}

#' editDT module server function
#'
#' @export
#'
editDT <- function(input, output, session, data_sets, ...) {

  cat("### ", format(Sys.time(), "%X"), " module editDT\n")
  require(shinydashboard)
  require(DT)

  data2 <- reactiveValues(data = data_sets )

  observeEvent(data2, {

    cat("### ", format(Sys.time(), "%X"), " observeEvent(data2\n")

    nsNames = paste0("dataset_", 1:(length(data2$data)))
    print("in observeEvent")
    print(nsNames)
    lapply(seq_len(length(data2$data)), function(i) {
      output[[nsNames[i]]] <<- renderDT(data_sets[[i]], selection = 'none', server = FALSE, editable = TRUE, ...) })

    gen_observers()
  }, priority = 0)

  output$DT_table_UI <- renderUI({

    cat("### ", format(Sys.time(), "%X"), " output$DT_table_UI\n")

    # must use data_sets here; if using data2, the table will be reactive to any change and reloaded
    ns <- session$ns
    validate(need(data_sets, message = FALSE))
    nTabs = length(data_sets)
    shNames = names(data_sets)
    nsNames = paste0("dataset_", 1:nTabs)

    # output$table <- DT::renderDataTable({df}, options = list(scrollX = TRUE))

    myTabs = lapply(seq_len(nTabs), function(i) {
      tabPanel(title = shNames[i],
               # DT::renderDT(dataTableOutput(ns(nsNames[i])), options = list(scrollX = TRUE))
               DT::DTOutput(ns(nsNames[i]))
      )
    })

    do.call(tabBox, c(myTabs, width = 12, id = ns('tabBox1')))
  })

  gen_observers <- eventReactive(data2, {

    cat("### ", format(Sys.time(), "%X"), " gen_observers() <- eventReactive(data2, ...\n")

    nTabs = length(data2$data)
    shNames = names(data2$data)
    nsNames = paste0("dataset_", 1:nTabs)

    res <- lapply(1:nTabs, function(k) {

      this = paste0("dataset_", k, "_cell_edit")
      observeEvent( input[[this]], {

          info = input[[this]]
          str(info)
          i = info$row
          j = info$col
          v = info$value
          temp = as.data.frame(data2$data[[k]])
          v2 = DT::coerceValue(v, temp[i, j])
          print(v2)
          if (!is.null(v2)) data2$data[[k]][i, j] <- v2
      })
    })

    res
  })

  # return(data2$data)
  return(reactive(data2$data))
}
