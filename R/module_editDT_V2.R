#' editDT2 module UI function
#'
#' @export
#'
editDT2Input <- function(id, label = "") {

  ns <- NS(id)
  tagList(
    fluidRow(column(width = 1),
             column(width = 2,
                    actionButton(inputId = ns("add_row_btn"), label = "Add a new row", icon = icon("plus")), br()),
             column(width = 2,
                    actionButton(inputId = ns("del_row_btn"), label = "Delete highlighted row", icon = icon("plus")), br())),
    fluidRow(column(width = 1),
             column(width = 10,
                    uiOutput(ns("DT_table_UI"))))
    )

}

#' editDT2 module server function
#'
#' @export
#'
editDT2 <- function(input, output, session, data_sets, ...) {

  require(shinydashboard)
  require(tidyverse)
  require(DT)

  # reactiveValues

  data2 <- reactiveValues(data = data_sets )

  # create DT tablewith data_sets

  output$DT_table_UI <- renderUI({


    ns <- session$ns
    validate(need(data_sets, message = FALSE))
    nTabs = length(data_sets)
    shNames = names(data_sets)
    nsNames = paste0("dataset_", 1:nTabs)

    myTabs = lapply(seq_len(nTabs), function(i) {
      tabPanel(title = shNames[i], value = nsNames[i],
               DT::DTOutput(ns(nsNames[i]))
      )
    })

    do.call(tabBox, c(myTabs, width = 12, id = ns('tabBox2')))
  })

  # add a new row (based on https://stackoverflow.com/questions/54974143/add-a-new-empty-row-in-shiny-table-with-a-button-click)

  getEmptyTbl <- function (tbl) {
    tbl %>% slice(0) %>% add_row(.before = 0) %>% as_tibble(rownames = NULL)
  }

  # add a new empty row

  observeEvent(input$add_row_btn, {

    req(input$tabBox2)

    activeTabSelected = paste0(input$tabBox2, "_rows_selected")

    selectedRows = input[[activeTabSelected]]

    nsNames = paste0("dataset_", 1:(length(data2$data)))

    dataSetInd = match(input$tabBox2, nsNames)

    if (is.na(dataSetInd)) return()

    if (length(selectedRows) > 0) { # append the new row
      selectedRow = tail(selectedRows, 1)
      data2$data[[dataSetInd]] <- data2$data[[dataSetInd]] %>%
        tibble::add_row(getEmptyTbl(data2$data[[dataSetInd]]), .before = selectedRow)
    } else {
      # insert to the place of the highlighted row
      data2$data[[dataSetInd]] <- data2$data[[dataSetInd]] %>%
      tibble::add_row(getEmptyTbl(data2$data[[dataSetInd]]))
    }

  })

  # delete row(s)

  observeEvent(input$del_row_btn, {

    req(input$tabBox2)

    activeTabSelected = paste0(input$tabBox2, "_rows_selected")
    selectedRows = input[[activeTabSelected]]

    nsNames = paste0("dataset_", 1:(length(data2$data)))
    dataSetInd = match(input$tabBox2, nsNames)

    if (is.na(dataSetInd)) return()

    if (length(selectedRows) > 0) {
      data2$data[[dataSetInd]] <- data2$data[[dataSetInd]] %>% slice(-selectedRows)
    }

  })

  # when editinng

  observeEvent(data2, {

    nsNames = paste0("dataset_", 1:(length(data2$data)))
    lapply(seq_len(length(data2$data)), function(i) {
      output[[nsNames[i]]] <<-
        renderDT(data2$data[[i]], server = T, editable = TRUE, ...) })
    gen_observers()
  }, priority = 0)

  gen_observers <- eventReactive(data2, {

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
          if (!is.null(v2)) data2$data[[k]][i, j] <- v2
      })
    })

    res
  })

  return(reactive(data2$data))
}
