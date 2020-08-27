#' Generate IRT plot(s)
#'
#' The supported IRT plots include "Information", "CSEM", "Expected_Percent_Score",
#' "Expected_Raw_Score".
#'
#' @param data a data frame contains IRT parameters and categorical attributes.
#' @param IRT_fun one or multiple IRT plot names, which could be "Information", "CSEM",
#' "Expected_Percent_Score", or/and "Expected_Raw_Score".
#' @param theta
#' A vector of ability values
#'
#' @param IRTParCols
#' A vector of colnames that have the columns required by the IRT model of choice.
#'
#' @param IRTModelCol
#' A column that specifies the IRT model for each item.
#' This can also be a single value, which will be recycled.
#'
#' @param IRTScale
#' A vector of scaling parameters used in the IRT models.
#' This can also be a single value, which will be recycled.
#'
#' @param groupByCol a column that groups the data for IRT functions and assigns plot colors
#'
#' @param colorByCol a column that uses for colors. To use colorByCol, the groupByCol must not be
#' NULL and the number of unique values in groupByCol must be more than the number of unique values
#' in colorByCol.
#'
#' @import dplyr purrr plotly RColorBrewer myFormAssembler
#' @export

gen_IRT_plot = function (data, IRT_fun = "Information", theta = c(-4,4), IRTParCols, IRTModelCol, IRTScale = 1.0,
                         groupByCol = NULL, colorByCol = NULL, showLegend = T, opacity = 0.6) {

  require(dplyr)
  require(purrr)
  require(plotly)
  require(myFormAssembler)
  require(RColorBrewer)

  keepCols <- purrr::compact(c(IRTParCols, IRTModelCol, groupByCol))

  if (!all(keepCols %in% names(data))) {
    print(paste0("Missing columns: ", paste0(setdiff(keepCols, names(data)), collapse = ",")));
    return()}

  # if groupByCol is NULL, add a fake group column named "group_" with "group1"

  if (!is.null(groupByCol))
    forms <- data %>% select(all_of(keepCols)) %>% group_by(!!as.name(groupByCol)) else
  {
    forms <- data %>% select(all_of(keepCols)) %>%
      mutate(group_ = "group1") %>% group_by(group_)
    groupByCol = "group_"
    showLegend = F
  }

  thetas = seq(theta[1], theta[2], by = 0.1)

  # use group_kep as id and for any NA group, it is named "NA"

  groupNames = group_keys(forms) %>% unlist()
  if (any(is.na(groupNames))) groupNames[which(is.na(groupNames))] = "NA"

  val0 = list()
  for (IRT_funName in IRT_fun) {

    theFun = switch(IRT_funName,
                    Information = test_fisher,
                    Avg_information = avg_fisher,
                    CSEM = CSEM,
                    'Expected Percent Score' = test_expected_percent_score,
                    'Expected Raw Score' = test_expected_raw_score)

    val0[[IRT_funName]] <- forms %>%
      group_map(~ {
        tibble(theta = thetas,
               val = theFun(theta = thetas,
                            IRT.par   = .x[, IRTParCols],
                            IRT.model = .x[[IRTModelCol]],
                            IRT.scale = IRTScale))
      }) %>%
      set_names(groupNames) %>%
      dplyr::bind_rows(.id = "group")
  }

  # color map

  use_color_map = F
  color_map = list()
  if (!is.null(groupByCol) && !is.null(colorByCol) && colorByCol %in% names(data)) {
    n_color = length(unique(data[[colorByCol]]))
    n_group = length(unique(data[[groupByCol]]))
    if (n_group > n_color) {
      # get colors
      color_tbl = tibble(
        color_by = unique(data[[colorByCol]]),
        # color_num = ifelse(n_color > 8, colorRampPalette(brewer.pal(3, "Dark2"))(n_color),
        #                    brewer.pal(8, "Dark2")[1:n_color])
        color_num = colorRampPalette(brewer.pal(3, "Dark2"))(n_color)
      ) %>% set_names(colorByCol, "color_num")
      color_tbl <- data %>% select(groupByCol, colorByCol) %>% distinct() %>%
        left_join(y = color_tbl)
      color_map = color_tbl$color_num
      names(color_map) = color_tbl[[groupByCol]]
      use_color_map = T
    }
  }

  return(imap(val0, ~{
    if (use_color_map)
      p <- plot_ly(data = .x, x = ~theta, y = ~val, color = ~group, type = 'scatter',
                   mode = 'lines', opacity = opacity, line = list(color = color_map[.$group])) else
      p <- plot_ly(data = .x, x = ~theta, y = ~val, color = ~group, type = 'scatter',
                   mode = 'lines', opacity = opacity)
    if (.y == "Expected Percent Score")
      p <- p %>% layout(xaxis = list(title = "Theta"),
                        yaxis = list(title = .y, range = c(0,1.0)),
                        showlegend = showLegend,
                        annotations = groupByCol) else
      p <- p %>% layout(xaxis = list(title = "Theta"),
                        yaxis = list(title = .y),
                        showlegend = showLegend,
                        annotations = groupByCol)
  }))

}

#' Generate IRT plots given forms
#'
#' The supported IRT plots include "Information", "CSEM", "Expected_Percent_Score",
#' "Expected_Raw_Score".
#'
#' @param metadata a data frame contains IRT parameters and categorical attributes
#' @param formInf form information in tibble that must contain columns named form_id and item_id
#' @param joinBy join for formInf and metadata
#' @param IRT_fun one or multiple IRT plot names, which could be "Information", "CSEM",
#' "Expected_Percent_Score", or/and "Expected_Raw_Score".
#' @param theta
#' A vector of ability values
#'
#' @param IRTParCols
#' A vector of colnames that have the columns required by the IRT model of choice.
#'
#' @param IRTModelCol
#' A column that specifies the IRT model for each item.
#' This can also be a single value, which will be recycled.
#'
#' @param IRTScale
#' A vector of scaling parameters used in the IRT models.
#' This can also be a single value, which will be recycled.
#'
#' @import dplyr purrr plotly myFormAssembler
#' @export


gen_formIRTPlot = function (metadata, formInf, joinBy, IRT_fun = "Information", theta = c(-4,4),
                            IRTParCols, IRTModelCol, IRTScale = 1.0, showLegend = T, opacity = 0.6,
                            plotType = c("curve", "")) {

  require(dplyr)
  require(purrr)
  require(plotly)
  require(myFormAssembler)

  keepCols <- purrr::compact(c(joinBy, IRTParCols, IRTModelCol))

  if (!all(keepCols %in% names(metadata))) {
    print(paste0("Missing columns: ", paste0(setdiff(keepCols, names(data)), collapse = ",")));
    return()}

  thetas = seq(theta[1], theta[2], by = 0.1)

  forms <- formInf %>% left_join(metadata %>% select(all_of(keepCols))) %>% group_by(form_id)
  groupNames = group_keys(forms) %>% unlist()

  val0 = list()
  for (IRT_funName in IRT_fun) {

    print(IRT_funName)

    theFun = switch(IRT_funName,
                    Information = test_fisher,
                    CSEM = CSEM,
                    'Expected Percent Score' = test_expected_percent_score,
                    'Expected Raw Score' = test_expected_raw_score)

    val0[[IRT_funName]] <- forms %>%
      group_map(~ {
        tibble(theta = thetas,
               val = theFun(theta = thetas,
                            IRT.par   = .x[, IRTParCols],
                            IRT.model = .x[[IRTModelCol]],
                            IRT.scale = IRTScale))
      }) %>%
      set_names(groupNames) %>%
      dplyr::bind_rows(.id = "group")
  }

  return(imap(val0, ~{

    p <- plot_ly(data = .x, x = ~theta, y = ~val, color = ~group, type = 'scatter',
                 mode = 'lines', opacity = opacity)
    if (.y == "Expected Percent Score")
      p <- p %>% layout(xaxis = list(title = "Theta"),
                        yaxis = list(title = .y, range = c(0,1.0)),
                        showlegend = showLegend
                        ) else
                          p <- p %>% layout(xaxis = list(title = "Theta"),
                                            yaxis = list(title = .y),
                                            showlegend = showLegend)
  }))

}


