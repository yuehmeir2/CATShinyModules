#'
#' @export
as.formula2 = function (var) as.formula(paste0("~ `", var, "`"))

#' A correlation plot
#'
#' @import plotly
#' @export


correlationPlot = function (plotData, xVar, yVar, xVarTitle = "Estimated Theta", yVarTitle = "True Theta",
                            xRange = c(floor(min(c(plotData[[xVar]], plotData[[yVar]]))),
                                       ceiling(max(c(plotData[[xVar]], plotData[[yVar]])))),
                            opacity = .4, addDiagLine = F, useDiffAsColor = T, cutsAt = NULL,
                            addTitle = T, plotName = "") {
  require(plotly)

  titleTxt = str_c("r = ", round(cor(plotData[[xVar]], plotData[[yVar]]), 3))

  if (useDiffAsColor) {

    plotData <- plotData %>% mutate(Difference = abs(.[[xVar]] - .[[yVar]]))

    p <- plotData %>%
      plot_ly(type = "scatter", mode = "markers", x = as.formula2(xVar), y = as.formula2(yVar),
              color = ~ Difference, colors = 'Blues', opacity = opacity, name = plotName)
  } else {
    p <- plotData %>%
      plot_ly(type = "scatter", mode = "markers", x = as.formula2(xVar), y = as.formula2(yVar),
              colors = 'Blues', opacity = opacity, name = plotName)
  }


  # add r = 1 line

  if (addDiagLine)
    p <- p %>%
    add_segments(name = "perfectCorrelationLine",
                 x = xRange[1], xend = xRange[2], opacity=0.9,
                 y = xRange[1], yend = xRange[2], color = I("steelblue"),
                 showlegend = F)

  # add cut score line(s)

  if (length(cutsAt) > 0) {
    for (i in 1:length(cutsAt)) {
      p <- p %>%
        add_segments(x = cutsAt[i], xend = cutsAt[i], opacity=.6,
                     y = xRange[1], yend = xRange[2], color = I('orange'), showlegend = F) %>%
        add_segments(y = cutsAt[i], yend = cutsAt[i], opacity=.6,
                     x = xRange[1], xend = xRange[2], color = I('orange'), showlegend = F)
    }

    breaks = c(-Inf, cutsAt, Inf)
    labels = 1:(length(cutsAt)+1)
    simuleeTrueGroups = cut(plotData[[xVar]], breaks = breaks, labels = labels)
    simuleeFinalGroups = cut(plotData[[yVar]], breaks = breaks, labels = labels)

    sameGroup = simuleeTrueGroups == simuleeFinalGroups

    titleTxt = str_c(titleTxt, " & CA = ", round(sum(sameGroup) / length(sameGroup), 3))
  }

  p <- p %>% layout(title = if (addTitle) titleTxt else "",
                    xaxis = list(title = xVarTitle, range = xRange, zeroline = FALSE),
                    yaxis = list(title = yVarTitle, range = xRange, zeroline = FALSE))

  return (p)
}

