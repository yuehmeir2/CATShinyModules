% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_IRTPlot.R
\name{IRTPlot_mod_server}
\alias{IRTPlot_mod_server}
\title{IRTPlot module server-side processing}
\usage{
IRTPlot_mod_server(input, output, session, dataset, IRTDataConf, groupByCols)
}
\arguments{
\item{input, output, session}{are standard parameters necessary for Shiny modules.}

\item{dataset}{the item metadata}

\item{IRTDataConf}{a list of IRT related column information. An example:
IRTDataConf = list(IRTParCols = c("IRT_PAR_A", "IRT_PAR_B", "IRT_PAR_C"), IRTModelCol = "IRT_MODEL",
IRTScale = 1.0, theta = c(-4,4))}

\item{groupByCols}{a vector of column names that can be used as group in plotly plot.}
}
\description{
This module produces an IRT plot.
}
