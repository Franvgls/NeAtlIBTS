% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gearPlotHHNS.nodp.R
\name{gearPlotHHNS.nodp}
\alias{gearPlotHHNS.nodp}
\title{Function gearPlotHH.nodp to plot net opening vs. depth including the NS-IBTS and producing plots by country}
\usage{
gearPlotHHNS.nodp(
  Survey = "NS-IBTS",
  years,
  quarter,
  country,
  c.inta = 0.8,
  c.intb = 0.3,
  col1 = "darkblue",
  col2 = "steelblue2",
  getICES = TRUE,
  pF = TRUE,
  ti = TRUE
)
}
\arguments{
\item{Survey:}{either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter}

\item{years:}{years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.}

\item{quarter:}{the quarter of the survey to be ploted}

\item{country:}{The country chosen to be plotted (checks if it's available in the HH file)}

\item{c.inta:}{the confidence interval to be used in the confint function for all data if only one sweep length, and for the short sweeps in case there are two}

\item{c.intb:}{the confidence interval to be used in the confint function for the long set of sweeps.}

\item{col1:}{color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.}

\item{col2:}{color for the symbols and lines for the data from the short sweeps in case there are two.}

\item{getICES:}{Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.}

\item{pF:}{takes out the points and leaves only the lines in the graphs}

\item{ti:}{if T includes autmoatically the title, F leaves it blank an can be added later.}
}
\value{
Produces Net Vertical opening vs. Depth plot it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
}
\description{
Produces Net Vertical opening vs. Depth plot and a model with nls R function. Data are taken directly from DATRAS getting all the data from DATRAS using function getHHdata from library(icesDatras)
it only produces plots for surveys with HH files uploaded in DATRAS
If there are two different sweeps in the data, produces a model for each sweep length.
}
\details{
Surveys available in DATRAS: i.e. NS-IBTS,SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
}
\examples{
gearPlotHHNS.nodp(Survey="NS-IBTS",years=c(2014:2016),quarter=3,country="ENG")
}
