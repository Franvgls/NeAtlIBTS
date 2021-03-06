% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gearPlotsHH.R
\name{gearPlotsHH}
\alias{gearPlotsHH}
\title{Function gearPlotsHH to plot gear parameters and behaviour}
\usage{
gearPlotsHH(
  Survey,
  years,
  quarter = T,
  c.inta = 0.5,
  c.intb = 0.5,
  col1 = "darkblue",
  col2 = "steelblue2",
  getICES = T,
  pF = T
)
}
\arguments{
\item{Survey:}{either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter}

\item{years:}{years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.}

\item{quarter:}{the quarter of the survey to be ploted}

\item{c.inta:}{the confidence interval to be used in the confint function for long sweeps and for sweeps if there is only one length}

\item{c.intb:}{the confidence interval to be used in the confint function for short sweeps if there are two}

\item{col1:}{color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.}

\item{col2:}{color for the symbols and lines for the data from the short sweeps in case there are two.}

\item{getICES:}{Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.}

\item{pF:}{takes out the points and leaves only the lines in the graphs}
}
\value{
Produces a set of 4 or 2 graphs: Warp shot vs. depth, DoorSpread vs. WingSpread, WingSpread vs. Depth, DoorSpread vs. Depth, Vertical Opening vs Depth  it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
}
\description{
Data are taken directly from DATRAS getting all the data from DATRAS using function getHHdata from library(icesDatras)
it only produces plots for surveys with HH files uploaded in DATRAS
If there are DoorSpread and WingSpread values in HH records produces four graphs, if only DoorSpread values are available produces only two graphs
}
\details{
Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
}
\examples{
gearPlotsHH("SWC-IBTS",c(2014:2016),1)
gearPlotsHH("SWC-IBTS",c(2013:2016),4)
gearPlotsHH("ROCKALL",c(2013:2016),3)
gearPlotsHH("IE-IGFS",c(2011:2016),4)
gearPlotsHH("SP-PORC",c(2003:2016),3)
gearPlotsHH("FR-CGFS",c(2015:2016),4)
gearPlotsHH("EVHOE",c(1997:2015),4)
gearPlotsHH("SP-NORTH",c(2014:2016),4)
gearPlotsHH("SP-ARSA",c(2014:2016),1)
gearPlotsHH("SP-ARSA",c(2014:2016),4)
gearPlotsHH(damb,c(2014:2016),4,getICES=F)
}
