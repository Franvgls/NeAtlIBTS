% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gearPlotHH.wgdp.R
\name{gearPlotHH.wgdp}
\alias{gearPlotHH.wgdp}
\title{Function gearPlotHH.wgdp plots Wing Spread vs. Depth}
\usage{
gearPlotHH.wgdp(
  Survey,
  years,
  quarter,
  c.inta = 0.8,
  c.intb = 0.3,
  es = FALSE,
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

\item{c.inta:}{the confidence interval to be used in the confint function for long sweeps and for sweeps if there is only one length}

\item{c.intb:}{the confidence interval to be used in the confint function for short sweeps if there are two}

\item{es:}{if TRUE all titles legends... are in Spanish, if FALSE in English}

\item{col1:}{color for the symbols and lines for the whole set if only one set of sweeps are used, and for the data from the long set of sweeps.}

\item{col2:}{color for the symbols and lines for the data from the short sweeps in case there are two.}

\item{getICES:}{Should the data be downloaded from DATRAS? If T, default, the data are taken from DATRAS through the icesDatras package.}

\item{pF:}{takes out the points and leaves only the lines in the graphs}

\item{ti:}{if F title will not be included automatically and can be addedd later}
}
\value{
Produces a graph with WingSpread vs. Depth. it also includes information on the ship, the time series used, the models and parameters estimated.
}
\description{
Produces a WingSpread vs. DoorSpread plot and a model with nls R function. Data are taken directly from DATRAS using function getDATRAS from library(icesDatras)
It only produces plots for surveys with HH files uploaded in DATRAS
If there are two different sweeps in the data, produces a model for each sweep length.
}
\details{
Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
}
\examples{
gearPlotHH.wgdp("SWC-IBTS",c(2014:2016),1,.6,.2,col1="darkblue",col2="steelblue")
gearPlotHH.wgdp("SWC-IBTS",c(2013:2016),4,.6,.2)
gearPlotHH.wgdp("ROCKALL",c(2013:2016),3,.8)
gearPlotHH.wgdp("NIGFS",c(2005:2016),1,.2)
gearPlotHH.wgdp("IE-IGFS",c(2005:2016),4,.9,.8,pF=F)
gearPlotHH.wgdp("SP-PORC",c(2015:2016),3,.5)
gearPlotHH.wgdp("FR-CGFS",c(1998:2016),4,.8)
gearPlotHH.wgdp("EVHOE",c(1997:2016),4,.9)
gearPlotHH.wgdp("SP-NORTH",c(2014:2016),4,.3,col1="darkblue")
gearPlotHH.wgdp("SP-ARSA",c(2014:2016),1,.2)
gearPlotHH.wgdp("SP-ARSA",c(2014:2016),4,.5)
gearPlotHH.wgdp(damb,c(2014:2016),4,pF=F,getICES=F)
}
