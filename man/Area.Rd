\name{Area}
\alias{Area}
\docType{data}
\title{ICES statrec/areas}
\description{ICES statistical rectangles (StatRecs) have been in use since the 1970's, and were first officially referenced in CM document 1977/Gen:3 document 'ICES Statistical Rectangle Coding System'.

ICES statistical rectangles provide a grid covering the area between 36°N and 85°30'N and 44°W and 68°30'E.}
\usage{data("Area")}
\format{
  A data frame with 6256 observations on the following 6 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{ICESNAME}}{a character vector: name of the ICES rectangle}
    \item{\code{Area}}{ICES area the rectangle belongs to with only main areas and subareas}
    \item{\code{AREA_KM2}}{a numeric vector: area of the ICES rectangle in square km}
    \item{\code{stat_x}}{a numeric vector: ICES Rectangle lat central point}
    \item{\code{stat_y}}{a numeric vector: ICES Rectangle long central point}
    \item{\code{Area_27}}{full area definition according to ICES GIS map 20170124}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://www.ices.dk/marine-data/maps/Pages/ICES-statistical-rectangles.aspx
}
\references{

}
\examples{
str(Area)
IBTSNeAtl_map(nl=55,sl=49,out="def",dens=0,leg=F,ICESrect=T)
text(stat_y~stat_x,Area,ICESNAME,cex=.7,font=2)
text(stat_y~stat_x,Area,Area,cex=.6,col=2,pos=1)
}
\keyword{datasets}
