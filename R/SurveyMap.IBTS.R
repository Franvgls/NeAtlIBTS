#' Map of hauls (stations) from an IBTS survey taken directly from DATRAS
#'
#' Uses the DATRAS HH data of a survey, year and quarter to plot stations in a map
#' @param Survey Code to plot. Has to be one of those available from DATRAS survey list, and from a year and quarter avalable
#' @param Year year to plot
#' @param Quarter to plot
#' @param ti TRUE includes the Survey Year Quarter title by default, a text includes the txt as title
#' @param leg TRUE includes a legend with the country-colors codes
#' @return Presents the map with the stations and a legend with the countries that have participated in the survey
#' @examples
#' SurveyMap.IBTS("NS-IBTS",2021,3)
#' @family maps
#' @export
SurveyMap.IBTS<-function(Survey,Year,Quarter,ti=TRUE,leg=TRUE){
  if (length(Year)>1) stop("Only one year can be shown in this function")
  hhNS<-icesDatras::getHHdata(Survey,Year,Quarter)
  print(tapply(hhNS$Country,hhNS[,c("Country","Year")],"length"))
  NeAtlIBTS::IBTSNeAtl_map(load=F,NS=T,leg = F,xlims = c(min(hhNS$HaulLong)-1,1+max(max(hhNS$HaulLong),-5)),sl=min(hhNS$HaulLat)-.5,nl=.5+max(hhNS$HaulLat))
  points(HaulLat~HaulLong,hhNS,pch=21,col="black",bg=as.factor(hhNS$Country))
  if (leg) {legend("bottomright","Hauls",pch = 21,bg ="white",pt.bg=as.factor(unique(hhNS$Country)),inset = c(.02))}
  if (is.logical(ti)) {
    if (ti) {tit<-list(paste0(Survey," ",Year," ","Q",Quarter),font=2,cex=1.2)}
    else {tit<-NULL}
  }
  else {
    if(is.list(ti)) tit<-ti
    else tit<-list(ti)
  }
  if (ti) title(tit,line = 1.5)
  }

