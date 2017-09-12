#' Function getHHdata.ts to download from DATRAS HH records getting several years in a row, uses getHHdata from icesDatras that only gets one year
#' @param Survey: Surveys available in DATRAS see details
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param  quarter: the quarter of the survey to be ploted
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @examples getHHdata.ts("SWC-IBTS",c(2014:2016),1)
#' @export
   getHHdata.ts<-function(Survey,years,quarter){
      listSurveys<-c("SWC-IBTS","ROCKALL","NIGFS","IE-IGFS","SP-PORC","FR-CGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")
      if (!Survey %in% listSurveys) { stop(paste("Survey",Survey,"does not exist")) }       
      dumbHH<-icesDatras::getHHdata(Survey,years[1],quarter)
      if (length(years)>1) {
        for (i in years[2:length(years)]) {
          dumbHH<-rbind(dumbHH,icesDatras::getHHdata(Survey,c(i),quarter))
        }
        }
          dumbHH
          }
