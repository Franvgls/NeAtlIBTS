#' Function SpeciesCAperYear Takes a CA file for one survey and produces a table with number of biological samples per year and species
#' @param Survey: Surveys available in DATRAS see details
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param  quarter: the quarter of the survey to be ploted
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @examples SpeciesCAperYear("SP-NORTH",c(2014:2016),4)
#' @export
SpeciesCAperYear<-function(Survey,years,quarter){
  library(SSOAP)
  listSurveys<-c("SWC-IBTS","ROCKALL","NIGFS","IE-IGFS","SP-PORC","FR-CGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")
  if (!Survey %in% listSurveys) { stop(paste("Survey",Survey,"does not exist")) }       
  CA<-getDATRAS("CA",Survey,years,quarter)
  w= SSOAP::processWSDL("http://www.marinespecies.org/aphia.php?p=soap&wsdl=1",verbose = FALSE)
  iface = genSOAPClientInterface(, w)
  species<-iface@functions$getAphiaNameByID(CA$Valid_Aphia[1],('http://www.marinespecies.org/aphia.php?p=soap'))
  aphias<-unique(CA$Valid_Aphia)
  daphias<-data.frame(aphias=aphias,species=NA)
  for (i in c(1:length(aphias))) {
    daphias$species[i]<-iface@functions$getAphiaNameByID(daphias$aphias[i],('http://www.marinespecies.org/aphia.php?p=soap'))
  }
  CA$SpecName<-NA
  for (i in 1:nrow(CA)) {
  CA$SpecName[i]<-daphias[daphias$aphias==CA$Valid_Aphia[i],"species"]
  }
  tapply(CA$Sex,CA[,c("SpecName","Year")],"length")
}
