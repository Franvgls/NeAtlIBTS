#' Function getDatras2 modification of function getDATRAS to give the possibility of not getting the list of files downloaded
#' @param Survey: either the Survey to be downloaded from DATRAS (see details), or a data frame with the HH information with  the DATRAS HH format  and the years and quarter selected in years and quarter 
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param quarter: the quarter of the survey to be ploted
#' @details Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @return Downloads from DATRAS the HH files from the survey and the years selected.
#' @examples getDatras2("HH","SP-NORTH",c(2014:2016),4,listData=FALSE)
#' @export
getDatras2<-function (record = "HH", survey, years, quarters,listData=FALSE) 
{
  if (!record %in% c("HH", "HL", "CA")) {
    message("Please specify record type:", "\n\t\tHH (haul data)", 
            "\n\t\tHL (length-based data)", "\n\t\tCA (age-based data)")
    return(FALSE)
  }
  if (!icesDatras::checkSurveyOK(survey)) 
    return(FALSE)
  available_years <- icesDatras::getSurveyYearList(survey)
  available_years_req <- intersect(years, available_years)
  if (length(available_years_req) == 0) {
    message("Supplied years (", paste(years, collapse = ", "), 
            ") are not available.\n  Available options are:\n", 
            paste(capture.output(print(available_years)), collapse = "\n"))
    return(FALSE)
  }
  else if (length(available_years_req) < length(years)) {
    message("Some supplied years (", paste(setdiff(years, 
                                                   available_years), collapse = ", "), ") are not available.")
  }
  mat <- sapply(as.character(available_years_req), function(y) icesDatras::getSurveyYearQuarterList(survey, 
                                                                                        as.integer(y)), simplify = FALSE)
  mat <- sapply(mat, function(x) as.integer(1:4 %in% x))
  row.names(mat) <- 1:4
  if (sum(mat[quarters, ]) == 0) {
    message("Supplied quarters (", paste(quarters, collapse = ", "), 
            ") are not available.\n  Available options are:\n", 
            paste(capture.output(print(mat)), collapse = "\n"))
    return(FALSE)
  }
  else if (sum(mat[quarters, ] == 0) > 0) {
    message("Some supplied quarter and year combinations are not available.")
  }
  amat <- mat[quarters, , drop = FALSE]
  qvec <- quarters[row(amat)[amat == 1]]
  yvec <- available_years_req[col(amat)[amat == 1]]
  if (listData) message("Data being extracted for:\n", paste(capture.output(print(cbind.data.frame(survey = survey, 
                                                                                     year = yvec, quarter = qvec))), collapse = "\n"))
  url <- sprintf("https://datras.ices.dk/WebServices/DATRASWebService.asmx/get%sdata?survey=%s&year=%i&quarter=%i", 
                 record, survey, yvec, qvec)
  out <- lapply(url, function(x) {
    x <- readDatras(x)
    icesDatras::parseDatras(x)
  })
  out <- do.call(rbind, out)
  out
}
