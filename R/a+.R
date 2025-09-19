safe_getDATRAS <- function(datatype = "HH", survey, years, quarters,
                           context = c("console","shiny")) {
  context <- match.arg(context)
  
  res <- suppressMessages(
    icesDatras::getDATRAS(datatype, survey, years, quarters)
  )
  
  if (identical(res, FALSE)) {
    msg <- paste0("Survey ", survey,
                  " with Year ", years,
                  " and Quarter ", paste(quarters, collapse = ","),
                  " does not exist.\n",
                  "Check available options with:\n",
                  "icesDatras::getDATRAS('", datatype, "', '", survey,
                  "', ", years, ", 1:4)")
    
    if (context == "console") {
      stop(msg, call. = FALSE)
    } else {
      shiny::validate(shiny::need(FALSE, msg))
    }
  }
  
  res
}
