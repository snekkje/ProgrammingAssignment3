rankall <- function(outcome, num = "best") {
  if(outcome == "heart attack"){
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else stop("invalid outcome")
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  s <- split(data, data$State)
  #hospitalList <- lapply(s, function(x) x$"Hospital.Name"[!is.na()] )
  hospitalList <- lapply(s, function(x) x$"Hospital.Name"[num] )
  #remove hospitals that do not have data for required oucome
  
  data.frame(hospital = unlist(hospitalList), state = names(hospitalList))
  
  
}