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
  hospitalList <- lapply(s, function(x) x$"Hospital.Name"[order(as.numeric(x[[colName]]),
                                                x$"Hospital.Name", na.last = NA)] )

  if (num == "best") {
    hospitalList <- lapply(hospitalList, function(x) x[1])
  }else if (num == "worst") {
    hospitalList <- lapply(hospitalList, function(x) x[length(hospitalList[x])])
  }else {
    hospitalList <- lapply(hospitalList, function(x) x[num])
  }

  data.frame(hospital = unlist(hospitalList), state = names(hospitalList))

}
