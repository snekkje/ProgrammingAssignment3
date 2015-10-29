rankhospital <- function(state, outcome, num = "best") {
  if(outcome == "heart attack"){
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else stop("invalid outcome")
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (state %in% data$State) {
    s <- split(data, data$State)
    
    hospitalList <- lapply(s, function(x) x$"Hospital.Name"[order(as.numeric(x[[colName]]),
                                                  x$"Hospital.Name", na.last = NA)] )

    lengthList <- length(hospitalList[[state]])
    
    if (num == "best") {
      hospitalList[[state]][[1]]
    }else if (num == "worst") {
      hospitalList[[state]][[lengthList]]
    }else if (num > lengthList) {
      NA
    }else {
      hospitalList[[state]][[num]]
    }

  } else stop("invalid state")
}