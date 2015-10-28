best <- function(state, outcome) {
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

    hospitalList <- lapply(s, function(x) x$"Hospital.Name"[as.numeric(x[[colName]]) ==
                                                   min(as.numeric(x[[colName]]), na.rm = TRUE)] )

    sortedHospitalList <- sort(hospitalList[[state]][!is.na(hospitalList[[state]])])
    sortedHospitalList[1]

  } else stop("invalid state")

}
