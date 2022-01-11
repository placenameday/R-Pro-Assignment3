best <- function(state, outcome) { ## Read outcome data
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  statelist <- unique(dt[,"State"])
  if (!(state %in% statelist)) {
    stop("invalid state")
  } else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death ## rate
  if (outcome == "heart attack") {
    ot <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    ot <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    ot <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  dt <- dt[which(dt$State == state),]
  dt[,ot] = suppressWarnings(as.numeric(dt[,ot]))
  bestt <- dt[which(dt[,ot] == min(dt[,ot], na.rm = TRUE)), 2]
  
  bestt
}