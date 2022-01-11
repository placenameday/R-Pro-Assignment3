rankhospital <- function(state, outcome, num = "best") { 
  ## Read outcome data
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  statelist <- unique(dt[,"State"])
  if (!(state %in% statelist)) {
    stop("invalid state")
  } else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank ## 30-day death rate
  if (outcome == "heart attack") {
    ot <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    ot <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    ot <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  dt <- dt[which(dt$State == state),]
  dt[,ot] = suppressWarnings(as.numeric(dt[,ot]))
  dt <- dt[which(!is.na(dt[,ot])),]
  rankdt <- dt[order(dt[, ot],dt[, 2]),]
  
  if (num == "best"){
    return(rankdt[1,2])
  } else if (num == "worst") {
    return(rankdt[nrow(rankdt),2])
  } else if (num > nrow(rankdt)) {
    return(NA)
  } else return(rankdt[num,2])
}
