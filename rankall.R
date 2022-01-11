rankall <- function(outcome, num = "best") { 
  ## Read outcome data
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  statelist <- unique(dt[,"State"])
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  if (outcome == "heart attack") {
    ot <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    ot <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    ot <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  ## For each state, find the hospital of the given rank
  nrh <- function(state, outcome, num = "best") {
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
  
  ## Initial a dataframe.
  arank <- data.frame(hospital = character(), state = character(), stringsAsFactors=FALSE)
  
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  for (state in statelist) {
    hosp <- nrh(state, outcome, num)
    mh <- data.frame(hospital = hosp, state = rep(state, times=length(hosp)))
    arank <- rbind(arank, mh)
  }
 
  arank
}
