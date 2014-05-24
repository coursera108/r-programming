rankhospital  <- function(state, outcome, num="best") {
    data.outcome <- read.csv("outcome-of-care-measures.csv", header=TRUE, colClasses="character")
    outcomes <- c("heart failure", "heart attack", "pneumonia")
    states <- unique(data.outcome$State)
    if (!(state %in% states))
        stop("invalid state")
    if(!(outcome %in% outcomes))
        stop("invalid outcome")
    if(num == "best")
        num = 1
   
     
    colnames(data.outcome)[colnames(data.outcome) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- "heart failure"
    colnames(data.outcome)[colnames(data.outcome) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- "heart attack"
    colnames(data.outcome)[colnames(data.outcome) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- "pneumonia"
    data.outcome[, "heart failure"] <- as.numeric(data.outcome[, "heart failure"])
    data.outcome[, "heart attack"] <- as.numeric(data.outcome[, "heart attack"])
    data.outcome[, "pneumonia"] <- as.numeric(data.outcome[, "pneumonia"])
    
    data.outcome.state <- data.outcome[data.outcome$State == state,c("Hospital.Name", "heart failure", "heart attack", "pneumonia")]
    outcome.sort <- data.outcome.state[order(data.outcome.state[, outcome], data.outcome.state[, "Hospital.Name"]), ]
    if (num == "worst")
        num = nrow(outcome.sort[!is.na(outcome.sort[, outcome]),])
    outcome.sort[num,"Hospital.Name"]

}
