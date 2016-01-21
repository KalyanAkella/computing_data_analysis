best <- function (state, outcome) {
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- table(row.names = c("heart attack", "heart failure", "pneumonia"))
  outcomes[1] <- 11; outcomes[2] <- 17; outcomes[3] <- 23
  
  for (i in 1:3) {
    outcomeData[, outcomes[i]] <- as.numeric(outcomeData[, outcomes[i]])
  }
  
  if (!(state %in% outcomeData$State)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% row.names(outcomes))) {
    stop("invalid outcome")
  }
  
  outcomeDataGivenState <- outcomeData[outcomeData$State == state, ]
  inputOutcomes <- outcomeDataGivenState[, outcomes[outcome]]
  sortedOutcomeData <- outcomeDataGivenState[
    order(inputOutcomes, outcomeDataGivenState$Hospital.Name), ]
  sortedOutcomeData[1, "Hospital.Name"]
}