rankall <- function (outcome, num = "best") {
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- table(row.names = c("heart attack", "heart failure", "pneumonia"))
  outcomes[1] <- 11; outcomes[2] <- 17; outcomes[3] <- 23
  
  for (i in 1:3) {
    outcomeData[, outcomes[i]] <- as.numeric(outcomeData[, outcomes[i]])
  }
  
  if (!(outcome %in% row.names(outcomes))) {
    stop("invalid outcome")
  }
  
  result <- data.frame(hospital = NA, state = NA)
  for (state in unique(factor(outcomeData$State))) {
    outcomeByState <- outcomeData[outcomeData$State == state, ]
    outcomeByState <- outcomeByState[complete.cases(outcomeByState[, outcomes[outcome]]), ]
    sortedOutcomeByState <- outcomeByState[order(outcomeByState[, outcomes[outcome]], outcomeByState$Hospital.Name), ]
    if (num == "best")
      result <- rbind(result, c(sortedOutcomeByState[1, "Hospital.Name"], state))
    else if (num == "worst")
      result <- rbind(result, c(sortedOutcomeByState[nrow(sortedOutcomeByState), "Hospital.Name"], state))
    else if (is.numeric(num)) {
      if (num <= nrow(sortedOutcomeByState))
        result <- rbind(result, c(sortedOutcomeByState[num, "Hospital.Name"], state))
      else
        result <- rbind(result, c(NA, state))
    }
  }
  result <- result[complete.cases(result$state), ]
  result[order(result$state), ]
}
