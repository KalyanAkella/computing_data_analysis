sortMedianPlot <- function () {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[, 11] <- as.numeric(outcome[, 11])
  
  stateFactor <- factor(outcome$State)
  mediansByState <- sapply(outcomeByState, function (ele) median(ele[, 11], na.rm = TRUE))
  for (state in unique(stateFactor)) {
    medianForState <- mediansByState[[state]]
    outcome[outcome$State == state, "median"] <- medianForState
  }
  outcome <- outcome[order(outcome$median), ]
  deaths <- outcome[, 11]
  states <- factor(outcome$State, ordered = TRUE)
  boxplot(deaths ~ states)
}