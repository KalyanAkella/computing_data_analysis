corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  result <- vector()
  ids = 1:332
  for (id in ids) {
    csvName <- paste(sprintf("%03d", id), "csv", sep = ".")
    fileName <- paste(directory, csvName, sep = "/")
    data <- read.csv(fileName)
    completeData <- data[complete.cases(data),]
    numRows <- nrow(completeData)
    if (numRows > threshold) {
      corr <- cor(completeData$sulfate, completeData$nitrate)
      result <- append(result, corr)
    }
  }
  result
}
