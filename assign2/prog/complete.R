complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  result <- data.frame(id = NA, nobs = NA)
  for (i in id) {
    csvName <- paste(sprintf("%03d", i), "csv", sep = ".")
    fileName <- paste(directory, csvName, sep = "/")
    data <- read.csv(fileName)
    completeData <- data[complete.cases(data),]
    numRows <- nrow(completeData)
    result <- rbind(result, c(i, numRows))
  }
  result[complete.cases(result),]
}
