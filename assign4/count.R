count <- function (cause = NULL) {
  causes <- paste("^asphyxiation$", "^blunt force$", "^other$", "^shooting$", "^stabbing$", "^unknown$", sep = "|")
  match <- grep(causes, cause, ignore.case = TRUE)
  if (length(match) == 0) {
    stop("invalid cause")
  }
  
  homicides <- readLines("homicides.txt")
  causeRegExp <- paste("Cause:", cause)
  result <- grep(causeRegExp, homicides, ignore.case = TRUE)
  length(result)
}
