agecount <- function (age = NULL) {
  if (is.null(age)) {
    stop("missing age")
  }
  homicides <- readLines("homicides.txt")
  r <- regexec("([0-9]+) years old", homicides)
  matches <- regmatches(homicides, r)
  ages <- sapply(matches, function (x) as.numeric(x[2]))
  ages <- ages[complete.cases(ages)]
  result <- 0
  for (i in seq_along(ages)) {
    if (age == ages[i]) {
      result <- result + 1
    }
  }
  result
}
