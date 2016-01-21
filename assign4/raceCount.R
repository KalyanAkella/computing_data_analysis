raceCount <- function () {
  homicides <- readLines("homicides.txt")
  r <- regexec("<dd>([a-zA-Z]+) +(female|man|male), .*?</dd>|<dd>.*?Race: +([a-zA-Z ]+).*?<br", homicides)
  matches <- regmatches(homicides, r)
  races <- sapply(matches, function (x) {
    match1 <- nchar(x[2]) > 0
    match2 <- nchar(x[4]) > 0
    if (match1) {
      tolower(x[2])
    } else if (match2) {
      tolower(x[4])
    } else {
      NULL
    }
  })
  table(races)
}