library(lattice)
package ? lattice
library(help = lattice)
data(environmental)
?environmental
# simple scatter plot between ozone (y-axis) and radiation (x-axis)
xyplot(ozone ~ radiation, data = environmental)
xyplot(ozone ~ radiation, data = environmental, main = "Ozone vs Radiation")
xyplot(ozone ~ temperature, data = environmental)
summary(environmental$temperature)
?equal.count

# create 4 different ranges for the temperature variable
temp.cut <- equal.count(environmental$temperature, 4)

xyplot(ozone ~ radiation | temp.cut, data = environmental)

xyplot(ozone ~ radiation | temp.cut, data = environmental, layout = c(1,4))

xyplot(ozone ~ radiation | temp.cut, data = environmental, layout = c(1,4), as.table = TRUE)

xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table = TRUE)

xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table = TRUE,
       panel = function (x, y, ...) {
         panel.xyplot(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })

xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table = TRUE, pch = 20,
       panel = function (x, y, ...) {
         panel.xyplot(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit, lwd = 2)
       })

xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table = TRUE, pch = 20,
       panel = function (x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y)
       })

wind.cut <- equal.count(environmental$wind, 4)

xyplot(ozone ~ radiation | temp.cut * wind.cut, data = environmental,
       panel = function (x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y)
       }, as.table = TRUE, pch = 20, xlab = "Solar Radiation",
       ylab = "Ozone (ppb)", main = "Ozone vs Solar Radiation")

splom(~ environmental)
histogram(~ temperature, data = environmental)

histogram(~ temperature | wind.cut, data = environmental)

histogram(~ ozone | wind.cut, data = environmental)

histogram(~ ozone | wind.cut * temp.cut, data = environmental)

