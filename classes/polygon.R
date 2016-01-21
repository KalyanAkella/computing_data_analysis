setClass("polygon", representation(x = "numeric", y = "numeric"))
setMethod("plot", "polygon", 
          function (x, y, ...) {      # must match the definition of plot() exactly
            plot(x@x, x@y, type = "n", ...)   #the polygon is passed inside 'x' and argument 'y' is ignored
            xp <- c(x@x, x@x[1])
            yp <- c(x@y, x@y[1])
            lines(xp, yp)
          })

p <- new("polygon", x = c(1, 2, 3, 4), y = c(1, 2, 3, 1))
plot(p)
