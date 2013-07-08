## panel.ahull <- function(x, y, alpha=10, lty=1, lwd=1.5, col='black'){
##   calcArc <- function (c, r, v, theta) ##adapted from alphahull::arc
##     {
##       angles <- anglesArc(v, theta)
##       seqang <- seq(angles[1], angles[2], length = 100)
##       x <- c[1] + r * cos(seqang)
##       y <- c[2] + r * sin(seqang)
##       res=cbind(x, y)
##     } 
##   ah <- ahull(x, y, alpha=alpha)
##   arcos <- subset(ah$arcs, ah$arcs[, 3] > 0)
##   arcos <- data.frame(t(arcos))
##   lapply(arcos, function(x)llines(calcArc(x[1:2], x[3], x[4:5], x[6]), lty=lty, lwd=lwd, col=col))
## }

## panel.ashape <- function(x, y, alpha=30, lty=1, lwd=1.5, col='black'){
##   as <- ashape(x, y, alpha=alpha)
##   lineas <- as$edges
##   lineas <- data.frame(t(lineas))
##   lapply(lineas, function(x)panel.segments(x[3], x[4], x[5], x[6], lty=lty, lwd=lwd, col=col))
## }
