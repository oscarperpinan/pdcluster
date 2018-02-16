pdPanel <- function(..., yvar){
    panel.xyplot(...)
    centro <- median(yvar)
    amplitud <-1.5*IQR(yvar)
    ang <- seq(0, 360, 5)
    panel.xyplot(ang, amplitud * sin(ang * pi/180) + centro,
                 type = 'l', col.line = 'gray50', lwd = 1.4)
    panel.refline(v=c(0, 90,180, 270, 360))
    panel.grid(h=-1, v=0)
}

pdHexPanel <- function(..., yvar){
    panel.hexbinplot(...)
    centro <- median(yvar)
    amplitud <-1.5*IQR(yvar)
    ang <- seq(0, 360, 5)
    panel.xyplot(ang, amplitud * sin(ang * pi/180) + centro,
                 type = 'l', col.line = 'gray50', lwd = 1.4)
    panel.refline(v=c(0, 90,180, 270, 360))
    panel.grid(h=-1, v=0)
}

angleScale <- function(...){
  ans <- xscale.components.default(...)
  where <- seq(0, 360, 90)
  ans$bottom$ticks$at <- where
  ans$bottom$labels$at <- where
  ans$bottom$labels$labels <- as.character(where)
  ans
}



