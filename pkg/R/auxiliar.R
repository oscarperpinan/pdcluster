outlier<- function(x){
  five <- fivenum(x)
  iqr <- five[4L]-five[2L]
  limUp <- five[4L]+1.5*iqr
  limLow <- five[2L]-1.5*iqr
  out <- (x>limUp)|(x<limLow)
  out
}

## intervals <- function(x, nInt=10, nChosen=5, style='fisher') {
##   cls <- classIntervals(x, n=nInt, style=style)
##   int <- findInterval(cls$var, cls$brks, all.inside=TRUE)
##   intChosen <- rev(order(table(int)))[1:nChosen]
##   chosen <- (int %in% intChosen)
##   idx <- which(chosen)
##   res <- list(class=cls, interval=int, intChosen=intChosen, idx=idx, chosen=chosen)
## }
