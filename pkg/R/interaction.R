setGeneric('identifyPD', function(object, ...){standardGeneric('identifyPD')})

setMethod('identifyPD', signature=(object='PD'),
          definition=function(object, label='energy', column=1, row=1, pch=13, cex=0.6, col='darkgreen',...){
            trellis.focus('panel', column, row, ...)
            trellisType <- as.character(trellis.last.object()$call)[1]
            if (trellisType=='splom'){
              idx <- panel.link.splom(pch=pch, cex=cex, col=col,...)
              object[idx,]
            } else {
              lbl=round(object@data[label], 1)
              idx <- panel.identify(label=lbl[,1], pch=pch, cex=cex, col=col,...)
              as.data.frame(object)[idx,]
            }
            trellis.unfocus()
          }
          )

choosePoints <- function(...){
  trellis.focus('panel', 1, 1)
  x <- trellis.panelArgs()$x
  y <- trellis.panelArgs()$y
  xy <- xy.coords(x, y, recycle = TRUE)
  x <- xy$x
  y <- xy$y
  px <- convertX(unit(x, "native"), "points", TRUE)
  py <- convertY(unit(y, "native"), "points", TRUE)
  pointsData <- cbind(px, py)

  border <- as.numeric()

  while (TRUE){
    ll <- grid.locator(unit='native')
    if (!is.null(ll)){
      lpoints(ll, col='red', cex=0.7, pch=3)
      lx <- convertX(unit(ll$x, 'native'), 'points', FALSE)
      ly <- convertY(unit(ll$y, 'native'), 'points', FALSE)
      border <- rbind(border, c(lx, ly))
    } else {    
      break
    }
  }
  inside <- in.out(border, pointsData)
  dataInside <- data.frame(xin=x[inside], yin=y[inside])
  drawLayer(layer(panel.points(xin, yin, col='red', cex=0.4),
                  data=dataInside)
            )
  trellis.unfocus()
  result <- inside
}
