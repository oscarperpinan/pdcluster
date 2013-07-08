layerRef <- function(object){
  layer({
    centro <- median(energy)
    amplitud <-1.5*IQR(energy)
    ang <- seq(0, 360, 5)
    panel.xyplot(ang, amplitud*sin(ang*pi/180)+centro, type='l', col.line='gray50', lwd=1.4)
  }, data=object)
}

angleScale <- function(...){
  ans <- xscale.components.default(...)
  where <- seq(0, 360, 90)
  ans$bottom$ticks$at <- where
  ans$bottom$labels$at <- where
  ans$bottom$labels$labels <- as.character(where)
  ans
}

layerGrid <- layer(panel.refline(v=c(0, 90,180, 270,360))) + layer(panel.grid(h=-1, v=0))

setGeneric('xyplot')##, function(x, data,...){standardGeneric('xyplot')})

setMethod('xyplot',
          signature=c(x='PD', data='missing'),
          definition <- function(x, plot.refl=TRUE, ...){
            dt=as.data.frame(x)
            settings <- list(xlab='phase', ylab='energy',
                                                       xscale.components=angleScale,
                             par.settings=pd.theme, alpha=0.2)
            call <- modifyList(settings, list(...))
            call$data <- dt
            if (x@refl.rm==FALSE & plot.refl==TRUE){ ##muestro los reflejos en paneles separados
              call$strip=strip.custom(strip.names=c(TRUE, TRUE),
                            strip.levels=c(TRUE, TRUE), bg='gray')
              call$x <- as.formula('energy~angle|refl')
              p <- do.call(xyplot, call)
            } else { ##todo junto
              call$x <- as.formula('energy~angle')
              p <- do.call(xyplot, call)
            }
            result <- p+layerRef(dt)+layerGrid 
            print(result)
          }
          )

setMethod('xyplot',
          signature=c(x='PDCluster', data='missing'),
          definition <- function(x,
                                 distances, clusters,
                                 plot.refl=TRUE,
                                 panelClust=TRUE,
                                 ...
                                 ){
            if (missing(distances)) distances <- seq_along(levels(factor(x@dist$distFactor)))
            if (missing(clusters)) clusters <- seq_along(levels(factor(x@cluster)))
            dt <- as.data.frame(x)
            settings <- list(xlab='phase', ylab='energy',
                             alpha=0.2,
                             xscale.components=angleScale,
                             auto.key=list(space='right',
                               cex.title=0.8,
                               lines=FALSE, points=TRUE, cex=1),
                             strip=strip.custom(strip.names=c(TRUE, TRUE),
                               strip.levels=c(TRUE, TRUE), bg='gray'))
            call <- modifyList(settings, list(...))           
            call$data=subset(dt, (distFactor %in% distances) & (cluster %in% clusters))
            if (panelClust){
              call$auto.key$title <- 'Distance\nto Medoid'
                              call$groups=call$data$distFactor
                call$par.settings <- custom.theme.4
              if (x@refl.rm==FALSE & plot.refl==TRUE){ ##muestro los reflejos en paneles separados
                call$x <- as.formula('energy~angle|cluster+refl')
                px <- do.call(xyplot, call)
                p <- useOuterStrips(px,
                                    strip=strip.custom(strip.names=c(TRUE, TRUE),
                                      strip.levels=c(TRUE, TRUE), bg='gray'),
                                    strip.left=strip.custom(strip.levels=c(TRUE, TRUE),
                                      strip.names=c(TRUE, TRUE), bg='gray')
                                    )
              } else { ##todo junto
                call$x <- as.formula('energy~angle|cluster')
                p <- do.call(xyplot, call)
              }  
            } else { ##end of panelClust==TRUE
              call$auto.key$title <- 'Clusters'
              call$groups=call$data$cluster
              call$par.settings <- pd.theme
              if (plot.refl & !x@refl.rm){ ##muestro los reflejos en paneles separados
                call$x <- as.formula('energy~angle|refl')
                p <- do.call(xyplot, call)
              } else { ##todo junto
                call$x <- as.formula('energy~angle')
                p <- do.call(xyplot, call)
              }
            }
            result <- p + layerRef(call$data) + layerGrid
            print(result)
          }
          )
