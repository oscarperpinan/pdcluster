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
          definition <- function(x,
                                 xlab='phase', ylab='energy',
                                 par.settings=pd.theme,
                                 plot.refl=TRUE,
                                 alpha=0.2,...
                                 ){
            dt=x@data
            dt$angle=x@angle
            if (x@refl.rm==FALSE & plot.refl==TRUE){ ##muestro los reflejos en paneles separados
              dt$refl=x@refl
              p <- xyplot(energy~angle|refl, data=dt,
                          xscale.components=angleScale,
                          xlab=xlab, ylab=ylab,
                          par.settings=par.settings, alpha=alpha,
                          strip=strip.custom(strip.names=c(TRUE, TRUE),
                            strip.levels=c(TRUE, TRUE), bg='gray'),
                          ...
                          )
            } else { ##todo junto
              p <- xyplot(energy~angle, data=dt,
                          xscale.components=angleScale,
                          xlab=xlab, ylab=ylab,
                          par.settings=par.settings, alpha=alpha, ...)
            }
            result <- p+layerRef(dt)+layerGrid 
            print(result)
          }
          )

setMethod('xyplot',
          signature=c(x='PDCluster', data='missing'),
          definition <- function(x,
                                 distances, clusters,
                                 xlab='phase', ylab='energy',
                                 par.settings=custom.theme.4,
                                 plot.refl=TRUE,
                                 panelClust=TRUE,
                                 alpha=0.2,
                                 ...
                                 ){
            if (missing(distances)) distances <- seq_along(levels(factor(x@dist$distFactor)))
            if (missing(clusters)) clusters <- seq_along(levels(factor(x@cluster)))
            dt=x@data
            dt$angle=x@angle
            dt$cluster=x@cluster
            dt$distFactor=x@dist$distFactor
            dt$refl=x@refl
            fooplot <- function(formula, groups, data, keyTitle,
                                ...){
              environment(formula) <- environment() 
              data=subset(data, (distFactor %in% distances) & (cluster %in% clusters))
              groups=eval(substitute(groups), data, environment(formula))
              p <- xyplot(formula, groups=groups, data=data,
                          xscale.components=angleScale,
                          xlab=xlab, ylab=ylab,
                          auto.key=list(space='right', ##corner=c(1,1),
                            title=keyTitle, cex.title=0.8,
                            lines=FALSE, points=TRUE, cex=1),
                          strip=strip.custom(strip.names=c(TRUE, TRUE),
                            strip.levels=c(TRUE, TRUE), bg='gray'),
                          ...)
              p
            }
            if (panelClust){
              if (x@refl.rm==FALSE & plot.refl==TRUE){ ##muestro los reflejos en paneles separados
                formula <- as.formula('energy~angle|cluster+refl')
                p <- useOuterStrips(fooplot(formula, distFactor, dt,
                                            par.settings=par.settings,
                                            keyTitle='Distance\nto Medoid'),
                                    strip=strip.custom(strip.names=c(TRUE, TRUE),
                                      strip.levels=c(TRUE, TRUE), bg='gray'),
                                    strip.left=strip.custom(strip.levels=c(TRUE, TRUE),
                                      strip.names=c(TRUE, TRUE), bg='gray')
                                    )
              } else { ##todo junto
                formula <- as.formula('energy~angle|cluster')
                p <- fooplot(formula, distFactor, dt, alpha=alpha,
                             par.settings=par.settings, keyTitle='Distance\nto Medoid', ...)
              }  
            } else {                       ##end of panelClust==TRUE
              if (plot.refl & !x@refl.rm){ ##muestro los reflejos en paneles separados
                formula <- as.formula('energy~angle|refl')
                p <- fooplot(formula, cluster, dt, alpha=alpha,
                             par.settings=custom.theme.3,
                             keyTitle='Clusters', ...)
              } else { ##todo junto
                formula <- as.formula('energy~angle')
                p <- fooplot(formula, cluster, dt, alpha=alpha,
                             par.settings=custom.theme.3,
                             keyTitle='Clusters', ...)
              }
            }
            result <- p + layerRef(dt) + layerGrid
            print(result)
          }
          )
