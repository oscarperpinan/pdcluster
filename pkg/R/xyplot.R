layerRef <- function(object){
  layer({
    centro <- median(energy)
    amplitud <-2*IQR(energy)
    ang <- seq(0, 360, 5)
    panel.xyplot(ang, amplitud*sin(ang*pi/180)+centro, type='l', col='gray', lwd=1.4)
  }, data=object)
}
setGeneric('xyplot', function(x, data,...){standardGeneric('xyplot')})
setMethod('xyplot',
          signature=c(x='PD', data='missing'),
          definition <- function(x,
                                 par.settings=pd.theme,
                                 plot.refl=TRUE,
                                 alpha=0.2,...
                                 ){
            dt=x@data
            dt$angle=x@angle
            if (x@refl.rm==FALSE & plot.refl==TRUE){ ##muestro los reflejos en paneles separados
              dt$refl=x@refl
              p <- xyplot(energy~angle|refl, data=dt,
                          par.settings=par.settings, alpha=alpha, type=c('p', 'g'),
                          strip=strip.custom(strip.names=c(TRUE, TRUE),
                            strip.levels=c(TRUE, TRUE), bg='gray'),
                          ...
                          )
            } else { ##todo junto
              p <- xyplot(energy~angle, data=dt,
                          par.settings=par.settings, alpha=alpha, type=c('p', 'g'))
            }
            result <- p+layerRef(dt)
            print(result)
          }
          )

setMethod('xyplot',
          signature=c(x='PDCluster', data='missing'),
          definition <- function(x,
                                 distances, clusters,
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
            fooplot <- function(formula, groups, data,
                                ...){
              environment(formula) <- environment() 
              data=subset(data, (distFactor %in% distances) & (cluster %in% clusters))
              groups=eval(substitute(groups), data, environment(formula))
              p <- xyplot(formula, groups=groups, data=data,
                          type=c('p', 'g'),
                          auto.key=list(corner=c(1,1),
                                        #title='Cluster', cex.title=1.1,
                            lines=FALSE, points=TRUE, cex=1),
                          strip=strip.custom(strip.names=c(TRUE, TRUE),
                            strip.levels=c(TRUE, TRUE), bg='gray'),
                          ...)
              p
            }
            if (panelClust){
              if (x@refl.rm==FALSE & plot.refl==TRUE){ ##muestro los reflejos en paneles separados
                formula <- as.formula('energy~angle|cluster+refl')
                p <- useOuterStrips(fooplot(formula, distFactor, dt, par.settings=par.settings),
                                    strip=strip.custom(strip.names=c(TRUE, TRUE),
                                      strip.levels=c(TRUE, TRUE), bg='gray'),
                                    strip.left=strip.custom(strip.levels=c(TRUE, TRUE),
                                      strip.names=c(TRUE, TRUE), bg='gray')
                                    )
              } else { ##todo junto
                formula <- as.formula('energy~angle|cluster')
                p <- fooplot(formula, distFactor, dt, alpha=alpha, par.settings=par.settings)
              }  
            } else { ##end of panelClust==TRUE
                   if (plot.refl & !x@refl.rm){ ##muestro los reflejos en paneles separados
                formula <- as.formula('energy~angle|refl')
                p <- fooplot(formula, cluster, dt, alpha=alpha, par.settings=custom.theme.3)
              } else { ##todo junto
                formula <- as.formula('energy~angle')
                p <- fooplot(formula, cluster, dt, alpha=alpha, par.settings=custom.theme.3)
              }
            }
            result <- p + layerRef(dt)
            print(result)
          }
          )
