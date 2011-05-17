setGeneric('hexbinplot')
setMethod('hexbinplot',
          signature=c(x='PD', data='missing'),
          definition=function(x, 
            xlab='angle',
            ylab='energy',
            plot.refl=TRUE
            ){
            pens <- t(matrix(brewer.pal('PuBu', n=4), nrow=2))
            dt <- x@data
            dt$refl <- x@refl
            dt$angle <- x@angle
            if (plot.refl){ ##muestro los reflejos en paneles separados
              p <- hexbinplot(energy~angle|refl, data=dt,
                              aspect=2, style='nested.centroids',
                              pen=pens, border=0,
                              xlab=xlab, ylab=ylab,
                              strip=strip.custom(strip.names=c(TRUE, TRUE),
                                strip.levels=c(TRUE, TRUE), bg='gray')
                              )
            } else { ##todo junto
              p <- hexbinplot(energy~angle, data=dt,
                              xlab=xlab, ylab=ylab,
                              aspect=2, style='nested.centroids',
                              pen=pens, border=0)
            }
            result <- p + layerRef(dt)
            print(result)
          }
          )

setMethod('hexbinplot',
          signature=c(x='PDCluster', data='missing'),
          definition=function(x,
            clusters,
            panelClust=TRUE,
            xlab='angle',
            ylab='energy',
            plot.refl=TRUE
            ){
            pens <- t(matrix(brewer.pal('PuBu', n=4), nrow=2))
            dt <- x@data
            dt$cluster=x@cluster
            dt$refl <- x@refl
            dt$angle <- x@angle
            if (missing(clusters)) clusters <- seq_along(levels(factor(x@cluster)))
            if (panelClust){
              if (plot.refl){ ##muestro los reflejos en paneles separados
                p <- useOuterStrips(hexbinplot(energy~angle|cluster+refl, data=dt,
                                               aspect=2, style='nested.centroids',
                                               pen=pens, border=0, 
                                               subset=(cluster %in% clusters),
                                               xlab=xlab, ylab=ylab),
                                    strip=strip.custom(strip.names=c(TRUE, TRUE),
                                      strip.levels=c(TRUE, TRUE), bg='gray'),
                                    strip.left=strip.custom(strip.levels=c(TRUE, TRUE),
                                      strip.names=c(TRUE, TRUE), bg='gray'))
              } else { ##plot.refl=TRUE
                p <- hexbinplot(energy~angle|cluster, data=dt,
                                aspect=2, style='nested.centroids', pen=pens, border=0, 
                                subset=(cluster %in% clusters),
                                xlab=xlab, ylab=ylab,
                                strip=strip.custom(strip.names=c(TRUE, TRUE),
                                  strip.levels=c(TRUE, TRUE), bg='gray'))
              }
              result <- p + layerRef(dt)
              print(result)
            } else { ##panelClust=FALSE
              hexbinplot(as(x, 'PD'), xlab=xlab, ylab=ylab, plot.refl=plot.refl)
            }
          }
          )
