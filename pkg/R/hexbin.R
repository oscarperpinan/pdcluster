setGeneric('hexbinplot')
setMethod('hexbinplot',
          signature=c(x='PD', data='missing'),
          definition=function(x, 
            plot.refl=TRUE, ...
            ){
            pens <- t(matrix(brewer.pal('PuBu', n=4), nrow=2))
            dt <- x@data
            dt$refl <- x@refl
            anyRefl <- any(x@refl)
            dt$angle <- x@angle
            settings <- list(xscale.components=angleScale,
                             aspect=2, style='nested.centroids',
                             pen=pens, border=0,
                             xlab='phase', ylab='energy')
            if (plot.refl & anyRefl){ ##muestro los reflejos en paneles separados
              settings$strip=strip.custom(strip.names=c(TRUE, TRUE),
                strip.levels=c(TRUE, TRUE), bg='gray')
              call <- modifyList(settings, list(...))
              call$x <- as.formula('energy~angle|refl')
              call$data <- dt
              p <- do.call(hexbinplot, call)

            } else { ##todo junto
              call <- modifyList(settings, list(...))
              call$x <- as.formula('energy~angle')
              call$data <- dt
              p <- do.call(hexbinplot, call)

            }
            result <- p + layerRef(dt) + layerGrid
            print(result)
          }
          )

setMethod('hexbinplot',
          signature=c(x='PDCluster', data='missing'),
          definition=function(x,
            clusters,
            panelClust=TRUE,
            plot.refl=TRUE, ...
            ){
            pens <- t(matrix(brewer.pal('PuBu', n=4), nrow=2))
            dt <- x@data
            dt$cluster=x@cluster
            dt$refl <- x@refl
            anyRefl <- any(x@refl)
            dt$angle <- x@angle
            if (missing(clusters)) clusters <- seq_along(levels(factor(x@cluster)))

            settings <- list(xscale.components=angleScale,
                             aspect=2, style='nested.centroids',
                             pen=pens, border=0,
                             xlab='phase', ylab='energy')

            call <- modifyList(settings, list(...))

            if (panelClust){
              if (plot.refl & anyRefl){ ##muestro los reflejos en paneles separados
                call$x <- as.formula('energy~angle|cluster+refl')
                call$data <- subset(dt, subset=(cluster %in% clusters))
                ph <- do.call(hexbinplot, call)

                p <- useOuterStrips(ph,
                                    strip=strip.custom(strip.names=c(TRUE, TRUE),
                                      strip.levels=c(TRUE, TRUE), bg='gray'),
                                    strip.left=strip.custom(strip.levels=c(TRUE, TRUE),
                                      strip.names=c(TRUE, TRUE), bg='gray'))
              } else { ##plot.refl=TRUE
                call$strip=strip.custom(strip.names=c(TRUE, TRUE),
                  strip.levels=c(TRUE, TRUE), bg='gray')
                call$x <- as.formula('energy~angle|cluster')
                call$data <- subset(dt, subset=(cluster %in% clusters))
                p <- do.call(hexbinplot, call)
              }
              
            } else { ##panelClust=FALSE
              p <- hexbinplot(as(x, 'PD'),
                              plot.refl=plot.refl, ...)
            }
            result <- p + layerRef(dt) + layerGrid
            print(result)
          }
          )
