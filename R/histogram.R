setGeneric('histogram')
setMethod('histogram',
          signature=c(x='PD', data='missing'),
          definition = function(x, vars, ...){
            if (missing(vars)) vars <- names(x@data)
            formula <- as.formula(paste('~', paste(vars, collapse='+'), sep=''))
            settings <- list(scales=list(x=list(relation='free'),
                             y=list(relation='free',
                               draw=FALSE)),
                           breaks=100, col='gray',
                           xlab='',
                           strip.names=c(TRUE, TRUE),
                             bg='gray', fg='darkblue')
            call <- modifyList(settings, list(...))
            call$x <- formula
            call$data <- x@data
            p <- do.call(histogram, call)
            print(p)
          }
)


setMethod('histogram',
          signature=c(x='PDCluster', data='missing'),
          definition = function(x, vars, clusters, ...){
            if (missing(vars)) vars <- names(x@data)
            if (missing(clusters)) clusters <- seq_along(levels(factor(x@cluster)))
            dtLong <- PD2Long(x)
            settings <- list(scales=list(x=list(relation='free'),
                            y=list(relation='free',
                              draw=FALSE)),
                          breaks=100, col='gray',
                          xlab='')
            call <- modifyList(settings, list(...))
            call$x <- as.formula('~value|var+cluster')
            call$data=subset(dtLong, subset=(var %in% vars) & (cluster %in% clusters))
            p <- do.call(histogram, call)
            pOuter <- useOuterStrips(p,
                                     strip=strip.custom(
                                       strip.levels=c(TRUE, TRUE),
                                       strip.names=c(FALSE, FALSE),
                                       bg='gray', fg='transparent'),
                                     strip.left=strip.custom(
                                       strip.levels=c(TRUE, TRUE),
                                       strip.names=c(TRUE, TRUE),
                                       bg='gray', fg='transparent')
                                     )
            print(combineLimits(pOuter, margin.y=NULL))
          }
          )
