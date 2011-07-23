label.densityplot <- function(x, group.number, col.line){
  d <- density(x)
  i <- which.max(d$y)
  ltext(d$x[i],d$y[i],group.number,adj=c(0.2,0),col=col.line, cex=0.7)
}

setGeneric('densityplot')
setMethod('densityplot', signature=c(x='PD', data='missing'),
          definition=function(x, vars, ...){
            if (missing(vars)) vars <- names(x@data)
            dtLong <- PD2Long(x)
            settings <- list(scales=list(x=list(relation='free', cex=0.7),
                          y=list(relation='free', draw=FALSE)),
                             par.settings=pd.theme, pch='.',
                        xlab='', ylab='')
            call <- modifyList(settings, list(...))
            call$x <- as.formula('~value|var')
            call$data <- subset(dtLong, subset=var %in% vars)
            p <- do.call(densityplot, call)
            p
          }
          )

setMethod('densityplot', signature=c(x='PDCluster', data='missing'),
          definition = function(x, vars, clusters,...){
            if (missing(vars)) vars <- names(x@data)
            if (missing(clusters)) clusters <- seq_along(levels(factor(x@cluster)))
            dtLong <- PD2Long(x)
            settings <- list(scales=list(x=list(relation='free', cex=0.7),
                               y=list(relation='free', draw=FALSE)),
                             between=list(x=0.3),
                             par.settings=pd.theme, pch='.',
                             xlab='', ylab='')
            call <- modifyList(settings, list(...))
            call$x <- as.formula('~value|var')
            call$data <- subset(dtLong, subset=(var %in% vars) & (cluster %in% clusters))
            call$groups <- dtLong$cluster
            pd <- do.call(densityplot, call)
            print(pd+glayer(label.densityplot(x, group.number, col.line)))
          }
          )
