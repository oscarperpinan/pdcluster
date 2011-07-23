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
            densityplot(~value|var,
                        subset=var %in% vars,
                        data=dtLong,    #auto.key=list(x=0.8, y=0.8),
                        scales=list(x=list(relation='free', cex=0.7),
                          y=list(relation='free', draw=FALSE)),
                        breaks=100, par.settings=custom.theme.3, pch='.',
                        xlab='', ylab='', ...) #, layout=c(8,2))
          }
          )

setMethod('densityplot', signature=c(x='PDCluster', data='missing'),
          definition = function(x, vars, clusters, between=list(x=0.3),...){
            if (missing(vars)) vars <- names(x@data)
            if (missing(clusters)) clusters <- seq_along(levels(factor(x@cluster)))
            dtLong <- PD2Long(x)
            pd <- densityplot(~value|var, groups=cluster,
                              data=dtLong,
                              subset=(var %in% vars) & (cluster %in% clusters),
                              scales=list(x=list(relation='free', cex=0.7),
                                y=list(relation='free',
                                  draw=FALSE)),
                              between=between,
                              par.settings=custom.theme.3, pch='.',
                              xlab='', ylab='', ...)
            print(pd+glayer(label.densityplot(x, group.number, col.line)))
          }
          )
