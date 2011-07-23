setGeneric('splom')
setMethod('splom',
          signature=c(x='PD', data='missing'),
          definition=function(x, ...){
            splom(~x@data,
                  xlab='',
                  panel=panel.hexbinplot,
                  diag.panel = function(x, ...)
                  {
                    yrng <- current.panel.limits()$ylim
                    d <- density(x)
                    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
                    panel.lines(d)
                    diag.panel.splom(x, ...)
                  },
                  lower.panel = function(x, y, ...){
                    panel.hexbinplot(x, y, ...)
                    panel.loess(x, y, ..., col = 'red')
                  },
                  pscale=0)
          }
          )
