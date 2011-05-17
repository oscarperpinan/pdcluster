df2PD <- function(x, select=c('RefMax', 'W1', 'W2', 'W3', 'W4', 'range', 'N', 'energy', 'nZC', 'freq1', 'damp1'),
                  angle='angulo', refl='separacionOriginal', description=''){
  
  dt <- x[, select]
  angle <- x[,angle]
  refl <- x[,refl]!=0
  
  result<-new(Class='PD',
              angle=x$angulo,
              data=dt,
              filtered=FALSE,
              transformed=FALSE,
              refl=refl,
              refl.rm=FALSE,
              description=description)
  result

  
}
## AS.DATA.FRAME
setGeneric('as.data.frame', function(x, ...){standardGeneric('as.data.frame')})
setMethod('as.data.frame', signature=(x='PD'),
          definition=function(x, ...){
            df <- x@data
            df$angle <- x@angle
            df$refl <- x@refl
            df
          }
          )

setMethod('as.data.frame', signature=(x='PDCluster'),
          definition=function(x, ...){
            df <- as.data.frame(as(x, 'PD'))
            df$cluster <- x@cluster
            df <- cbind(df, x@dist)
            df
          }
          )

##PD2Long
setGeneric('PD2Long', function(object){standardGeneric('PD2Long')})

setMethod('PD2Long', signature=(object='PD'),
          definition=function(object){
            long <- stack(object@data)
            names(long) <- c('value', 'var')
            long
          }
          )

setMethod('PD2Long', signature=(object='PDCluster'),
          definition=function(object){
            long <- stack(object@data)
            names(long) <- c('value', 'var')
            long$cluster <- rep(object@cluster, nlevels(long$var))
            long
          }
)
