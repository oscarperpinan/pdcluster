setGeneric('filterPD', function(object, ...){standardGeneric('filterPD')})

setMethod('filterPD',
          signature=(object='PD'),
          definition <- function(object, filter=N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl){
            ##                                 refl.rm=TRUE){
            ## e <- substitute(filter)##from subset.data.frame
            ## filter <- eval(e, object@data, parent.frame())
            
            ## if (refl.rm) filter = filter & !object@refl

            ## object@angle <- object@angle[filter]
            ## object@data <- object@data[filter,]
            ## object@refl <- object@refl[filter]

            ## object@filtered=TRUE
            ## object@refl.rm=refl.rm

            ## object
            if (missing(filter)){
              e <- substitute(N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl)
              objectSub <- subset(object, subset=N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl)
            } else {
              e <- substitute(filter)
              objectSub <- subset(object, subset=filter)
            }
            objectSub@filtered=TRUE
            objectSub@filter=e
            objectSub@refl.rm=("!refl" %in% strsplit(deparse(e), ' ')[[1]])
            objectSub
    
          }
          )

setGeneric('isFiltered', function(object, ...){standardGeneric('isFiltered')})
setMethod('isFiltered', signature=(object='PD'), definition <- function(object) object@filtered)


setGeneric('subset')

setMethod('subset',
          signature=(object='PD'),
          definition <- function(x,
                                 subset,
                                 select,
                                 drop=FALSE,
                                 ...){
##some code borrowed from base::subset.data.frame
            df <- as.data.frame(x)
            dat <- x@data
            if (missing(subset)) {
              r <- TRUE 
            } else {
              e <- substitute(subset)
              r <- eval(e, df, environment(df))
            }
            if (missing(select)) 
              vars <- TRUE
            else {
              nl <- as.list(seq_along(dat))
              names(nl) <- names(dat)
              vars <- eval(substitute(select), nl, environment(dat))
            }
            x@angle <- df$angle[r]
            x@data <- as.data.frame(dat[r, vars])
            x@refl <- df$refl[r]
            x
          }
          )
