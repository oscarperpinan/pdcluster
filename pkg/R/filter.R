setGeneric('filterPD', function(object, ...){standardGeneric('filterPD')})

setMethod('filterPD',
          signature=(object='PD'),
          definition <- function(object, filter=N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7,
                                 refl.rm=TRUE){
            e <- substitute(filter)##from subset.data.frame
            filter <- eval(e, object@data, parent.frame())
            
            if (refl.rm) filter = filter & !object@refl

            object@angle <- object@angle[filter]
            object@data <- object@data[filter,]
            object@refl <- object@refl[filter]

            object@filtered=TRUE
            object@refl.rm=refl.rm

            object
          }
          )

setGeneric('isFiltered', function(object, ...){standardGeneric('isFiltered')})
setMethod('isFiltered', signature=(object='PD'), definition <- function(object) object@filtered)
