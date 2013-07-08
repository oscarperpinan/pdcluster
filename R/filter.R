setGeneric('filterPD', function(object, ...){standardGeneric('filterPD')})

setMethod('filterPD',
          signature=(object='PD'),
          definition <- function(object,
                                 filter=N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl,
                                 select){
            df <- as.data.frame(object)
            dat <- object@data
            if (missing(filter)) {
              filter <- substitute(N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl)
            } else {
              filter <- substitute(filter)
              }
            ##some code borrowed from base::subset.data.frame
            r <- eval(filter, df, parent.frame())
            if (missing(select)) 
              vars <- TRUE
            else {
              nl <- as.list(seq_along(dat))
              names(nl) <- names(dat)
              vars <- eval(substitute(select), nl, environment(dat))
              ##FIX: energy should be always in vars
            }
            object@angle <- df$angle[r]
            object@data <- as.data.frame(dat[r, vars])
            object@refl <- df$refl[r]
            object@filtered=TRUE
            object@filter=filter
            object@refl.rm=("!refl" %in% strsplit(deparse(filter), ' ')[[1]])
            object
          }
          )

setGeneric('isFiltered', function(object, ...){standardGeneric('isFiltered')})
setMethod('isFiltered', signature=(object='PD'), definition <- function(object) object@filtered)
