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
            r <- eval(filter, df, environment(df))
            if (missing(select)) 
              vars <- TRUE
            else {
              nl <- as.list(seq_along(dat))
              names(nl) <- names(dat)
              vars <- eval(substitute(select), nl, environment(dat))
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


## setGeneric('subset')

## setMethod('subset',
##           signature=(object='PD'),
##           definition2 <- function(x,
##                                  subset,
##                                  select,
##                                  drop=FALSE,
##                                  ...){

##             df <- as.data.frame(x)
##             dat <- x@data
##             if (missing(subset)) {
##               r <- TRUE 
##             } else {
##               isLanguage <- try(is.language(subset), silent=TRUE)
##               if (class(isLanguage)=='try-error' || !isLanguage) subset <- substitute(subset)
##               ##              e <- substitute(subset)
##               r <- eval(subset, df, environment(df))
##             }
##             if (missing(select)) 
##               vars <- TRUE
##             else {
##               nl <- as.list(seq_along(dat))
##               names(nl) <- names(dat)
##               vars <- eval(substitute(select), nl, environment(dat))
##             }
##             x@angle <- df$angle[r]
##             x@data <- as.data.frame(dat[r, vars])
##             x@refl <- df$refl[r]
##             x
##           }
##           )
