setGeneric('filterPD', function(object, ...){standardGeneric('filterPD')})

setMethod('filterPD',
          signature=(object='PD'),
          definition <- function(object, filter=N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl){
            if (missing(filter)){
              e <- substitute(N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl)
              objectSub <- definition2(object, subset=N>10 & nZC>0 & W4>0 & freq1>1 & freq1<2e7 & !refl)
            } else {
              e <- substitute(filter)
              objectSub <- definition2(object, subset=e)
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
          definition2 <- function(x,
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
              isLanguage <- try(is.language(subset), silent=TRUE)
              if (class(isLanguage)=='try-error' || !isLanguage) subset <- substitute(subset)
              ##              e <- substitute(subset)
              r <- eval(subset, df, environment(df))
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
