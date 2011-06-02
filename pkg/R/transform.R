setGeneric('transformPD', function(object, fun){standardGeneric('transformPD')})

setMethod('transformPD',
          signature=(object='PD'),
          definition <- function(object, fun){
            if (missing(fun)) fun=function(x){
              lambda <- powerTransform(x~1)
              res <- bcPower(x, coef(lambda))
            }
            trans <- lapply(object@data, fun)
            trans <- as.data.frame(trans)
            object@data <- trans
            object@transformed <- TRUE
            object
          }
          )

setGeneric('isTransformed', function(object, ...){standardGeneric('isTransformed')})
setMethod('isTransformed', signature=(object='PD'), definition <- function(object) object@transformed)
