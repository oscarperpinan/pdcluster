setGeneric('transformPD', function(object){standardGeneric('transformPD')})

setMethod('transformPD',
          signature=(object='PD'),
          definition <- function(object){
            trans <- lapply(object@data,
                            function(x){
                              lambda <- powerTransform(x~1)
                              res <- bcPower(x, coef(lambda))
                            }
                            )
            trans <- as.data.frame(trans)
            object@data <- trans
            object@transformed <- TRUE
            object
          }
          )

setGeneric('isTransformed', function(object, ...){standardGeneric('isTransformed')})
setMethod('isTransformed', signature=(object='PD'), definition <- function(object) object@transformed)
