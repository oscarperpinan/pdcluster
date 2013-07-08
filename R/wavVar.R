wavVarPD <- function(x, ...){
    aux <- wavVar(x, ...)
    v <- var(x)
    result=rep(0, 10)
    ##scales=2^seq(0,9)
    unbiased.data <- aux$block$unbiased
    n=length(unbiased.data)
    result[1:n] <- unbiased.data/v
    return(result)
  }
