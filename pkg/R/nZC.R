nZeroCross <- function(x, ...){
  x=no0(x, ...)
  len=length(x)
  signo=ifelse(x>=0, 1, -1)
  signo.diff=diff(signo)
  result=1/(2*len)*sum(abs(signo.diff))
  return(result)
  }
