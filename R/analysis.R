analysis <- function(x, threshold = 8e-3, n.levels = 4, ...){
  x0=no0(x, threshold = threshold)
  if (length(x0)> 10){    
      ## Wavelet variance
      wavelet=wavVarPD(x0, ...)
      ## Choose the number of levels
      n.levels <- min(length(wavelet), n.levels)
      wavelet <- wavelet[seq_len(n.levels)]
      ## Other features
      max.where <- which(x0==max(x0))[1] 
      rango <- abs(diff(range(x0)))
      len <- length(x0)
      energy <- sum(x0^2)
      nZC <- nZeroCross(x0)
      ## Prony method
      err <- try(pr <- prony(x0, M=10), silent=TRUE) ##descomposición por prony
      if (class(err)!='try-error'){
          freq <- pr$freq[1:2] ##Elijo como frecuencias aquellas 2  que transportan más energía
          damp <- abs(pr$damp[1:2])
      } else {
          freq <- c(1,1)
          ##names(freq)=c('freq1', 'freq2')
          damp <- c(1,1)
          ##names(damp)=c('damp1', 'damp2')
      }
      ## Return the result
      result <- c(max.where, wavelet, rango, len, energy, nZC, freq, damp)
      names(result) <- c('RefMax',
                         paste0('W', seq_len(n.levels)),
                         'range', 'N',
                         'energy',
                         'nZC',
                         'freq1', 'freq2', 'damp1', 'damp2')
      return(as.data.frame(t(result)))
  } else {
      return <- NULL
  }
}



