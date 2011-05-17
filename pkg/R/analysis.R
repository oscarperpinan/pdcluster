analysis <- function(x, ...){
  x0=no0(x,...)
  if (length(x0)> 10){    
    wavelet=wavVarPD(x0, ...)[1:4] ##cuatro primeros niveles de varianza wavelet
    max.where=which(x0==max(x0))[1] 
    rango=abs(diff(range(x0)))
    len=length(x0)
    energy=sum(x0^2)
    nZC=nZeroCross(x0)
    err <- try(pr <- prony(x0, M=10), silent=TRUE) ##descomposición por prony
    if (class(err)!='try-error'){
      freq <- pr$freq[1:2] ##Elijo como frecuencias aquellas 2  que transportan más energía
      damp <- abs(pr$damp[1:2])
    } else {
      freq=c(1,1)
      ##names(freq)=c('freq1', 'freq2')
      damp=c(1,1)
      ##names(damp)=c('damp1', 'damp2')
    }
    ##result=c(max.where, wavelet, rango, len, energy, nZC)
    result=c(max.where, wavelet, rango, len, energy, nZC, freq, damp)
    names(result)=c('RefMax',
           'W1', 'W2', 'W3', 'W4',
           'range', 'N',
           'energy',
           'nZC',
           'freq1', 'freq2', 'damp1', 'damp2')
    return(as.data.frame(t(result)))
  } else {return <- NULL}
  
}

##example
## setwd('~/Investigacion/PD/Datos/20100922')##Con estas he hecho todo el desarrollo inicial

## load('DescargasRAW.RData')
## analysisList <- lapply(lista[1:10], analysis)
## pdData <- do.call(rbind, analysisList)

## pdSummary <- read.csv('descargas.csv')[1:10,]
## idxOrderSummary=order(pdSummary$sumaCuadrados)
## idxOrderData=order(pdData$energy)

## pdDataOrdered=cbind(pdData[idxOrderData,], pdSummary[idxOrderSummary,c('angulo', 'separacionOriginal')])

## ##necesario para que las descargas NO estén ordenadas por nivel de energía, lo que provoca que clara no funcione bien (las muestras no son aleatorias)
## idx <- do.call(order, pdSummary[idxOrderSummary, c('segundo', 'inicio')])
## pdDataOrdered <- pdDataOrdered[idx,]

## pd <- df2PD(pdDataOrdered)
#####################################3


