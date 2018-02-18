library(pdCluster)

load('data/signalList.RData')

xyplot(signalList, y.same=NA, FUN=function(x){xyplot(ts(no0(x)))})

signal <- signalList[[3]]
pr <- prony(signal, M=10)
xyplot(pr)

compProny(signal, M=c(10, 20, 30, 40))

analysis(signal)

analysisList <- lapply(signalList[1:10], analysis)
pdData <- do.call(rbind, analysisList)

load('data/pdSummary.RData')

idxOrderSummary=order(pdSummary$sumaCuadrados)
idxOrderData=order(pdData$energy)

pdDataOrdered=cbind(pdData[idxOrderData,], 
pdSummary[idxOrderSummary,c('angulo', 'separacionOriginal')])

idx <- do.call(order, pdSummary[idxOrderSummary, c('segundo', 'inicio')])
pdDataOrdered <- pdDataOrdered[idx,]

pd <- df2PD(pdDataOrdered)

load('data/dfHibr.RData')

dfHibr <- df2PD(dfHibr)

dfFilter <- filterPD(dfHibr)

dfTrans <- transformPD(dfFilter)

nZCbefore <- as.data.frame(dfFilter)$nZC
nZCafter <- as.data.frame(dfTrans)$nZC
comp <- data.frame(After=nZCafter, Before=nZCbefore)

histogram(~After+Before, data=comp,
          scales=list(x=list(relation='free'),
                      y=list(relation='free',
                             draw=FALSE)),
          breaks=100, col='gray',
          xlab='',
          strip.names=c(TRUE, TRUE), bg='gray', fg='darkblue')

splom(dfTrans)

densityplot(dfTrans)

histogram(dfTrans)

xyplot(dfTrans)

hexbinplot(dfTrans)

dfTransCluster <- claraPD(dfTrans, noise.rm = FALSE)

xyplot(dfTransCluster)

xyplot(dfTransCluster, panelClust=FALSE)

histogram(dfTransCluster)

densityplot(dfTransCluster)
