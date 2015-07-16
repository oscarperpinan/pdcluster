setGeneric('claraPD', function(object, ...){standardGeneric('claraPD')})

setMethod('claraPD',
          signature=(object='PD'),
          definition <- function(object,
                                 vars, useAngle = TRUE,
                                 nSims=25, nCl=7, metric='manhattan',
                                 noise.level=0.7, noise.rm=TRUE,
                                 seed=421){
    dt <- object@data
    ##Include angle as another feature for clara
    if (isTRUE(useAngle)) dt$angle <- object@angle 
    ## Preparo matrices
    set.seed(seed)              # (reproducibility)
    cl <- matrix(NA, nrow(dt), nSims)
    medoids <- array(NA, c(nCl, ncol(dt), nSims))
    ## Ejecuto simulaciones de CLARA
    for(i in 1:nSims){
        claraSim <- clara(dt, nCl,
                          stand=TRUE, metric=metric,
                          medoids.x = TRUE, rngR = TRUE)
        cl[,i] <- claraSim$cluster
        medoids[,,i] <- as.matrix(dt[claraSim$i.med,])
    }
    ## En base a todas las simulaciones establezco las agrupaciones finales
    ## y decido las muestras que son "ruido"
    tcl <- apply(cl, 1, tabulate, nbins = nCl)
    timesCl <- t(tcl)/nSims
    noise <- apply(timesCl, 1, function(x)!any(x>noise.level))
    fuzzyCl <- apply(timesCl, 1, which.max)
    ## Calculo los centroides promedio
    ##y las distancias asociadas para cada cluster
    meanMedoids <- apply(medoids, c(1, 2), mean)
    distancia <- function(x, y){sqrt(sum((x-y)^2))}
    distFoo <- function(i, data){
        x1 <- apply(data[fuzzyCl==i,], 1, FUN=distancia, y=meanMedoids[i,])
    }
    distMedoid <- do.call(c, lapply(1:nCl, distFoo, dt))
    distRel <- ave(distMedoid, fuzzyCl,
                   FUN=function(x)1-x/max(x)) ##distancia relativa por cluster
    distFactor <- ave(distRel,  fuzzyCl,
                      FUN = function(x){
                          brks <- quantile(x, seq(0.1, 1, 0.1))
                          cut(x, unique(brks))
                      })
    distDF <- data.frame(dist=distMedoid,
                         distRel=distRel,
                         distFactor=distFactor)
    ##Entrego resultados
    if (!('energy' %in% names(dt))) ##energy must be in the output
            dt$energy <- object@data$energy
    
    if(noise.rm){
        object@angle <- object@angle[!noise]
        object@refl <- object@refl[!noise]
        dt <- dt[!noise,]
        fuzzyCl <- fuzzyCl[!noise]
        distDF <- distDF[!noise,]
    }
    object@data <- dt
    result <- new(Class='PDCluster',
                  object, ##PDCluster contains a PD object
                  cluster=fuzzyCl,
                  nSims=nSims,
                  nClusters=nCl,
                  metric=metric,
                  noise.level=noise.level,
                  noise.rm=noise.rm,
                  dist=distDF)
    result
}
)
