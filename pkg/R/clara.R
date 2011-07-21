setGeneric('claraPD', function(object, ...){standardGeneric('claraPD')})

setMethod('claraPD',
          signature=(object='PD'),
          definition <- function(object,
                                 vars,
                                 nSims=25, nCl=7, metric='manhattan',
                                 noise.level=0.7, noise.rm=TRUE,
                                 seed=421){
            if (missing(vars)){
              dt <- object@data
              notInVars <- FALSE
            } else {
              dt <- object@data[vars]
              if (!('energy' %in% vars)){
                energy <- object@data$energy
                notInVars <- TRUE
              } else {
                notInVars <- FALSE
                }
            }
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
                              FUN=function(x)cut(x, quantile(x, seq(0.1, 1, .1))))
            distDF <- data.frame(dist=distMedoid,
                                 distRel=distRel,
                                 distFactor=distFactor)
            ##Entrego resultados
            if (notInVars) dt$energy <- energy

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
