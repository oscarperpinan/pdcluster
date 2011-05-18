hankel <- function(x, ncol=length(x)%/%2){
  n=length(x)
  nrow=n-ncol
  A=matrix(0, nrow=nrow, ncol=ncol)
  idx=col(A)+row(A)-1
  M <- matrix(x[idx], nrow=nrow, ncol=ncol)
  M
  }

## http://www.ele.uri.edu/~gopi/report.pdf
prony <- function(x, M=6, dt=1e-8, clean=TRUE){
  if (clean) {
    x <- no0(x)          ##Elimino ceros iniciales y finales
    idx=which(x==max(x)) ##Sólo utilizo la señal desde el primer máximo
    x=x[idx:length(x)]
  }
  time=seq_along(x)*dt
  N=length(x)
  A=hankel(x, ncol=M)
  A=A[,ncol(A):1] ##reordeno las columnas
  A2=t(A)%*%A ##Covarianza del proceso, pero cuidado: NO es estacionario. 
  h=-x[(M+1):N]
  h2=t(A)%*%h
  ##La solución de este sistema son los coeficientes de las exponenciales complejas
  b=solve(A2, h2)
  ##Las exponenciales complejas son las raices de un polinomio con los anteriores coeficientes
  rts=polyroot(c(1,b)) 
  ##Extraigo el número complejo de la exponencial
  s = log(rts); 
  ##Divido entre el tiempo de muestreo para relacionar lo discreto con lo continuo
  dampings = -Re(s)/dt;
  frequencies = Im(s)/(2*pi)/dt;
  S=dampings + 2i*pi*frequencies
  ##Matriz con la evolución temporal de las cuatro señales reconstruidas
  Comp=exp(outer(time, S, '*')) 
  ##Desfase
  a=qr.solve(Comp, x) ##Resuelvo un sistema sobredeterminado
  ##Matriz con la evolución temporal de las señales con sus correspondientes amplitudes complejas
  CompFase=t(a*t(Comp)) ##OJO: probar crossprod 
  ##La señal reconstruida es la suma en cada instante de estas cuatro señales
  xsim=Re(rowSums(CompFase))
  ##Sólo muestro los resultados para las frecuencias positivas. 
  pos=(frequencies>1)
  ##energía transportada por cada complejo
  energy=colSums(Re(CompFase[,pos])^2, na.rm=1)
  ordEnergy=order(energy, decreasing=TRUE)
  stopifnot(sum(energy)>0)

  comp <- as.data.frame(2*Re(CompFase[,pos]))
  comp <- comp[,ordEnergy]##order columns by energy
  names(comp) <- paste('C', seq_along(ordEnergy), sep='')
  
  comp <- stack(comp)
  names(comp) <- c('value', 'wave')
  comp$time <- rep(time, sum(pos))
  ##Main result
  signal=data.frame(orig=x, prony=xsim, time=time)

  

  damp=dampings[pos][ordEnergy]
 
  freq=frequencies[pos][ordEnergy]
   
  result <- list(damp=damp, freq=freq,
                 energy=energy[ordEnergy]/sum(energy),
                 comp=comp,
                 signal=signal)
  class(result) <- 'prony'
  result
}

## drop.levels <- function(dat){
##   # Drop unused factor levels from all factors in a data.frame
##   # Author: Kevin Wright.  Idea by Brian Ripley.
##   dat[] <- lapply(dat, function(x) x[,drop=TRUE])
##   return(dat)
## }

xyplot.prony <- function(x, all=FALSE,...){
  
  ##dat=droplevels(subset(x$comp, wave %in% c('x', 'xsim')))
  dat <- x$signal
  p <- xyplot(prony+orig~time, data=dat, type='l', ylab='', lwd=1.2,
              par.settings=custom.theme(symbol=c("#000000", "#BDBDBD")),
              auto.key=list(corner=c(1,0), lines=TRUE, points=FALSE))

  if (all) {
    dat2 <- x$comp
    ## idx <- as.numeric(tapply(pr$comp$value, pr$comp$wave,
    ##                          FUN=function(x)which.max(abs(x))))
    p2 <- xyplot(value~time, groups=wave, data=dat2, type='l',
                 par.settings=custom.theme.2(), ylab='')
    ## p2 <- p2 + glayer({
    ##   idx <- which.max(abs(y))
    ##   panel.text(x[idx], y[idx], label=group.number, col=col.line, pos=3, cex=0.8)
    ## })
  
    p <- p + p2
  }
  p
}

  compProny <- function(x, dt=1e-8, M=seq(4, 20, 4), clean=TRUE){
    if (clean) {
      x <- no0(x)        ##Elimino ceros iniciales y finales
      idx=which(x==max(x)) ##Sólo utilizo la señal desde el primer máximo
      x=x[idx:length(x)]
    }
    df1 <- data.frame(value=x, wave='x', time=seq_along(x)*dt)
    foo <- function(M, x, ...){
      pr <- prony(x=x, dt=dt, M=M, clean=FALSE) ##no hace falta volver a usar CLEAN
      ## xsim <- droplevels(subset(pr$comp, wave=='xsim'))
      ## xsim$wave=M                       #paste('M', M, sep='')
      ## xsim
      xsim <- data.frame(value=pr$signal$prony, time=pr$signal$time, wave=M)
    }
    df2 <- lapply(M, foo, x=x, dt=dt)
    df2 <- do.call('rbind', df2)
    p <- xyplot(value~time, groups=wave, data=df2, type='l',
                par.settings=custom.theme.2(), ylab='',
                auto.key=list(corner=c(1,0), lines=TRUE, points=FALSE))
    p + layer(panel.xyplot(time, value, type='l', col.line='black', lwd=2), data=df1)
  }

