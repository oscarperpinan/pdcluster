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
  stopifnot(sum(energy)>0)
  comp <- as.data.frame(2*Re(CompFase[,pos]))
  comp$x=x
  comp$xsim=xsim
  comp <- stack(comp)
  names(comp) <- c('value', 'wave')
  comp$time <- rep(time, sum(pos)+2)
  ##maxEnergy=which(max(energy)==energy)
  ordEnergy=order(energy, decreasing=TRUE)

  damp=dampings[pos][ordEnergy]
  ##names(damp) <- paste('damp', seq_along(damp), sep='')

  freq=frequencies[pos][ordEnergy]
  ##names(freq) <- paste('freq', seq_along(freq), sep='')
  
  result <- list(damp=damp, freq=freq,
                 energy=energy[ordEnergy]/sum(energy),
                 comp=comp)
  class(result) <- 'prony'
  result
}

## drop.levels <- function(dat){
##   # Drop unused factor levels from all factors in a data.frame
##   # Author: Kevin Wright.  Idea by Brian Ripley.
##   dat[] <- lapply(dat, function(x) x[,drop=TRUE])
##   return(dat)
## }

plot.prony <- function(x, all=TRUE,...){
  if (all){
    dat=x$comp
  } else {
    dat=droplevels(subset(x$comp, wave %in% c('x', 'xsim')))
  }
  xyplot(value~time, groups=wave, data=dat, type=c('l', 'g'), 
         ylab='', auto.key=list(corner=c(1,0), lines=TRUE, points=FALSE))

}

compProny <- function(x, dt=1e-8, M=seq(4, 20, 4), clean=TRUE){
  if (clean) {
    x <- no0(x)          ##Elimino ceros iniciales y finales
    idx=which(x==max(x)) ##Sólo utilizo la señal desde el primer máximo
    x=x[idx:length(x)]
  }
  df1 <- data.frame(value=x, wave='x', time=seq_along(x)*dt)
  foo <- function(M, x, ...){
    pr <- prony(x=x, dt=dt, M=M, clean=FALSE)##no hace falta volver a usar CLEAN
    xsim <- droplevels(subset(pr$comp, wave=='xsim'))
    xsim$wave=M#paste('M', M, sep='')
    xsim
  }
  df2 <- lapply(M, foo, x=x, dt=dt)
  df2 <- do.call('rbind', df2)
  p <- xyplot(value~time, groups=wave, data=df2, type='l',
              par.settings=custom.theme.2(), ylab='',
              auto.key=list(corner=c(1,0), lines=TRUE, points=FALSE))
  p + layer(panel.xyplot(time, value, type='l', col.line='black', lwd=2), data=df1)
  }


##EXAMPLE
