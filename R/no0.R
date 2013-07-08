no0 <- function(x, threshold=8e-3){
  len=length(x)
  mx=max(abs(x))##máximo valor de la señal
  thr=mx*threshold##umbral para considerar ruido
  xrle <- rle(abs(x)>thr)##tramos en los que la señal supera el umbral
  xrle.len <- xrle$lengths##longitud de cada tramo
  xrle.value <- xrle$values##valores en cada tramo
  n=length(xrle.value)##cuantos tramos

  hd <- 1:xrle.len[1]##cabecera de la señal
  tl <- (len-xrle.len[n]):len##cola de la señal
  
  if(!xrle.value[1] & !xrle.value[n]) {
    return(x[-c(hd, tl)])##elimino cabeza y cola porque no superan umbral
    } else if(!xrle.value[1] & xrle.value[n]) {
      return(x[-hd])##solo elimino cabeza, porque la cola sí supera el umbral
      } else if(xrle.value[1] & !xrle.value[n]) {
        return(x[-tl])##elimino cola, porque la cabeza si supera el umbral
        } else return(x)##entrego la señal sin modificar porque el comienzo y final están por encima del umbral
  }


