join.echogram <-
function(echogram1, echogram2){
  echo1 <- echogram1
  echo2 <- echogram2
  #any(unlist(lapply(list(eco, ECO, idx), FUN = function(x) !inherits(x, "echogram"))))
  if ( !inherits(echo1, "echogram") & !inherits(echo2, "echogram") ) 
    stop ("need objects of class 'echogram'")
  
  m1 <- echo1$Sv
  frq1 <- attr(m1, "frequency")
  m2 <- echo2$Sv
  frq2 <- attr(m2, "frequency")
  if ( frq1 != frq2 )
    stop("you are attempting to merge two echograms of different frequencies")
  d1 <- echo1$depth
  d2 <- echo2$depth
  if(length(d1) > length(d2))
    depth <- d1
  else 
    depth <- d2
  m3 <- mergeSvmat(m1, m2)
  attr(m3, "frequency") <- frq1
  
  pings <- rbind(echo1$pings, echo2$pings)
  np <- nrow(pings)
  
  tdif <- c(0, difftime(pings[2:np, 'pingTime'], pings[1:(np-1), 'pingTime'], units = 'hours'))
  dist <- pings$speed * tdif
  pings$cumdist <- cumsum(dist)
  ans <- list(depth = depth, Sv = m3, pings = pings)
  class(ans) <- "echogram"  
  ans
}
