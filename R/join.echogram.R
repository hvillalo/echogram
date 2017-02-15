join.echogram <-
function(echogram1, echogram2){
  echo1 <- echogram1
  echo2 <- echogram2
  if ( class(echo1) != "echogram" & class(echo2) != "echogram" ) 
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
  
  pt1 <- echo1$pings$pingTime
  db1 <- echo1$pings$detBottom
  sp1 <- echo1$pings$speed
  
  pt2 <- echo2$pings$pingTime
  db2 <- echo2$pings$detBottom
  sp2 <- echo2$pings$speed
  
  dbot <- data.frame(pingTime=c(pt1, pt2), detBottom=c(db1, db2), speed=c(sp1, sp2))
  np <- nrow(dbot)
  
  tdif <- c(0, difftime(dbot[2:np, 'pingTime'], dbot[1:(np-1), 'pingTime'], units = 'hours'))
  dist <- dbot$speed * tdif
  dbot$cumdist <- cumsum(dist)
  ans <- list(depth = depth, Sv = m3, pings = dbot)
  class(ans) <- "echogram"  
  ans
}
