match.echogram <-
function(echogram1, echogram2){
  echo1 <- echogram1
  echo2 <- echogram2
  if ( class(echo1) != "echogram" & class(echo2) != "echogram" ) 
    stop ("need objects of class 'echogram'")
	
  Sv1 <- echo1$Sv
    frq1 <- attr(Sv1, "frequency")
  Sv2 <- echo2$Sv
    frq2 <- attr(Sv2, "frequency")
  bot1 <- echo1$pings
  bot2 <- echo2$pings
  pt1 <- bot1$pingTime
  pt2 <- bot2$pingTime
  
  # First, need to find unmatched pings in both sets and in both directions)
  idx <- which(!pt1 %in% pt2) 
  if (length(idx) > 0){
    pt1 <- pt1[-idx]
    bot1 <- bot1[-idx, ]
    Sv1 <- Sv1[ , -idx]
  }
  
  idx <- which(!pt2 %in% pt1)
  if (length(idx) > 0){
    pt2 <- pt2[-idx]
    bot2 <- bot2[-idx, ]
    Sv2 <- Sv2[ , -idx]
  }
  
  # Next, see if there are duplicates
  Dupl1 <- as.POSIXct(names(which( (table(pt1) / table(pt2)) > 1 )), tz="UTC")
  Dupl2 <- as.POSIXct(names(which( (table(pt2) / table(pt1)) > 1 )), tz="UTC")
  
  if ( length(Dupl1) > 0){
    idx2 <- which(pt1 %in% Dupl1)
    pt1 <- pt1[-idx2]
    bot1 <- bot1[-idx2, ]
    Sv1 <- Sv1[ , -idx2]
  }
  
  if ( length(Dupl2) > 0){
    idx2 <- which(pt2 %in% Dupl2)
    pt2 <- pt2[-idx2]
    bot2 <- bot2[-idx2, ]
    Sv2 <- Sv2[ , -idx2]
  }

  idx <- which(!pt1 %in% pt2) 
  if (length(idx) > 0){
    pt1 <- pt1[-idx]
    bot1 <- bot1[-idx, ]
   Sv1 <- Sv1[ , -idx]
  }
  
  idx <- which(!pt2 %in% pt1)
  if (length(idx) > 0){
    pt2 <- pt2[-idx]
    bot2 <- bot2[-idx, ]
    Sv2 <- Sv2[ , -idx]
  }
  attr(Sv1, "frequency") <- frq1
  attr(Sv2, "frequency") <- frq2
  echo1$Sv <- Sv1
  echo1$pings <- bot1
  echo2$Sv <- Sv2
  echo2$pings <- bot2
  class(echo1) <- "echogram"   
  class(echo2) <- "echogram"   
  ans <- list(echogram1 = echo1, echogram2 = echo2)
  ans
}
