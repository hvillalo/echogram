mask.echogram <-
function(echogram, surf.off = NULL, bott.off = NULL, mask = TRUE){
  echo <- echogram
  if ( class(echo) != "echogram" ) 
    stop ("need object of class 'echogram'")
  vp <- echo$depth
  dbot <- echo$pings
  vt <- dbot$pingTime
  detB <- dbot$detBottom
  frq <-   attr(echo$Sv, "frequency")  
  dim.Sv <- dim(echo$Sv) 
    nr <- dim.Sv[1]
    nc <- dim.Sv[2]
  idxP <- seq(0, 1, len=nr)
  idxT <- seq(0, 1, len=nc)
  mBX <- matrix(NA, nrow=nr, ncol=nc)
  if ( !missing(bott.off) )
    detB <- detB + bott.off
  for( k in 1:nc){
    wm <- which.min(abs(vp + detB[k]))
    mBX[1:(wm-1), k] <- 1
  }
  if ( !missing(surf.off) ){
    wm <- which.min(abs(vp - surf.off))
    mBX[1:wm, ] <- NA
  }
  ans <- mBX
  if ( mask == TRUE ){
    Sv <- echo$Sv * mBX
	attr(Sv, "frequency") <- frq
	echo$Sv <- Sv
	ans <- echo
	class(ans) <- "echogram"
  }
  ans
}
