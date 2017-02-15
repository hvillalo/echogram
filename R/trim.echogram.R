trim.echogram <-
function( echogram, depth.max = NULL, ping.ini = 1, ping.end = NULL ){
  echo <- echogram
  if ( class(echo) != "echogram" ) 
    stop ("need object of class 'echogram'")
  vp <- echo$depth
  mSv <- echo$Sv
  frq <-   attr(mSv, "frequency")
  bot <- echo$pings
  if ( !missing(depth.max) ) {
    wm <- which.min(abs(vp - depth.max))
    if ( vp[wm] > 500 )
    wm <- wm - 1
    mSv <- mSv[1:wm, ]
    vp <- vp[1:wm]
  }	
  if ( !missing(ping.end) ) {
    mSv <- mSv[ , ping.ini:ping.end]
    bot <- bot[ping.ini:ping.end, ]
  }
  echo$depth <- vp
  attr(mSv, "frequency") <- frq
  echo$Sv <- mSv
  echo$pings <- bot 
  class(echo) <- "echogram"
  echo
}
