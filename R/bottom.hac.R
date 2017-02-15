bottom.hac <-
function( hac, channel = 1, plot = FALSE, maxDepth = NULL) {
  #require(readHAC)
  hacR <- hac
  if ( !"HAC" %in% class(hac) ) 
    hacR  <- readHAC::readHAC( hac )
  tuple.types <- unique(hacR$type) # available tuples
  pingtupt <- c(10000, 10010, 10030, 10040, 10050) 
  ptt <- pingtupt[which(pingtupt %in% tuple.types)]               
  pngTup <- hacR[hacR[["type"]] == ptt & hacR[["softwarechannel"]] == channel]
  pTl <- unique(pngTup$length)
  nm <- length(pTl)
  
  if ( nm < 3 ){
    ans <- list()
    for ( i in 1:nm ){
      pTup <- subset(pngTup, length == pTl[i])
      detbot <- -readHAC::parseHAC(pTup)$'Detected bottom range [m]'
      fracSec <- readHAC::parseHAC(pTup)$'Time fraction'
      pingTime <- readHAC::parseHAC(pTup)$'Time CPU ANSI'
      pingTime <- pingTime + fracSec
      pingTime <- as.POSIXlt(pingTime, tz="UTC", format="%Y-%m-%d", origin="1970-01-01 00:00:00")
      dbot <- data.frame(pingTime, detBottom = detbot)
      ans[[i]] <- dbot
    }
    ans <- do.call(rbind, ans)
  } else {
    x <- split(pngTup, pngTup$pointer)
    y <- lapply(x, readHAC::parseHAC)
    dbr <- lapply(y, function(x)x$"Detected bottom range [m]")
    detbot <- -unlist(dbr)
    tf <- lapply(y, function(x)x$"Time fraction") 
      fracSec <- unlist(tf)
    pT <- lapply(y, function(x)x$'Time CPU ANSI')
      pingTime <- unlist(pT)
      pingTime <- pingTime + fracSec
      pingTime <- as.POSIXlt(pingTime, tz="UTC", format="%Y-%m-%d", origin="1970-01-01 00:00:00")
      dbot <- data.frame(pingTime, detBottom = detbot)
    ans <- dbot
  }  
  if (plot == TRUE) {
    if ( missing(maxDepth) )
      maxDepth <- min(ans$detBottom, na.rm=TRUE)
    if (maxDepth < -500 )
      maxDepth <- -600
    plot(ans$pingTime, ans$detBottom, type = "l", xlab = "ping time", ylab = "detected bottom (m)", 
	  ylim = c(maxDepth, 0), col="red")
      abline(h=0, col="grey")
  }  
  ans
}
