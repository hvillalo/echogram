read.echogram <-
function( hac, channel = 1 ) {
  #require(readHAC)
  hacR  <- readHAC::readHAC( hac )
  tuple.types <- unique(hacR$type) # available tuples
  echosTupt <- c(100, 200, 210, 901)
  channelTupt <- c(1000, 1001, 2000,  2001, 2002, 2100, 9001)
  pingtupt <- c(10000, 10010, 10030, 10040, 10050) 
  ett <- echosTupt[which(echosTupt %in% tuple.types)] 
  ctt <- channelTupt[which(channelTupt %in% tuple.types)] 
  ptt <- pingtupt[which(pingtupt %in% tuple.types)] 
  
  # subset tuples
  esTup <- hacR[hacR[["type"]] == ett] # echosounder tuple
  chanTup <- hacR[hacR[["type"]] == ctt & hacR[["softwarechannel"]] == channel] # channel tuple
  pngTup <- hacR[hacR[["type"]] == ptt & hacR[["softwarechannel"]] == channel] # ping tuple
  
  # find out if more than one matrix is present in ping tuple
  pTl <- unique(pngTup$length)
  nm <- length(pTl)
  
  if ( nm < 3){
    for ( i in 1:nm ) {
      pTup <- subset(pngTup, length == pTl[i])
    
      # Get Sample value data
      Sv <- readHAC::parseHAC(pTup)$"Sample value"
      Sv[Sv > 0] <- NA  # discard positive dB values
      if ( ptt == 10030 )
        Sv <- Sv * 10 # units are not parsed correctly, need to multiply by 10 ### NOT ALWAYS ###
      if ( class(Sv) != "matrix" )
        Sv <- matrix(Sv, ncol=1)
      assign(paste("Sv", i, sep="."), Sv)
    }
    Sv <- get("Sv.1")
    if ( nm > 1)
    for ( m in 2:nm ){
      Sv <- mergeSvmat(Sv, get(paste("Sv", m, sep=".")))  
    } 
  } else {
     x <- split(pngTup, pngTup$pointer)
     y <- lapply(x, readHAC::parseHAC)
     z <- lapply(y, function(x) x$"Sample value") 
     zz <- lapply(z, function(x) matrix(x, ncol=1) ) 
  
     Sv <- zz[[1]]
     for (g in 2:length(zz))
      Sv <- mergeSvmat(Sv, zz[[g]])
  }
  frq <- readHAC::parseHAC(chanTup)$"Acoustic frequency"/1000
  attr(Sv, "frequency") <- paste(frq, "kHz")
  
  # Calculate sample length
  tsi <- readHAC::parseHAC(chanTup)$"Time sample interval"
  ss <- readHAC::parseHAC(esTup)$"Sound speed"
  dim1 <- dim(Sv)[1] # depth vector length
  if ( is.null(tsi) ) {
    sLen <- readHAC::parseHAC(chanTup)$"Sampling interval"
  } else sLen <- (ss * tsi)/2 # sample length in meters
  vLen <- c(sLen/2, rep(sLen, dim1-1))
  depth <- cumsum(vLen)  
  
  # Info by ping: detected bottom, ping time, ...
  bot <- bottom.hac( hacR, channel ) # detected bottom
  pos <- position.hac( hacR ) # GPS position data
    pos <- navigation.hac(pos) # bearing, distance and speed between gps positions
  np <- nrow(bot) # number of pings
  dbot <- data.frame(bot, speed = NA, cumdist = NA)
  
  # Find speed between pings
  if (nrow(pos) > 1){
    for (k in 1:np){
      p <- bot$pingTime[k]
      tdif <- abs(difftime(pos$time.cpu, p, units="secs"))
      idx <- which.min(tdif)
      dbot[k, 'speed'] <- pos$navspeed[idx]
    }
  
  tdif <- c(0, difftime(dbot[2:np, 'pingTime'], dbot[1:(np-1), 'pingTime'], units = 'hours'))
  dist <- dbot$speed * tdif
  dbot$cumdist <- cumsum(dist)
  }
  ans <- list(depth = depth, Sv = Sv, pings = dbot)
  class(ans) <- "echogram"
  ans
}
