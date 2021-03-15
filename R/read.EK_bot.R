# read.EK_bot() Read SIMRAD EK bot file with bottom Data (BOT0 datagram)
read.EK_bot <- function(file){
  bot <- read.EK_raw(file)
  dgIdx <- get_dgIdx(bot)	
  idx <- dgIdx[dgIdx$dgType == "BOT0", ]
  
  # number of transceivers
  ntr <-  readBin(bot[529:532], 'integer', 1, 4, signed	= TRUE, endian = "little")
  
  # number of pings
  np <- nrow(idx)
 
  bottom <- matrix(NA, ncol = ntr, nrow = np)
  pingTime <- .POSIXct(double(np))
  
  # 8 bytes four pingtime, and 4 for transceiver count
  for (p in 1:np){
    pingTime[p] <-  dgTime(bot, ini = idx$BiD[p])
    bottom[p, ] <- readBin(bot[(idx$BiD[p]+12):idx$BfD[p]], 'double', ntr, endian = "little")
  }
  ans <- data.frame(pingTime = pingTime, bottom)
  names(ans)[2:(2+ntr-1)] <- paste("detbot.tr", 1:ntr, sep="")
  ans
}  


