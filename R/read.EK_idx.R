# read.EK_idx() Read SIMRAD EK idx file with index data (IDX0 datagram)
read.EK_idx <- function(file){
  idx0 <- read.EK_raw(file)
  dgIdx <- get_dgIdx(idx0)	
  idx <- dgIdx[dgIdx$dgType == "IDX0", ]
  
  # number of pings
  np <- nrow(idx)
  
  pingTime <- .POSIXct(double(np))
  pings <- data.frame(pingNumber = rep(NA, np), vesselDistance = NA, lat = NA, lon = NA, offset = NA)
  
  for (p in 1:np){
    pingTime[p] <-  dgTime(idx0, ini = idx$BiD[p])
    pings$pingNumber[p] <- readBin(idx0[(idx$BiD[p]+8):(idx$BiD[p]+11)], 'integer', 1, 4, endian = "little") # ping number
    pings[p, 2:4] <- readBin(idx0[(idx$BiD[p]+12):(idx$BiD[p]+35)], 'double', 3, 8, endian = "little")
    pings$offset[p] <- readBin(idx0[(idx$BiD[p]+36):idx$BfD[p]], 'integer', 1, 8, endian = "little") # offset
  }
  ans <- data.frame(pingTime = pingTime, pings)
  ans[ans$lon == 0, c('lat', 'lon')] <- NA
  ans
}
