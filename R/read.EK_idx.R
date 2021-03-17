#' Read EK* idx files from Simrad echosounders 
#' 
#' This function will read Simrad IDX0 datagrams with ping data.
#' 
#' @param file EK* idx file name
#'
#' @details This function calls \code{read.EK_raw}, and \code{get_dgIdx} functions
#' to read the idx file, find the IDX0 datagrams, and then extract ping information:
#' ping number, vessel distance, latitude, longitude and fileoffset.
#'
#' @return A data frame with the above ping information plus ping times.
#'
#' @author Héctor Villalobos   
#'
#' @examples
#' fn <- system.file("extdata", "demo-D20130504-T083828.idx", package = "echogram")
#' pings <- read.EK_idx(fn)
#' head(pings)
#'
read.EK_idx <- function(file){
  idx0 <- read.EK_raw(file)
  dgIdx <- get_dgIdx(idx0)	
  idx <- dgIdx[dgIdx$dgType == "IDX0", ]
  
  # number of pings
  np <- nrow(idx)
  
  pings <- data.frame(pingNumber = rep(NA, np), vesselDistance = NA, lat = NA, lon = NA, offset = NA)
  pingTime <- .POSIXct(double(np))
  
  # loop over the pings
  for (p in 1:np){
    pingTime[p] <-  dgTime(idx0, ini = idx$BiD[p])
    # skip the 8 bytes with ping time
	pings$pingNumber[p] <- readBin(idx0[(idx$BiD[p] + 8):(idx$BiD[p] + 11)], 'integer', 1, 4, endian = "little") 
    pings[p, 2:4] <- readBin(idx0[(idx$BiD[p]+12):(idx$BiD[p]+35)], 'double', 3, 8, endian = "little")
    pings$offset[p] <- readBin(idx0[(idx$BiD[p]+36):idx$BfD[p]], 'integer', 1, 8, endian = "little") 
  }
  ans <- data.frame(pingTime = pingTime, pings)
  ans[ans$lon == 0, c('lat', 'lon')] <- NA
  ans
}
