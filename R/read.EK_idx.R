#' Read idx files from Simrad EK* echosounders 
#' 
#' This function will read Simrad IDX0 datagrams with ping data.
#' 
#' @param file EK* idx file name.
#'
#' @details This function calls \code{read.EK_raw}, and \code{get_dgIdx} functions
#' to read the idx file, find the IDX0 datagrams, and then extracts ping information.
#'
#' @return A data frame with ping data: ping number, vessel distance, latitude, 
#' longitude, and fileoffset.
#' 
#' @seealso \code{read.EK_bot}.
#'
#' @author Héctor Villalobos.  
#'
#' @examples
#' fn <- system.file("extdata", "demo-D20130504-T083828.idx", package = "echogram")
#' pings <- read.EK_idx(fn)
#' head(pings)
#' plot(pings$lon, pings$lat)
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
    pingTime[p] <-  dgTime(idx0, ini = idx$sdgD[p])
    # skip the 8 bytes with ping time to find ping number
	pings$pingNumber[p] <- readBin(idx0[(idx$sdgD[p] + 8):(idx$sdgD[p] + 11)], 'integer', 1, 4, endian = "little") 
    # the next 24 bytes are vessel distance (8 bytes), latitude (8 bytes), and longitude (8 bytes)
	pings[p, 2:4] <- readBin(idx0[(idx$sdgD[p] + 12):(idx$sdgD[p] + 35)], 'double', 3, 8, endian = "little")
	# finally, the last 8 bytes contain the offset of every ping... Have to see if this is useful.
    pings$offset[p] <- readBin(idx0[(idx$sdgD[p] + 36):idx$edgD[p]], 'integer', 1, 8, endian = "little") 
  }
  ans <- data.frame(pingTime = pingTime, pings)
  # in some pings, lat and lon are being wrongly set to 0.
  ans[ans$lon == 0, c('lat', 'lon')] <- NA
  ans
}
