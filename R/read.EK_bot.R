#' Read EK* bot files from Simrad echosounders 
#' 
#' This function will read Simrad BOT0 datagrams with bottom depth data.
#' 
#' @param file EK* bot file name
#'
#' @details This function calls \code{read.EK_raw}, and \code{get_dgIdx} functions
#' to read the bot file, find the BOT0 datagrams, and then extract ping times and
#' depth(s) associated with the transceiver(s) in the file.
#'
#' @return A data frame with ping times and depth data for every transceiver in 
#' the file.
#'
#' @author Héctor Villalobos   
#'
#' @example
#' \dontrun{
#'  depth <- read.EK_bot("D20140510-T102540.bot")
#' }
#'
#'
read.EK_bot <- function(file){
  bot <- read.EK_raw(file)
  dgIdx <- get_dgIdx(bot)	
  idx <- dgIdx[dgIdx$dgType == "BOT0", ]
  
  # get number of transceivers from configuration
  ntr <-  readBin(bot[529:532], 'integer', 1, 4, signed	= TRUE, endian = "little")
  
  # we have one BOT0 datagram for every ping
  np <- nrow(idx)
 
  bottom <- matrix(NA, ncol = ntr, nrow = np)
  pingTime <- .POSIXct(double(np))
  
  # loop over the pings
  for (p in 1:np){
    pingTime[p] <-  dgTime(bot, ini = idx$BiD[p])
	# in BOT0 datagrams, 8 bytes contain the pingtime, and the next 4 the transceiver count
    bottom[p, ] <- readBin(bot[(idx$BiD[p] + 12):idx$BfD[p]], 'double', ntr, endian = "little")
  }
  ans <- data.frame(pingTime = pingTime, bottom)
  names(ans)[2:(2 + ntr - 1)] <- paste("depth.tr", 1:ntr, sep = "")
  ans
}  


