#' Read bot files from Simrad EK60 echosounder
#' 
#' This function will read Simrad BOT0 datagrams with bottom depth data.
#' 
#' @param file EK60 bot file name.
#'
#' @details This function read the bot file by calling \code{read.EK_raw}, 
#' and \code{get_dgIdx} functions. From the datagram index, it finds the BOT0
#' datagrams, and then extracts ping times and depth(s) associated with 
#' all the transceiver(s) in the file.
#'
#' @return A data frame with ping times and detected bottom depth for 
#' every transceiver in the file.
#' 
#' @seealso \code{read.EK_idx}.
#'
#' @author Héctor Villalobos.   
#'
#' @examples
#' fn <- system.file("extdata", "demo-D20130504-T083828.bot", package = "echogram")
#' depth <- read.EK_bot(fn)
#' head(depth)
#' plot(depth[, 1:2], type = "l", ylim = rev(c(0, max(depth[,2]))))
#' 
read.EK_bot <- function(file){
  bot <- read.EK_raw(file)
  dgIdx <- get_dgIdx(bot)	
  idx <- dgIdx[dgIdx$dgType == "BOT0", ]
  
  # get number of transceivers from configuration
  ntr <-  readBin(bot[529:532], 'integer', 1, 4, signed	= TRUE, endian = "little")
  
  # there is one BOT0 datagram for every ping
  np <- nrow(idx)
 
  bottom <- matrix(NA, ncol = ntr, nrow = np)
  pingTime <- .POSIXct(double(np))
  
  # loop over the pings
  for (p in 1:np){
    pingTime[p] <-  dgTime(bot, ini = idx$sdgD[p])
    # in BOT0 datagrams, 8 bytes contain the pingtime, and the next 4 the transceiver count
    bottom[p, ] <- readBin(bot[(idx$sdgD[p] + 12):idx$edgD[p]], 'double', ntr, endian = "little")
  }
  ans <- data.frame(pingTime = pingTime, bottom)
  names(ans)[2:(2 + ntr - 1)] <- paste("depth.tr", 1:ntr, sep = "")
  ans
}  


