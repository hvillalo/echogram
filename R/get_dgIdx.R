#' Get datagram indices from EK* raw files
#'
#' Find the datagram types and lengths, and their respective indices in EK* raw files.
#' 
#' @param raw A raw vector imported via \code{read.EK_raw} or an EK* raw file name.
#' 
#' @details In EK* raw files, the first 4 bytes before every datagram contain its
#' length (dgLen), which is repeated after the datagram data and before the length 
#' of the next datagram. So, for skipping from one datagram to the next, it takes
#' 4 + dgLen + 4 + 1.  
#' Within each datagram, the first 4 bytes give its name (dgType: CON0, NME0, RAW0,
#' etc.), followed by the time (next 8 bytes), and then the datagram data 
#' according to its type. 
#' The output of this fuction is key for extracting configuration data (header and 
#' transceiver information), nmea sentences, and acoustic raw data (received power 
#' and angles).
#'
#' @return A data frame with datagram types (CON0, NME0, RAW0, etc.) and lengths, 
#'         and most importantly, the indices where each datagram is located.
#' 
#' @author Héctor Villalobos.  
#' 
#' @examples
#' if(interactive()){
#' dgIdx <- get_dgIdx("D20130504-T083828.raw")
#' head(dgIdx)
#' } 
get_dgIdx <- function(raw){
  if (!inherits(raw, "raw"))
    raw <- read.EK_raw(raw)
  fsize <- length(raw)
  
  # starting (sdgL) and ending (edgL) bytes with datagram length 
  sdgL <- i <- 1 
  edgL <- k <- 4
  dgLen <- dgL <- readBin(raw[i:k], 'integer', size = 4) 
  dgType <- dgT <- rawToChar(raw[(i + 4):(k + 4)]) 

  while (k < fsize){
    i <- k + dgL + 5 
    k <- i + 3 
    dgL <- readBin(raw[i:k], 'integer', size = 4) 
    if (dgL == 0)
      break()
    sdgL <- c(sdgL, i) 
    edgL <- c(edgL, k) 
    dgLen <- c(dgLen, dgL)
    dgT <- rawToChar(raw[(i + 4):(k + 4)])
    dgType <- c(dgType, dgT)
  }

  # starting (sdgT) and ending (edgT) bytes with datagram type 
  sdgT <- edgL + 1
  edgT <- sdgT + 3
  
  # starting (sdgD) and ending (edgD) bytes with datagram data  
  sdgD <- sdgL + 8
  edgD <- sdgD + dgLen - 5

  # Datagram index table
  dgIdx <- data.frame(sdgL, edgL, dgLen, sdgT, edgT, dgType, sdgD, edgD)
  dgIdx
}