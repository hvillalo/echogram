#' Get datagram lengths (dgLen), types  (dgType), and indices from imported 
#' EK* raw files.
#'
#' The first 4 Bytes before every datagram contain the dgLen, which is repeated
#' in the 4 Bytes at the end, so 4 + dgLen + 4 (+ 1 for skipping to the next).  
#' Considering only data in dgLen, the first 4 Bytes are the dgType, and the 
#' next 8 contain the time (4 Bytes for lowDT and 4 Bytes for highDT).
#' 
#' @param raw An imported EK* raw file or a raw file name
#' 
#' @return A table with datagram types and lengths, and most important, the 
#'         indices for extracting the data
#' 
#' @examples
#'  \dontrun{
#'  dgIdx <- dgIndices(ekraw)
#'  }
#'  
get_dgIdx <- function(raw){
  if (!inherits(raw, "raw"))
    raw <- read.EK_raw(raw)
  fsize <- length(raw)
  
  # initial (BiL) and final (BfL) Bytes with datagram length 
  BiL <- i <- 1 
  BfL <- k <- 4
  dgLen <- dgL <- readBin(raw[i:k], 'integer', size = 4) 
  dgType <- dgT <- rawToChar(raw[(i + 4):(k + 4)]) 

  while (k < fsize){
    i <- k + dgL + 5 
    k <- i + 3 
    dgL <- readBin(raw[i:k], 'integer', size = 4) 
    if (dgL == 0)
      break()
    BiL <- c(BiL, i) 
    BfL <- c(BfL, k) 
    dgLen <- c(dgLen, dgL)
    dgT <- rawToChar(raw[(i+4):(k+4)])
    dgType <- c(dgType, dgT)
  }

  # initial and final Bytes with datagram type 
  BiT <- BfL + 1
  BfT <- BiT + 3
  # initial and final Bytes with datagram data  
  BiD <- BiL + 8
  BfD <- BiD + dgLen - 5

  # Datagram index table
  dgIdx <- data.frame(BiL, BfL, dgLen, BiT, BfT, dgType, BiD, BfD)
  dgIdx
}