#' Get datagram indices from EK* raw files
#'
#' Find the datagram types and lengths, and their respective indices in EK* raw files
#' 
#' @param raw A raw vector imported via \code{read.EK_raw} or an EK* raw file name
#' 
#' @details In EK* raw files, the first 4 bytes before every datagram contain its
#' length (dgLen), which is repeated after the datagram data, before the length 
#' of the next datagram. So, for skipping from one datagram to the next, it takes
#' 4 + dgLen + 4 + 1.  
#' Within each datagram, the first 4 bytes give its name (dgType: CON0, NME0, RAW0, etc.),
#' followed by the time in the next 8 bytes, and then the datagram data according to its type. 
#' The output of this fuction is key for extracting configuration data (header and 
#' transceiver info), nmea sentences, and acoustic raw data (received power and angles).
#'
#' @return A data frame with datagram types (CON0, NME0, RAW0, etc.) and lengths, 
#'         and most important, the indices for extracting each datagram.
#' 
#' @author Héctor Villalobos  
#' 
#' @examples
#' fn <- system.file("extdata", "demo-D20130504-T083828.raw", package = "echogram")
#' dgIdx <- get_dgIdx(fn)
#' head(dgIdx)
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