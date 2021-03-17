#' Get NME0 datagrams from imported EK* raw files.
#'
#' Read the NMEA sentences stored in NME0 datagrams.
#'
#' @param raw An imported EK* raw file or a raw file name
#' 
#' @details For parsing NMEA sentences, \code{parse.nmea()} function
#' should be used. 
#'
#' @return A data frame with CPU time and corresponding NMEA data
#'
#' @seealso \code{parse.nmea} 
#' 
#' @author Héctor Villalobos 
#'
#' @examples
#' fn <- system.file("extdata", "demo-D20130504-T083828.raw", package = "echogram")
#' nmea <- get_NME0(fn)
#' head(nmea)
#'  
get_NME0 <- function(raw){
  if (!inherits(raw, "raw"))
    raw <- read.EK_raw(raw)
  dgIdx <- get_dgIdx(raw)	
  idx <- dgIdx$dgType == "NME0"
  if (sum(idx) > 0){
    idx <- dgIdx[idx, ]
    nmea <- rawToChar(raw[(idx$BiD[1] + 8):idx$BfD[1]])
    dgtime <- dgTime(raw, ini = idx$BiD[1])
    for (k in 2:nrow(idx)){
      nm <- rawToChar(raw[(idx$BiD[k] + 8):idx$BfD[k]])
      nmea <- c(nmea, nm)
      dgT <-  dgTime(raw, ini = idx$BiD[k])
      dgtime <- c(dgtime, dgT)
    }
    nmea <- data.frame(dgTime = dgtime, nmea = nmea)
  } else {
     stop("No NMEA data have been found in raw data")
  }  
  nmea
}
