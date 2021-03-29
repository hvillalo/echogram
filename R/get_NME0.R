#' Get NME0 datagrams from imported EK* raw files
#'
#' Read the NMEA sentences stored in NME0 datagrams.
#'
#' @param raw A raw vector imported via \code{read.EK_raw} or an EK* raw file name.
#' 
#' @details The GPS data stored in the output of this function needs to be parsed
#' to be useful. This is done with \code{parse.nmea()} function. 
#'
#' @return A data frame with CPU time and corresponding NMEA sentences
#' as text strings.
#'
#' @seealso \code{parse.nmea} 
#' 
#' @author Héctor Villalobos. 
#'
#' @examples
#' if(interactive()){
#' nmea <- get_NME0("D20130504-T083828.raw")
#' head(nmea)
#' } 
get_NME0 <- function(raw){
  if (!inherits(raw, "raw"))
    raw <- read.EK_raw(raw)
  dgIdx <- get_dgIdx(raw)	
  idx <- dgIdx$dgType == "NME0"
  if (sum(idx) > 0){
    idx <- dgIdx[idx, ]
    n <- nrow(idx)
    dgT <- .POSIXct(double(n), tz = "UTC")
    string <- rep("", n)
    for (k in 1:n){
      dgT[k] <-  dgTime(raw, ini = idx$sdgD[k])     
      string[k] <- rawToChar(raw[(idx$sdgD[k] + 8):idx$edgD[k]])
    }
    nmea <- data.frame(dgTime = dgT, string = string)
  } else {
     stop("No NMEA data have been found in raw data")
  }  
  nmea
}
