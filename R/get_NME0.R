#' Get NMEA sentences from NME0 datagrams in imported EK* raw files.
#' 
#' @param raw An imported EK* raw file or a raw file name
#' 
#' @return A data frame with CPU time and corresponding NMEA data
#'
#' 
#' @examples
#'  \dontrun{
#'  nmea <- get.NME0(ekraw)
#'  }
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
