#' Convert alongship and athwartship split beam angles
#'
#' Convert angles in imported EK60 raw files from electrical to mechanical.
#'
#' @param ekraw Imported EK60 data, as returned by \code{read.EK60_raw}.
#' 
#' @return A four dimensions array [depth samples, pings, transceivers, 2]
#' with mechanical angles.
#' 
#' @author Héctor Villalobos 
#'
#' @examples
#' if(interactive()){
#' ek <- read.EK60_raw("D20130504-T083828.raw", parseNMEA = TRUE, angles = TRUE)
#' angles <- convertAngles(ek)
#' dim(angles)
#' } 
convertAngles <- function(ekraw){
  ans <- angles <- ekraw$pings$angles
  conf <- ekraw$config

  xcvrConf <- conf$Transceiver
  ntr <- conf$Header[, 'transceiverCount']
  ka <- 180/128

  denAlon <- xcvrConf$angleSensitivityAlongship - xcvrConf$angleOffsetAlongship
  denAthw <- xcvrConf$angleSensitivityAthwartship - xcvrConf$angleOffsetAthwartship

  for (j in 1:ntr){
    ans[ , , j, 1] <- angles[ , , j, 1] * ka / denAlon[j]
    ans[ , , j, 2] <- angles[ , , j, 2] * ka / denAthw[j]
  }
  ans
}