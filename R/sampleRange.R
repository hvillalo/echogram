#' Determine acoustic samples depth range
#' 
#' Calculate sample range according to sound speed and sample interval for imported EK60 raw files.
#' 
#' @param ekraw Imported EK60 data, as returned by \code{read.EK60_raw}.
#'
#' @param frequency An integer corresponding to index of the transceiver
#'  in the data for which to obtain the sample range.
#'
#' @details While the function operates on the EK60 data, it should be preferable 
#' used through \code{ek2echogram}, which will convert the ek data to echogram.
#'
#' @return A vector with sample range (m).
#'
#' @author Héctor Villalobos.   
#'
#' @seealso \code{ek2echogram}.
#'
#' @examples
#' if(interactive()){
#' ek <- read.EK60_raw("D20130504-T083828.raw", parseNMEA = TRUE, angles = TRUE)
#' 
#' R <- sampleRange(ek, frequency = 1)
#' R
#' }
sampleRange <- function(ekraw, frequency = NULL){
  sdat <- ekraw$pings$sampleData
  if (missing(frequency))
    frequency <- 1
  frq <- frequency
  # number of depth samples
  nd <- dim(ekraw$pings$Pr)[1]
  # sound speed (m/s) assuming sound speed and sample interval are constant accross all pings
  ss <-  sdat[, , frq][1, 'soundVelocity']
  # sample interval (s/sample)
  si <- sdat[, , frq][1, 'sampleInterval']
  sl <- (ss * si)/2
  R <- rep(sl, nd) # it must be the same for all pings
  R <- as.numeric(cumsum(R)) # sample range  
  R
}