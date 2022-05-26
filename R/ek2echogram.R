#' Convert EK60 data to echogram
#' 
#' Convert imported EK60 raw data to class ``echogram''.
#' 
#' @param ekraw Imported EK60 data, as returned by \code{read.EK60_raw}.
#'
#' @param frequency An integer corresponding to index of the transceiver
#'  in the data to be converted.
#' 
#' @param data A string indicating the desired data: acoustic backscattering 
#' strength (``Sv'') or target strength (``TS'').
#'
#' @param environment An optional list with two elements: ``soundVelocity'' and 
#' ``absorptionCoeff'' calculated according to sea temperature and salinity
#' measured during data collection.   
#' 
#' @param calibration A list as above, with ``gain'' and ``saCorr'' obtained 
#' from the echosounder calibration.
#' 
#' @details This function calls the \code{sampleRange} and \code{convertPower} 
#' functions to produce an object of  \code{class} ``echogram''.
#'
#' @return A vector with sample range (m).
#'
#' @author Héctor Villalobos.   
#'
#' @seealso \code{read.echogram}.
#'
#' @examples
#' if(interactive()){
#' ek <- read.EK60_raw("D20130504-T083828.raw", parseNMEA = TRUE, angles = TRUE)
#' 
#' eco <- ek2echogram(ek, frequency = 1, data = "Sv")
#' echogram(eco)
#' }
ek2echogram <- function(ekraw, frequency = 1, data = "Sv", environment = NULL, 
                         calibration = NULL){
  saCorr <- 0
  
  if (!is.null(environment)){
    env <- environment
    if (any(!inherits(env, "list") | (!names(env) %in% c("soundVelocity", "absorptionCoeff"))))
      stop("'environment' must be a named list with 'soundVelocity' and 'absorptionCoeff' values")	
    ekraw$pings$sampleData[ , , frequency][ , 'soundVelocity'] <- env$soundVelocity
    ekraw$pings$sampleData[, , frequency][ , 'absorptionCoeff'] <- env$absorptionCoeff
  }
  
  if (!is.null(calibration)){
    cal <- calibration 
    if (any(!inherits(cal, "list") | (!names(cal) %in% c("gain", "saCorr")))) 
      stop("'calibration' must be a named list with 'gain' and 'saCorr' values")	
    ekraw$config$Transceiver[frequency, 'gain'] <- cal$gain
    saCorr <- cal$saCorr
  }
  
  # sample range
  R <- sampleRange(ekraw, frequency = frequency)
  # ping time. Caution, is local time ******
  pingTime <- ekraw$pings$sampleData[, , frequency][, 'pingTime']
  class(pingTime) <- c("POSIXct", "POSIXt")
  attr(pingTime, "tzone") = "UTC"
  pings <- data.frame(pingTime, detBottom = NA, speed = NA, cumdist = NA)
  Sv <- convertPower(ekraw, frequency = frequency, output = data, saCorr)
  ans <- list(depth = R, Sv = Sv, pings = pings)
  attr(ans, "class") <- "echogram"
  ans
}
