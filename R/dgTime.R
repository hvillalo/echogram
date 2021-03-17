#' Get datagram time from imported EK* raw files.
#' 
#' Extract datagram time and convert it to POSIXct.
#'
#' @param raw A raw vector imported via \code{read.EK_raw}.
#'
#' @param ini Initial byte with time data
#' 
#' @details The cpu time is stored in the first 8 bytes of every datagram, 
#' after its name. It is assumed that the time zone is UTC, which may not
#' be the case. By comparig with gps time in nmea data, a correction may 
#' be applied if needed. The index (ini) comes from output of get_dgIDX() 
#' function. This function should not be called directly by the user.
#'
#' @return datagram time in POSIXct format
#'
#' @author Héctor Villalobos   
#' 
dgTime <- function(raw, ini){
  # Convert integer to unsigned integer (from bmp package).
  ConvertIntToUInt <- function (x, adjustment = 2^32) {
    x <- as.numeric(x)
    signs <- sign(x)
    x[signs < 0] <- x[signs < 0] + adjustment
    x
  }
  
  xDT <- readBin(raw[ini:(ini + 7)], 'integer', n = 2, size = 4)
  xDT <- ConvertIntToUInt(xDT)
  ans <- (xDT[2] * 2^32 + xDT[1]) / 10e6 - 11644473600
  class(ans) <- c("POSIXct", "POSIXt")
  attr(ans, "tzone") = "UTC"
  ans
}