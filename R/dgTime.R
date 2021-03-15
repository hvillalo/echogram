#' Get datagram time from imported EK* raw files.
#' 
#' Function get.dgTime() is not meant to be run by the user. it 
#' is called by other functions.
#'
#' The time is cpu time, and the time zone is assumed to be UTC
#' which may not be the case.
#'
#' @param raw An imported EK* raw file
#' @param ini Initial Byte with time data
#' 
#' @return datagram time in POSIXct format
#'
#' 
#' @examples
#'  
#'  
dgTime <- function(raw, ini){
  # Convert integer to unsigned integer (from bmp package).
  ConvertIntToUInt <- function (x, adjustment = 2^32) {
    x <- as.numeric(x)
    signs <- sign(x)
    x[signs < 0] <- x[signs < 0] + adjustment
    x
  }
  
  # the first 8 Bytes of every datagram (after the name) store the time
  xDT <- readBin(raw[ini:(ini + 7)], 'integer', n = 2, size = 4)
  xDT <- ConvertIntToUInt(xDT)
  ans <- (xDT[2] * 2^32 + xDT[1]) / 10e6 - 11644473600
  class(ans) <- c("POSIXct", "POSIXt")
  attr(ans, "tzone") = "UTC"
  ans
}