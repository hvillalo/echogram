#' Read raw files from Simrad EK60 scientific echosounders 
#' 
#' This function imports all data in EK60 raw files.
#' 
#' @param file EK60 raw file name.
#'
#' @param parseNMEA logical. When false (the default) all the nmea sentences found 
#' in the raw file are returned as text strings. If \code{TRUE}, the \code{parse.nmea} 
#' function tries to parse the most appropriate nmea sentence(s) type(s).
#' 
#' @param angles logical. If \code{TRUE} and angle data is present, a four dimensions array
#' [depth samples, pings, transceivers, anlges] containing alongship and atwarthship 
#' electrical angles is also returned.
#'
#' @details This function imports the file as raw bytes with \code{read.EK_raw}. 
#' Then, the configuration information is obtained from datagram CON0 with 
#' \code{get_CON0}, nmea sentences (NME0) with \code{get_NME0}, and finally  
#' acoustic data in RAW0 datagrams with \code{get_RAW0}.
#'
#' @return A three elements list: (1) configuration, with Header and Transceiver(s) 
#'         information; (2) nmea sentences (full or parsed); and (3) pings data, 
#'         which includes at least a sample data table, and received power in a 
#'         three dimensions array [depth samples, pings, transceivers]. 
#'         When \code{angles = TRUE}, alongship and athwartship electrical angles 
#'         are also returned as an array with an extra dimension.
#'
#' @author Héctor Villalobos.   
#'
#' @seealso \code{read.EK_raw}, \code{get_CON0}, \code{get_NME0}, and \code{get_RAW0}
#'
#' @examples
#' if(interactive()){
#' ekraw <- read.EK60_raw("D20130504-T083828.raw", parseNMEA = TRUE, angles = TRUE)
#' 
#' # Header
#' ekraw$config$Header
#' 
#' # Transceivers
#' ekraw$config$Transceiver
#' 
#' # GPS data
#' head(ekraw$nmea)
#' }
read.EK60_raw <- function(file, parseNMEA = FALSE, angles = TRUE){
  # read EK60 raw file as raw vector
  raw <- read.EK_raw(file)
  
  # get configuration datagram CON0
  conf <- get_CON0(raw)
  
  # get nmea sentences from NME0 datagrams
  nmea <- get_NME0(raw)
  
  # read sample data from RAW0 datagrams
  sampleData <- get_RAW0(raw, angles)
  
  # parse nmea
  if (parseNMEA == TRUE){
    nmea <- parse.nmea(nmea)
  }
  
  # assemble data
  ans <- list(configuration = conf, nmea = nmea, pings = sampleData)
  ans
}
