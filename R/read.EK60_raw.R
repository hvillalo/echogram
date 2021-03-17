#' Read raw files from Simrad EK60 scientific echosounders 
#' 
#' This function will read all data in EK60 raw files.
#' 
#' @param file EK60 raw file name
#'
#' @param parseNMEA logical. When false (the default) all the nmea sentences are
#' returned as is. If TRUE, the parse.nmea function try to parse the most appropriate.
#' 
#' @param angles Logical. If TRUE and angle data is present, an array
#' containing alongship and atwarthship electrical angles is also returned
#'
#' @details This function first calls \code{read.EK_raw}, and \code{get_dgIdx} functions.
#' With the raw vector and datagram indices returned, it finds the and extracts the 
#' configuration (CON0), GPS positions (NMEO) and acosutic data (RAW0) datagrams.
#'
#' @return A list .
#'
#' @author Héctor Villalobos   
#'
#' @examples
#' fn <- system.file("extdata", "demo-D20130504-T083828.raw", package = "echogram")
#' ekraw <- read.EK60_raw(fn, parseNMEA = TRUE, angles = TRUE)
#' 
#' # Header
#' ekraw$config$Header
#' 
#' # Transceivers
#' ekraw$config$Transceiver
#' 
#' # GPS data
#' head(ekraw$nmea)
#'
read.EK60_raw <- function(file, parseNMEA = FALSE, angles = TRUE){
  # read EK60 raw file as raw vector
  raw <- read.EK_raw(file)
  
  # get indices for datagrams
  dgIdx <- get_dgIdx(raw)
  
  # read configuration datagram CON0
  conf <- get_CON0(raw)
  
  # read nmea sentences from NME0 datagrams
  nmea <- get_NME0(raw)
  
  # parse nmea
  if (parseNMEA == TRUE){
    nmea <- parse.nmea(nmea)
  }
  
  # read sample data from RAW0 datagrams
  sampleData <- get_RAW0(raw, angles)

  ans <- list(config = conf, nmea = nmea, pings = sampleData)
  ans
}
