#' Read raw files from Simrad EK60 scientific echosounders 
#' 
#' This function will read all data in EK60 raw files.
#' 
#' @param file EK60 raw file name
#'
#' @param parseNMEA logical. When false (the default) all the nmea sentences are
#' returned as is. If TRUE, the parse.nmea function try to parse the most appropriate.
#'
#' @details This function first calls \code{read.EK_raw}, and \code{get_dgIdx} functions.
#' With the raw vector and datagram indices returned, it finds the and extracts the 
#' configuration (CON0), GPS positions (NMEO) and acosutic data (RAW0) datagrams.
#'
#' @return A list .
#'
#' @author Héctor Villalobos   
#'
#' @example
#' \dontrun{
#'  depth <- read.EK60_raw("D20140510-T102540.raw")
#' }
#'
#'
read.EK60_raw <- function(file, parseNMEA = FALSE, angles = TRUE){
  # open file
  raw <- read.EK_raw(file)
  
  # get Datagrams indices
  dgIdx <- get_dgIdx(raw)
  
  # read configuration datagram CON0
  conf <- get_CON0(raw)
  ntr <- conf$Header$transceiverCount
  
  # read NMEA sentences
  nmea <- get_NME0(raw)
  
  # parse NMEA
  if (parseNMEA == TRUE){
    nmea <- parse.nmea(nmea)
  }
  
  # read sample data from RAW0
  sampleData <- get_RAW0(raw, angles)

  ans <- list(config = conf, nmea = nmea, pings = sampleData)
  ans
}
