#**********************  readRAW_EK60() ****************************************
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
