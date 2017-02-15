position.hac <-
function( hac ) {
  #require(readHAC)
  hacR <- hac
  if ( !"HAC" %in% class(hac) ) 
    hacR  <- readHAC::readHAC( hac )
  if ( !20 %in%  unique(hacR$type) )
    stop ( "Position Tuple (20) is missing")
  posTuple <- hacR[hacR[["type"]] == 20]
  lat <- readHAC::parseHAC(posTuple)$Latitude
  lon <- readHAC::parseHAC(posTuple)$Longitude
  fracSec <- readHAC::parseHAC(posTuple)$'Time fraction'
  cpuTime <- readHAC::parseHAC(posTuple)$'Time CPU ANSI'
  cpuTime <- cpuTime + fracSec
  cpuTime <- as.POSIXlt(cpuTime, tz="UTC", format="%Y-%m-%d", origin="1970-01-01 00:00:00")
  gpsTime <- readHAC::parseHAC(posTuple)$'GPS time (GMT) [s]'
  gpsTime <- as.POSIXlt(gpsTime, tz="UTC", format="%Y-%m-%d", origin="1970-01-01 00:00:00")
  ans <- data.frame(time.gps = gpsTime, time.cpu = cpuTime, lon = lon, lat = lat)
  ans
}
