#' Parse NMEA sentences 
#'
#' Find and parse GPS data from NMEA sentences.
#'
#' @param nmea A data frame with CPU time and corresponding NMEA sentences
#' as returned by \code{get_NME0}.
#' 
#' @param sentence A string identifying the NMEA sentence to process. It
#' can be one of "GPRMC", "GPGGA", "INGGA", or "GPGLL".
#'
#' @param returnAll logical. If \code{TRUE}, all data found in the selected
#' sentence is returned.
#'
#' @details This function looks first for the "GPRMC" sentence, if not found  
#' it tries then with "GPGGA", "INGGA", or "GPGLL", and process the one with
#' more information (more GPS fixes). Be aware that sometimes strings may be
#' corrupted, in which case, it is possible to choose manually the sentence 
#' to process. This is why \code{get_NME0} does not automatically parse the 
#' NMEA strings by default. 
#' When one of "GPVTG", "INVTG", "GNVTG", or "IIVTG" sentences is found, vessel 
#' speed and bearing are also parsed and returned. 
#' Differences in time.cpu and time.gps may reveal wrong clock settings in the 
#' data aquisition computer.
#'
#' @return A data frame with cpu time, gps time (includes miliseconds if found),
#' longitude, and latitude. Additionally, vessel speed and bearing are returned 
#' if found in the data.
#'
#' @author Héctor Villalobos.   
#'
#' @seealso \code{get_NME0}.
#'
#' @references NMEA sentences structure can be seen in: 
#' \url{https://gpsd.gitlab.io/gpsd/NMEA.html}
#'
#' @examples
#' if(interactive()){
#' ek <- read.EK60_raw("D20130504-T083828.raw", angles = FALSE)
#' 
#' gps <- parse.nmea(ek$nmea)
#' head(gps)
#' }
parse.nmea <- function(nmea, sentence = NULL, returnAll = FALSE)
{
  # Available sentences in the data
  sentences <- substr(nmea[, 'string'], 2, 6)
  # Sentences with position data. More than one may be present.
  swp <- c("GPRMC", "GPGGA", "INGGA", "GPGLL")
  # columns' indices
  idx <- list(GPRMC = c(2, 4:7, 10), GPGGA = 2:6, INGGA = 2:6, GPGLL = c(6, 2:5))  
  # find out which are present
  px <- swp %in% sentences
  if (sum(px) == 0){
    ans <- data.frame(time.cpu = NA, time.gps = NA, lon = NA, lat = NA)
  } else {	
  swp <- swp[px]
  idx <- idx[px]
  
  # Manual selection
  if (!missing(sentence)){
    if(!sentence %in% swp)
      stop ("NMEA sentence ", sentence, " not found in data.", 
      "\n It should be one of: ", paste(swp, collapse = ", "))
    at <- grep(sentence, sentences)
    idx <- idx[[sentence]]
  } else { 
    if ("GPRMC" %in% swp){ # try GPRMC
      at <- grep("GPRMC", sentences)
      idx <- idx[["GPRMC"]]
    } else { # or one of the remaining...
      ns <- length(swp)
      for ( j in 1:ns ){
        at <- grep(swp[j], sentences)
        len <- length(at)
        assign(paste("at", j, sep="."), at)
        assign(paste("len", j, sep="."), len)
      }
      # process the one with more data, if equal, take the first
      px <- mget(paste("len", 1:ns, sep="."))
      ix <- which.max(px)
      at <- grep(swp[ix], sentences) 
      idx <- idx[[ix]]
    }
  }  
  GPSs <- nmea[at, 'string']
  dgTime <- nmea[at, 'dgTime']
  nc <- length(unlist(strsplit(GPSs[1], split = ",")))
  spl <- strsplit(GPSs, split = ",")
  #spl <- unlist(strsplit(GPSs, split = ","))
  #GPSs <- as.data.frame(matrix(spl, ncol = nc, byrow = TRUE))
  GPSs <- plyr::ldply(spl, rbind)
  
  # Time
  ti <- GPSs[, idx[1]]
  nchar.ti <- unlist(lapply(ti, nchar))
  
  # create time string
  time <- paste(substr(ti, 1, 2), substr(ti, 3, 4), substr(ti, 5, nchar.ti), sep=":")
  
  # Date
  if(length(idx) > 5){
    da <- GPSs[, idx[6]]
    date <- paste(substr(da, 1, 2), substr(da, 3, 4), substr(da, 5, 6), sep="/")
    DT <- paste(date, time)
    time <- strptime(DT, format = "%d/%m/%y %H:%M:%OS", tz = "UTC")
  } else {
    date <- as.Date(dgTime)
    DT <- paste(date, time)
    time <- strptime(DT, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  }
      
  # Decimal Longitudes and Latitudes     
  lat <- as.numeric(GPSs[, idx[2]])/100
  lat <- floor(lat) + (lat %% 1) * 10/6
    
  lon <- as.numeric(GPSs[, idx[4]])/100
  lon <- floor(lon) + (lon %% 1) * 10/6
    
  idxS <- which(GPSs[, idx[3]] == "S")
  if (length(idxS) > 0 )
    lat[idxS] <- -lat[idxS]
    
  idxW <- which(GPSs[, idx[5]] == "W")
  if (length(idxW) > 0 )
    lon[idxW] <- -lon[idxW]

  ans <- data.frame(time.cpu = dgTime, time.gps = time, lon = lon, lat = lat)
  nr <- nrow(ans)
  
  # If vessel speed and bearing are found 
  # Possible sentences identified so far: 
  vs <- c("GPVTG", "INVTG", "GNVTG", "IIVTG")
  # can't filter just by "VTG" because some files have data from two different VTG sentences
  vs <- vs[vs %in% sentences]
  ns <- length(vs)
  lens <- rep(NA, ns)
  if (ns > 0){  
    for ( j in 1:ns ){
      at <- grep(vs[j], sentences)
      len <- length(at)
      assign(paste("at", j, sep="."), at)
      lens[j] <- len
    }  
  
    dr <- nr - lens
    idx <- which.min(dr)
    dr <- dr[idx]
    
    atVTG <- grep(vs[idx], sentences) 
    VTG <- nmea[atVTG, 'string']
    len <- length(unlist(strsplit(VTG[1], split = ",")))
    spl <- unlist(strsplit(VTG, split = ","))
    VTG <- as.data.frame(matrix(spl, ncol = len, byrow = TRUE))
    bearing <- as.numeric(VTG$V2)
    speed <- as.numeric(VTG$V6) 
    lb <- length(bearing)
    if (lb != nr){
      if (dr > 0){
        bearing <- c(bearing, rep(NA, dr))
        speed <- c(speed, rep(NA, dr))
      } else {
        bearing <- bearing[1:(lb+dr)]
        speed <- speed[1:(lb+dr)]
      }
    }
    ans$bearing <- bearing
    ans$speed <- speed
  }
  if (returnAll == TRUE){
    ans <- data.frame(ans, GPSs)
  }
  }
  return(ans)
}