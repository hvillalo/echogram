## Parse NMEA data 
#
#  The function can automatically detect the NMEA sentences present in the data,
# however, sometimes data is corrupted, so the user can select which of the 
# various sentences to process
#
#  See NMEA sentences structure in: https://gpsd.gitlab.io/gpsd/NMEA.html
#
parse.nmea <- function(nmea, sentence = NULL, returnAll = FALSE)
{
  #datetime <- paste(substr(date.real, 1, 10), GPS$time.gps)
  #GPS$time.gps <- strptime(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  #dt <- date.real - GPS$time.gps[1] 
  #if (dt != 0)
  #  warning("cpu time and gps time mismatch. Time difference is ", 
  #          round(dt, 2), " ", attr(dt, "units"))
  
  
  # Available sentences in data
  sentences <- substr(nmea[, 2], 2, 6)
  # Sentences with position data. More than one may be present.
  poss <- c("GPGGA", "INGGA", "GPGLL", "GPRMC")
  # columns' indices
  idx <- list(GPGGA = 2:6, INGGA = 2:6, GPGLL = c(6, 2:5), GPRMC = c(2, 4:7))  
  # find out which are present
  px <- poss %in% sentences
  poss <- poss[px]
  
  # If manual selection
  if (!missing(sentence)){
    if(!sentence %in% poss)
      stop ("NMEA sentence ", sentence, " not found in data.", 
      "\n It should be one of: ", paste(poss, collapse = ", "))
    at <- grep(sentence, sentences)
    idx <- idx[[sentence]]
  } else {
    idx <- idx[px]
    ns <- length(poss)
    for ( j in 1:ns ){
      at <- grep(poss[j], sentences)
      len <- length(at)
      assign(paste("at", j, sep="."), at)
      assign(paste("len", j, sep="."), len)
    }  
    # keep the one with more data, if equal, take the first
    px <- mget(paste("len", 1:ns, sep="."))
    ix <- which.max(px)
    at <- grep(poss[ix], sentences) 
    idx <- idx[[ix]]
  }  
  GPSs <- nmea[at, 2]
  dgTime <- nmea[at, 'dgTime']
  nc <- length(unlist(strsplit(GPSs[1], split = ",")))
  spl <- unlist(strsplit(GPSs, split = ","))
  GPSs <- as.data.frame(matrix(spl, ncol = nc, byrow = TRUE))
    
  # Time
  ti <- GPSs[, idx[1]]
  nchar.ti <- unlist(lapply(ti, nchar))
  time <- paste(substr(ti, 1, 2), substr(ti, 3, 4), substr(ti, 5, 6), sep=":")
  
  # there are miliseconds? Normally present in GGA, but not in GLL nor RMC
  if (all(nchar.ti - 9 == 0)){
    ms <- as.numeric(ti) %% 1
  } else {
    ms <- rep(0, length(ti))
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

  ans <- data.frame(dgTime = dgTime, time.gps = time, ms = ms, lon = lon, lat = lat)

  if (returnAll == TRUE){
    ans <- GPSs
    ans[, idx[1]] <- time
    ans[, idx[2]] <- lat
    ans[, idx[4]] <- lon
    ans$ms <- ms
  }
  nr <- nrow(ans)
  
  # If vessel speed and bearing are found 
  # Possible sentences identified so far: 
  vs <- c("GPVTG", "INVTG", "GNVTG", "IIVTG")
  # can't filter just by "VTG" because some files have data from two different VTG sentences
  # find which are present
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
    VTG <- nmea[atVTG, 2]
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
  return(ans)
}