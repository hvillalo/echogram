# convert electrical angles to mechanical angles
  convertAngles <- function(ek){
    ans <- angles <- ek$pings$angles
	conf <- ek$config
	
	xcvrConf <- conf$Transceiver
	ntr <- conf$Header[, 'transceiverCount']
    ka <- 180/128
        
    # Angles Sensitivity
    denAlon <- xcvrConf$angleSensitivityAlongship - xcvrConf$angleOffsetAlongship
    denAthw <- xcvrConf$angleSensitivityAthwartship - xcvrConf$angleOffsetAthwartship
    
    for (j in 1:ntr){
      ans[ , , j, 1] <- angles[ , , j, 1] * ka / denAlon[j]
      ans[ , , j, 2] <- angles[ , , j, 2] * ka / denAthw[j]
    }
   	ans
  }