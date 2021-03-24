#' Extract transceiver configuration from EK60 raw files
#' 
#' Find and extract transceiver configuration information from imported EK60 raw files.
#'
#' @param raw A raw vector imported via \code{read.EK_raw}.
#'
#' @param ini Initial byte with transceiver data
#' 
#' @details The index (ini) comes from output of \code{get_dgIdx} 
#' function. This function should not be called directly by the user.
#'
#' @return a data frame with transceiver configuration: channel ID, beamtypes, 
#' frequency, gain, equivalent beam angle, etc. for each transceiver.
#'
#' @author Héctor Villalobos   
#' 
  xcvrConf <- function(raw, ini){
    len <- c(128, rep(4, 16), 20, 8, 20, 8, 20, 52)
    i <- c(ini, ini + cumsum(len))[1:length(len)]
    k <- i + len - 1
    channelId <- rawToChar(raw[i[1]:k[1]])
    beamType <- readBin(raw[i[2]:k[2]], 'integer', 1, 4, endian = 'little')
    tmp <- readBin(raw[i[3]:k[17]], 'double', n = 15, 4, endian = 'little')
      frequency <- tmp[1]
      gain <- tmp[2]
      EBA <- tmp[3]
      beamwidthAlongship <- tmp[4]
      beamwidthAthwartship <- tmp[5]
      angleSensitivityAlongship <- tmp[6]
      angleSensitivityAthwartship <- tmp[7]
      angleOffsetAlongship <- tmp[8]
      angleOffsetAthwartship <- tmp[9]
      posx <- tmp[10]
      posy <- tmp[11]
      posz <- tmp[12]
      dirx <- tmp[13]
      diry <- tmp[14]
      dirz <- tmp[15]
    pulseLengthT <- readBin(raw[i[18]:k[18]], 'double', 5, 4, endian = 'little')
    spare2 <- rawToChar(raw[i[19]:k[19]])
    gainT <- readBin(raw[i[20]:k[20]], 'double', 5, 4, endian = 'little')
    spare3 <- rawToChar(raw[i[21]:k[21]])
    saCorrT <- readBin(raw[i[22]:k[22]], 'double', 5, 4, endian = 'little')
    spare4 <- paste(rawToChar(raw[i[23]:k[23]], multiple = TRUE), collapse = "") 
    
    ans <- 
      data.frame(channelId, beamType, frequency, gain, EBA, beamwidthAlongship, 
                 beamwidthAthwartship, angleSensitivityAlongship, 
                 angleSensitivityAthwartship, angleOffsetAlongship, 
                 angleOffsetAthwartship, posx, posy, posz, dirx, diry, dirz, 
                 pulseLengthT= I(list(pulseLengthT)), spare2, 
                 gainT = I(list(gainT)), spare3, saCorrT = I(list(saCorrT)), spare4)
    return(ans)
  }
  
