# readCON0(). Read configuration datagram CON0
get_CON0 <- function(raw){
  if (!inherits(raw, "raw"))
    raw <- read.EK_raw(raw)
  dgIdx <- get_dgIdx(raw)	
  idx <- dgIdx$dgType == "CON0"
  if (sum(idx) > 0){
    idx <- dgIdx[idx, ]
    ini <- idx$BiD
    dgtime <- dgTime(raw, ini) # Nota: No se ocupa?
    len <- c(4, 4, rep(128, 4), 4)
    i <- c(ini, ini + cumsum(len))[1:length(len)]
    k <- i + len - 1
    surveyName <- rawToChar(raw[i[3]:k[3]])
    transectName <- rawToChar(raw[i[4]:k[4]])
    sounderName <- rawToChar(raw[i[5]:k[5]])
    spare <- rawToChar(raw[i[6]:(k[6] - 123)]) # this one gives error when 
    # reading the full 128 Bytes. The fix used in xcvrConf() doesn't work 
    # very good.
    transceiverCount <- readBin(raw[i[7]:k[7]], what = 'integer', n = 1, 
                                size = 4, signed	= TRUE, endian = "little")
    
    Header <- data.frame(surveyName, transectName, sounderName, spare, 
                           transceiverCount)
    
    # Transceiver(s) configuration
    ntr <- Header$transceiverCount
    transceiver <- list()
      
    ini <- k[length(k)] + 1 
    for(i in 1:ntr){
      transceiver[[i]] <- xcvrConf(raw, ini)
      ini <- ini + 320
    }   
    transceiver <- as.data.frame(data.table::rbindlist(transceiver),
                              stringsAsFactors=FALSE)
    Configuration = list(Header = Header, Transceiver = transceiver)
  } else {
    stop ("No configuration datagram 'CON0' has been found")
  }
  Configuration
}
