#' Get CON0 datagram from EK60 raw files
#' 
#' Read the echosounder configuration information stored in CON0 datagram.
#' 
#' @param raw An imported EK60 raw file or a raw file name
#' 
#' @return A list with header and transceiver configuration
#' 
#' @details Not to be called directly by the user.
#'
#' @author Héctor Villalobos   
#'
#' @examples
#' fn <- system.file("extdata", "demo-D20130504-T083828.raw", package = "echogram")
#' config <- get_CON0(fn)
#' config
#'
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
    version <- rawToChar(raw[i[6]:(k[6]-123)]) 
    transceiverCount <- readBin(raw[i[7]:k[7]], what = 'integer', n = 1, 
                                size = 4, signed	= TRUE, endian = "little")
    
    Header <- data.frame(surveyName, transectName, sounderName, version, 
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
