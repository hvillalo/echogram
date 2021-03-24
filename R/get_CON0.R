#' Get CON0 datagram from EK60 raw files
#' 
#' Read the echosounder configuration information stored in CON0 datagram.
#' 
#' @param raw A raw vector imported via \code{read.EK_raw} or an EK60 raw file name.
#' 
#' @return A list with two data frames: Header, containing survey-, transect- 
#' and sounder names, software version, and number of transceivers; Transceiver,
#' with channel ID, beamtypes, frequency, gain, equivalent beam angle, etc. for 
#' each transceiver.
#'
#' @details While it can be used for reading the configuration data from an 
#' EK60 raw file, this function is not meant to be called directly by the user.
#'
#' @author Héctor Villalobos.   
#'
#' @examples
#' if(interactive()){
#' config <- get_CON0("D20130504-T083828.raw")
#' config
#' }
get_CON0 <- function(raw){
  if (!inherits(raw, "raw"))
    raw <- read.EK_raw(raw)
  dgIdx <- get_dgIdx(raw)	
  idx <- dgIdx$dgType == "CON0"
  if (sum(idx) > 0){
    idx <- dgIdx[idx, ]
    ini <- idx$sdgD
    dgT <- dgTime(raw, ini)
    len <- c(4, 4, rep(128, 4), 4)
    i <- c(ini, ini + cumsum(len))[1:length(len)]
    k <- i + len - 1
    surveyName <- rawToChar(raw[i[3]:k[3]])
    transectName <- rawToChar(raw[i[4]:k[4]])
    sounderName <- rawToChar(raw[i[5]:k[5]])
    version <- rawToChar(raw[i[6]:(k[6] - 123)]) # the full chunk causes an error
    transceiverCount <- readBin(raw[i[7]:k[7]], 'integer', 1, 4, endian = "little")
    
    Header <- data.frame(dgTime = dgT, surveyName, transectName, sounderName, 
                         version, transceiverCount)
    
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
