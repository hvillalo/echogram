#' Get RAW0 datagrams from imported EK60 raw files
#'
#' Read the acoustic sample data stored in RAW0 datagrams.
#'
#' @param raw A raw vector imported via \code{read.EK_raw} or an EK60 raw file name.
#' 
#' @param angles Logical. If \code{TRUE} and angle data is present, a four dimensions
#' array containing alongship and atwarthship electrical angles is also returned.
#' 
#' @details This function is not meant to be called directly by the user.
#'
#' @return A list with pings data, which includes a sample data table, and 
#'         received power in a three dimensions array corresponding to the 
#'         number of depth samples, number of pings, and number of transceivers. 
#'         When \code{angles = TRUE}, alongship and athwartship electrical 
#'         angles are also returned as an array with an extra dimension. 
#'
#' @author Héctor Villalobos. 
#' 
get_RAW0 <- function(raw, angles){
  if (!inherits(raw, "raw"))
    raw <- read.EK_raw(raw)
  dgIdx <- get_dgIdx(raw)	
  idx <- dgIdx[dgIdx$dgType == "RAW0", ]
  
  # number of transceivers
  ntr <-  readBin(raw[529:532], 'integer', 1, 4, signed	= TRUE, endian = "little")
  
  # number of pings
  nraw <- nrow(idx)
  npings <- ceiling(nraw/ntr) # using ceiling in case of missing pings
  
  # determine the maximum number of depth samples
  ns <- rep(NA, nraw) 
  i <- idx$sdgD + 76
  j <- i + 3 
  for (p in 1:nraw)
    ns[p] <- readBin(raw[i[p]:j[p]], 'integer', 1, 4, endian = "little")
  ns <- max(ns)
    
  # Define arrays for sample data, power, and angles
  # *** need to fix differences in pulse lengths *****
  sampleData <- array(NA, dim = c(npings, 21, ntr))
  dimnames(sampleData)[2] <- list(c("pingTime", "channel", "mode", "transducerDepth",
     "frequency", "transmitPower", "pulseLength", "bandWidth", "sampleInterval", 
     "soundVelocity", "absorptionCoeff", "heave", "roll", "pitch", "temperature", 
     "trawlUpperDepthValid", "trawlOpeningValid", "trawlUpperDepth", "trawlOpening",
     "offset", "count"))
  Pr <- array(NA, dim=c(ns, npings, ntr))
  if (angles == TRUE)
    Ang <- array(NA, dim=c(ns, npings, ntr, 2))  
    
  len <- c(8, 2, 1, 1, rep(4, 12), 2, 2, rep(4, 4))
  # get sample data
  for (tr in 1:ntr){
    tridx <- idx$sdgD[seq(tr, nraw, ntr)]
    for (j in 1:npings){
      ini <- tridx[j]
      i <- c(ini, ini + cumsum(len))[1:length(len)]
      k <- i + len - 1
      chan <- readBin(raw[i[2]:k[2]], 'integer', 1, 2, endian = "little")
      sampleData[j, 1, chan] <- dgTime(raw, i[1])
      sampleData[j, 2, chan] <- chan
      mode <- readBin(raw[i[3]:k[4]], "integer", 2, 1, endian = "little")
      mode <- 256 * mode[2] + mode[1]
      sampleData[j, 3, chan] <- mode
      sampleData[j, 4:15, chan] <- readBin(raw[i[5]:k[16]], 'double', 12, 4, endian = "little")
      sampleData[j, 16:17, chan] <- readBin(raw[i[17]:k[18]], "integer", 2, 2, endian = "little")
      sampleData[j, 18:19, chan] <- readBin(raw[i[19]:k[20]], "double", 2, 4, endian = "little")
      sampleData[j, 20, chan] <- readBin(raw[i[21]:k[21]], 'integer', 1, 4, endian = "little")
      count <- readBin(raw[i[22]:k[22]], 'integer', 1, 4, endian = "little")
      sampleData[j, 21, chan] <- count
       
      # Received Power
      ip <- ini + 80
      ipc <- ini + count * 2; 
      power <- readBin(raw[ip:ipc], 'integer', count, 2, endian = "little")
      d <- ns - length(power)
      if (length(power) < ns)
        power <- c(power, rep(NA, d))
      Pr[ , j, chan] <- power
       
      # Angles
      if (angles == TRUE & mode > 1){
        ipa <- ipc + 1 + count * 4
        angl  <- readBin(raw[(ipc + 1):ipa], 'integer', count * 2, 1, endian = "little")
        angl <- t(matrix(angl, nrow = 2))
        along <- as.vector(angl[, 1]) 
        athw <- as.vector(angl[, 2])
        d <- ns - length(along)
        if (length(along) < ns){
          along <- c(along, rep(NA, d))
          athw <- c(athw, rep(NA, d))
        }   
        Ang[ , j, chan, 1] <- along
        Ang[ , j, chan, 2] <- athw
      }
    }
  }
  Pr <- Pr * 10 * log10(2)/256
  
  freqs <- as.vector(unique(sampleData[ , 'frequency', ]))
  dimnames(sampleData)[3] <- list(freqs)
  dimnames(Pr)[3] <- list(freqs)
  ans <- list(sampleData = sampleData, Pr = Pr)
  
  if (angles == TRUE & mode > 1){
  	dimnames(Ang)[3:4] <- list(freqs, c("alongship", "athwartship"))
    ans <- list(sampleData = sampleData, Pr = Pr, angles = Ang)
  }		
  ans
} 