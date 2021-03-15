# Determine sample range (depth)
sampleRange <- function(ek, frq = NULL){
  if (missing(frq))
    frq <- 1
  # number of samples
  nd <- dim(Xraw$pings$Pr)[1]
  # sound speed (m/s) ****** should I use the mean?? ******
  ss <-  Xraw$pings$sampleData[, , frq][1, 'soundVelocity']
  # sample interval (s/sample)
  si <- Xraw$pings$sampleData[, , frq][1, 'sampleInterval']
  sl <- (ss * si)/2
  R <- rep(sl[1], nd) # it must be the same for all pings and freqs
  R <- as.numeric(cumsum(R)) # sample range  
  R
}