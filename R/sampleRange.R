# Determine sample range (depth)
sampleRange <- function(ek, frq = NULL){
  sdat <- ek$pings$sampleData
  if (missing(frq))
    frq <- 1
  # number of samples
  nd <- dim(ek$pings$Pr)[1]
  # sound speed (m/s) ****** should I use the mean?? ******
  ss <-  sdat[, , frq][1, 'soundVelocity']
  # sample interval (s/sample)
  si <- sdat[, , frq][1, 'sampleInterval']
  sl <- (ss * si)/2
  R <- rep(sl[1], nd) # it must be the same for all pings and freqs
  R <- as.numeric(cumsum(R)) # sample range  
  R
}