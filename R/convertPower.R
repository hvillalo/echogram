## Convert Received Power to Sv ====
convertPower <- function(ek, frq = NULL, out = "Sv"){
  xcvrConf <- ek$config$Transceiver
  sdat <- ek$pings$sampleData
  if (missing(frq))
    frq <- 1
  # Received power (Pr)
  Pr <- ek$pings$Pr[ , , frq]
  # dimensions. 1: samples; 2: pings
  dims <- dim(Pr)
  # sample range (depth)
  R <- sampleRange(ek, frq)
  # absortion coefficient
  alpha <- as.numeric(sdat[, , frq][1, 'absorptionCoeff'])
  # Transmitted power
  Pt <- as.numeric(sdat[, , frq][1, 'transmitPower'])
  # Gain
  gain <-  xcvrConf[frq, 'gain']
  Go <- 10^(gain/10)
  # Frequencies
  fr <- xcvrConf[frq, 'frequency']
  # sound speed
  ss <-  as.numeric(sdat[, , frq][1, 'soundVelocity'])
  # wavelength (m)
  lambda <- ss / fr
  # pulse length (\mu s)
  tau <- as.numeric(sdat[, , frq][1, 'pulseLength'])
  # equivalent beam angle (in dB re 1 steradian)
  EBA <- xcvrConf[frq, 'EBA']
  # equivalent beam angle in steradians
  psi <- 10^(EBA/10) 
  
  # sa correction table
  plT <- xcvrConf[frq, 'pulseLengthT'][[1]]
  gT <- xcvrConf[frq, 'gainT'][[1]]
  saCT <- xcvrConf[frq, 'saCorrT'][[1]]
  tmp <- data.frame(plT, gT, saCT)
  sAcorr <- tmp[tmp$plT == tau, 'saCT']
 
  # Received Power to Sv 
  # S_V(R, P_r) = P_r + 20log(R) + 2\alpha R - 10log(\frac{P_t G_0^2 \lambda ^2}{16 \pi^2}) -10log(\frac{c \tau \psi}{2}) -2 S_a\;corr
  if (out == "Sv"){
    S1 <- (20 * log10(R)) + (2 * alpha * R) - (10 * log10((Pt * Go^2 * lambda^2) / (16 * pi^2))) - (10 * log10((ss * tau * psi) / 2)) - (2 * sAcorr)
  }	
  # Received Power to TS
  # S_V(R, P_r) = P_r + 40log(R) + 2\alpha R - 10log(\frac{P_t G_0^2 \lambda ^2}{16 \pi^2})
  if ( out == "TS"){
    S1 <- (40 * log10(R)) + (2 * alpha * R) - (10 * log10((Pt * Go^2 * lambda^2) / (16 * pi^2)))
  }
  ans <- sweep(Pr, MARGIN = 1, S1, "+")
  ans <- ans[sort(nrow(ans):1), ]
  attr(ans, "frequency") <- paste(fr/1000, "kHz")
  attr(ans, "variable") <- out
  ans
}
    

