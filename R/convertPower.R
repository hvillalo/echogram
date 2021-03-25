#' Convert Received Power to Sv or TS 
#' 
#' Convert acoustic power from imported EK60 raw files to Sv or TS data.
#' 
#' @param ekraw Imported EK60 data, as returned by \code{read.EK60_raw}.
#'
#' @param frequency An integer corresponding to index of the transceiver
#'  in the data to convert.
#'
#' @param output A string indicating whether the power should be converted to 
#' acoustic backscattering strength (``Sv'') or to target strength (``TS'').
#' 
#' @details While the function operates on the EK60 data, it should be preferable 
#' used through \code{ek2echogram}, which will convert the ek data to echogram.
#'
#' @return A two dimensions array [depth samples, pings] with the desired 
#' output.
#'
#' @author Héctor Villalobos.   
#'
#' @seealso \code{ek2echogram}.
#'
#' @examples
#' if(interactive()){
#' ek <- read.EK60_raw("D20130504-T083828.raw", parseNMEA = TRUE, angles = TRUE)
#' 
#' Sv <- convertPower(ek, output = "Sv")
#' image(Sv)
#' }
convertPower <- function(ekraw, frequency = NULL, output = "Sv"){
  xcvrConf <- ekraw$config$Transceiver
  sdat <- ekraw$pings$sampleData
  if (missing(frequency))
    frequency <- 1
  frq <- frequency	
  # Received power (Pr)
  Pr <- ekraw$pings$Pr[ , , frq]
  # dimensions. 1: samples; 2: pings
  dims <- dim(Pr)
  # sample range (depth)
  R <- sampleRange(ekraw, frq)
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
  if (output == "Sv"){
    S1 <- (20 * log10(R)) + (2 * alpha * R) - (10 * log10((Pt * Go^2 * lambda^2) / (16 * pi^2))) - (10 * log10((ss * tau * psi) / 2)) - (2 * sAcorr)
  }	
  # Received Power to TS
  # S_V(R, P_r) = P_r + 40log(R) + 2\alpha R - 10log(\frac{P_t G_0^2 \lambda ^2}{16 \pi^2})
  if (output == "TS"){
    S1 <- (40 * log10(R)) + (2 * alpha * R) - (10 * log10((Pt * Go^2 * lambda^2) / (16 * pi^2)))
  }
  ans <- sweep(Pr, MARGIN = 1, S1, "+")
  ans <- ans[sort(nrow(ans):1), ]
  attr(ans, "frequency") <- paste(fr/1000, "kHz")
  attr(ans, "variable") <- output
  ans
}
    

