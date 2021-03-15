## Sv matrix Ecointegration ====
echoint <- function(X, Svthr = NULL, H = NULL) {
  if (!inherits(X, "matrix"))
    stop("X should be a Sv matrix")
  if (missing(Svthr))
    Svthr <- min(X, na.rm = TRUE)
  if (missing(H))
    H <- nrow(X)*0.1 # assuming a sample length of 10 cm
  Sv <- as.vector(X)
  Sv <- Sv[!is.na(Sv)]
  
  # total number of samples
  Nt <- length(Sv) 

  # valid samples
  tau.s <- ifelse(Sv < Svthr, 0, 1)
  Ni <- sum(tau.s)
  
  # Sv --> sv
  sv <- 10^(Sv / 10)
  
  # mean Sv
  mSv <- 10 * log10(sum(tau.s * sv) / Nt)
  
  # NASC
  sA <-  4 * pi * 1852^2 * 10^(mSv/10) * H
  ans <- list(sA = sA, mSv = mSv, Nt = Nt, Ni = Ni, H = H, Svthr = Svthr)
  ans
}

