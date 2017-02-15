  mergeSvmat <- function(m1, m2) {
    nr1 <- nrow(m1)
    nr2 <- nrow(m2)
    if ( nr1 > nr2 )
      m2 <- rbind(m2, matrix(NA, nrow = nr1 - nr2, ncol = ncol(m2)))
    if ( nr2 > nr1 )
      m1 <- rbind(m1, matrix(NA, nrow = nr2 - nr1, ncol = ncol(m1)))
    m0 <- cbind(m1, m2)
    m0
  }
