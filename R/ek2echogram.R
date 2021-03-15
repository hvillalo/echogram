## ek2echogram()
## Falta rediseñar clase 'echogram'
ek2echogram <- function(ek, frequency = 1, data = "Sv"){
  # sample range
  R <- sampleRange(ek, frq = frequency)
  # ping time. Caution, is local time ******
  pingTime <- ek$pings$sampleData[, , frequency][, 'pingTime']
  class(pingTime) <- c("POSIXct", "POSIXt")
  attr(pingTime, "tzone") = "UTC"
  pings <- data.frame(pingTime, detBottom = NA, speed = NA, cumdist = NA)
  Sv <- convertPower(ek, frq = frequency, out = data)
  ans <- list(depth = R, Sv = Sv, pings = pings)
  attr(ans, "class") <- "echogram"
  ans
}
