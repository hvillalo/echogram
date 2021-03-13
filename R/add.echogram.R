add.echogram <-
function(echogram1, echogram2, operator = c("plus", "minus"), 
domain = c("linear", "dB")) {
  echo1 <- echogram1
  echo2 <- echogram2
    if ( !inherits(echo1, "echogram") & !inherits(echo2, "echogram") ) 
    stop ("need objects of class 'echogram'")
  m1 <- echo1$Sv
  m2 <- echo2$Sv
  if (dim(m1)[1] != dim(m2)[1] | dim(m1)[2] != dim(m2)[2])
     stop("non-conformable echograms, run match.echogram(echogram1, echogram2)")
  dB2linear <- function(X) 10^(X/10)
  linear2dB <- function(X) 10*log10(X)
  if ( missing(domain) )
     domain <- "dB"
  if (domain == "linear"){
     m1 <- dB2linear(m1)
     m2 <- dB2linear(m2)
  } 
  ans <- echo1
  operator <- match.arg(operator)
  ans0 <- switch(operator,
                 plus = `+`(m1, m2),
                 minus = `-`(m1, m2))
  if (domain == "linear")
     ans0 <- linear2dB(ans0) 
  ans$Sv <- ans0
  class(ans) <- "echogram" 
  ans
}
