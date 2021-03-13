noise.echogram <-
  function(echogram, ping=NULL, dB1m = NULL, alpha = NULL, plot = TRUE, out = FALSE){
    echo <- echogram
    if ( !inherits(echo, "echogram") ) 
      stop ("need object of class 'echogram'")
    if ( missing(ping) ) {
      np <- dim(echo$Sv)[2]
      coox <- locator(n=1, type="n")$x
      ping <- which.min(abs(seq(0, 1, len=np) - coox))
    }	
    Sv.pngs <- echo$Sv[ , ping]
    nc <- dim(Sv.pngs)[2]
    if ( is.null(nc) )
      nc <- 1
    Sv.vec <- as.vector(Sv.pngs)
    frq <- as.character(unlist(attributes(echo$Sv)[2]))
    depth <- echo$depth
    
    DS <- data.frame(Depth = rep(depth, nc), Sv = Sv.vec)
    DS <- DS[order(depth, decreasing = TRUE), ]
    if (missing(dB1m))
      dB1m <- mean(Sv.vec[depth < 10], na.rm=TRUE)
    if (missing(alpha)){
      alpha <- 0.01
      warning("absortion coefficient must be provided")
    }
    noise <- dB1m + 20 * log10(depth) + 2*alpha*(depth - 1)
	if ( plot == TRUE ) {
      plot(x=DS$Sv, y=-DS$Depth, xlab=expression(paste(S[v], " (dB)")), ylab="Depth (m)", cex=0.5, 
                pch = 16, type="o", col=rgb(0, 0, 1, 0.3), 
				main = paste("estimated noise;", frq, "(alpha=", alpha, "dB/km; N(1 m)=", round(dB1m), "dB )"), las=1); 
      abline(v=dB1m, col="red"); grid()
      lines(x=noise, y=-depth, lwd=2)
	}
    if ( out == TRUE){
      dm <- dim(echo$Sv)
      nm <- matrix(rep(noise, dm[2]), nrow=dm[1], ncol=dm[2])
      attr(noise, "frequency") <- paste(frq, "(noise; dB at 1m =", dB1m, ")") 
      echo$Sv <- nm
      class(echo) <- "echogram" 
      echo
    }
  }
