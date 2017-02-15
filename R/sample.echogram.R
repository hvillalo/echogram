sample.echogram <-
function(echogram, plot = TRUE, coords = NULL){
     echo <- echogram
     if ( class(echo) != "echogram" ) 
         stop ("need object of class 'echogram'")
     if ( plot == TRUE )
         echogram(echo)
 
     reshape.echogram <- function(echo) {
         dm <- dim(echo$Sv)
         X <- seq(0, 1, len=dm[2]); nx <- length(X)
         Y <- sort(seq(0, 1, len=dm[1]), decreasing = TRUE) ; ny <- length(Y)
         vX <- rep(X, each = ny)
         vY <- rep(Y, nx)
         pt <- rep(echo$pings$pingTime, each=ny) 
         dp <- rep(echo$depth, nx)
         z <- as.vector(echo$Sv)
         ans <- data.frame(X = vX, Y = vY, pingTime = pt, depth = dp, Sv = z)
         attr(ans, "frequency")  <- attributes(echo$Sv)$frequency
         ans
    }
	
     echoL <- reshape.echogram(echo)
     frq <- attr(echoL, "frequency")
     if ( missing(coords) ) {
         pts <- locator(type = "n")       
         coords <- as.data.frame(pts)
    }
     points(coords, cex=1.5)
     points(coords, cex=2, pch=3)
     nPts <- nrow(coords)
     param <- matrix(rep(NA, nPts), ncol = 1)
     ans <- data.frame(id = 1:nPts, coords, d = NA, pingTime = NA, 
                       depth = NA, Sv = param)
     p <- coords
     for ( i in 1:nPts ){
         d <- sqrt(outer(p[i, 'x'], echoL[, 'X'], "-")^2 + 
		           outer(p[i, 'y'], echoL[, 'Y'], "-")^2)
     d <- as.vector(d)
     minD <- min(d)
     ans[i, 'd'] <- minD
     ans[i, 'pingTime'] <- echoL[which.min(d), 'pingTime']
     ans[i, 'depth'] <- echoL[which.min(d), 'depth']
     ans[i, 'Sv'] <- echoL[which.min(d), 'Sv']
    }
     ans$pingTime <- as.POSIXlt(ans$pingTime, tz="UTC", format="%Y-%m-%d", 
	                            origin="1970-01-01 00:00:00")
     attr(ans, "frequency") <- frq
     ans
}
