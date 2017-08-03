sample.echogram <-
function(echogram, plot = TRUE, coords = NULL, col = "black"){
    echo <- echogram
    if ( class(echo) != "echogram" ) 
        stop ("need object of class 'echogram'")
    if ( plot == TRUE ) {
        dev.new(); echogram(echo)
    }
    reshape.echogram <- function(echo) {
        dm <- dim(echo$Sv)
        nx <- dm[2]; X <- 1:nx # number and sequence of pings
        ny <- dm[1]; Ym <- 1:ny # number and sequence of depth samples in data
        Yi <- ny:1 # sequence of detph samples in image
        vX <- rep(X, each = ny)
        vYm <- rep(Ym, nx)
        vYi <- rep(Yi, nx)
        pt <- rep(echo$pings$pingTime, each=ny) 
        dp <- rep(echo$depth, nx)
        z <- as.vector(echo$Sv)
        ans <- data.frame(X=vX, Ym=vYm, Yi=vYi, pingTime=pt, depth=dp, Sv=z)
        attr(ans, "frequency")  <- attributes(echo$Sv)$frequency
        ans
    }
	
    echoL <- reshape.echogram(echo)
    frq <- attr(echoL, "frequency")
    if ( missing(coords) ) {
        pts <- locator(type = "p", pch = 10, col = col)       
        coords <- round(as.data.frame(pts))
    }
    points(coords, pch=10, col=col)
    nPts <- nrow(coords)
    ans <- data.frame(id = 1:nPts, coords, d = NA, pingNumber = coords$x, 
	                  pingTime = NA, depth = NA, Sv = NA)
    for ( i in 1:nPts ){
        d <- sqrt(outer(coords[i, 'x'], echoL[, 'X'], "-")^2 + 
	              outer(coords[i, 'y'], echoL[, 'Yi'], "-")^2)
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
