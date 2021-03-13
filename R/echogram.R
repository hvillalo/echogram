echogram <-
function(echogram, Svthr = -70, Svmax = 0, col.sep = NULL, col.nb = NULL, scheme = NULL,  
         depth.grid = NULL, x.grid = NULL, x.ref = c("pings", "nmi", "seconds"), 
         seabed = FALSE, depth.max = NULL, ping.ini = NULL, ping.max = NULL, colbar=TRUE, 
         main = NULL, tformat = "%H:%M", ...){
  echo <- echogram
<<<<<<< HEAD
  if (!inherits(echo, "echogram"))
=======
  if ( !inherits(echo, "echogram") ) 
>>>>>>> b5b855f94c22523cfca67f36624c6e329bdf0ae3
    stop ("need object of class 'echogram'") 
  if (!missing(depth.max )){
    if (missing(ping.ini))
      ping.ini <- 1
    if (missing(ping.max))
      ping.max <- ncol(echo$Sv)
    echo <- trim.echogram(echo, depth.max, ping.ini, ping.max)
  }  
  
#------------------------------------------------------------------------------ 
# imageScale() function from package "sinkr" by Marc Taylor: 
#  https://github.com/marchtaylor/sinkr/
#
# Make a color scale to accompany an image or other plot
imageScale <- function(z, zlim, col = heat.colors(12),
breaks, axis.pos=1, add.axis=TRUE, xlim=NULL, ylim=NULL, ...){
 if(!missing(breaks)){
  if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
 }
 if(missing(breaks) & !missing(zlim)){
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
 }
 if(missing(breaks) & missing(zlim)){
  zlim <- range(z, na.rm=TRUE)
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
 }
 poly <- vector(mode="list", length(col))
 for(i in seq(poly)){
  poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
 }
 if(axis.pos %in% c(1,3)){YLIM<-c(0,1); XLIM<-range(breaks)}
 if(axis.pos %in% c(2,4)){YLIM<-range(breaks); XLIM<-c(0,1)}
 if(!missing(ylim)){ YLIM <- ylim }
 if(!missing(xlim)){ XLIM <- xlim }

 plot(1, 1, t="n", ylim=YLIM, xlim=XLIM, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i", ...)  
 for(i in seq(poly)){
  if(axis.pos %in% c(1, 3)){
   polygon(poly[[i]], c(0, 0, 1, 1), col=col[i], border=col[i])
  }
  if(axis.pos %in% c(2, 4)){
   polygon(c(0, 0, 1, 1), poly[[i]], col=col[i], border=col[i])
  }
 }
 box()
 if(add.axis) {axis(axis.pos, las=1, ...)}
}
#------------------------------------------------------------------------------  
  Sv <- echo$Sv
  if (missing(main))
    main <- attr(Sv, 'frequency')
  nx <- ncol(Sv) # number of pings
  ny <- nrow(Sv) # number of depth samples

# determine number of colors by col.sep
  if (!missing(col.sep)){
    breaks <- seq(Svthr, Svmax, by = col.sep)
    lb <- length(breaks)
    if (breaks[lb] < Svmax)
      breaks <- c(breaks, breaks[lb] + col.sep)
    nbcols = length(breaks) - 1
  } else {
  # determine breaks by number of colors   
    if (!missing(col.nb)){
      breaks <- seq(Svthr, Svmax, len = col.nb + 1)
      col.sep <- breaks[2]-breaks[1]
      nbcols <- col.nb
    }
  }	

  if (missing(col.sep) & missing(col.nb)){
    col.sep <- 1
    breaks <- seq(Svthr, Svmax, by = col.sep)
    lb <- length(breaks)
    if (breaks[lb] < Svmax)
      breaks <- c(breaks, breaks[lb] + col.sep)
    nbcols = length(breaks) - 1
  }	
  
  # new default palette from pals package.
  if (missing(scheme)) 
    scheme <- parula(nbcols)
	
    cb <- palette.echogram(Svthr, Svmax, col.sep, nbcols, scheme = scheme)

    # Truncate Sv values above Svmax
    Sv[Sv > Svmax] <- Svmax
    Sv <- t(Sv[ny:1, ])

    x.ref <- match.arg(x.ref)
    Xlab <- switch(x.ref, pings = "Ping number", nmi = "Distance (nm)", 
                   seconds = "Ping time")
    
    if (colbar == TRUE){
        zlab <- expression(paste(S[v], "  (dB re 1 ", m^{-1}, ")")) # def of zlab
<<<<<<< HEAD
		op <- par(no.readonly = TRUE)  
        on.exit(par(op))
        layout( matrix(c(2, 1), ncol=2), widths=c(7/8, 1/8), heights = c(1, 1) )
        par(mar=c(5.1, 0.1, 4.1, 4.0)) # Antes 3.5
        imageScale(z = Sv, col = cb$palette, breaks = cb$breaks, axis.pos = 4)
=======
        layout( matrix(c(2, 1), ncol=2), widths=c(7/8, 1/8), heights = c(1, 1) )
        par(mar=c(5.1, 0, 4.1, 4.0)) # Antes 3.5
        imageScale(z=flip.matrix(Sv), col = cb$palette, breaks = cb$breaks, axis.pos = 4, ...)
>>>>>>> b5b855f94c22523cfca67f36624c6e329bdf0ae3
        mtext(zlab, side=4, line=-1.5, outer=TRUE)
        par(mar=c(5.1, 4.1, 4.1, 0.1))
    }
    image(x=1:nx, y=1:ny, z = Sv, axes = FALSE, ylab = "Depth (m)", xlab = Xlab,
          col = cb$palette, breaks = cb$breaks, main = main); box()

    if(seabed == TRUE){
      R <- echo$depth
      bl <- bot  <- echo$pings$detBottom
      for (k in 1:length(bl)){
        bl[k] <- which.min(abs(R - bot[k]))
      }
	  bl <- length(R) - bl
      lines(bl, ...)	
    }
	
    # Depth grid
    depth.max <- floor(max(echo$depth))
    if (missing(depth.grid))
     depth.grid <- pretty(0:depth.max)[2]
    dgs <- seq(depth.grid, depth.max, depth.grid)
    pos.y <- apply( abs(outer(echo$depth, dgs, "-")), 2, which.min )
    at.y <- length(echo$depth) + 1 - pos.y
<<<<<<< HEAD
    axis(2, at = at.y, labels = dgs, las = 1)
  
    # X grid (pings) 
    if (x.ref == "pings"){
      if (missing(x.grid))
        x.grid <- floor(nx*0.1)
      at.x <- seq(x.grid, nx, x.grid) 
      axis(1, at = at.x, labels = at.x)
    }
    if (x.ref == "nmi"){
      distMax <- max(echo$pings$cumdist)
      if (missing(x.grid))
        x.grid <- floor(distMax*0.1)
      at.x <- seq(x.grid, distMax, x.grid) 
      axis(1, at = at.x, labels = at.x, ...)
    }
    if (x.ref == "seconds"){
      pt <- echo$pings$pingTime
      timeRng <- range(pt)
      if (missing(x.grid))
        x.grid <- (timeRng[2]-timeRng[1])*0.1
      lab.x <- seq.POSIXt(timeRng[1], timeRng[2], x.grid) 
      at.x <- rep(NA, length(lab.x))
      for (k in 1:length(lab.x))
        at.x[k] <- which.min(abs(pt - lab.x[k]))
      lab.x <- format(lab.x, format = tformat)
      axis(1, at = at.x, labels = lab.x, ...)
    }
=======
    axis(2, at = at.y, labels = lab.y, las = 1, ...)
  
    xidx <- 1:nx
    at.x <- pretty(xidx)
    pos.x <- apply(abs(outer(xidx, at.x, "-")), 2, which.min)
    lab.x <- Xnum[pos.x] 
    if ( !inherits(lab.x, "numeric") )
        lab.x <- round(lab.x, 1)
    else 
        lab.x <- format(lab.x, format = '%H:%M')
    axis(1, at = at.x, labels = lab.x, ...)
>>>>>>> b5b855f94c22523cfca67f36624c6e329bdf0ae3
}

