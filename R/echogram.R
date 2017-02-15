echogram <-
function(echogram, xref = c("distance", "time", "ping"), scheme = "echov",
  Svthr = -80, Svmax = NULL, col.sep = 1, colbar=TRUE, ...){
  echo <- echogram
  if ( class(echo) != "echogram" ) 
    stop ("need object of class 'echogram'") 
  
#------------------------------------------------------------------------------ 
# image.scale() function retrieved from: 
# https://www.r-bloggers.com/new-version-of-image-scale-function/ 
#
#This function creates a color scale for use with the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "axis.pos" argument
#defines the side of the axis. The "add.axis" argument defines
#whether the axis is added (default: TRUE)or not (FALSE).
image.scale <-
function(z, zlim, col = heat.colors(12), breaks, axis.pos=1, add.axis=TRUE, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  if(axis.pos %in% c(1,3)){ylim<-c(0,1); xlim<-range(breaks)}
  if(axis.pos %in% c(2,4)){ylim<-range(breaks); xlim<-c(0,1)}
  plot(1,1,t="n",ylim=ylim, xlim=xlim, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(axis.pos %in% c(1,3)){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(axis.pos %in% c(2,4)){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
  box()
  if(add.axis) {axis(axis.pos)}
}
#------------------------------------------------------------------------------  
  Sv <- echo$Sv
  flip.matrix <- function(x)t(x[nrow(x):1, ])
  if ( missing(Svmax) ) Svmax <- 0
  cb <- palette.echogram(Svthr, Svmax, col.sep = col.sep, scheme = scheme)
  op <- par(no.readonly = TRUE)
  
  if ( missing(xref) ){
     Xlab <- "Distance (nm)"
	 Xnum <- echo$pings$cumdist
  }
  
  xref <- match.arg(xref)
    Xlab <- switch(xref,
	               distance = "Distance (nm)",
				   time = "Ping time",
				   ping = "Ping number")
	Xnum <- switch(xref,
	               distance = echo$pings$cumdist,
				   time = echo$pings$pingTime,
				   ping = 1:nrow(echo$pings))
  
  if (colbar == TRUE){
  layout( matrix(c(2, 1), ncol=2), widths=c(7/8, 1/8), heights = c(1, 1) )
     par(mar=c(5.1, 0, 4.1, 3.5))
     image.scale(z=flip.matrix(Sv), col = cb$palette, breaks = cb$breaks, axis.pos = 4)
     par(mar=c(5.1, 4.1, 4.1, 0.1))  	 
  }	 
  if (colbar == FALSE)
     par(mar=c(5.1, 4.1, 4.1, 3.5))  
     image(z = flip.matrix(Sv), axes = FALSE, ylab = "Depth (m)", xlab = Xlab,
        col = cb$palette, breaks = cb$breaks, ...); box()
  
  yidx <- seq(0, 1, len=nrow(Sv))
  at.y <- pretty(yidx)
  pos.y <- apply(abs(outer(yidx, at.y, "-")), 2, which.min)
  lab.y <- sort(round(echo$depth[pos.y]), decreasing = TRUE)
  axis(2, at= at.y, labels=lab.y, las = 1)
  
  xidx <- seq(0, 1, len=length(Xnum))
  at.x <- pretty(xidx)
  pos.x <- apply(abs(outer(xidx, at.x, "-")), 2, which.min)
  lab.x <- Xnum[pos.x] 
  if (any(class(lab.x) == "numeric"))
    lab.x <- round(lab.x, 1)
  else 
    lab.x <- format(lab.x, format='%H:%M')
  axis(1, at= at.x, labels=lab.x)

  on.exit(par(op))
}
