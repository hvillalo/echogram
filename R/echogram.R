echogram <-
function(echogram, xref = c("ping", "distance", "time"), scheme = "echov",
  Svthr = -80, Svmax = NULL, col.sep = 1, colbar=TRUE, main=NULL, ...){
  echo <- echogram
  if ( class(echo) != "echogram" ) 
    stop ("need object of class 'echogram'") 
  flip.matrix <- function(x)t(x[nrow(x):1, ])
  
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
  if(axis.pos %in% c(1,3)){
   polygon(poly[[i]], c(0,0,1,1), col=col[i], border=col[i])
  }
  if(axis.pos %in% c(2,4)){
   polygon(c(0,0,1,1), poly[[i]], col=col[i], border=col[i])
  }
 }
 box()
 if(add.axis) {axis(axis.pos, las=1)}
}
#------------------------------------------------------------------------------  
    Sv <- echo$Sv
    if ( missing(main) )
      main <- attr(Sv, 'frequency')
    nx <- ncol(Sv) # number of pings
    ny <- nrow(Sv) # number of depth samples

    if ( missing(Svmax) ) Svmax <- 0
    cb <- palette.echogram(Svthr, Svmax, col.sep = col.sep, scheme = scheme)

    xref <- match.arg(xref)
    Xlab <- switch(xref, ping = "Ping number", distance = "Distance (nm)", 
                   time = "Ping time")
    Xnum <- switch(xref, ping = 1:nx, distance = echo$pings$cumdist,
                   time = echo$pings$pingTime)

    if (colbar == TRUE){
        layout( matrix(c(2, 1), ncol=2), widths=c(7/8, 1/8), heights = c(1, 1) )
        par(mar=c(5.1, 0, 4.1, 3.5))
        imageScale(z=flip.matrix(Sv), col = cb$palette, breaks = cb$breaks, axis.pos = 4)
        par(mar=c(5.1, 4.1, 4.1, 0.1))
    }
    if (colbar == FALSE)
        par(mar=c(5.1, 4.1, 4.1, 3.5))  
    image(x=1:nx, y=1:ny, z = flip.matrix(Sv), axes = FALSE, ylab = "Depth (m)", xlab = Xlab,
        col = cb$palette, breaks = cb$breaks, main = main, ...); box()
  
    md <- max(echo$depth)
    rmd <- c(floor(md), ceiling(md))
    max.depth <- rmd[which.min(abs(rmd - md))]
    lab.y <- pretty(0:max.depth)
    lab.y <- lab.y[1:which.min(abs(max.depth - lab.y))]
    pos.y <- apply( abs(outer(echo$depth, lab.y, "-")), 2, which.min )
    at.y <- length(echo$depth) + 1 - pos.y
    axis(2, at = at.y, labels = lab.y, las = 1)
  
    xidx <- 1:nx
    at.x <- pretty(xidx)
    pos.x <- apply(abs(outer(xidx, at.x, "-")), 2, which.min)
    lab.x <- Xnum[pos.x] 
    if (any(class(lab.x) == "numeric"))
        lab.x <- round(lab.x, 1)
    else 
        lab.x <- format(lab.x, format = '%H:%M')
    axis(1, at = at.x, labels = lab.x)
}
