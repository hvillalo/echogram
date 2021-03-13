palette.echogram <-
function(Svthr = -70, Svmax = 0, col.sep = NULL, col.nb = NULL, scheme = NULL, visu = FALSE)
{
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
    scheme <- "parula"
  if (length(scheme) > 1) {
     cols <- scheme 
  } else {
     if ( !scheme %in% c("parula", "EK500", "echov") )
      stop ("scheme must be 'parula', 'EK500', 'echov', a vector of valid color names or a function generating valid color names")	 
     if ( scheme == "parula" )
	  cols <- pals::parula(nbcols)
	 if ( scheme == "EK500" )
      cols <- c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
	 "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28") 
	 if ( scheme == "echov" )
      cols <- c("#EBEBEB", "#E6E6E6", "#E1E1E1", "#DCDCDC", "#D7D7D7", "#D2D2D2",
	 "#CDCDCD", "#C8C8C8", "#C3C3C3", "#BEBEBE", "#B9B9B9", "#B4B4B4", "#AFAFAF",
	 "#AAAAAA", "#A5A5A5", "#A0A0A0", "#9B9B9B", "#969696", "#919191", "#8C8C8C",
	 "#878787", "#828282", "#7D7D7D", "#7878FF", "#6D6DFF", "#6262FF", "#5757FF",
	 "#4C4CFF", "#4141FF", "#3737FF", "#2C2CFF", "#2121FF", "#1616FF", "#0B0BFF",
	 "#0000FF", "#0000F6", "#0000ED", "#0000E3", "#0000DA", "#0000D1", "#0000C8",
	 "#0000BF", "#0000B6", "#0000AD", "#0000A3", "#00009A", "#000091", "#FF7878",
	 "#FF6E6E", "#FF6464", "#FF5A5A", "#FF5050", "#FF4646", "#FF3C3C", "#FF3232",
	 "#FF2828", "#FF1E1E", "#FF1414", "#FF0A0A", "#FF0000", "#F50000", "#EB0000",
	 "#E10000", "#D70000", "#CD0000", "#C30000", "#B90000", "#AF0000", "#A50000",
	 "#9B0000", "#910000")
  }
 
  fpal <- colorRampPalette(colors = cols)
  ans <- list(palette = fpal(nbcols), breaks = breaks)
  
  if (visu == TRUE){
     nb <- length(breaks)
     xl <- breaks[1:(nb-1)]
     xr <- breaks[2:nb]
     xlims <- range(breaks)
  
     plot(breaks, rep(0.5, nb), ylim=c(0, 1), bty="n", xaxt="n", yaxt="n", 
	   xlab="", ylab="", type="n" )
     rect(xl, 0, xr, 1, col=fpal(nbcols))
     axis(1, at=breaks, breaks, line=-0.7)
   }
  ans
}
