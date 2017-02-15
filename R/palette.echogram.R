palette.echogram <-
function(Svthr = -70, Svmax = 0, col.sep = 1, scheme = "echov", visu = FALSE)
{
  if ( missing(scheme) ) scheme <- "echov"
  if ( length(scheme) > 1 ) {
     cols <- scheme 
  } else {
     if ( !scheme %in% c("echov", "EK500") )
      stop ("scheme must be 'echov', 'EK500' or a vector of valid color names")	 
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
     if ( scheme == "EK500" )
      cols <- c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
	 "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28")
  }
 
  fpal <- colorRampPalette(colors = cols)
  breaks <- seq(Svthr, Svmax, by = col.sep)
  nbcols = length(breaks) - 1
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
