\name{echogram}
\alias{echogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Echogram visualisation 
}
\description{
 This function allows to produce echogram visualisations from imported hac data. The user can define the visualisation Sv threshold and select between two built-in color schemes or define a custom scheme.  
}
\usage{
echogram(echogram, Svthr = -70, Svmax = 0, col.sep = NULL, col.nb = NULL,
         scheme = NULL, depth.grid = NULL, x.grid = NULL, 
         x.ref = c("pings", "nmi", "seconds"), seabed = FALSE, 
         depth.max = NULL, ping.ini = NULL, ping.max = NULL, colbar=TRUE, 
         main = NULL, tformat = "\%H:\%M", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{echogram}{an object of \code{class} ``echogram'' as returned by \code{\link{read.echogram}}.}
  \item{Svthr}{ Sv visualisation threshold, in decibels (dB). }
  \item{Svmax}{ maximum Sv visualisation value, in dB. }
  \item{col.sep}{ separation between colors. Defaults to 1 dB. }
  \item{col.nb}{ number of colors. }
  \item{scheme}{ color scheme for echogram, ``parula'' (the default), ``EK500'', or  ``echov''. It can also be a vector of valid color names, or a function generating color names.} 
  \item{depth.grid}{ spacing between depth labels (in m). } 
  \item{x.grid}{ spacing between labels in the horizontal dimension according to x.ref.}  
  \item{x.ref}{ horizontal reference in echogram: ``pings'' (the default), ``nmi'' or ``seconds''.}
  \item{seabed}{logical. When \code{TRUE} and data on detected bottom is present, a line is added to the echogram. } 
  \item{depth.max}{maximum depth to visualise.} 
  \item{ping.ini}{initial ping to visualise.} 
  \item{ping.max}{last ping to visualise.} 
  \item{colbar}{ logical. If \code{TRUE} a color bar is added to the echogram. }
  \item{main}{ the acoustic frequency, by default.}
  \item{tformat}{ time format for annotating when horizontal dimension when x.ref = 'seconds'.} 
  \item{\dots}{other options to \code{image}.}
}
\details{
 Besides the two built-in color schemes, the user can define its own by specifying a vector of valid color names (see examples). This function uses \code{imageScale} function from \code{sinkr} package by Marc Taylor.
}
\author{
Héctor Villalobos
}


\seealso{
\code{\link{palette.echogram}}.
}
\examples{
# import hac file
hacfile <- system.file("extdata", "D20150510-T202500.hac", package = "echogram")
echo2.038 <- read.echogram(hacfile)

# echogram by default 
echogram(echo2.038)

# using alternative color schemes
echogram(echo2.038, Svthr = -70, col.sep = 1.5, scheme = "EK500")
echogram(echo2.038, Svthr = -70, col.sep = 3, scheme = c("white", "blue", "grey", "black"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

