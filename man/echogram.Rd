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
echogram(echogram, xref = c("distance", "time", "ping"), scheme = "echov", 
  Svthr = -80, Svmax = NULL, col.sep = 1, colbar = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{echogram}{an object of \code{class} ``echogram'' as returned by \code{\link{read.echogram}}.}
  \item{xref}{ horizontal reference in echogram: ``distance'' (the default), ``time'' or ``ping''.}
  \item{scheme}{ color scheme for echogram, either: ``echov'' (the default) or ``EK500''. It can also be a vector of valid color names.}
  \item{Svthr}{ Sv visualisation threshold, in decibels (dB). }
  \item{Svmax}{ maximum Sv visualisation value, in dB. }
  \item{col.sep}{ separation between colors in dB.}
  \item{colbar}{ logical. If \code{TRUE} a color bar is added to the echogram. }
  \item{\dots}{other options to \code{image}.}
}
\details{
 Besides the two built-in color schemes, the user can define its own by specifying a vector of valid color names (see examples).
}
\author{
Héctor Villalobos
}


\seealso{
\code{\link{palette.echogram}}.
}
\examples{
# import hac file
hacfile <- system.file("hac", "D20150510-T202500.hac", package = "echogram")
echo2.038 <- read.echogram(hacfile, channel = 1)

# echogram by default 
echogram(echo2.038)

# using alternative color schemes
echogram(echo2.038, Svthr = -70, col.sep = 1.5, scheme = "EK500")
echogram(echo2.038, Svthr = -70, col.sep = 3, scheme = c("white", "blue", "grey", "black"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

