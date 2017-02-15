\name{trim.echogram}
\alias{trim.echogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trim an echogram vertically or horizontally
}
\description{
 This function allows to trim an echogram by depth or ping number by actually trimming the underlying data matrices and vectors.
}
\usage{
trim.echogram(echogram, depth.max = NULL, ping.ini = 1, ping.end = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{echogram}{ an object of \code{class} ``echogram'' as returned by \code{\link{read.echogram}}. }
  \item{depth.max}{ maximum depth to keep in the echogram. }
  \item{ping.ini}{ start ping to keep. }
  \item{ping.end}{ end ping to keep. }
}
\details{
 This funcion has been conceived to discard undesired data below a given depth (e.g. the sea bottom), therefore, the initial depth is always the surface, so the vertical trimming is limited to select the maximum depth.  
 }
\value{
 An object of  \code{\link{class}} ``echogram''.
}
\author{
Héctor Villalobos
}

\examples{
# import 38 kHz data from an HAC file 
hacfile <- system.file("hac", "D20150510-T202500.hac", package = "echogram")
echo2.038 <- read.echogram(hacfile, channel = 1)

# echogram by default
echogram(echo2.038)

# trim the echogram
echo.tmp <- trim.echogram(echo2.038, depth.max = 70, ping.end = 250)
echogram(echo.tmp)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

