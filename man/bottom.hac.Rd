\name{bottom.hac}
\alias{bottom.hac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Read detected bottom range from an HAC file
}
\description{
 This function imports, for a given acoustic channel, the detected bottom range stored in the ping tuple of an HAC file.
}
\usage{
bottom.hac(hac, channel = 1, plot = FALSE, maxDepth = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hac}{ name of an HAC file. }
  \item{channel}{ acoustic channel number. }
  \item{plot}{ logical. if \code{TRUE} a plot is produced. }
  \item{maxDepth}{ maximum depth (in m) represented in the plot. }
}
\details{
 The possible values for the acoustic channel ranges from 1 to \code{n}, where \code{n} is the number of frequencies stored during data acquisition. The argument \code{channel = 1}, refers to the lowest acoustic frequency in the HAC file (the default), while \code{channel = n} will be the highest frequency in the file. When a graphical representation is desired (\code{plot = TRUE}), the maximum displayed in the echogram depth can be provided as a negative integer with argument \code{maxDepth}.
}
\value{
 A data frame with two variables where every row represents one emitted ping:
  \item{ pingTime }{time of emitted ping.}
  \item{ detBottom }{detected depth range in m.}
}
\author{
Héctor Villalobos
}
\examples{
hacfile <- system.file("hac", "D20150510-T202221.hac", package="echogram")
bottom.hac( hacfile )
bottom.hac( hacfile, plot = TRUE )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }

