\name{navigation.hac}
\alias{navigation.hac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Compute bearing, navigated distance and speed
}
\description{
 This function computes navigation course (bearing), navigated distance, time diference and navigation speed between GPS fixes in position data imported from an HAC file.
}
\usage{
navigation.hac(pos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos}{ geographic position data from an HAC file, as imported with \code{position.hac}. }
}
\details{
 The bearing and navigated distance are computed with functions  \code{bearingRhumb} and \code{distVincentyEllipsoid}  from package \code{geosphere}. This function is intended to be called inside \code{read.echogram}, rather than being used directly.
}
\value{
 A data frame with seven variables:
 \item{time.cpu}{date and time from the computer CPU during data acquisition.}
 \item{lon}{longitudes.}
 \item{lat}{latitudes.}
 \item{bearing}{navigation course between two consecutive GPS fixes.}
 \item{navdist}{navigated distance between two consecutive GPS fixes.}
 \item{time.dif}{time difference between two consecutive GPS fixes.}
 \item{navspeed}{navigation speed between two consecutive GPS fixes.}
}
\author{
 Héctor Villalobos
}

\seealso{
 \code{\link{position.hac}}, \code{\link{bearingRhumb}}, \code{\link{distVincentyEllipsoid}}.
}
\examples{
 hacfile <- system.file("hac", "D20150510-T202221.hac", package="echogram")
 pos <- position.hac( hacfile )
 pos
 pos2 <- navigation.hac(pos)
 pos2
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

