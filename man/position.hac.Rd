\name{position.hac}
\alias{position.hac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Read geographic position data from an HAC file
}
\description{
 This function imports time and geographic positions recorded by a GPS in an HAC file during data acquisition.
}
\usage{
 position.hac(hac)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hac}{ name of an HAC file }
}
\details{
 The function looks for the Position tuple (20) in the HAC file, and if found, imports the time, latitude and longitude of GPS fixes stored in the digital echogram, as well as the CPU time of the acquisition PC.  
}
\value{
 A data frame with four variables:
 \item{time.gps}{date and time from the GPS during data acquisition.}
 \item{time.cpu}{date and time from the computer CPU during data acquisition.}
 \item{lon}{longitudes.}
 \item{lat}{latitudes.}
}
\references{
 ICES, 2005. Description of the ICES HAC Standard Data Exchange Format, Version 1.60. Technical Report 278, ICES Cooperative Research Report.
}
\author{
Héctor Villalobos
}
\note{
If during acoustic data acqusition the PC clock is set to UTC time, as recomended, time.gps and time.cpu will be approximately equal, because a fraction of a second is added to time.cpu to obtain a precision of 0.0001 s.
}



\seealso{
 \code{\link{navigation.hac}}
}
\examples{
 hacfile <- system.file("hac", "D20150510-T202221.hac", package="echogram")
 pos <- position.hac( hacfile )
 pos
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
