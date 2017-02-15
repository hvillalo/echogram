\name{read.echogram}
\alias{read.echogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Read echogram data from an HAC file
}
\description{
 This function imports from different tuples in the HAC file, the necessary information to visualise and analyse an echogram in R. 
}
\usage{
 read.echogram(hac, channel = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hac}{ name of an HAC file. }
  \item{channel}{ acoustic channel number. }
}
\details{
  This function calls internally other \code{echogram}'s functions (\code{postion.hac}, \code{navigation.hac} and \code{bottom.hac}) to import data from an HAC file. The possible values for the acoustic channel ranges from 1 to \code{n}, where \code{n} is the number of frequencies used during data acquisition. When \code{channel = 1} (the default), data from the lowest acoustic frequency is imported, while \code{channel = n} refers to the highest frequency present in the HAC file. A text string with the frequency value (in kilohertz) is stored as an attribute of the Sv matrix (see examples below). 
}
\value{
 An object of  \code{class} ``echogram'' (a list) with components:
  \item{depth}{a vector of mean sample depth (in m) of length p.}
  \item{Sv}{a p by k matrix of sampled values, currently the mean volume backscattering strength (Sv, in dB).}
  \item{pings}{a k by four data frame with ping time, detected bottom depth, vessel speed and cummulated traveled distance.}
}
\references{
 ICES, 2005. Description of the ICES HAC Standard Data Exchange Format, Version 1.60. Technical Report 278, ICES Cooperative Research Report.
}
\author{
Héctor Villalobos
}
\note{
Currently, \code{read.echogram} has been successfully tested importing HAC data from the following ping tuples: 10000 (U-32); 10030 (U-16) and 10040 (C-16).
}

\examples{
hacfile <- system.file("hac", "D20150510-T202221.hac", package = "echogram")
echo1 <- read.echogram(hacfile, channel = 1)
class(echo1)
str(echo1)
attr(echo1$Sv, "frequency")
echogram(echo1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }

