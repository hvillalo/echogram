\name{match.echogram}
\alias{match.echogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Match ping times from two echograms
}
\description{
This function verifies ping times between corresponding echograms from two frequencies, eliminating non-matching and duplicated pings.
}
\usage{
match.echogram(echogram1, echogram2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{echogram1}{an object of \code{class} ``echogram'' as returned by \code{\link{read.echogram}}. }
  \item{echogram2}{an object of \code{class} ``echogram'' from a different acoustic frequency than \code{echogram1} above.}
}
\details{
 Corresponding echograms refers to data acquired at the same time with different acoustic frequencies. Unmatching pings, i.e. those present in only one frequency, and duplicated pings, are identified by it's associated time and subsequently eliminated.
}
\value{
A list with the two matched echograms.
}
\author{
Héctor Villalobos and Violeta E. González-Maynez
}
\seealso{ \code{\link{add.echogram}} }
\examples{

# import 38 and 120 kHz data from an HAC file 
hacfile <- system.file("extdata", "D20150510-T202221.hac", package = "echogram")
echo1.038 <- read.echogram(hacfile, channel = 1)
echo1.120 <- read.echogram(hacfile, channel = 2)

# Sv matrices have different number of pings
dim(echo1.038$Sv); dim(echo1.120$Sv)

# Apply match ping times
tmp <- match.echogram(echo1.038, echo1.120)

# split the list in the two echograms 
echo1.038 <- tmp$echogram1
echo1.120 <- tmp$echogram2

# number of pings and ping times are now the same for both frequencies
dim(echo1.038$Sv); dim(echo1.120$Sv)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

