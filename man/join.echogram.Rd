\name{join.echogram}
\alias{join.echogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge echograms
}
\description{
 This function allows to join two echograms.
}
\usage{
join.echogram(echogram1, echogram2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{echogram1}{an object of \code{class} ``echogram'' as returned by \code{\link{read.echogram}}. }
  \item{echogram2}{an object of \code{class} ``echogram'', preferentially contiguous in space and time with \code{echogram1} above.}
}
\details{
 This function is designed to join echograms of the same acoustic frequency, giving an error if frequencies differ. Desirably, echograms should be contiguous in space and time, but as this is not verified, it is possible to join non-contiguous echograms.
}
\value{
An object of \code{class} ``echogram'' resulting from the merging operation.
}
\author{
Héctor Villalobos
}
\examples{
# import 38 kHz data from two consecutive HAC files
hacfile1 <- system.file("hac", "D20150510-T202221.hac", package = "echogram")
echo1.038 <- read.echogram(hacfile1, channel = 1)

hacfile2 <- system.file("hac", "D20150510-T202500.hac", package = "echogram")
echo2.038 <- read.echogram(hacfile2, channel = 1)

# join into one echogram
echo.038 <- join.echogram(echo1.038, echo2.038)
str(echo.038)
echogram(echo.038)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

