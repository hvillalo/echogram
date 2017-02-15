\name{add.echogram}
\alias{add.echogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add two echograms
}
\description{
 This function allows addition or subtraction of Sv data matrices of corresponding echograms from two frequencies in the linear or logarithmic domain.
}
\usage{
add.echogram(echogram1, echogram2, operator = c("plus", "minus"), 
    domain = c("linear", "dB"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{echogram1}{an object of \code{class} ``echogram'' as returned by \code{\link{read.echogram}}.}
  \item{echogram2}{an object of \code{class} ``echogram'' from a different acoustic frequency than \code{echogram1} above.}
  \item{operator}{ a string indicating addition (``plus'') or subtraction (``minus''). May be abbreviatted. }
  \item{domain}{ a string indicating the domain where the operation will be performed (see details). }
}
\details{
Corresponding echograms refers to data acquired at the same time with different acoustic frequencies. In order to add echograms, the Sv data matrices must have the same dimensions. If they don't, \code{\link{match.echogram}} can be used for this purpose. It is also important to mask undesired echoes beforehand, as those belonging  to the bottom and below. When \code{domain = "dB"} (the default), the Sv matrices are added as they are. When \code{domain = "linear"}, the Sv values are transformed with 10^(Sv/10) before addition, and the result (X) is then back transformed to dB (10*log10(X)).
}
\value{
An object of \code{class} ``echogram'' where the Sv component is the result of  the performed operation.
}
\author{
Héctor Villalobos
}
\seealso{
\code{\link{match.echogram}}, \code{\link{mask.echogram}}
}
\examples{
# import 38 and 120 kHz data from an HAC file 
hacfile <- system.file("hac", "D20150510-T202500.hac", package = "echogram")
echo2.038 <- read.echogram(hacfile, channel = 1)
echo2.120 <- read.echogram(hacfile, channel = 2)

\dontrun{
# attempting to add the two frequencies with unequal number of pings gives an error
add.echogram(echo2.038, echo2.120, "plus", "dB")
}

# running match.echogram() solves this
tmp <- match.echogram(echo2.038, echo2.120)
str(tmp) # both frequencies are in a list, need to split
echo2.038 <- tmp$echogram1
echo2.120 <- tmp$echogram2

# we don't want to add bottom echoes, mask bottom and surface from both frequencies
echo2.038m <- mask.echogram(echo2.038, surf.off = 2, bott.off = 0.2)
echo2.120m <- mask.echogram(echo2.120, surf.off = 2, bott.off = 0.2)

# adding Sv values and plot result
echo.sum <- add.echogram(echo2.038m, echo2.120m, "plus", "dB")
Min <- min(as.vector(echo.sum$Sv), na.rm=TRUE) # useful to set Sv threshold
echogram(echo.sum, Svthr=floor(Min), scheme = "EK500")

# subtract 38 from 120 kHz
echo.minus <- add.echogram(echo2.120m, echo2.038m, "minus", "dB")
Min <- min(as.vector(echo.minus$Sv), na.rm=TRUE)
echogram(echo.minus, Svthr=floor(Min), scheme = "EK500")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }

