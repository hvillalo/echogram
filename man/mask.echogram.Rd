\name{mask.echogram}
\alias{mask.echogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Mask an echogram
}
\description{
 This function creates, and optionally applies, a mask by ``blanking'' portions of the Sv data matrix of an echogram. 
}
\usage{
mask.echogram(echogram, surf.off = NULL, bott.off = NULL, mask = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{echogram}{ an object of \code{class} ``echogram'' as returned by \code{\link{read.echogram}}. }
  \item{surf.off}{ surface offset in m defining the upper layer (refered to the surface) to be masked. }
  \item{bott.off}{ bottom offset in m defining the bottom layer (refered to the bottom) to be masked. }
  \item{mask}{ logical. If \code{FALSE}, the function returns a masking matrix. If \code{TRUE} (the default), the function returns a masked echogram (see details).}
}
\details{
 The masking process consists in producing a matrix of the same dimensions as the original Sv data matrix with \code{NA}'s in the masked portion and 1's otherwise. The product of both matrices gives the masked echogram.
}
\value{
 When \code{mask = FALSE}, a masking matrix is returned. When \code{mask = TRUE} (the default), an object of \code{class} ``echogram'' with the mask applied.
}
\author{
Héctor Villalobos
}

\examples{

# import 38 kHz data from HAC file
hacfile <- system.file("hac", "D20150510-T202500.hac", package = "echogram")
echo2.038 <- read.echogram(hacfile, channel = 1)

# make a copy of the original echogram
tmp <- echo2.038

# Create a mask, which is a matrix with 1's and NA's 
mask <- mask.echogram(tmp, surf.off = 1, bott.off = -1, mask = FALSE)
image(t(mask[nrow(mask):1, ])) 

# Apply mask to echogram
tmp$Sv <- tmp$Sv * mask 
echogram(tmp)

# By default, the function returns the masked echogram
echo2.038mask <- mask.echogram(echo2.038, surf.off = 2, bott.off = 0.2)
echogram(echo2.038mask)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

