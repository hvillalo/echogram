\name{sample.echogram}
\alias{sample.echogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Select and sample data values from an echogram
}
\description{
 This function allows to select individual pixels from an echogram and returns the Sv value, ping time and depth of the sampled pixel.
}
\usage{
sample.echogram(echogram, plot = TRUE, coords = NULL, col = "black") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{echogram}{an object of \code{class} ``echogram'' as returned by \code{\link{read.echogram}}.}
  \item{plot}{logical. If \code{TRUE} (the default), the echogram to be sampled is plotted. }
  \item{coords}{ (x, y) coordinates (in plot units) of the desired samples. They could result from previous sampling of another frequency. }
  \item{col}{ color for the sampled points pixels. }
}
\details{
 The selection of pixels to sample can be done by clicking on the echogram or by passing the coordinates of the desired pixels to the function. The coordinates should be in plot units, and therefore, these typically come from a previous selection by clicking on another frequency's echogram (see examples). 
 
 \code{sample.echogram} makes use of \code{locator} function, and therefore it only works in devices supported by the latter, such as \code{X11}, \code{windows} and \code{quartz}.
}
\value{
A data frame with seven variables:
  \item{ id }{ pixel id. }
  \item{ x }{ x coordinate in plot units. }
  \item{ y }{ y coordinate in plot units. }
  \item{ d }{ distance in plot units from the selected location to a valid pixel. }
  \item{ pingTime }{ time of sampled  ping. }
  \item{ depth}{ depth of the sample. }
  \item{ Sv }{ Sv value. }
}
\author{
Héctor Villalobos
}

\examples{

# import 38 and 120 kHz data from an HAC file 
hacfile <- system.file("hac", "D20150510-T202500.hac", package = "echogram")
echo2.038 <- read.echogram(hacfile, channel = 1)
echo2.120 <- read.echogram(hacfile, channel = 2)

# plot 38 kHz echogram 
echogram(echo2.038)

\dontrun{ 
# select points coordinates with the mouse 
# click to select several locations and escape when done
pts038 <- sample.echogram(echo2.038) 
pts038

# plot 120 kHz echogram 
echogram(echo2.120)

# use the points previously selected for 38 kHz
pts120 <- sample.echogram(echo2.120, coords = pts038[ , 2:3])
pts120
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }


