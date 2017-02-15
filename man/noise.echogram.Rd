\name{noise.echogram}
\alias{noise.echogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modelling ambient noise in echograms 
}
\description{
 This function allows to estimate a model of the background noise in an echogram by fitting the equation proposed by De Robertis and Higginbottom (2007).
}
\usage{
noise.echogram(echogram, ping = NULL, dB1m = NULL, alpha = NULL, plot = TRUE, out = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{echogram}{an object of \code{class} ``echogram''.}
  \item{ping}{ ping number for which the Sv values are to be modeled.}
  \item{dB1m}{ noise level at 1m from the face of the transducer.}
  \item{alpha}{ absortion coefficient of sound in sea water for echogram's frequency. }
  \item{plot}{ logical. If  \code{TRUE} (the default) a plot of the data and adjusted model is produced.}
  \item{out}{ logical. If  \code{TRUE} an echogram with the modeled noise is returned. }
}
\details{
The estimation of an ambient noise model for a particular acoustic frequency, eventually allows the ``cleaning'' of echograms by subtracting this noise. 
}
\value{
When \code{plot = TRUE} and \code{out = FALSE} (the default), only a plot is produced. With \code{out = TRUE}, the function returns an object of  \code{class} ``echogram'' with the noise modelled.
}
\references{
De Robertis, A. and I. Higginbottom. 2007. A post-processing technique to estimate the signal-to-noise ratio and remove echosounder background noise 64:1282–1291.
}
\author{
Héctor Villalobos
}

\examples{
# load echogram for noise estimation at 120 kHz (deep waters, no scatterers)
data("echo.noise")
attr(echo.noise$Sv, "frequency")
echogram(echo.noise, xref = "ping")

# a first look to the Sv values at ping 2
noise.echogram(echo.noise, ping = 2)

# To better adjust the model, we need to provide the absortion coefficient for 120 kHz and adjust
# the dB1m parameter. For this example, using data from a nearby CTD profile, alpha was calculated
# as being 0.03550554, while -131 dB is choosen for dB1m
noise <- noise.echogram(echo.noise, ping = 2, dB1m = -131, alpha = 0.03550554, out = TRUE)
echogram(noise)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

