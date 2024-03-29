% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.EK_bot.R
\name{read.EK_bot}
\alias{read.EK_bot}
\title{Read bot files from Simrad EK60 echosounder}
\usage{
read.EK_bot(file)
}
\arguments{
\item{file}{EK60 bot file name.}
}
\value{
A data frame with ping times and detected bottom depth for 
every transceiver in the file.
}
\description{
This function will read Simrad BOT0 datagrams with bottom depth data.
}
\details{
This function read the bot file by calling \code{read.EK_raw}, 
and \code{get_dgIdx} functions. From the datagram index, it finds the BOT0
datagrams, and then extracts ping times and depth(s) associated with 
all the transceiver(s) in the file.
}
\examples{
fn <- system.file("extdata", "demo-D20130504-T083828.bot", package = "echogram")
depth <- read.EK_bot(fn)
head(depth)
plot(depth[, 1:2], type = "l", ylim = rev(c(0, max(depth[,2]))))

}
\seealso{
\code{read.EK_idx}.
}
\author{
Héctor Villalobos.
}
