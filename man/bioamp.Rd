\name{bioamp}
\alias{bioamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to analyze plot the raw data from a BioRad droplet digital PCR
experiment
}
\description{
\code{bioamp} is a function to plot and analyze the amplitude data of a 
BioRad droplet digital PCR experiment.
}
\usage{
bioamp(data = data, amp_x = 1, amp_y = 2, cluster = 3,
       robust = TRUE, plot = TRUE, stat = TRUE, 
       xlab = "Assay 1 Amplitude", 
       ylab = "Assay 2 Amplitude", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
object of class \code{what} containing the amplitude data.
}
  \item{robust}{
Is the method used to calculate the location (mean or median) and 
dispersion (standard deviation or median absolute deviation).
}
  \item{plot}{
logical, if \code{TRUE}, the plot is printed.
}
  \item{amp_x}{
is the first amplitude (x-axis).
}
  \item{amp_y}{
is the second amplitude (y-axis).
}
  \item{cluster}{
are the clusters of the plot.
}
  \item{stat}{
logical, if \code{TRUE}, the statistics of the droplet digital PCR
experiment are calculated.
}
  \item{xlab}{
x-label of the plot.
}
  \item{ylab}{
y-label of the plot.
}
  \item{\dots}{
other arguments passed to the \code{plot} function (see \code{plot.default} for 
details).
}
}
\author{
Stefan Roediger, Michal Burdukiewcz
}
\examples{
par(mfrow = c(1,2))
bioamp(data = pds_raw[["A01"]], main = "Well A01", pch = 19)
bioamp(data = pds_raw[["A02"]], main = "Well A02", pch = 19)
par(mfrow = c(1,1))
}
\keyword{ Amplitude }
\keyword{ BioRad }
