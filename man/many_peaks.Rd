\name{many_peaks}
\alias{many_peaks}
\docType{data}
\title{
Artificial data set with many peaks
}
\description{
A set of data from an artificial droplet flow experiment. It contains various kinds of peaks - 
positive peaks, negative peaks and peak-like noise.
}
\usage{data(many_peaks)}
\format{
  A data frame with 493 observations on the following 2 variables.
  \describe{
    \item{\code{t}}{a numeric vector}
    \item{\code{F}}{fluorescence value}
  }
}

\examples{
data(many_peaks)
plot(many_peaks, type = "b")
}
\keyword{datasets}
