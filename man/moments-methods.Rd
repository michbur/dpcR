\name{moments-methods}
\docType{methods}
\alias{moments-methods}
\alias{moments,adpcr-method}
\alias{moments,ddpcr-method}
\alias{moments,numeric-method}
\alias{moments}
\title{
Calculate Moments of Poisson Distribution
}
\description{
The function allows user to quickly calculate moments of Poisson distributions. The calculations
are based on values of positive and total partitions or theoretical lambda value.
}
\usage{
\S4method{moments}{adpcr}(input)
\S4method{moments}{ddpcr}(input)
\S4method{moments}{numeric}(input)
}
\arguments{
  \item{input}{
a single numeric object (lambda) or a two element vector (first element is treated as the number 
of positive partitions and the second as the number of total partitions) or an object of classes 
\code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}.
}
}
\value{
A named vector of four elements (mean, variance, skewness and kurtosis). In case of 
\code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}} containing results from more than one 
experiment, a n*4 matrix, where n is the number of experiments.
}

\author{
Michal Burdukiewicz.
}

\note{
Four first moments of a Poisson distribution.

Mean : \eqn{\lambda}{lambda}.

Variance: \eqn{\lambda}{lambda}.

Skewness: \eqn{\sqrt{\lambda}}{lambda^(-0.5)}.

Kurtosis: \eqn{\frac{1}{\lambda}}{lambda^(-1)}.
}

\examples{
# moments for lambda = 2
moments(2)

# moments for 100 positive partitions of 765 total partitions
moments(c(100, 765))

# calculate moments for an array digital PCR
adpcr <- sim_adpcr(m = 10, n = 40, times = 50, pos_sums = FALSE, n_panels = 50)
moments(adpcr)
}

\keyword{ moments }
\keyword{ Mean }
\keyword{ Variance }
\keyword{ Skewness }
\keyword{ Kurtosis }
