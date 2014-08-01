\name{rtadpcr-class}
\docType{class}
\alias{rtadpcr-class}
\alias{rtadpcr}

\title{Class \code{"rtadpcr"} - real-time array digital PCR experiments}
\description{
A class designed to contain results from real-time array digital PCR 
experiments. Data is represented as matrix, where each column describes different 
measurement point (i.e. cycle number) and every row different partition.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} containing data from array. See Description.}
    \item{\code{n}:}{Object of class \code{"integer"} equal to the number of partitions.}
    \item{\code{type}:}{Object of class \code{"character"} defining type of data.}
  }
}
\section{Methods}{
TO DO
}

\section{Note}{
\code{"rtadpcr"} objects are suitable only for real-time digital PCR experiments, where
measurements are taken in real time during the amplification reaction.
}

\author{
Michal Burdukiewicz.
}

\seealso{
End-point array digital PCR:
\code{\linkS4class{adpcr}}.

Droplet digital PCR:
\code{\linkS4class{ddpcr}}.
}
\examples{
#none
}
\keyword{classes}
