\name{qpcrpp-class}
\docType{class}
\alias{qpcrpp-class}
\alias{qpcrpp}
\alias{show.qpcrpp}
\alias{show,qpcrpp-method}
\alias{summary.qpcrpp}
\alias{summary,qpcrpp-method}

\title{Class \code{"qpcrpp"}}
\description{
An object representing digital PCR reaction depicted as Poisson process.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} with three columns containing: number of cycles, 
    amplification curves and cumulative sum of events.}
    \item{\code{mu}:}{\code{"numeric"} of the expected number of events in defined interval.}
    \item{\code{CT}:}{\code{"numeric"} value of the "average time" between the occurence of a
    positive reaction and another positive reaction.}
    \item{\code{partitions}:}{\code{"integer"} value equal to the number of partitions.}
    \item{\code{events}:}{\code{"integer"} value equal number of events (positive partitions 
    taken to further analysis)}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(object = "qpcrpp")}: plots Poisson process. See 
    \code{\link[dpcR]{plot.qpcrpp}}}
    \item{summary}{\code{signature(object = "qpcrpp")}: prints pretty summary of the object.}
    \item{show}{\code{signature(object = "qpcrpp")}: shows \code{.Data} slot of the object.}
   }
}

\author{
Stefan Roediger, Michal Burdukiewicz.
}

\seealso{
\code{\link{plot.qpcrpp}},
}
\keyword{classes}