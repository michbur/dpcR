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

}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} with three columns containing: number of cycles, 
    amplification curves TODO}
    \item{\code{mu}:}{expected number of events in defined interval.}
    \item{\code{CT}:}{\code{"numeric"} value of the "average time" between the occurence of a positive reaction and another positive reaction.}
    \item{\code{partitions}:}{\code{"integer"} value equal to the number of partitions.}
    \item{\code{events}:}{\code{"integer"} value equal number of events (positive partitions taken to further analysis)}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(object = "qpcrpp")}: ... }
    \item{summary}{\code{signature(object = "qpcrpp")}: ... }
    \item{show}{\code{signature(object = "qpcrpp")}: ... }
   }
}

\author{
Stefan Roediger, Michal Burdukiewicz.
}

\seealso{
\code{\link{plot-qpcrpp}},
}
\keyword{classes}