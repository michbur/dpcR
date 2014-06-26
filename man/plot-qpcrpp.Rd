\name{plot-qpcrpp}
\alias{plot.qpcrpp}
\alias{plot,qpcrpp-method}

\title{
Plot \code{qpcrpp} objects
}
\description{

}
\usage{
\S4method{plot}{qpcrpp}(x, mincyc = 1, maxcyc = 45, rug = TRUE)
}

\arguments{
  \item{x}{
is a \code{\linkS4class{qpcrpp}} object.
}
  \item{mincyc}{
  is the first cycle to start the plot from.
}
  \item{maxcyc}{
   the the last cycle for the plot.
}
  \item{rug}{
  Adds a rug representation of the data to the plot.
} 
}


\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{
\code{\linkS4class{qpcrpp}}
}
\examples{
Cq.range <- c(20, 30)
ranged <- limit_cy0(data = test, cyc = 1, fluo = NULL,
                     Cq_range = Cq.range, model = l5)
                     
plot(qpcr2pp(ranged[,1], ranged[,2], delta = 5), rug = TRUE)
}
\keyword{hplot}