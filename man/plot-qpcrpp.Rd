\name{plot-qpcrpp}
\alias{plot.qpcrpp}
\alias{plot,qpcrpp-method}

\title{
Plot \code{qpcrpp} objects
}
\description{
An analytical plot describing relationship between the cycle number and the 
current value of Poisson mean. The plot can be used for quality control of
process.
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
\details{
The \code{rug} parameter allows user to add density of the number of events 
to the plot. 
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{
\code{\linkS4class{qpcrpp}}
}
\examples{

test <- cbind(reps[1L:45, ], reps2[1L:45, 2L:ncol(reps2)], reps3[1L:45, 
	      2L:ncol(reps3)])
ranged <- limit_cq(data = test, cyc = 1, fluo = NULL, model = l5)
        
plot(qpcr2pp(ranged[, 1], ranged[, 2], delta = 5), rug = TRUE)
}
\keyword{hplot}