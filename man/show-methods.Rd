\name{show-methods}
\docType{methods}
\alias{show-methods}
\alias{show-methods}
\alias{show,adpcr-method}
\alias{show,ddpcr-method}
\alias{show}
\title{Methods for Function \code{show}}
\description{
Expands function \code{\link[methods]{show}} allowing showing objects of the class 
\code{\linkS4class{adpcr}} to or \code{\linkS4class{ddpcr}}.
}
\usage{
\S4method{show}{adpcr}(object)
\S4method{show}{ddpcr}(object)
}
\arguments{
  \item{object}{an object of class \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}.}
}
\author{
Michal Burdukiewicz.
}

\examples{
#array dpcr
ptest <- sim_adpcr(400, 765, 5, FALSE, n_panels = 1)
show(ptest)

#multiple experiments
ptest <- sim_adpcr(400, 765, 5, FALSE, n_panels = 5)
show(ptest)

#droplet dpcr - fluorescence
dropletf <- sim_ddpcr(7, 20, times = 5, fluo = list(0.1, 0))
show(dropletf)

#droplet dpcr - number of molecules
droplet <- sim_ddpcr(7, 20, times = 5)
show(droplet)
}
\keyword{methods}
\keyword{utilities}

