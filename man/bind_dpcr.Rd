\name{bind_dpcr}
\alias{bind_dpcr}
\alias{bind_dpcr,adpcr}
\alias{bind_dpcr,adpcr-method}

\title{
Bind dpcr objects
}
\description{
BLA
}
\usage{
\S4method{test_ratio}{adpcr}(input, ...)
}
\arguments{
  \item{input}{
an object of class \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}.
}
  \item{...}{
objects of class \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}. See Details.
}
}
\details{
\code{bind_dpcr} can bind (analogously to \code{\link[base]{cbind}}) two or more 
\code{adpcr} or \code{ddpcr} objects, but without recycling. In case on unqueal length,
shorter objects will be filled in with additional \code{NA} values.
}

\value{
An object of class \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}, 
depending on the input.
}

\author{
Michal Burdukiewicz
}

\seealso{
\code{\link{extract_dpcr}}
}

\examples{
bigger_array <- sim_adpcr(400, 765, 1000, pos_sums = FALSE, n_panels = 5)
smaller_array <- sim_adpcr(100, 700, 1000, pos_sums = FALSE, n_panels = 3)
res <- bind_dpcr(bigger_array, smaller_array)
}

\keyword{ manip }

