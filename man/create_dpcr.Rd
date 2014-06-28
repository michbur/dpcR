\name{create_dpcr}
\alias{create_dpcr}

\title{
Create dpcR object
}
\description{
Quick creation of \code{\linkS4class{adpcr}} and\code{\linkS4class{ddpcr}} 
objects.
}
\usage{
create_dpcr(data, n, threshold = NULL, breaks = NULL, type, adpcr = TRUE)
}

\arguments{
  \item{data}{
  a \code{"numeric"} vector or matrix of data from dPCR experiments.
  Data frames will be converted to matrices.
}
  \item{n}{
  \code{"integer"} equal to number of partitions.
}
  \item{threshold}{
  \code{"numeric"} value giving the threshold above which droplet is 
counted as positive.
  Ignored if \code{adpcr} is \code{TRUE}.
}
  \item{breaks}{
  \code{"numeric"} vector giving the number of intervals into which 
\code{data} should be cut.
  Ignored if \code{adpcr} is \code{FALSE}.
}
  \item{type}{
  Object of class \code{"character"} defining type of data. Could be 
    \code{"nm"} (number of molecules per partition), \code{"tp"} (total 
number of positive wells 
    in panel), \code{"fluo"} (fluorescence) or \code{"ct"} (threshold 
cycle).
}
  \item{adpcr}{
  logical. If \code{TRUE}, function creates \code{\linkS4class{adpcr}} 
object. 
  If \code{FALSE}, function creates \code{\linkS4class{ddpcr}} object.
}
}
\details{
This function assists in creation of objects used by other functions of the 
package. Its 
in build capabilities include checking the correctness of arguments.

A warning is prompted whenever any of arguments is converted to other type.
}
\value{
An \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}} object.
}

\note{
Currently only end point measurements are supported.
}
\author{
Michal Burdukiewicz, Stefan Roediger.
}
\note{
It is still possible (although not recomended) to create 
\code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}} objects manually 
by \code{\link{new}} function.
}

\examples{
# Droplet digital PCR example
sample_runs <- matrix(rpois(60, lambda = 1.5), ncol = 2)
ddpcr1 <- create_dpcr(sample_runs[,1], n = 30L, 
threshold = 1, type = "nm", adpcr = FALSE)
ddpcr2 <- create_dpcr(sample_runs[,2], n = 30L, 
threshold = 1, type = "nm", adpcr = FALSE)
plot_vic_fam(ddpcr1, ddpcr2)

# Array digital PCR example
sample_adpcr <- create_dpcr(rpois(765, lambda = 0.8), n = 765L, 
			    type = "nm", adpcr = TRUE)
plot_panel(sample_adpcr, 45, 17)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ddPCR }
\keyword{ adPCR }
