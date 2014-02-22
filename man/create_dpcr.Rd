\name{create_dpcr}
\alias{create_dpcr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create dpcR object
}
\description{
Quick creation of \code{\linkS4class{adpcr}} and\code{\linkS4class{ddpcr}} objects.
}
\usage{
create_dpcr(data, n, threshold = NULL, breaks = NULL, type, adpcr = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  a \code{"numeric"} vector or matrix of data from dPCR experiments.
}
  \item{n}{
  \code{"integer"} equal to number of partitions.
}
  \item{threshold}{
  \code{"numeric"} value giving the threshold above which droplet is counted as positive.
  Ignored if \code{adpcr} is \code{TRUE}.
}
  \item{breaks}{
  \code{"numeric"} vector giving the number of intervals into which \code{data} should be cut.
  Ignored if \code{adpcr} is \code{FALSE}.
}
  \item{type}{
  Object of class \code{"character"} defining type of data. Could be 
    \code{"nm"} (number of molecules per partition), \code{"tp"} (total number of positive wells 
    in panel), \code{"fluo"} (fluorescence) or \code{"ct"} (threshold cycle).
}
  \item{adpcr}{
  logical. If \code{TRUE}, function creates \code{\linkS4class{adpcr}} object. 
  If \code{FALSE}, function creates \code{\linkS4class{ddpcr}} object.
}
}
\details{
This function assists in creation of objects used by other functions of the package. Its 
in build capabilities include checking the correctness of arguments.

A warning is prompted whenever any of arguments is converted to other type.
}
\value{
An \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}} object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michal Burdukiewicz, Stefan Roediger.
}
\note{
It is still possible (although not recomended) to create \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}} objects manually by \code{\link{new}} function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sample_run <- sample(0:500, 200, replace = TRUE)
sample_ddpcr_object <- create_dpcr(matrix(sample_run), n = 765L, 
threshold = 250, type = "nm", adpcr = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
