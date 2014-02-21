\name{create_dpcr}
\alias{create_dpcr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create dpcR object
}
\description{
Create dpcR object.
}
\usage{
create_dpcr(data, n, threshold = NULL, type, adpcr = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  a vector or matrix of data from dpcr experiments.
}
  \item{n}{
  \code{"integer"} equal to number of partitions.
}
  \item{threshold}{
  \code{"numeric"} value giving the threshold above which droplet is counted as positive.
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
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
