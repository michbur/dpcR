\name{c_test}
\alias{c_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
C-test
}
\description{
The conditional test for comparing two Poisson means.
}
\usage{
c_test(x, y, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a (non-empty) numeric vector of data values.
}
  \item{y}{
an (non-empty) numeric vector of data values.
}
  \item{c}{
a specific number.
}
}
\details{
Normally C-test is used to compare the proportion between two samples. The 
conditional method (C-test) was proposed by Przyborowski and Wilenski 
(1940) to to test the Poisson mean. The test can be used to evaluate if two 
dPCR runs (e.g., replicates) with different number of total events are 
identical.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  Przyborowski J, Wilenski H.
  \emph{homogeneity of results in testing samples from poisson series: with an application to
  testing clover seed for dodder}
  Biometrika 31 (3-4), 
  1940.
}
\author{
Michal Burdukiewicz, Stefan Roediger
}
\note{
This test is not related to Cochran's C test (R function 
\code{\link[outliers]{cochran.test}}) for one-sided upper limit variance 
outlier test by William G. Cochran.
}

\seealso{
See also \code{\link[stats]{poisson.test}} and \code{\link[rateratio.test]{rateratio.test}}.
}

\examples{
x1 <- rpois(100, 1.5)
x2 <- rpois(100, 1.49)
c_test(x1, x2, 1)

x2 <- rpois(100, 3.5)
c_test(x1, x2, 1)
}

\keyword{ compare }
\keyword{ mean }
\keyword{ poisson }
