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
Normally C-test is used to compare the proportion between
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
x1 <- rpois(100, 1.5)
x2 <- rpois(100, 1.49)
c_test(x1, x2, 1)

x2 <- rpois(100, 3.5)
c_test(x1, x2, 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
