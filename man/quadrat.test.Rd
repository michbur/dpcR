\name{quadrat.test}
\alias{quadrat.test}
\alias{quadrat.test.adpcr}
\title{
Dispersion Test for Spatial Point Pattern in array dPCR Based on Quadrat Counts
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
quadrat.test(X, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of quadrat.test results.
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
many_panels <- sim_adpcr(m = 400, n = 765, times = 1000, pos_sums = FALSE, 
                   n_panels = 5)
quadrat.test(many_panels, nx_a = 45, ny_a = 17)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
