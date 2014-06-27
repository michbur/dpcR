\name{test_panel}
\alias{test_panel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dispersion Test for Spatial Point Pattern in Array dPCR Based on Quadrat Counts
}
\description{
The function \code{test_panel} is a convenient wrapper around \code{\link[spatstat]{quadrat.test}} 
function. Under optimal conditions, the point pattern of dPCR events (e.g., positive droplet & 
negative droplets). This function can be used to analyze if the pattern on a planar chip is 
random. 
Arrays with non-random patterns should be checked for integrity.
}
\usage{
test_panel(X, nx_a, ny_a, nx = 5, ny = 5, 
           alternative = c("two.sided", "regular", "clustered"),
           method = c("Chisq", "MonteCarlo"), conditional = TRUE, nsim = 1999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Object of the \code{\linkS4class{adpcr}} class containing data from one or more panels.
}
  \item{nx_a}{
Number of columns in a plate.
}
  \item{ny_a}{
Number of rows in a plate.
}
  \item{nx}{
Numbers of quadrats in the x direction.
}
  \item{ny}{
Numbers of quadrats in the y direction.
}
  \item{alternative}{
Character string (partially matched) specifying the alternative hypothesis.
}
  \item{method}{
Character string (partially matched) specifying the test to use: either method="Chisq" for the
chi-squared test (the default), or method="MonteCarlo" for a Monte Carlo test.
}
  \item{conditional}{
Logical. Should the Monte Carlo test be conducted conditionally upon the observed number of 
points of the pattern? Ignored if method="Chisq".
}
  \item{nsim}{
The number of simulated samples to generate when method="MonteCarlo".
}
}
\details{
This function quick-to-use version of \code{\link[spatstat]{quadrat.test}} function. It works 
directly on the objects of \code{\linkS4class{adpcr}}. \code{test_panel} performs a test of
Complete Spatial Randomness for each plate.
}
\value{
An list of objects of class "htest" with the length equal to the number of plates (minimum 1).
}
\references{
http://www.spatstat.org/
}
\author{
Adrian Baddeley, Rolf Turner, Michal Burdukiewcz, Stefan Roediger.
}
\note{
A similar result can be achived by using \code{\link{adpcr2ppp}} and 
\code{\link[spatstat]{quadrat.test}}. See Examples.
}


\seealso{
\code{\link[spatstat]{quadrat.test}}.
}
\examples{
many_panels <- sim_adpcr(m = 400, n = 765, times = 1000, pos_sums = FALSE, 
                   n_panels = 5)
test_panel(many_panels, nx_a = 45, ny_a = 17)

#test only one plate
test_panel(extract_dpcr(many_panels, 3), nx_a = 45, ny_a = 17)

#do test_panel manually
require(spatstat)
ppp_data <- adpcr2ppp(many_panels, nx_a = 45, ny_a = 17)
lapply(ppp_data, function(single_panel) quadrat.test(single_panel))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ pattern }
\keyword{ quadrat }
\keyword{ spatial}
\keyword{ dPCR }

