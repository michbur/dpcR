\name{plate_test}
\alias{plate_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dispersion Test for Spatial Point Pattern in array dPCR Based on Quadrat Counts
}
\description{
A convinient wrapper around \code{\link[spatstat]{quadrat.test}} function.
}
\usage{
plate_test(X, nx_a, ny_a, nx = 5, ny = 5, 
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
directly on the objects of \code{\linkS4class{adpcr}}. \code{plate_test} performs a test of
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
A similar result can be achived by using \code{\link{adpcr_to_ppp}} and 
\code{\link[spatstat]{quadrat.test}}. See Examples.
}


\seealso{
\code{\link[spatstat]{quadrat.test}}.
}
\examples{
many_panels <- sim_adpcr(m = 400, n = 765, times = 1000, pos_sums = FALSE, 
                   n_panels = 5)
plate_test(many_panels, nx_a = 45, ny_a = 17)

#test only one plate
plate_test(extract_dpcr(many_panels, 3), nx_a = 45, ny_a = 17)

#do plate_test manually
require(spatstat)
ppp_data <- adpcr_to_ppp(many_panels, nx_a = 45, ny_a = 17)
lapply(ppp_data, function(single_panel) quadrat.test(single_panel))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
