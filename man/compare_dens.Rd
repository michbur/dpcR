\name{compare_dens}
\alias{compare_dens}
\title{
Plot and Compare Densities
}
\description{
Plot empirical and theoretical density of the result of the digital PCR experiment.
}
\usage{
compare_dens(input, moments = TRUE, ...)
}
\arguments{
  \item{input}{
object of class \code{\linkS4class{adpcr}}  or \code{\linkS4class{ddpcr}} containing only one panel.
}
  \item{moments}{
logical, if \code{TRUE}, both theoretical and epirical moments are printed on the plot.
}
  \item{\dots}{
other arguments passed to the \code{plot} function.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\author{
Michal Burdukiewcz.
}
\seealso{
\link{moments}.
}
\examples{
adpcr_big <- sim_adpcr(m = 35, n = 40, times = 50, pos_sums = FALSE, n_panels = 1)
compare_dens(adpcr_big, TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
