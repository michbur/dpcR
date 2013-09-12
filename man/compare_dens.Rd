\name{compare_dens}
\alias{compare_dens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot and Compare Densities
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compare_dens(input, moments = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
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
adpcr_big <- sim_adpcr(m = 35, n = 40, times = 50, pos_sums = FALSE, n_panels = 1)
compare_dens(adpcr_big, TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
