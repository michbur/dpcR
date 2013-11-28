\name{AUCtest}
\alias{AUCtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AUCtest
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AUCtest(x = x, y = y, threshold = 0.05, cut = 0.05, savgol = TRUE, norm = FALSE, filter.q = c(0.7, 0.8))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{cut}{
a numeric value between 0 and 1. All data between 0 and \code{cut} quantile would be considered 
noise in the further analysis.
}
  \item{savgol}{
logical value. If \code{TRUE}, Savitzky-Golay smoothing filter is used.
}
  \item{norm}{
logical value. If \code{TRUE}, data is normalised.
}
  \item{filter.q}{
a vector of two numeric values. The first element represents the quantile of the noise and the 
second one is the quantile of the negative peaks.
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
Stefan Roediger, Michal Burdukiewicz.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
