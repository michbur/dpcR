\name{AUCtest}
\alias{AUCtest}
\title{
AUCtest
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AUCtest(x = x, y = y, threshold = 0.05, cut = 0.05, savgol = TRUE, norm = FALSE, filter.q = c(0.7, 0.8))
}

\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
a vector of flurescence value.
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
A list of length 2. The first element is a data frame containing: peak number, peak group (noise, 
negative, positive), position of the peak maximum, area under the peak, peak width, peak height, 
position of the peak and time resultion.

The second element contains smoothed data.

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

data(many_peaks)
par(mfrow = c(3,1))
plot(many_peaks, type = "l", main = "Noisy raw data")
abline(h = 0.01, col = "red")

tmp.out <- AUCtest(many_peaks[,1], many_peaks[,2], threshold = 0.01, cut = 0.1, savgol = TRUE)
plot(tmp.out[["data"]], type = "l", main = "Only smooted")
abline(h = 0.01, col = "red")
abline(v = many_peaks[tmp.out[["res"]][,3], 1])

tmp.out <- AUCtest(many_peaks[,1], many_peaks[,2], threshold = 0.01, cut = 0.1, savgol = TRUE, 
                  norm = TRUE)
plot(tmp.out[["data"]], type = "l", main = "Smoothed and preaks detected")
abline(v = many_peaks[tmp.out[["res"]][,3], 1])
for(i in 1:nrow(tmp.out$res)) {
  if(tmp.out$res[i,2] == 1) {col = 1}
  if(tmp.out$res[i,2] == 2) {col = 2}
  if(tmp.out$res[i,2] == 3) {col = 3}
  points(tmp.out$res[i,7], tmp.out$res[i,6], col = col, pch = 19)
}

positive <- sum(tmp.out$res[,2] == 3)
negative <- sum(tmp.out$res[,2] == 2)
total <- positive + negative

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
