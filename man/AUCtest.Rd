\name{AUCtest}
\alias{AUCtest}
\title{
AUCtest
}
\description{
Detect, separate and count positive peaks, negative peaks and peak-like noise. In addtion, function calculates area of the peaks.
}
\usage{
AUCtest(x = x, y = y, threshold = 0.05, noise_cut = 0.05, savgol = TRUE, norm = FALSE, 
filter.q = c(0.7, 0.8))
}

\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
a vector of fluorescence value.
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{noise_cut}{
a numeric value between 0 and 1. All data between 0 and \code{noise_cut} quantile would be considered 
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
The localization of peaks is determined by the \code{\link[pracma]{findpeaks}} function.
}
\value{
A list of length 2. The first element is a data frame containing: peak number, peak group (noise, 
negative, positive), position of the peak maximum, area under the peak, peak width, peak height, 
position of the peak and time resolution.

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

tmp.out <- AUCtest(many_peaks[, 1], many_peaks[, 2], threshold = 0.01, noise_cut = 0.1, savgol = TRUE)
plot(tmp.out[["data"]], type = "l", main = "Only smoothed")
abline(h = 0.01, col = "red")
abline(v = many_peaks[tmp.out[["peaks"]][, 3], 1], lty = "dashed")

tmp.out <- AUCtest(many_peaks[, 1], many_peaks[, 2], threshold = 0.01, noise_cut = 0.1, savgol = TRUE, 
                  norm = TRUE)
plot(tmp.out[["data"]], type = "l", main = "Smoothed and preaks detected")
abline(v = many_peaks[tmp.out[["peaks"]][, 3], 1])
for(i in 1:nrow(tmp.out$peaks)) {
  if(tmp.out$peaks[i,2] == 1) {col = 1}
  if(tmp.out$peaks[i,2] == 2) {col = 2}
  if(tmp.out$peaks[i,2] == 3) {col = 3}
  points(tmp.out$peaks[i, 7], tmp.out$peaks[i,6], col = col, pch = 19)
}

positive <- sum(tmp.out$peaks[, 2] == 3)
negative <- sum(tmp.out$peaks[, 2] == 2)
total <- positive + negative

}
\keyword{smooth}

