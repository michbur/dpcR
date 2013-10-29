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
%%     ~~Describe \code{cut} here~~
}
  \item{savgol}{
%%     ~~Describe \code{savgol} here~~
}
  \item{norm}{
%%     ~~Describe \code{norm} here~~
}
  \item{filter.q}{
%%     ~~Describe \code{filter.q} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x = x, y = y, threshold = 0.05, cut = 0.05, savgol = TRUE, 
    norm = FALSE, filter.q = c(0.7, 0.8)) 
{
    if (is.null(x)) 
        stop("Enter 'x' value.", call. = TRUE, domain = NA)
    if (is.null(y)) 
        stop("Enter 'y' value.", call. = TRUE, domain = NA)
    if (length(x) != length(y)) 
        stop("'x' and 'y' differ in length.", call. = TRUE, domain = NA)
    y[which(y < quantile(y, cut))] <- 0
    qval <- 0.05
    if (norm) 
        y <- (y - quantile(y, qval))/(quantile(y, 1 - qval) - 
            quantile(y, qval))
    if (savgol == TRUE) {
        data <- data.frame(x, sgolayfilt(y))
    }
    else (data <- cbind(x, smooth.spline(x, y)$yin))
    tmp.peaks <- findpeaks(data[, 2])
    peak.quntiles <- quantile(tmp.peaks[, 1], filter.q)
    no.peak <- tmp.peaks[, 1] <= peak.quntiles[1]
    neg.peak <- tmp.peaks[, 1] > peak.quntiles[1] & tmp.peaks[, 
        1] <= peak.quntiles[2]
    pos.peak <- tmp.peaks[, 1] > peak.quntiles[2]
    test.res <- data.frame(no.peak, neg.peak, pos.peak)
    res.peaks <- matrix(data = NA, nrow = nrow(tmp.peaks), ncol = 8)
    colnames(res.peaks) <- c("Peak number", "State", "Position", 
        "AUC", "Width", "Height", "Index", "Resolution")
    for (i in 1L:nrow(tmp.peaks)) {
        x.tmp <- data[tmp.peaks[i, 3]:tmp.peaks[i, 4], 1]
        y.tmp <- data[tmp.peaks[i, 3]:tmp.peaks[i, 4], 2]
        delta <- function(x) {
            delta <- vector()
            for (i in 1:(length(x) - 1)) {
                tmp <- abs(x[i] - x[i + 1])
                delta <- c(delta, tmp)
            }
            delta.mean <- mean(delta)
            delta.mean
        }
        sp <- smooth.spline(x, y)
        psp <- function(x = x.tmp) {
            psp.tmp <- predict(sp, x)
            psp <- psp.tmp$y
        }
        try(integrate.tmp <- integrate(psp, lower = min(x.tmp), 
            upper = max(x.tmp))$value)
        res.peaks[i, 1] <- i
        res.peaks[i, 2] <- which(test.res[i, ] == TRUE)
        res.peaks[i, 3] <- tmp.peaks[i, 2]
        res.peaks[i, 4] <- integrate.tmp
        res.peaks[i, 5] <- max(x.tmp) - min(x.tmp)
        res.peaks[i, 6] <- tmp.peaks[i, 1]
        res.peaks[i, 7] <- data[tmp.peaks[i, 2], 1]
        res.peaks[i, 8] <- delta(x.tmp)
    }
    return(list(res = res.peaks, data = data))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
