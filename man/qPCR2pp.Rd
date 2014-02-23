\name{qPCR2pp}
\alias{qPCR2pp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
qPCR to Poisson Process
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qPCR2pp(cycles, process, NuEvents = 1, delta = 1, mincyc = 1, maxcyc = 45, rug = TRUE, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cycles}{
%%     ~~Describe \code{cycles} here~~
}
  \item{process}{
%%     ~~Describe \code{process} here~~
}
  \item{NuEvents}{
%%     ~~Describe \code{NuEvents} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
  \item{mincyc}{
%%     ~~Describe \code{mincyc} here~~
}
  \item{maxcyc}{
%%     ~~Describe \code{maxcyc} here~~
}
  \item{rug}{
%%     ~~Describe \code{rug} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
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
function (cycles, process, NuEvents = 1, delta = 1, mincyc = 1, 
    maxcyc = 45, rug = TRUE, plot = TRUE) 
{
    res.qPCR <- data.frame(cycles, process)
    colnames(res.qPCR) <- c("Cycles", "result")
    res.qPCR <- res.qPCR[order(res.qPCR[["Cycles"]]), ]
    res.qPCR <- cbind(res.qPCR, cumsum(res.qPCR[, 2]))
    colnames(res.qPCR) <- c("Cycles", "result")
    dens.tmp <- dpcr_density(sum(res.qPCR[, 2]), nrow(res.qPCR), 
        plot = FALSE)
    cycle.time <- exp(1/dens.tmp$k)
    mu <- dens.tmp$k * delta
    fact.NuEvents <- factorial(NuEvents)
    if (fact.NuEvents != "Inf") {
        cyc.occ <- (exp(-mu) * mu^NuEvents)/fact.NuEvents
    }
    else cyc.occ <- "to large"
    if (plot) 
        plot(res.qPCR[, 1], res.qPCR[, 3], xlim = c(mincyc, maxcyc), 
            ylim = c(0, nrow(res.qPCR)), xlab = "Cycle", ylab = "lambda (cycle)", 
            type = "S", lwd = 1.5)
    abline(h = nrow(res.qPCR) * 0.5, col = "grey")
    legend(mincyc, nrow(res.qPCR), c(paste0("Chambers", dens.tmp$n, 
        sep = " "), paste0("Events", dens.tmp$k, sep = " "), 
        paste0("mu", mu, sep = " "), paste0("CT", cycle.time, 
            sep = " "), paste0("CO", cyc.occ, sep = " ")))
    if (rug) 
        rug(res.qPCR[, 1])
    list(out = res.qPCR, mu = mu, CT = cycle.time, CO = cyc.occ)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
