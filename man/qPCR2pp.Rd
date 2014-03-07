\name{qPCR2pp}
\alias{qPCR2pp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
qPCR to Poisson Process
}
\description{
dPCR can be described by Poisson statistics. The function qPCR2pp takes a step furter and 
interpretates the dPCR as a Poisson process if it is analyzed as a "time" based process.
}
\usage{
qPCR2pp(cycles, process, NuEvents = 1, delta = 1, mincyc = 1, maxcyc = 45, rug = TRUE, 
        plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cycles}{
the column containing the cycle data. Defaults to first column.
}
  \item{process}{
%%     ~~Describe \code{process} here~~
}
  \item{NuEvents}{
  NuEvents is "number of expected events" within a time frame (interval)
}
  \item{delta}{
 is the difference "time (cycles) points" e.g., Cycle 18 and 25
}
  \item{mincyc}{
  is the first cycle to start the analysis from.
}
  \item{maxcyc}{
   the the last cycle for the analysis of the Poisson process.
}
  \item{rug}{
  Adds a rug representation of the data to the plot.
}
  \item{plot}{
  If \code{TRUE}, plot Poisson process.
}
}
\details{
PCR data derived from a qPCR experiment can be seen as a series of events over time. We define 
t_i as the time between the first (i - 1)^st and the i^th event. Therefore, the time S_n
is the sum of all t_i 1 to n. This is the time to the n^th event. S(t) is the number of events 
in [0, t]. This can be seen as a Poisson proces. The Poisson statistics is the central theorem to 
random processes in digital PCR. The function \code{qPCR2pp} is used to model random point events 
in time units (PCR cycles), such as the increase of signa durin a qPCR reaction in a single 
compartment. A poisson process can be used to modle times at which an event occurs in a "system". 
The qPCR2pp (quantitative Real-Time PCR to Poisson process) function transforms the qPCR 
amplification curve data to quantification points (Cq) which are visualized as Poisson process. 
This functions helps to spot differences between replicate runs of digital PCR experiments. In 
ideal scenarios the qPCR2pp plots are highly similar. This tool might help to spot differences 
between experiments (e.g., inhibition of amplification reactions, influence of the chip arrays). 
The qPCR is an odd duck here because the amplification of conventional qPCRs takes place in 
discrete steps (cylces. 1, 2 ... 45) but the specific Cq values are calculated with continous 
outcomes (Cq 18.2, 25.7, ...). Other amplification methods such as isothermal amplifications are
time based and thus better suited for Poisson process.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{cycle.time }{is the "average time" between the occurence of a positive reaction and 
		     another positive reaction}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michal Burdukiewicz, Stefan Roediger.
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
\keyword{ Poisson }
\keyword{ Process }
