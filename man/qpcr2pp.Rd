\name{qpcr2pp}
\alias{qpcr2pp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
qPCR to Poisson Process
}
\description{
dPCR can be described by Poisson statistics. The function qPCR2pp takes a step furter and 
interpretates the dPCR as a Poisson process if it is analyzed as a "time" based process.
}
\usage{
qpcr2pp(cycles, process, data = NULL, NuEvents = 1, delta = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cycles}{
the column containing the cycle data. Defaults to first column.
}
  \item{process}{
the column containing fluorescence values.
}  \item{data}{
  a dataframe containing the qPCR data.
}
  \item{NuEvents}{
  NuEvents is "number of expected events" within a time frame (interval).
}
  \item{delta}{
 is the difference "time (cycles) points" e.g., Cycle 18 and 25.
}
}
\details{
PCR data derived from a qPCR experiment can be seen as a series of events over time. We define 
t_i as the time between the first (i - 1)^st and the i^th event. Therefore, the time 
\eqn{S_n}{S_n}
is the sum of all \eqn{t_i}{t_i} from \eqn{i = 1}{i = 1} to \eqn{i = n}{i = n}. This is the time
to the n^th event. \eqn{S(t)}{S(t)} is the number of events 
in \eqn{[0, t]}{[0, t]}. This can be seen as a Poisson proces. The Poisson statistics is the 
central theorem to random processes in digital PCR. 

The function \code{qPCR2pp} is used to model random point events 
in time units (PCR cycles), such as the increase of signal during a qPCR reaction in a single 
compartment. A Poisson process can be used to modle times at which an event occurs in a "system". 
The qPCR2pp (quantitative Real-Time PCR to Poisson process) function transforms the qPCR 
amplification curve data to quantification points (Cq) which are visualized as Poisson process. 
This functions helps to spot differences between replicate runs of digital PCR experiments. In 
ideal scenarios the qPCR2pp plots are highly similar. 

This tool might help to spot differences 
between experiments (e.g., inhibition of amplification reactions, influence of the chip arrays). 
The qPCR is very unique because the amplification of conventional qPCRs takes place in 
discrete steps (cycles: 1, 2 ... 45), but the specific Cq values are calculated with continous 
outcomes (Cq: 18.2, 25.7, ...). Other amplification methods such as isothermal amplifications are
time based and thus better suited for Poisson process.
}
\value{
An object of \code{\linkS4class{qpcrpp}} class.
}

\author{
Stefan Roediger, Michal Burdukiewicz.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

test <- cbind(reps[1:45, ], reps2[1:45 ,2:ncol(reps2)], reps3[1:45 ,2:ncol(reps3)])

# results.dPCR contains a column with the Cy0 values and a column with converted values.
Cq.range <- c(20, 30)
ranged <- limit_cy0(data = test, cyc = 1, fluo = NULL,
                     Cq_range = Cq.range, model = l5)
                     
qpcr2pp(ranged[,1], ranged[,2], delta = 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Poisson }
\keyword{ Process }
