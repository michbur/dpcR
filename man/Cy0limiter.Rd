\name{Cy0limiter}
\alias{Cy0limiter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cy0 Limiter
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Cy0limiter(data = data, cyc = 1, fluo = NULL, Cq.range = c(NA, NA), model = l5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a dataframe containing the qPCR data.
}
  \item{cyc}{
the column containing the cycle data. Defaults to first column.
}
  \item{fluo}{
        the column(s) (runs) to be analyzed. If NULL, all runs will be considered. 
	      Use fluo = 2 to chose the second column for example.
}
  \item{Cq.range}{
%%     ~~Describe \code{Cq.range} here~~
}
  \item{model}{
    is the model to be used for the analysis for all runs. Defaults to 'l5' 
      (see \code{\link[qpcR]{pcrfit}}).
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
library(qpcR)
test <- cbind(reps[1:45, ], reps2[1:45 ,2:ncol(reps2)], reps3[1:45 ,2:ncol(reps3)])
# results.dPCR contains a column with the Cy0 values and a column with converted values.

par(mfrow = c(2,2))
Cq.range <- c(20, 30)
results.dPCR <- Cy0limiter(data = test, cyc = 1, fluo = NULL,
                           Cq.range = Cq.range, model = l5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
