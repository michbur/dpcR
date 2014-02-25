\name{Cy0limiter}
\alias{Cy0limiter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cy0 Limiter
}
\description{
The function \code{Cy0limiter} calculates the Cq values of a qPCR experiment within a defined range
of cycles. The function can be used to extract Cq values of a chamber based qPCR for conversion
into a dPCR experiment. All Cq values are obtained by Cy0 method (Guescini et al. (2008)).
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
    is a user defined range of cycles to be used for the determination of the Cq values.
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
Guescini M, Sisti D, Rocchi MB, Stocchi L & Stocchi V (2008) \emph{A new real-time PCR method to 
overcome significant quantitative inaccuracy due to slight amplification inhibition}. BMC 
Bioinformatics, 9: 326.
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
\keyword{ Cy0 }
\keyword{ qPCR }
\keyword{ dPCR }
