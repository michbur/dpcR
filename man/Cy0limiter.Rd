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
Cy0limiter(data = data, cyc = 1, fluo.range = c(NA), Cq.range = c(NA, NA), model = l5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{cyc}{
%%     ~~Describe \code{cyc} here~~
}
  \item{fluo.range}{
%%     ~~Describe \code{fluo.range} here~~
}
  \item{Cq.range}{
%%     ~~Describe \code{Cq.range} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
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
function (data = data, cyc = 1, fluo.range = c(NA), Cq.range = c(NA, 
    NA), model = l5) 
{
    Cy0 <- vector()
    Cy0.res <- vector()
    pb <- txtProgressBar(min = 1, max = length(fluo.range), initial = 0, 
        style = 3)
    for (i in fluo.range) {
        Cy0.tmp <- efficiency(pcrfit(data = data, cyc = cyc, 
            fluo = i, model = model), type = "Cy0", plot = FALSE)$Cy0
        Cy0 <- c(Cy0, Cy0.tmp)
        if (Cq.range[1] <= Cy0.tmp && Cy0.tmp <= Cq.range[2]) {
            Cy0.res.tmp <- 1
        }
        else (Cy0.res.tmp <- 0)
        Cy0.res <- c(Cy0.res, Cy0.res.tmp)
        setTxtProgressBar(pb, i)
    }
    close(pb)
    data.frame(Cy0 = Cy0, result = Cy0.res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
