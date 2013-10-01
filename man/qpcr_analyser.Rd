\name{qpcr_analyser}
\alias{qpcr_analyser}
\title{
qPCR Analyser
}
\description{
Calculate statistics based on fluorescence. 
}
\usage{
qpcr_analyser(input, cyc = 1, fluo = NULL, model = l5, norm = FALSE, iter_tr = 50, type = "Cy0", takeoff = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{cyc}{
%%     ~~Describe \code{cyc} here~~
}
  \item{fluo}{
%%     ~~Describe \code{fluo} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{norm}{
%%     ~~Describe \code{norm} here~~
}
  \item{iter_tr}{
%%     ~~Describe \code{iter_tr} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{takeoff}{
%%     ~~Describe \code{takeoff} here~~
}
}
\details{
The \code{qpcRanalyzer} is a functions to automatize the analysis of amplification curves from
conventional quantitative real-time PCR (qPCR) experiments and is adapted for the needs in dPCR. 
This function calls instances of the the \code{qpcR} package \cite{Spiess2013} to calculate the
quantification points (cpD1, cpD2, Cy0 (default), TOP (optional)), the amplification efficiency,
fluorescence at the quantification point (Cq), the absolute change of fluorescence and the take-off
point (TOP). Most of the central functionality of the \code{qpcR} package is accesable. The user 
can assign concentrations to the samples. One column contains binary converted (pos (1) and neg 
(0)) results for the amplification reaction based on a user defined criteria (Cq-range, 
fluorescence cut-off, ...). \code{qpcRanalyzer} tries to detect cases where an amplification did 
not take place of was not possible to analyse. By default \code{qpcRanalyzer} analyses uses the 
Cy0 as described in Guescini et al. (2008) for estimation of the quantification point since method
is considered to be better suited for many probe systems. By default a 5-parameter model is used 
to fit the amplification curves. As such \code{qpcRanalyzer} is a functions which serves for 
preliminary data inspection (see Example section) and as input for other R functions from the 
\code{dpcR} package (e.g., plot_panel).
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
Stefan Roediger, Michal Burdukiewicz
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
