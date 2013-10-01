\name{qpcr_analyser-methods}
\docType{methods}
\alias{qpcr_analyser-methods}
\alias{qpcr_analyser,adpcr-method}
\alias{qpcr_analyser,data.frame-method}
\alias{qpcr_analyser,modlist-method}
\alias{qpcr_analyser}
\title{
qPCR Analyser
}
\description{
Calculate statistics based on fluorescence. 
}
\usage{
\S4method{qpcr_analyser}{data.frame}(input, cyc = 1, fluo = NULL, model = l5, norm = FALSE, iter_tr = 50, type = "Cy0", takeoff = FALSE)
\S4method{qpcr_analyser}{adpcr}(input, cyc = 1, fluo = NULL, model = l5, norm = FALSE, iter_tr = 50, type = "Cy0", takeoff = FALSE)
\S4method{qpcr_analyser}{modlist}(input, type = "Cy0", takeoff = FALSE)

}
\arguments{
  \item{input}{
a dataframe containing the qPCR data or a result of function \code{modlist} or an object of the class 
 \code{\linkS4class{adpcr}}.
}
  \item{cyc}{
the column containing the cycle data. Defaults to first column.
}
  \item{fluo}{
the column(s) (runs) to be analyzed. If NULL, all runs will be considered.
}
  \item{model}{
the model to be used for all runs.
}
  \item{norm}{
logical. Should the raw data be normalized within [0, 1] before model fitting?
}
  \item{iter_tr}{
%%     ~~Describe \code{iter_tr} here~~
}
  \item{type}{
the method of efficiency estimation. See \link[qpcR]{efficiency}.
}
  \item{takeoff}{
logical; if \code{TRUE} calculated takeoff values are added to the result.
}
}
\details{
The \code{qpcRanalyzer} is a functions to automatize the analysis of amplification curves from
conventional quantitative real-time PCR (qPCR) experiments and is adapted for the needs in dPCR. 
This function calls instances of the the \code{qpcR} package to calculate the
quantification points (cpD1, cpD2, Cy0 (default), TOP (optional)), the amplification efficiency,
fluorescence at the quantification point (Cq), the absolute change of fluorescence and the take-off
point (TOP). Most of the central functionality of the \code{qpcR} package is accesable. The user 
can assign concentrations to the samples. One column contains binary converted (pos (1) and neg 
(0)) results for the amplification reaction based on a user defined criteria (Cq-range, 
fluorescence cut-off, ...). \code{qpcr_analyser} tries to detect cases where an amplification did 
not take place of was not possible to analyse. By default \code{qpcr_analyser} analyses uses the 
Cy0 as described in Guescini et al. (2008) for estimation of the quantification point since method
is considered to be better suited for many probe systems. By default a 5-parameter model is used 
to fit the amplification curves. As such \code{qpcr_analyser} is a functions which serves for 
preliminary data inspection (see Example section) and as input for other R functions from the 
\code{dpcR} package (e.g., \link{plot_panel}).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  Ritz C, Spiess An-N,
  \emph{qpcR: an R package for sigmoidal model selection in quantitative real-time polymerase
  chain reaction analysis}.
  Bioinformatics 24 (13),
  2008.
}
\author{
Stefan Roediger, Andrej-Nikolai Spiess, Michal Burdukiewicz.
}

\seealso{
\link[qpcR]{modlist}.
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
