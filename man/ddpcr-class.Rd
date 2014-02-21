\name{ddpcr-class}
\docType{class}
\alias{ddpcr-class}
\alias{ddpcr}

\title{Class \code{"ddpcr"}}
\description{
A class specifically designed to contain results from droplet digital PCR experiments. Data is
represented as matrix, where each column describes different experiment. Type of data in all
columns is specified in slot \code{"type"} and could be a by number of molecules
\code{"nm"}, number of positive droplets \code{"tnm"} (in this case whole experiment is 
represented by one row) or fluorescence \code{"fluo"}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} containing data from all droplets. See Description.}
    \item{\code{n}:}{\code{"integer"} representing number of partitions.}
    \item{\code{threshold}:}{ \code{"numeric"} value giving the threshold above which droplet is counted as positive.}
    \item{\code{type}:}{Object of class \code{"character"} defining type of data. Could be
    \code{"nm"} (Number of molecules per partition), \code{"tp"} (number of positive droplets) or 
    \code{"fluo"} (fluorescence).}   
  }
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "ddpcr")}: ... }
    \item{show}{\code{signature(object = "ddpcr")}: ... }
   }
}
\author{
Michal Burdukiewicz.
}


\seealso{
Ploting and managment:
\code{\link{extract_dpcr}}, \code{\link{plot_vic_fam}}.

Simulation:
\code{\link{sim_ddpcr}}.

Array digital PCR:
\code{\linkS4class{adpcr}}.
}
\examples{
ddpcr_fluo <- sim_ddpcr(m = 10, n = 20, times = 5, fluo = list(0.1, 0))
plot(ddpcr_fluo)

ddpcr <- sim_ddpcr(m = 10, n = 20, times = 5)
}

\keyword{classes}


