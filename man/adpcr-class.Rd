\name{adpcr-class}
\docType{class}
\alias{adpcr-class}
\alias{adpcr}

\title{Class \code{"adpcr"}}
\description{
A class specifically designed to contain results from array digital PCR experiments. Data is
represented as matrix, where each column describes different experiment. Type of data in all
columns is specified in slot \code{"type"} and could be a number of molecules
\code{"nm"}, a number of positive droplets \code{"tnm"} (in this case whole experiment is 
represented by one row), a cycle threshold of each well \code{"ct"} or fluorescence values
\code{"fluo"}.

}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} containing data from array. See Description.}
    \item{\code{n}:}{Object of class \code{"integer"} equal to the number of partitions.}
    \item{\code{breaks}:}{\code{"numeric"}  vector giving the number of intervals into
    which \code{.Data} should be cut.}
    \item{\code{type}:}{Object of class \code{"character"} defining type of data. Could be 
    \code{"nm"} (number of molecules per partition), \code{"tp"} (total number of positive wells 
    in panel), \code{"fluo"} (fluorescence) or \code{"ct"} (threshold cycle).}
  }
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "adpcr")}: ... }
    \item{show}{\code{signature(object = "adpcr")}: ... }
   }
}

\author{
Michal Burdukiewicz.
}

\seealso{
Ploting and management:
\code{\link{extract_dpcr}}, \code{\link{plot_panel}}. 

Simulation:
\code{\link{sim_adpcr}}.

Droplet digital PCR:
\code{\linkS4class{ddpcr}}.
}
\examples{
rand_array <- sim_adpcr(400, 1600, 100, pos_sums = FALSE, n_panels = 5)
one_rand_array <- extract_dpcr(rand_array, 1)
plot_panel(one_rand_array, 40, 40)
}
\keyword{classes}

