\name{extract_dpcr}
\alias{extract_dpcr}
\title{
Extract Digital PCR Experiment
}
\description{
Extract panel(s) or experiment(s) from a matrix while preserving all other attributes.
}
\usage{
extract_dpcr(input, id)
}
\arguments{
  \item{input}{object of the class \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}.}
  \item{id}{vector of indices or names of experiments or panels.}
}
\details{
The \code{extract_dpcr} function allows to choose one or more panels from an object of the 
\code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}} class and save it without changing
other attributes. It is the most recommended method of extracting a subset from an array of
panels, because it preserves class and structure of the object in contrary to standard operator
\link[base]{Extract}.
}
\value{
The object of the input's class (\code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}).
}

\author{
Michal Burdukiewicz.
}

\examples{
#sample extracting
panels <- sim_adpcr(10, 40, 1000, pos_sums = FALSE, n_panels = 50)
single_panel <- extract_dpcr(panels, 5)
random_three <- extract_dpcr(panels, sample.int(ncol(panels), 3))
all_but_one <- extract_dpcr(panels, -5)

#the same for fluorescence data
fluos <- sim_ddpcr(10, 40, 1000, pos_sums = FALSE, n_exp = 50, fluo = list(0.1, 0))
single_fluo <- extract_dpcr(fluos, 5)

}
\keyword{ manip }
\keyword{ extract }
\keyword{ panel }
