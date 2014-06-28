\name{adpcr2ppp}
\alias{adpcr2ppp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert adpcr to ppp 
}
\description{
Quick conversion of \code{\linkS4class{adpcr}} object to the list of 
\code{\link[spatstat]{ppp.object}}s.
}
\usage{
adpcr2ppp(input, nx_a, ny_a)
}

\arguments{
  \item{input}{
Object of the \code{\linkS4class{adpcr}} class containing data from one or 
more panels.
}
  \item{nx_a}{
Number of columns in a plate.
}
  \item{ny_a}{
Number of rows in a plate.
}
}
\details{
Each plate is independently converted by \code{\link[spatstat]{ppp}} 
function. \code{marks}
attached to each point represent values contained by the 
\code{\linkS4class{adpcr}} 
object.
}
\value{
A list containing objects with class \code{\link[spatstat]{ppp.object}} 
with the length equal to
the number of plates (minimum 1).
}

\author{
Michal Burdukiewcz, Stefan Roediger.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[spatstat]{ppp.object}}, \code{\link[spatstat]{ppp}}.
}
\examples{
many_panels <- sim_adpcr(m = 400, n = 765, times = 1000, pos_sums = FALSE, 
                   n_panels = 5)

# Convert all plates to ppp objects
adpcr2ppp(many_panels, nx_a = 45, ny_a = 17)

# Convert all plates to ppp objects and get third plate
third_plate <- adpcr2ppp(many_panels, nx_a = 45, ny_a = 17)[[3]]

# Convert only third plate to ppp object
third_plate2 <- adpcr2ppp(extract_dpcr(many_panels, 3), nx_a = 45, ny_a = 
17)

# Check the class of a new object
class(third_plate2)

# It's a list with the length 1. The third plate is a first element on this 
list
class(third_plate2[[1]])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ panel }
