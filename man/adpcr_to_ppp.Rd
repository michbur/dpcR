\name{adpcr_to_ppp}
\alias{adpcr_to_ppp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert adpcr object to ppp object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adpcr_to_ppp(input, nx_a, ny_a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
object of the \code{\linkS4class{adpcr}} class containing data from one or more panels.
}
  \item{nx_a}{
Number of columns in a plate.
}
  \item{ny_a}{
Number of rows in a plate.
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
function (input, nx_a, ny_a) 
{
    if (class(input) != "adpcr") 
        stop("Input must have 'adpcr' class", call. = TRUE, domain = NA)
    array_data <- slot(input, ".Data")
    nrow_array <- nrow(array_data)
    if (nrow_array != nx_a * ny_a) 
        stop(paste0("Can not process with conversion since the input \n                 legnth (", 
            length(input), ") differs from the size of nx_a*ny_a (", 
            nx_a * ny_a, ").\n                   \n Change nx_a*ny_a to have the same number of elements."))
    apply(array_data, 2, function(array_col) {
        data_points <- matrix(NA, nrow = nrow_array, ncol = 3)
        i = 1
        for (x in 1L:nx_a) {
            for (y in ny_a:1L) {
                data_points[i, ] <- c(x, y, array_col[i])
                i <- i + 1
            }
        }
        data_points <- data_points[data_points[, 3] > 0, ]
        data_ppp <- ppp(data_points[, 1], data_points[, 2], c(1, 
            nx_a), c(1, ny_a), marks = data_points[, 3])
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
