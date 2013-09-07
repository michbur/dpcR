\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,adpcr-method}
\alias{summary,ddpcr-method}
\alias{summary}
\title{Methods for Function \code{summary}}
\description{
Expands function \code{\link[base]{summary}} allowing printing summaries objects of the class 
\code{\linkS4class{adpcr}} to or \code{\linkS4class{ddpcr}}.
}
\usage{
\S4method{summary}{adpcr}(object, print = TRUE)
\S4method{summary}{ddpcr}(object, print = TRUE)
}
\arguments{
  \item{object}{an object of class \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}.}
  \item{print}{if \code{FALSE}, no output is printed, only calculations are performed.}}


\details{
The function prints summary of the dPCR reaction, including k (number of positive chambers), n 
(total number of chambers), estimated lambda and m (number of molecules per plate), as well as 
confidence intervals for the last two variables.
}

\value{
The data frame with estimated values of lambda, m and corresponding confidence intervals.
}
\references{
  Bhat S, Herrmann J, Corbisier P, Emslie K,
  \emph{ Single molecule detection in nanofluidic digital array enables accurate measurement 
  of DNA copy number}.
  Analytical and Bioanalytical Chemistry 2 (394),
  2009.

  Dube S, Qin J, Ramakrishnan R,
  \emph{Mathematical Analysis of Copy Number Variation in a DNA Sample Using Digital PCR on a 
  Nanofluidic Device}.
  PLoS ONE 3(8),
  2008.
  
  }
  
\author{
Michal Burdukiewicz, Stefan Roediger.
}  
  
\note{
If summary is used on an object containing results of many experiments, all experiments would be 
independently summarized. Currently supported only for objects of class \code{\linkS4class{adpcr}}.
}

\examples{
# array dpcr
# Simulates a chamber based digital PCR with m total number of template molecules  
# and n number of chambers per plate and assigns it as object ptest of the class 
# adpcr for a single panel. The summary function on ptest gets assigned to summ 
# and the result with statistics according to Dube et al. 2008 and Bhat et al. 2009 
# gets printed.
ptest <- sim_adpcr(m = 400, n = 765, times = 5, dube = FALSE, n_panels = 1)
summ <- summary(ptest) #save summary
print(summ)

# multiple experiments
# Similar to the previous example but with five panels
ptest <- sim_adpcr(m = 400, n = 765, times = 5, dube = FALSE, n_panels = 5)
summary(ptest)

# droplet dpcr - fluorescence
# Simulates a droplet digital PCR with m = 7 total number of template molecules 
# and n = 20 number of  droplets. The summary function on dropletf gives the 
# statistics according to Dube et al. 2008 and Bhat et al. 2009. The fluo parameter 
# is used to change the smoothness of the fluorescence curve and the space between
#  two consecutive measured peaks (droplets).
dropletf <- sim_ddpcr(m = 7, n = 20, times = 5, fluo = list(0.1, 0))
summary(dropletf)

# droplet dpcr - number of molecules
# Similar to the previous example but with five panels but without and modifications
#  to the peaks.
droplet <- sim_ddpcr(m = 7, n = 20, times = 5)
summary(droplet)

# Visualize the results of dropletf and dropletf
# The curves of dropletf are smoother.
par(mfrow = c(1,2))
plot(dropletf, main = "With fluo parameter", type = "l")
plot(droplet, main = "Without fluo parameter", type = "l")

}
\keyword{methods}
\keyword{utilities}

