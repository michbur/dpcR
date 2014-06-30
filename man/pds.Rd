\name{pds}
\alias{pds}
\docType{data}
\title{
Plasmid dilution series results
}
\description{
These are the results data from the \code{pds_raw} data as calculated by 
the BioRad QX100 Droplet Digital PCR System.

Setup: Duplex assay with constant amount of genomic DNA and six 10-fold 
dilutions of plasmid DNA with 4 replicates, ranging theoretically from ~ 
10^4 to 10^-1 copies/ micro L plus  4 replicates without plasmid DNA. 
Included are No-gDNA-control and No-template-control, 2 replicates each.

Annotation: FX.Y (X = dilution number, Y = replicate number).
Hardware: Bio-Rad QX100 Droplet digital PCR system
Details:
Genomic DNA isolated from Pseudomonas putida KT2440.
Plasmid is pCOM10-StyA::EGFP StyB [Jahn et al., 2013, Curr Opin 
Biotechnol, Vol. 24 (1): 79-87].
Template DNA was heat treated at 95 degree Celsius for 5 min prior to PCR.
Channel 1, primers for genomic DNA marker ileS, Taqman probes (FAM labelled).
Channel 2, primers for plasmid DNA marker styA, Taqman probes (HEX labelled).
}
\usage{data(pds)}
\format{
  A data frame with 64 observations on the following 44 variables.
  \describe{
    \item{\code{Well}}{a factor with levels \code{A01} to \code{H04}}
    \item{\code{ExptType}}{a factor with levels \code{Absolute 
			    Quantification}}
    \item{\code{Experiment}}{a factor with levels \code{ABS}}
    \item{\code{Sample}}{a factor with levels \code{B} \code{B + P 10^2} 
			  \code{gDNA} \code{gDNA + P 10^0} 
			  \code{gDNA + P 10^1} \code{gDNA + P 10^-1} 
			  \code{gDNA + P 10^2} \code{gDNA + P 10^3} 
			  \code{gDNA + P 10^4}}
    \item{\code{TypeAssay}}{a factor with levels \code{Ch1NTC} 
			    \code{Ch1Unknown} \code{Ch2NTC} 
			    \code{Ch2Unknown}}
    \item{\code{Assay}}{a factor with levels \code{ileS} 
			\code{styA}}
    \item{\code{Status}}{a factor with levels \code{Manual}}
    \item{\code{Concentration}}{a numeric vector}
    \item{\code{TotalConfMax}}{a logical vector}
    \item{\code{TotalConfMin}}{a logical vector}
    \item{\code{PoissonConfMax}}{a numeric vector}
    \item{\code{PoissonConfMin}}{a numeric vector}
    \item{\code{Positives}}{a numeric vector}
    \item{\code{Negatives}}{a numeric vector}
    \item{\code{Ch1.Ch2.}}{a numeric vector}
    \item{\code{Ch1.Ch2..1}}{a numeric vector}
    \item{\code{Ch1.Ch2..2}}{a numeric vector}
    \item{\code{Ch1.Ch2..3}}{a numeric vector}
    \item{\code{Linkage}}{a numeric vector}
    \item{\code{AcceptedDroplets}}{a numeric vector}
    \item{\code{CNV}}{a logical vector}
    \item{\code{TotalCNVMax}}{a logical vector}
    \item{\code{TotalCNVMin}}{a logical vector}
    \item{\code{PoissonCNVMax}}{a logical vector}
    \item{\code{PoissonCNVMin}}{a logical vector}
    \item{\code{ReferenceCopies}}{a logical vector}
    \item{\code{UnknownCopies}}{a logical vector}
    \item{\code{Ratio}}{a numeric vector}
    \item{\code{TotalRatioMax}}{a logical vector}
    \item{\code{TotalRatioMin}}{a logical vector}
    \item{\code{PoissonRatioMax}}{a numeric vector}
    \item{\code{PoissonRatioMin}}{a numeric vector}
    \item{\code{FractionalAbundance}}{a numeric vector}
    \item{\code{TotalFractionalAbundanceMax}}{a logical vector}
    \item{\code{TotalFractionalAbundanceMin}}{a logical vector}
    \item{\code{PoissonFractionalAbundanceMax}}{a numeric vector}
    \item{\code{PoissonFractionalAbundanceMin}}{a numeric vector}
    \item{\code{ReferenceAssayNumber}}{a numeric vector}
    \item{\code{TargetAssayNumber}}{a numeric vector}
    \item{\code{MeanAmplitudeofPositives}}{a numeric vector}
    \item{\code{MeanAmplitudeofNegatives}}{a numeric vector}
    \item{\code{MeanAmplitudeTotal}}{a numeric vector}
    \item{\code{ExperimentComments}}{a logical vector}
    \item{\code{MergedWells}}{a logical vector}
  }
}

\author{
Michael Jahn, Stefan Roediger, Michal Burdukiewcz
}
\source{
Michael Jahn
Flow cytometry group / Environmental microbiology
Helmholtz Centre for Environmental Research - UFZ
Permoserstrasse 15 / 04318 Leipzig / Germany
phone +49 341 235 1318
michael.jahn [at] ufz.de / www.ufz.de
}
\references{
Jahn et al., 2013, \emph{Curr Opin Biotechnol}, Vol. 24 (1): 79-87
}
\examples{
\dontrun{
# Loading libraries
library(lattice)
library(latticeExtra)
library(gplots)
library(Hmisc)
library(memisc)

dat <- pds
dat$Dilution <- rep(c(rep(c(10^(4:-1),0), each = 4), 
		    "NGC", "NGC", "NTC", "NTC"), 2)

print(xyplot(Concentration ~ factor(Dilution, levels=Dilution), dat, 
      groups = Assay, ewidth = 0.08, ylim = c(10^-1.5, 10^4.5),
      ylab = "Concentration cp / micro L", xlab = "Theoretical plasmid 
	      concentration [cp/micro L]", strip = FALSE,
    scales = list(y = list(log = 10), alternating = FALSE),
    panel = function(x, y, groups = groups, ...) {
        panel.rect(7.5,-2, 10,5, col = grey(0.9), border = NA)
        panel.grid(h = -1,v = -1, col = grey(0.8), lty = 2)
        panel.xyplot(x, y, groups = groups, col = grey(0.6), ...)
        panel.key(c("IleS", "StyA"), corner = c(0.95,0.95))
        means <- tapply(y, list(x, groups), function(x) mean(x, na.rm = TRUE))
        stdev <- tapply(y, list(x, groups), function(x) sd(x, na.rm = TRUE))
        panel.errbars(1:9 + 0.25, y = cbind(means[, 1], 
		      means[, 1]-stdev[, 1], means[, 1] + stdev[, 1]), 
		      make.grid = "none", pch = 1,...)
        panel.errbars(1:9 + 0.25, y = cbind(means[, 2], 
		      means[, 2] - stdev[, 2], means[, 2] + stdev[, 2]), 
		      make.grid = "none", pch = 3, ...)
        panel.abline(a = c(5,-1), lty = 2, col = 2)
    }
  )
)
}
}
\keyword{datasets}
