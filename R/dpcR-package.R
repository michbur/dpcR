#' Digital PCR Analysis
#' 
#' The dpcR package is a collection of functions for a digital Polymerase Chain
#' Reaction (dPCR) analysis. dPCR comprises methods to quantify nucleic acids,
#' copy number variations (CNV), homo-/heterozygosity, and rare mutations
#' (including single nucleotide polymorphisms (SNP)). The chemical basis of
#' dPCR is similar to conventional PCR but the reaction-mix is divided into
#' hundredths to thousands of small compartments with parallel amplifications
#' reactions. The analysis is based on counting the number of positive
#' compartments and to relate it to the total number of compartments be means
#' of Poission statistics which enables an absolute quantification. The package
#' includes plot functions, summary functions, data sets and simulations for
#' dPCR and customizable GUI creators for droplet digital PCRs and
#' chamber-based digital PCRs. The authors of the package aim to include all
#' statistical approaches published in peer-review literature and additional
#' selected sources of expertise currently available and to make them available
#' to the scientific community in an open and cross-platform environment. As
#' such the dpcR packages has a list of expressions/functions and may serve in
#' future a reference to a unified nomenclature in dpcR.  The package is
#' primarily targeted at researchers who which to use it with an existing
#' technology or during the development of novel digital PCR systems. In
#' addition the dpcR package provides interactive tools that can be used in
#' classes or by individuals to better learn about digital PCR concepts and
#' data interpretation.
#' 
#' \tabular{ll}{ Package: \tab dpcR\cr Type: \tab Package\cr Version: \tab
#' 0.1.1\cr Date: \tab 2013-09-07\cr License: \tab GPL2 \cr }
#' 
#' @name dpcR-package
#' @aliases dpcR-package dpcR
#' @docType package
#' @author Michal Burdukiewicz, Stefan Roediger.
#' 
#' Maintainer: Michal Burdukiewicz <michalburdukiewicz@@gmail.com>
#' @seealso \link[qpcR]{qpcR.news}.
#' @references Huggett J, Foy CA, Benes V, Emslie K, Garson JA, Haynes R,
#' Hellemans J, Kubista M, Mueller RD, Nolan T, Pfaffl MW, Shipley GL,
#' Vandesompele J, Wittwer CT, Bustin SA \emph{The Digital MIQE Guidelines:
#' Minimum Information for Publication of Quantitative Digital PCR Experiments}
#' Clinical Chemistry, 2013. 59(6): p.892-902.
#' 
#' Vogelstein B, Kinzler KW, \emph{Digital PCR}. PNAS, 1999. 96(16): p.
#' 9236-9241. %%MORE REFERENCES
#' @keywords package
#' @import methods
#' @importFrom binom binom.confint
#' @importFrom chipPCR inder
#' @importFrom dgof ks.test cvm.test
#' @importFrom e1071 skewness kurtosis
#' @importFrom multcomp cld glht mcp
#' @importFrom qpcR efficiency pcrfit modlist
#' @importFrom pracma findpeaks
#' @importFrom rateratio.test rateratio.test
#' @importFrom shiny runApp
#' @importFrom signal sgolayfilt
#' @importFrom spatstat ppp quadrat.test
#' @examples
#' 
#' adpcr <- sim_adpcr(m = 400, n = 765, times = 20, pos_sums = FALSE, n_panels = 1)
#' plot_panel(adpcr, 45, 17, col = "green")
#' pos_chambers <- sum(adpcr > 0)
#' dpcr_density(k = pos_chambers, n = 765)
#' 
NULL


