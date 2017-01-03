[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dpcR)](https://cran.r-project.org/package=dpcR)
[![Downloads](http://cranlogs.r-pkg.org/badges/dpcR)](https://cran.r-project.org/package=dpcR)
[![Build Status](https://api.travis-ci.org/michbur/dpcR.png)](https://travis-ci.org/michbur/dpcR)


![dpcR](https://github.com/michbur/dpcR/blob/master/vignettes/dpcR_logo.png)
====
Analysis, visualisation and simulation of digital PCR experiments.

# Installation

dpcR is available [on CRAN](https://cran.r-project.org/package=dpcR), so installation is as simple as:

```R
install.packages("dpcR")
```

You can install the latest development version of the code using the `devtools` R package.

```R
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("michbur/dpcR")
```

# dpcReport - web server and standalone GUI

Our web server [dpcReport](http://www.smorfland.uni.wroc.pl/shiny/dpcReport/) allows analysis of dPCR data without installing the package. It may be also downloaded as a standalone application (does not require prior installation of R) from [SourceForge](http://sourceforge.net/projects/dpcreport/).
