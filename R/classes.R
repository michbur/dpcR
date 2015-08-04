#' Class \code{"dpcr"} - general digital PCR
#' 
#' A class containing results of any digital PCR experiment. 
#' Type of data in all columns is specified in slot \code{"type"}.
#' 
#' @name dpcr-class
#' @aliases dpcr-class dpcr
#' @docType class
#' @slot .Data \code{"matrix"} containing data from array. See Description.
#' @slot n \code{"integer"} equal to the number of partitions in each
#' experiment.
#' @slot exper \code{"factor"} representing the id or name of experiments.
#' @slot replicate \code{"factor"} representing the id or name of replicate.
#' @slot type Object of class \code{"character"} defining type of data. See Details.
#' @details
#' Possible \code{type} values of \code{dpcr} objects:
#' \enumerate{
#'  \item{\code{"ct"}: cycle threshold of each partition,}
#'  \item{\code{"fluo"}: fluorescence of each partition,}
#'  \item{\code{"nm"}: number of molecules in each partition,}
#'  \item{\code{"np"}: status (positive (1) or negative(0)) of each partition,}
#'  \item{\code{"tnp"}: total number of positive partitions in the run (single 
#'  value per each run, not per partition).}}
#' @author Michal Burdukiewicz.
#' @note This class should not be directly used. Instead, users should use more 
#' specific class: \code{\linkS4class{adpcr}}, \code{\linkS4class{ddpcr}} or
#' \code{\linkS4class{rtadpcr}}
#' @keywords classes

setClass("dpcr", contains = "matrix", representation(.Data = "matrix",
                                                     n = "integer",
                                                     exper = "factor",
                                                     replicate = "factor",
                                                     type = "character"))

construct_dpcr <- function(data, n, exper = "Experiment1", 
                           replicate = NULL, type) {
  
  # data
  if (is.vector(data))
    data <- as.matrix(data)
  if (!(is.matrix(data))) {
    warning("'data' converted to matrix.")
    data <- as.matrix(data)
  }
  
  # n
  if (!(is.integer(n))) {
    n <- num2int(n)
  }
  
  if(length(n) != ncol(data)) {
    if(length(n) == 1) {
      warning(paste0("Assumed the number of partitions in each experiment is equal to ",
                     n, "."))
      n <- rep(n, ncol(data))
    } else {
      stop("Each run must have known number of partitions.")
    }
  }
  
  # exper
  if(length(exper) != ncol(data)) {
    if(length(exper) == 1) {
      exper <- as.factor(rep(exper, ncol(data)))
    } else {
      stop("Each run be assigned to an experiment.")
    }
  } 
  
  # replicate
  if(is.null(replicate)) {
    replicate <- factor(1L:ncol(data))
  }
  if(length(replicate) != ncol(data)) {
    stop("Each run have replicate id.")
  } 
  
  # type
  if (!(type %in% c("ct", "fluo", "nm", "np", "tnp"))) 
    stop(paste0(type, " is not recognized type value."))
  
  colnames(data) <- paste0(exper, ".", replicate)
  
  result <- new("dpcr", .Data = data, exper = exper, 
                replicate = replicate, type = type)
  #since n cannot be defined in new(), because of some strange error
  slot(result, "n") <- n
  result
}


#' Class \code{"adpcr"} - end-point array digital PCR experiments
#' 
#' A class specifically designed to contain results from end-point array
#' digital PCR experiments. Data is represented as matrix, where each column
#' describes different experiment. Type of data in all columns is specified in
#' slot \code{"type"}. Inherits from \code{\linkS4class{dpcr}}.
#' 
#' @name adpcr-class
#' @aliases adpcr-class adpcr
#' @docType class
#' @slot breaks \code{"numeric"} vector giving the number of intervals into which 
#' \code{.Data} should be cut. The second element in \code{breaks} vector is considered 
#' a threshold. Partition above or equal to threshold is counted as 
#' positive.
#' @slot type Object of class \code{"character"} defining type of data. See Details.
#' @details
#' Possible \code{type} values of \code{adpcr} objects:
#' \enumerate{
#'  \item{\code{"ct"}: cycle threshold of each well,}
#'  \item{\code{"fluo"}: fluorescence of each well,}
#'  \item{\code{"nm"}: number of molecules in each well,}
#'  \item{\code{"np"}: status (positive (1) or negative(0)) of each well,}
#'  \item{\code{"tnp"}: total number of positive wells in the panel (single value per each 
#'  panel, not per well).}}
#' @author Michal Burdukiewicz.
#' @seealso Ploting and management: \code{\link{bind_dpcr}},
#' \code{\link{extract_dpcr}}, \code{\link{plot_panel}}.
#' 
#' Tests: \code{\link{test_panel}}.
#' 
#' Simulation: \code{\link{sim_adpcr}}.
#' 
#' Real-time array digital PCR: \code{\linkS4class{rtadpcr}}.
#' 
#' Droplet digital PCR: \code{\linkS4class{ddpcr}}.
#' @keywords classes
#' @examples
#' 
#' rand_array <- sim_adpcr(400, 1600, 100, pos_sums = FALSE, n_panels = 5)
#' one_rand_array <- extract_dpcr(rand_array, 1)
#' plot_panel(one_rand_array, 40, 40)
#' 
setClass("adpcr", contains = "dpcr", representation(breaks = "numeric"))


#constructor
create_adpcr <- function(data, n, exper = "Experiment1", 
                         replicate = NULL, type, breaks) {
  result <- construct_dpcr(data = data, n = n, exper = exper, 
                               replicate = replicate, type = type)
  
  if (type == "ct")
    stop("'ct' type is not implemented for 'ddpcr' objects.")
  
  if (is.null(breaks))
    breaks <- 0L:max(data)
  
  class(result) <- "adpcr"
  slot(result, "breaks") <- breaks
  result
}

#' Class \code{"ddpcr"}
#' 
#' A class specifically designed to contain results from droplet digital PCR
#' experiments. Data is represented as matrix, where each column describes
#' different experiment. Type of data in all columns is specified in
#' slot \code{"type"}. Inherits from \code{\linkS4class{dpcr}}.
#' 
#' 
#' @name ddpcr-class
#' @aliases ddpcr-class ddpcr
#' @docType class
#' @slot threshold \code{"numeric"} value giving the threshold. Droplets equal or
#' bigger than threshold are counted as positive.
#' @details
#' Possible \code{type} values of \code{adpcr} objects:
#' \enumerate{
#'  \item{\code{"fluo"}: fluorescence of each droplet,}
#'  \item{\code{"nm"}: number of molecules in each droplet,}
#'  \item{\code{"np"}: status (positive (1) or negative(0)) of each droplet,}
#'  \item{\code{"tnp"}: total number of positive droplets in the reaction 
#'  (single value per each reaction, not per droplet).}}
#' @author Michal Burdukiewicz.
#' @seealso Ploting and managment: \code{\link{bind_dpcr}},
#' \code{\link{extract_dpcr}}, \code{\link{plot_vic_fam}}.
#' 
#' Simulation: \code{\link{sim_ddpcr}}.
#' 
#' Array digital PCR: \code{\linkS4class{adpcr}}.
#' @keywords classes
#' @examples
#' 
#' ddpcr_fluo <- sim_ddpcr(m = 10, n = 20, times = 5, fluo = list(0.1, 0))
#' plot(ddpcr_fluo)
#' 
#' ddpcr <- sim_ddpcr(m = 10, n = 20, times = 5)

setClass("ddpcr", contains = "dpcr", representation(threshold = "numeric"))

#constructor
create_ddpcr <- function(data, n, exper = "Experiment1", 
                         replicate = NULL, type, threshold) {
  
  result <- construct_dpcr(data = data, n = n, exper = exper, 
                               replicate = replicate, type = type)
  
  class(result) <- "ddpcr"
  slot(result, "threshold") <- threshold
  result
}