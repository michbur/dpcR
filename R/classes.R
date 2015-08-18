#' Class \code{"dpcr"} - general digital PCR
#' 
#' A class containing results of any digital PCR experiment. 
#' Type of data in all columns is specified in slot \code{"type"}.
#' 
#' @name dpcr-class
#' @aliases dpcr-class dpcr
#' @docType class
#' @slot .Data \code{matrix} containing data from array. See Description.
#' @slot n \code{integer} equal to the number of partitions in each
#' experiment.
#' @slot exper \code{factor} representing the id or name of experiments.
#' @slot replicate \code{factor} representing the id or name of replicate.
#' @slot assay \code{factor} representing the id or name of the assay.
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
                                                     assay = "factor",
                                                     type = "character"))

construct_dpcr <- function(data, n, exper = "Experiment1", 
                           replicate = NULL, assay = "Unknown", type) {
  
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
      exper <- rep(exper, ncol(data))
    } else {
      stop("Each run must be assigned to an experiment.")
    }
  } 
  if(class(exper) != "factor")
    exper <- as.factor(exper)
  
  
  # replicate
  if(is.null(replicate)) {
    replicate <- 1L:ncol(data)
  }
  if(length(replicate) != ncol(data)) {
    stop("Each run have replicate id.")
  } 
  if(class(replicate) != "factor")
    replicate <- as.factor(replicate)
  

  # assay
  if(length(assay) != ncol(data)) {
    if(length(assay) == 1) {
      assay <- rep(assay, ncol(data))
    } else {
      stop("Each run must be assigned to an assay.")
    }
  } 
  
  if(class(assay) != "factor")
    assay <- as.factor(assay)
  
  
  # type
  if (!(type %in% c("ct", "fluo", "nm", "np", "tnp"))) 
    stop(paste0(type, " is not recognized type value."))
  
  colnames(data) <- paste0(exper, ".", replicate)
  
  result <- new("dpcr", .Data = data, exper = exper, 
                replicate = replicate, assay = assay, type = type)
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
#' @slot col_names \code{"character"} vector naming the columns in the array.
#' @slot row_names \code{"character"} vector naming the rows in the array.
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
#' @seealso Data management: \code{\link{adpcr2panel}}, \code{\link{bind_dpcr}},
#' \code{\link{extract_dpcr}}.
#' 
#' Plotting: \code{\link{plot_panel}}.
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
setClass("adpcr", contains = "dpcr", representation(breaks = "numeric",
                                                    col_names = "character",
                                                    row_names = "character"))


#constructor
create_adpcr <- function(data, n, exper = "Experiment1", 
                         replicate = NULL, assay = "Unknown", type, breaks, 
                         col_names = NULL, row_names = NULL) {
  result <- construct_dpcr(data = data, n = n, exper = exper, 
                               replicate = replicate, assay = assay, type = type)
  
  if (type == "ct")
    stop("'ct' type is not implemented for 'adpcr' objects.")
  
  if (is.null(breaks)) {
    breaks <- 0L:max(data, na.rm = TRUE)
    #if data range is too big, add smaller breaks
    if(length(breaks) > 5) {
      breaks <- hist(data, 5, plot = FALSE)[["breaks"]]
    }
  }  
  
  if(is.null(col_names) & is.null(row_names)) {
    #access .Data slot, because its already in the matrix form
    if(type == "tnp") {
      #in case of tnp the whole apcr object represents one array
      total <- ncol(slot(result, ".Data"))
    } else {
      total <- nrow(slot(result, ".Data"))
    }
    #nice proportion of the rows to columns is 0.37
    edge_a <- round(sqrt(total/0.37), 0)
    edge_b <- floor(total/edge_a)
    
    while(edge_a * edge_b != total) {
      edge_a <- edge_a - 1
      edge_b <- floor(total/edge_a)
    }
      
    col_names <- as.character(1L:edge_a)
    row_names <- as.character(1L:edge_b)
  }
  
  if(xor(is.null(col_names), is.null(row_names))) {
    stop("Both 'col_names' and 'row_names' must be either NULL or specified.")
  }
  
  class(result) <- "adpcr"
  slot(result, "breaks") <- breaks
  slot(result, "col_names") <- col_names
  slot(result, "row_names") <- row_names
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
#' @slot threshold \code{numeric} value giving the threshold. Partition with the value equal or 
#' bigger than threshold are considered positive.
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
                         replicate = NULL, assay = "Unknown", type, threshold) {
  
  result <- construct_dpcr(data = data, n = n, exper = exper, 
                               replicate = replicate, assay = assay, type = type)
  
  class(result) <- "ddpcr"
  if(is.null(threshold))
    threshold <- mean(range(data))
    
  slot(result, "threshold") <- threshold
  result
}