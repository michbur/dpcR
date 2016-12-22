#' Class \code{"dpcr"} - general digital PCR
#' 
#' A class containing results of any digital PCR experiment. 
#' Type of data in all columns is specified in slot \code{"type"}.
#' 
#' @name dpcr-class
#' @aliases dpcr-class dpcr
#' @docType class
#' @slot .Data \code{matrix} data from digital PCR experiments. See Details.
#' @slot n \code{integer} equal to the number of partitions in each run.
#' @slot exper \code{factor} the id or name of experiments.
#' @slot replicate \code{factor} the id or name of replicates.
#' @slot assay \code{factor} the id or name of the assay.
#' @slot v \code{"numeric"} volume of the partition [nL].
#' @slot uv \code{"numeric"} uncertainty of the volume of the partition [nL]. 
#' @slot threshold \code{"numeric"} value specifying the threshold. Partition with 
#' the value equal or bigger than threshold are considered positive.
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
#'  
#' Digital PCR data is always a matrix, where columns and rows represent 
#' respectively runs and data points. For example, matrix with 2 columns and 765 rows
#' means two runs with 765 data points each. In case of \code{"tnp"} data, each run is
#' represented by only one measurement, the count of all positive partitions.
#' 
#' The number of partitions is defined in slot \code{n}. In the previous example, 
#' two runs have 765 data points, but they can have less detected partitions 
#' (for example some reads may be not available). In this case, the data point will 
#' have value NA. 
#' 
#' The structure of \code{dpcr} class is described more deeply in the vignette.
#' @author Michal Burdukiewicz.
#' @note 
#' This class represent the most general droplet-based digital PCR. In more specific 
#' cases, the user is directed to other classes: \code{\linkS4class{adpcr}}, where 
#' results can be placed over a plate, \code{\linkS4class{qdpcr}} where digital 
#' assay is based on multiple qPCR experiments and \code{\linkS4class{rtadpcr}},
#' where data points represent the status of partitions measured in the 
#' real time.
#' @keywords classes
#' @examples
#' 
#' dpcr_fluo <- sim_dpcr(m = 10, n = 20, times = 5, fluo = list(0.1, 0))
#' plot(dpcr_fluo)
#' 
#' dpcr <- sim_dpcr(m = 10, n = 20, times = 5)


setClass("dpcr", contains = "matrix", representation(.Data = "matrix",
                                                     n = "integer",
                                                     exper = "factor",
                                                     replicate = "factor",
                                                     assay = "factor",
                                                     v = "numeric",
                                                     uv = "numeric",
                                                     type = "character",
                                                     threshold = "numeric"))

construct_dpcr <- function(data, n, exper = "Experiment1", 
                           replicate = NULL, assay = "Unknown", type,
                           v = 1, uv = 0, threshold = NULL) {
  
  # data
  if (is.vector(data))
    data <- as.matrix(data)
  if (!(is.matrix(data))) {
    message("'data' converted to matrix.")
    data <- as.matrix(data)
  }
  
  # n
  if (!(is.integer(n))) {
    n <- num2int(n)
  }
  
  if(length(n) != ncol(data)) {
    if(length(n) == 1) {
      message(paste0("The assumed number of partitions in each run is equal to ",
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
  if(!is.factor(exper))
    exper <- as.factor(exper)
  
  
  # replicate
  if(is.null(replicate)) {
    replicate <- 1L:ncol(data)
  }
  if(length(replicate) != ncol(data)) {
    stop("Each run have replicate id.")
  } 
  if(!is.factor(replicate))
    replicate <- as.factor(replicate)
  
  run_names <- paste0(exper, ".", replicate)
  dups <- duplicated(run_names)
  
  if(any(dups)) {
    # awful workaround
    exper <- as.character(exper)
    exper[!dups] <- paste0(exper[!dups], "1")
    exper[dups] <- paste0(exper[dups], "2")
    
    exper <- as.factor(exper)
    run_names <- paste0(exper, ".", replicate)
  }
  
  # assay
  if(length(assay) != ncol(data)) {
    if(length(assay) == 1) {
      assay <- rep(assay, ncol(data))
    } else {
      stop("Each run must be assigned to an assay.")
    }
  } 
  
  if(!is.factor(assay))
    assay <- as.factor(assay)
  
  # type
  if (!(type %in% c("ct", "fluo", "nm", "np", "tnp"))) 
    stop(paste0(type, " is not recognized type value."))
  
  # volume
  if(length(v) != ncol(data)) {
    if(length(v) == 1) {
      message(paste0("The assumed volume of partitions in each run is equal to ",
                     v, "."))
      v <- rep(v, ncol(data))
    } else {
      stop("Each run must have known volume.")
    }
  }
  
  # volume uncertainty
  if(length(uv) != ncol(data)) {
    if(length(uv) == 1) {
      message(paste0("The assumed volume uncertainty in each run is equal to ",
                     uv, "."))
      uv <- rep(uv, ncol(data))
    } else {
      stop("Each run must have known volume uncertainty.")
    }
  }
  
  colnames(data) <- run_names
  
  if(is.null(threshold))
    threshold <- mean(range(data))
  
  result <- new("dpcr", .Data = data, exper = exper, 
                replicate = replicate, assay = assay, v = v, uv = uv, 
                threshold = threshold, type = type)
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
#' @slot col_names \code{"character"} vector naming columns in the array.
#' @slot row_names \code{"character"} vector naming rows in the array.
#' @slot row_id \code{"integer"} vector providing row indices of all runs.
#' @slot col_id \code{"integer"} vector providing column indices of all runs.
#' @slot panel_id \code{"factor"} naming the panel to which experiment belong.
#' @details
#' For more in-depth explanation of digital PCR data structure, see 
#' \code{\linkS4class{dpcr}}.
#' @author Michal Burdukiewicz.
#' @seealso Data management: \code{\link{adpcr2panel}}, \code{\link{bind_dpcr}},
#' \code{\link{extract_run}}.
#' 
#' Plotting: \code{\link{plot_panel}}.
#' 
#' Tests: \code{\link{test_panel}}.
#' 
#' Simulation: \code{\link{sim_adpcr}}.
#' 
#' Real-time array digital PCR: \code{\linkS4class{rtadpcr}}.
#' 
#' Droplet digital PCR: \code{\linkS4class{dpcr}}.
#' @keywords classes
#' @examples
#' 
#' rand_array <- sim_adpcr(400, 1600, 100, pos_sums = FALSE, n_panels = 5)
#' one_rand_array <- extract_run(rand_array, 1)
#' plot_panel(one_rand_array, 40, 40)
#' 
setClass("adpcr", contains = "dpcr", representation(col_names = "character",
                                                    row_names = "character",
                                                    col_id = "numeric",
                                                    row_id = "numeric",
                                                    panel_id = "factor"))


#constructor
create_adpcr <- function(data, n, exper = "Experiment1", 
                         replicate = NULL, assay = "Unknown", v = 1, uv = 0, type, breaks, 
                         col_names = NULL, row_names = NULL, 
                         col_id = NULL, row_id = NULL,
                         panel_id = NULL, threshold = NULL) {
  result <- construct_dpcr(data = data, n = n, exper = exper, 
                           replicate = replicate, assay = assay, 
                           v = v, uv = uv, type = type, threshold = threshold)
  
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
  
  if(is.null(col_id) & is.null(row_id)) {
    col_id <- sort(rep(1L:edge_a, edge_b))
    row_id <- rep(1L:edge_b, edge_a)
  }
  
  if(xor(is.null(col_names), is.null(row_names))) {
    stop("Both 'col_names' and 'row_names' must be either NULL or specified.")
  }
  
  if(is.null(panel_id)) {
    if(type != "tnp") {
      panel_id <- as.factor(1L:ncol(slot(result, ".Data")))
    } else {
      panel_id <- as.factor(rep(1L, ncol(slot(result, ".Data"))))
    }
  } 
  
  class(result) <- "adpcr"
  slot(result, "col_names") <- col_names
  slot(result, "row_names") <- row_names
  slot(result, "col_id") <- col_id
  slot(result, "row_id") <- row_id
  slot(result, "panel_id") <- panel_id
  result
}
