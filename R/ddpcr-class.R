#' Class \code{"ddpcr"}
#' 
#' A class specifically designed to contain results from droplet digital PCR
#' experiments. Data is represented as matrix, where each column describes
#' different experiment. Type of data in all columns is specified in
#' slot \code{"type"}.
#' 
#' 
#' @name ddpcr-class
#' @aliases ddpcr-class ddpcr
#' @docType class
#' @slot .Data \code{"matrix"} containing data from all droplets. See Description.
#' @slot n Object of class \code{"integer"} equal to the number of droplets in each
#' experiment.
#' @slot threshold \code{"numeric"} value giving the threshold. Droplets equal or
#' bigger than threshold are counted as positive.
#' @slot type Object of class \code{"character"} defining type of data. See Details.
#' 
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
#' 
setClass("ddpcr", contains = "matrix", representation(.Data = "matrix", n = "integer", 
                                                      threshold = "numeric", 
                                                      type = "character"))

#constructor
create_ddpcr <- function(data, n, threshold = NULL, type, col_names = 1L:ncol(data)) {
  result <- new("ddpcr")
  if (!is.null(col_names) && is.null(colnames(data)))
    colnames(data) <- col_names
  slot(result, ".Data") <- data
  slot(result, "n") <- n
  if (type %in% c("fluo", "nm", "np", "tnp")) {
    slot(result, "type") <- type
  } else {
    stop(paste0(type, " is not recognized type value."))
  }
  slot(result, "threshold") <- threshold
  result
}