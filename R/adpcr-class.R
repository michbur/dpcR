#' Class \code{"adpcr"} - end-point array digital PCR experiments
#' 
#' A class specifically designed to contain results from end-point array
#' digital PCR experiments. Data is represented as matrix, where each column
#' describes different experiment. Type of data in all columns is specified in
#' slot \code{"type"}.
#' 
#' @name adpcr-class
#' @aliases adpcr-class adpcr
#' @docType class
#' @slot .Data \code{"matrix"} containing data from array. See Description.
#' @slot n Object of class \code{"integer"} equal to the number of wells in each
#' experiment.
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
setClass("adpcr", contains = "matrix", representation(.Data = "matrix",
                                                      n = "integer",
                                                      breaks = "numeric",
                                                      type = "character"))


#constructor
create_adpcr <- function(data, n, breaks = NULL, type, models = NULL, 
                         col_names = 1L:ncol(data)) {
  result <- new("adpcr")
  if (!is.null(col_names) && is.null(colnames(data)))
    colnames(data) <- col_names
  slot(result, ".Data") <- data
  slot(result, "n") <- n
  if (type %in% c("ct", "fluo", "nm", "np", "tnp")) {
    slot(result, "type") <- type
  } else {
    stop(paste0(type, " is not recognized type value."))
  }
  if (is.null(breaks)) {
    slot(result, "breaks") <- 0L:max(data)
  } else {
    slot(result, "breaks") <- breaks
  }
  if (!is.null(models))
    slot(result, "models") <- models
  result
}