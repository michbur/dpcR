#' Class \code{"adpcr"} - end-point array digital PCR experiments
#' 
#' A class specifically designed to contain results from end-point array
#' digital PCR experiments. Data is represented as matrix, where each column
#' describes different experiment. Type of data in all columns is specified in
#' slot \code{"type"} and could be a number of molecules \code{"nm"}, a number
#' of positive droplets \code{"tnm"} (in this case whole experiment is
#' represented by one row), a cycle threshold of each well \code{"ct"} or
#' fluorescence values \code{"fluo"}.
#' 
#' 
#' @name adpcr-class
#' @aliases adpcr-class adpcr
#' @docType class
#' @section Slots: \describe{ \item{list(".Data")}{\code{"matrix"} containing
#' data from array. See Description.}\item{list("n")}{Object of class
#' \code{"integer"} equal to the number of partitions in each
#' experiment.} \item{list("breaks")}{\code{"numeric"}
#' vector giving the number of intervals into which \code{.Data} should be
#' cut.} \item{list("type")}{Object of class
#' \code{"character"} defining type of data. Could be \code{"nm"} (number of
#' molecules per partition), \code{"tp"} (total number of positive wells in
#' panel), \code{"fluo"} (fluorescence) or \code{"ct"} (threshold
#' cycle).} }
#' @author Michal Burdukiewicz.
#' @seealso Ploting and management: \code{\link{bind_dpcr}},
#' \code{\link{extract_dpcr}}, \code{\link{plot_panel}}.
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
  slot(result, "type") <- type
  if (is.null(breaks)) {
    slot(result, "breaks") <- 0L:max(data)
  } else {
    slot(result, "breaks") <- breaks
  }
  if (!is.null(models))
    slot(result, "models") <- models
  result
}