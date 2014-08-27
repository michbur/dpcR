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
#' data from array. See Description.}\item{:}{\code{"matrix"} containing data
#' from array. See Description.} \item{list("n")}{Object of class
#' \code{"integer"} equal to the number of partitions in each
#' experiment.}\item{:}{Object of class \code{"integer"} equal to the number of
#' partitions in each experiment.} \item{list("breaks")}{\code{"numeric"}
#' vector giving the number of intervals into which \code{.Data} should be
#' cut.}\item{:}{\code{"numeric"} vector giving the number of intervals into
#' which \code{.Data} should be cut.} \item{list("type")}{Object of class
#' \code{"character"} defining type of data. Could be \code{"nm"} (number of
#' molecules per partition), \code{"tp"} (total number of positive wells in
#' panel), \code{"fluo"} (fluorescence) or \code{"ct"} (threshold
#' cycle).}\item{:}{Object of class \code{"character"} defining type of data.
#' Could be \code{"nm"} (number of molecules per partition), \code{"tp"} (total
#' number of positive wells in panel), \code{"fluo"} (fluorescence) or
#' \code{"ct"} (threshold cycle).} }
#' @author Michal Burdukiewicz.
#' @seealso Ploting and management: \code{\link{bind_dpcr}},
#' \code{\link{extract_dpcr}}, \code{\link{plot_panel}}.
#' 
#' Simulation: \code{\link{sim_adpcr}}.
#' 
#' Real-time array digital PCR: \code{\linkS4class{rtadpcr}}.
#' 
#' Droplet digital PCR: \code{\linkS4class{ddpcr}}.
#' @keywords classes real-time
#' @examples
#' 
#' rand_array <- sim_adpcr(400, 1600, 100, pos_sums = FALSE, n_panels = 5)
#' one_rand_array <- extract_dpcr(rand_array, 1)
#' plot_panel(one_rand_array, 40, 40)
#' 
NULL
setClass("adpcr", contains = "matrix", representation(.Data = "matrix",
                                                      n = "integer",
                                                      breaks = "numeric",
                                                      type = "character"))


setMethod("summary", signature(object = "adpcr"), function(object, print = TRUE) {
  data <- slot(object, ".Data")
  
  col_dat <- ncol(data)
  type <- slot(object, "type")
  n <- slot(object, "n")
  
  if (type %in% c("fluo", "ct")) 
    stop(paste0("Summary not currently implemented for data type ", type, "."), call. = TRUE, domain = NA)
  
  if (type %in% c("nm", "tp")) {
    k <- colSums(data > 0, na.rm = TRUE)
  }
  
  invisible(print_summary(k, col_dat, type, n, print, colnames(data)))
})

# Special method declared to hide slots other than .Data
setMethod("show", signature(object = "adpcr"), function(object) {
  print(slot(object, ".Data"))
  cat(paste0("\nType: '", slot(object, "type"), "'"))
})



