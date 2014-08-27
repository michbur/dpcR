#' Class \code{"ddpcr"}
#' 
#' A class specifically designed to contain results from droplet digital PCR
#' experiments. Data is represented as matrix, where each column describes
#' different experiment. Type of data in all columns is specified in slot
#' \code{"type"} and could be a by number of molecules \code{"nm"}, number of
#' positive droplets \code{"tnm"} (in this case whole experiment is represented
#' by one row) or fluorescence \code{"fluo"}.
#' 
#' 
#' @name ddpcr-class
#' @aliases ddpcr-class ddpcr
#' @docType class
#' @section Slots: \describe{ \item{list(".Data")}{\code{"matrix"} containing
#' data from all droplets. See Description.}\item{:}{\code{"matrix"} containing
#' data from all droplets. See Description.} \item{list("n")}{\code{"integer"}
#' representing number of partitions.}\item{:}{\code{"integer"} representing
#' number of partitions.} \item{list("threshold")}{ \code{"numeric"} value
#' giving the threshold above which droplet is counted as positive.}\item{:}{
#' \code{"numeric"} value giving the threshold above which droplet is counted
#' as positive.} \item{list("type")}{Object of class \code{"character"}
#' defining type of data. Could be \code{"nm"} (Number of molecules per
#' partition), \code{"tp"} (number of positive droplets) or \code{"fluo"}
#' (fluorescence).}\item{:}{Object of class \code{"character"} defining type of
#' data. Could be \code{"nm"} (Number of molecules per partition), \code{"tp"}
#' (number of positive droplets) or \code{"fluo"} (fluorescence).} }
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
NULL


setClass("ddpcr", contains = "matrix", representation(.Data = "matrix", n = "integer", 
                                                      threshold = "numeric", 
                                                      type = "character"))

setMethod("summary", signature(object = "ddpcr"), function(object, print = TRUE) {
  data <- slot(object, ".Data")
  col_dat <-ncol(data)
  type <- slot(object, "type")
  n <- slot(object, "n")
  
  if (type %in% c("nm", "tp")) 
    k <- colSums(data > 0, na.rm = TRUE)
  
  if (type %in% c("fluo")) 
    k <- apply(data, 2, function(x) get_k_n(x, slot(object, "threshold")))
  
  invisible(print_summary(k, col_dat, type, n, print, colnames(data)))
})

setMethod("show", signature(object = "ddpcr"), function(object) {
  print(slot(object, ".Data"))
  cat(paste0("\nType: '", slot(object, "type"), "'"))     
})


