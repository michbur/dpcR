#' Create dpcR object
#' 
#' Quick creation of \code{\linkS4class{adpcr}} and\code{\linkS4class{ddpcr}}
#' objects.
#' 
#' This function assists in creation of objects used by other functions of the
#' package. Its inbuilt capabilities include checking the correctness of
#' arguments.
#' 
#' A warning is prompted whenever any of arguments is converted to other type.
#' 
#' @param data a \code{"numeric"} vector or matrix of data from dPCR
#' experiments.  Data frames will be converted to matrices.
#' @param n \code{"integer"} equal to number of partitions.
#' @param threshold \code{"numeric"} value giving the threshold above which
#' droplet is counted as positive.  Ignored if \code{adpcr} is \code{TRUE}.
#' @param breaks \code{"numeric"} vector giving the number of intervals into
#' which \code{data} should be cut.  Ignored if \code{adpcr} is \code{FALSE}.
#' @param type Object of class \code{"character"} defining type of data. Could
#' be \code{"nm"} (number of molecules per partition), \code{"tp"} (total
#' number of positive wells in the panel), \code{"fluo"} (fluorescence) or
#' \code{"ct"} (threshold cycle).
#' @param adpcr logical. If \code{TRUE}, function creates
#' \code{\linkS4class{adpcr}} object.  If \code{FALSE}, function creates
#' \code{\linkS4class{ddpcr}} object.
#' @return An \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}} object.
#' @note Currently only end-point measurements are supported.
#' @author Michal Burdukiewicz, Stefan Roediger.
#' @keywords ddPCR adPCR
#' @examples
#' 
#' # Droplet digital PCR example
#' sample_runs <- matrix(rpois(60, lambda = 1.5), ncol = 2)
#' ddpcr1 <- create_dpcr(sample_runs[,1], n = 30L, 
#' threshold = 1, type = "nm", adpcr = FALSE)
#' ddpcr2 <- create_dpcr(sample_runs[,2], n = 30L, 
#' threshold = 1, type = "nm", adpcr = FALSE)
#' plot_vic_fam(ddpcr1, ddpcr2)
#' 
#' # Array digital PCR example
#' sample_adpcr <- create_dpcr(rpois(765, lambda = 0.8), n = 765L, 
#' 			    type = "nm", adpcr = TRUE)
#' plot_panel(sample_adpcr, 45, 17)
#' 
#' @export create_dpcr
create_dpcr <- function(data, n, threshold = NULL, breaks = NULL, type, adpcr = TRUE) {
  if (adpcr != TRUE && adpcr != FALSE)
    stop("'adpcr' argument must have TRUE or FALSE value.", call. = TRUE, domain = NA)
  
  if (type == "ct" && adpcr == FALSE)
    stop("'ct' type is not implemented for 'ddpcr' objects.", call. = TRUE, domain = NA)
  
  if (!(is.integer(n))) {
    warning("'n' converted to integer.")
    n <- as.integer(n)
  }
  
  if (is.vector(data))
    data <- as.matrix(data)
  if (!(is.matrix(data))) {
    warning("'data' converted to matrix.")
    data <- as.matrix(data)
  }
  
  if (adpcr) {
    create_adpcr(data, n, breaks, type)
  } else {
    create_ddpcr(data, n, threshold, type)
  }
}
