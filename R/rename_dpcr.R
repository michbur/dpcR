#' Rename object
#' 
#' Renames objects of class \code{\linkS4class{adpcr}} or \code{\linkS4class{dpcr}}.
#' 
#' @param x an \code{\linkS4class{adpcr}} or \code{\linkS4class{dpcr}} object.
#' @param exper a vector of new experiments' names. If \code{NULL},
#' experiments' names are not changed.
#' @param replicate a vector of new replicates' ids. If \code{NULL},
#' replicates' names are not changed.
#' @param assay a vector of new assays' names. If \code{NULL},
#' assays' names are not changed.
#' @details The valid \code{exper}, \code{replicate} and \code{assay} names 
#' are factors. For the sake of convenience, this function converts other types 
#' to factors if it is possible.
#' @keywords manip
#' @export


rename_dpcr <- function(x, exper = NULL, replicate = NULL, assay = NULL) {
  #add check if numeric
  if (!is.null(exper)) {
    if(length(exper) == 1) {
      exper <- as.factor(rep(exper, ncol(slot(x, ".Data"))))
    }
    if(!is.factor(exper)) {
      exper <- as.factor(exper)
      message("'exper' converted to factor.")
    }
    slot(x, "exper") <- exper
  }
    
  if (!is.null(replicate)) {
    if(!is.factor(replicate)) {
      replicate <- as.factor(replicate)
      message("'replicate' converted to factor.")
    }
    slot(x, "replicate") <- replicate
  }
    
  if (!is.null(assay)) {
    if(!is.factor(assay)) {
      assay <- as.factor(assay)
      message("'assay' converted to factor.")
    }
    slot(x, "assay") <- assay
  }
  colnames(slot(x, ".Data")) <- paste0(slot(x, "exper"), ".", slot(x, "replicate"))
  x
}