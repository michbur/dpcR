#' Read digital PCR data
#' 
#' Reads digital PCR data in various formats.
#' 
#' @param file name of the input file.
#' @param format of the file.
#' @param ... additional arguments for the appropriate format.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @export
#' @seealso 
#' \code{\link{read_raw}}

read_dpcr <- function(file, format, ...) {
  switch(format,
         raw_adpcr = read_raw(file, adpcr = TRUE, ...),
         raw_ddpcr = read_raw(file, adpcr = FALSE, ...))
}

#' Read digital PCR raw data
#' 
#' Reads digital PCR data in raw format.
#' 
#' @inheritParams create_dpcr
#' @param file name of the input file.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @export

read_raw <- function(file, adpcr) {
  dat <- read.csv(file)
  
  n <- rowSums(!apply(dat, 1, is.na))
  
  exp_rep <- matrix(unlist(strsplit(colnames(dat), ".", fixed = TRUE)), ncol = 2, byrow = TRUE)
  
  create_dpcr(data = as.matrix(dat), n = n, exper = exp_rep[, 1], replicate = exp_rep[, 2], type = "np",
              adpcr = adpcr)
}
