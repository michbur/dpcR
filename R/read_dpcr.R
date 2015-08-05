#' Read digital PCR data
#' 
#' Reads digital PCR data in various formats.
#' 
#' @param file name of the input file.
#' @param format of the file.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @export

read_dpcr <- function(file, format) {
  switch(format,
         raw = read_raw(file))
}

#' Read digital PCR raw data
#' 
#' Reads digital PCR data in raw format.
#' 
#' @param file name of the input file.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @export

read_raw <- function(file) {
  dat <- read.csv(file)
  
  n <- rowSums(!apply(dat, 1, is.na))
  
  exp_rep <- matrix(unlist(strsplit(colnames(dat), ".", fixed = TRUE)), ncol = 2, byrow = TRUE)
  
  create_dpcr(data = as.matrix(dat), n = n, exper = exp_rep[, 1], replicate = exp_rep[, 2], type = "np",
              adpcr = TRUE)
}
