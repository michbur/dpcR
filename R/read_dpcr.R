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
  read.csv(file)
}
