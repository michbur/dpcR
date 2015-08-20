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
         raw_ddpcr = read_raw(file, adpcr = FALSE, ...),
         QX100 = read_QX100(file))
}

#' Read digital PCR raw data
#' 
#' Reads digital PCR data in raw format.
#' 
#' @inheritParams create_dpcr
#' @inheritParams read_dpcr
#' @author Michal Burdukiewcz, Stefan Roediger
#' @export

read_raw <- function(file, adpcr) {
  dat <- read.csv(file)
  
  n <- rowSums(!apply(dat, 1, is.na))
  
  exp_rep <- matrix(unlist(strsplit(colnames(dat), ".", fixed = TRUE)), ncol = 2, byrow = TRUE)
  
  create_dpcr(data = as.matrix(dat), n = n, exper = exp_rep[, 1], replicate = exp_rep[, 2], type = "np",
              adpcr = adpcr)
}


#' Read QX100
#' 
#' Reads digital PCR data from the QX100 Droplet Digital PCR System (Bio-Rad)
#' 
#' @inheritParams create_dpcr
#' @param file name of the input file.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @export

read_QX100 <- function(file) {
  dat <- read.csv(file)
  
  n <- dat[["AcceptedDroplets"]]
  counts <- matrix(dat[["Positives"]], nrow = 1)
  exper <- dat[["TypeAssay"]]
  replicate <- paste0(dat[["Well"]], ".", dat[["Sample"]])

  create_dpcr(data = matrix(dat[["Positives"]], nrow = 1), n = n, 
              exper = exper, replicate = replicate, type = "tnp",
              assay = dat[["Assay"]], adpcr = TRUE, 
              col_names = LETTERS[1L:8], row_names = as.character(1L:4),
              panel_id = dat[["Assay"]])
}