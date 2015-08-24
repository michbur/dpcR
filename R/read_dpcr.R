#' Read digital PCR data
#' 
#' Reads digital PCR data in various formats.
#' 
#' @param file name of the input file.
#' @param format of the file.
#' @param ... additional arguments for the appropriate format.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @details Input files may be in .csv, .xls or .xlsx format. In case of Excel files with 
#' multiple sheets, only the first sheet will be analyzed.
#' @export
#' @seealso 
#' \code{\link{read_raw}}

read_dpcr <- function(file, format, ...) {
  switch(format,
         raw = read_raw(file, ...),
         QX100 = read_QX100(file),
         BioMark = read_BioMark(file, ...))
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
#' @param file name of the input file.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @seealso See \code{\link{read_dpcr}} for detailed description of input files.
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


#' Read BioMark
#' 
#' Reads digital PCR data from the BioMark (Fluidigm).
#' 
#' @param file name of the input file.
#' @param detailed logical, if \code{TRUE}, the input file is processed as if it was 
#' 'Detailed Table Results'. In the other case, the expected input file structure is
#' 'Summary Table Results'.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @seealso See \code{\link{read_dpcr}} for detailed description of input files.
#' @export

read_BioMark <- function(file, detailed = FALSE) {
  dat <- read.csv(file)
  
  data_range <- 10L:57
  
  #dat[apply(dat, 1, function(row) sum(is.na(row))) == 0, ]
  
  names1 <- as.vector(dat[8, ])
  names2 <- as.vector(dat[9, ])
  
  #exper
  exper <- rep(paste0(dat[data_range, names1 == "Sample Information" & names2 == "Name"], "_",
                      dat[data_range, names1 == "Sample Information" & names2 == "Type"]), 2)
  
  #replicate
  replicate <- paste0(rep(dat[data_range, names1 == "Panel" & names2 == "ID"], 2),
                      unlist(lapply(c("VIC-TAMRA", "FAM-MGB"), function(channel_name)
                        dat[data_range, names1 == channel_name & names2 == "Type"])))
  
  #dat[data_range, names1 == "Sample Information" & names2 == "rConc."]
  
  #assay
  assay <- unlist(lapply(c("VIC-TAMRA", "FAM-MGB"), function(channel_name)
    dat[data_range, names1 == channel_name & names2 == "Name"]
  ))
  
  #data
  count_data <- unlist(lapply(c("VIC-TAMRA", "FAM-MGB"), function(channel_name)
    as.numeric(dat[data_range, names1 == channel_name & names2 == "Count"])))
  
  
  res <- create_dpcr(data = matrix(count_data, nrow = 1), n = rep(765, length(count_data)), 
                     exper = exper, replicate = replicate, type = "tnp",
                     assay = assay, adpcr = TRUE, row_names = as.character(1L:4), 
                     col_names = as.character(1L:12), 
                     panel_id = factor(c(rep(1, length(exper)/2), rep(2, length(exper)/2))))
  
  names_df <- data.frame(table(slot(res, "panel_id"), slot(res, "assay")))
  levels(slot(res, "panel_id")) <- as.character(sapply(levels(names_df[["Var1"]]), 
                                                       function(single_name) {
                                                         sub_data <- names_df[names_df[["Var1"]] == single_name, ]
                                                         sub_data[which.max(sub_data[["Freq"]]), "Var2"]
                                                       }))
  res
  
}


#checks the extension and returns proper read function
# read_file <- function(file) {
#   ext <- strsplit(file, ".", fixed = TRUE)[[1]]
#   
#   #add multisheet excel
#   
#   fun <- switch(ext[[length(ext)]],
#                 csv = read.csv,
#                 xls = read_excel,
#                 xlsx = read_excel)
#   browser()
# 
#   fun(file)
# }