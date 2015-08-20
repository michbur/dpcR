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
         QX100 = read_QX100(file),
         BioMark = read_BioMark(file))
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


#' Read BioMark
#' 
#' Reads digital PCR data from the BioMark (Fluidigm) (Summary Table results)
#' 
#' @inheritParams create_dpcr
#' @param file name of the input file.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @export

read_BioMark <- function(file) {
  sheets <- lapply(excel_sheets(file), read_excel, path = file)
  
  data_range <- 10L:57
  
  #single_sheet[apply(single_sheet, 1, function(row) sum(is.na(row))) == 0, ]
  
  res <- do.call(bind_dpcr, lapply(1L:length(sheets), function(single_sheet_id) {
    single_sheet <- sheets[[single_sheet_id]]
    names1 <- as.vector(single_sheet[8, ])
    names2 <- as.vector(single_sheet[9, ])
    
    #exper
    exper <- rep(paste0(single_sheet[data_range, names1 == "Sample Information" & names2 == "Name"], "_",
                        single_sheet[data_range, names1 == "Sample Information" & names2 == "Type"]), 2)
    
    #replicate
    replicate <- paste0(rep(single_sheet[data_range, names1 == "Panel" & names2 == "ID"], 2),
                        unlist(lapply(c("VIC-TAMRA", "FAM-MGB"), function(channel_name)
                          single_sheet[data_range, names1 == channel_name & names2 == "Type"])))
    
    #single_sheet[data_range, names1 == "Sample Information" & names2 == "rConc."]
    
    #assay
    assay <- unlist(lapply(c("VIC-TAMRA", "FAM-MGB"), function(channel_name)
      paste0(single_sheet[data_range, names1 == channel_name & names2 == "Name"], "_",
             single_sheet_id)
    ))
    
    #data
    count_data <- unlist(lapply(c("VIC-TAMRA", "FAM-MGB"), function(channel_name)
      as.numeric(single_sheet[data_range, names1 == channel_name & names2 == "Count"])))
    
    
    create_dpcr(data = matrix(count_data, nrow = 1), n = rep(765, length(count_data)), 
                exper = exper, replicate = replicate, type = "tnp",
                assay = assay, adpcr = TRUE, row_names = as.character(1L:4), 
                col_names = as.character(1L:12), 
                panel_id = factor(c(rep(1, length(exper)/2), rep(2, length(exper)/2))))
  }))
  
  names_df <- data.frame(table(slot(res, "panel_id"), slot(res, "assay")))
  levels(slot(res, "panel_id")) <- as.character(sapply(levels(names_df[["Var1"]]), 
                                                       function(single_name) {
                                                         sub_data <- names_df[names_df[["Var1"]] == single_name, ]
                                                         sub_data[which.max(sub_data[["Freq"]]), "Var2"]
                                                       }))
  res
}