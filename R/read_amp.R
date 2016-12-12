read_zipped_amps <- function(file) {
  files_names <- unzip(file, list = TRUE)[["Name"]]
  amp_files <- files_names[grep("Amplitude", files_names)]
  
  wells <- sapply(strsplit(amp_files, "_"), function(i) i[length(i) - 1])
  s_well <- sort(wells)
  
  raw_status <- do.call(rbind, lapply(amp_files[order(wells)], function(single_file) {
    n_clust <- tabulate(read.table(unz(file, single_file), 
                                   sep = ",", dec = ".", 
                                   nrows = 25000, header = TRUE,
                                   colClasses = "numeric")[["Cluster"]])
    if(length(n_clust) != 4)
      n_clust <- c(n_clust, rep(0, 4 - length(n_clust)))
    n_clust
  }))
  
  dat_summ <- data.frame(x = substr(s_well, 0, 1),
                         y = sub("[A-Z]", "", s_well),
                         channel = c(rep(1, length(wells)), rep(2, length(wells))),
                         positives = c(rowSums(raw_status[, 2L:3]), rowSums(raw_status[, 3L:4])),
                         negatives = c(rowSums(raw_status[, c(1, 4)]), rowSums(raw_status[, 1L:2]))) 
  
  dat_summ[["k"]] <- dat_summ[["positives"]]
  dat_summ[["n"]] <- dat_summ[["positives"]] + dat_summ[["negatives"]]
  dat_summ[, c("x", "y", "channel", "k", "n")]
}

amp2dpcr <- function(x) {
  create_dpcr(data = matrix(x[["k"]], nrow = 1), n = x[["n"]], 
              exper = 1L:nrow(x), replicate = rep(1, nrow(x)), type = "tnp",
              assay = paste0("ch", x[["channel"]]), adpcr = TRUE, 
              col_names = as.character(x[["x"]]), row_names = as.character(x[["y"]]),
              panel_id = NULL, threshold = 1)
}

