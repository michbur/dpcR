read_amps <- function(file) {
  files_names <- unzip(file, list = TRUE)[["Name"]]
  amp_files <- files_names[grep("Amplitude", files_names)]
  
  wells <- sapply(strsplit(amp_files, "_"), function(i) i[length(i) - 1])
  
  raw_status <- do.call(rbind, lapply(amp_files[order(wells)], function(single_file) {
    n_clust <- tabulate(read.csv(unz(file, single_file))[["Cluster"]])
    if(length(n_clust) != 4)
      n_clust <- c(n_clust, rep(0, 4 - length(n_clust)))
    n_clust
  }))
  
  dat_summ <- data.frame(well = sort(wells), 
                         channel = c(rep(1, length(wells)), rep(2, length(wells))),
                         positives = c(rowSums(raw_status[, 2L:3]), rowSums(raw_status[, 3L:4])),
                         negatives = c(rowSums(raw_status[, c(1, 4)]), rowSums(raw_status[, 1L:2]))) 
  
  dat_summ[["k"]] <- dat_summ[["positives"]]
  dat_summ[["n"]] <- dat_summ[["positives"]] + dat_summ[["negatives"]]
  dat_summ[, c("well", "channel", "k", "n")]
}