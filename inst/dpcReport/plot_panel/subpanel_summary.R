new_dat <- input_dat()
roi <- extract_dpcr(new_dat, input[["array_choice"]])

summs <- summary(roi, print = FALSE)[["summary"]][-c(9L:11)]
summs <- cbind(region = rep("Whole array", nrow(summs)), summs)

if(!is.null(array_val[["selected"]])) {
  slot(roi, ".Data") <- slot(roi, ".Data")[array_val[["selected"]], , drop = FALSE]
  slot(roi, "n") <- sum(array_val[["selected"]])
  summs <- rbind(summs, cbind(region = rep("Selected region", nrow(summs)), 
                              summary(roi, print = FALSE)[["summary"]]))
}

colnames(summs) <- c("Region", "Experiment name", "Replicate ID", "Assay", "Method", "&lambda;", 
                     "&lambda; (lower CI)", "&lambda; (upper CI)", 
                     "Concentration", "Concentration (lower CI)", "Concentration (upper CI)", "k", "n")
summs
