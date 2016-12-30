new_dat <- input_dat()
roi <- if(slot(new_dat, "type") == "tnp") {
  extract_dpcr(new_dat, id_assay = input[["array_choice"]])
} else {
  extract_run(new_dat, input[["array_choice"]])
}

summs <- summary(roi, print = FALSE)[["summary"]][-c(9L:11)]
summs <- cbind(region = rep("Whole array", nrow(summs)), summs)

if(!is.null(array_val[["selected"]])) {
  if(slot(new_dat, "type") == "tnp") {
    summs <- data.frame(region = "Selected region", 
                  summary(extract_run(roi, which(array_val[["selected"]][!is.na(plot_panel_dat()[["value"]])])), 
                          print = FALSE)[["summary"]][-c(9L:11)])
  } else {
    slot(roi, ".Data") <- slot(roi, ".Data")[array_val[["selected"]], , drop = FALSE]
    slot(roi, "n") <- sum(array_val[["selected"]])
    summs <- rbind(summs, cbind(region = rep("Selected region", nrow(summs)), 
                                summary(roi, print = FALSE)[["summary"]][-c(9L:11)]))
  }
}

colnames(summs) <- c("Region", "Experiment name", "Replicate ID", "Assay", "Method", "&lambda;", 
                     "&lambda; (lower CI)", "&lambda; (upper CI)", 
                     "Concentration", "Concentration (lower CI)", "Concentration (upper CI)", "k", "n")
summs
