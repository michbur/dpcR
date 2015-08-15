#exp_run must be declared before
id_df <- data.frame(which(matrix(TRUE, nrow = length(slot(new_dat, "row_names")), 
                                 ncol = length(slot(new_dat, "col_names"))), arr.ind = TRUE))

id_df[["col"]] <- as.factor(id_df[["col"]])
levels(id_df[["col"]]) <- slot(new_dat, "col_names")
id_df[["row"]] <- as.factor(ny_a() - id_df[["row"]] + 1)
levels(id_df[["row"]]) <- slot(new_dat, "row_names")



df <- if (slot(new_dat, "type") == "tnp") {
  cbind(id_df, value = ncol(slot(new_dat, ".Data")), 
        selected = rep(FALSE, nlevels(id_df[["row"]]) * nlevels(id_df[["col"]])),
        exp_run = nlevels(id_df[["row"]]) * nlevels(id_df[["col"]]))
} else {
  cbind(id_df, value = as.factor(new_dat[, exp_run]), 
        selected = rep(FALSE, nlevels(id_df[["row"]]) * nlevels(id_df[["col"]])),
        exp_run = nlevels(id_df[["row"]]) * nlevels(id_df[["col"]]))
}


