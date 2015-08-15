#exp_run must be declared before
id_df <- data.frame(which(matrix(TRUE, nrow = length(slot(input_dat(), "row_names")), 
                                 ncol = length(slot(input_dat(), "col_names"))), arr.ind = TRUE))

id_df[["col"]] <- as.factor(id_df[["col"]])
levels(id_df[["col"]]) <- slot(input_dat(), "col_names")
id_df[["row"]] <- as.factor(ny_a - id_df[["row"]] + 1)
levels(id_df[["row"]]) <- slot(input_dat(), "row_names")

df <- cbind(id_df, value = as.factor(new_dat[, exp_run]), selected = rep(FALSE, ny_a * nx_a),
            exp_run = rep(exp_run, ny_a * nx_a))
