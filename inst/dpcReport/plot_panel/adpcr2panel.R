#exp_run must be declared before
id_df <- data.frame(which(matrix(TRUE, nrow = ny_a, ncol = nx_a), arr.ind = TRUE))

id_df[["col"]] <- as.factor(id_df[["col"]])
id_df[["row"]] <- as.factor(ny_a - id_df[["row"]] + 1)

df <- cbind(id_df, value = as.factor(new_dat[, exp_run]), selected = rep(FALSE, ny_a * nx_a),
            exp_run = rep(exp_run, ny_a * nx_a))
