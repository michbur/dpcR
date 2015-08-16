#exp_run must be declared before

extr_dat <- extract_dpcr(new_dat, exp_run)
id_df <- plot_panel(extr_dat, plot = FALSE)[["ggplot_coords"]]

id_df[["x"]] <- factor(slot(new_dat, "col_names"))
id_df[["y"]] <- factor(slot(new_dat, "row_names"))

df <- if (slot(new_dat, "type") == "tnp") {
  cbind(id_df, value = ncol(slot(extr_dat, ".Data")), 
        selected = rep(FALSE, nlevels(id_df[["y"]]) * nlevels(id_df[["x"]])))
} else {
  cbind(id_df, value = as.factor(extr_dat), 
        selected = rep(FALSE, nlevels(id_df[["y"]]) * nlevels(id_df[["x"]])))
}


