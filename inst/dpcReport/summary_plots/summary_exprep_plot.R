summ <- summary_exprep_plot_dat()
dat <- cbind(summ, selected = rep(FALSE, nrow(summary_exprep_plot_dat())))
dat[as.numeric(summary_exprep_point[["selected"]]), "selected"] <- TRUE

p <- ggplot(dat, aes(x = exprep, y = lambda, shape = selected, colour = experiment,
                     ymax = lambda.up, ymin = lambda.low, linetype = selected)) +
  geom_point(size = 4) + cool_theme +
  ggtitle(expression(atop("Top line", atop(italic("2nd line"), "")))) +
  ggtitle(paste0("Experiment/replicate scatter chart\nCI method: ", cap1L(input[["CI_method"]]))) +
  scale_x_discrete("Replicate id", labels = dat[["replicate"]] ) +
  scale_y_continuous(expression(lambda)) + 
  scale_color_discrete("Experiment name") +
  scale_linetype_manual(guide = FALSE, values = c("solid", "dashed")) + 
  scale_shape_manual(guide = FALSE, values = c(15, 18)) + 
  geom_errorbar(size = 1.2, width = nlevels(dat[["exprep"]])/80)