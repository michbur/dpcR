dat <- test_counts_groups_summary()
dat[["selected"]] <- rep(FALSE, nrow(dat))
dat[as.numeric(test_count_point[["selected"]]), "selected"] <- TRUE

p <- ggplot(dat, aes(x = run, y = lambda, shape = selected, colour = experiment,
                ymax = lambda.up, ymin = lambda.low, linetype = selected, label = group)) +
  geom_point(size = 4) + cool_theme +
  geom_text(aes(x = run, y = lambda.up), size = 7, vjust = -0.5, show_guide = FALSE) +
  ggtitle("Grouped experiments") +
  scale_x_discrete("Replicate id", labels = dat[["replicate"]] ) +
  scale_y_continuous(expression(lambda)) + 
  coord_cartesian(ylim = c(ifelse(min(dat[["lambda.low"]]) > 0,
                                  min(dat[["lambda.low"]]) * 0.95, 
                                  min(dat[["lambda.low"]]) * 1.05),
                           ifelse(max(dat[["lambda.up"]]) < 0,
                                  max(dat[["lambda.up"]]) * 0.95, 
                                  max(dat[["lambda.up"]]) * 1.15))) +
  scale_color_discrete("Experiment name") +
  scale_linetype_manual(guide = FALSE, values = c("solid", "dashed")) + 
  scale_shape_manual(guide = FALSE, values = c(15, 18)) + 
  geom_errorbar(size = 1.2, width = nlevels(dat[["run"]])/80)