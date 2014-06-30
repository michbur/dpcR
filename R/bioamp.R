
bioamp <- function(data = data, amp_x = 1, amp_y = 2, cluster = 3, 
		   robust = TRUE, plot = TRUE, stat = TRUE, 
		   xlab = "Assay 1 Amplitude", 
		   ylab = "Assay 2 Amplitude", ...) {
  # Determine number of clusters
  cluster_count <- unique(data[, 3])
  
  # Create a matirx for results of clusters
  res_ma <- matrix(NA, nrow = 6, ncol = length(cluster_count), 
		   dimnames = list(c("Number 1", "Location 2", "Location 1", 
				     "Median 2", "Dispersion 1", "Dispersion 2"),
                               c(cluster_count)))

  # Decide if a robus method is used for the calculation of the statistics
  if (robust) {
    loc_method <- median
    disp_method <- mad
  } else {
      loc_method <- mean
      disp_method <- sd
  }
  
  # Calculate the results of the clusters
  if (stat) {
    for (i in cluster_count) {
      res_ma[1, i] <- length(data[data[cluster] == i, amp_x])
      res_ma[2, i] <- length(data[data[cluster] == i, amp_y])
      res_ma[3, i] <- loc_method(data[data[cluster] == i, amp_x])
      res_ma[4, i] <- loc_method(data[data[cluster] == i, amp_y])
      res_ma[5, i] <- disp_method(data[data[cluster] == i, amp_x])
      res_ma[6, i] <- disp_method(data[data[cluster] == i, amp_y])
    }
  }
  
  # Plot the cluster
  if (plot) {
  plot(data[, amp_x], data[, amp_y], col = data[, cluster], xlab = xlab, 
	ylab = ylab, ...)
  #abline(v = c(res_ma[3, 1], res_ma[3, 2]), h = c(res_ma[4, 1], res_ma[4, 2]), 
  #       col = cluster_count)
  points(c(res_ma[3, ]), c(res_ma[4, ]), 
	 pch = 1, cex = 2, col = "grey", lwd = 4)
  }
  res_ma
}
