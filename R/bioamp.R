#' A function to analyze plot the raw data from a Bio-Rad droplet digital PCR
#' experiment
#'
#' \code{bioamp} is a function to plot and analyze the amplitude data of a
#' Bio-Rad droplet digital PCR experiment.
#'
#'
#' @param data object of class \code{what} containing the amplitude data.
#' @param robust Is the method used to calculate the location (mean or median)
#' and dispersion (standard deviation or median absolute deviation).
#' @param plot logical, if \code{TRUE}, the plot is printed.
#' @param amp_x is the first amplitude channel (x-axis).
#' @param amp_y is the second amplitude channel (y-axis).
#' @param cluster are the clusters of the plot. The number indicates the column
#' of a table, which contains the cluster information.
#' @param stat logical, if \code{TRUE}, the statistics of the droplet digital
#' PCR experiment are calculated.
#' @param xlab x-label of the plot.
#' @param ylab y-label of the plot.
#' @param \dots other arguments passed to the \code{plot} function (see
#' \code{plot.default} for details).
#' @author Stefan Roediger, Michal Burdukiewcz
#' @keywords Amplitude Bio-Rad
#' @examples
#'
#' par(mfrow = c(1, 2))
#' bioamp(data = pds_raw[["D01"]], main = "Well D01", pch = 19)
#' bioamp(data = pds_raw[["D02"]], main = "Well D02", pch = 19)
#' par(mfrow = c(1, 1))
#'
#' @export bioamp
bioamp <- function(data = data, amp_x = 1, amp_y = 2, cluster = 3,
                   robust = TRUE, plot = TRUE, stat = TRUE,
                   xlab = "Assay 1 Amplitude",
                   ylab = "Assay 2 Amplitude", ...) {
  # Determine number of clusters
  cluster_count <- unique(data[, 3])

  # Create a matirx for results of clusters
  res_ma <- matrix(NA,
    nrow = 6, ncol = length(cluster_count),
    dimnames = list(
      c(
        "Counts Ch. 1", "Counts Ch. 2",
        "Location Ch. 1", "Location Ch. 2",
        "Dispersion Ch. 1", "Dispersion Ch. 2"
      ),
      paste0("Cluster.", c(cluster_count))
    )
  )

  # Decide if a robust method is used for the calculation of the statistics
  if (robust) {
    loc_method <- median
    disp_method <- mad
  } else {
    loc_method <- mean
    disp_method <- sd
  }

  # Calculate the results of the clusters
  res_ma <- t(do.call(cbind, lapply(c(length, loc_method, disp_method), function(single_function) {
    sapply(c(amp_x, amp_y), function(single_channel) {
      sapply(sort(unique(data[, 3])), function(single_cluster) {
        single_function(data[data[cluster] == single_cluster, single_channel])
      })
    })
  })))

  colnames(res_ma) <- paste0("Cluster.", sort(unique(data[, 3])))
  rownames(res_ma) <- unlist(lapply(c("Counts Ch. ", "Location Ch. ", "Dispersion Ch. "), function(row_name) {
    paste0(row_name, 1L:2)
  }))

  # Plot the cluster
  if (plot) {
    plot(data[, amp_x], data[, amp_y],
      col = data[, cluster], xlab = xlab,
      ylab = ylab, ...
    )
    # abline(v = c(res_ma[3, 1], res_ma[3, 2]), h = c(res_ma[4, 1], res_ma[4, 2]),
    #       col = cluster_count)
    points(c(res_ma[3, ]), c(res_ma[4, ]),
      pch = 1, cex = 2, col = "grey", lwd = 4
    )
  }
  res_ma
}
