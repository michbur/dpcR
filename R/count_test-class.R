#' Class \code{"count_test"} 
#' 
#' A class for results of \code{\link{test_counts}} function.
#' 
#' @name count_test
#' @aliases count_test-class count_test
#' @rdname count_test-class
#' @docType class
#' @section Slots: 
#' \describe{ 
#' \item{group_coef}{\code{"data.frame"} containing experiments, groups to which they
#' belong and calculated values of rate (lambda).}
#' \item{t_res}{\code{"matrix"} containing result of multiple comparisions t-test.} }
#' @author Michal Burdukiewicz.
#' @seealso Nothing yet.
#' @export
#' @keywords classes
setClass("count_test", representation(group_coef = "data.frame", 
                                      t_res = "matrix"))

#' @describeIn count_test Summary statistics of assigned groups.
#' @param object of class \code{count_test}.
#' @export
setMethod("summary", signature(object = "count_test"), function(object) {
  aggregate(. ~ group, slot(object, "group_coef"), mean)
})

#' @describeIn count_test Print both \code{group_coef} and \code{t_res}
#' @export
setMethod("show", "count_test", 
          function(object) {
            cat("Groups:\n")
            print(slot(object, "group_coef"))
            
            cat("\nResults of multiple comparison:\n")
            print(slot(object, "t_res"))
          })

#' @describeIn count_test
#' @param x object of class \code{count_test}.
#' @param aggregate logical, if \code{TRUE} experiments are aggregated according
#' to their group.
#' @param nice logical, if \code{TRUE} a more aesthetically pleasing (but harder to 
#' customize) version of the plot is created.
#' @details In case of aggregated plot, mean confidence intervals for groups are presented
#' as dashed lines.
#' @export
setMethod("plot", signature(x = "count_test"), function(x, aggregate = FALSE, 
                                                        nice = TRUE) {
  group_coef <- slot(x, "group_coef")
  if (aggregate) {
    summ <- aggregate(. ~ group, group_coef, mean)
    #possible groups
    pos_groups <- group_coef[["group"]]
    
    plot(c(0.55, nrow(summ) + 0.45), range(summ[, c("lambda.low", "lambda.up")]), 
         xlab = "Group", ylab = expression(lambda), xaxt = "n", cex = 0)
    axis(side = 1, labels = summ[["group"]], at = 1L:nrow(summ))
    
    colors <- if(nice) {
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
           col = adjustcolor("grey", alpha.f = 0.30))
      axis(1, tck = 1, col.ticks = "white", labels = FALSE)
      axis(2, tck = 1, col.ticks = "white", labels = FALSE)
      rainbow(nlevels(pos_groups))
    } else {
      rep("black", nlevels(pos_groups))
    }
    
    sapply(1L:length(levels(pos_groups)), function(i) {
      points(i + seq(-2, 2, length.out = sum(levels(pos_groups)[i] == pos_groups))/10,
             group_coef[group_coef[["group"]] == levels(pos_groups)[i], "lambda"],
             col = colors[i], pch = ifelse(nice, 17, 1))
    })
    
    abline(h = summ[1L:nrow(summ), "lambda.low"], lty = "dashed", col = colors)
    abline(h = summ[1L:nrow(summ), "lambda.up"], lty = "dashed", col = colors)
    
    
  } else {
    plot(1L:nrow(group_coef), group_coef[["lambda"]], 
         ylim = range(group_coef[, c("lambda.low", "lambda.up")]), xaxt = "n",
         xlab = "Experiment", ylab = expression(lambda))
    axis(1, at = 1L:nrow(group_coef), labels = rownames(group_coef))
    sapply(1L:nrow(group_coef), function(i) 
      lines(c(i, i), c(group_coef[i, "lambda.low"], group_coef[i, "lambda.up"]))
    )
    axis(3, at = 1L:nrow(group_coef), labels = as.character(group_coef[["group"]]),
         lwd.ticks = NA, padj = 1)
  }
})


