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
#' belong and calculated values of rate.}
#' \item{t_res}{\code{"matrix"} containing result of t-test.} }
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
#' @details In case of aggregated plot, mean confidence intervals for groups are presented
#' as dashed lines.
#' @export
setMethod("plot", signature(x = "count_test"), function(x, aggregate = FALSE) {
  browser()
  group_coef <- slot(x, "group_coef")
  if (aggregate) {
    summ <- aggregate(. ~ group, group_coef, mean)
    stripchart(lambda ~ group, data = group_coef, vertical = TRUE, xlab = "Group",
               ylab = expression(lambda), method = "jitter", 
               ylim = range(summ[, c("lambda.low", "lambda.up")]))
    #TO DO: add something for 1 group case.
    abline(h = summ[1L:nrow(summ), "lambda.low"], lty = "dashed")
    abline(h = summ[1L:nrow(summ), "lambda.up"], lty = "dashed")
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


