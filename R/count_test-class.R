#' Class \code{"count_test"} 
#' 
#' A class 
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
#' @keywords classes

#setOldClass(c("data.frame"))
setClass("count_test", representation(group_coef = "data.frame", 
                                      t_res = "matrix"))

#' @describeIn count_test Summary statistics of assigned groups.
setMethod("summary", signature(object = "count_test"), function(object) {
  aggregate(. ~ group, slot(object, "group_coef"), mean)
})

#' @describeIn count_test Print both \code{group_coef} and \code{t_res}
setMethod("show", signature(object = "count_test"), function(object) {
  cat("Groups:\n")
  print(slot(object, "group_coef"))
  
  cat("\nResults of multiple comparison:\n")
  print(slot(object, "t_res"))
})

#' @describeIn count_test Plots groups
#' @param x object of class \code{count_test}.
#' @param y ignored.
#' @param ... ignored.
setMethod("plot", signature(x = "count_test"), function(x, ...) {
  group_coef <- slot(x, "group_coef")
  plot(1L:nrow(group_coef), group_coef[["lambda"]], 
       ylim = range(group_coef[, c("lambda.low", "lambda.up")]), xaxt = "n",
       xlab = "Experiment", ylab = expression(lambda))
  axis(1, at = 1L:nrow(group_coef), labels = rownames(group_coef))
  axis(3, at = 1L:nrow(group_coef), labels = as.character(group_coef[["group"]]),
       lwd.ticks = NA)
  sapply(1L:nrow(group_coef), function(i) 
    lines(c(i, i), c(group_coef[i, "lambda.low"], group_coef[i, "lambda.up"]))
  )
})


