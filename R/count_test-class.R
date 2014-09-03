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
#' \item{test}{\code{"matrix"} containing result of t-test.} }
#' @author Michal Burdukiewicz.
#' @seealso Nothing yet.
#' @keywords classes

#setOldClass(c("data.frame"))
setClass("count_test", representation(group_coef = "data.frame", 
                                      test = "matrix"))

setMethod("summary", signature(object = "count_test"), function(object) {
  aggregate(. ~ group, slot(object, "group_coef"), mean)
})

setMethod("show", signature(object = "count_test"), function(object) {
  print(slot(object, "group_coef"))
})

# setGeneric("show")
# 
# setMethod("show",
#           c(object = "count_test"),
#           function(object) {
#             print(slot(object, "group_coef"))
#           }
# )
