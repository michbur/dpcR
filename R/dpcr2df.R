#' Convert dpcr object to data frame
#'
#' Converts \code{\linkS4class{adpcr}} or \code{\linkS4class{dpcr}} object to
#' \code{\link{data.frame}} object.
#'
#' @param input \code{\linkS4class{adpcr}} or \code{\linkS4class{dpcr}} object.
#' @name dpcr2df-methods
#' @aliases dpcr2df-methods dpcr2df,adpcr-method dpcr2df,dpcr-method
#' @return data frame with 5 (if input was  \code{\linkS4class{dpcr}}) or
#' 8 columns (if input was  \code{\linkS4class{adpcr}}).
#' @seealso Inverse function: \code{\link{df2dpcr}}
#' @author Michal Burdukiewcz, Stefan Roediger
#' @keywords utilities
#' @export
#' @examples
#' dpcr2df(six_panels)
#'
dpcr2df <- function(input) {
  stop("Wrong class of 'input'")
}

setMethod("dpcr2df", signature(input = "dpcr"), function(input) {
  tab_dat <- summary(input, print = FALSE)
  data.frame(tab_dat[["summary"]][tab_dat[["summary"]][["method"]] == "dube", c("experiment", "replicate", "assay")],
    k = tab_dat[["partitions"]][["k"]],
    n = tab_dat[["partitions"]][["n"]],
    v = slot(input, "v"),
    uv = slot(input, "uv"),
    threshold = slot(input, "threshold")
  )
})

setMethod("dpcr2df", signature(input = "adpcr"), function(input) {
  # add here column and row name
  tab_dat <- summary(input, print = FALSE)
  data.frame(tab_dat[["summary"]][tab_dat[["summary"]][["method"]] == "dube", c("experiment", "replicate", "assay")],
    k = tab_dat[["partitions"]][["k"]],
    n = tab_dat[["partitions"]][["n"]],
    v = slot(input, "v"),
    uv = slot(input, "uv"),
    threshold = slot(input, "threshold"),
    panel_id = slot(input, "panel_id")
  )
})
