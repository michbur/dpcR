#' Convert data.frame to dpcr object
#' 
#' Converts \code{\link{data.frame}} object to to \code{\linkS4class{adpcr}} or 
#' \code{\linkS4class{ddpcr}} object. The resulting object will have \code{"tnp"}
#' type.
#' 
#' @param df data frame with specified column names. See Details.
#' @details The data frame must have following columns with exactly specified names:
#' \describe{
#' \item{experiment}{names of experiments}
#' \item{replicate}{indices of replicates}
#' \item{assay}{names of assays}
#' \item{k}{number of positive partitions}
#' \item{n}{total number of partitions}
#' }
#' There are also one optional column:
#' \describe{
#' \item{panel_id}{indices of panels}
#' }
#' 
#' If the additional column is present, the resulting object has 
#' \code{\linkS4class{adpcr}} type.
#' @return An object of \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}} type, 
#' depends on the presence of additional column with panel indices (see Details). 
#' @seealso Flexibly create \code{\linkS4class{dpcr}} objects: \code{\link{create_dpcr}}
#' Inverse function: \code{\link{dpcr2df}}
#' @author Michal Burdukiewcz, Stefan Roediger
#' @keywords utilities
#' @export
#' @examples
#' dat <- data.frame(experiment = factor(rep(paste0("Experiment", 1L:2), 3)),
#'                   replicate = c(1, 1, 2, 2, 3, 3),
#'                   assay = "Assay1",
#'                   k = c(55, 121, 43, 150, 70, 131),
#'                   n = 765,
#'                   v = 1)
#' df2dpcr(dat)

df2dpcr <- function(df) {
  if((!all(colnames(df) %in% c("experiment", "replicate", "assay", "k", "n", "v")) && 
      ncol(df) == 6) |
     (!all(colnames(df) %in% c("experiment", "replicate", "assay", "k", "n", "v",
                               "panel_id")) && 
      ncol(df) == 7))
    stop("Wrong column names")
  
  create_dpcr(data = matrix(df[["k"]], nrow = 1),
              n = df[["n"]],
              exper = df[["experiment"]],
              replicate = df[["replicate"]],
              assay = df[["assay"]],
              row_names = df[["row_names"]],
              col_names = df[["col_names"]],
              panel_id = df[["panel_id"]],
              v = df[["v"]],
              adpcr = ncol(df) == 7,
              type = "tnp")
}