create_ddpcr <- function(data, n, threshold = NULL, type, col_names = 1L:ncol(data)) {
  result <- new("ddpcr")
  if (!is.null(col_names) && is.null(colnames(data)))
    colnames(data) <- col_names
  slot(result, ".Data") <- data
  slot(result, "n") <- n
  slot(result, "type") <- type
  slot(result, "threshold") <- threshold
  result
}