#' Convert adpcr object to array
#' 
#' \code{\linkS4class{adpcr}} objects contains all data from array-based digital
#' PCR experiments. This function converts them into matrices, where rows and 
#' columns represent rows and columns in a real array.
#' 
#' @param input object of the \code{\linkS4class{adpcr}} class.
#' @param use_breaks if \code{TRUE}, input is cutted into intervals using the 
#' \code{breaks} slot. If \code{FALSE}, input is converted to factor using
#' \code{\link[base]{as.factor}}. Ignored if data has \code{"np"} type (see 
#' possible types of \code{\linkS4class{adpcr}} objects).
#' @return A named list of length equal to the number of arrays in the \code{input}. 
#' Each element is a single array in matrix-like form, where dimensions are set 
#' exactly as in case of the real plate. Names of the list corresponds to the names 
#' of assays (\code{tnp} data) or runs (any other type of \code{\linkS4class{adpcr}} 
#' data).
#' 
#' The matrices contain values from array, either integers (when \code{use_break} is
#' \code{FALSE}) or characters (when \code{use_break} is \code{TRUE}).
#' @author Michal Burdukiewicz.
#' @seealso \code{\link{extract_dpcr}}.
#' @export
adpcr2panel <- function(input, use_breaks = FALSE) {
  if (class(input) == "adpcr") {
    if (!(slot(input, "type") %in% c("nm", "np", "ct", "tnp")))
      stop("Input must contain data of type 'nm', 'np', 'tnp' or 'ct'.") 
  } else {
    stop("Input must have the 'adpcr' class")
  }
  
  nx_a <- length(slot(input, "col_names"))
  ny_a <- length(slot(input, "row_names"))
  
  #in case of tnp, we analyze all experiments (all columns)
  #in the all other case, we analyze only a single value of n
  #assumption - number of experiments in each assay is the same
  len_n <- ifelse(slot(input, "type") == "tnp", table(slot(input, "assay"))[1], slot(input, "n"))
  
  if (len_n != nx_a * ny_a)
    stop(paste0("Input length (", len_n, ") differs from the array size (", 
                nx_a * ny_a, ")."))
  
  #apply in case input contains more than 1 array
  #here list of?
  if(slot(input, "type") == "tnp") {
    array_data <- lapply(levels(slot(input, "assay")), function(single_level) {
      #data for a single assay
      assay_data <- extract_dpcr(input, which(slot(input, "assay") == single_level))
      # Use breaks points to split input 
      if(use_breaks)
        assay_data <- as.character(cut(as.vector(assay_data), breaks = slot(input, "breaks"), 
                          include.lowest = TRUE, right = FALSE, dig.lab = 5))
      
      matrix(assay_data, ncol = nx_a, 
             dimnames = list(slot(input, "row_names"), slot(input, "col_names")))
    })
    names(array_data) <- levels(slot(input, "assay"))
  } else {
    array_data <- lapply(1L:ncol(input), function(single_run) {
      run_data <- input[, single_run]
      
      if (slot(input, "type") == "np")
        use_breaks = FALSE
      
      # Use breaks points to split input 
      if(use_breaks)
        run_data <- as.character(cut(as.vector(run_data), breaks = slot(input, "breaks"), 
                        include.lowest = TRUE, right = FALSE, dig.lab = 5))
      
      matrix(run_data, ncol = nx_a, dimnames = list(slot(input, "row_names"), 
                                                    slot(input, "col_names")))
      
    })
    names(array_data) <- colnames(input)
  }
  
  array_data
}