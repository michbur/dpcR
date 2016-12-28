#' Convert adpcr object to array
#' 
#' Converts \code{\linkS4class{adpcr}} object into the list of array-like matrices.
#' 
#' @param input object of the \code{\linkS4class{adpcr}} class.
#' @param breaks if \code{TRUE}, the data is divided into intervals.
#' @return A named list of length equal to the number of arrays in the \code{input}. 
#' Each element is a single array in matrix-like form, where dimensions are set 
#' exactly as in case of the real plate. Names of the list corresponds to the names 
#' of assays (\code{"tnp"} data) or runs (any other type of \code{\linkS4class{adpcr}} 
#' data).
#' 
#' The matrices contain values from array, either integers (when \code{use_break} is
#' \code{FALSE}) or characters (when \code{use_break} is \code{TRUE}).
#' @author Michal Burdukiewicz.
#' @export
#' @keywords manip
#' @examples 
#' #generate data
#' ttest <- sim_adpcr(m = 400, n = 765, times = 20, pos_sums = FALSE, 
#'                    n_panels = 3)
#' #convert object into three arrays
#' arrays <- adpcr2panel(ttest)
#' length(arrays)
#' #print an array
#' arrays[[1]]


adpcr2panel <- function(input, breaks = FALSE) {
  if (class(input) == "adpcr") {
    if (!(slot(input, "type") %in% c("nm", "np", "tnp", "fluo", "ct")))
      stop("Input must contain data of type 'nm', 'np', 'tnp', 'fluo' or 'ct'.") 
  } else {
    stop("Input must have the 'adpcr' class")
  }
  
  nx_a <- length(slot(input, "col_names"))
  ny_a <- length(slot(input, "row_names"))
  
  #in case of tnp, we analyze all experiments (all columns)
  #in the all other case, we analyze only a single value of n
  #assumption - number of experiments in each panel is the same
  len_n <- ifelse(slot(input, "type") == "tnp", table(slot(input, "panel_id"))[1], slot(input, "n"))
  
  # if (len_n != nx_a * ny_a)
  #   stop(paste0("Input length (", len_n, ") differs from the array size (", 
  #               nx_a * ny_a, ")."))
  
  #apply in case input contains more than 1 array
  #here list of?
  
  array_data <- lapply(levels(slot(input, "panel_id")), function(single_level) {
    #data for a single assay
    #browser()
    single_panel <- extract_run(input, which(slot(input, "panel_id") == single_level))
    # Use breaks points to split input 
    if (breaks)
      single_panel <- calc_breaks(single_panel)

    #browser()
    res <- matrix(NA, ncol = nx_a, nrow = ny_a,
                  dimnames = list(slot(input, "row_names"), slot(input, "col_names")))
    
    res[cbind(slot(input, "row_id"), slot(input, "col_id"))] <- single_panel
    res
  })
  
  if(slot(input, "type") == "tnp") {
    names(array_data) <- levels(slot(input, "panel_id"))
  } else {
    names(array_data) <- colnames(input)
  }
  
  array_data
}