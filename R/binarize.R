# TODO extend this to adpcr

#' Binarize digital PCR data
#' 
#' Transforms multinomial (number of molecules per partition) or continuous (fluorescence)
#' digital PCR data to binary (positive/negative partition) format.
#' 
#' @aliases binarize
#' @param input object of the class \code{\linkS4class{adpcr}} or
#' \code{\linkS4class{dpcr}} with one of following types:\code{"ct"}, \code{"fluo"} or
#' \code{"nm"}.
#' @return object of the class \code{\linkS4class{adpcr}} or
#' \code{\linkS4class{dpcr}} (depending on \code{input}) with type \code{"np"}.
#' @author Michal Burdukiewicz.
#' @keywords manip
#' @export
#' @examples
#' 
#' #adpcr object
#' rand_array <- sim_adpcr(200, 300, 100, pos_sums = FALSE, n_panels = 1)
#' binarize(rand_array)
#' 
#' #dpcr object
#' rand_droplets <- sim_dpcr(200, 300, 100, pos_sums = FALSE, n_exp = 1)
#' binarize(rand_droplets)
binarize <- function(input) {
  if (class(input) %in% c("adpcr", "dpcr")) {
    if(slot(input, "type") %in% c("tp", "tnp"))
      stop("Cannot binarize already binary data")
    
    positive_threshold <- slot(input, "threshold")
  } else {
    stop("Input must have 'adpcr' or 'dpcr' class.")
  }
  
  bin_data <- slot(input, ".Data") >= positive_threshold
  storage.mode(bin_data) <- "integer"
  slot(input, ".Data") <-  bin_data
  slot(input, "type") <- "np"
  slot(input, "threshold") <- 1
  input
}