#' Convert adpcr to ppp
#' 
#' Quick conversion of \code{\linkS4class{adpcr}} object to the list of
#' \code{\link[spatstat]{ppp.object}}s.
#' 
#' Each plate is independently converted by \code{\link[spatstat]{ppp}}
#' function. \code{marks} attached to each point represent values contained by
#' the \code{\linkS4class{adpcr}} object.
#' 
#' @param input Object of the \code{\linkS4class{adpcr}} class containing data
#' from one or more panels.
#' @param nx_a Number of columns in a plate.
#' @param ny_a Number of rows in a plate.
#' @return A list containing objects with class
#' \code{\link[spatstat]{ppp.object}} with the length equal to the number of
#' plates (minimum 1).
#' @author Michal Burdukiewcz, Stefan Roediger.
#' @seealso \code{\link[spatstat]{ppp.object}}, \code{\link[spatstat]{ppp}}.
#' @keywords manip panel
#' @examples
#' 
#' many_panels <- sim_adpcr(m = 400, n = 765, times = 1000, pos_sums = FALSE, 
#'                    n_panels = 5)
#' 
#' # Convert all plates to ppp objects
#' adpcr2ppp(many_panels, nx_a = 45, ny_a = 17)
#' 
#' # Convert all plates to ppp objects and get third plate
#' third_plate <- adpcr2ppp(many_panels, nx_a = 45, ny_a = 17)[[3]]
#' 
#' # Convert only third plate to ppp object
#' third_plate2 <- adpcr2ppp(extract_dpcr(many_panels, 3), nx_a = 45, ny_a = 
#' 17)
#' 
#' # Check the class of a new object
#' class(third_plate2)
#' 
#' # It's a list with the length 1. The third plate is a first element on this 
#' list
#' class(third_plate2[[1]])
#' 
#' 
#' @export adpcr2ppp
adpcr2ppp <- function(input, nx_a, ny_a) {
  if (class(input) != "adpcr")
    stop("Input must have 'adpcr' class", call. = TRUE, domain = NA)
  
  array_data <- slot(input, ".Data")
  nrow_array <- nrow(array_data)
  
  if (nrow_array != nx_a * ny_a)
    stop (paste0("Can not process with conversion since the input 
                 legnth (", length(input) ,
                 ") differs from the size of nx_a * ny_a (", nx_a * ny_a, ").
                 \n Change nx_a * ny_a to have the same number of elements."))  
  
  #apply in case input contains more than 1 array
  apply(array_data, 2, function(array_col) { 
    data_points <- matrix(NA, nrow = nrow_array, ncol = 3)
    i = 1
    for (x in 1L:nx_a) {
      for (y in ny_a:1L) {
        data_points[i, ] <- c(x, y, array_col[i])
        i <- i + 1
      }
    }
    
    data_points <- data_points[data_points[, 3] > 0, ]
    data_ppp <- ppp(data_points[, 1], data_points[, 2], 
                    c(1, nx_a), c(1, ny_a), marks = data_points[, 3])
  })
}
