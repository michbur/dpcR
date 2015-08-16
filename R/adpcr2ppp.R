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
#' @param marks If \code{TRUE}, marks values for non-empty partitions.
#' @param plot If \code{TRUE}, array is plotted.
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
#' adpcr2ppp(many_panels)
#' 
#' # Convert all plates to ppp objects and get third plate
#' third_plate <- adpcr2ppp(many_panels)[[3]]
#' 
#' # Convert only third plate to ppp object
#' third_plate2 <- adpcr2ppp(extract_dpcr(many_panels, 3))
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
adpcr2ppp <- function(input, marks = TRUE, plot = FALSE) {
  if (class(input) != "adpcr")
    stop("Input must have 'adpcr' class", call. = TRUE, domain = NA)
  
  array_data <- slot(input, ".Data")
  nrow_array <- nrow(array_data)
  
  nx_a <- length(slot(input, "col_names"))
  ny_a <- length(slot(input, "row_names"))
  
  if (nrow_array != nx_a * ny_a)
    stop (paste0("Can not process with conversion since the input 
                 length (", length(input) ,
                 ") differs from the size of nx_a * ny_a (", nx_a * ny_a, ").
                 \n Change nx_a * ny_a to have the same number of elements."))  
  
  #apply in case input contains more than 1 array
  #here list of?
  if(slot(input, "type") == "tnp") {
    create_ppp(as.vector(array_data), nx_a, ny_a, plot, marks)
  } else {
    apply(array_data, 2, function(array_col) 
      create_ppp(array_col, nx_a, ny_a, plot, marks))
  }
}


create_ppp <- function(data_vector, nx_a, ny_a, plot, marks) {
  #strange syntax, because spatstat use different localizations
  #than dpcR.
  data_points <- which(matrix(data_vector, ncol = nx_a, nrow = ny_a) > 0,
                       arr.ind = TRUE)
  data_points[, "row"] <- ny_a - data_points[, "row"] + 1
  if(plot)
    plot(ppp(data_points[, 2], data_points[, 1], 
             c(1, nx_a), c(1, ny_a)))
  #check if marks are properly assigned
  if (marks) {
    ppp(data_points[, 2], data_points[, 1], 
        c(1, nx_a), c(1, ny_a), marks = data_vector[data_vector != 0])
  } else {
    ppp(data_points[, 2], data_points[, 1], 
        c(1, nx_a), c(1, ny_a))
  }
}