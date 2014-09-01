#' Convert numeric to integer 
#' 
#' Converts \code{numeric} values to positive \code{integer}s with a warning. 
#' 
#' @details num2int uses \code{\link[base]{as.integer}} functionality.
#' @keywords manip
#' @export
#' @examples
#' suppressWarning(num2int(pi) == 3L)

# Test if x is a positive integer value
# warn defines the warning level
num2int <- function (x) {
  if (x != abs(round(x)) || x <= 0) {
    warning(paste(x, " was not a positive integer (e.g., 1, 11, 23).
                  Automatically tried to convert ", x, " to integer"))
  }
  abs(as.integer(x))
}