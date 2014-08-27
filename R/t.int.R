# Test if x is a positive integer value
# warn defines the warning level
t.int <- function (x, warn = 0) {
  options(warn = warn)
  if (x != abs(round(x)) || x <= 0) {
    warning(paste(x, " was not a positive integer (e.g., 1, 11, 23).
                  Automatically tried to convert ", x, " to integer"))
  }
  abs(as.integer(x))
}