# Function for all cases when we need breaks calculated
calc_breaks <- function(vals, breaks = "Sturges", threshold = NULL) {
  if(length(unique(vals)) > 5) {
    br <- hist(vals, breaks = breaks, plot = FALSE)[["breaks"]]
    cut(vals, breaks = br, include.lowest = TRUE)
  } else {
    factor(vals)
  }
}