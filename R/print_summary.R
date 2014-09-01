# SUMMARY - droplet, array ------------------------------
#summary workhorse + plot, should be not called directly by user
print_summary <- function(k, col_dat, type, n, print, exp_names) {
  
  sums <- cbind(id = rep(exp_names, 2), calc_lambda(k, n))
  sums <- sums[order(sums[,1]), ]
  #rownames(sums) <- 1L:(2*col_dat)
  
  if (print) {
    k_print <- ifelse(col_dat < 5, k, 
                      paste0(paste0(k[1:4], collapse = (", ")), ", ..."))
    cat("\nNumber of positive partitions:", k_print, "\n")
    n_print <- ifelse(col_dat < 5, n, 
                      paste0(paste0(n[1:4], collapse = (", ")), ", ..."))
    cat("Total number of partitions:   ", n_print, "\n")
    cat("\n")
    print(head(sums, 20L), row.names = FALSE)
    if (col_dat > 20)
      cat(col_dat*2 - 20, "rows ommited.")
  }
  list(partitions = list(k = k, n = n), summary = sums)
  
}