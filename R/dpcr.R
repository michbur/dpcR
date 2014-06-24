# SUMMARY - droplet, array ------------------------------
#summary workhorse + plot, should be not called directly by user

fl <- function(p)
  -log(1 - p)

#calculate values of lambda (average number of molecules per partition)
calc_lambda <- function(k, n) {
  #   if (length(k) > 1) {
  #     k_n <- get_k_n(k)
  #     k <- k_n[1]
  #     n <- k_n[2]
  #   }
  p <- k/n
  p_conf <- sqrt(p * (1 - p)/n) #dube
  u_lambda <- sqrt(p/(n * (1 - p))) #bhat
  l <- fl(p)
  lower <- c(fl(p - qnorm(0.975)*p_conf), l - u_lambda)
  upper <- c(fl(p + qnorm(0.975)*p_conf), l + u_lambda)
  res <- data.frame(method = c(rep("dube", length(p)), rep("bhat", length(p))), 
                    lambda = c(l,l), lambda.low = lower, lambda.up = upper, m = l*n,
                    m.low = lower*n, m.up = upper*n)
  res
}




print_summary <- function(k, col_dat, type, n, print) {
  
  id <- 1L:col_dat 
  sums <- cbind(id = rep(id, 2), calc_lambda(k, n))
  sums <- sums[order(sums[,1]), ]
  rownames(sums) <- 1L:(2*col_dat)
  
  if (print) {
    k_print <- ifelse(col_dat  < 5, k, 
                      paste0(paste0(k[1:4], collapse = (", ")), ", ..."))
    cat("\nNumber of positive partitions:", k_print, "\n")
    cat("Total number of partitions:   ", n, "\n")
    cat("\n")
    print.noquote(head(sums, 20L))
    if (col_dat > 20)
      cat(col_dat*2 - 20, "rows ommited.")
  }
  list(partitions = list(k = k, n = n), summary = sums)
  
}

#first four moments of distribution
moms <- function(lambda) {
  res <- c(lambda, lambda, lambda^(-0.5), 1/lambda)
  names(res) <- c("mean", "var", "skewness", "kurtosis")
  res
}

empir_moms <- function(input) {
  res <- c(mean(input), var(input), skewness(input), kurtosis(input))
  names(res) <- c("mean", "var", "skewness", "kurtosis")
  res
}

# GENERAL USE - droplet, array ------------------------------

#create adpr and ddpcr objects
create_dpcr <- function(data, n, threshold = NULL, breaks = NULL, type, adpcr = TRUE) {
  if (adpcr != TRUE && adpcr != FALSE)
    stop("'adpcr' argument must have TRUE or FALSE value.", call. = TRUE, domain = NA)
  
  if (type == "ct" && adpcr == FALSE)
    stop("'ct' type is not implemented for 'ddpcr' objects.", call. = TRUE, domain = NA)
  
  if (!(is.integer(n))) {
    warning("'n' converted to integer.")
    n <- as.integer(n)
  }
  
  if (is.vector(data))
    data <- as.matrix(data)
  if (!(is.matrix(data))) {
    warning("'data' converted to matrix.")
    data <- as.matrix(data)
  }
  
  if (adpcr) {
    create_adpcr(data, n, breaks, type)
  } else {
    create_ddpcr(data, n, threshold, type)
  }
}


extract_dpcr <- function(input, id) {
  if (!(class(input) %in% c("adpcr", "ddpcr")))
    stop("Input must have 'adpcr' or 'ddpcr' class", call. = TRUE, domain = NA)
  selected <- input[, id]
  
  #because when id is single negative value, usually the
  #result has more than one column
  if (length(id) == 1 && id > 0) {
    selected <- matrix(selected, ncol = 1)
    colnames(selected) <- colnames(input)[id]
  }
  result <- input
  slot(result, ".Data") <- selected
  result
}
