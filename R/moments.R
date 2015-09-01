#' Calculate Moments of Poisson Distribution
#' 
#' Computes moments of a Poisson distribution. The calculations are based on 
#' values of positive and total partitions or the theoretical lambda value.
#' 
#' 
#' @name moments-methods
#' @aliases moments-methods moments,adpcr-method moments,ddpcr-method
#' moments,numeric-method moments
#' @docType methods
#' @param input a single numeric object (lambda) or a two element vector (first
#' element is treated as the number of positive partitions and the second as
#' the number of total partitions) or an object of class
#' \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}.
#' @return A named vector of four elements (mean, variance, skewness and
#' kurtosis).
#' 
#' In case of \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}} object
#' containing total number of positive molecules, a n-by-4 matrix, where n is
#' the number of experiments.
#' 
#' In case of \code{\linkS4class{adpcr}} or \code{\linkS4class{ddpcr}}
#' containing number of molecules per partition, a n*2-by-4 matrix, where n is
#' the number of experiments. In this case empirical moments are calculated
#' directly from a distribution of the data. Theoretical moments from a Poisson
#' distribution with \eqn{\lambda}{lambda} parameter taken from the data.
#' @note Four first moments of a Poisson distribution.
#' 
#' Mean : \eqn{\lambda}{lambda}.
#' 
#' Variance: \eqn{\lambda}{lambda}.
#' 
#' Skewness: \eqn{\sqrt{\lambda}}{lambda^(-0.5)}.
#' 
#' Kurtosis: \eqn{\frac{1}{\lambda}}{lambda^(-1)}.
#' @author Michal Burdukiewicz.
#' @keywords moments mean variance skewness kurtosis
#' @export
#' @examples
#' 
#' # moments for lambda = 2
#' moments(2)
#' 
#' # moments for 100 positive partitions of 765 total partitions
#' moments(c(100, 765))
#' 
#' # calculate moments for an array digital PCR, total number of positive partitions 
#' ddpcr1 <- sim_ddpcr(m = 10, n = 40, times = 50, pos_sums = TRUE, n_exp = 5)
#' moments(ddpcr1)
#' 
#' # calculate moments for an array digital PCR, detailed number of molecules in each partition 
#' ddpcr2 <- sim_ddpcr(m = 10, n = 40, times = 50, pos_sums = FALSE, n_exp = 5)
#' moments(ddpcr2)
#' 
moments <- function (input) {
  stop("Wrong class of 'input'")
}

# setMethod("moments", signature(input = "numeric"), function(input) {
#   if (length(input) == 1) {
#     #input contains only number of positive partitions and total number of partitions
#     moms(input)
#   }
#   if (length(input) == 2) {
#     #input contains only number of positive partitions and total number of partitions
#     moms(fl(input[1]/input[2]))
#   } else {
#     res <- cbind(moms(fl(sum(input > 0)/length(input))), empir_moms(input))
#     colnames(res) <- c("Theoretical", "Empirical")
#     res
#   }  
# })


setMethod("moments", signature(input = "dpcr"), function(input) {
  browser()
  
  summ <- summary(input, print = FALSE)[["summary"]]
  kn <- summ[summ[["method"]] == "bhat", c("k", "n")]
  
  res <- rbind(do.call(cbind, lapply(1L:nrow(kn), function(single_row)
    moms(kn[single_row, "k"], kn[single_row, "n"]))),
    do.call(cbind, lapply(1L:nrow(kn), function(single_row)
      empir_moms(kn[single_row, "k"], kn[single_row, "n"]))))
})




#first four moments of distribution
#kn_df - data frame, row is run, first column is k, second is n
moms <- function(k, n) {
  lambda <- fl(k/n)
  res <- c(lambda, lambda, lambda^(-0.5), 1/lambda)
  names(res) <- c("mean", "var", "skewness", "kurtosis")
  res
}

#sample moments
empir_moms <- function(k, n) {
  exp_kn <- c(rep(1, k), rep(0, n - k))
  #res <- c(k/n, k(n - k)*n^-2, skewness(input), kurtosis(input))
  res <- c(mean(exp_kn), var(exp_kn), skewness(exp_kn), kurtosis(exp_kn))
  names(res) <- c("mean", "var", "skewness", "kurtosis")
  res
}