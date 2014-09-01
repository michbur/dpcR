#' Calculate Moments of Poisson Distribution
#' 
#' The function allows user to quickly calculate moments of a Poisson
#' distributions. The calculations are based on values of positive and total
#' partitions or the theoretical lambda value.
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

setMethod("moments", signature(input = "numeric"), function(input) {
  if (length(input) == 2) {
    #input contains only number of positive partitions and total number of partitions
    moms(fl(input[1]/input[2]))
  } else {
    res <- cbind(moms(fl(sum(input > 0)/length(input))), empir_moms(input))
    colnames(res) <- c("Theoretical", "Empirical")
    res
  }  
})


setMethod("moments", signature(input = "adpcr"), function(input) {
  data <- slot(input, ".Data")
  col_dat <-ncol(data)
  type <- slot(input, "type")
  n <- slot(input, "n")
  
  
  switch(type,
         tp = {
           k <- data
           vapply(k, function(x) moments(fl(x/n)), rep(0, 4))
         },
         nm = {
           n_cols <- ncol(data)
           res <- do.call(cbind, lapply(1L:n_cols, function(i) 
             moments(data[1L:n[i], i])))
           #need only two names, because they are repeating
           nms <- colnames(res)[1:2]
           rep_colnames <- unlist(lapply(colnames(data), function(i) rep(i, 2)))
           colnames(res) <- unlist(lapply(1L:(n_cols * 2), function(i) 
             paste0(nms[i %% 2 + 1], ".", rep_colnames[i])))
           res
         })  
})

setMethod("moments", signature(input = "ddpcr"), function(input) {
  data <- slot(input, ".Data")
  col_dat <- ncol(data)
  type <- slot(input, "type")
  n <- slot(input, "n")
  
  switch(type,
         tp = {
           k <- data
           vapply(k, function(x) moments(fl(x/n)), rep(0, 8))
         },
         nm = {
           n_cols <- ncol(data)
           res <- do.call(cbind, lapply(1L:n_cols, function(i) 
             moments(data[1L:n[i], i])))
           #need only two names, because they are repeating
           nms <- colnames(res)[1:2]
           rep_colnames <- unlist(lapply(colnames(data), function(i) rep(i, 2)))
           colnames(res) <- unlist(lapply(1L:(n_cols * 2), function(i) 
             paste0(nms[i %% 2 + 1], ".", rep_colnames[i])))
           res
         },
         fluo = {
           k <- apply(data, 2, function(x) get_k_n(x, slot(input, "threshold")))
           vapply(k, function(x) moments(fl(x/n)), rep(0, 4))
         })  
})


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