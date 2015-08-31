#' Check if Sample Comes from Poisson Distribution
#' 
#' Calculates coordinates of points on plot to represent a digital PCR array.
#' 
#' @param x A vector of counts, \code{\linkS4class{adpcr}}, \code{\linkS4class{ddpcr}} or
#' \code{\linkS4class{qdpcr}} object.
#' @return An object of \code{is_poisson_c} class.
#' @export
#' @author Michal Burdukiewicz, Piotr Sobczyk, Stefan Roediger.
#' @keywords htest
is_poisson <- function(k, n, times = 10000) {
  hat_lambda <- fl(k/n)
  
  chi <- chisq.test(table(factor(c(rep(1, k), rep(0, n - k)), levels = 0L:1)), 
                    p = dpois2dbinom(hat_lambda), rescale.p = TRUE,
                    simulate.p.value = TRUE)[["statistic"]]
  
  chi_perm <- replicate(times, {
    y <- as.numeric(rpois(n, hat_lambda) > 0)
    
    if(length(unique(y)) < 2) {
      Inf #sometimes lambda is low and y is all zeroes, in this case statistic should be Inf
    } else {
      chisq.test(table(factor(y, levels = 0L:1)), 
                 p = dpois2dbinom(fl(sum(y)/n)), rescale.p = TRUE, 
                 simulate.p.value = TRUE)[["statistic"]]
    }
  })
  mean(chi_perm > chi)
}

#calcuty late binomial density for poisson distrubution
dpois2dbinom <- function(lambda) {
  dens <- dpois(0:qpois(1 - 1e-6, lambda), lambda)
  #density of positive partitions
  dens[2L] <- sum(dens[2L:length(dens)])
  dens[1L:2]
} 