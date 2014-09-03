#' Counts test
#' 
#' The test for comparing two or more digital PCR experiments.
#' 
#' 
#' @aliases test_counts
#' @param input adpcr or dpcr object with with "nm" type.
#' @param ... additional arguments for \code{\link{glm}} function.
#' @details \code{test_counts} fits General Linear Model (using Poisson 
#' \code{\link[stats]{family}}) to the counts data from different digital PCR experiments.
#' @export
#' @return an object of class
#' @author Michal Burdukiewicz, Stefan Roediger
#' @examples
#' adpcr1 <- sim_adpcr(m = 10, n = 765, times = 1000, pos_sums = FALSE, n_panels = 3)
#' adpcr2 <- sim_adpcr(m = 10, n = 600, times = 1000, pos_sums = FALSE, n_panels = 3)
#' adpcr3 <- sim_adpcr(m = 60, n = 550, times = 1000, pos_sums = FALSE, n_panels = 3)
#' combo <- bind_dpcr(adpcr1, adpcr2, adpcr3)
#' res <- test_counts(combo)
#' summary(res)


test_counts <- function(input, ...) {
  
  #dpcr version of melt
  n_vector <- slot(input, "n")
  m_dpcr <- do.call(rbind, lapply(1L:length(n_vector), function(i) {
    vals <- input[1L:n_vector[i], i]
    data.frame(experiment = rep(colnames(input)[i], length(vals)), values = vals)
  }))
  
  #remove intercept
  fit <- glm(values ~ experiment + 0, data = m_dpcr, family = quasipoisson)
  multi_comp <- glht(fit, linfct = mcp(experiment = "Tukey"))
  
  coefs <- summary(fit)[["coefficients"]][, 1:2]
  lambdas <- exp(matrix(c(coefs[, 1], 
                          coefs[, 1] - coefs[, 2], 
                          coefs[, 1] + coefs[, 2]), ncol = 3))
  
  summ_mc <- summary(multi_comp)
  groups <- cld(multi_comp)[["mcletters"]][["LetterMatrix"]]
  groups_vector <- apply(groups, 1, which)
  group_coef <- data.frame(LETTERS[groups_vector], lambdas)
  colnames(group_coef) <- c("group", "lambda", "lambda.low", "lambda.up")
  rownames(group_coef) <- colnames(input)
  new("count_test", group_coef = group_coef, test = cbind(summ_mc[["test"]][["tstat"]], 
                                                     summ_mc[["test"]][["pvalues"]]))
}

#old version with comments
test_counts2 <- function(input) {
  #   args <- c(Filter(Negate(is.null), list(...)))
  #   if(length(args) > 1) {
  #     input <- bind_dpcr(...)
  #   } else {
  #     input <- args[[1]]
  #   }
  
  n_vector <- slot(input, "n")
  
  m_dpcr <- do.call(rbind, lapply(1L:length(n_vector), function(i) {
    vals <- input[1L:n_vector[i], i]
    data.frame(experiment = rep(colnames(input)[i], length(vals)), values = vals)
  }))
  
  #remove intercept
  fit <- glm(values ~ experiment + 0, data = m_dpcr, family = quasipoisson)
  multi_comp <- glht(fit, linfct = mcp(experiment = "Tukey"))
  #fit <- zeroinfl(values ~ experiment + 0, data = m_dpcr, dist = "poisson")
  #   fit <- hurdle(values ~ experiment + 0, data = m_dpcr, dist = "poisson")
  #   reps_vector <- rep(1, ncol(input))
  #   names(reps_vector) <- colnames(input)
  #   ctr_matrix <- contrMat(reps_vector, type = "Tukey")
  #   ctr_matrix0 <- ctr_matrix[,] 
  #   ctr_matrix0[,] <- 0 
  # 
  #   multi_comp <- glht(fit, linfct = cbind(ctr_matrix, ctr_matrix0))
  
  coefs <- summary(fit)[["coefficients"]][, 1:2]
  lambdas <- exp(matrix(c(coefs[, 1], 
                          coefs[, 1] - coefs[, 2], 
                          coefs[, 1] + coefs[, 2]), ncol = 3))
  
  groups <- cld(multi_comp)[["mcletters"]][["LetterMatrix"]]
  groups_vector <- apply(groups, 1, which)
  res <- data.frame(LETTERS[groups_vector], lambdas)
  colnames(res) <- c("group", "lambda", "lambda.low", "lambda.up")
  rownames(res) <- colnames(input)
  #
  res
}