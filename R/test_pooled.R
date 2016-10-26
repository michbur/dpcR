#' Compare pooled digital PCR 
#' 
#' Estimates mean number of template molecules per partition and concentration of sample 
#' from pooled replicates of experiments.
#' 
#' @aliases test_pooled
#' @param input object of class \code{\linkS4class{adpcr}} or \code{\linkS4class{dpcr}}.
#' @param conf.level confidence level of the intervals and groups.
#' @export
#' @note This function was implemented using the code in supplemental materials in 
#' Dorazio, 2015 (see References).
#' @return data frame with the number of rows equal to the number of experiments 
#' (not runs). The unit of concentration is the number template molecules per 
#' nanoliter (nL).
#' @author Robert M. Dorazio, Margaret E. Hunter.
#' @examples 
#' test_pooled(six_panels)
#' @references Dorazio RM, Hunter ME, \emph{Statistical Models for the Analysis 
#' and Design of Digital Polymerase Chain Reaction (dPCR) Experiments}. 
#' Analytical Chemistry 2015. 87(21): p.10886-10893

test_pooled <- function(input, conf.level = 0.05) {
  # functionality and code below are taken from: 
  # Dorazio, R. M.; Hunter, M. E. Anal. Chem. 2015, 87 (21), 10886-10893.
  comp_data <- dpcr2df(input)

  fit <- glm(cbind(k, n) ~ experiment - 1, 
             data = comp_data,
             family = binomial(link = "cloglog"), 
             offset = log(comp_data[["v"]]))
  
  beta.mle <- fit[["coefficients"]]
  beta.vcv <- vcov(fit)
  zcrit <- qnorm(1 - conf.level/2)
  
  Xvec = matrix(rep(1, nlevels(comp_data[["experiment"]])), ncol=1)
  loglambda.var = beta.vcv %*% Xvec
  
  data.frame(sample_name = comp_data[["experiment"]],
             c = unname(exp(beta.mle)), 
             c.low = unname(exp(beta.mle - zcrit * sqrt(loglambda.var))),
             c.up = unname(exp(beta.mle + zcrit * sqrt(loglambda.var))), 
             GOF = 1 - pchisq(fit[["deviance"]], df = nrow(comp_data) - length(beta.mle)))
}
