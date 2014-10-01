# Example of an artificial chamber dPCR experiment using the test data set from
# qpcR. The function limit_cq is used to calculate the Cy0 value and converts 
# all values between a defined range to 1 and the remaining to 0.


#' Limit Cy0 values
#' 
#' The function \code{limit_cq} calculates the Cq values of a qPCR experiment
#' within a defined range of cycles. The function can be used to extract Cq
#' values of a chamber based qPCR for conversion into a dPCR experiment. All Cq
#' values are obtained by Second Derivative Maximum or by Cy0 method (Guescini
#' et al. (2008)).
#' 
#' The \code{Cq_range} for this function an be defined be the user. The default
#' is to take all amplification curves into consideration. However, under
#' certain circumstances it is recommended to define a range. For example if
#' amplifications are positive in early cycle numbers (less than 10).
#' 
#' Approximated second derivative is influenced both by how often interpolation
#' takes place in each data interval and by the smoothing method used. The user
#' is encouraged to seek optimal parameters for his data himself. See
#' \code{\link[chipPCR]{inder}} for details.
#' 
#' The calculation of the Cy0 value (equivalent of Cq) is based on a
#' five-parameter function. From experience this functions leads to good
#' fitting and avoids overfitting of critical data sets. Regardless, the user
#' is recommended to test for the optimal fitting function himself (see
#' \code{\link[qpcR]{mselect}} for details).
#' 
#' @param data a dataframe containing the qPCR data.
#' @param cyc the column containing the cycle data. Defaults to first column.
#' @param fluo the column(s) (runs) to be analyzed. If NULL, all runs will be
#' considered. Use fluo = 2 to choose the second column for example.
#' @param Cq_range is a user defined range of cycles to be used for the
#' determination of the Cq values.
#' @param model is the model to be used for the analysis for all runs. Defaults
#' to 'l5' (see \code{\link[qpcR]{pcrfit}}).
#' @param SDM if \code{TRUE}, Cq is approximated by the second derivative
#' method.  If \code{FALSE}, Cy0 method is used instead.
#' @return A data frame with two columns and number of rows equal to the number
#' of runs analyzed. The column \code{Cy0} contains calculated Cy0 values. The
#' column \code{in.range} contains adequate logical constant if given Cy0 value
#' is in user-defined \code{Cq_range}.
#' @author Michal Burdukiewicz, Stefan Roediger.
#' @seealso SDM method: \code{\link[chipPCR]{inder}},
#' \code{\link[chipPCR]{summary.der}}.
#' 
#' Cy0 method: \code{\link[qpcR]{mselect}}, \code{\link[qpcR]{efficiency}}.
#' @references Guescini M, Sisti D, Rocchi MB, Stocchi L & Stocchi V (2008)
#' \emph{A new real-time PCR method to overcome significant quantitative
#' inaccuracy due to slight amplification inhibition}. BMC Bioinformatics, 9:
#' 326.
#' 
#' Ruijter JM, Pfaffl MW, Zhao S, et al. (2013) \emph{Evaluation of qPCR curve
#' analysis methods for reliable biomarker discovery: bias, resolution,
#' precision, and implications}. Methods, San Diego Calif 59:32--46.
#' @keywords Cy0 qPCR dPCR
#' @examples
#' 
#' library(qpcR)
#' test <- cbind(reps[1L:45, ], reps2[1L:45, 2L:ncol(reps2)], reps3[1L:45, 
#' 	      2L:ncol(reps3)])
#' 
#' # results.dPCR contains a column with the Cy0 values and a column with 
#' # converted values.
#' Cq.range <- c(20, 30)
#' ranged <- limit_cq(data = test, cyc = 1, fluo = NULL,
#'                            Cq_range = Cq.range, model = l5)
#' # Same as above, but without Cq.range
#' no_range <- limit_cq(data = test, cyc = 1, fluo = NULL, model = l5)
#' 
#' 
#' @export limit_cq
limit_cq <- function(data, cyc = 1, fluo = NULL,
                     Cq_range = c(1, max(data[cyc])), model = l5, SDM = TRUE) {
  if (Cq_range[1] > Cq_range[2]) {
    warning("First value of Cq_range is greater than second. Cq_range reversed.")
    Cq_range <- rev(Cq_range)
  }
  
  if (is.null(fluo))
    fluo <- (1L:ncol(data))[-cyc]
  
  pb <- txtProgressBar(min = 1, max = length(fluo), initial = 0, style = 3)
  
  if (SDM) {
    Cy0 <- vapply(fluo, function(fluo_col) {
      setTxtProgressBar(pb, fluo_col)
      summary(inder(x = data[, cyc], y = data[, fluo_col]), print = FALSE)["SDM"]
    }, 0)
  } else {
    Cy0 <- vapply(fluo, function(fluo_col) {
      setTxtProgressBar(pb, fluo_col)
      efficiency(pcrfit(data = data, cyc = cyc, fluo = fluo_col,
                        model = model), type = "Cy0", plot = FALSE)[["Cy0"]]
    }, 0)
  }
  
  Cy0.res <- vapply(Cy0, function(Cy0_i)
    Cq_range[1] <= Cy0_i & Cy0_i <= Cq_range[2], TRUE)
  
  data.frame(Cy0 = Cy0, in.range = Cy0.res)
}


