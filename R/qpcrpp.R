#' Class \code{"qpcrpp"}
#' 
#' An object representing digital PCR reaction depicted as Poisson process.
#' 
#' 
#' @name qpcrpp-class
#' @aliases qpcrpp-class qpcrpp show.qpcrpp show,qpcrpp-method summary.qpcrpp
#' summary,qpcrpp-method
#' @docType class
#' @section Slots: \describe{ \item{list(".Data")}{\code{"matrix"} with three
#' columns containing: number of cycles, amplification curves and cumulative
#' sum of events.}\item{:}{\code{"matrix"} with three columns containing:
#' number of cycles, amplification curves and cumulative sum of events.}
#' \item{list("mu")}{\code{"numeric"} of the expected number of events in
#' defined interval.}\item{:}{\code{"numeric"} of the expected number of events
#' in defined interval.} \item{list("CT")}{\code{"numeric"} value of the
#' "average time" between the occurrence of a positive reaction and another
#' positive reaction.}\item{:}{\code{"numeric"} value of the "average time"
#' between the occurrence of a positive reaction and another positive reaction.}
#' \item{list("partitions")}{\code{"integer"} value equal to the number of
#' partitions.}\item{:}{\code{"integer"} value equal to the number of
#' partitions.} \item{list("events")}{\code{"integer"} value equal number of
#' events (positive partitions taken to further
#' analysis)}\item{:}{\code{"integer"} value equal number of events (positive
#' partitions taken to further analysis)} }
#' @author Stefan Roediger, Michal Burdukiewicz.
#' @seealso \code{\link{plot.qpcrpp}},
#' @keywords classes
NULL

setClass("qpcrpp", contains = "matrix", representation(.Data = "matrix", mu = "numeric", 
                                                       CO = "numeric",
                                                       CT = "numeric", 
                                                       partitions = "integer",
                                                       events = "integer"))




#' qPCR to Poisson Process
#' 
#' Selected platforms (e.g., Open Array) are real-time platforms. dPCR can be
#' described by Poisson statistics. The function \code{qpcr2pp} takes a step
#' further and interprets the dPCR as a Poisson process if it is analyzed as a
#' "time" based process.
#' 
#' The dPCR Technology breaks fundamentally with the previous concept of
#' nucleic acid quantification. dPCR can be seen as a next generation nucleic
#' acid quantification method based on PCR. The key difference between dPCR and
#' traditional PCR lies in the method of measuring (absolute) nucleic acids
#' amounts. This is possible after ``clonal DNA amplification'' in thousands of
#' small separated partitions (e.g., droplets, nano chambers).  Partitions with
#' no nucleic acid remain negative and the others turn positive. Selected
#' technologies (e.g., OpenArray(R) Real-Time PCR System) monitor amplification
#' reactions in the chambers in real-time. Cq values are calculated from the
#' amplification curves and converted into discrete events by means of positive
#' and negative partitions and the absolute quantification of nucleic acids is
#' done by Poisson statistics.
#' 
#' PCR data derived from a qPCR experiment can be seen as a series of events
#' over time. We define t_i as the time between the first (i - 1)^st and the
#' i^th event. Therefore, the time \eqn{S_n}{S_n} is the sum of all
#' \eqn{t_i}{t_i} from \eqn{i = 1}{i = 1} to \eqn{i = n}{i = n}. This is the
#' time to the n^th event. \eqn{S(t)}{S(t)} is the number of events in \eqn{[0,
#' t]}{[0, t]}. This can be seen as a Poisson process. The Poisson statistics
#' is the central theorem to random processes in digital PCR.
#' 
#' The function \code{qpcr2pp} is used to model random point events in time
#' units (PCR cycles), such as the increase of signal during a qPCR reaction in
#' a single compartment. A Poisson process can be used to model times at which
#' an event occurs in a "system". The \code{qpcr2pp} (quantitative Real-Time
#' PCR to Poisson process) function transforms the qPCR amplification curve
#' data to quantification points (Cq) which are visualized as Poisson process.
#' This functions helps to spot differences between replicate runs of digital
#' PCR experiments. In ideal scenarios the \code{qpcr2pp} plots are highly
#' similar.
#' 
#' This tool might help to spot differences between experiments (e.g.,
#' inhibition of amplification reactions, influence of the chip arrays). The
#' qPCR is unique because the amplification of conventional qPCRs takes place
#' in discrete steps (cycles: 1, 2 ... 45), but the specific Cq values are
#' calculated with continuous outcomes (Cq: 18.2, 25.7, ...). Other
#' amplification methods such as isothermal amplifications are time based and
#' thus better suited for Poisson process.
#' 
#' @param cycles the column containing the cycle data. Defaults to first
#' column.
#' @param process the column containing fluorescence values.
#' @param data a dataframe containing the qPCR data.
#' @param NuEvents NuEvents is "number of expected events" within a time frame
#' (interval).
#' @param delta is the difference "time (cycles) points" e.g., Cycle 18 and 25.
#' @return An object of \code{\linkS4class{qpcrpp}} class.
#' @author Stefan Roediger, Michal Burdukiewicz.
#' @keywords Poisson Process qPCR
#' @export qpcr2pp
#' @examples
#' 
#' library(qpcR)
#' test <- cbind(reps[1L:45, ], reps2[1L:45, 2L:ncol(reps2)], 
#' 	      reps3[1L:45, 2L:ncol(reps3)])
#' 
#' # before interpolation qPCR experiment must be converted into dPCR
#' Cq.range <- c(20, 30)
#' ranged <- limit_cq(data = test, cyc = 1, fluo = NULL,
#'                      Cq_range = Cq.range, model = l5)
#'                      
#' qpcr2pp(ranged[,1], ranged[,2], delta = 5)


qpcr2pp <- function(cycles, process, data = NULL, NuEvents = 1, delta = 1) {
  
  if (!is.null(data)) {
    res_qPCR <- data
  } else {
    # Rearrange the input
    # NOTE: should use an object from analyze_qpcR if it turns out to be a useful
    # function
    res_qPCR <- data.frame(cycles, process)
  }
  
  res_qPCR <- res_qPCR[order(res_qPCR[[1]]), ]
  res_qPCR <- cbind(res_qPCR, cumsum(res_qPCR[, 2]))
  colnames(res_qPCR) <- c("Cycles", "result", "lambda") 
  
  # do not know if this is correct, WIP
  # cycle.time should give the "average time" between the occurrence of a 
  # positive reaction and another positive reaction
  dens.tmp <- dpcr_density(sum(res_qPCR[, 2]), nrow(res_qPCR), plot = FALSE)
  cycle.time <- exp(1/dens.tmp[["k"]])
  # Determine probaility how often a events occur according to Poisson process 
  # with a certia rate per time frame (interval).
  # NuEvents is "number of expected events" within a time frame (interval)
  # dens.tmp$k gives the rate of the process according to dpcr_density()
  # delta is the difference "time (cycles) points" e.g., Cycle 18 and 25
  # mu is the expected number of events in defined interval (but this is somewhat
  # stupid since the intervals are discrete ... and so on)
  # cyc.occ gives the occurrence in an interval
  mu <- dens.tmp[["k"]] * delta
  fact.NuEvents <- factorial(NuEvents)
  if (fact.NuEvents != "Inf") {
    cyc.occ <- (exp(-mu) * mu^NuEvents)/fact.NuEvents
  } else cyc.occ <- "too large"
  # END WIP
  
  new("qpcrpp", .Data = data.matrix(res_qPCR), mu = mu, CT = cycle.time, CO = cyc.occ, 
      partitions = dens.tmp[["n"]], events = dens.tmp[["k"]])
}


#' Plot \code{qpcrpp} objects
#' 
#' An analytical plot describing relationship between the cycle number and the
#' current value of Poisson mean. The plot can be used for quality control of
#' process.
#' 
#' The \code{rug} parameter allows user to add density of the number of events
#' to the plot.
#' 
#' @name plot.qpcrpp
#' @aliases plot.qpcrpp plot,qpcrpp-method
#' @param x is a \code{\linkS4class{qpcrpp}} object.
#' @param mincyc is the first cycle to start the plot from.
#' @param maxcyc the the last cycle for the plot.
#' @param rug Adds a rug representation of the data to the plot.
#' @param digits how many significant digits are to be used in plot.
#' @author Stefan Roediger, Michal Burdukiewicz
#' @seealso \code{\linkS4class{qpcrpp}}
#' @keywords hplot
#' @examples
#' 
#' library(qpcR)
#' test <- cbind(reps[1L:45, ], reps2[1L:45, 2L:ncol(reps2)], reps3[1L:45, 
#'         2L:ncol(reps3)])
#' ranged <- limit_cq(data = test, cyc = 1, fluo = NULL, model = l5)
#'         
#' plot(qpcr2pp(ranged[, 1], ranged[, 2], delta = 5), rug = TRUE)
#' 
NULL

setMethod("plot", signature(x = "qpcrpp"), function(x, mincyc = 1, maxcyc = 45, rug = TRUE,
                                                    digits = getOption("digits") - 3) {
  # Plot the calculated qPCR data as Poisson processes
  res_qPCR <- slot(x, ".Data")
  plot(res_qPCR[, 1], res_qPCR[,3], xlim = c(mincyc, maxcyc), 
       ylim = c(0, nrow(res_qPCR)), xlab = "Cycle", 
       ylab = expression(paste(lambda,
                               " (cycles)")), type = "S", lwd = 1.5)
  abline(h = nrow(res_qPCR) * 0.5, col = "grey")
  legend_texts <- c(paste0("Partitions: ", slot(x, "partitions")),
                    paste0("Events: ", slot(x, "events")),
                    as.expression(bquote(paste(mu, ": ", .(slot(x, "mu"))))),
                    paste0("CT: ", format(slot(x, "CT"), digits = digits)),
                    paste0("CO: ", format(slot(x, "CO"), digits = digits)))
  legend(mincyc, nrow(res_qPCR), legend_texts)
  # Add rug to the the plot the illustrate the density of events
  if (rug) 
    rug(res_qPCR[, 1])
})



setMethod("show", signature(object = "qpcrpp"), function(object) {
  print(slot(object, ".Data"))    
})


setMethod("summary", signature(object = "qpcrpp"), function(object, print = TRUE) {
  cat("\nmu: ", slot(object, "mu"), "\n")
  cat("C0: ", format(slot(object, "CO")), "\n")
  cat("Cycle time: ", format(slot(object, "CT")), "\n")
  cat("Number of partitions: ", slot(object, "partitions"), "\n")
  cat("Number of events: ", slot(object, "events"), "\n")
  cat("\n")
})
