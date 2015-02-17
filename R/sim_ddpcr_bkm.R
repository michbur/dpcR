#' Simulate Droplet Digital PCR
#'
#' Simulates results of a droplet digital PCR.
#'
#' @param m is either the concentration of templates molecules in the raw sample 
#' (copies/microliter)
#' or the expected number of template molecules per droplet.
#' See \code{mexp} parameter for details
#' Must be a (vector of) positive integers.
#' 
#' @param n the expected number of droplets per experiment. Must be a positive integer.
#' Default 20000 based on the Bio-Rad ddPCR QX100 theoretical expected values
#' 
#' @param mexp If \code{TRUE}, m is the expected number of template molecules per droplet
#' If \code{FALSE}, m is the concentration of the raw sample
#' Default \code{TRUE} as in Jacobs et al.
#' 
#' @param n_exp the number of experiments that are simulated by the function for each given 
#' \code{m}.
#' Default 8 for eight replicates for each given \code{m} as in Jacobs et al. 2014
#' 
#' @param pos_sums if \code{TRUE}, function returns only the total number and
#' the number of positive (containing at least one molecule) droplets per well.
#' If \code{FALSE}, the function returns a vector of length equal to the number
#' of droplets. Each element of the vector represents whether the given droplet
#' contained at least one target molecule or was void of target molecules.
#' 
#' @param fluo if \code{NULL}, the function calculates total number of positive droplets.
#' If \code{TRUE}, the function returns the florescence intensities of all droplets
#' If a positive real number, the function returns the full fluorescence curve
#' with the given number the expected space between two consecutive measured droplets.
#' Values between 10-20 give nice results 
#' Default \code{NULL} to mimic automatic commercial output.
#' 
#' @param sddropc standard deviation of the number of droplets generated
#' Must be a real number between 0 and \code{n} divided by 10.
#' Default 0 for constant number of droplets

#' @param mudropr average proportion (between 0 and 1) of retained partitions
#' Must be a real number between 0 and 1
#' Default 1 for no loss

#' @param sddropr relative standard deviation of the proportion of retained partitions
#' Must be a real number, preferably close to 0.
#' Default 0 for a constant loss

#' @param Pvar If \code{TRUE}, number of copies in constant volume follows P(c) distribution
#' If \code{FALSE},  number of copies in constant volume is constant
#' Default \code{TRUE} for the realistic Poisson model.

#' @param piperr coefficient of variation of the actual pipetted volume from the raw material.
#' Must be a positive real number, preferably close to 0 (0.1 = 10% is very large).
#' Default 0 for constant volume equal to the expected volume

#' @param dropsd relative variability of the droplet volume
#' parameter sigma of a lognormal distribution with mu = 0
#' Must be a positive real number, preferably close to 0.
#' Default 0 for constant droplet size

#' @param falpos probability that a partition containing no copy gives a positive result
#' Must be a real number between 0 and 1
#' Default 0 for no false positives
#' Only used with \code{fluo} is \code{NULL}
#' 
#' @param falneg probability that a partition containing at least one copy gives a negative 
#' result
#' Must be a real number between 0 and 1
#' Default 0 for no false negatives
#' Only used with \code{fluo} is \code{NULL}
#' 
#' @param rain parameter that defines how much inhibition is enforced on positive droplets.
#' Must be a real number between 0 and 1 with 0 no rain, 1 positive droplets follow same 
#' distribution as negative droplets
#' Default 0 for no rain
#' Not used with \code{fluo} is \code{NULL}
#' 
#' @details sim_ddpcr_bkm is based on the R code from Jacobs et al. (2014) (see references).
#' @references
#' Jacobs B, Goetghebeur E, Clement L \emph{Impact of variance components on reliability of 
#' absolute quantification using digital PCR} BMC Bioinformatics, 2014.
#' @export
#' @examples
#' \dontrun{
#' # no parameters, no replicates, one given concentration
#' test1 <- sim_ddpcr_bkm(0.5, n_exp = 1L)
#' str(test1)
#' -log(1-test1[[1]][1]/test1[[1]][2])
#' # changed parameters, no replicates, one given concentration
#' test2 <- sim_ddpcr_bkm(0.5, n_exp = 1, seed = 2, sddropc = 500, mudropr = 0.7,
#'                        sddropr = 0.1, Pvar = TRUE, piperr = 0.02, dropsd = 0.2,
#'                        falpos = 0.001, falneg = 0.01)
#' str(test2)
#' -log(1-test2[[1]][1]/test2[[1]][2])
#' # changed parameters, no replicates, one given concentration
#' # output all droplets and their peak fluorescence
#' test3 <- sim_ddpcr_bkm(0.5, n_exp = 1, seed = 3, pos_sums = TRUE, fluo = TRUE,
#'                        sddropc = 500, mudropr = 0.7, sddropr = 0.1, Pvar = TRUE,
#'                        piperr = 0.02, dropsd = 0.2, falpos = 0.001, falneg = 0.01)
#' str(test3)
#' -log(1-sum(test3[[1]])/length(test3[[1]]))
#' plot(density(test3[[2]])) # not logical: falpos & falneg do not influence distribution
#' 
#' # output only number of positive droplet, but also their fluorescence
#' test3b <- sim_ddpcr_bkm(0.5, n_exp = 1, seed = 3, pos_sums = FALSE, fluo = TRUE, 
#'                         sddropc = 500, mudropr = 0.7, sddropr = 0.1, Pvar = TRUE, 
#'                         piperr = 0.02, dropsd = 0.2, falpos = 0.001, falneg = 0.01)
#' str(test3b)
#' 
#' # changed parameters, no replicates, one given concentration
#' # output all droplets and the full fluorescence (huge, length about 128 800)
#' # becomes somewhat slower (16 seconds on my relatively slow computer)
#' system.time(test4 <- sim_ddpcr_bkm(0.5, n_exp = 1, seed = 4, pos_sums = TRUE, fluo = 10, sddropc = 500, 
#'                                    mudropr = 0.7, sddropr = 0.1, Pvar = TRUE, piperr = 0.02,
#'                                    dropsd = 0.2, rain = 0.1))
#' str(test4)
#' -log(1-sum(test4[[1]])/length(test4[[1]]))
#' par(mar = rep(0,4))
#' par(mfrow = c(10,1))
#' x <- 12880
#' for(i in 1:10){
#'   plot(test4[[2]][((i-1)*x+1):(i*x)], type = "l", xaxt = "n")}
#' par(mfrow = c(1,1))
#' # better zoom in and plot less
#' par(mfrow = c(10,1))
#' x <- 250
#' for(i in 11:20){
#'   plot(test4[[2]][((i-1)*x+1):(i*x)], type = "l", xaxt = "n")}
#' par(mfrow = c(1,1))
#' 
#' # 8 replicates, one given concentration
#' # output all droplets and peak fluorescence
#' system.time(test5 <- sim_ddpcr_bkm(0.5, n_exp = 8, seed = 5, pos_sums = TRUE, fluo = TRUE, 
#'                                    sddropc = 500, mudropr = 0.7, sddropr = 0.1, Pvar = TRUE, 
#'                                    piperr = 0.02, dropsd = 0.2, falpos = 0.001, falneg = 0.01))
#' str(test5)
#' conc <- NULL
#' for(i in 1:8){
#'   conc <- c(conc, -log(1-sum(test5[[2*i-1]])/length(test5[[2*i-1]])))}
#' conc
#' # 8 replicates, several concentrations
#' # output all droplets and peak fluorescence
#' # higher concentrations (and more droplets) take more time.
#' # This set-up took about 1 minute on my computer
#' system.time(test6 <- sim_ddpcr_bkm(exp(-4:1), n_exp = 8, seed = 6, pos_sums = TRUE, fluo = TRUE, 
#'                                    sddropc = 500, mudropr = 0.7, sddropr = 0.1, Pvar = TRUE, 
#'                                    piperr = 0.02, dropsd = 0.2, falpos = 0.001, falneg = 0.01))
#' str(test6)
#' conc <- NULL
#' for(j in 1:6){
#'   conct <- NULL
#'   for(i in 1:8){
#'     conct <- c(conct, -log(1-sum(test6[[16*(j-1)+2*i-1]])/length(test6[[16*(j-1)+2*i-1]])))
#'   }
#'   conc <- cbind(conc, conct)
#' }
#' colnames(conc) <- round(exp(-4:1), 3)
#' conc
#' }


sim_ddpcr_bkm <- function(m, n = 20000L, mexp = TRUE, n_exp = 8L, pos_sums = FALSE, 
                          fluo = NULL, sddropc = 0, mudropr = 1, sddropr = 0, Pvar = TRUE,
                          piperr = 0, dropsd = 0, falpos = 0, falneg = 0, 
                          rain = 0) {
  
  ##############
  ### checks ###
  ##############
  
  if(!is.logical(mexp)) stop("mexp must be a logical argument (TRUE or FALSE).", call. = TRUE, domain = NA)
  if(max(!is.finite(m))) stop("Concentrations should all be numeric.", call. = TRUE, domain = NA)
  if(min(m) < 0) stop("Concentrations cannot be negative.", call. = TRUE, domain = NA)
  lambda <- ifelse(mexp, m, m * 0.89 / 1000)
  
  if(!is.numeric(n)) stop("Number of droplets must have a numeric argument.", call. = TRUE, domain = NA)
  if(n < 10) stop("Number of droplets must be larger than 10.", call. = TRUE, domain = NA)
  if(!is.integer(n)) {
    warning("Number of droplets will be rounded up to the next integer.", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NA)
                      n <- ceiling(n)
  }
  
  if(!is.numeric(n_exp)) stop("number of replicates must have a numeric argument.", call. = TRUE, domain = NA)
  if(n_exp < 1) stop("number of replicates must be at least 1.", call. = TRUE, domain = NA)
  if(!is.integer(n_exp)) {
    warning("number of replicates 'n_exp' will be rounded up to the next integer.", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NA)
                          n_exp <- ceiling(n_exp)
  }
  
  if(!is.logical(pos_sums)) stop("pos_sums must be a logical argument (TRUE or FALSE).", call. = TRUE, domain = NA)
  
  if(!is.numeric(sddropc)) stop("sddropc must have a numeric argument.", call. = TRUE, domain = NA)
  if(sddropc < 0) {warning("sddropc will be set to 0.", call. = TRUE, domain = NA)
                   sddropc <- 0}
  if(sddropc > n / 5) {warning("sddropc will be set to n/5.", call. = TRUE, domain = NA)
                     sddropc <- n / 5}
  
  if(!is.numeric(mudropr)) stop("mudropr must have a numeric argument.", call. = TRUE, domain = NA)
  if(mudropr * n < 10) stop("mudropr too small, too few droplets will be returned.", call. = TRUE, domain = NA)
  if(mudropr > 1) warning("mudropr will be set to 1.", call. = TRUE, domain = NA) # happens in code
  
  if(mudropr < 1){if(!is.numeric(sddropr)) stop("sddropr must have a numeric argument.", call. = TRUE, domain = NA)
                  if(sddropr < 0) {warning("sddropr will be set to 0.", call. = TRUE, domain = NA)
                                   sddropr <- 0}
                  if((sddropr >= mudropr) | ((sddropr + mudropr) >= 1)) stop("sddropr too large.", call. = TRUE, domain = NA)
  }
  
  if(!is.numeric(piperr)) stop("pipette error must have a numeric argument.", call. = TRUE, domain = NA)
  if(piperr < 0) stop("pipette error should be positive or 0.", call. = TRUE, domain = NA)
  
  if(!is.numeric(dropsd)) stop("dropsd must have a numeric argument.", call. = TRUE, domain = NA)
  if(dropsd < 0) {
    warning("dropsd will be set to 0.", call. = TRUE, domain = NA)
    dropsd <- 0
  }
  
  if(!is.logical(Pvar)) stop("Pvar must be a logical argument (TRUE or FALSE).", call. = TRUE, domain = NA)
  
  if(is.null(fluo)) {
    fluoselect <- 1
    if(!is.numeric(falpos)) stop("falpos must have a numeric argument.", call. = TRUE, domain = NA)
    if(falpos < 0) {
      warning("falpos will be set to 0.", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NA)
      falpos <- 0
    }
    if(!is.numeric(falneg)) stop("falneg must have a numeric argument.", call. = TRUE, domain = NA)
    if(falneg < 0) {
      warning("falneg will be set to 0.", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NA)
      falneg <- 0
    }
    if(falpos >= 1) stop("falpos too large, set a number between 0 and 1.", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NA)
  }
  else{
    if(!is.numeric(rain)) stop("rain must have a numeric argument.", call. = TRUE, domain = NA)
    if(rain < 0) {
      warning("rain will be set to 0.", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NA)
      rain <- 0
    }
    if(rain >= 1) {
      warning("rain will be set to 1.", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NA)
      rain <- 1
    }
    if(is.logical(fluo)) 
      fluoselect <- ifelse(fluo, 2, 1)
    else{
      if(is.numeric(fluo)) {
        if(fluo > 0) {
          fluoselect <- 3
        } else {
          stop("fluo does not have a valid argument.", call. = TRUE, domain = NA)
        }
      } else {
        stop("fluo does not have a valid argument.", call. = TRUE, domain = NA)
      }
    }
  }
  
  ###############
  ### repfunc ###
  ###############
  
  # Same procedure for all replicates
  # repfunc is called internally in samfunc
  repfunc <- function(repdat) {
    dropmem <- sample(repdat[1], repdat[2], replace = TRUE, prob = rlnorm(repdat[1], 0, dropsd))
    # droplet membership, probability proportional to size, size following a lognormal distribution
    dropn <- ifelse(mudropr >= 1, repdat[1], 
                    round(repdat[1]*plogis(rnorm(1, log(mudropr/(1 - mudropr)), log((mudropr + sddropr)/(mudropr - sddropr)*(1 - mudropr + sddropr)/(1 - mudropr - sddropr))/2))))
    # number of droplets retained
    dropmem <- dropmem[dropmem <= dropn]
    # only retain copies of which the droplet is retained (lower rank)
    if(fluoselect == 1){
      if(pos_sums) { #changed from !pos_sums - I think it was bug
        dropno <- dropn - length(as.vector(table(dropmem)))
        # number of droplets without copy (total - number with copies)
        droppos <- dropn - (rbinom(1, dropno, 1 - falpos) + rbinom(1, dropn - dropno, falneg))
        return_drops <- c(droppos, dropn)
      }
      # number of droplets with a negative signal (true neg + false neg)
      else {
        dropvec <- sapply(1L:dropn, function(i)
          any(dropmem == i))
        
        # vector with TRUE for positive droplets and FALSE for negative
        dropfin <- (sapply(dropvec,function(x)
          ifelse(x, rbinom(1, 1, falneg), rbinom(1, 1, falpos))))%%2
        return_drops <- dropfin
      }
      # vector TRUE for positive signal and FALSE for negative signal
      return_fluo <- NULL
    }
    else {
      dropvec <- sapply(1L:dropn, function(i) any(dropmem == i))
      # vector with TRUE for positive droplets and FALSE for negative
      fluopeaks <- rnorm(dropn, 1000, 100) + 
        8000*dropvec*(1 - runif(dropn)^(1/rain - 1))*(1 - rain^2) + 
        2000*(1 - dropvec)*(1 - runif(dropn)^rain)*(1 - rain^2)
      # random variation+downward rain+upward rain
      dropfin <- (fluopeaks > 2500)
      # hard threshold as in most software these days
      # vector TRUE for positive signal and FALSE for negative signal
      
      return_drops <-  if (pos_sums) {
        dropfin
      } else {
        c(sum(dropfin), dropn)
      }
      
      return_fluo <- if (fluoselect==2) {
        fluopeaks
      } else{
        fluopos <- (1L:dropn)*fluo + 9 + runif(dropn)*2 + rnorm(dropn)
        # vector of positions where the peak was found
        # peaks on average 10 positions away from each other.
        fluox <- 1L:(round(dropn*fluo + 90, -2))
        fluoy <- rnorm(length(fluox), 50, 10)
        # define fluo vectors with random background
        j <- 1
        for (i in fluox) {
          if (j <= dropn) {
            if (fluopos[j] < (i - 20)) {
              j <- j + 1
            }
            for(k in j:round(j + 30/fluo)){
              # move j such that only the influence of peaks close by are counted
              # influence of peaks further away would be marginally small anyway
              if(k <= dropn) {
                dist <- fluopos[k] - i
                # distance between peak and current location
                fluoy[i] <- fluoy[i] + dnorm(dist/2 + rnorm(1, 0, 0.1))/dnorm(0)*fluopeaks[k]*0.95
                # add fluorescence signal stemming from this specific droplet
              }
            }
          }
        }
        fluoy
      }
    }
    list(drop = return_drops, fluo = return_fluo)
    # returns list with first element vector of droplets 1/0
    # or pair of number of pos droplets and total number of droplets
    # and second element either NULL, peak fluorescence of droplets
    # or vector with continuous fluorescence output 
  }
  
  
  ###############
  ### samfunc ###
  ###############
  
  # Same procedure for all simulations
  # samfunc is called in sim_ddpcr
  samfunc <- function(lambdan){
    dropstart <- round(rnorm(n_exp,n,sddropc))
    # number of droplets
    copyvar <- lambdan*rnorm(n_exp, 1, piperr)*dropstart
    copyvar[copyvar < 0] <- 0
    # expected number of copies after pipette variation
    copyn <- ifelse(rep(Pvar, n_exp), rpois(n_exp, copyvar), round(copyvar))
    # number of copies
    lamdummy <- rep(lambdan, n_exp)
    repdat <- data.frame(dropstart, copyn, lamdummy)
    # number of droplets and copies in a list with n_exp elements, all pairs
    # droplets is the first element, copies the second
    repres <- apply(repdat, 1, repfunc)
    repres
    # returns a list with n_exp*2 elements with
    # first element vector of droplets 1/0 or
    #   pair of number of pos droplets and total number of droplets
    # second element either NULL, peak fluorescence of droplets or
    #   vector with continuous fluorescence output 
    # and so on for each replicate
  }
  
  
  ###############
  ### execute ###
  ###############
  
  out <- unlist(lapply(lambda, samfunc), recursive = FALSE)
  
  if(is.null(fluo)) {
    suppressMessages(bind_dpcr(lapply(out, function(single_run)
      create_dpcr(single_run[["drop"]], length(single_run[["drop"]]), NULL, type = "fluo")
    )))
  } else {
    suppressMessages(bind_dpcr(lapply(out, function(single_run)
      create_dpcr(single_run[["fluo"]], length(single_run[["drop"]]), NULL, type = "fluo")
    )))
  }
  
  
  
  # out returns a list with entries for each lambda
  # each entry is a list with n_exp*2 elements from samfunc
  # first element vector of droplets 1/0 or
  #   pair of number of pos droplets and total number of droplets
  # second element either NULL, peak fluorescence of droplets or
  #   vector with continuous fluorescence output
  # and so on
  
  # To do: create ddpcr object?
  # create_ddpcr(res, rep(n, n_exp), threshold = 2500, type = type)
}

