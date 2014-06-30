# Test if x is a positive integer value
# warn defines the warning level
t.int <- function (x, warn = 0) {
  options(warn = warn)
  if (x != abs(round(x)) || x <= 0) {
    warning(paste(x, " was not a positive integer (e.g., 1, 11, 23).
                  Automatically tried to convert ", x, " to integer"))
  }
  abs(as.integer(x))
}

# OTHER CLASSES ---------------------------------------------
setOldClass("modlist")

# GENERICS ---------------------------------------------
setGeneric("summary")
setGeneric("show")

moments <- function (input, ...) {
  stop("Wrong class of 'input'", call. = TRUE, domain = NA)
}
setGeneric("moments")


qpcr_analyser <- function (input, cyc = 1, fluo = NULL, model = l5, norm = FALSE, iter_tr = 50, 
                           type = "Cy0", takeoff = FALSE) {
  stop("Wrong class of 'input'", call. = TRUE, domain = NA)
}
setGeneric("qpcr_analyser")


# OTHER METHODS ---------------------------------------------
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

setMethod("qpcr_analyser", signature(input = "data.frame"), function(input, 
                                                                     cyc = 1, 
                                                                     fluo = NULL, 
                                                                     model = l5, 
                                                                     norm = FALSE, 
                                                                     iter_tr = 50, 
                                                                     type = "Cy0", 
                                                                     takeoff = FALSE) {
  all_fits <- fit_adpcr(input, cyc, fluo, model, norm, iter_tr)
  res <- analyze_qpcR(all_fits, type, takeoff)
  res <- cbind(res, deltaF = calc_deltaF(input, cyc, fluo))
  res
})


setMethod("qpcr_analyser", signature(input = "modlist"), function(input, type = "Cy0", takeoff = FALSE) {
  res <- analyze_qpcR(input, type, takeoff)
  res
})


# analysis <- qpcRanalyzer(fits, arrs[[1]][[1]]@fluo[[1]])


#qpcRanalyzer(tmp_fits[[1]], arrs[[1]][[1]]@fluo[[1]], cyc = 1, "Cy0", takeoff = TRUE)

# zeros <- cbind(reps384, matrix(rnorm(45*300, 5), ncol = 300))
# sreps <- cbind(reps384, matrix(rnorm(45*100, 5), ncol = 100))
# 
# plot(zeros[, 1], zeros[, 2], ylim = c(0, 20000), cex = 0)
# sapply(2L:ncol(zeros), function(i) lines(zeros[, 1], zeros[, i]))
# 
# tmp_res <- qpcRanalyzer(zeros, sort(rep(10^(1L:7), 12)), log = TRUE)

# out <- qpcRanalyzer(guescini1, sort(rep(10^(1L:7), 12)), log = TRUE)
# LinReg <- lm(out[, 2] ~ out[, 1])
# summary.plot <- summary(LinReg)
# plot(out[, 1], out[, 2])
# 
# EFF <- 3.32/((10^(-1/summary.plot[["coefficients"]][2])) - 1) * 100
# 
# abline(LinReg, col = 2)
# 
# text(4, 25, paste(summary.plot[["coefficients"]][1], 
#                   summary.plot[["coefficients"]][2],
#                   summary.plot[["r.squared"]],
#                   EFF, sep = "\n"))




#Stefan's validation method
valid_amp <- function(x) {
  tres <- t.test(head(x), tail(x), alternative="less")$p.value < 0.01
  sigres <- mean(tail(x)) > mean(x[5:15]) + 3*sd(x[5:15])
  as.logical(tres * sigres)
}

fit_adpcr <- function(pcr_data, cyc = 1, fluo = NULL, model = l5, norm = FALSE, 
                      iter_tr = 50){
  if (class(pcr_data) == "list") {
    lapply(pcr_data, function (i) fit_single_adpcr(i, cyc, fluo, model, norm, iter_tr))
  } else {
    fit_single_adpcr(pcr_data, cyc, fluo, model, norm, iter_tr)
  }
}

#not for users
fit_single_adpcr <- function(pcr_data, cyc, fluo, model, norm, iter_tr) {
  if (is.null(fluo)) {
    all_fluos <- (1L:ncol(pcr_data))[-cyc]
  } else {
    all_fluos <- fluo
  }
  anal_fluo <- all_fluos[vapply(all_fluos, function(x) 
    valid_amp(pcr_data[[x]]), TRUE)]
  
  #only validated columns are fitted
  all_fits <- modlist(pcr_data, cyc, anal_fluo, model = model, norm = norm, 
                      verbose=FALSE, remove = "KOD")
  
  #temporary solution, should change nls.lm.control instead
  good_fits_ind <- vapply(all_fits, function(x) 
    x[["convInfo"]][["finIter"]], 0) < iter_tr
  
  good_fits <- all_fits[(1L:length(all_fits))[good_fits_ind]]
  class(good_fits) <- c("modlist", "pcrfit")
  names(good_fits) <- vapply(good_fits, function(x) x[["names"]], "a")
  good_fits
}

safe_efficiency <- function(fit, type) {
  res <- try(efficiency(fit, type = type, plot = FALSE)[c(type, "eff", "fluo")], 
             silent = TRUE)
  if (class(res) == "try-error") {
    res <- rep(NaN, length(c(type, "eff", "fluo"))) 
  } else {
    if (length(res[["eff"]]) > 1)
      res$eff <- NaN
  }
  unlist(res)
}

calc_deltaF <- function(pcr_data, cyc, fluo) {
  if (is.null(fluo)) {
    all_fluos <- (1L:ncol(pcr_data))[-cyc]
  } else {
    all_fluos <- fluo
  }
  vapply(all_fluos, function(x)  
    quantile(tail(pcr_data[, x]), 0.85) - quantile(head(pcr_data[, x]), 0.25), 0)
}

analyze_qpcR <- function(fit_list, type = "Cy0",  takeoff = FALSE) {
  res <- t(vapply(fit_list, function(fit) 
    safe_efficiency(fit, type), c(0, 0, 0)))
  if (takeoff) {
    res <- cbind(res, t(vapply(fit_list, function(fit) 
      unlist(takeoff(fit)[c("top", "f.top")]), c(0, 0))))
  }
  
  res
}
#to do
#general function to test dpcr objects
#k - positive droplets/chambers
#total number of droplets/chambers
#different principle than dube/bhat method. Calculate confidence interval for k, not for m
# test_dpcr <- function(k, n) {
#   #theoretical values
#   theor <- round(fl(unlist(binom.confint(k, n, methods = "wilson", conf.level = 0.95)[, 4:6]))*n, 0)
#   sim_dpcr(theor[1], n, times = 1000, dube = TRUE, pos_sums = TRUE, n_panels = 1000)
# }



# COMPARE DISTRIBUTION ------------------------------

#calculates coordinates of bars in our barcharts
calc_bars <- function(x, w = 1, names = "counts") {
  if (class(x)[1] == "matrix") {
    ytops <- x[ ,2]
    xs <- x[ ,1] 
  } else {
    if (names == "counts") {
      ytops <- as.vector(x)
      xs <- as.numeric(names(x))
    } else {
      ytops <- x
      xs <- 1L:length(x) - 1
    }
  }
  matrix(c(xs - 0.5*w, rep(0, length(ytops)), xs + 0.5*w, ytops), ncol = 4, byrow = F)
}


dpcr_calculator <- function(k, n, average = FALSE, log = FALSE) {
  if (n <= k)  
    stop("'n' must be larger than 'k'.", call. = TRUE, domain = NA)
  if (k == 0)  
    stop("'k' must be larger then 0. No template molecules in the sample", call. = TRUE, 
         domain = NA)
  if (!(average %in% c(TRUE, FALSE)))
    stop("'average' must be TRUE or FALSE.", call. = TRUE, domain = NA)
  
  sig5 <- round(sqrt(k*(1 - k/n))*5, 0)
  start_r <- ifelse(k - sig5 < 0, 0, k - sig5)
  stop_r <- ifelse(k + sig5 >= n, n - 1, k + sig5)
  range <- start_r:stop_r
  res <- dbinom(range, size = n, prob = k/n, log = log)
  if (average)
    range <- -log(1 - range/n)
  matrix(c(range, res), ncol = 2)
}

#helper function returning y values of confidence intervals
y_val_conf <- function(conf, data, side) {
  side_id <- ifelse(side == "left", 2, 3)
  id <- which(sort(c(data[ ,1], conf[[side_id]])) == conf[[side_id]])
  if (id != 1 && id <= nrow(data)) {
    y_l <- data[id - 1, 2]
    y_r <- data[id, 2]
    x_l <- data[id - 1, 1]
    x_r <- data[id, 1]
    #calculate y value for conf
    c(id + side_id - 3, (y_r - y_l)/(x_r - x_l)*(conf[[side_id]] - x_l) + y_l,
      conf[[side_id]])
  } 
}


plot_conf_int <- function(conf_int, data, side, 
                          conf_int_col = adjustcolor("cyan4", alpha.f = 0.15), 
                          conf_int_border = adjustcolor("cyan4", alpha.f = 0.15), 
                          ...) {
  #get position of conf between values in data
  y_val <- y_val_conf(conf_int, data, side)
  if (!is.null(y_val)) {
    if (side == "left") {
      id1 <- 1
      id2 <- y_val[1]
      dat_plot <- rbind(data[id1:id2,], y_val[3:2])   
    }
    if (side == "right") {
      id1 <- y_val[1]
      id2 <- nrow(data)
      dat_plot <- rbind(y_val[3:2], data[id1:id2,])
    }
    dat_plot <- rbind(c(dat_plot[1,1], 0), dat_plot, c(dat_plot[nrow(dat_plot),1], 0))
    polygon(dat_plot, border = NA, col = conf_int_col, ...)
    lines(dat_plot, col = conf_int_border)
  }
}

plot_distr <- function(data, add = FALSE,
                       distr_col = adjustcolor("lightskyblue1", alpha.f = 0.4),
                       distr_border = "lightskyblue1",
                       bar_col = adjustcolor("azure3", alpha.f = 0.65),
                       bar_border = adjustcolor("azure3", alpha.f = 0.65), 
                       xlab = "", ylab = "", ...) {
  if (!add) {
    plot(x = data[ ,1], y = data[, 2], cex = 0, ylab = ylab, xlab = xlab, ...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
         col = adjustcolor("grey", alpha.f = 0.30))
    axis(1, tck = 1, col.ticks = "white", labels = FALSE)
    axis(2, tck = 1, col.ticks = "white", labels = FALSE)
    if (length(data) < 80)
      axis(3, at = data[, 1], labels = ifelse(as.integer(data[, 1]) == data[, 1], 
                                              as.integer(data[, 1]), round(data[, 1], 2)), 
           mgp = c(3, 0.35, 0), cex.axis = 0.85, tcl = -0.3, lwd.ticks = 1.2)
    
  }
  x <- c(data[1, 1], data[, 1], data[nrow(data), 1])
  y <- c(0, data[, 2], 0)
  polygon(x = x, y = y, col = distr_col, 
          border = NA)
  lines(x = x, y = y, col = distr_border, lwd = 2)
  if (length(data) < 80) {
    bars <- calc_bars(data, w = (par("usr")[2] - par("usr")[1])/50)
    apply(bars, 1, function(x) 
      rect(x[1], x[2], x[3], x[4], col = bar_col, border = bar_border))
  }
}


dpcr_density <- function(k, n, average = FALSE, methods = "wilson", 
                         conf.level = 0.95, plot = TRUE) {
  dat <- dpcr_calculator(k, n, average)
  conf <- binom.confint(k, n, methods = methods, conf.level = conf.level)
  if (average) {
    conf[, c(4:6)] <- - log(1 -  conf[, c(4:6)])
    names(conf)[4] <- "lambda"
    xlab = "Molecules/partition"
    main = "Number of molecules per partition"
  } else {
    conf[, 4:6] <- conf[, 4:6] * n
    xlab = "Positive partitions"
    main = "Number of positive partitions"
  }
  names(conf)[2] <- "k"
  if (plot) {
    plot_distr(dat, ylab = "Density", xlab = xlab,
               main = main)
    plot_conf_int(conf[1, 4:6], dat, "left", conf_int_col = adjustcolor("cyan4", 
                                                                        alpha.f = 0.15), 
                  conf_int_border = adjustcolor("cyan4", alpha.f = 0.15))
    plot_conf_int(conf[1, 4:6], dat, "right", conf_int_col = adjustcolor("cyan4", 
                                                                         alpha.f = 0.15), 
                  conf_int_border = adjustcolor("cyan4", alpha.f = 0.15))
  }
  conf
}


dpcr_density_gui <- function()
  runApp(system.file("dpcr_density_gui", package = "dpcR"))


compare_dens <- function(input, moments = TRUE, ...) {  
  #moments() checks class and so on
  
  if (ncol(input) > 1)
    stop("Input must contain only one panel.", call. = TRUE, domain = NA)    
  
  all_moms <- moments(input)
  lambda <- all_moms[1,1]
  
  xup <- max(input)
  data <- table(factor(input, levels = 0L:xup))
  bars <- calc_bars(data)
  theor <- dpois(0L:xup, lambda)*length(input)
  ytop <- ifelse(max(theor) >= max(data), max(theor), max(data))
  
  plot(NA, NA, xlim = c(-0.5, xup + 0.5), ylim = c(-0, ytop), 
       xlab = "Number of molecules", ylab = "Counts", ...)
  
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
       col = adjustcolor("grey", alpha.f = 0.30))
  axis(2, tck = 1, col.ticks = "white", labels = FALSE)
  
  apply(bars, 1, function(x) 
    rect(x[1], x[2], x[3], x[4]))
  #   axis(4, at = theor, labels = 0L:xup, tck = 1, lty = "dotted", 
  #        col.ticks = "darkgrey")
  #   mtext("Theoretical counts", side = 4, line = 2) 
  sapply(0L:xup, function(x) 
    lines(c(x, x), c(0, theor[x + 1]), lty = "dotted", col = "grey12", lwd = 2))
  
  if (moments) {
    labels <- rownames(all_moms)
    sapply(1L:4, function(i) {
      text(0.85*xup, (98 - 5*i)/100*ytop, paste0(labels[i], ":"), pos = 2)
      text(0.89*xup, (98 - 5*i)/100*ytop, round(all_moms[i, 2], 4))
      text(0.98*xup, (98 - 5*i)/100*ytop, round(all_moms[i, 1], 4))
    })
    text(0.89*xup, 0.99*ytop, "Theoretical", pos = 1)
    text(0.98*xup, 0.99*ytop, "Empirical", pos = 1)
  }
}

# POISSON PROCESS INTEGRATION AND TESTS ----------------------------------
setClass("qpcrpp", contains = "matrix", representation(.Data = "matrix", mu = "numeric", 
                                                       CO = "numeric",
                                                       CT = "numeric", 
                                                       partitions = "integer",
                                                       events = "integer"))


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
  # cycle.time should give the "average time" between the occurence of a 
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
  # cyc.occ gives the occurence in an interval
  mu <- dens.tmp[["k"]] * delta
  fact.NuEvents <- factorial(NuEvents)
  if (fact.NuEvents != "Inf") {
    cyc.occ <- (exp(-mu) * mu^NuEvents)/fact.NuEvents
  } else cyc.occ <- "too large"
  # END WIP
  
  new("qpcrpp", .Data = data.matrix(res_qPCR), mu = mu, CT = cycle.time, CO = cyc.occ, 
      partitions = dens.tmp[["n"]], events = dens.tmp[["k"]])
}


setMethod("plot", signature(x = "qpcrpp"), function(x, mincyc = 1, maxcyc = 45, rug = TRUE) {
  # Plot the calculated qPCR data as Poisson processes
  res_qPCR <- slot(x, ".Data")
  plot(res_qPCR[, 1], res_qPCR[,3], xlim = c(mincyc, maxcyc), 
       ylim = c(0, nrow(res_qPCR)), xlab = "Cycle", 
       ylab = expression(paste(lambda,
                               " (cycles)")), type = "S", lwd = 1.5)
  abline(h = nrow(res_qPCR) * 0.5, col = "grey")
  legend(mincyc,nrow(res_qPCR), c(paste0("Partitions: ", slot(x, "partitions")),
                                  paste0("Events: ", slot(x, "events")),
                                  paste0("mu: ", slot(x, "mu")), 
                                  paste0("CT: ", slot(x, "CT")),
                                  paste0("CO: ", slot(x, "CO"))))
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


# Example of an artificial chamber dPCR experiment using the test data set from
# qpcR. The function limit_cq is used to calculate the Cy0 value and converts 
# all values between a defined range to 1 and the remaining to 0.
limit_cq <- function(data = data, cyc = 1, fluo = NULL,
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
      summary(inder(x = data[, cyc], y = data[, fluo_col]), print = FALSE)["SDM"]
      setTxtProgressBar(pb, fluo_col)
    }, 0)
  } else {
    Cy0 <- vapply(fluo, function(fluo_col) {
      efficiency(pcrfit(data = data, cyc = cyc, fluo = fluo_col,
                        model = model), type = "Cy0", plot = FALSE)[["Cy0"]]
      setTxtProgressBar(pb, fluo_col)
    }, 0)
  }
  
  Cy0.res <- vapply(Cy0, function(Cy0_i)
    Cq_range[1] <= Cy0_i & Cy0_i <= Cq_range[2], TRUE)
  
  data.frame(Cy0 = Cy0, in.range = Cy0.res)
}



