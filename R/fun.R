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

qpcr_analyser <- function (input, cyc = 1, fluo = NULL, model = l5, norm = FALSE, iter_tr = 50, 
                           type = "Cy0", takeoff = FALSE) {
  stop("Wrong class of 'input'", call. = TRUE, domain = NA)
}

setGeneric("moments")
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

setMethod("qpcr_analyser", signature(input = "data.frame"), function(input, cyc = 1, fluo = NULL, 
                                                                     model = l5, 
                                                                     norm = FALSE, iter_tr = 50, 
                                                                     type = "Cy0", takeoff = FALSE) {
  all_fits <- fit_adpcr(input, cyc, fluo, model, norm, iter_tr)
  res <- analyze_qpcR(all_fits, type, takeoff)
  res <- cbind(res, deltaF = calc_deltaF(input, cyc, fluo))
  res
})


setMethod("qpcr_analyser", signature(input = "modlist"), function(input, type = "Cy0", takeoff = FALSE) {
  res <- analyze_qpcR(input, type, takeoff)
  res
})


# CLASS AND METHODS - droplet ---------------------------------------------

setClass("ddpcr", contains = "matrix", representation(.Data = "matrix", n = "integer", 
                                                      threshold = "numeric", 
                                                      type = "character"))

setMethod("summary", signature(object = "ddpcr"), function(object, print = TRUE) {
  data <- slot(object, ".Data")
  col_dat <-ncol(data)
  type <- slot(object, "type")
  n <- slot(object, "n")
  
  if (type %in% c("nm", "tp")) 
    k <- colSums(data > 0)
  
  if (type %in% c("fluo")) 
    k <- apply(data, 2, function(x) get_k_n(x, slot(object, "threshold")))
  
  invisible(print_summary(k, col_dat, type, n, print))
})

setMethod("show", signature(object = "ddpcr"), function(object) {
  print(slot(object, ".Data"))
  cat(paste0("\nType: '", slot(object, "type"), "'"))     
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
             moments(data[ , i])))
           ids <- sort(rep(1:n_cols, 2))
           nms <- colnames(res)
           colnames(res) <- unlist(lapply(1L:(n_cols*2), function(i) 
             paste0(nms[i], ".", ids[i])))
           res
         },
         fluo = {
           k <- apply(data, 2, function(x) get_k_n(x, slot(input, "threshold")))
           vapply(k, function(x) moments(fl(x/n)), rep(0, 4))
         })  
})


# SIMULATONS - droplet ---------------------------------------------

sim_ddpcr <- function(m, n, times, n_exp = 1, dube = FALSE, pos_sums = FALSE, 
                      fluo = NULL) {
  if (!is.null(fluo))
    if (pos_sums)
      stop("During fluorescence simulation 'pos_sums' must be TRUE", call. = TRUE, 
           domain = NA)
  n <- t.int(n)
  res <- sim_dpcr(m, n, times, dube, pos_sums, n_exp)
  if (!is.null(fluo)) {
    res <- apply(res, 2, function(x) sim_ddpcr_fluo(x, n, fluo[[1]], fluo[[2]]))
  }
  #simplify
  type = ifelse(pos_sums, "tp", "nm")
  if (!is.null(fluo))
    type <- "fluo"
  create_ddpcr(res, n, threshold = 0.5, type = type)
}


sim_ddpcr_fluo <- function(res, n, resolution, space) {
  if (length(space) > 1) {
    a <- lapply(space, function(x) c(sin(seq(0, pi, resolution)), rep.int(0, x)))
  } else {
    a <- lapply(1L:n, function(x) c(sin(seq(0, pi, resolution)), rep.int(0, space)))
  }
  result <- matrix(unlist(lapply(1L:n, function(x)
    a[[x]] * res[x])), ncol = 1)
  result
}


# OTHER FUNCTIONS - droplet ---------------------------------------------

create_ddpcr <- function(data, n, threshold = NULL, type) {
  result <- new("ddpcr")
  slot(result, ".Data") <- data
  slot(result, "n") <- n
  slot(result, "type") <- type
  slot(result, "threshold") <- threshold
  result
}

#helper function counting peaks, takes threshold
get_k_n <- function(x, threshold = 1) {
  #   if (is.null(threshold)) {
  #     x <- x[x != 0]
  #     threshold <- median(x)
  #   }
  peaks <- findpeaks(x, threshold = threshold)
  pos <- nrow(peaks)
  if (is.null(pos))
    pos <- 0
  #list(k = pos, n = nrow(findpeaks(x, threshold = min(x))), thr = threshold)
  pos
}

#helper function plotting colorful circles below the vic/fam
plot_vf_circ <- function(x, y, radius, bg) {
  symbols(x = x, y = rep(y, length(x)), circles = rep(radius, length(x)), 
          bg = bg, fg = "black", inches = FALSE, add = TRUE)
}

plot_vic_fam <- function(vic, fam, col_vic = "green", col_fam = "blue") {
  if (class(vic) == "ddpcr" && class(fam) == "ddpcr") { 
    if (ncol(vic) > 1 && ncol(fam) > 1)
      stop("Both 'vic' and 'fam' must contain only one panel.", call. = TRUE, domain = NA)    
    if (nrow(vic) == 1 && nrow(fam) == 1)
      stop("Both 'vic' and 'fam' cannot contain total number of positive chambers.", call. = TRUE, 
           domain = NA)    
  } else {
    stop("Both 'vic' and 'fam' must have the 'ddpcr' class", call. = TRUE, domain = NA)
  }
  vic_thr <- slot(vic, "threshold")
  fam_thr <- slot(fam, "threshold")
  vic <- as.vector(vic)
  fam <- as.vector(fam)
  
  y_max_lim <- ifelse(max(vic) > max(fam), max(vic), max(fam))
  y_min_lim <- ifelse(min(vic) > min(fam), min(vic), min(fam))
  
  #center of the circle
  y_points <- -y_max_lim * 0.06
  
  #end of the 'tick'
  y_points2 <- y_points * 0.3
  plot(vic, col = col_vic, type = "l", lwd = 2, ylim = c(y_points, y_max_lim), 
       axes = FALSE, xlab = "", ylab = "Number of molecules")
  axis(2)
  lines(fam, col = col_fam, lty = "dashed")
  abline(h = 0)

  #translate color to rgb
  col_vic <- col2rgb(col_vic)/255
  col_fam <- col2rgb(col_fam)/255
  
  vic_pos <- findpeaks(vic, threshold = vic_thr)[, 1:2]
  fam_pos <- findpeaks(fam, threshold = fam_thr)[, 1:2]
  circ <- unique(c(vic_pos[, 2], fam_pos[, 2]))
  
  radius <- 0.4
  
  for (i in circ)
    lines(c(i, i), c(0, y_points2))
  
  plot_vf_circ(circ, y_points, radius, "white") 
  plot_vf_circ(vic_pos[, 2], y_points, radius, 
               rgb(col_vic[1, 1],
                   col_vic[2, 1], 
                   col_vic[3, 1], 
                   vic_pos[, 1]/y_max_lim))
  plot_vf_circ(fam_pos[, 2], y_points, radius, 
               rgb(col_fam[1, 1],
                   col_fam[2, 1], 
                   col_fam[3, 1], 
                   fam_pos[, 1]/y_max_lim))
  
  #peaks common for both channels
  common <- fam_pos[fam_pos[, 2] %in% vic_pos[, 2], 2]
  if (length(common) != 0) {
    fracs <- sapply(common, function(x) 
      c(vic[x]/(vic[x] + fam[x]), (vic[x] + fam[x])/2))
    common_cols <- rgb(0, fracs[1, ], 1 - fracs[1, ], alpha = fracs[2, ]/y_max_lim)
    plot_vf_circ(common, y_points, radius, common_cols)
  }
}

# CLASS AND METHODS - array ---------------------------------------------

setClass("adpcr", contains = "matrix", representation(.Data = "matrix",
                                                      n = "integer",
                                                      breaks = "numeric",
                                                      type = "character"))


setMethod("summary", signature(object = "adpcr"), function(object, print = TRUE) {
  data <- slot(object, ".Data")
  
  col_dat <- ncol(data)
  type <- slot(object, "type")
  n <- slot(object, "n")
  
  if (type %in% c("nm", "tp")) {
    k <- colSums(data > 0)
    invisible(print_summary(k, col_dat, type, n, print))
  }
  
  if (type %in% c("fluo", "ct")) 
    print(paste0("Summary not currently implemented for data type ", type, "."))
})

#special method declared to hide slots other than .Data
setMethod("show", signature(object = "adpcr"), function(object) {
  print(slot(object, ".Data"))
  cat(paste0("\nType: '", slot(object, "type"), "'"))     
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
             moments(data[ , i])))
           ids <- sort(rep(1:n_cols, 2))
           nms <- colnames(res)
           colnames(res) <- unlist(lapply(1L:(n_cols*2), function(i) 
             paste0(nms[i], ".", ids[i])))
           res
         })  
})

setMethod("qpcr_analyser", signature(input = "adpcr"), function(input, cyc = 1, fluo = NULL, 
                                                                model = l5, 
                                                                norm = FALSE, iter_tr = 50, 
                                                                type = "Cy0", takeoff = FALSE) {
  if (slot(input, "type") != "fluo")
    stop("'input' must contain fluorescence data.", call. = TRUE, domain = NA)
  input <- slot(input, ".Data")
  all_fits <- fit_adpcr(input, cyc, fluo, model, norm, iter_tr)
  res <- analyze_qpcR(all_fits, type, takeoff)
  res <- cbind(res, deltaF = calc_deltaF(input, cyc, fluo))
  res
})

setGeneric("quadrat.test")

setMethod("quadrat.test", signature(X = "adpcr"), function(X, nx_a, ny_a, nx = 5, ny = 5, 
                                                           alternative = c("two.sided", "regular", "clustered"), 
                                                           method = c("Chisq", "MonteCarlo"), 
                                                           conditional = TRUE,
                                                           ..., nsim = 1999) {
  ppp_data <- adpcr_to_ppp(X, nx_a, ny_a)
  lapply(ppp_data, function(single_panel)
    quadrat.test(single_panel, nx, ny, alternative, method, conditional, ..., nsim = 1999))
})

# SIMULATIONS - array ---------------------------------------------

#exact copy of dube simulation

sim_adpcr <- function(m, n, times, n_panels = 1, dube = FALSE, pos_sums = FALSE) {
  n <- t.int(n)
  res <- sim_dpcr(m, n, times, dube, pos_sums, n_panels)
  create_adpcr(res, n, 0L:max(res), type = ifelse(pos_sums, "tp", "nm"))
}

#OTHER FUNCTIONS - array ------------------------------------------

create_adpcr <- function(data, n, breaks = NULL, type, models = NULL) {
  result <- new("adpcr")
  slot(result, ".Data") <- data
  slot(result, "n") <- n
  slot(result, "type") <- type
  if (is.null(breaks)) {
    slot(result, "breaks") <- 0L:max(data)
  } else {
    slot(result, "breaks") <- breaks
  }
  if (!is.null(models))
    slot(result, "models") <- models
  result
}

plot_panel <- function(input, nx_a, ny_a, col = "red", legend = TRUE, 
                       half = "none", ...) {  
  if (class(input) == "adpcr") {
    if (!(slot(input, "type") %in% c("nm", "tp", "ct")))
      stop("Input must contain data of type 'nm', 'tp' or 'ct'.", 
           call. = TRUE, domain = NA) 
    if (ncol(input) > 1)
      stop("Input must contain only one panel.", call. = TRUE, domain = NA)    
    if (nrow(input) == 1)
      stop("Input cannot contain total number of positive chambers.", call. = TRUE, 
           domain = NA)    
  } else {
    stop("Input must have the 'adpcr' class", call. = TRUE, domain = NA)
  }
  if (length(input) != nx_a*ny_a)
    stop (paste0("Can not process with plot since the input 
                 legnth (", length(input) ,
                 ") differs from the size of nx_a*ny_a (", nx_a*ny_a, ").
                   \n Change nx_a*ny_a to have the same number of elements."))
  
  #use breaks to split input 
  cutted_input <- cut(slot(input, ".Data"), breaks = slot(input, "breaks"), 
                      include.lowest = TRUE, right = FALSE)
  
  plot(NA, NA, xlim = c(1, nx_a), ylim = c(1, ny_a), axes = FALSE, xlab = "", 
       ylab = "", ...)
  half <- tolower(half)
  half_val <- switch(half,
                     none =  c(0.25, 0.25),
                     left = c(0.25, 0),
                     right = c(0, 0.25))
  
  coords <- unlist(lapply(1L:nx_a, function(x) 
    lapply(ny_a:1L, function(y) 
      c(xleft = x - half_val[1], ybottom = y - 0.25, xright = x + half_val[2], 
        ytop = y + 0.25))), recursive = FALSE)
  cols <- cutted_input
  ncols <- nlevels(cutted_input)
  if (length(col) == 1) {   
    levels(cols) <- sapply(0:ncols/ncols, function(x) 
      adjustcolor(col, alpha.f = x))
  } else {
    if (length(col) != ncols) {
      stop("The vector of colors must have length equal to the number of levels of 
           the input.", 
           call. = TRUE, domain = NA)    
    }
    levels(cols) <- col
  }
  if (legend)
    legend(x = -0.085 * nx_a, 
           y = ny_a/1.6, 
           legend = levels(cutted_input),
           fill = levels(cols), 
           bty = "n", 
           xpd = TRUE, 
           x.intersp = 0.5)
  
  cols <- as.character(cols)
  args <- lapply(1L:length(input), function(i) 
    c(coords[[i]], list(col = cols[i])))
  sapply(1L:length(input), function(i) 
    do.call(rect, args[[i]]))
  invisible(args)
}

#function for all cases when we need breaks calculated
calc_breaks <- function(vals, breaks = "Sturges", threshold = NULL) {
  if (!is.vector(vals))
    vals <- as.vector(sapply(vals, function(x) as.vector(x)))
  if (!is.null(threshold)) {
    min_v <- min(vals)
    vals <- vals[vals > threshold]
  }
  br <- hist(vals, breaks, plot = FALSE)[["breaks"]]
  if (!is.null(threshold))
    br <- c(min_v, br)
  br
}


test_peaks <- function (x, ...) {
  stop("Wrong class of 'x'", call. = TRUE, domain = NA)
}

setGeneric("test_peaks")


setMethod("test_peaks", signature(x = "numeric"), function(x, y, threshold = 0.05, 
                                                           noise_cut = 0.05, savgol = TRUE, 
                                                           norm = FALSE,
                                                           filter.q = c(0.7, 0.8)) {
  # Initial checking of input values
  if (is.null(x)) 
    stop("Enter 'x' value.", call. = TRUE, domain = NA)
  if (is.null(y)) 
    stop("Enter 'y' value.", call. = TRUE, domain = NA)
  if (length(x) != length(y)) 
    stop("'x' and 'y' differ in length.", call. = TRUE, domain = NA)
  AUCtest(x = x, y = y, threshold = threshold, noise_cut = noise_cut, savgol = savgol, 
          norm = norm, filter.q = filter.q)
  
})

setMethod("test_peaks", signature(x = "adpcr"), function(x, threshold = 0.05, 
                                                         noise_cut = 0.05, savgol = TRUE, 
                                                         norm = FALSE,
                                                         filter.q = c(0.7, 0.8)) {
  # Initial checking of input values
  if (slot(x, "type") != "fluo") 
    stop("'adpcr' object must have type 'fluo'.", call. = TRUE, domain = NA)
  x_vals <- slot(x, ".Data")[[1]]
  y_vals <- slot(x, ".Data")[[2]]
  AUCtest(x = x_vals, y = y_vals, threshold = threshold, noise_cut = noise_cut, savgol = savgol, 
          norm = norm, filter.q = filter.q)
  
})

psp <- function(x, sp) 
  predict(sp, x)[["y"]]



AUCtest <- function(x = x, y = y, threshold = 0.05, noise_cut = 0.05, savgol = TRUE, norm = FALSE,
                    filter.q = c(0.7, 0.8)) {
  
  # Get the raw data and assign them to a data frame containing the abscissa values (e.g., 
  # time, count, ...) and the corresponding peak value (e.g., fluorescence)
  # Preprocessing:
  # Set values below a "noise_cut" value to 0
  y[y < quantile(y, noise_cut)] <- 0
  
  # Normalise data based on the quantiles
  qval <- 0.05
  if (norm) 
    y <- (y - quantile(y, qval)) / (quantile(y, 1 - qval) - quantile(y, qval))
  
  # Smooth the data by Splines or Savitzky-Golay Smoothing (default)
  if (savgol == TRUE) {
    data <- data.frame(x, sgolayfilt(y))
  } else (
    data <- cbind(x, smooth.spline(x,y)$yin)
  )
  
  # find *ALL* peaks of the input data based on findpeaks of the pracma package
  supposed_peaks <-  findpeaks(data[, 2])
  
  # Try to find out where "findpeaks" detected noise (no real peak but just a small spike), 
  # "negative peaks (no amplification, just primer dimer...)" and "positive peaks (true 
  # amplification)" based in the peak height.
  # filter.q is the quantile value of the noise and the quantile value of the negative peaks
  peak_quantiles <- c(min(supposed_peaks), 
                      quantile(supposed_peaks[, 1], filter.q), 
                      max(supposed_peaks))
  
  # test_res is checked later which element of the data is TRUE
  test_res <- cut(supposed_peaks[, 1], peak_quantiles, include.lowest = TRUE)
  levels(test_res) <- c("noise", "negative", "positive")
  
  sp <- smooth.spline(x, y)
  # create an empty matrix with the results of the area under the curve calculation
  # the column number of the matrix might grow depending addition of further elements
  all_peaks <- data.frame(do.call("rbind", lapply(1L:nrow(supposed_peaks), function(i) {
    # select range of single peak
    xy <- data[supposed_peaks[i, 3]:supposed_peaks[i, 4], 1:2]
    
    peak <- rep(0, 6)
    
    # Estimate the AUC by integration. NOTE: Needs improvements because the integration will 
    # fail badly if peak overlap considerably!
    try(integrate_tmp <- integrate(psp, lower = min(xy[, 1]), upper = max(xy[, 1]), sp)$value)
    peak[1] <- supposed_peaks[i, 2] # Position of the peak maximum
    #could use quadinf{pracma} or adaptIntegrate{cubature} instead of integrate, need further investigation
    peak[2] <- ifelse(class(integrate_tmp) == "try-error", NA, integrate_tmp) # Crude estimation of the AUC
    peak[3] <- max(xy[, 1]) - min(xy[, 1]) # crude estimation of the peak width
    peak[4] <- supposed_peaks[i, 1] # height of the peak depending on time ...
    peak[5] <- data[supposed_peaks[i,2],1] # position of the peak depending on the time ...
    # Determine the resolution of the data
    peak[6] <- mean(vapply(2L:nrow(xy), function(i) abs(xy[i, 1] - xy[i - 1, 1]), 0))  # time resolution of the peaks
    peak
  })))
  
  all_peaks <- cbind(1L:nrow(supposed_peaks), test_res, all_peaks)
  colnames(all_peaks) <- c("Peak number", "State", "Position", "AUC", "Width", "Height", 
                           "Index", "Resolution")
  
  list(peaks = all_peaks, data = data) # List containing the table with all values
  # and the smoothed data
}

adpcr_to_ppp <- function(input, nx_a, ny_a) {
  if (class(input) != "adpcr")
    stop("Input must have 'adpcr' class", call. = TRUE, domain = NA)
  
  array_data <- slot(input, ".Data")
  nrow_array <- nrow(array_data)
  
  if (nrow_array != nx_a*ny_a)
    stop (paste0("Can not process with conversion since the input 
                 legnth (", length(input) ,
                 ") differs from the size of nx_a*ny_a (", nx_a*ny_a, ").
                   \n Change nx_a*ny_a to have the same number of elements."))  
  
  #apply in case input contains more than 1 array
  apply(array_data, 2, function(array_col) { 
    data_points <- matrix(NA, nrow = nrow_array, ncol = 3)
    i = 1
    for (x in 1L:nx_a) {
      for (y in ny_a:1L) {
        data_points[i, ] <- c(x, y, array_col[i])
        i <- i + 1
      }
    }
    
    data_points <- data_points[data_points[, 3] > 0, ]
    data_ppp <- ppp(data_points[, 1], data_points[, 2], 
                    c(1, nx_a), c(1, ny_a), marks = data_points[, 3])
  })
}

# SIMULATIONS - droplet, array ------------------------------
#summary workhorse + plot, should be not called directly by user

#dube's simulation
sim_dpcr_dube <- function(m, n, times, pos_sums, n_panels) {
  mt <- m*times
  nt <- n*times
  distr_molec <- sample.int(nt, mt, replace = TRUE)
  counts <- rep(0, nt)
  for (i in distr_molec)
    counts[i] <- counts[i] + 1
  counts <- sample(counts) #is additional shuffle needed?
  if (pos_sums == TRUE) {
    times_v <- 1L:n_panels
    gr_id2 <- times_v*n
    gr_id1 <- gr_id2 - n + 1
    matrix(vapply(times_v, function(i) sum(counts[gr_id1[i]:gr_id2[i]] > 0), 0), 
           ncol = n_panels)
  } else {
    times_v <- 1L:n_panels
    gr_id2 <- times_v*n
    gr_id1 <- gr_id2 - n + 1
    sapply(times_v, function(i) counts[gr_id1[i]:gr_id2[i]])
  }
}

#our simple simulation
sim_dpcr_multi <- function(m, n, times, pos_sums, n_panels) {
  total_m <- m*times
  ms <- rnorm(times, m, 0.05*m)
  ms <- round(ms + (total_m - sum(ms))/times, 0)
  delta <- total_m - sum(ms)
  indices <- sample(1:times, abs(delta))
  ms[indices] <- ms[indices] + sign(delta)
  if (pos_sums) {
    matrix(vapply(ms[1L:n_panels], function(x) 
      sum(table(factor(sample.int(n, x, replace = TRUE), levels = 1L:n)) > 0), 0), 
           ncol = n_panels)
  } else {
    vapply(ms[1L:n_panels], function(x) 
      as.vector(table(factor(sample.int(n, x, replace = TRUE), levels = 1L:n))), 
           rep(0, n))
  }
}


sim_dpcr <- function(m, n, times, dube, pos_sums, n_panels) {
  n <- t.int(n)
  if (n_panels > times) 
    stop("The 'n_panels' argument cannot have larger value than the 'times'
         argument", call. = TRUE, domain = NA)
  if (m < 0) 
    stop("'m' must be a non-negative integer.", call. = TRUE, domain = NA)
  if (n < 1)  
    stop("'n' must be an integer bigger than 1.", call. = TRUE, domain = NA)
  if (dube) {
    res <- sim_dpcr_dube(m, n, times, pos_sums, n_panels)
  } else {
    res <- sim_dpcr_multi(m, n, times, pos_sums, n_panels)
  }
}

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
  list(list(k = k, n = n), summary = sums)
  
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


# COMPARE DISTRIBUTION ------------------------------

#calculates coordinates of bars in our barcharts
calc_bars <- function(x, w = 1, names = "counts") {
  if (class(x)[1] == "matrix") {
    ytops <- x[,2]
    xs <- x[,1] 
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
  id <- which(sort(c(data[,1], conf[[side_id]])) == conf[[side_id]])
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
    plot(x = data[,1], y = data[,2], cex = 0, ylab = ylab, xlab = xlab, ...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
         col = adjustcolor("grey", alpha.f = 0.30))
    axis(1, tck = 1, col.ticks = "white", labels = FALSE)
    axis(2, tck = 1, col.ticks = "white", labels = FALSE)
    if (length(data) < 80)
      axis(3, at = data[, 1], labels = ifelse(as.integer(data[, 1]) == data[, 1], 
                                              as.integer(data[, 1]), round(data[, 1], 2)), 
           mgp = c(3, 0.35, 0), cex.axis = 0.85, tcl = -0.3, lwd.ticks=1.2)
    
  }
  x <- c(data[1, 1], data[, 1], data[nrow(data), 1])
  y <- c(0, data[,2], 0)
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

# Poisson process integration ----------------------------------

qPCR2pp <- function(cycles, process, NuEvents = 1, delta = 1, mincyc = 1, maxcyc = 45, rug = TRUE, 
                    plot = TRUE){
  # Rearrange the imput
  # NOTE: should use an object from qpcr_analyzer if it turns out to be a useful
  # function
  res.qPCR <- data.frame(cycles, process)
  colnames(res.qPCR) <- c("Cycles", "result")
  res.qPCR <- res.qPCR[order (res.qPCR[["Cycles"]]),]
  res.qPCR <- cbind(res.qPCR, cumsum(res.qPCR[,2]))
  colnames(res.qPCR) <- c("Cycles", "result")
  
  # do not know if this is correct, WIP
  # cycle.time should give the "average time" between the occurence of a 
  # positive reaction and another positive reaction
  dens.tmp <- dpcr_density(sum(res.qPCR[, 2]), nrow(res.qPCR), plot = FALSE)
  cycle.time <- exp(1/dens.tmp$k)
  # Determine probaility how often a events occur according to Poisson process 
  # with a certia rate per time frame (interval).
  # NuEvents is "number of expected events" within a time frame (interval)
  # dens.tmp$k gives the rate of the process according to dpcr_density()
  # delta is the difference "time (cycles) points" e.g., Cycle 18 and 25
  # mu is the expected number of events in defined interval (but this is somewhat
  # stupid since the intervals are discrete ... and so on)
  # cyc.occ gives the occurence in an interval
  mu <- dens.tmp$k * delta
  fact.NuEvents <- factorial(NuEvents)
  if (fact.NuEvents != "Inf") {
    cyc.occ <- (exp(-mu) * mu^NuEvents)/fact.NuEvents
  } else cyc.occ <- "to large"
  # END WIP
  # Plot the calculated qPCR data as Poisson processes
  if (plot) plot(res.qPCR[,1], res.qPCR[,3], xlim = c(mincyc, maxcyc), 
                 ylim = c(0, nrow(res.qPCR)), xlab = "Cycle", 
                 ylab = "lambda (cycle)", type = "S", lwd = 1.5)
  abline(h = nrow(res.qPCR) * 0.5, col = "grey")
  legend(mincyc,nrow(res.qPCR), c(paste0("Chambers", dens.tmp$n, sep = " "),
                                  paste0("Events", dens.tmp$k, sep = " "),
                                  paste0("mu", mu, sep = " "), 
                                  paste0("CT", cycle.time, sep = " "),
                                  paste0("CO", cyc.occ, sep = " ")))
  # Add rug to the the plot the illustrate the density of events
  if (rug) 
    rug(res.qPCR[,1])
  list(out = res.qPCR, mu = mu, CT = cycle.time, CO = cyc.occ)
}


# Example of an artificial chamber dPCR experiment using the test data set from
# qpcR. The function Cy0limiter is used to calculate the Cy0 value and converts 
# all values between a defined range to 1 and the remaining to 0.
Cy0limiter <- function(data = data, cyc = 1, fluo = NULL, 
                       Cq.range = c(NA, NA), model = l5) {
  if (is.null(fluo))
    fluo <- (1L:ncol(data))[-cyc]
    
  Cy0 <- vector()
  Cy0.res <- vector()
  pb <- txtProgressBar(min = 1, max = length(fluo), initial = 0, 
                       style = 3)
  for (i in fluo) {
    Cy0.tmp <- efficiency(pcrfit(data = data, cyc = cyc, fluo = i, 
                                 model = model), type = "Cy0", plot = FALSE)$Cy0
    Cy0 <- c(Cy0, Cy0.tmp)
    Cy0.res <- c(Cy0.res, ifelse(Cq.range[1] <= Cy0.tmp && Cy0.tmp <= Cq.range[2], 1, 0))
    setTxtProgressBar(pb, i)
  }
  data.frame(Cy0 = Cy0, result = Cy0.res)
}



