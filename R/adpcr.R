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
  
  if (type %in% c("fluo", "ct")) 
    stop(paste0("Summary not currently implemented for data type ", type, "."), call. = TRUE, domain = NA)
  
  if (type %in% c("nm", "tp")) {
    k <- colSums(data > 0)
  }
  
  invisible(print_summary(k, col_dat, type, n, print))
})

# Special method declared to hide slots other than .Data
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
           colnames(res) <- unlist(lapply(1L:(n_cols * 2), function(i) 
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



test_panel <- function(X, nx_a, ny_a, nx = 5, ny = 5, alternative = c("two.sided", "regular", "clustered"), 
                       method = c("Chisq", "MonteCarlo"), conditional = TRUE, nsim = 1999) {
  ppp_data <- adpcr2ppp(X, nx_a, ny_a)
  lapply(ppp_data, function(single_panel)
    quadrat.test(single_panel, nx, ny, alternative, method, conditional, nsim = 1999))
}

# SIMULATIONS - array ---------------------------------------------

sim_adpcr <- function(m, n, times, n_panels = 1, dube = FALSE, pos_sums = FALSE) {
  n <- t.int(n)
  res <- sim_dpcr(m, n, times, dube, pos_sums, n_panels)
  create_adpcr(res, n, 0L:max(res), type = ifelse(pos_sums, "tp", "nm"))
}

# OTHER FUNCTIONS - array ------------------------------------------

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
  if (length(input) != nx_a * ny_a)
    stop (paste0("Can not process with plot since the input 
                 legnth (", length(input) ,
                 ") differs from the size of nx_a * ny_a (", nx_a * ny_a, ").
                 \n Change nx_a * ny_a to have the same number of elements."))
  
  # Use breaks points to split input 
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

# Function for all cases when we need breaks calculated
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
  # Pre-processing:
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

adpcr2ppp <- function(input, nx_a, ny_a) {
  if (class(input) != "adpcr")
    stop("Input must have 'adpcr' class", call. = TRUE, domain = NA)
  
  array_data <- slot(input, ".Data")
  nrow_array <- nrow(array_data)
  
  if (nrow_array != nx_a * ny_a)
    stop (paste0("Can not process with conversion since the input 
                 legnth (", length(input) ,
                 ") differs from the size of nx_a * ny_a (", nx_a * ny_a, ").
                 \n Change nx_a * ny_a to have the same number of elements."))  
  
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