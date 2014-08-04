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
  create_ddpcr(res, rep(n, n_exp), threshold = 0.5, type = type)
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

# helper function counting peaks, takes threshold
get_k_n <- function(x, threshold = 1) {
  #   if (is.null(threshold)) {
  #     x <- x[x != 0]
  #     threshold <- median(x)
  #   }
  peaks <- findpeaks(x, threshold = threshold)
  pos <- nrow(peaks)
  if (is.null(pos))
    pos <- 0
  # list(k = pos, n = nrow(findpeaks(x, threshold = min(x))), thr = threshold)
  pos
}

# helper function plotting colorful circles below the vic/fam
plot_vf_circ <- function(x, y, radius, bg) {
  symbols(x = x, y = rep(y, length(x)), circles = rep(radius, length(x)), 
          bg = bg, fg = "black", inches = FALSE, add = TRUE)
}

plot_vic_fam <- function(vic, fam, col_vic = "green", col_fam = "blue", circle = TRUE) {
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
  
  
  #circle drawing
  if(circle == TRUE) 
    radius <- (y_max_lim - y_min_lim)*5
  if(is.numeric(circle)) {
    if(length(circle) == 1) {
      radius <- circle
    } else {
      stop("If 'circle' has type 'numeric', it must have length 1.", call. = TRUE, domain = NA)
    }
  }
  
  for (i in circ)
    lines(c(i, i), c(0, y_points2))
  
  if (circle != FALSE) {
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
}

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