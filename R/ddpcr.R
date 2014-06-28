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