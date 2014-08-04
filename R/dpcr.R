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
                    m.low = lower*n, m.up = upper*n, 
                    n = rep(n, 2), row.names = 1L:(length(l)*2))
  res
}




print_summary <- function(k, col_dat, type, n, print) {
  
  id <- 1L:col_dat 
  sums <- cbind(id = rep(id, 2), calc_lambda(k, n))
  sums <- sums[order(sums[,1]), ]
  rownames(sums) <- 1L:(2*col_dat)
  
  if (print) {
    k_print <- ifelse(col_dat < 5, k, 
                      paste0(paste0(k[1:4], collapse = (", ")), ", ..."))
    cat("\nNumber of positive partitions:", k_print, "\n")
    n_print <- ifelse(col_dat < 5, n, 
                      paste0(paste0(n[1:4], collapse = (", ")), ", ..."))
    cat("Total number of partitions:   ", n_print, "\n")
    cat("\n")
    print(head(sums, 20L), row.names = FALSE)
    if (col_dat > 20)
      cat(col_dat*2 - 20, "rows ommited.")
  }
  list(partitions = list(k = k, n = n), summary = sums)
  
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

# DATA MANIPULATION - droplet, array ------------------------------

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


#extract single panel from dpcr object
extract_dpcr <- function(input, id) {
  if (!(class(input) %in% c("adpcr", "ddpcr")))
    stop("Input must have 'adpcr' or 'ddpcr' class.")
  selected <- input[, id]
  
  #because when id is single negative value, usually the
  #result has more than one column
  if (length(id) == 1 && id > 0) {
    selected <- matrix(selected, ncol = 1)
    colnames(selected) <- colnames(input)[id]
  }
  result <- input
  slot(result, ".Data") <- selected
  slot(result, "n") <- slot(input, "n")[id]
  result
}


bind_dpcr <- function (input, ...) {
  stop("Wrong class of 'input'.")
}


setMethod("bind_dpcr", 
          signature(input = "adpcr"), 
          function(input, ...) {
            args <- c(list(input), Filter(Negate(is.null), list(...)))
            all_classes <- all(sapply(args, class) == "adpcr")
            if (!all_classes)
              stop("All binded objects must have the same class.")
            bigger_breaks <- which.max(lapply(args, function(single_arg) 
              max(slot(single_arg, "breaks"))))
            breaks <- slot(args[[bigger_breaks]], "breaks")
            res <- cbind_dpcr(args)
            create_adpcr(res[["binded_data"]], 
                         res[["n"]], breaks, type = res[["type"]])
          })


setMethod("bind_dpcr", 
          signature(input = "ddpcr"), 
          function(input, ...) {
            args <- c(list(input), Filter(Negate(is.null), list(...)))
            all_classes <- all(sapply(args, class) == "ddpcr")
            if (!all_classes)
              stop("All binded objects must have the same class.")
            if (slot(input, "type") == "fluo")
              stop("Binding method for fluorescence result not implemented.")
            
            bigger_thresholds <- which.max(lapply(args, function(single_arg) 
              max(slot(single_arg, "threshold"))))
            thresholds <- slot(args[[bigger_thresholds]], "threshold")
            res <- cbind_dpcr(args)
            create_adpcr(res[["binded_data"]], 
                         res[["n"]], thresholds, type = res[["type"]])
          })

#helper function for internal use only
cbind_dpcr <- function(args) {
  #check types
  all_types <- sapply(args, function(single_arg) 
    slot(single_arg, "type"))
  if (length(unique(all_types)) > 1)
    stop("Input objects must have the same type.")
  type <- unique(all_types)
  
  
  #check partitions and add NA values if needed
  all_partitions <- unlist(lapply(args, function(single_arg) 
    slot(single_arg, "n")))
  n_max <- max(all_partitions)
  if (length(unique(all_partitions)) > 1) {
    message("Different number of partitions. Shorter objects completed with NA values.")
    for(i in 1L:length(args)) {
      rows_to_add <- n_max - nrow(args[[i]])
      if (rows_to_add > 0)
        args[[i]] <- rbind(args[[i]], 
                           matrix(rep(NA, ncol(args[[i]])*rows_to_add), 
                                  nrow = rows_to_add))
    }
  }
  
  
  binded_data <- do.call(cbind, args)
  
  col_names <- unlist(lapply(1L:length(args), function(i)
    paste0(i, ".", 1L:ncol(args[[i]]))))
  
  colnames(binded_data) <- col_names
  list(binded_data = binded_data, type = type, n = all_partitions)
}


# TESTS - droplet, array ------------------------------

#a wrapper around rateratio.test
test_ratio <- function(dpcr1, dpcr2, 
                       alternative = c("two.sided", "less", "greater"), 
                       conf.level = 0.95) {
  if (length(dpcr1) != 2) {
    n_x <- length(dpcr1)
    k_x <- sum(dpcr1 > 0)
  } else {
    n_x <- dpcr1[2]
    k_x <- dpcr1[1]
  }
  if (length(dpcr2) != 2) {
    n_y <- length(dpcr2)
    k_y <- sum(dpcr2 > 0)
  } else {
    n_y <- dpcr2[2]
    k_y <- dpcr2[1]
  }
  
  test_res <- rateratio.test(c(k_x, k_y), c(n_x, n_y), RR = 1, alternative = alternative, 
                             conf.level = conf.level)
  test_res[["data.name"]] <- paste0("dPCR 1: positive partitions: ", k_x, "; total partitions: ",
                                    n_x, ".\n       dPCR 2: positive partitions: ", k_y, 
                                    "; total partitions: ", n_y, ".")
  test_res[["estimate"]] <- c(calc_lambda(k_x, n_x)[1,2], calc_lambda(k_y, n_y)[1,2],
                              test_res[["estimate"]][1])
  test_res[["null.value"]] <- NULL
  names(test_res[["estimate"]]) <- c("Lambda1", "Lambda2", "Lambda1/Lambda2")
  test_res
}

setMethod("test_ratio", 
          signature(dpcr1 = "adpcr", dpcr2 = "adpcr"), 
          function(dpcr1, dpcr2, alternative = c("two.sided", "less", "greater"), 
                   conf.level = 0.95) {
            if(ncol(dpcr1) != 1 || ncol(dpcr2) != 1)
              stop("Both 'dpcr1' and 'dpcr2' must contain only one experiment.", 
                   call. = TRUE)
            if(!all(c(slot(dpcr1, "type"), slot(dpcr2, "type")) %in% c("nm", "tp")))
              stop("Both 'dpcr1' and 'dpcr2' must have type 'nm' or 'tp'", 
                   call. = TRUE)
            n_x <- slot(dpcr1, "n")
            n_y <- slot(dpcr2, "n")
            k_x <- ifelse(slot(dpcr1, "type") == "nm", sum(dpcr1 > 0), dpcr1[1])
            k_y <- ifelse(slot(dpcr2, "type") == "nm", sum(dpcr2 > 0), dpcr2[1])
            test_ratio(c(k_x, n_x), c(k_y, n_y), alternative = alternative, 
                       conf.level = conf.level)  
          })

setMethod("test_ratio", 
          signature(dpcr1 = "ddpcr", dpcr2 = "ddpcr"), 
          function(dpcr1, dpcr2, alternative = c("two.sided", "less", "greater"), 
                   conf.level = 0.95) {
            if(ncol(dpcr1) != 1 || ncol(dpcr2) != 1)
              stop("Both 'dpcr1' and 'dpcr2' must contain only one experiment.", 
                   call. = TRUE)
            if(!all(c(slot(dpcr1, "type"), slot(dpcr2, "type")) %in% c("nm", "tp")))
              stop("Both 'dpcr1' and 'dpcr2' must have type 'nm' or 'tp'", 
                   call. = TRUE)
            n_x <- slot(dpcr1, "n")
            n_y <- slot(dpcr2, "n")
            k_x <- ifelse(slot(dpcr1, "type") == "nm", sum(dpcr1 > 0), dpcr1[1])
            k_y <- ifelse(slot(dpcr2, "type") == "nm", sum(dpcr2 > 0), dpcr2[1])
            test_ratio(c(k_x, n_x), c(k_y, n_y), alternative = alternative, 
                       conf.level = conf.level)  
          })


#a wrapper around rateratio.test
# test_experiments <- function(dpcr_experiments) {
#   m_dat <- melt(dpcr_experiments)
#   colnames(m_dat)[1L:2] <- c("partition", "experiment")
#   m_dat[["experiment"]] <- factor(m_dat[["experiment"]])
#   
#   glm_fit <- glm(value ~ experiment, data = m_dat, family = quasipoisson)
#   
#   tuk <- glht(glm_fit, linfct = mcp(experiment = "Tukey"))
#   cld(tuk)
# }