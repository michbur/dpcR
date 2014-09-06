#' Bind dpcr objects
#' 
#' A convinient wrapper around \code{\link[base]{cbind}} and
#' \code{\link[base]{rbind}} tailored specially for binding multiple objects
#' containing results from digital PCR experiments.
#' 
#' In case of \code{adpcr} or \code{ddpcr} objects, \code{bind_dpcr} can works
#' analogously to \code{\link[base]{cbind}}), but without recycling. In case on
#' unequal length, shorter objects will be filled in with additional \code{NA}
#' values. The original length is always preserved in \code{n} slot.
#' 
#' @docType methods
#' @name bind_dpcr-methods
#' @aliases bind_dpcr bind_dpcr-methods bind_dpcr,adpcr bind_dpcr,adpcr-method 
#' bind_dpcr,ddpcr bind_dpcr,ddpcr-method bind_dpcr,list bind_dpcr,list-method
#' @param input an object of class \code{\linkS4class{adpcr}} or
#' \code{\linkS4class{ddpcr}} or a list.
#' @param ...  objects of class \code{\linkS4class{adpcr}} or
#' \code{\linkS4class{ddpcr}}. See Details. If \code{input} is a list, ignored.
#' @return An object of class \code{\linkS4class{adpcr}} or
#' \code{\linkS4class{ddpcr}}, depending on the input.
#' @details \code{bind_dpcr} automatically names binded experiments using format
#' \code{x}.\code{y}, where \code{x} is number of object passed to function and
#' \code{y} is a number of experiment in a given object.
#' @note Because \code{bind_dpcr} calls \code{\link[base]{do.call}} function, 
#' binding together at the same time more than 500 objects can lead to 
#' 'stack overflow' error.
#' @author Michal Burdukiewicz
#' @seealso Opposite function: \code{\link{extract_dpcr}}
#' @keywords manip
#' @export
#' @include adpcr-class.R
#' @include ddpcr-class.R
#' @examples
#' 
#' bigger_array <- sim_adpcr(400, 765, 1000, pos_sums = FALSE, n_panels = 5)
#' smaller_array <- sim_adpcr(100, 700, 1000, pos_sums = FALSE, n_panels = 3)
#' bound_arrays <- bind_dpcr(bigger_array, smaller_array)
#' 
#' smaller_droplet <- sim_ddpcr(m = 7, n = 20, times = 5, n_exp = 2)
#' bigger_droplet <- sim_ddpcr(m = 15, n = 25, times = 5, n_exp = 4)
#' biggest_droplet <- sim_ddpcr(m = 15, n = 35, times = 5, n_exp = 1)
#' bound_droplets <- bind_dpcr(smaller_droplet, bigger_droplet, biggest_droplet)
bind_dpcr <- function (input, ...) {
  stop("Wrong class of 'input'")
}

setGeneric("bind_dpcr")

setMethod("bind_dpcr", 
          signature(input = "list"), 
          function(input, ...) {
            bind_dpcr(input[[1]], input[-1])
          })

setMethod("bind_dpcr", 
          signature(input = "adpcr"), 
          function(input, ...) {
            if(length(list(...)) != 1) {
              all_args <- c(list(input), Filter(Negate(is.null), list(...)))
            } else {
              if(is.list(...)) {
                all_args <- c(list(input), Filter(Negate(is.null), ...))
              } else {
                all_args <- c(list(input), Filter(Negate(is.null), list(...)))
              }
            }
              
            all_classes <- all(sapply(all_args, class) == "adpcr")
            if (!all_classes)
              stop("All binded objects must have the same class.")
            bigger_breaks <- which.max(lapply(all_args, function(single_arg) 
              max(slot(single_arg, "breaks"))))
            breaks <- slot(all_args[[bigger_breaks]], "breaks")
            res <- cbind_dpcr(all_args)
            create_adpcr(res[["binded_data"]], 
                         res[["n"]], breaks, type = res[["type"]])
          })


setMethod("bind_dpcr", 
          signature(input = "ddpcr"), 
          function(input, ...) {
            if(length(list(...)) != 1) {
              all_args <- c(list(input), Filter(Negate(is.null), list(...)))
            } else {
              if(is.list(...)) {
                all_args <- c(list(input), Filter(Negate(is.null), ...))
              } else {
                all_args <- c(list(input), Filter(Negate(is.null), list(...)))
              }
            }
            
            all_classes <- all(sapply(all_args, class) == "ddpcr")
            if (!all_classes)
              stop("All binded objects must have the same class.")
            if (slot(input, "type") == "fluo")
              stop("Binding method for fluorescence result not implemented.")
            
            bigger_thresholds <- which.max(lapply(all_args, function(single_arg) 
              max(slot(single_arg, "threshold"))))
            thresholds <- slot(all_args[[bigger_thresholds]], "threshold")
            res <- cbind_dpcr(all_args)
            create_adpcr(res[["binded_data"]], 
                         res[["n"]], thresholds, type = res[["type"]])
          })


#helper function for internal use only
cbind_dpcr <- function(all_args) {
  #check types
  all_types <- sapply(all_args, function(single_arg) 
    slot(single_arg, "type"))
  if (length(unique(all_types)) > 1)
    stop("Input objects must have the same type.")
  type <- unique(all_types)
  
  
  #check partitions and add NA values if needed
  all_partitions <- unlist(lapply(all_args, function(single_arg) 
    slot(single_arg, "n")))
  n_max <- max(all_partitions)
  if (length(unique(all_partitions)) > 1) {
    message("Different number of partitions. Shorter objects completed with NA values.")
    for(i in 1L:length(all_args)) {
      rows_to_add <- n_max - nrow(all_args[[i]])
      if (rows_to_add > 0)
        all_args[[i]] <- rbind(all_args[[i]], 
                           matrix(rep(NA, ncol(all_args[[i]])*rows_to_add), 
                                  nrow = rows_to_add))
    }
  }
  
  
  binded_data <- do.call(cbind, all_args)
  
  col_names <- unlist(lapply(1L:length(all_args), function(i)
    paste0(i, ".", 1L:ncol(all_args[[i]]))))
  
  colnames(binded_data) <- col_names
  list(binded_data = binded_data, type = type, n = all_partitions)
}
