#' Plot Panel
#' 
#' The \code{plot_panel} function takes objects of the class
#' \code{\linkS4class{adpcr}} to enable customizable graphical representations
#' of a chamber-based digital PCR experiments (e.g., Digital Array (R) IFCs
#' (integrated fluidic circuits) of the BioMark (R) and EP1 (R)).
#' 
#' Currently only objects containing just one column of data (one panel) can be
#' plotted (see Examples how easily plot multipanel objects). Moreover the
#' object must contain fluorescence intensities or exact number of molecules or
#' the positive hits derived from the Cq values for each well. The Cq values
#' can be obtained by custom made functions (see example in
#' \code{\link{dpcr_density}})) or the yet to implement "qpcr_analyser function
#' from the dpcR package.
#' 
#' If the \code{col} argument has length one, a color is assigned for each
#' interval of the input, with the brightest colors for the lowest values.
#' 
#' @param input object of the \code{\linkS4class{adpcr}} class. See Details.
#' @param nx_a Number of columns in a plate.
#' @param ny_a Number of rows in a plate.
#' @param col A single color or vector of colors for each level of input.
#' @param legend If \code{TRUE}, a built-in legend is added to the plot.
#' @param half If \code{left} or \code{right}, every well is represented only
#' by the adequate half of the rectangle.
#' @param use_breaks if \code{TRUE}, input is cutted into intervals using 
#' \code{breaks} slot. If \code{FALSE}, input is converted to factor using
#' \code{\link[base]{as.factor}}. Ignored if data has \code{"tp"} type (see 
#' possible types of \code{\linkS4class{adpcr}} objects).
#' @param ... Arguments to be passed to \code{plot} function.
#' @return Invisibly returns a list of coordinates of each microfluidic well 
#' and an assigned color.
#' @author Michal Burdukiewicz, Stefan Roediger.
#' @seealso \code{\link{extract_dpcr}}.
#' @keywords hplot
#' @examples
#' 
#' # Create a sample dPCR experiment with 765 elements (~> virtual compartments)   
#' # of target molecule copies per compartment as integer numbers (0,1,2)
#' ttest <- sim_adpcr(m = 400, n = 765, times = 20, pos_sums = FALSE, 
#'                    n_panels = 1)
#' # Plot the dPCR experiment results with default settings
#' plot_panel(ttest, nx_a = 45, ny_a = 17)
#' 
#' #do it without breaks
#' plot_panel(ttest, nx_a = 45, ny_a = 17, use_breaks = FALSE)
#' 
#' # Apply a binary color code with blue as positive
#' slot(ttest, "breaks") <- c(0, 2, 4)
#' plot_panel(ttest, nx_a = 45, ny_a = 17, col = "blue")
#' 
#' # Apply a two color code for number of copies per compartment
#' plot_panel(ttest, nx_a = 45, ny_a = 17, col = c("blue", "red"))
#' 
#' 
#' 
#' # supply customized breaks and compare
#' par(mfcol = c(2, 1))
#' plot_panel(ttest, nx_a = 45, ny_a = 17)
#' slot(ttest, "breaks") <- c(0, 1, 2, (max(slot(ttest, "breaks")) + 1))
#' plot_panel(ttest, nx_a = 45, ny_a = 17)
#' par(mfcol = c(1, 1))
#' 
#' # plot few panels
#' ttest2 <- sim_adpcr(m = 400, n = 765, times = 40, pos_sums = FALSE, 
#'                     n_panels = 4)
#' par(mfcol = c(2, 2))
#' four_panels <- lapply(1:ncol(ttest2), function(i) 
#'        plot_panel(extract_dpcr(ttest2, i), nx_a = 45, ny_a = 17, legend = FALSE, 
#'          main = paste("Panel", LETTERS[i], sep = " ")))
#' par(mfcol = c(1, 1))
#' 
#' # two different channels 
#' plot_panel(extract_dpcr(ttest2, 1), nx_a = 45, ny_a = 17, legend = FALSE, 
#'            half = "left")
#' par(new = TRUE)
#' plot_panel(extract_dpcr(ttest2, 2), nx_a = 45, ny_a = 17, col = "blue", 
#'            legend = FALSE, half = "right")
#' 
#' # plot two panels with every well as only the half of the rectangle
#' ttest3 <- sim_adpcr(m = 400, n = 765, times = 40, pos_sums = FALSE, 
#'                     n_panels = 2)
#' par(mfcol = c(1, 2))
#' two_panels <- lapply(1:ncol(ttest3), function(i) 
#'        plot_panel(extract_dpcr(ttest3, i), nx_a = 45, ny_a = 17, legend = FALSE, 
#'          main = paste("Panel", LETTERS[i], sep = " ")))
#' par(mfcol = c(1, 1))
#' 
#' @export plot_panel
plot_panel <- function(input, nx_a, ny_a, col = "red", legend = TRUE, 
                       half = "none", use_breaks = TRUE, ...) {  
  if (class(input) == "adpcr") {
    if (!(slot(input, "type") %in% c("nm", "np", "ct")))
      stop("Input must contain data of type 'nm', 'np' or 'ct'.", 
           call. = TRUE, domain = NA) 
    if (ncol(input) > 1)
      stop("Input must contain only one panel.", call. = TRUE, domain = NA)    
    if (nrow(input) == 1)
      stop("Input cannot contain total number of positive chambers.", call. = TRUE, 
           domain = NA)    
  } else {
    stop("Input must have the 'adpcr' class", call. = TRUE, domain = NA)
  }
  if (slot(input, "n") != nx_a * ny_a)
    stop (paste0("Can not process with plot since the input 
                 length (", slot(input, "n"),
                 ") differs from the size of nx_a * ny_a (", nx_a * ny_a, ")."))
  
  if (slot(input, "type") == "np")
    use_breaks = FALSE
  
  # Use breaks points to split input 
  if(use_breaks) {
    cutted_input <- cut(slot(input, ".Data"), breaks = slot(input, "breaks"), 
                        include.lowest = TRUE, right = FALSE)
  } else {
    cutted_input <- factor(slot(input, ".Data"))
  }
  
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
           the input.")    
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
