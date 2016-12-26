#extract single panel from dpcr object


#' Extract Digital PCR Run
#' 
#' Extract runs from a code{\linkS4class{dpcr}} object while preserving all other
#' attributes.
#' 
#' The \code{extract_run} function allows to choose one or more panels from an
#' object of the \code{\linkS4class{adpcr}} or \code{\linkS4class{dpcr}} class
#' and save it without changing other attributes. It is the most recommended
#' method of extracting a subset from an array of panels, because it preserves
#' class and structure of the object in contrary to standard operator
#' \link[base]{Extract}.
#' 
#' @param input object of the class \code{\linkS4class{adpcr}} or
#' \code{\linkS4class{dpcr}}.
#' @param id vector of indices or names of runs.
#' @return The object of the input's class (\code{\linkS4class{adpcr}} or
#' \code{\linkS4class{dpcr}}).
#' @note The standard \code{\link[base]{Extract}} operator \code{x[i]} treats
#' dpcr objects as \code{matrix} and extracts values without preserving other
#' attributies of the object.
#' @author Michal Burdukiewicz.
#' @seealso Opposite function: \code{\link{bind_dpcr}}
#' Extract multiple runs belonging to an experiment of assay: 
#' \code{\link{extract_dpcr}}
#' @keywords manip extract panel
#' @examples
#' 
#' #sample extracting
#' panels <- sim_adpcr(10, 40, 1000, pos_sums = FALSE, n_panels = 50)
#' single_panel <- extract_run(panels, 5)
#' random_three <- extract_run(panels, sample.int(ncol(panels), 3))
#' all_but_one <- extract_run(panels, -5)
#' 
#' #the same for fluorescence data
#' fluos <- sim_dpcr(10, 40, 1000, pos_sums = FALSE, n_exp = 50, 
#'                    fluo = list(0.1, 0))
#' single_fluo <- extract_run(fluos, 5)
#' 
#' 
#' @export extract_run
extract_run <- function(input, id) {
  if (!(class(input) %in% c("adpcr", "dpcr")))
    stop("Input must have 'adpcr' or 'dpcr' class")
  
  #when id is a column name
  if(!is.numeric(id)) 
    id <- which(colnames(input) %in% id) 

  selected <- input[, id, drop = FALSE]
  
  #because when id is single negative value, usually the
  #result has more than one column
  if (length(id) == 1 && id > 0) {
    selected <- matrix(selected, ncol = 1)
    colnames(selected) <- colnames(input)[id]
  }
  
  result <- input
  slot(result, ".Data") <- selected
  slot(result, "n") <- slot(input, "n")[id]
  slot(result, "v") <- slot(input, "v")[id]
  slot(result, "uv") <- slot(input, "uv")[id]
  slot(result, "exper") <- droplevels(slot(input, "exper")[id])
  slot(result, "replicate") <- droplevels(slot(input, "replicate")[id])
  slot(result, "assay") <- droplevels(slot(input, "assay")[id])
  
  
  #in case of tnp type extract also columns names
  if (class(input) == "adpcr") {
    slot(result, "panel_id") <- droplevels(slot(input, "panel_id")[id])
    if (slot(input, "type") == "tnp") {
      slot(result, "col_names") <- slot(input, "col_names")[id]
      slot(result, "row_id") <- slot(input, "row_id")[id]
      slot(result, "col_id") <- slot(input, "col_id")[id]
    }
  }
  
  result
}


#' Extract Assays or Experiments
#' 
#' Extract all runs belonging to specific assay or experiment(s) from a 
#' code{\linkS4class{dpcr}} object while preserving all other
#' attributes.
#' 
#' @param input object of the class \code{\linkS4class{adpcr}} or
#' \code{\linkS4class{dpcr}}.
#' @param id_exper vector of indices or names of experiments. Must be \code{NULL} if
#' \code{id_assay} is specified.
#' @param id_assay vector of indices or names of assays. Must be \code{NULL} if
#' \code{id_exper} is specified.
#' @return The object of the input's class (\code{\linkS4class{adpcr}} or
#' \code{\linkS4class{dpcr}}).
#' @note The standard \code{\link[base]{Extract}} operator \code{x[i]} treats
#' dpcr objects as \code{matrix} and extracts values without preserving other
#' attributies of the object.
#' @author Michal Burdukiewicz.
#' @seealso Extract run(s): \code{\link{extract_run}}.
#' @keywords manip extract panel
#' @examples
#' 
#' #extract using only experiment's ID
#' extract_dpcr(six_panels, id_exper = 1)
#' 
#' #extract using assay name
#' extract_dpcr(six_panels, id_assay = "MYC")
#' 
#' #extract using multiple names
#' extract_dpcr(six_panels, id_exper = c("Experiment1", "Experiment2"))
#' 
#' @export extract_dpcr
extract_dpcr <- function(input, id_exper = NULL, id_assay = NULL) {
  if (!(class(input) %in% c("adpcr", "dpcr")))
    stop("Input must have 'adpcr' or 'dpcr' class.")
  
  if (is.null(id_exper) && is.null(id_assay))
    stop("One of id_exper or id_assay must be NULL.")
  
  if (!is.null(id_exper) && !is.null(id_assay))
    stop("One of id_exper or id_assay must not be NULL.")
  
  run_ids <- if(!is.null(id_exper)) {
    get_runs_ids(input, "exper", id_exper)
  } else {
    get_runs_ids(input, "assay", id_assay)
  }
  
  extract_run(input, which(run_ids))
}


get_runs_ids <- function(x, slot_name, id) {
  if(is.numeric(id)) 
    id <- slot(x, slot_name)[id]
  
  slot(x, slot_name) %in% id
}