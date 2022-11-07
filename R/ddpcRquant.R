#' Quantify droplets
#'
#' Cluster raw data from QX100 and QX200 systems.
#'
#' @aliases ddpcRquant
#' @param path \code{character} path to diectory with raw data.
#' @param threshold.int \code{numeric} probability of the threshold quantile.
#' @param reps \code{numeric} vector representing the number of replications.
#' @param blocks \code{numeric} vector representing the number of blocks.
#' @param threshold.manual if \code{numeric}, the value is used as the threshold. If
#' \code{NULL}, the threshold is calculated automatically.
#' @export
#' @note This function is a modification of code was implemented using the code found on
#' \url{http://www.ddpcrquant.ugent.be/}.
#' @return \code{\linkS4class{dpcr}} object.
#' @author Wim Trypsteen, Matthijs Vynck, Jan De Neve, Pawel Bonczkowski, Maja Kiselinova,
#' Eva Malatinkova, Karen Vervisch, Olivier Thas, Linos Vandekerckhove, Ward De Spiegelaere.
#' @references Trypsteen W, Vynck M, De Neve J, Bonczkowski P, Kiselinova M,
#' Malatinkova E, Vervisch K, Thas O, Vandekerckhove L, De Spiegelaere W,
#' \emph{ddpcRquant: Threshold Determination for Single Channel Droplet Digital PCR
#' Experiments}. Analytical and Bioanalytical Chemistry 2015. 407(19): p.5827-34.
#' @importFrom evd qgev fgev


ddpcRquant <- function(path, threshold.int = 0.9995, reps = 10, blocks = 150, threshold.manual = NULL) {
  all_files <- list.files(path)
  csv_files <- all_files[grepl("csv$", all_files)]
  raw_data_files <- csv_files[grepl("Amplitude", all_files)]

  summary_file <- read.csv(
    file = paste0(path, "/", csv_files[!grepl("Amplitude", all_files)]),
    header = TRUE, row.names = NULL
  )
  summary_file[, c("Well", "Sample", "TypeAssay", "Assay")]

  df2dpcr(do.call(rbind, lapply(unique(summary_file[["Assay"]]), function(single_assay) {
    dat <- summary_file[summary_file[["Assay"]] == single_assay, c("Well", "Sample", "TypeAssay", "Assay")]
    channel <- as.numeric(substr(as.character(unique(dat[["TypeAssay"]])), 3, 3))

    ntc_wells <- as.character(dat[grepl("NTC", dat[["Sample"]], ignore.case = TRUE), "Well"])
    ntc_processed <- lapply(
      raw_data_files[sapply(ntc_wells, function(single_well) grep(single_well, raw_data_files))],
      function(single_file) {
        fluo_dat <- read.csv(file = paste0(path, "/", single_file), header = TRUE, row.names = NULL)[, channel]
        corr_factor <- hsm(fluo_dat)
        list(
          fluo_dat = fluo_dat - corr_factor,
          corr_factor = corr_factor
        )
      }
    )

    ntc_file <- unlist(lapply(ntc_processed, function(i) i[["fluo_dat"]]))

    # deviation from original script, since we introduce mean
    corr_factor <- mean(unlist(lapply(ntc_processed, function(i) i[["corr_factor"]])), na.rm = TRUE)

    quantgev <- sapply(1L:reps, function(dummy_variable) {
      ### block creation
      random_ntc_file <- sample(ntc_file)

      ### determine maxima of subsamples
      signal.maxima <- sapply(
        base::split(random_ntc_file, ceiling(seq_along(ntc_file) / (length(ntc_file) / blocks))),
        function(single_blocks) {
          max(single_blocks)
        }
      )

      ## fit the GEV model using ML
      droplet.fit <- try(fgev(signal.maxima), silent = TRUE)

      quantgev_value <- try(qgev(
        threshold.int, droplet.fit[["estimate"]][1],
        droplet.fit[["estimate"]][2], droplet.fit[["estimate"]][3]
      ), silent = TRUE)
      unname(ifelse(quantgev_value > max(random_ntc_file) + 3000, NA, quantgev_value))
    })

    threshold <- if (threshold.manual == FALSE) {
      # calculate final threshold
      mean(quantgev, na.rm = TRUE)
    } else {
      threshold.manual
    }

    sample_wells <- sapply(as.character(dat[["Well"]]), function(single_well) grep(single_well, raw_data_files))

    cbind(do.call(rbind, lapply(raw_data_files[sample_wells], function(single_file) {
      fluo_dat <- sample(read.csv(
        file = paste0(path, "/", single_file),
        header = TRUE, row.names = NULL
      )[, channel])
      window <- subset(fluo_dat, fluo_dat < threshold + corr_factor)
      window.mode <- hsm(window)

      random_sample_file_corr <- fluo_dat - ifelse(is.na(window.mode), 0, window.mode)
      data.frame( # file = single_file,
        k = sum(random_sample_file_corr > threshold),
        n = length(random_sample_file_corr),
        v = 0.91,
        uv = 0
      )
    })), assay = single_assay, experiment = dat[["Sample"]], replicate = 1)
  })))
}
