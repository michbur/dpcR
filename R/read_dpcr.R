#' Read digital PCR data
#'
#' Reads digital PCR data in various formats.
#'
#' @param input name of the input file (\code{character}) or input object
#' (\code{data.frame}).
#' @param format of the file, for example: \code{"raw"}, \code{"QX100"},
#' \code{"BioMark"}, \code{"amp"} (raw amplitude compressed using \code{.zip}).
#' @param ext extension of the file ().
#' @param ... additional parameters for the appropriate function. For example, if
#' \code{format} has value \code{"raw"}, the additional parameter must be
#' \code{adpcr}.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @details Input files may have .csv, .xls or .xlsx extension. In case of Excel files
#' with multiple sheets, only the first sheet will be analyzed.
#' @return Always an object of \code{\linkS4class{adpcr}} or
#' \code{\linkS4class{dpcr}} type.
#' @export
#' @keywords utilities
#' @seealso
#' Read raw format files: \code{\link{read_redf}}.
#' Read BioMark format files: \code{\link{read_BioMark}}.
#' Read QX100 format files: \code{\link{read_QX100}}.
#' Read QX200 format files: \code{\link{read_QX200}}.

read_dpcr <- function(input, format, ext = NULL, ...) {
  if (!(format %in% c("redf", "QX100", "QX200", "BioMark", "amp"))) {
    stop("Unknown value of 'format' parameter.")
  }

  switch(format,
    redf = read_redf(input, ext = ext, ...),
    QX100 = read_QX100(input, ext = ext),
    QX200 = read_QX200(input, ext = ext),
    BioMark = read_BioMark(input, ext = ext, ...),
    amp = read_amp(input, ext = ext, ...)
  )
}


#' Read digital PCR amplitude raw data
#'
#' Reads digital PCR amplitude data.
#'
#' @details The amplitude data means a compressed directory of
#' amplification.
#' @inheritParams create_dpcr
#' @inheritParams read_dpcr
#' @return An object of \code{\linkS4class{adpcr}}.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @keywords utilities
#' @export

read_amp <- function(input, ext = NULL) {
  amp_data <- read_zipped_amps(input)
  amp2dpcr(amp_data)
}

#' Read digital PCR raw data
#'
#' Reads REDF (Raw Exchange Digital PCR format) data.

#' @inheritParams create_dpcr
#' @inheritParams read_dpcr
#' @details REDF (Raw Exchange Digital PCR format) data is preferably a .csv file
#' with following columns:
#' \describe{
#' \item{experiment}{names of experiments}
#' \item{replicate}{indices of replicates}
#' \item{assay}{names of assays}
#' \item{k}{number of positive partitions}
#' \item{n}{total number of partitions}
#' \item{v}{volume of partition (nL)}
#' \item{uv}{uncertainty of partition's volume (nL)}
#' \item{threshold}{partitions with \code{k} equal or higher than threshold are
#' treated as positve.}
#' \item{panel_id}{indices of panels}
#' }
#' Column \code{panel_id} should be specified only in case of
#' array-based dPCR.
#' @return An object of \code{\linkS4class{adpcr}} or \code{\linkS4class{dpcr}} type,
#' depends on the value of \code{adpcr} parameter.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @keywords utilities
#' @export

read_redf <- function(input, ext = NULL) {
  dat <- read_input(input, ext)

  # n <- rowSums(!apply(dat, 1, is.na))
  #
  # exp_rep <- matrix(unlist(strsplit(colnames(dat), ".", fixed = TRUE)), ncol = 2, byrow = TRUE)
  #
  # create_dpcr(data = as.matrix(dat), n = n, exper = exp_rep[, 1], replicate = exp_rep[, 2], type = "np",
  #             adpcr = adpcr)
  df2dpcr(dat)
}


#' Read QX100
#'
#' Reads digital PCR data from the QX100 Droplet Digital PCR System (Bio-Rad).
#'
#' @inheritParams read_dpcr
#' @author Michal Burdukiewcz, Stefan Roediger
#' @seealso See \code{\link{read_dpcr}} for detailed description of input files.
#'
#' Example of QX100 data: \code{\link{pds}}.
#' @return An object of \code{\linkS4class{adpcr}} class.
#' @note The volume and its uncertainty are taken from the literature (see
#' references).
#' @references
#' Corbisier, P. et al (2015). DNA copy number concentration measured by digital
#' and droplet digital quantitative PCR using certified reference materials.
#' Analytical and Bioanalytical Chemistry 407, 1831-1840.
#' @keywords utilities
#' @export

read_QX100 <- function(input, ext = NULL) {
  dat <- read_input(input, ext)

  n <- dat[["AcceptedDroplets"]]
  counts <- matrix(dat[["Positives"]], nrow = 1)
  well <- as.character(dat[["Well"]])
  exper <- dat[["Experiment"]]
  replicate <- paste0(well, ".", dat[["Sample"]])

  assay <- if (is.null(dat[["Assay"]])) {
    dat[["TargetType"]]
  } else {
    dat[["Assay"]]
  }

  # ids of panels
  row_id <- as.numeric(substr(well, nchar(well) - 1, nchar(well)))
  col_id <- substr(well, 0, 1)
  col_names <- 1L:8
  names(col_names) <- LETTERS[1L:8]

  create_adpcr(
    data = matrix(dat[["Positives"]], nrow = 1), n = n,
    exper = exper, replicate = replicate, type = "tnp",
    assay = assay, v = 0.834, uv = 0.017,
    col_names = names(col_names),
    row_names = as.character(1L:12),
    row_id = row_id,
    col_id = col_names[col_id],
    panel_id = as.factor(assay), threshold = 1
  )
}


#' Read QX200
#'
#' Reads digital PCR data from the QX200 Droplet Digital PCR System (Bio-Rad).
#'
#' @inheritParams read_dpcr
#' @author Michal Burdukiewcz, Stefan Roediger
#' @seealso See \code{\link{read_dpcr}} for detailed description of input files.
#'
#' @return An object of \code{\linkS4class{adpcr}} class.
#' @source Droplet Digital PCR Applications Guide, Rev. A, Bulletin 6407, Biorad,
#' accessed on 28.10.2016,
#' \url{http://www.bio-rad.com/webroot/web/pdf/lsr/literature/Bulletin_6407.pdf}.
#' @note The volume and its uncertainty are taken from the literature (see
#' references).
#' @references
#' Corbisier, P. et al (2015). DNA copy number concentration measured by digital
#' and droplet digital quantitative PCR using certified reference materials.
#' Analytical and Bioanalytical Chemistry 407, 1831-1840.
#' @keywords utilities
#' @export

read_QX200 <- function(input, ext = NULL) {
  dat <- read_input(input, ext)

  n <- dat[["AcceptedDroplets"]]
  counts <- matrix(dat[["Positives"]], nrow = 1)
  exper <- dat[["Experiment"]]
  replicate <- dat[["Sample"]]
  well <- as.character(dat[["Well"]])

  if (all(is.na(replicate))) {
    all_reps <- as.vector(table(dat[["TargetType"]], exper))
    replicate <- unlist(lapply(all_reps, function(single_rep) 1L:single_rep))
  }

  # ids of panels
  row_id <- as.numeric(substr(well, nchar(well) - 1, nchar(well)))
  col_id <- substr(well, 0, 1)
  col_names <- 1L:8
  names(col_names) <- LETTERS[1L:8]

  create_adpcr(
    data = matrix(dat[["Positives"]], nrow = 1), n = n,
    exper = exper, replicate = replicate, type = "tnp",
    assay = dat[["TargetType"]], v = 0.85, uv = 0.017,
    col_names = LETTERS[1L:8],
    row_names = as.character(1L:12),
    row_id = row_id,
    col_id = col_names[col_id],
    panel_id = as.factor(dat[["TargetType"]]), threshold = 1
  )
}

#' Read BioMark
#'
#' Reads digital PCR data from the BioMark (Fluidigm).
#'
#' @inheritParams read_dpcr
#' @param detailed logical, if \code{TRUE}, the input file is processed as if it was
#' 'Detailed Table Results'. In the other case, the expected input file structure is
#' 'Summary Table Results'.
#' @author Michal Burdukiewcz, Stefan Roediger
#' @return An object of \code{\linkS4class{adpcr}} class.
#' @seealso See \code{\link{read_dpcr}} for detailed description of input files.
#' @references
#' Dong, L. et al (2015). Comparison of four digital PCR platforms for accurate
#' quantification of DNA copy number of a certified plasmid DNA reference material.
#' Scientific Reports. 2015;5:13174.
#' @keywords utilities
#' @export

read_BioMark <- function(input, ext = NULL, detailed = FALSE) {
  if (detailed) {
    dat <- read_input(input, ext, skip = 11)

    exper <- rep(paste(
      as.character(sapply(0L:47, function(id_panel) {
        dat[770 * id_panel + 1, "Name"]
      })),
      as.character(sapply(0L:47, function(id_panel) {
        dat[770 * id_panel + 1, "Type"]
      })),
      sep = "_"
    ), 2)

    wells <- t(sapply(strsplit(levels(dat[["Chamber.ID"]]), "-", fixed = TRUE), function(i) {
      c(
        x = as.numeric(substr(i[[2]], 2, 3)),
        y = as.numeric(substr(i[[3]], 2, 3))
      )
    }))

    replicate <- c(as.character(sapply(0L:47, function(id_panel) {
      dat[770 * id_panel + 1, "Type.1"]
    })), as.character(sapply(0L:47, function(id_panel) {
      dat[770 * id_panel + 1, "Type.2"]
    })))

    replicate[replicate == "Test"] <- paste(replicate[replicate == "Test"],
      1L:length(replicate[replicate == "Test"]),
      sep = "_"
    )
    replicate[replicate == "Blank"] <- paste(replicate[replicate == "Blank"],
      1L:length(replicate[replicate == "Blank"]),
      sep = "_"
    )

    assay <- c(as.character(sapply(0L:47, function(id_panel) {
      dat[770 * id_panel + 1, "Name.1"]
    })), as.character(sapply(0L:47, function(id_panel) {
      dat[770 * id_panel + 1, "Name.2"]
    })))

    run_dat <- do.call(cbind, lapply(c("Target", "Target.1"), function(single_assay) {
      do.call(cbind, lapply(0L:47, function(id_panel) {
        do.call(rbind, lapply(1L:70, function(id_x) {
          # matrix(..., ncol = 1) instead of [,,drop = FALSE], because I use as.numeric
          matrix(as.numeric(dat[770 * id_panel + id_x + rev(0L:10 * 70), single_assay] == "Hit"), ncol = 1)
        }))
      }))
    }))

    create_adpcr(run_dat, 770L,
      exper = exper,
      replicate = replicate,
      col_names = as.character(1L:70),
      row_names = as.character(1L:11),
      type = "np",
      panel_id = as.factor(1L:96),
      row_id = wells[, "y"],
      col_id = wells[, "x"],
      v = 0.85, uv = 0.00595, threshold = 1
    )
  } else {
    dat <- data.frame(read_input(input, ext))

    data_range <- dat[-c(1L:9), ]

    # dat[apply(dat, 1, function(row) sum(is.na(row))) == 0, ]

    names1 <- as.vector(dat[8, ])
    names2 <- as.vector(dat[9, ])

    # exper
    exper <- rep(paste0(
      data_range[, which(names1 == "Sample Information" & names2 == "Name")], "_",
      data_range[, which(names1 == "Sample Information" & names2 == "Type")]
    ), 2)

    # replicate
    replicate <- paste0(
      unlist(lapply(c("VIC-TAMRA", "FAM-MGB"), function(channel_name) {
        data_range[, names1 == channel_name & names2 == "Type"]
      })),
      rep(data_range[, names1 == "Panel" & names2 == "ID"], 2)
    )

    # dat[data_range, names1 == "Sample Information" & names2 == "rConc."]

    # assay
    assay <- unlist(lapply(c("VIC-TAMRA", "FAM-MGB"), function(channel_name) {
      data_range[, names1 == channel_name & names2 == "Name"]
    }))

    # data
    count_data <- unlist(lapply(c("VIC-TAMRA", "FAM-MGB"), function(channel_name) {
      as.numeric(data_range[, names1 == channel_name & names2 == "Count"])
    }))

    res <- create_adpcr(
      data = matrix(count_data, nrow = 1), n = rep(765, length(count_data)),
      exper = exper, replicate = replicate, type = "tnp",
      assay = assay, row_names = as.character(1L:4),
      col_names = as.character(1L:12),
      panel_id = factor(c(rep(1, length(exper) / 2), rep(2, length(exper) / 2))),
      threshold = 1, v = 0.85, uv = 0.00595
    )

    names_df <- data.frame(table(slot(res, "panel_id"), slot(res, "assay")))
    levels(slot(res, "panel_id")) <- as.character(sapply(
      levels(names_df[["Var1"]]),
      function(single_name) {
        sub_data <- names_df[names_df[["Var1"]] == single_name, ]
        sub_data[which.max(sub_data[["Freq"]]), "Var2"]
      }
    ))
    res
  }
}


# checks the extension and returns proper read function
read_input <- function(input, ext = NULL, skip = 0) {
  if (is.character(input)) {
    if (is.null(ext)) {
      ext <- strsplit(input, ".", fixed = TRUE)[[1]]
    }

    # maybe add multisheet excel

    fun <- switch(ext[[length(ext)]],
      csv = read.csv,
      xls = read_excel,
      xlsx = read_excel,
      zip = read_zipped_amps
    )

    raw_read <- fun(input, skip = skip)

    # read_excel sometimes reads empty rows, workaround
    if (nrow(raw_read) == 65535 | all(unlist(apply(tail(raw_read, 1), 1, is.na)))) {
      nas <- apply(raw_read[1L:min(c(100000, nrow(raw_read))), ], 1, function(i) sum(is.na(i)))
      raw_read[1L:(which.min(nas != ncol(raw_read)) - 1), ]
    } else {
      raw_read
    }
  } else {
    input
  }
}
