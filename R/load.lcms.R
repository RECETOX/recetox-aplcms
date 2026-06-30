#' @import mzR
#' @import tibble
NULL
#> NULL

#' Loading LC/MS data.
#' 
#' This is an internal function. It loads LC/MS data into memory.
#' 
#' @param filename The CDF file name.
#' @return A tibble is returned with the following columns:
#' \itemize{
#'   \item mz - The vector of m/z values.
#'   \item rt - The vector of retention times.
#'   \item intensity - The vector of intensity values.
#' }
#' @export
load.lcms <- function(filename) {

  # Check if file is raw file, if so, use rawrr package
  if (tools::file_ext(filename) == "raw") {
    return(load.lcms.raw(filename))
  }

  mz_conn <- mzR::openMSfile(filename = filename)
  header <- mzR::header(mz_conn)
  
  b <- header$retentionTime

  masses <- NULL
  intensi <- NULL
  labels <- NULL

  segs <- seq(0, length(b), by = 200)
  if ((length(b) %% 200) != 0)
    segs <- c(segs, length(b))

  for (n in 2:length(segs)) {
    a <- mzR::peaks(mz_conn, scans = (segs[n - 1] + 1):segs[n])

    this_masses <- NULL
    this_intensi <- NULL
    this_labels <- NULL

    for (i in seq_along(a)) {
      this_a <- a[[i]]

      if (!is.null(nrow(this_a))) {
        this_a <- this_a[this_a[, 2] > 1e-10,]
        if (is.null(nrow(this_a)))
          this_a <- matrix(this_a, nrow = 1)

        this_masses <- c(this_masses, this_a[, 1])
        this_intensi <- c(this_intensi, this_a[, 2])
        this_labels <- c(this_labels, rep(b[segs[n - 1] + i], nrow(this_a)))
      }else {
        b[segs[n - 1] + i] <- NA
      }
    }

    masses <- c(masses, this_masses)
    intensi <- c(intensi, this_intensi)
    labels <- c(labels, this_labels)
  }

  mzR::close(mz_conn)
  features <- tibble::tibble(mz = masses, rt = labels, intensity = intensi) |>
              dplyr::mutate(sample_id = read_run_id(filename))
  attr(features, 'run_id') <- read_run_id(filename)
  return(features)
}

#' Process a chunk of spectra and return a list of m/z, rt, and intensities.
#'#' @param spectra A list of spectra, where each spectrum is a list containing m/z and intensity values.
#' @param start_times A vector of start times corresponding to each spectrum.
#' @return A list of lists, where each inner list contains m/z, rt, and intensities for a spectrum.
#' @export
process_chunk <- function(spectra, start_times) {
  lapply(seq_along(spectra), function(j) {
    spectrum <- spectra[[j]]
    n <- length(spectrum$mZ)
    points <- tibble::tibble(
      mz = spectrum$mZ,
      rt = rep(start_times[j], n),
      intensity = spectrum$intensity
    )
    return(dplyr::filter(points, intensity > 0))
  })
}

#' Loading MS data from raw files.
#'
#' This is an internal function. It loads MS data from raw files into memory.
#'
#' @param filename The raw file name.
#' @return A tibble is returned with the following columns:
#' \itemize{
#'   \item mz - The vector of m/z values.
#'   \item rt - The vector of retention times.
#'   \item intensities - The vector of intensity values.
#' }
#' @import tibble
#' @import dplyr
#' @export
load.lcms.raw <- function(filename, chunk_size = 100) {
  # Check if the rawrr package is installed
  if (!requireNamespace("rawrr", quietly = TRUE)) {
    stop("The 'rawrr' package is required but not installed. Please install it with install.packages('rawrr').")
  }

  # Check if rawrr is setup correctly
  if(rawrr::rawrrAssemblyPath() == "") {
    stop("The 'rawrr' package is not set up correctly. Please ensure that the rawrr package is installed and configured properly.")
  }

  # Check if the file exists
  if (!file.exists(filename)) {
    stop(paste("The file", filename, "does not exist."))
  }

  header <- rawrr::readFileHeader(filename)
  idx <- rawrr::readIndex(filename)
  n_scans <- length(idx$scan)
  chunk_starts <- seq(1, n_scans, by = chunk_size)

  results <- future.apply::future_lapply(chunk_starts, function(start) {
    end <- min(start + chunk_size - 1, n_scans)
    scans_to_load <- idx$scan[start:end]
    spectra <- rawrr::readSpectrum(filename, scan = scans_to_load)
    process_chunk(spectra, idx$StartTime[start:end])
  })

  features <- dplyr::bind_rows(results) |>
              dplyr::mutate(sample_id = read_run_id(filename))
  attr(features, 'run_id') <- read_run_id(filename)
  return(features)
}