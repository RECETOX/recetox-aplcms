#' @import tidyr
NULL
#> NULL

#' Load raw data from file
#' @export
load_file <- function(filename) {
  this <- load.lcms(filename)
  this <- tidyr::drop_na(this)
  return(this)
}

#' Load data either from cache or load raw file and detect peaks.
#' @description  THIS FUNCTION IS DEPRECATED!
#' @export
load_data <- function(filename,
                      cache,
                      min_run,
                      min_pres,
                      mz_tol,
                      intensity_weighted) {
  rawprof_filename <- paste(strsplit(tolower(filename), "\\.")[[1]][1], "_", min_run, "_", min_pres, "_", mz_tol, ".rawprof", sep = "")

  if (cache && file.exists(rawprof_filename)) {
    load(rawprof_filename)
  } else {
    raw.data <- load_file(filename)
    raw.prof <- adaptive.bin(
      raw.data,
      min_run = min_run,
      min_pres = min_pres,
      mz_tol = mz_tol,
      intensity_weighted = intensity_weighted
    )
  }

  if (cache && !file.exists(rawprof_filename)) {
    save(raw.prof, file = rawprof_filename)
  }

  return(raw.prof)
}

#' Filter noise and detect peaks from LC/MS data in CDF format
#' 
#' This function applies the run filter to remove noise. Data points are grouped into EICs in this step.
#' 
#' @param filename The CDF file name. If the file is not in the working directory, the path needs to be given.
#' @param min_pres Run filter parameter. The minimum proportion of presence in the time period for a series of 
#'  signals grouped by m/z to be considered a peak.
#' @param min_run Run filter parameter. The minimum length of elution time for a series of signals grouped by 
#'  m/z to be considered a peak.
#' @param max_run Run filter parameter. The maximum length of elution time for a series of signals grouped by 
#'  m/z to be considered a peak.
#' @param mz_tol m/z tolerance level for the grouping of data points. This value is expressed as the fraction of 
#'  the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. The recommended value is 
#'  the machine's nominal accuracy level. Divide the ppm value by 1e6. For FTMS, 1e-5 is recommended.
#' @param baseline_correct After grouping the observations, the highest intensity in each group is found. If 
#'  the highest is lower than this value, the entire group will be deleted.
#' @param baseline_correct_noise_percentile Only used for plotting. The percentile of signal strength of those EIC that don't pass the 
#'  run filter, to be used as the baseline threshold of signal strength.
#' @param intensity_weighted Whether to use intensity to weight mass density estimation.
#' @param do.plot Indicates whether plot should be drawn.
#' @param cache Whether to use cache
#' @param grouping_threshold The maximum difference between two scans to be considered the same EIC. Default is Inf. 
#' @return A matrix with four columns: m/z value, retention time, intensity, and group number.
#' @export
remove_noise <- function(filename,
                     min_pres,
                     min_run,
                     max_run,
                     mz_tol,
                     baseline_correct,
                     baseline_correct_noise_percentile,
                     intensity_weighted,
                     do.plot,
                     cache,
                     grouping_threshold) {
  raw.data <- load_file(filename)

  raw.prof <- adaptive.bin(
    raw.data,
    min_run = min_run,
    min_pres = min_pres,
    mz_tol = mz_tol,
    intensity_weighted = intensity_weighted
  )

  newprof <- cbind(
    raw.prof$features$mz,
    raw.prof$features$rt,
    raw.prof$features$intensities,
    raw.prof$features$grps
  )

  run.sel <- raw.prof$height.rec[which(raw.prof$height.rec[, 2] >= raw.prof$min.count.run * min_pres & raw.prof$height.rec[, 3] > baseline_correct), 1]

  newprof <- as.data.frame(newprof[newprof[, 4] %in% run.sel, ])
  colnames(newprof) <- c("mz", "rt", "intensity", "group_number")

  newprof <- tibble::tibble(newprof |>
    dplyr::group_by(group_number) |>
    dplyr::arrange_at("rt") |>
    dplyr::mutate(subset_group_number = cumsum(c(0, abs(diff(rt)) > grouping_threshold))) |>
    dplyr::group_by(group_number, subset_group_number) |>
    dplyr::mutate(grps = cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::select(mz, rt, intensity, grps))

  new.prof <- run_filter(
    newprof,
    min_pres = min_pres,
    min_run = min_run,
    max_run = max_run
  )

  if (do.plot) {
    plot_raw_profile_histogram(
      raw.prof,
      min_pres,
      baseline_correct,
      baseline_correct_noise_percentile,
      mz_tol,
      new.prof
    )
  }

  # new_rec_tibble <- tibble::tibble(
  #   mz = new.prof$new_rec[, 1],
  #   rt = new.prof$new_rec[, 2],
  #   intensity = new.prof$new_rec[, 3],
  #   group_number = new.prof$new_rec[, 4]
  # )

  return(new.prof)
}
