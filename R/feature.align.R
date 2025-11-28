#' Create a metadata row tibble with min, max and mean mz and RT values.
#' @param sample_grouped A dataframe with grouped mz and RT values for a particular cluster.
#' @param sample_names A list of sample names.
#' @export
create_metadata <- function(sample_grouped, sample_names) {
  sample_presence <- sapply(sample_names,
    FUN = function(x) {
      as.numeric(any(sample_grouped$sample_id == x))
    }
  )

  metadata_row <- dplyr::summarise(
    sample_grouped,
    mzmean = mean(mz),
    mzmin = min(mz),
    mzmax = max(mz),
    rtmean = mean(rt),
    rtmin = min(rt),
    rtmax = max(rt),
    npeaks = n(),
    sd1_mean = mean(sd1),
    sd1_min = min(sd1),
    sd1_max = max(sd1),
    sd2_mean = mean(sd2),
    sd2_min = min(sd2),
    sd2_max = max(sd2)
  ) %>% rename(mz = "mzmean", rt = "rtmean")

  metadata_row <- dplyr::bind_cols(metadata_row, as.list(sample_presence))
  return(metadata_row)
}

#' Compute summed area for each sample
#' @param sample_grouped A dataframe with grouped mz and RT values for a particular cluster.
#' @return Summed area for each sample.
#' @export
create_intensity_row <- function(sample_grouped) {
  sample_grouped %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(intensity = sum(area)) %>%
    tidyr::pivot_wider(names_from = "sample_id", values_from = "intensity")
}

#' Compute median RT for each sample
#' @param sample_grouped A dataframe with grouped mz and RT values for a particular cluster.
#' @return Median RT for each sample.
#' @export
create_rt_row <- function(sample_grouped) {
  sample_grouped %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(rt = median(rt)) %>%
    tidyr::pivot_wider(names_from = "sample_id", values_from = "rt")
}

#' Create a list containing 3 tibbles: metadata, intensities and RTs.
#' @param sample_grouped A dataframe with grouped mz and RT values for a particular cluster.
#' @param sample_names A list of sample names.
#' @return A list containing 3 tibbles: metadata, intensities and RTs.
#' @export
create_output <- function(sample_grouped, sample_names) {
  metadata_row <- create_metadata(sample_grouped, sample_names)
  intensity_row <- create_intensity_row(sample_grouped)
  rt_row <- create_rt_row(sample_grouped)

  return(list(
    metadata_row = metadata_row,
    intensity_row = intensity_row,
    rt_row = rt_row
  ))
}

#' Validates if the data is present in more than "min_occurence" of samples.
#' @param samples A subset of the features_table.
#' @param min_occurrence A minimal number of profiles a feature has to be present in.
#' @return boolean value whether it is TRUE or FALSE.
#' @export
validate_contents <- function(samples, min_occurrence) {
  if (!is.null(nrow(samples))) {
    if (length(unique(samples$sample_id)) >= min_occurrence) {
      return(TRUE)
    }
    return(FALSE)
  }
  return(FALSE)
}

#' Compute the kernel density estimation and find the peaks and valleys of a smooth curve.
#' @param data A vector of m/z or RTs for a particular cluster.
#' @param bandwidth A bandwidth value for the KDE computation.
#' @return A list of peaks and valleys positions.
#' @export
find_optima <- function(data, bandwidth) {
  den <- density(data, bw = bandwidth)
  turns <- find.turn.point(den$y)
  return(list(peaks = den$x[turns$pks], valleys = den$x[turns$vlys]))
}

#' Subset data within lower and upper bound from density estimation
#' @param sample A subset of the features_table.
#' @param turns A list of peaks and valleys positions.
#' @param index Whether it subsets on m/z [1] or RT [2] column.
#' @param i Iterates over the peaks in the turns list.
#' @return Dataframe subsetted within lower and upper bound from density estimation.
#' @export
filter_based_on_density <- function(sample, turns, index, i) {
  lower_bound <- max(turns$valleys[turns$valleys < turns$peaks[i]])
  upper_bound <- min(turns$valleys[turns$valleys > turns$peaks[i]])
  selected <- which(sample[, index] > lower_bound & sample[, index] <= upper_bound)
  return(sample[selected, ])
}

#' Group the mz and RT for particular cluster.
#' @param features The features table subsetted for a particular cluster.
#' @param mz_tol_relative The m/z tolerance level for peak alignment.
#' @param rt_tol_relative The retention time tolerance level for peak alignment.
#' @param min_occurrence A minimal number of profiles a feature has to be present in.
#' @param sample_names A list of sample names.
#' @return A list containing 3 tibbles: metadata, intensities and RTs.
#' @export
create_features_from_cluster <- function(features,
                                         mz_tol_relative,
                                         rt_tol_relative,
                                         min_occurrence,
                                         sample_names) {
  if (!validate_contents(features, min_occurrence)) {
    return(NULL)
  }

  # create empty tibble rows
  metadata <- NULL
  intensity <- NULL
  rt <- NULL

  # split according to mz values
  turns_mz <- find_optima(features$mz, bandwidth = mz_tol_relative * median(features$mz))
  for (i in seq_along(turns_mz$peaks)) {
    sample_grouped_mz <- filter_based_on_density(features, turns_mz, 1, i)
    if (validate_contents(sample_grouped_mz, min_occurrence)) {
      # split according to rt values
      turns_rt <- find_optima(sample_grouped_mz$rt, bandwidth = rt_tol_relative / 1.414)
      for (ii in seq_along(turns_rt$peaks)) {
        sample_grouped_rt <- filter_based_on_density(sample_grouped_mz, turns_rt, 2, ii)

        # create output rows if valid
        if (validate_contents(sample_grouped_rt, min_occurrence)) {
          metadata <- dplyr::bind_rows(metadata, create_metadata(sample_grouped_rt, sample_names))
          intensity <- dplyr::bind_rows(intensity, create_intensity_row(sample_grouped_rt))
          rt <- dplyr::bind_rows(rt, create_rt_row(sample_grouped_rt))
        }
      }
    }
  }

  return(list(metadata_row = metadata, intensity_row = intensity, rt_row = rt))
}

#' Combines the output (i.e. metadata, intensity and RT) from different clusters to one respective tibble.
#' @return Tibbles combining the output (metadata, intensity and RT respectively) from different clusters.
#' @export
comb <- function(x, ...) {
  mapply(plyr::rbind.fill, x, ..., SIMPLIFY = FALSE)
}

#' Replace NA values by zero, relocate 'sample_names' column to the very beginning and convert to a tibble
#' @param x A dataframe
#' @param sample_names List of sample names.
#' @return Cleaned tibble.
#' @export
clean_data_matrix <- function(x, sample_names) {
  x <- x %>%
    replace(is.na(.), 0) %>%
    dplyr::relocate(all_of(sample_names)) |>
    add_feature_ids()
  return(x)
}

#' Add `id` column to a dataframe
#' @param x A dataframe
#' @return The same dataframe but with an additional `id` column
#' in first place which contains the rownames.
#' @export
add_feature_ids <- function(x) {
  x$id <- as.integer(rownames(x))
  return(tibble::as_tibble(x |> dplyr::relocate(id)))
}

#' Align peaks from spectra into a feature table.
#'
#' @param features_table A list object. Each component is a matrix which is the output from compute_clusters().
#' @param min_occurrence  A feature has to show up in at least this number of profiles to be included in the final result.
#' @param sample_names list List of sample names.
#' @param mz_tol_relative The m/z tolerance level for peak alignment. The default is NA, which allows the
#'  program to search for the tolerance level based on the data. This value is expressed as the
#'  percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.
#' @param rt_tol_relative The retention time tolerance level for peak alignment. The default is NA, which
#'  allows the program to search for the tolerance level based on the data.
#' @param cluster The number of CPU cores to be used
#' @return A list of 3 tibbles containing aligned metadata, intensities an RTs.
#'
#' @export
create_aligned_feature_table <- function(features_table,
                                         min_occurrence,
                                         sample_names,
                                         rt_tol_relative,
                                         mz_tol_relative,
                                         cluster = 4) {
  if (!is(cluster, "cluster")) {
    cluster <- parallel::makeCluster(cluster)
    on.exit(parallel::stopCluster(cluster))

    # NOTE: side effect (doParallel has no functionality to clean up)
    doParallel::registerDoParallel(cluster)
    register_functions_to_cluster(cluster)
  }

  # table with number of values per group
  groups_cardinality <- table(features_table$cluster)
  # count those with minimal occurrence
  sel.labels <- as.numeric(names(groups_cardinality)[groups_cardinality >= min_occurrence])

  # retention time alignment
  aligned_features <- foreach::foreach(
    i = seq_along(sel.labels), .combine = "comb", .multicombine = TRUE
  ) %dopar% {
    rows <- create_features_from_cluster(
      dplyr::filter(features_table, cluster == sel.labels[i]),
      mz_tol_relative,
      rt_tol_relative,
      min_occurrence,
      sample_names
    )
    list(metadata = rows$metadata_row, intensity = rows$intensity_row, rt = rows$rt_row)
  }

  aligned_features$intensity <- clean_data_matrix(aligned_features$intensity, sample_names)
  aligned_features$rt <- clean_data_matrix(aligned_features$rt, sample_names)
  aligned_features$metadata <- add_feature_ids(aligned_features$metadata)

  return(aligned_features)
}
