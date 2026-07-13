#' @import dplyr stringr tibble tools tidyr parallel doParallel snow
NULL
#> NULL

#' Internal function: Convert feature tables to crosstab format.
#'
#' @description
#' Combines metadata and data tables into a single crosstab format with feature metadata
#' columns followed by sample data columns.
#'
#' @param sample_names A character vector of sample names to use as column headers.
#' @param metadata A tibble containing feature metadata (id, mz, rt, mzmin, mzmax).
#' @param data A tibble containing sample data to be joined with metadata.
#'
#' @return A tibble in crosstab format with metadata columns followed by sample columns.
#'
#' @importFrom dplyr select inner_join
#' @export
as_feature_crosstab <- function(sample_names, metadata, data) {
  metadata_cols <- c('id', 'mz', 'rt', 'mzmin', 'mzmax')
  data <- select(metadata, metadata_cols) |>
    inner_join(data, by ='id')
  colnames(data) <- c(metadata_cols, sample_names)
  
  return(data)
}

#' Internal function: Recover weaker signals across multiple samples.
#'
#' @description
#' Performs weak signal recovery for features across multiple samples using the aligned
#' feature table as a reference. This function parallelizes the recovery process across
#' samples using a compute cluster.
#'
#' @param cluster A parallel cluster object for distributed computation.
#' @param filenames A character vector of file paths to the raw data files.
#' @param extracted_features A list of extracted feature tables, one per sample.
#' @param corrected_features A list of time-corrected feature tables, one per sample.
#' @param aligned_rt_crosstab A crosstab table of aligned retention times.
#' @param aligned_int_crosstab A crosstab table of aligned intensities.
#' @param original_mz_tolerance The original m/z tolerance used in feature extraction.
#' @param aligned_mz_tolerance The m/z tolerance used in alignment.
#' @param aligned_rt_tolerance The retention time tolerance used in alignment.
#' @param recover_mz_range The m/z range around features to search for weak signals.
#' @param recover_rt_range The retention time range around features to search for weak signals.
#' @param use_observed_range Logical; whether to use observed ranges for recovery.
#' @param min_bandwidth Minimum bandwidth for signal smoothing.
#' @param max_bandwidth Maximum bandwidth for signal smoothing.
#' @param recover_min_count Minimum number of data points for a recovered signal.
#'
#' @return A list containing extracted_features, corrected_features, rt_crosstab, and int_crosstab.
#' @export
recover_weaker_signals <- function(
  cluster,
  filenames,
  extracted_features,
  corrected_features,
  aligned_rt_crosstab,
  aligned_int_crosstab,
  original_mz_tolerance,
  aligned_mz_tolerance,
  aligned_rt_tolerance,
  recover_mz_range,
  recover_rt_range,
  use_observed_range,
  min_bandwidth,
  max_bandwidth,
  recover_min_count,
  min_occurrence
) {
  snow::clusterExport(cluster, c('recover.weaker'))
  snow::clusterEvalQ(cluster, library("splines"))
  
  sample_names <- get_sample_names(corrected_features)

  recovered <- lapply(seq_along(filenames), function(i) {
    recover.weaker(
      filename = filenames[i],
      sample_name = sample_names[i],
      metadata_table = aligned_int_crosstab,
      intensity_table = aligned_int_crosstab,
      rt_table = aligned_rt_crosstab,
      mz_tol_relative = aligned_mz_tolerance,
      rt_tol_relative = aligned_rt_tolerance,
      extracted_features = as_tibble(extracted_features[[i]]),
      adjusted_features = as_tibble(corrected_features[[i]]),
      recover_mz_range = recover_mz_range,
      recover_rt_range = recover_rt_range,
      use_observed_range = use_observed_range,
      mz_tol = original_mz_tolerance,
      min_bandwidth = min_bandwidth,
      max_bandwidth = max_bandwidth,
      bandwidth = 0.5,
      recover_min_count = recover_min_count,
      intensity_weighted = TRUE
    )
  })   

  adjusted_features <- lapply(recovered, function(x) x$adjusted_features)

  res <- compute_clusters(
    feature_tables = adjusted_features,
    mz_tol_relative = aligned_mz_tolerance,
    mz_tol_absolute = 0.01,
    mz_max_diff = 10 * aligned_mz_tolerance,       
    rt_tol_relative = aligned_rt_tolerance,
    do.plot = FALSE,
    sample_names = sample_names
  )
  
  aligned_features <- create_aligned_feature_table(
    features_table = dplyr::bind_rows(res$feature_tables),
    min_occurrence = min_occurrence,     
    sample_names = sample_names,
    rt_tol_relative = res$rt_tol_relative,
    mz_tol_relative = res$mz_tol_relative
   )

  return(aligned_features)
}

#' Internal function: Pivot feature values from long to wide format.
#'
#' @description
#' Converts a long-format feature table to wide format by pivoting sample-specific
#' values (RT or intensity) into separate columns for each sample.
#'
#' @param feature_table A tibble in long format with columns: mz, rt, sample, and sample-specific values.
#' @param variable A character string specifying which variable to pivot ("rt" or "intensity").
#'
#' @return A tibble in wide format with one row per feature (mz, rt) and one column per sample.
#' @export
pivot_feature_values <- function(feature_table, variable) {
  extended_variable <- paste0("sample_", variable)
  values <- dplyr::select(feature_table, mz, rt, sample, !!sym(extended_variable))
  values <- tidyr::pivot_wider(values, names_from = sample, values_from = !!sym(extended_variable))
  variable_colnames <- colnames(values)[3:ncol(values)]
  variable_colnames <- paste0(variable_colnames, "_", variable)
  colnames(values)[3:ncol(values)] <- variable_colnames
  return(values)
}

#' Internal function: Convert feature table from long to wide format.
#'
#' @description
#' Transforms a long-format feature table (one row per feature-sample combination) into
#' wide format (one row per feature with sample data in columns).
#'
#' @param feature_table A tibble in long format with columns: mz, rt, sample, sample_rt, sample_intensity.
#'
#' @return A tibble in wide format with columns: mz, rt, and sample-specific RT and intensity columns.
#' @export
long_to_wide_feature_table <- function(feature_table) {
  sample_rts <- pivot_feature_values(feature_table, "rt")
  sample_intensities <- pivot_feature_values(feature_table, "intensity")
  feature_table <- dplyr::select(feature_table, mz, rt) %>%
    dplyr::distinct(mz, rt) %>%
    dplyr::inner_join(sample_rts, by = c("mz", "rt")) %>%
    dplyr::inner_join(sample_intensities, by = c("mz", "rt"))
}

#' Internal function: Convert feature table from wide to long format.
#'
#' @description
#' Transforms a wide-format feature table (one row per feature with sample data in columns)
#' into long format (one row per feature-sample combination).
#'
#' @param wide_table A tibble in wide format with sample-specific RT and intensity columns.
#' @param sample_names A character vector of sample names (currently unused in implementation).
#'
#' @return A tibble in long format with columns: feature, mz, rt, mz_min, mz_max, sample, sample_rt, sample_intensity.
#' @export
wide_to_long_feature_table <- function(wide_table, sample_names) {
  wide_table <- tibble::rowid_to_column(wide_table, "feature")
  
  long_rt <- tidyr::gather(wide_table, sample, sample_rt, contains("_rt"), factor_key=FALSE) %>%
    dplyr::select(-contains("_intensity")) %>%
    mutate(sample = stringr::str_remove_all(sample, "_rt"))
  long_int <- tidyr::gather(wide_table, sample, sample_intensity, contains("_intensity"), factor_key=FALSE) %>%
    dplyr::select(-contains("_rt")) %>%
    mutate(sample = stringr::str_remove_all(sample, "_intensity"))
  
  long_features <- dplyr::full_join(long_rt, long_int, by = c("feature", "mz", "rt", "mzmin", "mzmax", "sample"))
  
  return(long_features)
}

#' Extract column names matching a pattern.
#' @description
#' This function extracts the column names from a dataframe that contain a specified pattern.
#' @param dataframe A dataframe from which to extract column names.
#' @param pattern A character string containing the pattern to match in the column names.
#' @return A character vector of column names that match the specified pattern.
#' @export
extract_pattern_colnames <- function(dataframe, pattern) {
  dataframe <- dplyr::select(dataframe, contains(pattern))
  return(colnames(dataframe))
}

#' Internal function: Convert aligned feature tables to wide format.
#'
#' @description
#' Transforms aligned feature tables (RT and intensity crosstabs) into a single wide-format
#' table suitable for downstream analysis.
#'
#' @param aligned A list containing rt_crosstab and int_crosstab elements.
#'
#' @return A tibble in wide format with feature metadata and sample-specific RT and intensity values.
#' @export
as_wide_aligned_table <- function(aligned) {
  mz_scale_table <- aligned$metadata[, c("mz", "rt", "mzmin", "mzmax")]
  aligned <- as_feature_sample_table(
    metadata = aligned$metadata,
    rt_crosstab = aligned$rt,
    int_crosstab = aligned$int
  )
  aligned <- long_to_wide_feature_table(aligned)
  aligned <- dplyr::inner_join(aligned, mz_scale_table, by = c("mz", "rt")) 
  return(aligned)
}

#' Internal function: Merge known feature tables from multiple batches.
#'
#' @description
#' Combines the updated known feature tables from multiple batch processing results
#' into a single comprehensive known feature table.
#'
#' @param batchwise A list of batch processing results, each containing an updated.known.table.
#' @param batches_idx A vector of batch indices to process.
#'
#' @return A tibble containing the merged known feature table with standardized columns.
#' @export
merge_known_tables <- function(batchwise, batches_idx) {
  colnames <- c("chemical_formula", "HMDB_ID", "KEGG_compound_ID", "mass", "ion.type", "m.z",
              "Number_profiles_processed", "Percent_found", "mz_min", "mz_max", 
              "RT_mean", "RT_sd", "RT_min", "RT_max", "int_mean(log)", "int_sd(log)", 
              "int_min(log)", "int_max(log)")

  known_table <- tibble(
    chemical_formula = character(),
    HMDB_ID = character(),
    KEGG_compound_ID = character(),
    mass = numeric(),
    ion.type = character(),
    m.z = numeric(),
    Number_profiles_processed = numeric(),
    Percent_found = numeric(),
    mz_min = numeric(),
    mz_max = numeric(),
    RT_mean = numeric(),
    RT_sd = numeric(),
    RT_min = numeric(),
    RT_max = numeric(),
    "int_mean(log)" = numeric(),
    "int_sd(log)" = numeric(),
    "int_min(log)" = numeric(),
    "int_max(log)" = numeric()
  )

  for (batch in batches_idx) {
    known_table <- dplyr::full_join(known_table, batchwise[[batch]]$updated_known_table, by = colnames)
  }

  return(known_table)
}

#' Internal function: Readjust retention times to match between-batch alignment.
#'
#' @description
#' Adjusts the retention times in within-batch corrected features to match the
#' retention times from between-batch alignment, ensuring consistency across batches.
#'
#' @param within_batch A list containing recovered_feature_sample_table and corrected_features.
#' @param between_batch A list containing the rt (retention time) table from between-batch alignment.
#'
#' @return A list of corrected feature tables with adjusted retention times.
#' @export
readjust_times <- function(within_batch, between_batch) {
  within_batch_recovered <- long_to_wide_feature_table(
    within_batch$recovered_feature_sample_table
  )
  between_batch_rts <- between_batch$rt
  for (j in 1:length(within_batch$corrected_features)) {
    for (i in 1:nrow(within_batch$corrected_features[[j]])) {
      diff.time <- abs(
        within_batch_recovered$rt -
          within_batch$corrected_features[[j]][i, "rt"]
      )
      min_idx <- which(diff.time == min(diff.time))[1]
      within_batch$corrected_features[[j]][i, "rt"] <- between_batch_rts[min_idx]
    }
  }
  return(within_batch$corrected_features)
}

#' Internal function: Compute median intensities for features.
#'
#' @description
#' Calculates the median intensity for each feature across all samples in which it appears.
#' The median is computed by grouping features by their m/z and retention time.
#'
#' @param feature_table A tibble containing feature data with a sample_intensity column.
#'
#' @return The feature table with an additional median_intensity column.
#' @export
compute_intensity_medians <- function(feature_table) {
  stopifnot("sample_intensity" %in% colnames(feature_table))
  feature_table <- dplyr::group_by(feature_table, mz, rt) %>%
    dplyr::mutate(median_intensity = median(sample_intensity)) %>%
    dplyr::ungroup()
  return(feature_table)
}

#' Internal function: Bind batch labels to filenames.
#'
#' @description
#' Combines filename information with metadata by matching sample names, adding batch
#' labels to each filename for batch-wise processing.
#'
#' @param filenames A character vector of file paths.
#' @param metadata A tibble containing sample_name and batch columns.
#'
#' @return A tibble with filename and batch columns.
#' @export
bind_batch_label_column <- function(filenames, metadata) {
  stopifnot(nrow(metadata) == length(filenames))

  filenames <- as_tibble(filenames)
  colnames(filenames)[1] <- "filename"
  filenames <- mutate(filenames, sample_name = get_sample_name(filename))
  filenames <- inner_join(filenames, metadata, by = "sample_name")
  return(dplyr::select(filenames, -sample_name))
}


#' Internal function: Align batch tables returned by feature_recovery.
#'
#' @description
#' Combines individual batch tibbles into one list object containing aligned metadata, intensity and rt tibbles from all samples. 
#'
#' @param recovered_tables A list of tibbles containing joined metadata, intensity and rt tibbles produced by recover_weaker_signals.
#' @param batch_names A list of batch names - treated as sample names.
#' @param batch_align_mz_tol The m/z tolerance for batch alignment.
#' @param batch_align_rt_tol The retention time tolerance for batch alignment.
#' @param min_occurrence A feature has to show up in at least this number of profiles to be included in the final result.
#'
#' @return A list of aligned metadata, intensity and rt tibbles across batches.
#' @export

align_recovered_batch_tables <- function(recovered_tables, 
                                          batch_names,
                                          batch_align_mz_tol,
                                          batch_align_rt_tol,
                                          min_occurrence){

  batch_clustered <- compute_clusters_simple(feature_tables = recovered_tables, 
                                       sample_names = batch_names, 
                                       mz_tol_ppm = batch_align_mz_tol, 
                                       rt_tol = batch_align_rt_tol)

  batch_aligned <- create_aligned_feature_table_simple(features_table = dplyr::bind_rows(batch_clustered), 
                                      sample_names = batch_names, 
                                      min_occurrence = min_occurrence,
                                      batch = TRUE)
  return(batch_aligned)
}


#' Internal function: Recover features across batches.
#'
#' @description
#' Performs comprehensive feature recovery across multiple batches by matching features
#' from within-batch processing to between-batch aligned features, then performing
#' weak signal recovery for each batch.
#'
#' @param cluster A parallel cluster object for distributed computation.
#' @param step_one_features A list of feature tables from initial batch processing.
#' @param batchwise A list of batch processing results.
#' @param filenames_batchwise A tibble containing filename and batch information.
#' @param corrected A list of time-corrected feature tables for between-batch alignment.
#' @param aligned A wide-format aligned feature table from between-batch alignment.
#' @param batches_idx A vector of batch indices.
#' @param mz_tol The m/z tolerance for feature matching.
#' @param batch_align_mz_tol The m/z tolerance for batch alignment.
#' @param batch_align_rt_tol The retention time tolerance for batch alignment.
#' @param recover_mz_range The m/z range for weak signal recovery.
#' @param recover_rt_range The retention time range for weak signal recovery.
#' @param use_observed_range Logical; whether to use observed ranges.
#' @param min_bw Minimum bandwidth for smoothing.
#' @param max_bw Maximum bandwidth for smoothing.
#' @param recover_min_count Minimum count for recovered signals.
#' @param min_occurrence A feature has to show up in at least this number of profiles to be included in the final result.
#' @param batch_names A list of batch names - treated as sample names to differentiate between batches.
#'
#' @return A tibble containing recovered features across all batches in wide format.
#' @export
feature_recovery <- function(cluster,
                             step_one_features,
                             batchwise,
                             filenames_batchwise,
                             corrected,
                             aligned,
                             batches_idx,
                             mz_tol,
                             batch_align_mz_tol,
                             batch_align_rt_tol,
                             recover_mz_range,
                             recover_rt_range,
                             use_observed_range,
                             min_bw,
                             max_bw,
                             recover_min_count,
                             min_occurrence,
                             batch_names) {

  recovered_batchwise <- new("list")
  recovered_batchwise_joined <- new("list")

  for (batch_id in batches_idx)
  {
    this.fake <- long_to_wide_feature_table(step_one_features[[batch_id]])
    this.fake.medians <- apply(dplyr::select(this.fake, contains("_intensity")), 1, median)

    # adjusting the time (already within batch adjusted)
    this.features <- readjust_times(batchwise[[batch_id]], corrected[[batch_id]])
    aligned_intensities <- dplyr::select(aligned, contains("_intensity"))
    batchwise_intensities <- as.matrix(dplyr::select(this.fake, contains("_intensity")))

    this.fake.time <- as.matrix(dplyr::select(this.fake, contains("_rt")))
    this.pk.time <- this.aligned <- matrix(0, nrow = nrow(aligned), ncol = ncol(batchwise_intensities))

    for (sample in 1:nrow(aligned)) {
      # Present in aligned data
      if (aligned_intensities[sample, batch_id] != 0) {
        idx <- which(between(this.fake$mz, aligned[sample, "mzmin"], aligned[sample, "mzmax"]) &
          abs(this.fake.medians - aligned_intensities[sample, batch_id]) < 1)
        
        if (length(idx) < 1) {
          idx <- which(between(this.fake$mz, aligned[sample, "mzmin"], aligned[sample, "mzmax"]))
        }
        
        if (length(idx) < 1) {
          message("warning: batch ", batch_id, " sample ", sample, " has matching issue")
        } else {
          this.aligned[sample, ] <- as.numeric(apply(batchwise_intensities[idx, ,drop = FALSE], 2, sum))
          this.pk.time[sample, ] <- as.numeric(apply(this.fake.time[idx, ,drop = FALSE], 2, median))
        }
      } 
      
      else {
        ### go into individual feature tables to find a match - missing from aligned data, recapturing from individual features
        recaptured <- rep(0, ncol(this.aligned))
        recaptured.time <- rep(NA, ncol(this.aligned))

        for (j in 1:length(this.features)) {
          diff.mz <- abs(this.features[[j]][, "mz"] - aligned[sample, "mz"])
          diff.time <- abs(this.features[[j]][, "rt"] - aligned[sample, "rt"])
          idx <- which(diff.mz < aligned[sample, "mz"] * batch_align_mz_tol & diff.time <= batch_align_rt_tol)

          if (length(idx) > 0) {
            idx <- idx[which(diff.time[idx,] == min(diff.time[idx,]))[1]]
            recaptured[j] <- as.numeric(this.features[[j]][idx, "area"])
            recaptured.time[j] <- as.numeric(this.features[[j]][idx, "rt"])
          }
        }
        this.aligned[sample, ] <- recaptured
        this.pk.time[sample, ] <- recaptured.time
      }
    }

    colnames(this.aligned) <- extract_pattern_colnames(this.fake, "_intensity")
    colnames(this.aligned) <- stringr::str_remove_all(colnames(this.aligned), "_intensity")
    colnames(this.pk.time) <- extract_pattern_colnames(this.fake, "_rt")
    colnames(this.pk.time) <- stringr::str_remove_all(colnames(this.pk.time), "_rt")


    aligned_features <- dplyr::select(aligned, mz, rt, mzmin, mzmax)
    aligned_int_crosstab <- dplyr::bind_cols(aligned_features, as_tibble(this.aligned))
    aligned_rt_crosstab <- dplyr::bind_cols(aligned_features, as_tibble(this.pk.time))
    
    recovered_batchwise[[batch_id]] <- recover_weaker_signals(
      cluster = cluster,
      filenames = filter(filenames_batchwise, batch == batch_id)$filename,
      extracted_features = batchwise[[batch_id]]$extracted_features,
      corrected_features = batchwise[[batch_id]]$corrected_features,
      aligned_rt_crosstab = aligned_rt_crosstab,
      aligned_int_crosstab = aligned_int_crosstab,
      original_mz_tolerance = mz_tol,
      aligned_mz_tolerance = batch_align_mz_tol,
      aligned_rt_tolerance = batch_align_rt_tol,
      recover_mz_range = recover_mz_range,
      recover_rt_range = recover_rt_range,
      use_observed_range = use_observed_range,
      min_bandwidth = min_bw,
      max_bandwidth = max_bw,
      recover_min_count = recover_min_count,
      min_occurrence = min_occurrence
    )

    recovered_batchwise_joined[[batch_id]] <- dplyr::full_join(recovered_batchwise[[batch_id]]$intensity, 
                                                        recovered_batchwise[[batch_id]]$rt, 
                                                        by = 'id', suffix = c('_intensity', '_rt')) |> 
                                              dplyr::full_join(recovered_batchwise[[batch_id]]$metadata, by='id') |>
                                              dplyr::rename(sd1 = sd1_mean, sd2 = sd2_mean)
  }

  recovered <- align_recovered_batch_tables(recovered_tables = recovered_batchwise_joined,
                                            batch_names = batch_names,
                                            batch_align_mz_tol = batch_align_mz_tol*1e06,
                                            batch_align_rt_tol = batch_align_rt_tol,
                                            min_occurrence = 1)


  return(recovered)
}

#' Two step hybrid feature detection.
#' 
#' A two-stage hybrid feature detection and alignment procedure, for data generated in multiple batches.
#' 
#' @param filenames file names
#' @param metadata the batch label of each file.
#' @param work_dir The folder where all CDF files to be processed are located.
#' @param min_within_batch_prop_detect A feature needs to be present in at least this proportion of the files, 
#'  for it to be initially detected as a feature for a batch. This parameter replaces the "min.exp" parameter in semi.sup().
#' @param min_within_batch_prop_report A feature needs to be present in at least this proportion of the files, 
#'  in a proportion of batches controlled by "min_batch_prop", to be included in the final feature table. This parameter 
#'  replaces the "min.exp" parameter in semi.sup().
#' @param min_batch_prop A feature needs to be present in at least this proportion of the batches, for it to be 
#'  considered in the entire data.
#' @param min_occurrence A feature has to show up in at least this number of profiles to be included in the final result.
#' @param batch_align_mz_tol The m/z tolerance in ppm for between-batch alignment.
#' @param batch_align_rt_tol The RT tolerance for between-batch alignment.
#' @param known_table A data frame containing the known metabolite ions and previously found features.
#' @param cluster The number of CPU cores to be used
#' @param min_pres This is a parameter of the run filter, to be passed to the function remove_noise().
#' @param min_run This is a parameter of the run filter, to be passed to the function remove_noise().
#' @param mz_tol The user can provide the m/z tolerance level for peak identification. This value is expressed as the 
#'  percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.
#' @param mz_tol_relative The m/z tolerance level for peak alignment. The default is NA, which allows the program to search for the 
#'  tolerance level based on the data. This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z 
#'  value, becomes the cutoff level.
#' @param rt_tol_relative The retention time tolerance level for peak alignment. The default is NA, which allows the program to search for 
#'  the tolerance level based on the data.
#' @param baseline_correct_noise_percentile The perenctile of signal strength of those EIC that don't pass the run filter, 
#'  to be used as the baseline threshold of signal strength. This parameter is passed to remove_noise()
#' @param shape_model The mathematical model for the shape of a peak. There are two choices - "bi-Gaussian" and "Gaussian". 
#'  When the peaks are asymmetric, the bi-Gaussian is better. The default is "bi-Gaussian".
#' @param baseline_correct This is a parameter in peak detection. After grouping the observations, the highest observation 
#'  in each group is found. If the highest is lower than this value, the entire group will be deleted. The default value is NA, 
#'  which allows the program to search for the cutoff level.
#' @param peak_estim_method the bi-Gaussian peak parameter estimation method, to be passed to subroutine prof.to.features. 
#'  Two possible values: moment and EM.
#' @param min_bw The minimum bandwidth in the smoother in prof.to.features().
#' @param max_bw The maximum bandwidth in the smoother in prof.to.features().
#' @param sd_cut A parameter for the prof.to.features() function. A vector of two. Features with standard deviation outside 
#'  the range defined by the two numbers are eliminated.
#' @param sigma_ratio_lim A parameter for the prof.to.features() function. A vector of two. It enforces the belief of the 
#'  range of the ratio between the left-standard deviation and the right-standard deviation of the bi-Gaussian function used 
#'  to fit the data.
#' @param component_eliminate In fitting mixture of bi-Gaussian (or Gaussian) model of an EIC, when a component accounts 
#'  for a proportion of intensities less than this value, the component will be ignored.
#' @param moment_power The power parameter for data transformation when fitting the bi-Gaussian or Gaussian mixture model in an EIC.
#' @param align_mz_tol The user can provide the m/z tolerance level for peak alignment to override the program's selection. 
#'  This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.
#' @param align_rt_tol The user can provide the elution time tolerance level to override the program's selection. This value 
#'  is in the same unit as the elution time, normaly seconds.
#' @param max_align_mz_diff As the m/z tolerance in alignment is expressed in relative terms (ppm), it may not be suitable 
#'  when the m/z range is wide. This parameter limits the tolerance in absolute terms. It mostly influences feature matching 
#'  in higher m/z range.
#' @param pre_process Logical. If true, the program will not perform time correction and alignment. It will only generate peak tables 
#'  for each spectra and save the files. It allows manually dividing the task to multiple machines.
#' @param recover_mz_range A parameter of the recover.weaker() function. The m/z around the feature m/z to search for observations. 
#'  The default value is NA, in which case 1.5 times the m/z tolerance in the aligned object will be used.
#' @param recover_rt_range A parameter of the recover.weaker() function. The retention time around the feature retention time to 
#'  search for observations. The default value is NA, in which case 0.5 times the retention time tolerance in the aligned 
#'  object will be used.
#' @param use_observed_range A parameter of the recover.weaker() function. If the value is TRUE, the actual range of the observed 
#'  locations of the feature in all the spectra will be used.
#' @param match_tol_ppm The ppm tolerance to match identified features to known metabolites/features.
#' @param new_feature_min_count The number of profiles a new feature must be present for it to be added to the database.
#' @param recover_min_count The minimum time point count for a series of point in the EIC for it to be considered a true feature.
#' @param intensity_weighted Whether to use intensity to weight mass density estimation.
#' @param BIC_factor the factor that is multiplied on the number of parameters to modify the BIC criterion. If larger than 1, 
#'  models with more peaks are penalized more.
#' @return A list is returned.
#' \itemize{
#'   \item batchwise_features - A list. Each item in the list is the product of hybrid() from a single batch.
#'   \item known_table - Known table augmented with newly detected features.
#'   \item aligned_features - Pseudo table of aligned features across batches used for aligning and recovering final features.
#'   \item final_features - Feature table. This is the end product of the function.
#' }
#' @export
#' 
two.step.hybrid <- function(filenames,
                            metadata,
                            work_dir,
                            min_within_batch_prop_detect = 0.1,
                            min_within_batch_prop_report = 0.5,
                            min_batch_prop = 0.5,
                            min_occurrence = 2,
                            batch_align_mz_tol = 1e-5,
                            batch_align_rt_tol = 50,
                            known_table = NA,
                            cluster = 4,
                            min_pres = 0.5,
                            min_run = 12,
                            mz_tol = 1e-5,
                            mz_tol_relative = NA,
                            rt_tol_relative = NA,
                            baseline_correct_noise_percentile = 0.05,
                            shape_model = "bi-Gaussian",
                            baseline_correct = 0,
                            peak_estim_method = "moment",
                            min_bw = NA,
                            max_bw = NA,
                            sd_cut = c(0.1, 100),
                            sigma_ratio_lim = c(0.05, 20),
                            component_eliminate = 0.01,
                            moment_power = 2,
                            align_mz_tol = NA,
                            align_rt_tol = NA,
                            max_align_mz_diff = 0.01,
                            pre_process = FALSE,
                            recover_mz_range = NA,
                            recover_rt_range = NA,
                            use_observed_range = TRUE,
                            match_tol_ppm = NA,
                            new_feature_min_count = 2,
                            recover_min_count = 3,
                            intensity_weighted = FALSE,
                            BIC_factor = 2,
                            do_plot = FALSE,
                            grouping_threshold = Inf) {

  filenames_batchwise <- bind_batch_label_column(filenames, metadata)
  batches_idx <- unique(metadata$batch)
  batchwise <- new("list")
  message("* processing ", length(batches_idx), " batches separately")
  


  for (batch.i in batches_idx) {
    files_batch <- dplyr::filter(filenames_batchwise, batch == batch.i)$filename
    samples_in_batch <- get_sample_name(files_batch)
    message("* processing ", length(files_batch), " samples from batch ", batch.i, ":\n", 
    paste(samples_in_batch, collapse = "\n"))
    
    features <- hybrid(
      filenames = files_batch,
      known_table = known_table,
      min_occurrence = min_occurrence,
      min_pres = min_pres,
      min_run = min_run,
      max_run = Inf,
      mz_tol = mz_tol,
      baseline_correct = baseline_correct,
      baseline_correct_noise_percentile = baseline_correct_noise_percentile,
      shape_model = shape_model,
      BIC_factor = BIC_factor,
      peak_estim_method = peak_estim_method,
      bandwidth = 0.5,
      min_bandwidth = min_bw,
      max_bandwidth = max_bw,
      sd_cut = sd_cut,
      sigma_ratio_lim = sigma_ratio_lim,
      component_eliminate =  component_eliminate,
      moment_power =  moment_power,
      mz_tol_relative = mz_tol_relative,
      rt_tol_relative = rt_tol_relative,
      mz_tol_absolute = 0.01,
      match_tol_ppm = match_tol_ppm,
      new_feature_min_count = new_feature_min_count,
      recover_mz_range = recover_mz_range,
      recover_rt_range = recover_rt_range,
      use_observed_range = use_observed_range,
      recover_min_count = recover_min_count,
      intensity_weighted = intensity_weighted,
      do_plot = do_plot,
      cluster = cluster,
      grouping_threshold = grouping_threshold
    )
    batchwise[[batch.i]] <- features
  }

  step_one_features <- list()
  for (batch_id in batches_idx) {
    step_one_features[[batch_id]] <- compute_intensity_medians(
      batchwise[[batch_id]]$recovered_feature_sample_table
    )
  }

  cluster <- parallel::makeCluster(cluster)
  doParallel::registerDoParallel(cluster)
  register_functions_to_cluster(cluster)

  pseudo_features <- list()
  for (batch_id in batches_idx) {    
    # Extract metadata and intensities
    batchwise_intensities <- batchwise[[batch_id]]$recovered_aligned_features$intensity
    batchwise_metadata <- batchwise[[batch_id]]$recovered_aligned_features$metadata[,c(1:14)]   

     # Extract sample names
    samples_in_batch <- colnames(batchwise_intensities)[-1]

    # Merge metadata and intesities
    pseudo_features[[batch_id]] <- dplyr::full_join(
        batchwise_metadata, 
        batchwise_intensities, 
        by = 'id') |> 
      mutate(area = apply(across(all_of(samples_in_batch)), 1, median)) |>
      dplyr::rename(sd1 = "sd1_mean", sd2 = "sd2_mean")
  }

  message("* computing clusters")
  sample_names = paste0("batch_", batches_idx)
  clustered <- compute_clusters(
    feature_tables = pseudo_features,
    mz_tol_relative = batch_align_mz_tol,
    mz_tol_absolute = max_align_mz_diff,
    mz_max_diff = 10 * mz_tol,
    rt_tol_relative = 10,
    do.plot = FALSE,
    sample_names = sample_names
  )

  message("* aligning time")
  corrected <- adjust.time(clustered$feature_tables, clustered$rt_tol_relative)  

  message("* second time computing clusters")
  res <- compute_clusters(
      feature_tables = corrected,
      mz_tol_relative = batch_align_mz_tol,
      mz_tol_absolute = max_align_mz_diff,
      mz_max_diff = 10 * mz_tol,
      rt_tol_relative = batch_align_rt_tol,
      do.plot = FALSE,
      sample_names = sample_names
  )
  
  message("* aligning features")
  aligned_features <- create_aligned_feature_table(
      features_table = dplyr::bind_rows(res$feature_tables),
      min_occurrence = ceiling(min_batch_prop * length(batches_idx)),
      sample_names = sample_names,
      rt_tol_relative = res$rt_tol_relative,
      mz_tol_relative = res$mz_tol_relative
  )

  aligned_wide <- as_wide_aligned_table(aligned_features)

  message("* recovering features across batches")
  recovered <- feature_recovery(
    cluster = cluster,
    step_one_features = step_one_features,
    batchwise = batchwise,
    filenames_batchwise = filenames_batchwise,
    corrected = corrected,
    aligned = aligned_wide,
    batches_idx = batches_idx,
    mz_tol = mz_tol,
    batch_align_mz_tol = batch_align_mz_tol,
    batch_align_rt_tol = batch_align_rt_tol,
    recover_mz_range = recover_mz_range,
    recover_rt_range = recover_rt_range,
    use_observed_range = use_observed_range,
    min_bw = min_bw,
    max_bw = max_bw,
    recover_min_count = recover_min_count,
    min_occurrence = min_occurrence,
    batch_names = sample_names 
  )

  snow::stopCluster(cluster)

  features <- new("list")
  features$batchwise_features <- batchwise
  features$known_table <- merge_known_tables(batchwise, batches_idx)
  features$aligned_features <- aligned_features
  features$final_features <- recovered   
  return(features)

}
