#' @import dplyr tidyr tibble stringr arrow
NULL
#> NULL

register_functions_to_cluster <- function(cluster) {
    snow::clusterExport(cluster, list(
        'remove_noise',
        'prof.to.features',
        'load.lcms',
        'load.lcms.raw',
        'process_chunk',
        'adaptive.bin',
        'add_feature_ids',
        'find.turn.point',
        'msExtrema',
        'find_local_maxima',
        'aggregate_by_rt',
        'run_filter',
        'interpol.area',
        'load_file',
        'load_data',
        'plot_raw_profile_histogram',
        'compute_mass_values',
        'compute_densities',
        'compute_breaks',
        'compute_breaks_3',
        'compute_boundaries',
        'compute_rt_intervals_indices',
        'increment_counter',
        'rm.ridge',
        'compute_delta_rt',
        'bigauss.mix',
        'bigauss.esti',
        'rev_cum_sum',
        'compute_bounds',
        'validate_model_method_input',
        'preprocess_bandwidth',
        'preprocess_profile',
        'compute_gaussian_peak_shape',
        'compute_chromatographic_profile',
        'compute_dx',
        'compute_initiation_params',
        'compute_e_step',
        'compute_start_bound',
        'compute_end_bound',
        'compute_scale',
        'span',
        'compute_uniq_grp',
        'predict_smoothed_rt',
        'label_val_to_keep',
        "create_features_from_cluster",
        "validate_contents",
        "find_optima",
        "filter_based_on_density",
        "create_output",
        "create_metadata",
        "create_rt_row",
        "create_intensity_row",
        "comb",
        "clean_data_matrix",
        "create_aligned_feature_table",
        'bigauss.esti.EM',
        'solve_sigma',
        'prep_uv',
        'solve_a',
        'correct_time',
        'compute_comb',
        'compute_sel',
        'compute_template_adjusted_rt',
        'compute_corrected_features',
        'fill_missing_values',
        'recover.weaker',
        'get_custom_rt_tol',
        'compute_target_times',
        'predict_mz_break_indices',
        'compute_rectangle',
        'get_rt_region_indices',
        'refine_selection',
        'duplicate.row.remove',
        'get_times_to_use',
        'get_single_occurrence_mask',
        'compute_curr_rec_with_enough_peaks',
        'compute_mu_sc_std',
        'compute_pks_vlys_rt',
        'count_peaks',
        'get_features_in_rt_range',
        'get_mzrange_bound_indices',
        'compute_mass_density',
        'l2normalize',
        'compute_peaks_and_valleys',
        'as_feature_sample_table',
        'check_files',
        'get_sample_name',
        'draw_plot'
    ))
    snow::clusterEvalQ(cluster, library("dplyr"))
    snow::clusterEvalQ(cluster, library("stringr"))
    snow::clusterEvalQ(cluster, library("tidyselect"))
}

#' Concatenate multiple feature lists and add the sample id (origin of feature) as additional column.
#' 
#' @param features list List of tibbles containing extracted feature tables.
concatenate_feature_tables <- function(features, sample_names) {
    for (i in seq_along(features)) {
        if(!("sample_id" %in% colnames(features[[i]]))) {
            features[[i]] <- tibble::add_column(features[[i]], sample_id = sample_names[i])
        }
    }
    
    merged <- dplyr::bind_rows(features)
    return(merged)
}

#' @export
load_aligned_features <- function(metadata_file, intensities_file, rt_file) {
    metadata <- arrow::read_parquet(metadata_file)
    intensities <- arrow::read_parquet(intensities_file)
    rt <- arrow::read_parquet(rt_file)
    
    result <- list()
    result$metadata <- as_tibble(metadata)
    result$intensity <- as_tibble(intensities)
    result$rt <- as_tibble(rt)
    return(result)
}

#' Calculate the span of a numeric vector.
#' @description
#' This function calculates the span (range) of a numeric vector, ignoring NA values.
#' @param x A numeric vector.
#' @return A numeric value representing the span of the vector.
#' @export
span <- function(x) {
    diff(range(x, na.rm = TRUE))
}

#' Compute standard deviation of m/z values for feature groups.
#' @description
#' This function computes the standard deviation of m/z values for each group of features.
#' @param feature_groups A list of data frames, where each data frame represents a group of features with m/z values.
#' @return A numeric vector of standard deviations of m/z values for each feature group.
#' @export
compute_mz_sd <- function(feature_groups) {
    mz_sd <- c()
    for (i in seq_along(feature_groups)) {
        group <- feature_groups[[i]]
        
        if (nrow(group > 1)) {
            group_sd <- sd(group[, "mz"])
            mz_sd <- append(mz_sd, group_sd)
        }
    }
    return(mz_sd)
}

#' Get the number of available worker cores.
#' @description
#' This function determines the number of available worker cores, taking into account CRAN's limit on the number of cores.
#' @return An integer representing the number of available worker cores.
#' @export
get_num_workers <- function() {
    # CRAN limits the number of cores available to packages to 2
    # source https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions#50571533
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    
    if (nzchar(chk) && chk == "TRUE") {
        # use 2 cores in CRAN/Travis/AppVeyor
        num_workers <- 2L
    } else {
        # use all cores in devtools::test()
        num_workers <- parallel::detectCores()
    }
    return(num_workers)
}

read_parquet_files <- function(filename, folder, pattern) {
  testdata <- file.path("..", "testdata")

  input <- lapply(filename, function(x) {
    tibble::as_tibble(arrow::read_parquet(file.path(testdata, folder, paste0(x, pattern))))
  })

  return(input)
}
