update_expected <- function(actual) {
  arrow::write_parquet(actual$metadata, file.path("..", "testdata", "aligned", "metadata_table.parquet"))
  arrow::write_parquet(actual$intensity, file.path("..", "testdata", "aligned", "intensity_table.parquet"))
  arrow::write_parquet(actual$rt, file.path("..", "testdata", "aligned", "rt_table.parquet"))
}

patrick::with_parameters_test_that(
  "feature.align test",
  {
    testdata <- file.path("..", "testdata")

    corrected_features <- read_parquet_files(files, "adjusted", ".parquet")
    
    res <- compute_clusters(
        corrected_features,
        mz_tol_relative,
        mz_tol_absolute,
        10 * mz_tol,
        rt_tol_relative,
        do.plot,
        files
    )
    
    aligned_actual <- create_aligned_feature_table(
        dplyr::bind_rows(res$feature_tables),
        min_occurrence,
        files,
        res$rt_tol_relative,
        res$mz_tol_relative,
        cluster = get_num_workers()
    )
 
    aligned_expected <- load_aligned_features(
      file.path(testdata, "aligned", "metadata_table.parquet"),
      file.path(testdata, "aligned", "intensity_table.parquet"),
      file.path(testdata, "aligned", "rt_table.parquet")
    )

    expect_equal(aligned_actual, aligned_expected)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      min_occurrence = 2,
      mz_tol_relative = NA,
      rt_tol_relative = NA,
      mz_tol = 1e-05,
      mz_tol_absolute = 0.01,
      do.plot = FALSE
    )
  )
)


patrick::with_parameters_test_that(
  "compute_aligned_feature_table test",
  {
    testdata <- file.path("..", "testdata")

    corrected_features <- read_parquet_files(files, "clusters", "_adjusted_clusters.parquet")

    aligned_actual <- create_aligned_feature_table(
        dplyr::bind_rows(corrected_features),
        min_occurrence,
        files,
        rt_tol_relative,
        mz_tol_relative,
        cluster = get_num_workers()
    )

    aligned_expected <- load_aligned_features(
      file.path(testdata, "aligned", "metadata_table.parquet"),
      file.path(testdata, "aligned", "intensity_table.parquet"),
      file.path(testdata, "aligned", "rt_table.parquet")
    )

    expect_equal(aligned_actual, aligned_expected)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      min_occurrence = 2,
      mz_tol_relative = 6.84903911826453e-06,
      rt_tol_relative = 1.93185408267324
    )
  )
)
