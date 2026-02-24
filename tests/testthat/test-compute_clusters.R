update_expected <- function(input, files, actual) {
  testdata <- file.path("..", "testdata")
  for(i in seq_along(files)) {
    arrow::write_parquet(actual$feature_tables[[i]], file.path(testdata, "clusters", paste0(files[i], "_", input ,"_clusters.parquet")))
  }
}

patrick::with_parameters_test_that(
  "test compute_clusters",
  {
    testdata <- file.path("..", "testdata")

    extracted <- read_parquet_files(files, input, ".parquet")

    actual <- compute_clusters(
      feature_tables = extracted,
      mz_tol_relative = NA,
      rt_tol_relative = NA,
      mz_max_diff = mz_max_diff,
      mz_tol_absolute = mz_tol_absolute,
      do.plot = FALSE,
      sample_names = files
    )

    expected <- read_parquet_files(files, "clusters", paste0("_", input, "_clusters.parquet"))

    for(i in seq_along(files)) {
      expect_equal(actual$feature_tables[[i]], expected[[i]])
    }

    expect_equal(actual$mz_tol_relative, expected_mz_tol_relative, tolerance=0.1)
    expect_equal(actual$rt_tol_relative, expected_rt_tol_relative, tolerance=0.1)

  },
  patrick::cases(
    RCX_shortened_extracted = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      input = "extracted",
      mz_max_diff = 10 * 1e-05,
      mz_tol_absolute = 0.01,
      expected_mz_tol_relative = 6.849039e-06,
      expected_rt_tol_relative = 36.9
    ),
    RCX_shortened_adjusted = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      input = "adjusted",
      mz_max_diff = 10 * 1e-05,
      mz_tol_absolute = 0.01,
      expected_mz_tol_relative = 6.856763e-06,
      expected_rt_tol_relative = 0.82
    )
  )
)

test_that("compute clusters simple", {
  testdata <- file.path("..", "testdata")
  files <- c("8_qc_no_dil_milliq", "21_qc_no_dil_milliq", "29_qc_no_dil_milliq")

  extracted <- read_parquet_files(files, "extracted", ".mzml.parquet")

  actual <- compute_clusters_simple(
    feature_tables = extracted,
    sample_names = files,
    mz_tol_ppm = 10,
    rt_tol = 2
  )

  actual <- actual[order(sapply(actual, function(x) x$sample_id[1]))]

  expected <- read_parquet_files(files, "clusters", ".parquet")

  expected <- expected[order(sapply(expected, function(x) x$sample_id[1]))]

  expect_equal(as.list(actual), expected, tolerance = 0.02)
})